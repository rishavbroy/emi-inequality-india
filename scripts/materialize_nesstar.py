#!/usr/bin/env python3
"""Materialize contracted Nesstar blocks as deterministic gitignored tables."""

from __future__ import annotations

import argparse
import csv
import hashlib
import importlib.metadata
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "data/metadata/file_manifest.csv"
CONTRACTS = ROOT / "data/metadata/nesstar_conversion_contracts.csv"


def read_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def source_contract(source_id: str) -> list[dict[str, str]]:
    rows = [row for row in read_rows(CONTRACTS) if row["source_id"] == source_id]
    if not rows:
        available = sorted({row["source_id"] for row in read_rows(CONTRACTS)})
        raise SystemExit(
            f"Unknown Nesstar source_id {source_id!r}. Available sources: {', '.join(available)}"
        )
    block_ids = [row["block_id"] for row in rows]
    if len(block_ids) != len(set(block_ids)):
        raise SystemExit(f"Nesstar contract {source_id} contains duplicate block_id values.")
    singleton_fields = (
        "converter_package", "converter_executable", "converter_version", "nesstar_file_id", "ddi_file_id"
    )
    for field in singleton_fields:
        values = {row[field] for row in rows}
        if len(values) != 1 or not next(iter(values)):
            raise SystemExit(f"Nesstar contract {source_id} must declare one non-empty {field}.")
    return rows


def manifest_path(file_id: str) -> tuple[Path, int]:
    rows = [row for row in read_rows(MANIFEST) if row["file_id"] == file_id]
    if len(rows) != 1:
        raise SystemExit(f"Expected one manifest row for {file_id}.")
    row = rows[0]
    return ROOT / row["relative_path"], int(row["expected_size_bytes"])


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def csv_shape(path: Path) -> tuple[list[str], int]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.reader(handle)
        try:
            header = next(reader)
        except StopIteration:
            return [], 0
        return header, sum(1 for _ in reader)


def executable_package_version(executable: Path, package: str) -> str:
    """Read package metadata from the console script's own Python environment."""
    try:
        return importlib.metadata.version(package)
    except importlib.metadata.PackageNotFoundError:
        pass

    candidates: list[Path] = []
    if sys.platform == "win32":
        candidates.extend([executable.parent / "python.exe", executable.parent / "python3.exe"])
    else:
        try:
            first_line = executable.open("rb").readline().decode("utf-8", errors="replace").strip()
        except OSError:
            first_line = ""
        if first_line.startswith("#!"):
            interpreter = first_line[2:].split()[0]
            if interpreter and interpreter != "/usr/bin/env":
                candidates.append(Path(interpreter))
        candidates.extend([executable.parent / "python", executable.parent / "python3"])

    code = (
        "import importlib.metadata as m; "
        f"print(m.version({package!r}))"
    )
    for python in candidates:
        if not python.is_file():
            continue
        result = subprocess.run(
            [str(python), "-c", code], capture_output=True, text=True, check=False
        )
        if result.returncode == 0 and result.stdout.strip():
            return result.stdout.strip()
    raise SystemExit(
        f"Could not verify {package} from the environment that owns {executable}. "
        "Install the pinned CLI in an isolated environment (venv, pipx, or uv tool) "
        "and ensure its console script is on PATH."
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("source_id", help="source_id from nesstar_conversion_contracts.csv")
    parser.add_argument("--force", action="store_true", help="replace existing interim tables")
    args = parser.parse_args()

    contract = source_contract(args.source_id)
    package = contract[0]["converter_package"]
    expected_version = contract[0]["converter_version"]
    executable_name = contract[0]["converter_executable"]
    executable_raw = shutil.which(executable_name)
    if not executable_raw:
        raise SystemExit(
            f"{executable_name} is not on PATH. Install {package}=={expected_version} "
            "in an isolated environment; see REPLICATION.md."
        )
    executable = Path(executable_raw).resolve()
    observed_version = executable_package_version(executable, package)
    if observed_version != expected_version:
        raise SystemExit(f"Expected {package} {expected_version}, found {observed_version}.")

    nesstar, nesstar_bytes = manifest_path(contract[0]["nesstar_file_id"])
    ddi, ddi_bytes = manifest_path(contract[0]["ddi_file_id"])
    for path, expected in ((nesstar, nesstar_bytes), (ddi, ddi_bytes)):
        if not path.is_file():
            raise SystemExit(f"Missing Nesstar conversion source: {path}")
        observed = path.stat().st_size
        if observed != expected:
            raise SystemExit(f"Source size mismatch for {path}: {observed} != {expected}.")

    destinations = {row["block_id"]: ROOT / row["relative_path"] for row in contract}
    output_parents = {path.parent for path in destinations.values()}
    if len(output_parents) != 1:
        raise SystemExit(f"Nesstar contract {args.source_id} must use one output directory.")
    output_root = next(iter(output_parents))
    output_root.mkdir(parents=True, exist_ok=True)
    existing = [path for path in destinations.values() if path.exists()]
    if existing and not args.force:
        raise SystemExit(
            f"Converted files already exist for {args.source_id}; pass --force to replace them."
        )

    with tempfile.TemporaryDirectory(prefix=f"{args.source_id}_nesstar_") as temp_dir:
        subprocess.run(
            [str(executable), "convert", str(nesstar), str(ddi), temp_dir, "--formats", "csv"],
            check=True,
        )
        candidates = list(Path(temp_dir).rglob("*.csv"))
        if not candidates:
            raise SystemExit(f"{package} produced no CSV files for {args.source_id}.")

        shapes: dict[Path, tuple[list[str], int]] = {}
        selected: dict[str, tuple[Path, int]] = {}
        for row in contract:
            block_id = row["block_id"]
            signature = row["signature_column"]
            expected_rows = int(row["expected_rows"])
            matches = []
            for candidate in candidates:
                if candidate not in shapes:
                    shapes[candidate] = csv_shape(candidate)
                header, nrows = shapes[candidate]
                if signature in header and nrows == expected_rows:
                    matches.append(candidate)
            if len(matches) != 1:
                raise SystemExit(
                    f"Expected one converted {block_id} block with {expected_rows} rows "
                    f"and column {signature}; found {len(matches)}."
                )
            selected[block_id] = (matches[0], expected_rows)

        records = []
        for row in contract:
            block_id = row["block_id"]
            source, nrows = selected[block_id]
            destination = destinations[block_id]
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, destination)
            records.append(
                {
                    "source_id": args.source_id,
                    "block_id": block_id,
                    "relative_path": str(destination.relative_to(ROOT)),
                    "rows": nrows,
                    "bytes": destination.stat().st_size,
                    "sha256": sha256(destination),
                    "converter_package": package,
                    "converter_version": observed_version,
                }
            )

    sidecar = output_root / "conversion_manifest.csv"
    with sidecar.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(records[0]))
        writer.writeheader()
        writer.writerows(records)
    print(f"Wrote {sidecar.relative_to(ROOT)}")
    for record in records:
        print(
            f"  {record['block_id']}: {record['relative_path']} "
            f"({record['rows']} rows)"
        )


if __name__ == "__main__":
    main()
