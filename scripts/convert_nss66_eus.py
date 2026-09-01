#!/usr/bin/env python3
"""Materialize NSS66 Schedule 10 Nesstar blocks as deterministic interim CSVs."""

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
CONTRACT = ROOT / "data/metadata/nss66_conversion_contract.csv"


def read_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


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


def converter_version() -> str:
    try:
        return importlib.metadata.version("nesstar-converter")
    except importlib.metadata.PackageNotFoundError as exc:
        raise SystemExit(
            "nesstar-converter is not installed. Install the pinned converter with: "
            "python -m pip install nesstar-converter==1.0.4"
        ) from exc


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--force", action="store_true", help="replace existing interim CSVs")
    args = parser.parse_args()

    contract = read_rows(CONTRACT)
    versions = {row["converter_version"] for row in contract}
    if len(versions) != 1:
        raise SystemExit("NSS66 conversion contract must declare exactly one converter version.")
    expected_version = next(iter(versions))
    observed_version = converter_version()
    if observed_version != expected_version:
        raise SystemExit(
            f"Expected nesstar-converter {expected_version}, found {observed_version}."
        )
    executable = shutil.which("nesstar-converter")
    if not executable:
        raise SystemExit("nesstar-converter console script is not on PATH.")

    nesstar, nesstar_bytes = manifest_path("nss66_eus_nesstar")
    ddi, ddi_bytes = manifest_path("nss66_eus_ddi")
    for path, expected in ((nesstar, nesstar_bytes), (ddi, ddi_bytes)):
        if not path.is_file():
            raise SystemExit(f"Missing NSS66 conversion source: {path}")
        if path.stat().st_size != expected:
            raise SystemExit(
                f"NSS66 source size mismatch for {path}: {path.stat().st_size} != {expected}."
            )

    output_root = ROOT / "data/interim/nss66_eus"
    output_root.mkdir(parents=True, exist_ok=True)
    destinations = {row["file_id"]: ROOT / row["relative_path"] for row in contract}
    existing = [path for path in destinations.values() if path.exists()]
    if existing and not args.force:
        raise SystemExit("Converted NSS66 files already exist; pass --force to replace them.")

    with tempfile.TemporaryDirectory(prefix="nss66_nesstar_") as temp_dir:
        subprocess.run(
            [executable, "convert", str(nesstar), str(ddi), temp_dir, "--formats", "csv"],
            check=True,
        )
        candidates = list(Path(temp_dir).rglob("*.csv"))
        if not candidates:
            raise SystemExit("nesstar-converter produced no CSV files.")

        shapes: dict[Path, tuple[list[str], int]] = {}
        selected: dict[str, tuple[Path, int]] = {}
        for row in contract:
            file_id = row["file_id"]
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
                    f"Expected one converted {file_id} block with {expected_rows} rows "
                    f"and column {signature}; found {len(matches)}."
                )
            selected[file_id] = (matches[0], expected_rows)

        records = []
        for row in contract:
            file_id = row["file_id"]
            source, nrows = selected[file_id]
            destination = destinations[file_id]
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, destination)
            records.append(
                {
                    "file_id": file_id,
                    "relative_path": str(destination.relative_to(ROOT)),
                    "rows": nrows,
                    "bytes": destination.stat().st_size,
                    "sha256": sha256(destination),
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
        print(f"  {record['file_id']}: {record['relative_path']} ({record['rows']} rows)")


if __name__ == "__main__":
    main()
