#!/usr/bin/env python3
"""Rebuild DISE report-card English/Hindi counts from reviewed PDF/page provenance.

This maintainer tool is intentionally outside the R/targets pipeline.  The tracked
CSV supplies the reviewed district-to-page provenance; this script re-extracts
only the numeric language counts from those registered pages using Poppler's
`pdftotext -layout`.
"""

from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
import sys
from collections import defaultdict
from pathlib import Path
from typing import Iterable

LANGUAGES = ("english", "hindi")
NUMBER = re.compile(r"(?<![\w.])(?:\d{1,3}(?:,\d{3})+|\d+)(?![\w.])")
MEDIUM_HEADING = re.compile(r"medium\s+of\s+instruction", re.IGNORECASE)


def parse_int(text: str) -> int:
    return int(text.replace(",", ""))


def medium_block(text: str, radius: int = 28) -> list[str]:
    lines = text.splitlines()
    hit = next((i for i, line in enumerate(lines) if MEDIUM_HEADING.search(line)), None)
    if hit is None:
        raise ValueError("page has no 'medium of instruction' heading")
    return lines[hit : min(len(lines), hit + radius)]


def _has_total_header(lines: list[str], row_index: int) -> bool:
    start = max(0, row_index - 6)
    return any(re.search(r"\btotal\b", line, re.IGNORECASE) for line in lines[start:row_index])


def parse_languages_as_rows(lines: list[str]) -> dict[str, int | None] | None:
    values: dict[str, int | None] = {language: None for language in LANGUAGES}
    matched = False
    for i, line in enumerate(lines):
        stripped = line.strip()
        language = next(
            (name for name in LANGUAGES if re.match(rf"^{name}\b", stripped, re.IGNORECASE)),
            None,
        )
        if language is None:
            continue
        numbers = [parse_int(m.group(0)) for m in NUMBER.finditer(stripped)]
        if not numbers:
            continue
        if not _has_total_header(lines, i):
            raise ValueError(
                f"{language} row found without a nearby Total column; refusing to guess"
            )
        values[language] = numbers[-1]
        matched = True
    return values if matched else None


def _column_positions(header: str) -> dict[str, int]:
    positions = {}
    lower = header.lower()
    for language in LANGUAGES:
        pos = lower.find(language)
        if pos >= 0:
            positions[language] = pos
    return positions


def _nearest_number_at_column(line: str, position: int) -> int | None:
    matches = list(NUMBER.finditer(line))
    if not matches:
        return None
    nearest = min(matches, key=lambda m: abs(m.start() - position))
    return parse_int(nearest.group(0))


def parse_languages_as_columns(lines: list[str]) -> dict[str, int | None] | None:
    header_index = None
    positions: dict[str, int] = {}
    for i, line in enumerate(lines):
        candidate = _column_positions(line)
        if candidate:
            header_index = i
            positions = candidate
            break
    if header_index is None:
        return None

    total_line = next(
        (
            line
            for line in lines[header_index + 1 :]
            if re.match(r"^\s*(?:grand\s+)?total\b", line, re.IGNORECASE)
        ),
        None,
    )
    if total_line is None:
        raise ValueError("language columns found without an explicit Total row; refusing to guess")

    return {
        language: _nearest_number_at_column(total_line, position)
        for language, position in positions.items()
    } | {language: None for language in LANGUAGES if language not in positions}


def parse_language_counts(text: str) -> dict[str, int | None]:
    lines = medium_block(text)
    parsed = parse_languages_as_rows(lines)
    if parsed is None:
        parsed = parse_languages_as_columns(lines)
    if parsed is None or parsed.get("english") is None:
        raise ValueError("could not resolve English enrollment from medium-of-instruction table")
    return parsed


def extract_page(pdf: Path, page: int, pdftotext: str) -> str:
    command = [
        pdftotext,
        "-f",
        str(page),
        "-l",
        str(page),
        "-layout",
        str(pdf),
        "-",
    ]
    completed = subprocess.run(
        command,
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return completed.stdout


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        return list(csv.DictReader(handle))


def registered_reports(registry: Path) -> dict[str, Path]:
    reports: dict[str, Path] = {}
    for row in read_csv(registry):
        for column in ("report_primary", "report_secondary"):
            relative = (row.get(column) or "").strip()
            if not relative:
                continue
            name = Path(relative).name
            if name in reports and reports[name] != Path(relative):
                raise ValueError(f"duplicate registered report basename: {name}")
            reports[name] = Path(relative)
    return reports


def preferred_rows(rows: Iterable[dict[str, str]]) -> list[dict[str, str]]:
    grouped: dict[tuple[str, str, str], list[dict[str, str]]] = defaultdict(list)
    for row in rows:
        key = (
            row["academic_year"].strip(),
            row["state_report"].strip(),
            row["district_report"].strip(),
        )
        grouped[key].append(row)

    selected = []
    for key, group in grouped.items():
        priorities = [int(row["report_priority"]) for row in group]
        best = min(priorities)
        winners = [row for row in group if int(row["report_priority"]) == best]
        if len(winners) != 1:
            raise ValueError(f"non-unique preferred report provenance for {key}")
        selected.append(winners[0])
    return sorted(
        selected,
        key=lambda row: (
            row["academic_year"],
            row["state_report"],
            row["district_report"],
        ),
    )


def rebuild(
    metadata: Path,
    registry: Path,
    archive_root: Path,
    pdftotext: str,
) -> list[dict[str, str]]:
    report_map = registered_reports(registry)
    rebuilt = []
    for row in preferred_rows(read_csv(metadata)):
        source = row["source_pdf"].strip()
        if source not in report_map:
            raise ValueError(f"unregistered source_pdf in report metadata: {source}")
        pdf = archive_root / report_map[source]
        if not pdf.is_file():
            raise FileNotFoundError(f"missing registered DISE report PDF: {pdf}")
        page = int(row["source_page"])
        counts = parse_language_counts(extract_page(pdf, page, pdftotext))
        out = dict(row)
        out["english_enrollment"] = str(counts["english"])
        out["hindi_enrollment"] = "" if counts["hindi"] is None else str(counts["hindi"])
        rebuilt.append(out)
    return rebuilt


def write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    if not rows:
        raise ValueError("refusing to write an empty report-language extraction")
    fields = [
        "academic_year",
        "state_report",
        "district_report",
        "source_pdf",
        "source_page",
        "report_priority",
        "english_enrollment",
        "hindi_enrollment",
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    temporary.replace(path)


def check_equal(expected: Path, rows: list[dict[str, str]]) -> None:
    current = preferred_rows(read_csv(expected))
    normalized = [
        {key: (row.get(key) or "").strip() for key in row.keys()}
        for row in rows
    ]
    existing = [
        {key: (row.get(key) or "").strip() for key in row.keys()}
        for row in current
    ]
    if normalized != existing:
        raise ValueError(
            "rebuilt DISE report-language metadata differs from the tracked CSV; "
            "rerun with --output to inspect the candidate extraction"
        )


def self_test() -> None:
    rows_layout = """
ENROLMENT BY MEDIUM OF INSTRUCTION
Language          Primary   P+UP   P+UP+S+HS   UP+S+HS   Total
English            60,000   3,972      292,163    32,480 388,615
Hindi              10,000   2,000       18,000     1,000  31,000
"""
    assert parse_language_counts(rows_layout) == {
        "english": 388615,
        "hindi": 31000,
    }

    columns_layout = """
MEDIUM OF INSTRUCTION
Category                    English      Hindi      Urdu
Primary                       40,000     20,000     3,000
Upper Primary                 10,000      5,000     1,000
TOTAL                          50,000     25,000     4,000
"""
    assert parse_language_counts(columns_layout) == {
        "english": 50000,
        "hindi": 25000,
    }

    provenance = [
        {
            "academic_year": "2009-10",
            "state_report": "State",
            "district_report": "District",
            "report_priority": "2",
        },
        {
            "academic_year": "2009-10",
            "state_report": "State",
            "district_report": "District",
            "report_priority": "1",
        },
    ]
    assert preferred_rows(provenance)[0]["report_priority"] == "1"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive-root", type=Path)
    parser.add_argument(
        "--metadata",
        type=Path,
        default=Path("data/metadata/dise_report_language_enrollment.csv"),
    )
    parser.add_argument(
        "--registry",
        type=Path,
        default=Path("data/metadata/dise_archive_registry.csv"),
    )
    parser.add_argument("--output", type=Path)
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)

    if args.self_test:
        self_test()
        print("DISE report-language parser self-test: PASS")
        return 0

    if args.archive_root is None:
        parser.error("--archive-root is required unless --self-test is used")

    executable = shutil.which("pdftotext")
    if executable is None:
        raise SystemExit(
            "pdftotext is required for this maintainer command; "
            "install Poppler utilities and retry"
        )

    rows = rebuild(args.metadata, args.registry, args.archive_root, executable)
    if args.output is None:
        check_equal(args.metadata, rows)
        print(f"DISE report-language metadata verified: {len(rows)} district-years")
    else:
        write_csv(args.output, rows)
        print(f"Wrote {len(rows)} district-years to {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
