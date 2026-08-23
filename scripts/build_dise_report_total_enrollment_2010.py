#!/usr/bin/env python3
"""Rebuild published 2010-11 DISE district enrollment totals from reviewed report pages.

The tracked language metadata supplies reviewed district/page provenance. This
maintainer extracts only the current-year Total Pr. and Total U.P. counts from
those registered 2010-11 pages. It remains outside the targets graph so normal
replication does not acquire a Poppler dependency.
"""
from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
from pathlib import Path

NUMBER = re.compile(r"(?<![\w.])(?:\d{1,3}(?:,\d{3})+|\d+)(?![\w.])")
PRIMARY = re.compile(r"\bTotal\s+Pr\.\s*", re.IGNORECASE)
UPPER = re.compile(r"\bTotal\s+U\.?P\.?\s*", re.IGNORECASE)


def parse_int(text: str) -> int:
    return int(text.replace(",", ""))


def last_number_after(pattern: re.Pattern[str], lines: list[str]) -> int | None:
    for line in lines:
        hit = pattern.search(line)
        if hit is None:
            continue
        numbers = [parse_int(match.group(0)) for match in NUMBER.finditer(line[hit.end():])]
        if numbers:
            return numbers[-1]
    return None


def parse_current_total_enrollment(text: str) -> int:
    lines = text.splitlines()
    primary = last_number_after(PRIMARY, lines)
    upper = last_number_after(UPPER, lines)
    if primary is None or upper is None:
        raise ValueError("page lacks published current-year Total Pr. and Total U.P. counts")
    return primary + upper


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


def extract_page(pdf: Path, page: int, pdftotext: str) -> str:
    completed = subprocess.run(
        [
            pdftotext,
            "-f", str(page),
            "-l", str(page),
            "-layout",
            str(pdf),
            "-",
        ],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return completed.stdout


def provenance_rows(path: Path) -> list[dict[str, str]]:
    rows = [
        row for row in read_csv(path)
        if row["academic_year"].strip() == "2010-11"
    ]
    keys = {
        (
            row["academic_year"].strip(),
            row["state_report"].strip(),
            row["district_report"].strip(),
        )
        for row in rows
    }
    if len(keys) != len(rows):
        raise ValueError("2010-11 report provenance is not unique by district-year")
    return sorted(
        rows,
        key=lambda row: (row["state_report"], row["district_report"]),
    )


def rebuild(
    provenance: Path,
    registry: Path,
    archive_root: Path,
    pdftotext: str,
) -> list[dict[str, str]]:
    reports = registered_reports(registry)
    rebuilt = []
    for row in provenance_rows(provenance):
        source = row["source_pdf"].strip()
        if source not in reports:
            raise ValueError(f"unregistered source_pdf in DISE provenance: {source}")
        pdf = archive_root / reports[source]
        if not pdf.is_file():
            raise FileNotFoundError(f"missing registered DISE report PDF: {pdf}")
        page = int(row["source_page"])
        total = parse_current_total_enrollment(extract_page(pdf, page, pdftotext))
        rebuilt.append({
            "academic_year": "2010-11",
            "state_report": row["state_report"].strip(),
            "district_report": row["district_report"].strip(),
            "source_pdf": source,
            "source_page": str(page),
            "report_priority": row["report_priority"].strip(),
            "report_total_enrollment": str(total),
        })
    return rebuilt


FIELDS = [
    "academic_year",
    "state_report",
    "district_report",
    "source_pdf",
    "source_page",
    "report_priority",
    "report_total_enrollment",
]


def write_csv(path: Path, rows: list[dict[str, str]]) -> None:
    if not rows:
        raise ValueError("refusing to write empty 2010-11 report totals")
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS)
        writer.writeheader()
        writer.writerows(rows)
    temporary.replace(path)


def normalized_rows(path: Path) -> list[dict[str, str]]:
    return [
        {field: (row.get(field) or "").strip() for field in FIELDS}
        for row in read_csv(path)
    ]


def self_test() -> None:
    page = """
Performance indicators                                      Enrolment*
Grade                       2008-09    2009-10    2010-11
I                                981        894        963
Total Pr.                       4,814      4,943      4,861
Total U.P                       1,820      2,640      2,621
"""
    assert parse_current_total_enrollment(page) == 7482


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive-root", type=Path)
    parser.add_argument(
        "--provenance",
        type=Path,
        default=Path("data/metadata/dise_report_language_enrollment.csv"),
    )
    parser.add_argument(
        "--registry",
        type=Path,
        default=Path("data/metadata/dise_archive_registry.csv"),
    )
    parser.add_argument(
        "--tracked",
        type=Path,
        default=Path("data/metadata/dise_report_total_enrollment_2010_11.csv"),
    )
    parser.add_argument("--output", type=Path)
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()

    if args.self_test:
        self_test()
        print("DISE 2010-11 report-total parser self-test: PASS")
        return 0
    if args.archive_root is None:
        parser.error("--archive-root is required unless --self-test is used")
    executable = shutil.which("pdftotext")
    if executable is None:
        raise SystemExit("pdftotext is required; install Poppler utilities and retry")

    rows = rebuild(args.provenance, args.registry, args.archive_root, executable)
    if args.output is not None:
        write_csv(args.output, rows)
        print(f"Wrote {len(rows)} district totals to {args.output}")
    else:
        if normalized_rows(args.tracked) != [
            {field: row[field] for field in FIELDS} for row in rows
        ]:
            raise ValueError(
                "rebuilt 2010-11 report totals differ from the tracked metadata"
            )
        print(f"DISE 2010-11 report totals verified: {len(rows)} districts")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
