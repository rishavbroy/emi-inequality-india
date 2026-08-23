#!/usr/bin/env python3
"""Verify report-derived DISE school-quality metadata from reviewed pages.

The tracked CSV contains reviewed district/page provenance. This maintainer
re-extracts the published all-school PTR, single-teacher-school share, and
(given the reporting definition in force) girls'-toilet share from those pages.
It is intentionally outside the targets runtime graph so normal replication
does not depend on Poppler.
"""
from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

NUMBER = re.compile(r"(?<![A-Za-z])\d+(?:\.\d+)?")
PATTERNS = {
    "report_pupils_per_teacher": re.compile(r"Pupil[- ]Teacher Ratio(?:\s*\(PTR\))?", re.I),
    "report_single_teacher_school_share": re.compile(r"(?:% )?Single[- ]Teacher Schools", re.I),
    "report_girls_toilet_school_share": re.compile(r"(?:% )?Schools with Girls[’']? Toilet", re.I),
}
EXPECTED_PERFORMANCE_VALUES = {
    "2011-12": 6,
    "2012-13": 8,
    "2013-14": 8,
    "2014-15": 16,
}
FIELDS = [
    "academic_year",
    "state_report",
    "district_report",
    "source_pdf",
    "source_page",
    "report_pupils_per_teacher",
    "report_single_teacher_school_share",
    "report_girls_toilet_school_share",
    "girls_toilet_definition",
]


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


def extract_pdf_pages(pdf: Path, pdftotext: str, workdir: Path) -> list[str]:
    target = workdir / f"{pdf.stem}.txt"
    subprocess.run(
        [pdftotext, "-layout", str(pdf), str(target)],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return target.read_text(errors="ignore").split("\f")


def parse_published_value(text: str, pattern: re.Pattern[str], expected: int) -> float:
    for line in text.splitlines():
        hit = pattern.search(line)
        if hit is None:
            continue
        values = [float(match.group(0)) for match in NUMBER.finditer(line[hit.end():])]
        if len(values) >= expected:
            return values[expected - 1]
    raise ValueError(f"page lacks expected performance-indicator row: {pattern.pattern}")


def parsed_values(text: str, academic_year: str) -> dict[str, float]:
    if academic_year not in EXPECTED_PERFORMANCE_VALUES:
        raise ValueError(f"unsupported report-card school-quality year: {academic_year}")
    expected = EXPECTED_PERFORMANCE_VALUES[academic_year]
    values = {
        name: parse_published_value(text, pattern, expected)
        for name, pattern in PATTERNS.items()
    }
    if not (0 <= values["report_single_teacher_school_share"] <= 100):
        raise ValueError("single-teacher-school share is outside [0, 100]")
    if not (0 <= values["report_girls_toilet_school_share"] <= 100):
        raise ValueError("girls'-toilet share is outside [0, 100]")
    if values["report_pupils_per_teacher"] < 0:
        raise ValueError("PTR is negative")
    return values


def expected_toilet_definition(academic_year: str) -> str:
    return (
        "all_schools"
        if academic_year == "2011-12"
        else "girls_and_coeducational_schools"
    )


def normalized(value: str) -> str:
    return value.strip()


def verify_rows(
    tracked: Path,
    registry: Path,
    archive_root: Path,
    pdftotext: str,
) -> int:
    reports = registered_reports(registry)
    rows = read_csv(tracked)
    if not rows:
        raise ValueError("tracked DISE report school-quality metadata are empty")
    keys: set[tuple[str, str, str]] = set()
    grouped: dict[str, list[dict[str, str]]] = {}
    for row in rows:
        academic_year = normalized(row["academic_year"])
        key = (
            academic_year,
            normalized(row["state_report"]),
            normalized(row["district_report"]),
        )
        if key in keys:
            raise ValueError(f"duplicate tracked report school-quality district-year: {key}")
        keys.add(key)
        source = normalized(row["source_pdf"])
        if source not in reports:
            raise ValueError(f"unregistered source_pdf in school-quality metadata: {source}")
        grouped.setdefault(source, []).append(row)

    with tempfile.TemporaryDirectory(prefix="dise-school-quality-") as temporary:
        workdir = Path(temporary)
        for source, source_rows in grouped.items():
            pdf = archive_root / reports[source]
            if not pdf.is_file():
                raise FileNotFoundError(f"missing registered DISE report PDF: {pdf}")
            pages = extract_pdf_pages(pdf, pdftotext, workdir)
            for row in source_rows:
                academic_year = normalized(row["academic_year"])
                page = int(row["source_page"])
                if page < 1 or page > len(pages):
                    raise ValueError(f"tracked page is outside PDF range: {source}:{page}")
                values = parsed_values(pages[page - 1], academic_year)
                for column, parsed in values.items():
                    tracked_value = float(row[column])
                    if abs(parsed - tracked_value) > 1e-9:
                        raise ValueError(
                            f"tracked {column} differs from report at {source}:{page}: "
                            f"tracked={tracked_value}, parsed={parsed}"
                        )
                expected_definition = expected_toilet_definition(academic_year)
                if normalized(row["girls_toilet_definition"]) != expected_definition:
                    raise ValueError(
                        f"girls'-toilet definition mismatch for "
                        f"{academic_year}/{row['state_report']}/{row['district_report']}: "
                        f"expected {expected_definition}"
                    )
    return len(rows)


def self_test() -> None:
    page_2011 = """
Performance Indicators  P. only P + UP P+sec/hs U.P. only UP+sec All Schools
% Single-teacher schools 1.7 0.0 0.0 0.0 0.0 1.0 II 3,952
% Schools with girls toilet 94.9 97.1 0.0 0.0 100.0 96.6 III 3,999
Pupil-teacher ratio (PTR) 9 11 0 0 10 10 III 1,939 26 13
"""
    values = parsed_values(page_2011, "2011-12")
    assert values["report_single_teacher_school_share"] == 1.0
    assert values["report_girls_toilet_school_share"] == 96.6
    assert values["report_pupils_per_teacher"] == 10.0

    page_2014 = """
Performance Indicators  Primary Only ... All Schools
13-14 14-15 13-14 14-15 13-14 14-15 13-14 14-15 13-14 14-15 13-14 14-15 13-14 14-15 13-14 14-15
Single-Teacher Schools 1.6 1.2 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.9 0.6
Schools with Girls' Toilet 93.7 100.0 97.1 100.0 100.0 100.0 0.0 100.0 100.0 100.0 100.0 100.0 0.0 0.0 95.7 100.0
Pupil-Teacher Ratio 9 10 10 11 11 11 0 1 7 6 10 10 0 0 10 11
"""
    values = parsed_values(page_2014, "2014-15")
    assert values["report_single_teacher_school_share"] == 0.6
    assert values["report_girls_toilet_school_share"] == 100.0
    assert values["report_pupils_per_teacher"] == 11.0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--archive-root", type=Path)
    parser.add_argument(
        "--tracked",
        type=Path,
        default=Path("data/metadata/dise_report_school_quality_2011_15.csv"),
    )
    parser.add_argument(
        "--registry",
        type=Path,
        default=Path("data/metadata/dise_archive_registry.csv"),
    )
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test()
        print("DISE report school-quality parser self-test: PASS")
        return 0
    if args.archive_root is None:
        parser.error("--archive-root is required unless --self-test is used")
    executable = shutil.which("pdftotext")
    if executable is None:
        raise SystemExit("pdftotext is required; install Poppler utilities and retry")
    count = verify_rows(args.tracked, args.registry, args.archive_root, executable)
    print(f"DISE report school-quality metadata verified: {count} district-years")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
