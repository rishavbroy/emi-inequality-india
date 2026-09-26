"""Shared helpers for reviewed DISE report-card maintainer scripts."""

from __future__ import annotations

import csv
import subprocess
from pathlib import Path


def parse_int(text: str) -> int:
    return int(text.replace(",", ""))


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


def extract_pdf_page(pdf: Path, page: int, pdftotext: str) -> str:
    completed = subprocess.run(
        [
            pdftotext,
            "-f",
            str(page),
            "-l",
            str(page),
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
