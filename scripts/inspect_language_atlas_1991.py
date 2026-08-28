#!/usr/bin/env python3
"""Inspect the fixed Annexure-IV layout in the 1991 Language Atlas.

This maintainer tool is intentionally outside the targets graph.  It verifies
that the registered 56-page district table consists of seven eight-page blocks
and that each block exposes the complete 114-language column sequence (Atlas
columns 4 through 117).  Later extraction code can build on this checked layout
without making PDF parsing a normal build dependency.
"""

from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
from pathlib import Path

BLOCK_START_PAGES = tuple(range(205, 254, 8))
LANGUAGE_COLUMN_RANGES = (
    (4, 14), (15, 27), (28, 42), (43, 57),
    (58, 72), (73, 87), (88, 102), (103, 117),
)


def expected_columns(offset: int) -> tuple[int, ...]:
    lo, hi = LANGUAGE_COLUMN_RANGES[offset]
    return tuple(range(lo, hi + 1))


def header_columns(text: str, offset: int) -> tuple[int, ...]:
    # The column-number row is near the top of each page. Restricting the scan
    # avoids confusing speaker counts in the body with Atlas column numbers.
    # One scanned header renders column 115 as ``11 5``; permit whitespace
    # inside an expected number instead of special-casing that page.
    header = "\n".join(text.splitlines()[:28])
    observed = []
    for value in expected_columns(offset):
        digits = r"\s*".join(str(value))
        if re.search(rf"(?<!\d){digits}(?!\d)", header):
            observed.append(value)
    return tuple(observed)


def extract_layout_page(pdf: Path, page: int, pdftotext: str) -> str:
    completed = subprocess.run(
        [pdftotext, "-f", str(page), "-l", str(page), "-layout", str(pdf), "-"],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return completed.stdout


def inspect_atlas(pdf: Path, pdftotext: str) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for block_index, start_page in enumerate(BLOCK_START_PAGES, start=1):
        for offset in range(8):
            page = start_page + offset
            text = extract_layout_page(pdf, page, pdftotext)
            expected = expected_columns(offset)
            observed = header_columns(text, offset)
            rows.append({
                "block": block_index,
                "page": page,
                "page_offset": offset,
                "first_language_column": expected[0],
                "last_language_column": expected[-1],
                "expected_language_columns": len(expected),
                "observed_language_columns": len(observed),
                "layout_status": "verified" if observed == expected else "column_header_mismatch",
            })
    return rows


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fields = list(rows[0])
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def self_test() -> None:
    assert BLOCK_START_PAGES == (205, 213, 221, 229, 237, 245, 253)
    assert sum(len(expected_columns(i)) for i in range(8)) == 114
    assert tuple(v for i in range(8) for v in expected_columns(i)) == tuple(range(4, 118))
    synthetic = "\n".join(["header"] * 10 + ["  15 16 17 18 19 20 21 22 23 24 25 26 27"])
    assert header_columns(synthetic, 1) == tuple(range(15, 28))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pdf", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--pdftotext", default="pdftotext")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test()
        return 0
    if args.pdf is None or args.output is None:
        parser.error("--pdf and --output are required unless --self-test is used")
    executable = shutil.which(args.pdftotext)
    if executable is None:
        raise SystemExit("pdftotext is required to inspect the Language Atlas")
    rows = inspect_atlas(args.pdf, executable)
    bad = [row for row in rows if row["layout_status"] != "verified"]
    write_csv(args.output, rows)
    if bad:
        pages = ", ".join(str(row["page"]) for row in bad)
        raise SystemExit(f"Language Atlas layout check failed on PDF page(s): {pages}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
