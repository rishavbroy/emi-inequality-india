#!/usr/bin/env python3
"""Extract auditable candidate cells from Language Atlas of India 1991 Annexure IV.

This is a maintainer-side source-construction tool, not a targets dependency.
It uses Poppler's positioned text output to verify the fixed 56-page Annexure-IV
layout and writes raw cell text, conservative integer parses, and an explicit
review queue. Candidate values are not production-ready until district rows and
all flagged cells have been reviewed and validated against Census/SHRUG totals.
"""

from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from pathlib import Path

BLOCK_START_PAGES = tuple(range(205, 254, 8))
LANGUAGE_COLUMN_RANGES = (
    (4, 14), (15, 27), (28, 42), (43, 57),
    (58, 72), (73, 87), (88, 102), (103, 117),
)
XHTML = "{http://www.w3.org/1999/xhtml}"
ROW_Y_MIN = 195.0
ROW_Y_MAX = 1135.0
ROW_Y_TOLERANCE = 6.5
LABEL_Y_CLUSTER = 4.5


@dataclass(frozen=True)
class Word:
    text: str
    x0: float
    x1: float
    y0: float
    y1: float

    @property
    def x(self) -> float:
        return (self.x0 + self.x1) / 2

    @property
    def y(self) -> float:
        return (self.y0 + self.y1) / 2


def expected_columns(offset: int) -> tuple[int, ...]:
    lo, hi = LANGUAGE_COLUMN_RANGES[offset]
    return tuple(range(lo, hi + 1))


def extract_bbox_page(pdf: Path, page: int, pdftotext: str) -> list[Word]:
    completed = subprocess.run(
        [pdftotext, "-f", str(page), "-l", str(page), "-bbox-layout", str(pdf), "-"],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    root = ET.fromstring(completed.stdout)
    return [
        Word(
            text="".join(node.itertext()).strip(),
            x0=float(node.attrib["xMin"]),
            x1=float(node.attrib["xMax"]),
            y0=float(node.attrib["yMin"]),
            y1=float(node.attrib["yMax"]),
        )
        for node in root.iter(XHTML + "word")
    ]


def header_column_centers(words: list[Word], offset: int) -> tuple[dict[int, float], tuple[int, ...]]:
    expected = expected_columns(offset)
    centers: dict[int, float] = {}
    for column in expected:
        matches = [
            word for word in words
            if 155 <= word.y <= 205 and word.text == str(column)
        ]
        if matches:
            centers[column] = min(matches, key=lambda word: word.y).x
    missing = tuple(column for column in expected if column not in centers)
    if missing:
        if len(missing) > 1 or len(centers) < 2:
            raise ValueError(
                "cannot infer Atlas header centers; missing columns: "
                + ", ".join(map(str, missing))
            )
        positions = {column: i for i, column in enumerate(expected)}
        observed = sorted(centers)
        left, right = observed[0], observed[-1]
        step = (centers[right] - centers[left]) / (positions[right] - positions[left])
        for column in missing:
            centers[column] = centers[left] + step * (positions[column] - positions[left])
    return centers, missing


def data_column_centers(words: list[Word], offset: int) -> dict[int, float]:
    centers, _ = header_column_centers(words, offset)
    if offset != 0:
        return centers
    population = [
        word for word in words
        if 155 <= word.y <= 205 and word.text == "3"
    ]
    if not population:
        raise ValueError("missing Atlas district-population column 3 header")
    return {3: min(population, key=lambda word: word.y).x, **centers}


def column_bounds(centers: dict[int, float]) -> dict[int, tuple[float, float]]:
    columns = sorted(centers)
    xs = [centers[column] for column in columns]
    bounds: dict[int, tuple[float, float]] = {}
    for i, column in enumerate(columns):
        left = (xs[i - 1] + xs[i]) / 2 if i else xs[i] - (xs[i + 1] - xs[i]) / 2
        right = (xs[i] + xs[i + 1]) / 2 if i < len(xs) - 1 else xs[i] + (xs[i] - xs[i - 1]) / 2
        bounds[column] = (left, right)
    return bounds


def _label_region(offset: int) -> tuple[float, float]:
    return (88.0, 180.0) if offset % 2 == 0 else (640.0, 735.0)


def _serial_region(offset: int) -> tuple[float, float]:
    return (55.0, 90.0) if offset % 2 == 0 else (735.0, 775.0)


def group_label_rows(words: list[Word], offset: int) -> list[tuple[float, str]]:
    x_lo, x_hi = _label_region(offset)
    candidates = sorted(
        (word.y, word.x, word.text)
        for word in words
        if ROW_Y_MIN <= word.y <= ROW_Y_MAX and x_lo <= word.x < x_hi
    )
    groups: list[list[tuple[float, float, str]]] = []
    for candidate in candidates:
        if not groups:
            groups.append([candidate])
            continue
        mean_y = sum(item[0] for item in groups[-1]) / len(groups[-1])
        if candidate[0] - mean_y > LABEL_Y_CLUSTER:
            groups.append([candidate])
        else:
            groups[-1].append(candidate)
    return [
        (
            sum(item[0] for item in group) / len(group),
            " ".join(item[2] for item in sorted(group, key=lambda item: item[1])).strip(),
        )
        for group in groups
        if any(item[2] for item in group)
    ]


def words_in_box(words: list[Word], y: float, x_lo: float, x_hi: float) -> str:
    hits = sorted(
        (word.x, word.text)
        for word in words
        if x_lo <= word.x < x_hi and abs(word.y - y) <= ROW_Y_TOLERANCE
    )
    return " ".join(text for _, text in hits).strip()


def parse_serial(raw: str) -> int | None:
    match = re.search(r"(?<!\d)(\d{1,2})(?:[.:])?(?!\d)", raw.strip())
    return int(match.group(1)) if match else None


def parse_count(raw: str) -> tuple[int | None, str]:
    value = raw.strip()
    if not value:
        return None, "blank"
    if re.fullmatch(r"(?:\(\s*\)|[Oo])", value):
        return 0, "normalized_ocr_zero"
    if re.fullmatch(r"[\d\s,.]+", value):
        digits = re.sub(r"\D", "", value)
        return (int(digits), "parsed") if digits else (None, "unparsed")
    return None, "unparsed"


def page_layout_row(page: int, block: int, offset: int, words: list[Word]) -> dict[str, object]:
    centers, inferred = header_column_centers(words, offset)
    expected = expected_columns(offset)
    return {
        "block": block,
        "page": page,
        "page_offset": offset,
        "first_language_column": expected[0],
        "last_language_column": expected[-1],
        "expected_language_columns": len(expected),
        "observed_language_columns": len(expected) - len(inferred),
        "inferred_header_columns": ";".join(map(str, inferred)),
        "row_labels_detected": len(group_label_rows(words, offset)),
        "layout_status": "verified" if tuple(sorted(centers)) == expected else "column_header_mismatch",
    }


def extract_page_cells(page: int, block: int, offset: int, words: list[Word]) -> list[dict[str, object]]:
    centers = data_column_centers(words, offset)
    bounds = column_bounds(centers)
    serial_lo, serial_hi = _serial_region(offset)
    rows = group_label_rows(words, offset)
    out: list[dict[str, object]] = []
    for row_sequence, (y, label) in enumerate(rows, start=1):
        serial_raw = words_in_box(words, y, serial_lo, serial_hi)
        serial = parse_serial(serial_raw)
        for column in sorted(centers):
            raw = words_in_box(words, y, *bounds[column])
            parsed, parse_status = parse_count(raw)
            out.append({
                "block": block,
                "page": page,
                "page_offset": offset,
                "row_sequence": row_sequence,
                "row_y": round(y, 3),
                "row_label_raw": label,
                "serial_raw": serial_raw,
                "serial_candidate": "" if serial is None else serial,
                "atlas_column": column,
                "cell_kind": "district_population" if column == 3 else "language_speakers",
                "raw_value": raw,
                "speaker_count_candidate": "" if parsed is None else parsed,
                "parse_status": parse_status,
            })
    return out


def inspect_and_extract(pdf: Path, pdftotext: str) -> tuple[list[dict[str, object]], list[dict[str, object]]]:
    layout: list[dict[str, object]] = []
    cells: list[dict[str, object]] = []
    for block, start_page in enumerate(BLOCK_START_PAGES, start=1):
        for offset in range(8):
            page = start_page + offset
            words = extract_bbox_page(pdf, page, pdftotext)
            layout.append(page_layout_row(page, block, offset, words))
            cells.extend(extract_page_cells(page, block, offset, words))
    return layout, cells


def review_rows(layout: list[dict[str, object]], cells: list[dict[str, object]]) -> list[dict[str, object]]:
    row_counts: dict[int, list[int]] = {}
    for row in layout:
        row_counts.setdefault(int(row["block"]), []).append(int(row["row_labels_detected"]))
    expected_rows = {
        block: max(set(counts), key=lambda count: (counts.count(count), count))
        for block, counts in row_counts.items()
    }
    out: list[dict[str, object]] = []
    for row in layout:
        inferred_header = bool(str(row.get("inferred_header_columns", "")).strip())
        if (
            row["layout_status"] != "verified"
            or inferred_header
            or int(row["row_labels_detected"]) != expected_rows[int(row["block"])]
        ):
            out.append({
                "review_type": "page_row_contract",
                "block": row["block"],
                "page": row["page"],
                "row_sequence": "",
                "atlas_column": "",
                "cell_kind": "",
                "row_label_raw": "",
                "raw_value": "",
                "detail": (
                    f"layout={row['layout_status']}; inferred_header_columns="
                    f"{row.get('inferred_header_columns', '')}; rows={row['row_labels_detected']}; "
                    f"block_mode_rows={expected_rows[int(row['block'])]}"
                ),
            })
    for cell in cells:
        if cell["parse_status"] in {"blank", "unparsed"}:
            out.append({
                "review_type": "blank_cell" if cell["parse_status"] == "blank" else "unparsed_cell",
                "block": cell["block"],
                "page": cell["page"],
                "row_sequence": cell["row_sequence"],
                "atlas_column": cell["atlas_column"],
                "cell_kind": cell["cell_kind"],
                "row_label_raw": cell["row_label_raw"],
                "raw_value": cell["raw_value"],
                "detail": (
                    "no positioned text was recovered for this cell"
                    if cell["parse_status"] == "blank"
                    else "positioned text is not a conservative integer parse"
                ),
            })
    return out


def write_csv(path: Path, rows: list[dict[str, object]], fields: list[str] | None = None) -> None:
    if not rows and fields is None:
        raise ValueError("fields are required when writing an empty CSV")
    path.parent.mkdir(parents=True, exist_ok=True)
    columns = fields or list(rows[0])
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=columns)
        writer.writeheader()
        writer.writerows(rows)
    temporary.replace(path)


def self_test() -> None:
    assert BLOCK_START_PAGES == (205, 213, 221, 229, 237, 245, 253)
    assert tuple(v for i in range(8) for v in expected_columns(i)) == tuple(range(4, 118))
    assert sum(len(expected_columns(i)) for i in range(8)) == 114
    assert parse_count("2,134.680") == (2134680, "parsed")
    assert parse_count("40 ,67 3,814") == (40673814, "parsed")
    assert parse_count("( )") == (0, "normalized_ocr_zero")
    assert parse_count("O") == (0, "normalized_ocr_zero")
    assert parse_count("1'5") == (None, "unparsed")
    synthetic = [
        Word("15", 80, 90, 180, 188), Word("16", 120, 130, 180, 188),
        Word("DISTRICT", 645, 690, 240, 248), Word("1", 745, 750, 240, 248),
    ]
    assert parse_serial(words_in_box(synthetic, 244, 735, 775)) == 1
    assert group_label_rows(synthetic, 1) == [(244.0, "DISTRICT")]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pdf", type=Path)
    parser.add_argument("--candidate-output", type=Path)
    parser.add_argument("--review-output", type=Path)
    parser.add_argument("--layout-output", type=Path)
    parser.add_argument("--pdftotext", default="pdftotext")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args(argv)
    if args.self_test:
        self_test()
        print("Language Atlas 1991 extractor self-test: PASS")
        return 0
    if args.pdf is None or args.candidate_output is None or args.review_output is None:
        parser.error("--pdf, --candidate-output, and --review-output are required unless --self-test is used")
    executable = shutil.which(args.pdftotext)
    if executable is None:
        raise SystemExit("pdftotext is required to extract the Language Atlas")
    layout, cells = inspect_and_extract(args.pdf, executable)
    bad_layout = [row for row in layout if row["layout_status"] != "verified"]
    if bad_layout:
        pages = ", ".join(str(row["page"]) for row in bad_layout)
        raise SystemExit(f"Language Atlas column-layout check failed on PDF page(s): {pages}")
    review = review_rows(layout, cells)
    write_csv(args.candidate_output, cells)
    write_csv(
        args.review_output,
        review,
        fields=["review_type", "block", "page", "row_sequence", "atlas_column", "cell_kind", "row_label_raw", "raw_value", "detail"],
    )
    if args.layout_output is not None:
        write_csv(args.layout_output, layout)
    unparsed = sum(row["review_type"] == "unparsed_cell" for row in review)
    blank = sum(row["review_type"] == "blank_cell" for row in review)
    page_contracts = sum(row["review_type"] == "page_row_contract" for row in review)
    print(
        f"Language Atlas candidate extraction: {len(cells)} positioned cells; "
        f"{unparsed} unparsed cells; {blank} blank cells; "
        f"{page_contracts} page-row review flags"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
