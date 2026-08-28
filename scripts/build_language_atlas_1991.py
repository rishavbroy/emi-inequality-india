#!/usr/bin/env python3
"""Extract auditable candidate cells from Language Atlas of India 1991 Annexure IV.

This is a maintainer-side source-construction tool, not a targets dependency.
It uses Poppler's positioned text output to verify the fixed 56-page Annexure-IV
layout and writes raw cell text, conservative integer parses, and an explicit
review queue. Candidate values are not production-ready until district rows and
all flagged cells have been reviewed and validated against Census/SHRUG totals.
For population-validated districts the tool can also emit the first 11 language
columns from each block's population page, preserving unresolved cells for review.
"""

from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
import zipfile
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

PCA91_DISTRICT_MEMBER = "pc91_pca_clean_pc91dist.csv"
POPULATION_RELATIVE_TOLERANCE = 0.01


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


def parse_leading_serial(raw_label: str) -> int | None:
    match = re.match(r"^\s*(\d{1,2})(?:[.]|\s)+", raw_label)
    return int(match.group(1)) if match else None


def read_state_crosswalk(path: Path) -> dict[str, dict[str, str]]:
    with path.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle))
    required = {"atlas_label_raw", "state_code_1991", "state_name_1991"}
    if not rows or not required.issubset(rows[0]):
        raise ValueError("Language Atlas state crosswalk is missing required columns")
    labels = [row["atlas_label_raw"].strip() for row in rows]
    codes = [row["state_code_1991"].strip().zfill(2) for row in rows]
    if any(not label for label in labels) or len(labels) != len(set(labels)):
        raise ValueError("Language Atlas state crosswalk labels must be unique and nonblank")
    if len(codes) != len(set(codes)):
        raise ValueError("Language Atlas state crosswalk state codes must be unique")
    return {
        row["atlas_label_raw"].strip(): {
            "state_code_1991": row["state_code_1991"].strip().zfill(2),
            "state_name_1991": row["state_name_1991"].strip(),
        }
        for row in rows
    }


def read_pca91_districts(path: Path) -> dict[tuple[str, str], int]:
    with zipfile.ZipFile(path) as archive:
        names = archive.namelist()
        if PCA91_DISTRICT_MEMBER not in names:
            raise ValueError(f"SHRUG PCA91 archive is missing {PCA91_DISTRICT_MEMBER}")
        with archive.open(PCA91_DISTRICT_MEMBER) as raw:
            text = (line.decode("utf-8-sig") for line in raw)
            rows = list(csv.DictReader(text))
    required = {"pc91_state_id", "pc91_district_id", "pc91_pca_tot_p"}
    if not rows or not required.issubset(rows[0]):
        raise ValueError("SHRUG PCA91 district table is missing required columns")
    out: dict[tuple[str, str], int] = {}
    for row in rows:
        key = (row["pc91_state_id"].strip().zfill(2), row["pc91_district_id"].strip().zfill(2))
        if key in out:
            raise ValueError(f"Duplicate SHRUG PCA91 district key: {key[0]}-{key[1]}")
        population = float(row["pc91_pca_tot_p"])
        if not population.is_integer() or population < 0:
            raise ValueError(f"Invalid SHRUG PCA91 district population for {key[0]}-{key[1]}")
        out[key] = int(population)
    if len(out) != 452:
        raise ValueError(f"Expected 452 SHRUG PCA91 districts, found {len(out)}")
    return out


def atlas_population_cells(cells: list[dict[str, object]]) -> list[dict[str, object]]:
    return [
        cell for cell in cells
        if cell["cell_kind"] == "district_population" and int(cell["page_offset"]) == 0
    ]


def merge_population_continuations(
    cells: list[dict[str, object]],
    state_crosswalk: dict[str, dict[str, str]],
) -> list[dict[str, object]]:
    rows = [dict(cell) for cell in atlas_population_cells(cells)]
    out: list[dict[str, object]] = []
    seen_state_codes: set[str] = set()
    for row in rows:
        label = str(row["row_label_raw"]).strip()
        mapped_state = state_crosswalk.get(label)
        is_new_state_heading = (
            mapped_state is not None
            and mapped_state["state_code_1991"] not in seen_state_codes
        )
        if is_new_state_heading:
            seen_state_codes.add(mapped_state["state_code_1991"])
        serial = int(row["serial_candidate"]) if str(row["serial_candidate"]).strip() else parse_leading_serial(label)
        can_continue = (
            not is_new_state_heading
            and label != "INDIA"
            and serial is None
            and bool(out)
            and str(out[-1].get("serial_candidate", "")).strip() != ""
            and str(out[-1].get("speaker_count_candidate", "")).strip() == ""
            and str(row.get("speaker_count_candidate", "")).strip() != ""
        )
        if can_continue:
            previous = out[-1]
            previous["row_label_raw"] = f"{previous['row_label_raw']} {label}".strip()
            previous["raw_value"] = row["raw_value"]
            previous["speaker_count_candidate"] = row["speaker_count_candidate"]
            previous["parse_status"] = row["parse_status"]
            previous["row_sequence_end"] = row["row_sequence"]
            previous["row_merge_status"] = "continued_label"
            continue
        row["row_sequence_end"] = row["row_sequence"]
        row["row_merge_status"] = "single_row"
        out.append(row)
    return out


def cross_page_serial_evidence(cells: list[dict[str, object]]) -> dict[tuple[int, str], tuple[int, ...]]:
    """Collect repeated printed district-serial evidence for exact Atlas row labels.

    Annexure IV repeats the district rows on each of its eight language pages.
    Exact label repetition can therefore recover a dropped serial without fuzzy
    district matching. Conflicting repeated serials remain conflicts.
    """
    observed: dict[tuple[int, str], set[int]] = {}
    unique_rows = {
        (int(cell["block"]), int(cell["page"]), int(cell["row_sequence"]),
         str(cell["row_label_raw"]).strip(), str(cell["serial_candidate"]).strip())
        for cell in cells
    }
    for block, _page, _sequence, label, serial_raw in unique_rows:
        if not label or label == "INDIA":
            continue
        serial = int(serial_raw) if serial_raw else parse_leading_serial(label)
        if serial is not None:
            observed.setdefault((block, label), set()).add(serial)
    return {key: tuple(sorted(values)) for key, values in observed.items()}


def reconcile_district_serial(
    cell: dict[str, object],
    evidence: dict[tuple[int, str], tuple[int, ...]],
) -> tuple[int | None, str, str]:
    """Resolve one Atlas district serial using direct and exact-label evidence."""
    label = str(cell["row_label_raw"]).strip()
    serial_column = int(cell["serial_candidate"]) if str(cell["serial_candidate"]).strip() else None
    serial_label = parse_leading_serial(label)
    repeated = evidence.get((int(cell["block"]), label), ())
    repeated_text = ";".join(map(str, repeated))

    if serial_column is not None and serial_label is not None and serial_column != serial_label:
        return None, "direct_conflict", repeated_text
    direct = serial_column if serial_column is not None else serial_label
    direct_source = "serial_column" if serial_column is not None else "label_prefix"

    if direct is not None:
        # Direct page-1 serial evidence remains primary. Repeated pages are used
        # to recover a missing serial, not to overturn a directly printed code
        # because isolated OCR serial errors occur on continuation pages.
        return direct, direct_source, repeated_text
    if len(repeated) == 1:
        return repeated[0], "cross_page_exact_label", repeated_text
    if len(repeated) > 1:
        return None, "cross_page_conflict", repeated_text
    return None, "missing", repeated_text


def build_district_population_validation(
    cells: list[dict[str, object]],
    state_crosswalk: dict[str, dict[str, str]],
    pca91: dict[tuple[str, str], int],
) -> list[dict[str, object]]:
    rows = merge_population_continuations(cells, state_crosswalk)
    serial_evidence = cross_page_serial_evidence(cells)
    seen_state_codes: set[str] = set()
    current_state: dict[str, str] | None = None
    candidates: list[dict[str, object]] = []
    for cell in rows:
        label = str(cell["row_label_raw"]).strip()
        if label == "INDIA":
            continue
        mapped_state = state_crosswalk.get(label)
        if mapped_state is not None and mapped_state["state_code_1991"] not in seen_state_codes:
            current_state = mapped_state
            seen_state_codes.add(mapped_state["state_code_1991"])
            continue

        serial, serial_source, cross_page_serials = reconcile_district_serial(cell, serial_evidence)

        state_code = current_state["state_code_1991"] if current_state else ""
        state_name = current_state["state_name_1991"] if current_state else ""
        district_code = f"{serial:02d}" if serial is not None else ""
        pca_population = pca91.get((state_code, district_code)) if state_code and district_code else None
        atlas_population = (
            int(cell["speaker_count_candidate"])
            if str(cell["speaker_count_candidate"]).strip()
            else None
        )
        if atlas_population is not None and pca_population is not None and pca_population > 0:
            abs_diff = abs(atlas_population - pca_population)
            rel_diff = abs_diff / pca_population
        else:
            abs_diff = None
            rel_diff = None
        candidates.append({
            "block": cell["block"],
            "page": cell["page"],
            "row_sequence": cell["row_sequence"],
            "row_sequence_end": cell["row_sequence_end"],
            "row_merge_status": cell["row_merge_status"],
            "row_label_raw": label,
            "state_code_1991": state_code,
            "state_name_1991": state_name,
            "district_code_1991": district_code,
            "serial_source": serial_source,
            "cross_page_serials": cross_page_serials,
            "atlas_population_raw": cell["raw_value"],
            "atlas_population_candidate": "" if atlas_population is None else atlas_population,
            "pca91_population": "" if pca_population is None else pca_population,
            "population_abs_diff": "" if abs_diff is None else abs_diff,
            "population_relative_diff": "" if rel_diff is None else round(rel_diff, 8),
            "identity_status": "",
            "population_status": "",
            "promotion_status": "review_required",
        })

    expected_state_codes = {row["state_code_1991"] for row in state_crosswalk.values()}
    if seen_state_codes != expected_state_codes:
        missing = sorted(expected_state_codes - seen_state_codes)
        extra = sorted(seen_state_codes - expected_state_codes)
        raise ValueError(
            "Language Atlas state-heading coverage mismatch; "
            f"missing={missing}; extra={extra}"
        )
    pca_state_codes = {state_code for state_code, _ in pca91}
    if pca_state_codes != expected_state_codes:
        raise ValueError("Language Atlas state crosswalk and SHRUG PCA91 state codes disagree")

    key_counts: dict[tuple[str, str], int] = {}
    for row in candidates:
        key = (str(row["state_code_1991"]), str(row["district_code_1991"]))
        if all(key):
            key_counts[key] = key_counts.get(key, 0) + 1

    for row in candidates:
        key = (str(row["state_code_1991"]), str(row["district_code_1991"]))
        if not row["state_code_1991"]:
            identity_status = "missing_state_context"
        elif row["serial_source"] in {"direct_conflict", "cross_page_conflict"}:
            identity_status = "serial_conflict"
        elif not row["district_code_1991"]:
            identity_status = "missing_district_serial"
        elif key_counts.get(key, 0) > 1:
            identity_status = "duplicate_district_code"
        elif key not in pca91:
            identity_status = "missing_pca_district"
        else:
            identity_status = "candidate_official_code"
        row["identity_status"] = identity_status

        if row["atlas_population_candidate"] == "":
            population_status = "atlas_population_unparsed"
        elif row["pca91_population"] == "":
            population_status = "pca_population_missing"
        elif float(row["population_relative_diff"]) <= POPULATION_RELATIVE_TOLERANCE:
            population_status = "within_1pct"
        else:
            population_status = "over_1pct"
        row["population_status"] = population_status
        if identity_status == "candidate_official_code" and population_status == "within_1pct":
            row["promotion_status"] = "population_validated_candidate"
    return candidates


def district_population_review_rows(
    rows: list[dict[str, object]],
    pca91: dict[tuple[str, str], int],
) -> list[dict[str, object]]:
    review = [dict(row) for row in rows if row["promotion_status"] != "population_validated_candidate"]
    observed = {
        (str(row["state_code_1991"]), str(row["district_code_1991"]))
        for row in rows
        if row["identity_status"] == "candidate_official_code"
    }
    template = list(rows[0])
    for state_code, district_code in sorted(set(pca91) - observed):
        row = {field: "" for field in template}
        row.update({
            "state_code_1991": state_code,
            "district_code_1991": district_code,
            "pca91_population": pca91[(state_code, district_code)],
            "identity_status": "missing_atlas_district",
            "population_status": "atlas_population_missing",
            "promotion_status": "review_required",
        })
        review.append(row)
    return review

def build_validated_page0_language_cells(
    cells: list[dict[str, object]],
    district_validation: list[dict[str, object]],
) -> list[dict[str, object]]:
    """Bind page-0 language cells to independently population-validated districts."""
    page0 = {}
    for cell in cells:
        if cell["cell_kind"] != "language_speakers" or int(cell["page_offset"]) != 0:
            continue
        key = (int(cell["block"]), int(cell["page"]), int(cell["row_sequence"]), int(cell["atlas_column"]))
        if key in page0:
            raise ValueError(f"Duplicate Atlas page-0 language cell at {key}")
        page0[key] = cell

    out: list[dict[str, object]] = []
    validated = [row for row in district_validation if row["promotion_status"] == "population_validated_candidate"]
    for district in validated:
        block = int(district["block"])
        page = int(district["page"])
        row_sequence = int(district["row_sequence_end"])
        for atlas_column in expected_columns(0):
            key = (block, page, row_sequence, atlas_column)
            if key not in page0:
                raise ValueError(f"Missing Atlas page-0 language cell at {key}")
            cell = page0[key]
            parse_status = str(cell["parse_status"])
            out.append({
                "state_code_1991": district["state_code_1991"],
                "district_code_1991": district["district_code_1991"],
                "state_name_1991": district["state_name_1991"],
                "row_label_raw": district["row_label_raw"],
                "atlas_population_candidate": district["atlas_population_candidate"],
                "pca91_population": district["pca91_population"],
                "population_relative_diff": district["population_relative_diff"],
                "block": block,
                "page": page,
                "row_sequence": row_sequence,
                "atlas_column": atlas_column,
                "raw_value": cell["raw_value"],
                "speaker_count_candidate": cell["speaker_count_candidate"],
                "parse_status": parse_status,
                "language_cell_status": (
                    "candidate_value"
                    if parse_status in {"parsed", "normalized_ocr_zero"}
                    else "review_required"
                ),
            })

    expected_n = len(validated) * len(expected_columns(0))
    if len(out) != expected_n:
        raise ValueError(f"Expected {expected_n} validated page-0 language cells, found {len(out)}")
    keys = [(row["state_code_1991"], row["district_code_1991"], row["atlas_column"]) for row in out]
    if len(keys) != len(set(keys)):
        raise ValueError("Validated Atlas page-0 language cells are not unique by district and column")
    return out


def page0_language_review_rows(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    return [dict(row) for row in rows if row["language_cell_status"] == "review_required"]


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
    assert parse_leading_serial("14. DISTRICT") == 14
    assert parse_leading_serial("DISTRICT") is None
    toy_cells = [
        {"cell_kind": "district_population", "page_offset": 0, "block": 1, "page": 205,
         "row_sequence": 1, "row_label_raw": "STATE", "serial_candidate": "",
         "raw_value": "100", "speaker_count_candidate": 100},
        {"cell_kind": "district_population", "page_offset": 0, "block": 1, "page": 205,
         "row_sequence": 2, "row_label_raw": "DISTRICT", "serial_candidate": 1,
         "raw_value": "50", "speaker_count_candidate": 50},
    ]
    validated = build_district_population_validation(
        toy_cells,
        {"STATE": {"state_code_1991": "02", "state_name_1991": "STATE"}},
        {("02", "01"): 50},
    )
    assert validated[0]["promotion_status"] == "population_validated_candidate"
    toy_language_cells = list(toy_cells) + [
        {
            "cell_kind": "language_speakers", "page_offset": 0, "block": 1, "page": 205,
            "row_sequence": 2, "atlas_column": column, "raw_value": str(column),
            "speaker_count_candidate": column, "parse_status": "parsed",
        }
        for column in expected_columns(0)
    ]
    promoted = build_validated_page0_language_cells(toy_language_cells, validated)
    assert len(promoted) == 11
    assert all(row["language_cell_status"] == "candidate_value" for row in promoted)
    serial_cells = [
        {"block": 1, "page": 205, "row_sequence": 2, "row_label_raw": "DHUBRI", "serial_candidate": ""},
        {"block": 1, "page": 207, "row_sequence": 2, "row_label_raw": "DHUBRI", "serial_candidate": 1},
        {"block": 2, "page": 213, "row_sequence": 4, "row_label_raw": "LOHARDAGA", "serial_candidate": 3},
        {"block": 2, "page": 215, "row_sequence": 4, "row_label_raw": "LOHARDAGA", "serial_candidate": 36},
    ]
    serial_evidence = cross_page_serial_evidence(serial_cells)
    recovered = reconcile_district_serial(serial_cells[0], serial_evidence)
    assert recovered == (1, "cross_page_exact_label", "1")
    conflicted = reconcile_district_serial(
        {"block": 2, "row_label_raw": "LOHARDAGA", "serial_candidate": ""},
        serial_evidence,
    )
    assert conflicted == (None, "cross_page_conflict", "3;36")

    continued = merge_population_continuations(
        [
            {"cell_kind": "district_population", "page_offset": 0, "block": 1, "page": 205, "row_sequence": 1,
             "row_label_raw": "STATE", "serial_candidate": "", "raw_value": "100", "speaker_count_candidate": 100, "parse_status": "parsed"},
            {"cell_kind": "district_population", "page_offset": 0, "block": 1, "page": 205, "row_sequence": 2,
             "row_label_raw": "NORTH TWENTY FOUR", "serial_candidate": 8, "raw_value": "", "speaker_count_candidate": "", "parse_status": "blank"},
            {"cell_kind": "district_population", "page_offset": 0, "block": 1, "page": 205, "row_sequence": 3,
             "row_label_raw": "PARGANAS", "serial_candidate": "", "raw_value": "7,281,881", "speaker_count_candidate": 7281881, "parse_status": "parsed"},
        ],
        {"STATE": {"state_code_1991": "26", "state_name_1991": "STATE"}},
    )
    assert continued[-1]["row_label_raw"] == "NORTH TWENTY FOUR PARGANAS"
    assert continued[-1]["speaker_count_candidate"] == 7281881


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pdf", type=Path)
    parser.add_argument("--candidate-output", type=Path)
    parser.add_argument("--review-output", type=Path)
    parser.add_argument("--layout-output", type=Path)
    parser.add_argument("--pca-zip", type=Path)
    parser.add_argument("--state-crosswalk", type=Path)
    parser.add_argument("--district-output", type=Path)
    parser.add_argument("--population-review-output", type=Path)
    parser.add_argument("--page0-language-output", type=Path)
    parser.add_argument("--page0-language-review-output", type=Path)
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
    district_validation = None
    language_cells = None
    district_options = (
        args.pca_zip, args.state_crosswalk, args.district_output, args.population_review_output,
        args.page0_language_output, args.page0_language_review_output,
    )
    if any(value is not None for value in district_options):
        if args.pca_zip is None or args.state_crosswalk is None or args.district_output is None or args.population_review_output is None:
            parser.error("--pca-zip, --state-crosswalk, --district-output, and --population-review-output must be supplied together")
        state_crosswalk = read_state_crosswalk(args.state_crosswalk)
        pca91 = read_pca91_districts(args.pca_zip)
        district_validation = build_district_population_validation(cells, state_crosswalk, pca91)
        write_csv(args.district_output, district_validation)
        write_csv(
            args.population_review_output,
            district_population_review_rows(district_validation, pca91),
            fields=list(district_validation[0]),
        )
        if (args.page0_language_output is None) != (args.page0_language_review_output is None):
            parser.error("--page0-language-output and --page0-language-review-output must be supplied together")
        if args.page0_language_output is not None:
            language_cells = build_validated_page0_language_cells(cells, district_validation)
            write_csv(args.page0_language_output, language_cells)
            write_csv(
                args.page0_language_review_output,
                page0_language_review_rows(language_cells),
                fields=list(language_cells[0]),
            )
    unparsed = sum(row["review_type"] == "unparsed_cell" for row in review)
    blank = sum(row["review_type"] == "blank_cell" for row in review)
    page_contracts = sum(row["review_type"] == "page_row_contract" for row in review)
    message = (
        f"Language Atlas candidate extraction: {len(cells)} positioned cells; "
        f"{unparsed} unparsed cells; {blank} blank cells; "
        f"{page_contracts} page-row review flags"
    )
    if district_validation is not None:
        validated = sum(row["promotion_status"] == "population_validated_candidate" for row in district_validation)
        message += f"; {validated}/{len(district_validation)} district rows population-validated"
    if language_cells is not None:
        language_review = sum(row["language_cell_status"] == "review_required" for row in language_cells)
        message += f"; {len(language_cells) - language_review}/{len(language_cells)} page-0 language cells parsed"
    print(message)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
