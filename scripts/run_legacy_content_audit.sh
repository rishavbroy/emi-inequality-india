#!/usr/bin/env bash
set -euo pipefail

echo "=== LEGACY CONTENT AUDIT ==="
python3 -m py_compile scripts/audit_legacy_parity.py
python3 scripts/audit_legacy_parity.py
echo "=== LEGACY CONTENT AUDIT PASSED ==="
