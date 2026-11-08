"""Audit raw files in the approved data architecture.

This script intentionally performs lightweight checks only. It does not modify
raw files.
"""

from __future__ import annotations

import csv
from datetime import datetime

from config import METADATA_DIR, RAW_FILES


def main() -> None:
    rows = []
    for key, path in RAW_FILES.items():
        exists = path.exists()
        rows.append(
            {
                "key": key,
                "path": path.as_posix(),
                "exists": exists,
                "bytes": path.stat().st_size if exists else "",
                "last_modified": datetime.fromtimestamp(path.stat().st_mtime).isoformat(
                    timespec="seconds"
                )
                if exists
                else "",
            }
        )

    METADATA_DIR.mkdir(parents=True, exist_ok=True)
    out = METADATA_DIR / "raw_audit.csv"
    with out.open("w", newline="", encoding="utf-8-sig") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)
    print(f"Wrote {out}")


if __name__ == "__main__":
    main()

