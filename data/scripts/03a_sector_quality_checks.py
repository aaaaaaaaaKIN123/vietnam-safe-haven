"""Kiểm tra chất lượng dữ liệu cho phần mở rộng chỉ số ngành HOSE."""

from __future__ import annotations

import sys

import pandas as pd

from config import METADATA_DIR, SECTOR_INPUT_DIR


SOFT_OUTLIER = 10.0
HARD_OUTLIER = 30.0


if hasattr(sys.stdout, "reconfigure"):
    sys.stdout.reconfigure(encoding="utf-8", errors="replace")
if hasattr(sys.stderr, "reconfigure"):
    sys.stderr.reconfigure(encoding="utf-8", errors="replace")


def summarize_returns(df: pd.DataFrame, sector_cols: list[str]) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    n_rows = len(df)
    for col in sector_cols:
        series = pd.to_numeric(df[col], errors="coerce")
        missing = int(series.isna().sum())
        rows.append(
            {
                "variable": col,
                "valid_obs": n_rows - missing,
                "missing": missing,
                "missing_pct": 100 * missing / n_rows if n_rows else 0,
                "mean": series.mean(),
                "std": series.std(),
                "min": series.min(),
                "max": series.max(),
                "soft_outliers_abs_gt_10": int((series.abs() > SOFT_OUTLIER).sum()),
                "hard_outliers_abs_gt_30": int((series.abs() > HARD_OUTLIER).sum()),
            }
        )
    return rows


def to_markdown_rows(rows: list[dict[str, object]]) -> str:
    lines = []
    for row in rows:
        lines.append(
            "| `{variable}` | {valid_obs} | {missing} ({missing_pct:.2f}%) | "
            "{mean:.4f}% | {std:.4f}% | [{min:.2f}%, {max:.2f}%] | {soft} | {hard} |".format(
                variable=row["variable"],
                valid_obs=row["valid_obs"],
                missing=row["missing"],
                missing_pct=row["missing_pct"],
                mean=row["mean"],
                std=row["std"],
                min=row["min"],
                max=row["max"],
                soft=row["soft_outliers_abs_gt_10"],
                hard=row["hard_outliers_abs_gt_30"],
            )
        )
    return "\n".join(lines)


def main() -> None:
    print("=" * 72)
    print("BẮT ĐẦU: KIỂM TRA CHẤT LƯỢNG DỮ LIỆU NGÀNH")
    print("=" * 72)

    daily_path = SECTOR_INPUT_DIR / "final_sector_daily_returns.csv"
    weekly_path = SECTOR_INPUT_DIR / "final_sector_weekly_returns.csv"
    fill_audit_path = SECTOR_INPUT_DIR / "sector_fill_audit.csv"

    if not daily_path.exists() or not weekly_path.exists():
        print(f"  [Lỗi] Chưa có dữ liệu sector tại {SECTOR_INPUT_DIR}")
        print("  Hãy chạy `python data/scripts/02a_build_sector_input.py` trước.")
        return

    daily = pd.read_csv(daily_path)
    weekly = pd.read_csv(weekly_path)
    daily["date"] = pd.to_datetime(daily["date"])
    weekly["date"] = pd.to_datetime(weekly["date"])

    sector_cols = [c for c in daily.columns if c.startswith("r_vn") and c != "r_vnindex"]
    daily_rows = summarize_returns(daily, sector_cols)
    weekly_rows = summarize_returns(weekly, sector_cols)

    fill_summary = pd.DataFrame(columns=["sector", "filled_days", "max_fill_run_length"])
    if fill_audit_path.exists():
        fill_audit = pd.read_csv(fill_audit_path)
        if not fill_audit.empty:
            fill_summary = (
                fill_audit.groupby("sector")
                .agg(filled_days=("date", "count"), max_fill_run_length=("fill_run_length", "max"))
                .reset_index()
            )

    duplicate_daily = int(daily["date"].duplicated().sum())
    duplicate_weekly = int(weekly["date"].duplicated().sum())
    hard_fail_count = sum(row["hard_outliers_abs_gt_30"] for row in daily_rows)
    missing_count = sum(row["missing"] for row in daily_rows)
    fill_count = int(fill_summary["filled_days"].sum()) if not fill_summary.empty else 0

    fill_table = (
        "| Ngành | Số ngày forward-fill | Chuỗi fill dài nhất |\n"
        "|---|---:|---:|\n"
    )
    if fill_summary.empty:
        fill_table += "| Không có | 0 | 0 |"
    else:
        fill_table += "\n".join(
            f"| `{row.sector}` | {int(row.filled_days)} | {int(row.max_fill_run_length)} |"
            for row in fill_summary.itertuples(index=False)
        )

    verdict = "PASS"
    warnings = []
    if duplicate_daily or duplicate_weekly:
        verdict = "REVIEW"
        warnings.append("Có ngày bị trùng lặp trong daily hoặc weekly returns.")
    if missing_count > 0:
        verdict = "REVIEW"
        warnings.append("Có missing values trong lợi suất ngành sau ngày bắt đầu chung.")
    if hard_fail_count > 0:
        verdict = "FAIL"
        warnings.append("Có lợi suất ngành vượt ngưỡng tuyệt đối 30%, cần kiểm tra dữ liệu nguồn.")
    if fill_count > 0:
        warnings.append("Có forward-fill trong sector levels; cần xem sector_fill_audit.csv trước khi mô hình hóa.")
    if not warnings:
        warnings.append("Không phát hiện cảnh báo trọng yếu.")

    report = f"""# Báo Cáo Chất Lượng Dữ Liệu Chỉ Số Ngành HOSE

Báo cáo này kiểm tra bộ dữ liệu ngành dùng cho mô hình GARCH/DCC cấp ngành. Dữ liệu được căn theo lịch giao dịch VNIndex và lưu riêng trong `data/02_model_input/sector_extension/`.

## 1. Tổng Quan

| Tần suất | Số dòng | Ngày bắt đầu | Ngày kết thúc | Trùng lặp ngày |
|---|---:|---|---|---:|
| Daily | {len(daily)} | {daily['date'].min().date()} | {daily['date'].max().date()} | {duplicate_daily} |
| Weekly | {len(weekly)} | {weekly['date'].min().date()} | {weekly['date'].max().date()} | {duplicate_weekly} |

- Master calendar: lịch giao dịch VNIndex.
- Weekly convention: lấy quan sát giao dịch cuối cùng trong tuần có nhãn week-ending Friday.
- Quality verdict: **{verdict}**.

## 2. Thống Kê Lợi Suất Ngành Hằng Ngày

| Biến | Số mẫu hợp lệ | Missing | Trung bình | Độ lệch chuẩn | Khoảng lợi suất | abs(r) > 10% | abs(r) > 30% |
|---|---:|---:|---:|---:|---:|---:|---:|
{to_markdown_rows(daily_rows)}

## 3. Thống Kê Lợi Suất Ngành Hằng Tuần

| Biến | Số mẫu hợp lệ | Missing | Trung bình | Độ lệch chuẩn | Khoảng lợi suất | abs(r) > 10% | abs(r) > 30% |
|---|---:|---:|---:|---:|---:|---:|---:|
{to_markdown_rows(weekly_rows)}

## 4. Audit Forward-Fill

{fill_table}

## 5. Cảnh Báo Và Khuyến Nghị

{chr(10).join(f"- {item}" for item in warnings)}

## 6. Output Đã Kiểm Tra

- `data/02_model_input/sector_extension/final_sector_daily_returns.csv`
- `data/02_model_input/sector_extension/final_sector_weekly_returns.csv`
- `data/02_model_input/sector_extension/sector_fill_audit.csv`
"""

    METADATA_DIR.mkdir(parents=True, exist_ok=True)
    report_path = METADATA_DIR / "sector_model_input_report.md"
    report_path.write_text(report.strip(), encoding="utf-8")

    print(f"  [Thành công] Đã xuất báo cáo: {report_path.relative_to(report_path.parents[1])}")
    print(f"  Verdict: {verdict}")
    print("=" * 72)


if __name__ == "__main__":
    main()
