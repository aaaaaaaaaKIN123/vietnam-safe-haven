"""Create Phase 2b scientific color model-input visualizations.

The figures produced here are for presentation and paper drafting only. Missing
values are filled on a visualization copy of the data so time-series lines are
continuous. The official model-input files remain unchanged.
"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MPL_CACHE = ROOT / "results" / ".matplotlib_cache"
MPL_CACHE.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLBACKEND", "Agg")
os.environ.setdefault("MPLCONFIGDIR", str(MPL_CACHE))

import matplotlib
import matplotlib.dates as mdates
from matplotlib.colors import ListedColormap
import numpy as np
import pandas as pd

matplotlib.use("Agg", force=True)
import matplotlib.pyplot as plt

MODEL_INPUT = ROOT / "data" / "02_model_input"
METADATA = ROOT / "data" / "metadata"
FIGURES = ROOT / "results" / "figures" / "data_quality_bw"
TABLES = ROOT / "results" / "tables" / "data_quality"

CORE_PANEL = MODEL_INPUT / "core" / "model_panel_daily.csv"
CORE_RETURNS = MODEL_INPUT / "core" / "final_daily_returns.csv"
CRYPTO_RETURNS = MODEL_INPUT / "crypto_extension" / "final_crypto_daily_returns.csv"
OVERLAP = METADATA / "model_input_overlap_summary.csv"

OKABE_ITO = {
    "black": "#000000",
    "charcoal": "#1F2937",
    "grey": "#999999",
    "light_grey": "#D9D9D9",
    "orange": "#E69F00",
    "sky_blue": "#56B4E9",
    "bluish_green": "#009E73",
    "yellow": "#F0E442",
    "blue": "#0072B2",
    "vermillion": "#D55E00",
    "reddish_purple": "#CC79A7",
}

SERIES_COLORS = {
    "vnindex": OKABE_ITO["black"],
    "r_vnindex": OKABE_ITO["black"],
    "sjc_gold": OKABE_ITO["orange"],
    "r_sjc_gold": OKABE_ITO["orange"],
    "xau_vnd": OKABE_ITO["blue"],
    "r_xau_vnd": OKABE_ITO["blue"],
    "bond_tri_10y": OKABE_ITO["bluish_green"],
    "r_bond_tri": OKABE_ITO["bluish_green"],
    "brent": OKABE_ITO["vermillion"],
    "r_brent": OKABE_ITO["vermillion"],
    "wti": OKABE_ITO["reddish_purple"],
    "r_wti": OKABE_ITO["reddish_purple"],
    "btc": OKABE_ITO["reddish_purple"],
    "r_btc": OKABE_ITO["reddish_purple"],
    "eth": OKABE_ITO["sky_blue"],
    "r_eth": OKABE_ITO["sky_blue"],
    "vix": OKABE_ITO["grey"],
    "d_vix": OKABE_ITO["grey"],
    "policy_rate": OKABE_ITO["blue"],
    "d_policy_rate": OKABE_ITO["blue"],
    "interbank_rate": OKABE_ITO["bluish_green"],
    "d_interbank_rate": OKABE_ITO["bluish_green"],
    "d_vn10y_yield": OKABE_ITO["bluish_green"],
    "d_vn5y_yield": OKABE_ITO["sky_blue"],
}


@dataclass(frozen=True)
class SeriesSpec:
    figure_id: str
    variable: str
    title: str
    ylabel: str
    source: str
    group: str
    symmetric: bool = False
    zero_line: bool = False


@dataclass(frozen=True)
class FigureRecord:
    figure_id: str
    title: str
    png_file: str
    svg_file: str
    figure_type: str
    variables: str


def setup_style() -> None:
    plt.style.use("default")
    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "figure.dpi": 140,
            "savefig.dpi": 300,
            "axes.titlesize": 10,
            "axes.labelsize": 9,
            "xtick.labelsize": 8,
            "ytick.labelsize": 8,
            "legend.fontsize": 8,
            "figure.titlesize": 11,
            "axes.edgecolor": "#111111",
            "axes.labelcolor": "#111111",
            "xtick.color": "#111111",
            "ytick.color": "#111111",
            "axes.spines.top": False,
            "axes.spines.right": False,
            "grid.color": "#D9D9D9",
            "grid.linewidth": 0.55,
            "lines.color": OKABE_ITO["charcoal"],
            "text.color": "#111111",
        }
    )


def ensure_dirs() -> None:
    for sub in ["level_series", "return_series", "missingness", "stress_timeline", "correlation"]:
        (FIGURES / sub).mkdir(parents=True, exist_ok=True)
    TABLES.mkdir(parents=True, exist_ok=True)
    METADATA.mkdir(parents=True, exist_ok=True)


def read_csv_dates(path: Path) -> pd.DataFrame:
    return pd.read_csv(path, parse_dates=["date"])


def sample_window() -> tuple[pd.Timestamp, pd.Timestamp]:
    if OVERLAP.exists():
        overlap = pd.read_csv(OVERLAP)
        row = overlap.loc[overlap["sample_group"] == "core_assets_vnindex_sjc_xau_bond"]
        if not row.empty:
            return pd.Timestamp(row.iloc[0]["first_complete_date"]), pd.Timestamp(row.iloc[0]["last_complete_date"])
    return pd.Timestamp("2010-01-04"), pd.Timestamp("2026-05-27")


def apply_date_axis(ax) -> None:
    ax.xaxis.set_major_locator(mdates.YearLocator(base=2))
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.xaxis.set_minor_locator(mdates.YearLocator(base=1))
    ax.tick_params(axis="x", rotation=0)


def robust_ylim(series: pd.Series, symmetric: bool = False) -> tuple[float, float]:
    s = pd.to_numeric(series, errors="coerce").dropna()
    if s.empty:
        return -1, 1
    low, high = s.quantile([0.005, 0.995])
    if low == high:
        low, high = s.min(), s.max()
    if symmetric:
        bound = max(abs(low), abs(high))
        return -bound * 1.08, bound * 1.08
    pad = (high - low) * 0.08 if high != low else abs(high) * 0.08 + 1
    return low - pad, high + pad


def save_figure(fig, output_base: Path) -> tuple[Path, Path]:
    png = output_base.with_suffix(".png")
    svg = output_base.with_suffix(".svg")
    fig.savefig(png, bbox_inches="tight")
    fig.savefig(svg, bbox_inches="tight")
    plt.close(fig)
    return png, svg


def fill_visualization_gaps(
    df: pd.DataFrame,
    variables: list[str],
    source_name: str,
    step_variables: set[str] | None = None,
) -> tuple[pd.DataFrame, list[dict[str, object]]]:
    """Fill internal missing observations for plotting, without touching model data."""
    step_variables = step_variables or set()
    out = df.sort_values("date").copy()
    audit: list[dict[str, object]] = []

    for var in [v for v in variables if v in out.columns]:
        before = int(out[var].isna().sum())
        first = out[var].first_valid_index()
        last = out[var].last_valid_index()

        method = "none"
        if first is not None and last is not None:
            mask = out.index.to_series().between(first, last)
            if var in step_variables:
                filled = out.loc[mask, var].ffill()
                method = "internal forward-fill; event-driven or step-like series"
            else:
                indexed = out.loc[mask, ["date", var]].set_index("date")[var]
                filled = indexed.interpolate(method="time", limit_area="inside")
                filled.index = out.loc[mask].index
                method = "internal time interpolation for visualization only"
            out.loc[mask, var] = filled

        after = int(out[var].isna().sum())
        audit.append(
            {
                "source": source_name,
                "variable": var,
                "observations": int(len(out)),
                "missing_before_visual_fill": before,
                "filled_for_visualization": before - after,
                "missing_after_visual_fill": after,
                "method": method,
            }
        )

    return out, audit


def plot_single_series(df: pd.DataFrame, spec: SeriesSpec, output_base: Path) -> tuple[Path, Path]:
    plot_df = df.dropna(subset=[spec.variable]).copy()
    fig, ax = plt.subplots(figsize=(6.9, 3.9))
    ax.plot(
        plot_df["date"],
        plot_df[spec.variable],
        color=SERIES_COLORS.get(spec.variable, OKABE_ITO["charcoal"]),
        linewidth=0.95,
        linestyle="-",
    )
    if spec.zero_line:
        ax.axhline(0, color=OKABE_ITO["charcoal"], linewidth=0.7, linestyle="--")
    ax.set_title(spec.title, loc="left", fontweight="bold", pad=8)
    ax.set_ylabel(spec.ylabel)
    ax.set_xlabel("Date")
    ax.set_ylim(*robust_ylim(plot_df[spec.variable], symmetric=spec.symmetric))
    apply_date_axis(ax)
    ax.margins(x=0)
    fig.subplots_adjust(left=0.16, right=0.985, top=0.91, bottom=0.13, hspace=0.08)
    return save_figure(fig, output_base)


def plot_missingness(panel_original: pd.DataFrame, variables: list[str], output_base: Path) -> tuple[Path, Path]:
    variables = [v for v in variables if v in panel_original.columns]
    miss = panel_original.set_index("date")[variables].isna().astype(int).T
    monthly = miss.T.resample("ME").max().T

    fig, ax = plt.subplots(figsize=(6.9, 4.8))
    ax.imshow(monthly.to_numpy(), aspect="auto", interpolation="nearest", cmap=ListedColormap(["#FFFFFF", OKABE_ITO["vermillion"]]))
    ax.set_title("Missingness timeline for model-input variables", loc="left", fontweight="bold", pad=8)
    ax.set_xlabel("Month")
    ax.set_ylabel("")

    ticks, labels = [], []
    for idx, dt in enumerate(monthly.columns):
        if dt.month == 1 and dt.year % 2 == 0:
            ticks.append(idx + 0.5)
            labels.append(str(dt.year))
    ax.set_xticks(ticks)
    ax.set_xticklabels(labels, rotation=0)
    ax.set_yticks(np.arange(len(monthly.index)))
    ax.set_yticklabels(monthly.index)
    ax.tick_params(axis="y", labelsize=7)
    fig.tight_layout()
    return save_figure(fig, output_base)


def plot_stress_timeline(returns: pd.DataFrame, output_base: Path) -> tuple[Path, Path]:
    df = returns.dropna(subset=["r_vnindex"]).copy()
    fig, (ax_ret, ax_stress) = plt.subplots(
        2,
        1,
        figsize=(6.9, 4.6),
        sharex=True,
        gridspec_kw={"height_ratios": [3.2, 1.15], "hspace": 0.08},
    )
    ax_ret.plot(df["date"], df["r_vnindex"], color=OKABE_ITO["charcoal"], linewidth=0.62, linestyle="-")
    ax_ret.axhline(0, color=OKABE_ITO["charcoal"], linewidth=0.65, linestyle="--")

    ylim = robust_ylim(df["r_vnindex"], symmetric=True)
    ax_ret.set_ylim(*ylim)
    stress_specs = [
        ("stress_vnindex_q10", "10% stress", "-", 2, OKABE_ITO["orange"]),
        ("stress_vnindex_q05", "5% stress", "--", 1, OKABE_ITO["vermillion"]),
        ("stress_vnindex_q01", "1% stress", ":", 0, OKABE_ITO["reddish_purple"]),
    ]
    for col, _label, linestyle, y, color in stress_specs:
        stress_dates = df.loc[df[col] == 1, "date"]
        ax_stress.vlines(stress_dates, y - 0.34, y + 0.34, color=color, linewidth=0.65, linestyle=linestyle)

    ax_ret.set_title("VNIndex return and lower-tail stress events", loc="left", fontweight="bold", pad=8)
    ax_ret.set_ylabel("Daily return (%)")
    ax_ret.tick_params(axis="x", labelbottom=False)

    ax_stress.set_ylim(-0.65, 2.65)
    ax_stress.set_yticks([2, 1, 0])
    ax_stress.set_yticklabels(["10% stress", "5% stress", "1% stress"])
    ax_stress.set_ylabel("Stress events")
    ax_stress.set_xlabel("Date")
    ax_stress.grid(axis="x", color="#D9D9D9", linewidth=0.55)
    ax_stress.grid(axis="y", color="#EEEEEE", linewidth=0.45)
    ax_stress.spines["top"].set_visible(False)
    ax_stress.spines["right"].set_visible(False)
    apply_date_axis(ax_stress)
    fig.subplots_adjust(left=0.16, right=0.985, top=0.91, bottom=0.13, hspace=0.08)
    return save_figure(fig, output_base)


def plot_correlation_heatmap(returns: pd.DataFrame, variables: list[str], output_base: Path) -> tuple[Path, Path]:
    variables = [v for v in variables if v in returns.columns]
    corr = returns[variables].corr(method="pearson")

    fig, ax = plt.subplots(figsize=(6.9, 5.9))
    image = ax.imshow(corr.to_numpy(), cmap="RdBu_r", vmin=-1, vmax=1, aspect="equal")
    for i in range(corr.shape[0]):
        for j in range(corr.shape[1]):
            value = corr.iloc[i, j]
            ax.text(j, i, f"{value:.2f}", ha="center", va="center", fontsize=6, color=OKABE_ITO["charcoal"])
    ax.set_xticks(np.arange(len(corr.columns)))
    ax.set_xticklabels(corr.columns, rotation=45, ha="right")
    ax.set_yticks(np.arange(len(corr.index)))
    ax.set_yticklabels(corr.index)
    ax.set_xticks(np.arange(-0.5, len(corr.columns), 1), minor=True)
    ax.set_yticks(np.arange(-0.5, len(corr.index), 1), minor=True)
    ax.grid(which="minor", color="#FFFFFF", linestyle="-", linewidth=0.6)
    ax.tick_params(which="minor", bottom=False, left=False)
    cbar = fig.colorbar(image, ax=ax, shrink=0.82)
    cbar.set_label("Pearson correlation")
    ax.set_title("Pearson correlation matrix for core returns and controls", loc="left", fontweight="bold", pad=8)
    fig.tight_layout()
    return save_figure(fig, output_base)


def main() -> None:
    setup_style()
    ensure_dirs()
    start, end = sample_window()

    panel = read_csv_dates(CORE_PANEL)
    returns = read_csv_dates(CORE_RETURNS)
    crypto_returns = read_csv_dates(CRYPTO_RETURNS)

    panel_main = panel[(panel["date"] >= start) & (panel["date"] <= end)].copy()
    returns_main = returns[(returns["date"] > start) & (returns["date"] <= end)].copy()
    crypto_plot = returns_main[["date", "r_vnindex"]].merge(
        crypto_returns[["date", "r_btc", "r_eth"]],
        on="date",
        how="left",
    )

    level_specs = [
        SeriesSpec("Figure 1a", "vnindex", "VNIndex close", "Index level", "panel", "level_series"),
        SeriesSpec("Figure 1b", "sjc_gold", "SJC gold price", "VND per tael", "panel", "level_series"),
        SeriesSpec("Figure 1c", "xau_vnd", "Global gold converted to VND", "VND per tael", "panel", "level_series"),
        SeriesSpec("Figure 1d", "bond_tri_10y", "Vietnam 10-year government bond TRI", "Index level", "panel", "level_series"),
        SeriesSpec("Figure 2a", "brent", "Brent futures price", "USD per barrel", "panel", "level_series"),
        SeriesSpec("Figure 2b", "wti", "WTI futures price", "USD per barrel", "panel", "level_series"),
        SeriesSpec("Figure 2c", "vix", "VIX index", "Index level", "panel", "level_series"),
        SeriesSpec("Figure 2d", "policy_rate", "SBV policy rate", "Percent", "panel", "level_series"),
        SeriesSpec("Figure 2e", "interbank_rate", "VND interbank rate", "Percent", "panel", "level_series"),
    ]

    return_specs = [
        SeriesSpec("Figure 3a", "r_vnindex", "VNIndex daily return", "Return (%)", "returns", "return_series", True, True),
        SeriesSpec("Figure 3b", "r_sjc_gold", "SJC gold daily return", "Return (%)", "returns", "return_series", True, True),
        SeriesSpec("Figure 3c", "r_xau_vnd", "XAU/VND daily return", "Return (%)", "returns", "return_series", True, True),
        SeriesSpec("Figure 3d", "r_bond_tri", "Bond TRI daily return", "Return (%)", "returns", "return_series", True, True),
        SeriesSpec("Figure 3e", "r_brent", "Brent daily return", "Return (%)", "returns", "return_series", True, True),
        SeriesSpec("Figure 3f", "r_wti", "WTI daily return", "Return (%)", "returns", "return_series", True, True),
        SeriesSpec("Figure 4a", "d_vn10y_yield", "Daily change in Vietnam 10-year yield", "Change", "returns", "return_series", True, True),
        SeriesSpec("Figure 4b", "d_vn5y_yield", "Daily change in Vietnam 5-year yield", "Change", "returns", "return_series", True, True),
        SeriesSpec("Figure 4c", "d_interbank_rate", "Daily change in interbank rate", "Change", "returns", "return_series", True, True),
        SeriesSpec("Figure 4d", "d_policy_rate", "Daily change in policy rate", "Change", "returns", "return_series", True, True),
        SeriesSpec("Figure 4e", "d_vix", "Daily change in VIX", "Change", "returns", "return_series", True, True),
        SeriesSpec("Figure 5a", "r_btc", "Bitcoin daily return", "Return (%)", "crypto", "return_series", True, True),
        SeriesSpec("Figure 5b", "r_eth", "Ethereum daily return", "Return (%)", "crypto", "return_series", True, True),
    ]

    panel_viz, panel_audit = fill_visualization_gaps(
        panel_main,
        [s.variable for s in level_specs],
        "core_panel_daily",
        step_variables={"policy_rate"},
    )
    returns_viz, returns_audit = fill_visualization_gaps(
        returns_main,
        [s.variable for s in return_specs if s.source == "returns"],
        "final_daily_returns",
    )
    crypto_viz, crypto_audit = fill_visualization_gaps(
        crypto_plot,
        ["r_btc", "r_eth"],
        "final_crypto_daily_returns",
    )

    records: list[FigureRecord] = []
    for spec in level_specs:
        png, svg = plot_single_series(panel_viz, spec, FIGURES / spec.group / f"{spec.figure_id.lower().replace(' ', '_')}_{spec.variable}")
        records.append(FigureRecord(spec.figure_id, spec.title, str(png.relative_to(ROOT)), str(svg.relative_to(ROOT)), "level_series", spec.variable))

    for spec in return_specs:
        df = crypto_viz if spec.source == "crypto" else returns_viz
        png, svg = plot_single_series(df, spec, FIGURES / spec.group / f"{spec.figure_id.lower().replace(' ', '_')}_{spec.variable}")
        records.append(FigureRecord(spec.figure_id, spec.title, str(png.relative_to(ROOT)), str(svg.relative_to(ROOT)), "return_series", spec.variable))

    missing_vars = [
        "vnindex",
        "sjc_gold",
        "xau_vnd",
        "bond_tri_10y",
        "vn10y_yield",
        "vn5y_yield",
        "interbank_rate",
        "policy_rate",
        "brent",
        "wti",
        "vix",
        "btc",
        "eth",
    ]
    png, svg = plot_missingness(panel, missing_vars, FIGURES / "missingness" / "figure_06_missingness_timeline")
    records.append(FigureRecord("Figure 6", "Missingness timeline for model-input variables", str(png.relative_to(ROOT)), str(svg.relative_to(ROOT)), "missingness", ", ".join(missing_vars)))

    png, svg = plot_stress_timeline(returns_viz, FIGURES / "stress_timeline" / "figure_07_vnindex_stress_timeline")
    records.append(FigureRecord("Figure 7", "VNIndex return and lower-tail stress events", str(png.relative_to(ROOT)), str(svg.relative_to(ROOT)), "stress_timeline", "r_vnindex, stress_vnindex_q10, stress_vnindex_q05, stress_vnindex_q01"))

    corr_vars = [
        "r_vnindex",
        "r_sjc_gold",
        "r_xau_vnd",
        "r_gold_premium",
        "r_bond_tri",
        "r_brent",
        "r_wti",
        "d_vn10y_yield",
        "d_interbank_rate",
        "d_vix",
    ]
    png, svg = plot_correlation_heatmap(returns_viz, corr_vars, FIGURES / "correlation" / "figure_08_core_correlation_heatmap")
    records.append(FigureRecord("Figure 8", "Pearson correlation matrix for core returns and controls", str(png.relative_to(ROOT)), str(svg.relative_to(ROOT)), "correlation", ", ".join(corr_vars)))

    index = pd.DataFrame([r.__dict__ for r in records])
    index.to_csv(FIGURES / "figure_index.csv", index=False, encoding="utf-8-sig")

    audit = pd.DataFrame(panel_audit + returns_audit + crypto_audit)
    audit.to_csv(TABLES / "phase2b_visualization_imputation_audit.csv", index=False, encoding="utf-8-sig")

    report_lines = [
        "# Phase 2b Scientific Color Visualization Report",
        "",
        "## Scope",
        "",
        "This pass exports colorblind-safe scientific figures for presentation and paper drafting. Each core time series is exported as a separate figure. The plotted data use a visualization-only copy; official model-input files are not modified.",
        "",
        "## Missing-Data Treatment For Figures",
        "",
        "- Internal missing values are filled only for visualization continuity.",
        "- Continuous market series are filled by time interpolation within the observed sample range.",
        "- The event-driven policy-rate series is filled by internal forward-fill because it is a step-like monetary-policy variable.",
        "- Leading and trailing missing values remain missing, so asset samples are not extended beyond their real observed range.",
        f"- The imputation audit is saved at `{(TABLES / 'phase2b_visualization_imputation_audit.csv').relative_to(ROOT)}`.",
        "",
        "## Figure Index",
        "",
        "| Figure | Title | PNG | SVG | Variables |",
        "| --- | --- | --- | --- | --- |",
    ]
    for r in records:
        report_lines.append(f"| {r.figure_id} | {r.title} | `{r.png_file}` | `{r.svg_file}` | {r.variables} |")

    (METADATA / "model_input_visualization_report.md").write_text("\n".join(report_lines), encoding="utf-8")

    print(f"Created {len(records)} scientific-color figure files as PNG+SVG under {FIGURES}")
    print(f"Figure index: {FIGURES / 'figure_index.csv'}")
    print(f"Imputation audit: {TABLES / 'phase2b_visualization_imputation_audit.csv'}")
    print(f"Visualization report: {METADATA / 'model_input_visualization_report.md'}")


if __name__ == "__main__":
    main()
