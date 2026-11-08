from pathlib import Path


DATA_DIR = Path(__file__).resolve().parents[1]
RAW_DIR = DATA_DIR / "00_raw"
PROCESSED_DIR = DATA_DIR / "01_processed"
MODEL_INPUT_DIR = DATA_DIR / "02_model_input"
MODEL_OUTPUT_DIR = DATA_DIR / "03_model_output"
METADATA_DIR = DATA_DIR / "metadata"

# Sector configurations
SECTOR_RAW_DIR = RAW_DIR / "equity" / "sectors"
SECTOR_PROCESSED_DIR = PROCESSED_DIR / "sectors"
SECTOR_INPUT_DIR = MODEL_INPUT_DIR / "sector_extension"
BOND_TERM_RAW_DIR = RAW_DIR / "bonds" / "term_structure"
BOND_TERM_PROCESSED_DIR = PROCESSED_DIR / "bonds" / "term_structure"
BOND_TERM_INPUT_DIR = MODEL_INPUT_DIR / "bond_term_structure"
SECTOR_CODES = [
    "VNFIN", "VNREAL", "VNIND", "VNIT", "VNHEAL", 
    "VNUTI", "VNCONS", "VNCOND", "VNMAT", "VNENE"
]

CORE_START_DATE = "2010-01-04"

RAW_FILES = {
    "vnindex": RAW_DIR / "equity" / "chi_so_vnindex_vietnam.xlsx",
    "sjc_gold": RAW_DIR / "gold" / "gia_vang_sjc_lich_su.xlsx",
    "xau_usd": RAW_DIR / "gold" / "xau_usd_historical_data.csv",
    "gold_futures": RAW_DIR / "gold" / "gold_futures_gc_daily.csv",
    "vn10y_yield": RAW_DIR / "bonds" / "trai_phieu_chinh_phu_viet_nam_10n.xlsx",
    "vn5y_yield": RAW_DIR / "bonds" / "trai_phieu_chinh_phu_viet_nam_5n.xlsx",
    "vn1y_yield": BOND_TERM_RAW_DIR / "vn_gbond_1y_yield_raw.xlsx",
    "bond_tri": RAW_DIR / "bonds" / "chi_so_bond_total_return_vn.xlsx",
    "bond_yield_vol": RAW_DIR / "bonds" / "yield_volatility_10yr_lcy_bonds.csv",
    "interbank_rate": RAW_DIR / "macro" / "lai_suat_lien_ngan_hang_vn.xlsx",
    "policy_rate": RAW_DIR / "macro" / "lai_suat_dieu_hanh_vn.xlsx",
    "usd_vnd": RAW_DIR / "macro" / "ty_gia_usd_vnd_vn.xlsx",
    "usd_vnd_yahoo": RAW_DIR / "macro" / "usd_vnd_yahoo_daily.csv",
    "brent": RAW_DIR / "commodities" / "brent_futures_bz_daily.csv",
    "wti": RAW_DIR / "commodities" / "wti_futures_cl_daily.csv",
    "btc": RAW_DIR / "crypto" / "bitcoin_usd_daily.csv",
    "eth": RAW_DIR / "crypto" / "ethereum_usd_daily.csv",
    "vix": RAW_DIR / "risk" / "vix_daily.csv",
}

