from __future__ import annotations

import json
import re
import shutil
import time
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import Request, urlopen

from docx import Document


BASE = Path(__file__).resolve().parents[2]
SRC = BASE / "paper" / "final" / "BAI_LUAN_CK.docx"
OUT = BASE / "paper" / "final" / "BAI_LUAN_CK_VI.docx"
CACHE = BASE / ".codex_tmp_py" / "translation_cache_en_vi.json"


MANUAL = {
    "ABSTRACT": "TÓM TẮT",
    "Keywords:": "Từ khóa:",
    "TABLE OF CONTENTS": "MỤC LỤC",
    "LIST OF ABBREVIATIONS": "DANH MỤC TỪ VIẾT TẮT",
    "LIST OF TABLES": "DANH MỤC BẢNG",
    "LIST OF FIGURES": "DANH MỤC HÌNH",
    "REFERENCES": "TÀI LIỆU THAM KHẢO",
    "APPENDIX": "PHỤ LỤC",
    "For Portfolio Management:": "Đối với quản trị danh mục:",
    "For Macroeconomic Policy:": "Đối với chính sách kinh tế vĩ mô:",
}

TERM_MAP = {
    "VNIndex": "VNIndex",
    "VNINDEX": "VNINDEX",
    "SJC Gold": "vàng SJC",
    "SJC gold": "vàng SJC",
    "SJC": "SJC",
    "XAU/VND": "XAU/VND",
    "Bond TRI": "Bond TRI",
    "TRAS": "TRAS",
    "TRAS-layered": "TRAS-layered",
    "TRAS-flat": "TRAS-flat",
    "TRAS-ex-connectedness": "TRAS-ex-connectedness",
    "DCC": "DCC",
    "aDCC": "aDCC",
    "ADCC": "ADCC",
    "CCC": "CCC",
    "GARCH": "GARCH",
    "EGARCH": "EGARCH",
    "GJR-GARCH": "GJR-GARCH",
    "MSGARCH": "MSGARCH",
    "QVAR": "QVAR",
    "VaR": "VaR",
    "ES": "ES",
    "CoVaR": "CoVaR",
    "Delta-CoVaR": "Delta-CoVaR",
    "BTC": "BTC",
    "ETH": "ETH",
    "VIX": "VIX",
    "WTI": "WTI",
    "Brent": "Brent",
    "HOSE": "HOSE",
    "GICS": "GICS",
    "SHAP": "SHAP",
    "XGBoost": "XGBoost",
    "ARMA": "ARMA",
    "BIC": "BIC",
    "AIC": "AIC",
    "HAC": "HAC",
    "Student-t": "Student-t",
    "skewed Student-t": "skewed Student-t",
}


def load_cache() -> dict[str, str]:
    if CACHE.exists():
        return json.loads(CACHE.read_text(encoding="utf-8"))
    return {}


def save_cache(cache: dict[str, str]) -> None:
    CACHE.parent.mkdir(parents=True, exist_ok=True)
    CACHE.write_text(json.dumps(cache, ensure_ascii=False, indent=2), encoding="utf-8")


def protect(text: str) -> tuple[str, dict[str, str]]:
    protected: dict[str, str] = {}
    result = text

    # Protect citations such as (Baur & Lucey, 2010).
    for idx, match in enumerate(re.findall(r"\([A-Z][^()]*?\d{4}[^()]*?\)", result)):
        token = f"__CITE_{idx}__"
        result = result.replace(match, token)
        protected[token] = match

    offset = len(protected)
    for idx, term in enumerate(sorted(TERM_MAP, key=len, reverse=True), start=offset):
        token = f"__TERM_{idx}__"
        if term in result:
            result = result.replace(term, token)
            protected[token] = TERM_MAP[term]
    return result, protected


def restore(text: str, protected: dict[str, str]) -> str:
    for token, original in protected.items():
        text = text.replace(token, original)
        text = text.replace(token.lower(), original)
        text = text.replace(token.title(), original)
    return text


def postprocess(text: str) -> str:
    replacements = {
        "nơi trú ẩn an toàn": "trú ẩn an toàn",
        "tài sản an toàn": "tài sản trú ẩn an toàn",
        "phòng ngừa rủi ro": "phòng hộ",
        "tỷ lệ phòng hộ rủi ro": "tỷ lệ phòng hộ",
        "hiệu quả phòng hộ rủi ro": "hiệu quả phòng hộ",
        "học máy": "machine learning",
        "Máy học": "machine learning",
        "Trái phiếu Tri": "Bond TRI",
        "Trái phiếu TRI": "Bond TRI",
        "Vàng SJC": "vàng SJC",
        "Chỉ số tổng lợi nhuận trái phiếu": "Bond TRI",
        "Nguồn: Tác giả": "Nguồn: Tác giả",
        "Bảng của": "Bảng",
        "Hình của": "Hình",
    }
    for src, dst in replacements.items():
        text = text.replace(src, dst)
    return text


def google_translate(text: str, cache: dict[str, str]) -> str:
    stripped = text.strip()
    if stripped in MANUAL:
        return text.replace(stripped, MANUAL[stripped])
    if stripped in cache:
        return cache[stripped]

    leading = text[: len(text) - len(text.lstrip())]
    trailing = text[len(text.rstrip()) :]
    core = stripped
    protected_core, protected = protect(core)
    params = urlencode(
        {
            "client": "gtx",
            "sl": "en",
            "tl": "vi",
            "dt": "t",
            "q": protected_core,
        }
    )
    url = f"https://translate.googleapis.com/translate_a/single?{params}"
    req = Request(url, headers={"User-Agent": "Mozilla/5.0"})
    for attempt in range(3):
        try:
            with urlopen(req, timeout=12) as response:
                payload = json.loads(response.read().decode("utf-8"))
            translated = "".join(part[0] for part in payload[0] if part[0])
            translated = restore(translated, protected)
            translated = postprocess(translated)
            result = f"{leading}{translated}{trailing}"
            cache[stripped] = result
            return result
        except Exception:
            if attempt == 2:
                return text
            time.sleep(1.5 * (attempt + 1))
    return text


def mostly_symbols_or_numbers(text: str) -> bool:
    stripped = text.strip()
    if not stripped:
        return True
    letters = sum(ch.isalpha() for ch in stripped)
    if letters == 0:
        return True
    if letters / max(len(stripped), 1) < 0.12 and any(ch.isdigit() for ch in stripped):
        return True
    if re.fullmatch(r"[\d\s\.\,\-\+\(\)\[\]\%\:\;\/\\=<>]+", stripped):
        return True
    return False


def should_skip(paragraph, in_references: bool) -> bool:
    text = paragraph.text.strip()
    if not text:
        return True
    if text in MANUAL:
        return False
    style = paragraph.style.name if paragraph.style is not None else ""
    if style.startswith("TOC") or style.startswith("Index"):
        return True
    if "\t" in paragraph.text and ("." in paragraph.text or text[-1:].isdigit()):
        # Preserve generated TOC/List of Tables/List of Figures field results.
        return True
    if in_references and not text.upper().startswith("APPENDIX"):
        return True
    if mostly_symbols_or_numbers(text):
        return True
    return False


def replace_text(paragraph, new_text: str) -> None:
    if paragraph.runs:
        paragraph.runs[0].text = new_text
        for run in paragraph.runs[1:]:
            run.text = ""
    else:
        paragraph.add_run(new_text)


def main() -> None:
    shutil.copy2(SRC, OUT)
    doc = Document(OUT)
    cache = load_cache()
    in_references = False
    translated = 0
    skipped = 0

    total = len(doc.paragraphs)
    for i, paragraph in enumerate(doc.paragraphs, start=1):
        text = paragraph.text
        stripped = text.strip()
        upper = stripped.upper()
        if upper == "REFERENCES":
            in_references = True
        elif upper.startswith("APPENDIX"):
            in_references = False

        if should_skip(paragraph, in_references):
            skipped += 1
            continue

        new_text = google_translate(text, cache)
        if new_text != text:
            replace_text(paragraph, new_text)
            translated += 1
        else:
            skipped += 1

        if i % 25 == 0:
            save_cache(cache)
            print(f"progress {i}/{total} translated={translated} skipped={skipped}", flush=True)

    # Cover/title refinements and key academic terms.
    for paragraph in doc.paragraphs:
        t = paragraph.text.strip()
        if "BEYOND SAFE HAVEN" in t.upper() or "VƯỢT RA NGOÀI" in t.upper():
            replace_text(
                paragraph,
                "VƯỢT RA NGOÀI KHÁI NIỆM TRÚ ẨN AN TOÀN: KHẢ NĂNG HẤP THỤ RỦI RO ĐUÔI CỦA VÀNG, XAU/VND VÀ TRÁI PHIẾU CHÍNH PHỦ ĐỐI VỚI VNINDEX",
            )
        elif "Bài Dự Án Cuối Kỳ" in t:
            replace_text(paragraph, "Bài luận cuối kỳ môn Kinh tế lượng tài chính nâng cao")

    save_cache(cache)
    doc.save(OUT)
    print(f"created={OUT}", flush=True)
    print(f"translated={translated}", flush=True)
    print(f"skipped={skipped}", flush=True)


if __name__ == "__main__":
    main()
