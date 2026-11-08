import sys
import os
import re
from docx import Document
from docx.shared import Inches, Pt, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH, WD_TAB_ALIGNMENT, WD_TAB_LEADER
from docx.enum.section import WD_SECTION_START
from docx.oxml import OxmlElement, parse_xml
from docx.oxml.ns import qn

if hasattr(sys.stdout, 'reconfigure'):
    sys.stdout.reconfigure(encoding='utf-8')
if hasattr(sys.stderr, 'reconfigure'):
    sys.stderr.reconfigure(encoding='utf-8')

HAS_MATH_LIBS = False
try:
    import latex2mathml.converter
    import mathml2omml
    HAS_MATH_LIBS = True
except ImportError:
    pass

def to_omml(formula):
    mathml_str = latex2mathml.converter.convert(formula)
    omml_str = mathml2omml.convert(mathml_str)
    # Fix mathml2omml's groupChrPr tag mismatch bug
    omml_str = re.sub(r'(<m:groupChrPr>.*?)</m:groupChr>', r'\1</m:groupChrPr>', omml_str)
    if 'xmlns:m=' not in omml_str:
        omml_str = omml_str.replace('<m:oMath>', '<m:oMath xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math">')
    return parse_xml(omml_str)

def add_numbered_equation(doc, formula, tag):
    # Xác định xem công thức có thuộc phần 3.5 TRAS (từ 3.20 trở đi) hay không
    use_tab_stop = False
    try:
        parts = tag.split('.')
        if len(parts) >= 2 and int(parts[1]) >= 20:
            use_tab_stop = True
    except (ValueError, IndexError):
        pass

    if use_tab_stop:
        p = doc.add_paragraph()
        p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
        p.paragraph_format.space_before = Pt(6)
        p.paragraph_format.space_after = Pt(6)
        p.paragraph_format.tab_stops.add_tab_stop(Inches(6.5), alignment=WD_TAB_ALIGNMENT.RIGHT)
        
        if HAS_MATH_LIBS:
            try:
                omml_element = to_omml(formula)
                p._p.append(omml_element)
            except Exception as e:
                print(f"Error converting formula '{formula}': {e}")
                run = p.add_run(f"$${formula}$$")
                run.font.name = 'Times New Roman'
                run.font.size = Pt(13)
                run.font.color.rgb = RGBColor(0, 0, 0)
        else:
            run = p.add_run(f"$${formula}$$")
            run.font.name = 'Times New Roman'
            run.font.size = Pt(13)
            run.font.color.rgb = RGBColor(0, 0, 0)
            
        run_tag = p.add_run(f"\t({tag})")
        run_tag.font.name = 'Times New Roman'
        run_tag.font.size = Pt(13)
        run_tag.font.color.rgb = RGBColor(0, 0, 0)
    else:
        if HAS_MATH_LIBS:
            table = doc.add_table(rows=1, cols=2)
            table.alignment = WD_ALIGN_PARAGRAPH.CENTER
            table.autofit = False
            
            # Xóa viền bảng
            tblPr = table._tbl.tblPr
            tblBorders = tblPr.first_child_found_in("w:tblBorders")
            if tblBorders is not None:
                tblPr.remove(tblBorders)
            new_borders = parse_xml(
                '<w:tblBorders xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">'
                '<w:top w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                '<w:left w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                '<w:bottom w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                '<w:right w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                '<w:insideH w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                '<w:insideV w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                '</w:tblBorders>'
            )
            tblPr.append(new_borders)
            
            table.columns[0].width = Inches(5.2)
            table.columns[1].width = Inches(0.8)
            
            # Ô công thức căn giữa
            cell_eq = table.rows[0].cells[0]
            cell_eq.width = Inches(5.2)
            p_eq = cell_eq.paragraphs[0]
            p_eq.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
            p_eq.paragraph_format.space_before = Pt(6)
            p_eq.paragraph_format.space_after = Pt(6)
            
            try:
                omml_element = to_omml(formula)
                p_eq._p.append(omml_element)
            except Exception as e:
                print(f"Error converting formula '{formula}': {e}")
                run = p_eq.add_run(f"$${formula}$$")
                run.font.name = 'Times New Roman'
                run.font.size = Pt(13)
                run.font.color.rgb = RGBColor(0, 0, 0)
                
            # Ô số hiệu căn phải
            cell_tag = table.rows[0].cells[1]
            cell_tag.width = Inches(0.8)
            p_tag = cell_tag.paragraphs[0]
            p_tag.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.RIGHT
            p_tag.paragraph_format.space_before = Pt(6)
            p_tag.paragraph_format.space_after = Pt(6)
            
            run_tag = p_tag.add_run(f"({tag})")
            run_tag.font.name = 'Times New Roman'
            run_tag.font.size = Pt(13)
            run_tag.font.color.rgb = RGBColor(0, 0, 0)
        else:
            p = doc.add_paragraph()
            p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
            p.paragraph_format.space_before = Pt(6)
            p.paragraph_format.space_after = Pt(6)
            run = p.add_run(f"$${formula} \\tag{{{tag}}}$$")
            run.font.name = 'Times New Roman'
            run.font.size = Pt(13)
            run.font.color.rgb = RGBColor(0, 0, 0)

def add_unnumbered_equation(doc, formula):
    p = doc.add_paragraph()
    p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    p.paragraph_format.space_before = Pt(6)
    p.paragraph_format.space_after = Pt(6)
    
    if HAS_MATH_LIBS:
        try:
            omml_element = to_omml(formula)
            p._p.append(omml_element)
        except Exception as e:
            print(f"Error converting formula '{formula}': {e}")
            run = p.add_run(f"$${formula}$$")
            run.font.name = 'Times New Roman'
            run.font.size = Pt(13)
            run.font.color.rgb = RGBColor(0, 0, 0)
    else:
        run = p.add_run(f"$${formula}$$")
        run.font.name = 'Times New Roman'
        run.font.size = Pt(13)
        run.font.color.rgb = RGBColor(0, 0, 0)

# --- Special Pages and Formatting Helpers ---
def set_section_page_numbering(section, start=1, fmt="lowerRoman"):
    sectPr = section._sectPr
    pgNumType = sectPr.find(qn("w:pgNumType"))
    if pgNumType is None:
        pgNumType = OxmlElement("w:pgNumType")
        sectPr.append(pgNumType)
    pgNumType.set(qn("w:start"), str(start))
    pgNumType.set(qn("w:fmt"), fmt)

def set_update_fields_on_open(doc):
    settings = doc.settings.element
    update_fields = settings.find(qn("w:updateFields"))
    if update_fields is None:
        update_fields = OxmlElement("w:updateFields")
        settings.append(update_fields)
    update_fields.set(qn("w:val"), "true")

def add_page_number_to_footer(footer_paragraph):
    footer_paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    footer_paragraph.paragraph_format.space_before = Pt(0)
    footer_paragraph.paragraph_format.space_after = Pt(0)
    
    run = footer_paragraph.add_run()
    run.font.name = 'Times New Roman'
    run.font.size = Pt(10)
    run.font.color.rgb = RGBColor(0, 0, 0)
    
    fldChar1 = OxmlElement('w:fldChar')
    fldChar1.set(qn('w:fldCharType'), 'begin')
    fldChar1.set(qn('w:dirty'), 'true')
    run._r.append(fldChar1)
    
    instrText = OxmlElement('w:instrText')
    instrText.set(qn('xml:space'), 'preserve')
    instrText.text = "PAGE"
    run._r.append(instrText)
    
    fldChar2 = OxmlElement('w:fldChar')
    fldChar2.set(qn('w:fldCharType'), 'separate')
    run._r.append(fldChar2)
    
    t = OxmlElement('w:t')
    t.text = "1"
    run._r.append(t)
    
    fldChar3 = OxmlElement('w:fldChar')
    fldChar3.set(qn('w:fldCharType'), 'end')
    run._r.append(fldChar3)

def pre_scan_markdown(lines, is_english):
    tables = []
    figures = []
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if not line:
            i += 1
            continue
            
        clean_line = line.strip('*').strip('_').strip()
        
        match_table = re.match(r'^(?:Table|Bảng)\s+([A-Za-z0-9.]+):?\s*(.*)$', clean_line)
        if match_table:
            is_real_table = False
            for j in range(i + 1, min(i + 5, len(lines))):
                if lines[j].strip().startswith('|'):
                    is_real_table = True
                    break
            if is_real_table:
                table_num = match_table.group(1).strip()
                table_title = match_table.group(2).strip()
                tables.append((table_num, table_title, line))
                
        img_match = re.match(r'^!\[(.*?)\]\((.*?)\)$', line)
        if img_match:
            first_caption = img_match.group(1).strip()
            next_idx = i + 1
            while next_idx < len(lines):
                next_line = lines[next_idx].strip()
                if not next_line:
                    next_idx += 1
                    continue
                if re.match(r'^!\[(.*?)\]\((.*?)\)$', next_line):
                    next_idx += 1
                else:
                    break
                    
            main_title = first_caption
            title_parts = re.split(r'[-–]\s*Panel|:\s*Panel|Panel\s+[A-Za-z]', first_caption, flags=re.IGNORECASE)
            if title_parts:
                main_title = title_parts[0].strip()
                
            clean_main = main_title.strip('*').strip('_').strip()
            match_fig = re.match(r'^(?:Figure|Hình)\s+([A-Za-z0-9.]+):?\s*(.*)$', clean_main)
            if match_fig:
                fig_num = match_fig.group(1).strip()
                fig_title = match_fig.group(2).strip()
                figures.append((fig_num, fig_title, main_title))
                
            i = next_idx
            continue
            
        i += 1
        
    return tables, figures

def make_bookmark_name(prefix, num_str):
    clean_num = re.sub(r'[^A-Za-z0-9_]', '_', num_str)
    return f"{prefix}_{clean_num}"

def add_bookmark(paragraph, name, bookmark_id):
    bookmark_start = OxmlElement('w:bookmarkStart')
    bookmark_start.set(qn('w:id'), str(bookmark_id))
    bookmark_start.set(qn('w:name'), name)
    paragraph._p.append(bookmark_start)

    bookmark_end = OxmlElement('w:bookmarkEnd')
    bookmark_end.set(qn('w:id'), str(bookmark_id))
    paragraph._p.append(bookmark_end)

def insert_pageref(paragraph, bookmark_name):
    run = paragraph.add_run()
    run.font.name = 'Times New Roman'
    run.font.size = Pt(13)
    run.font.color.rgb = RGBColor(0, 0, 0)
    
    fldChar1 = OxmlElement('w:fldChar')
    fldChar1.set(qn('w:fldCharType'), 'begin')
    fldChar1.set(qn('w:dirty'), 'true')
    run._r.append(fldChar1)
    
    instrText = OxmlElement('w:instrText')
    instrText.set(qn('xml:space'), 'preserve')
    instrText.text = f'PAGEREF {bookmark_name} \\h'
    run._r.append(instrText)
    
    fldChar2 = OxmlElement('w:fldChar')
    fldChar2.set(qn('w:fldCharType'), 'separate')
    run._r.append(fldChar2)
    
    t = OxmlElement('w:t')
    t.text = '?'
    run._r.append(t)
    
    fldChar3 = OxmlElement('w:fldChar')
    fldChar3.set(qn('w:fldCharType'), 'end')
    run._r.append(fldChar3)

def add_table_of_contents(doc, title_text):
    h = doc.add_heading(level=1)
    h.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    h.paragraph_format.space_before = Pt(12)
    h.paragraph_format.space_after = Pt(6)
    h.paragraph_format.keep_with_next = True
    run = h.add_run(title_text)
    run.font.name = 'Times New Roman'
    run.font.size = Pt(16)
    run.font.bold = True
    run.font.color.rgb = RGBColor(0, 0, 0)
    
    p = doc.add_paragraph()
    p.paragraph_format.space_before = Pt(6)
    p.paragraph_format.space_after = Pt(6)
    p.paragraph_format.line_spacing = 1.5
    p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
    
    run_field = p.add_run()
    run_field.font.name = 'Times New Roman'
    run_field.font.size = Pt(13)
    run_field.font.color.rgb = RGBColor(0, 0, 0)
    
    fldChar1 = OxmlElement('w:fldChar')
    fldChar1.set(qn('w:fldCharType'), 'begin')
    fldChar1.set(qn('w:dirty'), 'true')
    run_field._r.append(fldChar1)
    
    instrText = OxmlElement('w:instrText')
    instrText.set(qn('xml:space'), 'preserve')
    instrText.text = 'TOC \\o "1-3" \\h \\z \\u'
    run_field._r.append(instrText)
    
    fldChar2 = OxmlElement('w:fldChar')
    fldChar2.set(qn('w:fldCharType'), 'separate')
    run_field._r.append(fldChar2)
    
    t = OxmlElement('w:t')
    t.text = 'Bấm chuột phải vào đây và chọn "Update Field" để cập nhật mục lục.' if 'MỤC' in title_text else 'Right-click here and select "Update Field" to update the table of contents.'
    run_field._r.append(t)
    
    fldChar3 = OxmlElement('w:fldChar')
    fldChar3.set(qn('w:fldCharType'), 'end')
    run_field._r.append(fldChar3)

def add_list_of_abbreviations(doc, is_english, text_content):
    title_text = "DANH MỤC CÁC TỪ VIẾT TẮT" if not is_english else "LIST OF ABBREVIATIONS"
    h = doc.add_heading(level=1)
    h.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    h.paragraph_format.space_before = Pt(12)
    h.paragraph_format.space_after = Pt(6)
    h.paragraph_format.keep_with_next = True
    run = h.add_run(title_text)
    run.font.name = 'Times New Roman'
    run.font.size = Pt(16)
    run.font.bold = True
    run.font.color.rgb = RGBColor(0, 0, 0)
    
    abbrev_dict_vi = {
        "TRAS": "Điểm hấp thụ rủi ro đuôi (Tail Risk Absorption Score)",
        "SJC": "Saigon Jewelry Company (Vàng miếng SJC)",
        "VNIndex": "Chỉ số chứng khoán VN-Index",
        "XAU/VND": "Giá vàng quốc tế quy đổi sang Việt Nam Đồng",
        "TRI": "Chỉ số tổng suất sinh lợi (Total Return Index)",
        "ADF": "Kiểm định Augmented Dickey-Fuller",
        "ADCC": "Mô hình tương quan động có điều kiện bất đối xứng",
        "AIC": "Tiêu chuẩn thông tin Akaike",
        "ARCH": "Mô hình tự hồi quy sai số thay đổi có điều kiện",
        "ARCH-LM": "Kiểm định nhân tử Lagrange đối với hiệu ứng ARCH",
        "ARMA": "Mô hình tự hồi quy trung bình trượt",
        "GARCH": "Mô hình tự hồi quy sai số thay đổi có điều kiện tổng quát",
        "EGARCH": "Mô hình GARCH hàm mũ",
        "GJR-GARCH": "Mô hình GARCH bất đối xứng Glosten-Jagannathan-Runkle",
        "sGARCH": "Mô hình GARCH tiêu chuẩn",
        "CCC": "Mô hình tương quan có điều kiện hằng số",
        "DCC": "Mô hình tương quan động có điều kiện",
        "aDCC": "Mô hình tương quan động có điều kiện bất đối xứng",
        "SBV": "Ngân hàng Nhà nước Việt Nam",
        "VIX": "Chỉ số đo lường trạng thái biến động thị trường",
        "BTC": "Bitcoin",
        "ETH": "Ethereum",
        "CoVaR": "Giá trị rủi ro có điều kiện hệ thống",
        "VAR": "Mô hình tự hồi quy vectơ",
        "TVP-VAR": "Mô hình tự hồi quy vectơ tham số thay đổi theo thời gian",
        "GDP": "Tổng sản phẩm quốc nội",
        "OECD": "Tổ chức Hợp tác và Phát triển Kinh tế",
        "JRC": "Trung tâm Nghiên cứu Chung của Ủy ban Châu Âu",
        "XGBoost": "Mô hình cây quyết định tăng cường gradient cực đại",
        "SHAP": "Giải thích cộng dồn Shapley",
        "APA": "Hiệp hội Tâm lý học Hoa Kỳ",
        "JABES": "Tạp chí Nghiên cứu Kinh tế và Kinh doanh Châu Á (UEH)",
        "UEH": "Đại học Kinh tế Thành phố Hồ Chí Minh",
        "MSGARCH": "Mô hình GARCH chuyển đổi trạng thái Markov",
        "TCI": "Chỉ số liên kết hệ thống (Total Connectedness Index)",
        "QVAR": "Mô hình tự hồi quy vectơ phân vị",
        "VaR": "Giá trị rủi ro (Value at Risk)",
        "ES": "Tổn thất kỳ vọng có điều kiện (Expected Shortfall)",
        "JB": "Kiểm định Jarque-Bera",
        "LB": "Kiểm định Ljung-Box",
        "NET": "Chỉ số lan tỏa ròng",
        "HOSE": "Sở Giao dịch Chứng khoán Thành phố Hồ Chí Minh",
        "GICS": "Tiêu chuẩn phân ngành kinh doanh toàn cầu",
        "ML": "Học máy (Machine Learning)",
        "BIC": "Tiêu chuẩn thông tin Bayesian",
        "std": "Phân phối t-Student",
        "sstd": "Phân phối t-Student lệch",
        "WTI": "Dầu thô ngọt nhẹ Texas (West Texas Intermediate)"
    }
    
    abbrev_dict_en = {
        "TRAS": "Tail Risk Absorption Score",
        "SJC": "Saigon Jewelry Company",
        "VNIndex": "VN-Index (Vietnam Stock Index)",
        "XAU/VND": "International gold price converted to Vietnamese Dong",
        "TRI": "Total Return Index",
        "ADF": "Augmented Dickey-Fuller test",
        "ADCC": "Asymmetric Dynamic Conditional Correlation",
        "AIC": "Akaike Information Criterion",
        "ARCH": "Autoregressive Conditional Heteroskedasticity",
        "ARCH-LM": "Lagrange multiplier test for ARCH effects",
        "ARMA": "Autoregressive Moving Average",
        "GARCH": "Generalized Autoregressive Conditional Heteroskedasticity",
        "EGARCH": "Exponential GARCH",
        "GJR-GARCH": "Glosten-Jagannathan-Runkle GARCH",
        "sGARCH": "Standard GARCH",
        "CCC": "Constant Conditional Correlation",
        "DCC": "Dynamic Conditional Correlation",
        "aDCC": "Asymmetric Dynamic Conditional Correlation",
        "SBV": "State Bank of Vietnam",
        "VIX": "Volatility Index",
        "BTC": "Bitcoin",
        "ETH": "Ethereum",
        "CoVaR": "Conditional Value at Risk",
        "VAR": "Vector Autoregression",
        "TVP-VAR": "Time-Varying Parameter Vector Autoregression",
        "GDP": "Gross Domestic Product",
        "OECD": "Organisation for Economic Co-operation and Development",
        "JRC": "Joint Research Centre",
        "XGBoost": "Extreme Gradient Boosting",
        "SHAP": "Shapley Additive exPlanations",
        "APA": "American Psychological Association",
        "JABES": "Journal of Asian Business and Economic Studies",
        "UEH": "University of Economics Ho Chi Minh City",
        "MSGARCH": "Markov Switching GARCH",
        "TCI": "Total Connectedness Index",
        "QVAR": "Quantile Vector Autoregression",
        "VaR": "Value at Risk",
        "ES": "Expected Shortfall",
        "JB": "Jarque-Bera test",
        "LB": "Ljung-Box test",
        "NET": "Net spillover index",
        "HOSE": "Ho Chi Minh City Stock Exchange",
        "GICS": "Global Industry Classification Standard",
        "ML": "Machine Learning",
        "BIC": "Bayesian Information Criterion",
        "std": "Student-t distribution",
        "sstd": "Skewed Student-t distribution",
        "WTI": "West Texas Intermediate (crude oil benchmark)"
    }
    
    source_dict = abbrev_dict_en if is_english else abbrev_dict_vi
    
    found_abbrevs = []
    for abb, meaning in source_dict.items():
        pattern = r'\b' + re.escape(abb) + r'\b'
        if re.search(pattern, text_content):
            found_abbrevs.append((abb, meaning))
            
    found_abbrevs.sort(key=lambda x: x[0].lower())
    
    if not found_abbrevs:
        p = doc.add_paragraph()
        run_empty = p.add_run("Không có từ viết tắt nào." if not is_english else "No abbreviations found.")
        run_empty.font.name = 'Times New Roman'
        run_empty.font.size = Pt(13)
        return
        
    table = doc.add_table(rows=len(found_abbrevs) + 1, cols=2)
    table.style = 'Table Grid'
    
    table.columns[0].width = Inches(1.5)
    table.columns[1].width = Inches(5.0)
    
    hdr_cells = table.rows[0].cells
    hdr_cells[0].text = "Từ viết tắt" if not is_english else "Abbreviation"
    hdr_cells[1].text = "Nghĩa đầy đủ" if not is_english else "Full Meaning"
    
    for cell in hdr_cells:
        for paragraph in cell.paragraphs:
            paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
            paragraph.paragraph_format.space_before = Pt(3)
            paragraph.paragraph_format.space_after = Pt(3)
            for run in paragraph.runs:
                run.font.name = 'Times New Roman'
                run.font.size = Pt(11)
                run.font.bold = True
                run.font.color.rgb = RGBColor(0, 0, 0)
                
    for row_idx, (abb, meaning) in enumerate(found_abbrevs):
        row_cells = table.rows[row_idx + 1].cells
        row_cells[0].text = abb
        row_cells[1].text = meaning
        
        for col_idx, cell in enumerate(row_cells):
            cell.width = Inches(1.5) if col_idx == 0 else Inches(5.0)
            for paragraph in cell.paragraphs:
                paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
                paragraph.paragraph_format.space_before = Pt(3)
                paragraph.paragraph_format.space_after = Pt(3)
                paragraph.paragraph_format.line_spacing = 1.15
                for run in paragraph.runs:
                    run.font.name = 'Times New Roman'
                    run.font.size = Pt(11)
                    if col_idx == 0:
                        run.font.bold = True
                    run.font.color.rgb = RGBColor(0, 0, 0)

def add_list_of_tables(doc, tables, is_english):
    title_text = "DANH MỤC BẢNG" if not is_english else "LIST OF TABLES"
    h = doc.add_heading(level=1)
    h.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    h.paragraph_format.space_before = Pt(12)
    h.paragraph_format.space_after = Pt(6)
    h.paragraph_format.keep_with_next = True
    run = h.add_run(title_text)
    run.font.name = 'Times New Roman'
    run.font.size = Pt(16)
    run.font.bold = True
    run.font.color.rgb = RGBColor(0, 0, 0)
    
    if not tables:
        p = doc.add_paragraph()
        run_empty = p.add_run("Không có bảng nào." if not is_english else "No tables found.")
        run_empty.font.name = 'Times New Roman'
        run_empty.font.size = Pt(13)
        return
        
    for num, title, raw in tables:
        p = doc.add_paragraph()
        p.paragraph_format.space_before = Pt(3)
        p.paragraph_format.space_after = Pt(3)
        p.paragraph_format.line_spacing = 1.5
        p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        p.paragraph_format.tab_stops.add_tab_stop(Inches(6.4), alignment=WD_TAB_ALIGNMENT.RIGHT, leader=WD_TAB_LEADER.DOTS)
        
        label = f"Bảng {num}: " if not is_english else f"Table {num}: "
        run_label = p.add_run(label)
        run_label.font.name = 'Times New Roman'
        run_label.font.size = Pt(13)
        run_label.font.bold = True
        
        run_title = p.add_run(title)
        run_title.font.name = 'Times New Roman'
        run_title.font.size = Pt(13)
        
        p.add_run("\t")
        bm_name = make_bookmark_name("RefTable", num)
        insert_pageref(p, bm_name)

def add_list_of_figures(doc, figures, is_english):
    title_text = "DANH MỤC HÌNH" if not is_english else "LIST OF FIGURES"
    h = doc.add_heading(level=1)
    h.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
    h.paragraph_format.space_before = Pt(12)
    h.paragraph_format.space_after = Pt(6)
    h.paragraph_format.keep_with_next = True
    run = h.add_run(title_text)
    run.font.name = 'Times New Roman'
    run.font.size = Pt(16)
    run.font.bold = True
    run.font.color.rgb = RGBColor(0, 0, 0)
    
    if not figures:
        p = doc.add_paragraph()
        run_empty = p.add_run("Không có hình nào." if not is_english else "No figures found.")
        run_empty.font.name = 'Times New Roman'
        run_empty.font.size = Pt(13)
        return
        
    seen_figs = set()
    unique_figures = []
    for num, title, raw in figures:
        if num not in seen_figs:
            seen_figs.add(num)
            unique_figures.append((num, title, raw))
            
    for num, title, raw in unique_figures:
        p = doc.add_paragraph()
        p.paragraph_format.space_before = Pt(3)
        p.paragraph_format.space_after = Pt(3)
        p.paragraph_format.line_spacing = 1.5
        p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
        p.paragraph_format.tab_stops.add_tab_stop(Inches(6.4), alignment=WD_TAB_ALIGNMENT.RIGHT, leader=WD_TAB_LEADER.DOTS)
        
        label = f"Hình {num}: " if not is_english else f"Figure {num}: "
        run_label = p.add_run(label)
        run_label.font.name = 'Times New Roman'
        run_label.font.size = Pt(13)
        run_label.font.bold = True
        
        run_title = p.add_run(title)
        run_title.font.name = 'Times New Roman'
        run_title.font.size = Pt(13)
        
        p.add_run("\t")
        bm_name = make_bookmark_name("RefFig", num)
        insert_pageref(p, bm_name)

# Original line 119

def add_formatted_text(paragraph, text):
    # Protect math blocks (both $$...$$, \\(...\\), and $...$) from markdown formatting
    math_blocks = []
    temp_text = text
    
    # 1. Extract $$...$$ display math
    display_math = re.findall(r'\$\$.*?\$\$', temp_text)
    for idx, math in enumerate(display_math):
        placeholder = f"MATHBLOCKDISPLAYPLACEHOLDER{idx}"
        temp_text = temp_text.replace(math, placeholder)
        math_blocks.append((placeholder, math))
        
    # 2. Extract \\(...\\) inline math
    inline_math = re.findall(r'\\\(.*?\\\)', temp_text)
    for idx, math in enumerate(inline_math):
        placeholder = f"MATHBLOCKINLINEPLACEHOLDER{idx}"
        temp_text = temp_text.replace(math, placeholder)
        math_blocks.append((placeholder, math))
        
    # 3. Extract $...$ inline math
    inline_math_dollar = re.findall(r'\$.*?\$', temp_text)
    for idx, math in enumerate(inline_math_dollar):
        placeholder = f"MATHBLOCKDOLLARPLACEHOLDER{idx}"
        temp_text = temp_text.replace(math, placeholder)
        math_blocks.append((placeholder, math))

    # Parse bold and italic formatting using regex on the text with placeholders
    parts = re.split(r'(\*\*.*?\*\*|__.*?__|__.*?__|__.*?__|\*.*?\*|_.*?_)', temp_text)
    for part in parts:
        if not part:
            continue
            
        is_bold = False
        is_italic = False
        clean_part = part
        
        if part.startswith('**') and part.endswith('**'):
            clean_part = part[2:-2]
            is_bold = True
        elif part.startswith('__') and part.endswith('__'):
            clean_part = part[2:-2]
            is_bold = True
        elif part.startswith('*') and part.endswith('*'):
            clean_part = part[1:-1]
            is_italic = True
        elif part.startswith('_') and part.endswith('_'):
            clean_part = part[1:-1]
            is_italic = True
            
        # Split clean_part by placeholders to restore and add runs
        sub_parts = re.split(r'(MATHBLOCKDISPLAYPLACEHOLDER\d+|MATHBLOCKINLINEPLACEHOLDER\d+|MATHBLOCKDOLLARPLACEHOLDER\d+)', clean_part)
        for sub_part in sub_parts:
            if not sub_part:
                continue
                
            run_text = sub_part
            is_math = False
            for placeholder, math_val in math_blocks:
                if sub_part == placeholder:
                    run_text = math_val
                    is_math = True
                    break
                    
            if is_math and HAS_MATH_LIBS:
                clean_formula = run_text
                if clean_formula.startswith('$$') and clean_formula.endswith('$$'):
                    clean_formula = clean_formula[2:-2].strip()
                elif clean_formula.startswith('\\(') and clean_formula.endswith('\\)'):
                    clean_formula = clean_formula[2:-2].strip()
                elif clean_formula.startswith('$') and clean_formula.endswith('$'):
                    clean_formula = clean_formula[1:-1].strip()
                
                try:
                    omml_element = to_omml(clean_formula)
                    paragraph._p.append(omml_element)
                except Exception as e:
                    print(f"Error converting inline math '{clean_formula}': {e}")
                    run = paragraph.add_run(run_text)
                    run.font.name = 'Times New Roman'
                    run.font.size = Pt(13)
                    run.font.color.rgb = RGBColor(0, 0, 0)
            else:
                run = paragraph.add_run(run_text)
                if is_bold:
                    run.font.bold = True
                if is_italic:
                    run.font.italic = True
                run.font.name = 'Times New Roman'
                run.font.size = Pt(13)
                run.font.color.rgb = RGBColor(0, 0, 0)

def parse_markdown_to_docx(md_path, docx_path):
    print(f"Reading markdown from {md_path}...")
    
    # Initialize bookmark counter
    bookmark_id_counter = 0
    
    # Look for cover page file
    cover_path = os.path.join(os.path.dirname(md_path), "bìa.docx")
    if not os.path.exists(cover_path):
        cover_path = "paper/final/bìa.docx"
        
    if os.path.exists(cover_path):
        print(f"Loading cover page directly from {cover_path}...")
        doc = Document(cover_path)
        has_cover = True
        
        # Explicitly bold cover page content to preserve visual appearance
        try:
            for p in doc.paragraphs:
                for run in p.runs:
                    run.font.bold = True
            for table in doc.tables:
                for row in table.rows:
                    for cell in row.cells:
                        for p in cell.paragraphs:
                            for run in p.runs:
                                run.font.bold = True
                                
            # Remove bold ('b') element from docDefaults so it doesn't affect the rest of the document
            doc_defaults = doc.styles.element.find('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}docDefaults')
            if doc_defaults is not None:
                rpr = doc_defaults.find('.//{http://schemas.openxmlformats.org/wordprocessingml/2006/main}rPr')
                if rpr is not None:
                    b_elem = rpr.find('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}b')
                    if b_elem is not None:
                        rpr.remove(b_elem)
        except Exception as e:
            print(f"Warning while fixing cover page defaults: {e}")
            
        # Copy missing default styles from a default blank document to prevent KeyError
        try:
            default_doc = Document()
            styles_to_copy = ['List Bullet', 'List Number', 'Heading 1', 'Heading 2', 'Heading 3', 'Heading 4', 'Heading 5', 'Heading 6']
            import copy
            for style_name in styles_to_copy:
                if style_name not in doc.styles:
                    if style_name in default_doc.styles:
                        source_style = default_doc.styles[style_name]
                        doc.styles.element.append(copy.copy(source_style.element))
        except Exception as e:
            print(f"Warning while copying default styles: {e}")
    else:
        print("No cover page found, creating new document...")
        doc = Document()
        has_cover = False
    
    # 1. Page setup - Lề chuẩn theo quy định: Top 1.5cm, Bottom 2.0cm, Left 2.5cm, Right 2.0cm
    sections = doc.sections
    for section in sections:
        section.top_margin = Inches(0.59)    # ~1.5 cm
        section.bottom_margin = Inches(0.79) # ~2.0 cm
        section.left_margin = Inches(0.98)   # ~2.5 cm
        section.right_margin = Inches(0.79)  # ~2.0 cm
        section.header_margin = Inches(0.47) # ~1.2 cm
        section.footer_margin = Inches(0.47) # ~1.2 cm
        
    # Set up Abstract Section
    if has_cover:
        section_abstract = doc.add_section(WD_SECTION_START.NEW_PAGE)
        section_abstract.footer.is_linked_to_previous = False
        section_abstract.top_margin = Inches(0.59)
        section_abstract.bottom_margin = Inches(0.79)
        section_abstract.left_margin = Inches(0.98)
        section_abstract.right_margin = Inches(0.79)
        section_abstract.header_margin = Inches(0.47)
        section_abstract.footer_margin = Inches(0.47)
    else:
        section_abstract = doc.sections[0]
        
    set_section_page_numbering(section_abstract, start=1, fmt="lowerRoman")
    add_page_number_to_footer(section_abstract.footer.paragraphs[0])
        
    # 2. Styles setup
    style_normal = doc.styles['Normal']
    font_normal = style_normal.font
    font_normal.name = 'Times New Roman'
    font_normal.size = Pt(13)
    font_normal.color.rgb = RGBColor(0, 0, 0)
    
    with open(md_path, 'r', encoding='utf-8') as f:
        content = f.read()
        
    # Split content by lines
    lines = content.split('\n')
    
    # Determine language
    is_english = False
    if "ABSTRACT" in content or "_en" in md_path:
        is_english = True
        
    # Pre-scan for tables and figures
    tables_list, figures_list = pre_scan_markdown(lines, is_english)
    
    current_section = ""
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        if not line:
            i += 1
            continue
            
        # 1. Parse Table
        if line.startswith('|') and i + 1 < len(lines) and re.match(r'^\s*\|?\s*:?-+:?\s*\|', lines[i+1].strip()):
            # Parse table structure
            headers = [c.strip() for c in line.split('|')[1:-1]]
            col_count = len(headers)
            
            # Skip separator line
            i += 2
            
            rows_data = []
            while i < len(lines) and lines[i].strip().startswith('|'):
                row_cells = [c.strip() for c in lines[i].strip().split('|')[1:-1]]
                if len(row_cells) < col_count:
                    row_cells += [''] * (col_count - len(row_cells))
                elif len(row_cells) > col_count:
                    row_cells = row_cells[:col_count]
                rows_data.append(row_cells)
                i += 1
                
            # Create Table in docx
            is_table_4_8 = False
            if (col_count == 10 and 
                any(h in headers[8] for h in ['Thứ hạng TRAS', 'TRAS Rank']) and 
                any(h in headers[9] for h in ['Điểm TRAS', 'TRAS Score']) and 
                len(rows_data) > 0 and 
                any(term in rows_data[0][0] for term in ['Phân lớp TRAS', 'TRAS Layer'])):
                is_table_4_8 = True

            if is_table_4_8:
                category_row = rows_data.pop(0)
                table = doc.add_table(rows=len(rows_data) + 2, cols=col_count)
            else:
                table = doc.add_table(rows=len(rows_data) + 1, cols=col_count)
            table.style = 'Table Grid'
            
            # Setup helper to strip markdown
            def clean_md(t):
                return re.sub(r'\*\*|__|\*|_', '', t).strip()

            # Populate header
            if is_table_4_8:
                from docx.enum.table import WD_CELL_VERTICAL_ALIGNMENT
                row0_cells = table.rows[0].cells
                row1_cells = table.rows[1].cells
                
                # Perform cell merging first
                row0_cells[0].merge(row1_cells[0]) # Asset column
                row0_cells[1].merge(row1_cells[1]) # Model column
                row0_cells[2].merge(row0_cells[3]).merge(row0_cells[4]).merge(row0_cells[5]) # Layer 1 columns
                row0_cells[8].merge(row1_cells[8]) # TRAS Rank column
                row0_cells[9].merge(row1_cells[9]) # TRAS Score column
                
                # Setup texts on the correct cells
                table.cell(0, 0).text = clean_md(headers[0])
                table.cell(0, 1).text = clean_md(headers[1])
                
                table.cell(0, 2).text = clean_md(category_row[2])
                table.cell(1, 2).text = clean_md(headers[2])
                table.cell(1, 3).text = clean_md(headers[3])
                table.cell(1, 4).text = clean_md(headers[4])
                table.cell(1, 5).text = clean_md(headers[5])
                
                table.cell(0, 6).text = clean_md(category_row[6])
                table.cell(1, 6).text = clean_md(headers[6])
                
                table.cell(0, 7).text = clean_md(category_row[7])
                table.cell(1, 7).text = clean_md(headers[7])
                
                table.cell(0, 8).text = clean_md(headers[8])
                table.cell(0, 9).text = clean_md(headers[9])
                
                # Cells to format
                cells_to_format = [
                    table.cell(0, 0), table.cell(0, 1),
                    table.cell(0, 2),
                    table.cell(1, 2), table.cell(1, 3), table.cell(1, 4), table.cell(1, 5),
                    table.cell(0, 6), table.cell(1, 6),
                    table.cell(0, 7), table.cell(1, 7),
                    table.cell(0, 8), table.cell(0, 9)
                ]
                
                for cell in cells_to_format:
                    cell.vertical_alignment = WD_CELL_VERTICAL_ALIGNMENT.CENTER
                    for paragraph in cell.paragraphs:
                        paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                        paragraph.paragraph_format.space_before = Pt(3)
                        paragraph.paragraph_format.space_after = Pt(3)
                        for run in paragraph.runs:
                            run.font.name = 'Times New Roman'
                            run.font.size = Pt(11)
                            run.font.bold = True
                            run.font.color.rgb = RGBColor(0, 0, 0)
            else:
                hdr_cells = table.rows[0].cells
                for col_idx, header in enumerate(headers):
                    hdr_cells[col_idx].text = header
                    for paragraph in hdr_cells[col_idx].paragraphs:
                        paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                        paragraph.paragraph_format.space_before = Pt(3)
                        paragraph.paragraph_format.space_after = Pt(3)
                        for run in paragraph.runs:
                            run.font.name = 'Times New Roman'
                            run.font.size = Pt(11)
                            run.font.bold = True
                            run.font.color.rgb = RGBColor(0, 0, 0)
                            
            # Populate data
            for row_idx, row_data in enumerate(rows_data):
                start_row = 2 if is_table_4_8 else 1
                row_cells = table.rows[row_idx + start_row].cells
                for col_idx, text in enumerate(row_data):
                    row_cells[col_idx].text = text
                    for paragraph in row_cells[col_idx].paragraphs:
                        # Kiểm tra nếu là số thì căn lề phải, chữ thì căn lề trái
                        if re.match(r'^[\d\s.,\-+%]+$', text.strip()) and len(text.strip()) > 0:
                            paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.RIGHT
                        else:
                            paragraph.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
                        paragraph.paragraph_format.space_before = Pt(3)
                        paragraph.paragraph_format.space_after = Pt(3)
                        paragraph.paragraph_format.line_spacing = 1.15
                        for run in paragraph.runs:
                            run.font.name = 'Times New Roman'
                            run.font.size = Pt(11)
                            run.font.color.rgb = RGBColor(0, 0, 0)
                            
            # Search for table notes or sources in subsequent lines
            table_notes = []
            next_idx = i
            while next_idx < len(lines):
                next_line = lines[next_idx].strip()
                if not next_line:
                    next_idx += 1
                    continue
                if next_line.startswith(('*Chú thích:', '*Note:', '*Nguồn:', '*Source:', 'Chú thích:', 'Note:', 'Nguồn:', 'Source:')):
                    table_notes.append(next_line)
                    next_idx += 1
                    i = next_idx
                else:
                    break
                    
            for note in table_notes:
                p_note = doc.add_paragraph()
                p_note.paragraph_format.space_before = Pt(3)
                p_note.paragraph_format.space_after = Pt(6)
                p_note.paragraph_format.line_spacing = 1.15
                
                # Check alignment
                clean_raw = note.strip()
                # Clean outer markdown formatting (italic/bold) safely without touching internal asterisks
                if clean_raw.startswith('***') and clean_raw.endswith('***'):
                    clean_text = clean_raw[3:-3]
                elif clean_raw.startswith('**') and clean_raw.endswith('**'):
                    clean_text = clean_raw[2:-2]
                elif clean_raw.startswith('__') and clean_raw.endswith('__'):
                    clean_text = clean_raw[2:-2]
                elif clean_raw.startswith('*') and clean_raw.endswith('*'):
                    clean_text = clean_raw[1:-1]
                elif clean_raw.startswith('_') and clean_raw.endswith('_'):
                    clean_text = clean_raw[1:-1]
                else:
                    clean_text = clean_raw
                clean_text = clean_text.strip()
                
                is_source = False
                for prefix in ['*Nguồn:', '_Nguồn:', 'Nguồn:', '*Source:', '_Source:', 'Source:']:
                    if clean_raw.startswith(prefix):
                        is_source = True
                        break
                        
                if is_source:
                    p_note.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.RIGHT
                else:
                    p_note.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
                    
                run_note = p_note.add_run(clean_text)
                run_note.font.name = 'Times New Roman'
                run_note.font.size = Pt(11)
                run_note.font.italic = True
                run_note.font.color.rgb = RGBColor(0, 0, 0)
                
            doc.add_paragraph() # Spacing after table and its notes
            continue
            
        # 2. Parse Image / Figure (Chèn hình ảnh đơn lẻ hoặc cụm hình ảnh dạng Grid)
        img_match = re.match(r'^!\[(.*?)\]\((.*?)\)$', line)
        if img_match:
            # Thu thập toàn bộ các hình ảnh liên tiếp
            image_lines = [(img_match.group(1).strip(), img_match.group(2).strip())]
            
            next_idx = i + 1
            while next_idx < len(lines):
                next_line = lines[next_idx].strip()
                if not next_line:
                    next_idx += 1
                    continue
                next_img_match = re.match(r'^!\[(.*?)\]\((.*?)\)$', next_line)
                if next_img_match:
                    image_lines.append((next_img_match.group(1).strip(), next_img_match.group(2).strip()))
                    next_idx += 1
                else:
                    break
            
            # Cập nhật con trỏ dòng i để nhảy qua các hình đã gom cụm
            i = next_idx - 1
            
            # Tìm nguồn trích dẫn chung của cụm/hình này (nếu có ở dòng tiếp theo)
            source_text = "Nguồn: Kết quả thực nghiệm của nghiên cứu."
            if next_idx < len(lines):
                s_line = lines[next_idx].strip()
                if s_line.startswith(('*Nguồn:', '_Nguồn:', 'Nguồn:', '*Source:', '_Source:', 'Source:')):
                    # Clean outer markdown formatting (italic/bold) safely without touching internal asterisks
                    if s_line.startswith('***') and s_line.endswith('***'):
                        source_text = s_line[3:-3]
                    elif s_line.startswith('**') and s_line.endswith('**'):
                        source_text = s_line[2:-2]
                    elif s_line.startswith('__') and s_line.endswith('__'):
                        source_text = s_line[2:-2]
                    elif s_line.startswith('*') and s_line.endswith('*'):
                        source_text = s_line[1:-1]
                    elif s_line.startswith('_') and s_line.endswith('_'):
                        source_text = s_line[1:-1]
                    else:
                        source_text = s_line
                    source_text = source_text.strip()
                    i += 1 # Bỏ qua dòng nguồn ở vòng lặp ngoài
            
            num_images = len(image_lines)
            
            # TH1: Chỉ có 1 hình đơn lẻ
            if num_images == 1:
                caption, image_uri = image_lines[0]
                local_path = image_uri.replace("file:///", "").replace("/", "\\")
                local_path = os.path.abspath(local_path)
                
                # Tiêu đề trên hình
                p_title = doc.add_paragraph()
                p_title.paragraph_format.space_before = Pt(12)
                p_title.paragraph_format.space_after = Pt(4)
                p_title.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                p_title.paragraph_format.keep_with_next = True
                
                run_title = p_title.add_run(caption)
                run_title.font.name = 'Times New Roman'
                run_title.font.size = Pt(13)
                run_title.font.bold = True
                run_title.font.color.rgb = RGBColor(0, 0, 0)
                
                # Add Bookmark for single figure
                fig_cap_match = re.match(r'^(?:Figure|Hình)\s+([A-Za-z0-9.]+):?\s*(.*)$', caption.strip().strip('*').strip('_').strip())
                if fig_cap_match:
                    fig_num = fig_cap_match.group(1)
                    bm_name = make_bookmark_name("RefFig", fig_num)
                    bookmark_id_counter += 1
                    add_bookmark(p_title, bm_name, bookmark_id_counter)
                
                # Chèn hình ảnh
                p_img = doc.add_paragraph()
                p_img.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                p_img.paragraph_format.space_after = Pt(4)
                try:
                    run_img = p_img.add_run()
                    run_img.add_picture(local_path, width=Inches(5.0))
                except Exception as e:
                    print(f"Error adding picture {local_path}: {e}")
                    p_img.add_run(f"[Hình ảnh không tìm thấy: {caption}]")
                    
                # Nguồn hình dưới
                p_src = doc.add_paragraph()
                p_src.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.RIGHT
                p_src.paragraph_format.space_after = Pt(12)
                
                run_src = p_src.add_run(source_text)
                run_src.font.name = 'Times New Roman'
                run_src.font.size = Pt(11)
                run_src.font.italic = True
                run_src.font.color.rgb = RGBColor(0, 0, 0)
                
            # TH2: Gom cụm hình ảnh theo lưới để tiết kiệm không gian và bố cục đẹp mắt
            else:
                if num_images == 2:
                    rows, cols = 1, 2
                    col_width = Inches(3.0)
                elif num_images == 3:
                    rows, cols = 1, 3
                    col_width = Inches(2.0)
                elif num_images == 4:
                    rows, cols = 2, 2
                    col_width = Inches(3.0)
                elif num_images == 6:
                    rows, cols = 2, 3
                    col_width = Inches(2.0)
                elif num_images == 8:
                    rows, cols = 2, 4
                    col_width = Inches(1.5)
                elif num_images == 9:
                    rows, cols = 3, 3
                    col_width = Inches(2.0)
                elif num_images == 12:
                    rows, cols = 3, 4
                    col_width = Inches(1.5)
                elif num_images == 13:
                    rows, cols = 4, 4
                    col_width = Inches(1.5)
                elif num_images == 16:
                    rows, cols = 4, 4
                    col_width = Inches(1.5)
                else:
                    # Fallback dynamic grid solver
                    if num_images <= 3:
                        rows, cols = 1, num_images
                        col_width = Inches(6.0 / cols)
                    elif num_images <= 6:
                        cols = 3
                        rows = (num_images + cols - 1) // cols
                        col_width = Inches(2.0)
                    elif num_images <= 8:
                        cols = 4
                        rows = (num_images + cols - 1) // cols
                        col_width = Inches(1.5)
                    elif num_images <= 9:
                        cols = 3
                        rows = (num_images + cols - 1) // cols
                        col_width = Inches(2.0)
                    elif num_images <= 12:
                        cols = 4
                        rows = (num_images + cols - 1) // cols
                        col_width = Inches(1.5)
                    elif num_images <= 16:
                        cols = 4
                        rows = (num_images + cols - 1) // cols
                        col_width = Inches(1.5)
                    else:
                        rows, cols = num_images, 1
                        col_width = Inches(5.0)
                
                # Trích xuất tiêu đề chính bằng cách cắt bỏ phần Panel hoặc chữ sau dấu gạch ngang
                first_caption = image_lines[0][0]
                main_title = first_caption
                title_parts = re.split(r'[-–]\s*Panel|:\s*Panel|Panel\s+[A-Za-z]', first_caption, flags=re.IGNORECASE)
                if title_parts:
                    main_title = title_parts[0].strip()
                
                # Đảm bảo tiêu đề chính viết hoa nếu thuộc cấp 1 (nhưng ở đây là tiêu đề Hình nên viết thường in đậm)
                p_title = doc.add_paragraph()
                p_title.paragraph_format.space_before = Pt(12)
                p_title.paragraph_format.space_after = Pt(6)
                p_title.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                p_title.paragraph_format.keep_with_next = True
                
                run_title = p_title.add_run(main_title)
                run_title.font.name = 'Times New Roman'
                run_title.font.size = Pt(13)
                run_title.font.bold = True
                run_title.font.color.rgb = RGBColor(0, 0, 0)
                
                # Add Bookmark for grid figure
                fig_cap_match = re.match(r'^(?:Figure|Hình)\s+([A-Za-z0-9.]+):?\s*(.*)$', main_title.strip().strip('*').strip('_').strip())
                if fig_cap_match:
                    fig_num = fig_cap_match.group(1)
                    bm_name = make_bookmark_name("RefFig", fig_num)
                    bookmark_id_counter += 1
                    add_bookmark(p_title, bm_name, bookmark_id_counter)
                
                # Tạo bảng chứa cụm hình vẽ (không có viền ngoài)
                grid_table = doc.add_table(rows=rows * 2, cols=cols)
                grid_table.autofit = False
                grid_table.alignment = WD_ALIGN_PARAGRAPH.CENTER
                
                # Bỏ viền bảng để ảnh hiển thị liền mạch không ô lưới
                tblPr = grid_table._tbl.tblPr
                tblBorders = tblPr.first_child_found_in("w:tblBorders")
                if tblBorders is not None:
                    tblPr.remove(tblBorders)
                
                # Gán độ rộng cột
                for col in grid_table.columns:
                    col.width = col_width
                    
                img_idx = 0
                for r in range(rows):
                    img_row = grid_table.rows[r * 2]
                    cap_row = grid_table.rows[r * 2 + 1]
                    
                    for c in range(cols):
                        if img_idx >= num_images:
                            break
                        caption, image_uri = image_lines[img_idx]
                        local_path = image_uri.replace("file:///", "").replace("/", "\\")
                        local_path = os.path.abspath(local_path)
                        
                        # Chèn ảnh vào ô
                        cell_img = img_row.cells[c]
                        cell_img.width = col_width
                        p_cell_img = cell_img.paragraphs[0]
                        p_cell_img.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                        p_cell_img.paragraph_format.space_before = Pt(2)
                        p_cell_img.paragraph_format.space_after = Pt(2)
                        
                        try:
                            run_cell_img = p_cell_img.add_run()
                            # Rút nhỏ hơn cột 0.1 inch để tránh bị đẩy dòng
                            run_cell_img.add_picture(local_path, width=col_width - Inches(0.1))
                        except Exception as e:
                            print(f"Error adding picture {local_path} in grid: {e}")
                            p_cell_img.add_run(f"[Hình ảnh lỗi]")
                            
                        # Chèn chú thích phụ (sub-caption, ví dụ: "Panel A: SJC Gold") vào ô bên dưới
                        cell_cap = cap_row.cells[c]
                        cell_cap.width = col_width
                        p_cell_cap = cell_cap.paragraphs[0]
                        p_cell_cap.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
                        p_cell_cap.paragraph_format.space_before = Pt(2)
                        p_cell_cap.paragraph_format.space_after = Pt(4)
                        
                        # Rút gọn chú thích chỉ lấy từ phần "Panel..."
                        sub_cap = caption
                        sub_match = re.search(r'(Panel\s+[A-Za-z]\s*[-:]\s*.*)$', caption, re.IGNORECASE)
                        if sub_match:
                            sub_cap = sub_match.group(1).strip()
                        elif ":" in caption:
                            sub_cap = caption.split(":", 1)[1].strip()
                            
                        run_cell_cap = p_cell_cap.add_run(sub_cap)
                        run_cell_cap.font.name = 'Times New Roman'
                        run_cell_cap.font.size = Pt(10)
                        run_cell_cap.font.italic = True
                        run_cell_cap.font.color.rgb = RGBColor(0, 0, 0)
                        
                        img_idx += 1
                        
                # Ghi nguồn chung ở chân cụm hình
                p_src = doc.add_paragraph()
                p_src.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.RIGHT
                p_src.paragraph_format.space_after = Pt(12)
                
                run_src = p_src.add_run(source_text)
                run_src.font.name = 'Times New Roman'
                run_src.font.size = Pt(11)
                run_src.font.italic = True
                run_src.font.color.rgb = RGBColor(0, 0, 0)
                
            i += 1
            continue
            
        # 3. Parse Headers (Tiêu đề cấp 1 đến 4)
        header_match = re.match(r'^(#{1,6})\s+(.*)$', line)
        if header_match:
            level = len(header_match.group(1))
            title = header_match.group(2)
            
            # Strip markdown bold/italic inside headers
            title = re.sub(r'\*\*|__|\*|_', '', title)
            
            # Tiêu đề cấp 1 phải VIẾT HOA theo quy định của UEH
            if level == 1:
                title = title.upper()
                current_section = title.strip()
                
            # If this is Part 1 (Introduction), start Section 2 (Arabic numbering, resets at 1)
            clean_title = title.upper().strip()
            if level == 1 and (clean_title.startswith("1. GIỚI THIỆU") or clean_title.startswith("1. INTRODUCTION")):
                section2 = doc.add_section(WD_SECTION_START.NEW_PAGE)
                set_section_page_numbering(section2, start=1, fmt="decimal")
                section2.footer.is_linked_to_previous = False
                add_page_number_to_footer(section2.footer.paragraphs[0])
                
            # Sử dụng add_heading của python-docx để tích hợp chuẩn vào Document Map / Table of Contents
            p = doc.add_heading(level=level)
            
            # Căn lề tiêu đề: Cấp 1 canh giữa, còn lại canh trái
            if level == 1:
                p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.CENTER
            else:
                p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.LEFT
                
            p.paragraph_format.space_before = Pt(12)
            p.paragraph_format.space_after = Pt(6)
            p.paragraph_format.keep_with_next = True
            
            run = p.add_run(title)
            run.font.name = 'Times New Roman'
            run.font.color.rgb = RGBColor(0, 0, 0) # Bỏ màu xanh mặc định, đổi thành đen
            
            # Định kích thước chữ theo quy chuẩn
            if level == 1:
                run.font.size = Pt(16)
                run.font.bold = True
            elif level == 2:
                run.font.size = Pt(14)
                run.font.bold = True
            elif level == 3:
                run.font.size = Pt(13)
                run.font.bold = False
                run.font.italic = True
            else:
                run.font.size = Pt(13)
                run.font.bold = False
                run.font.italic = True
                
            i += 1
            continue
            
        # 4. Parse Bullet List Items
        list_match = re.match(r'^[\*\-\+]\s+(.*)$', line)
        if list_match:
            text = list_match.group(1)
            p = doc.add_paragraph(style='List Bullet')
            p.paragraph_format.space_before = Pt(3)
            p.paragraph_format.space_after = Pt(3)
            p.paragraph_format.line_spacing = 1.5
            add_formatted_text(p, text)
            i += 1
            continue
            
        # 5. Parse Numbered List Items
        num_list_match = re.match(r'^(\d+)\.\s+(.*)$', line)
        if num_list_match:
            num = num_list_match.group(1)
            text = num_list_match.group(2)
            p = doc.add_paragraph()
            p.paragraph_format.left_indent = Inches(0.25)
            p.paragraph_format.space_before = Pt(3)
            p.paragraph_format.space_after = Pt(3)
            p.paragraph_format.line_spacing = 1.5
            add_formatted_text(p, f"{num}. {text}")
            i += 1
            continue
            
        # 6. Blockquote / Alert
        if line.startswith('>'):
            text = line.lstrip('>').strip()
            # If next line is also blockquote, read it
            i += 1
            while i < len(lines) and lines[i].strip().startswith('>'):
                text += ' ' + lines[i].strip().lstrip('>').strip()
                i += 1
            # Add blockquote paragraph
            p = doc.add_paragraph()
            p.paragraph_format.left_indent = Inches(0.5)
            p.paragraph_format.space_before = Pt(6)
            p.paragraph_format.space_after = Pt(6)
            p.paragraph_format.line_spacing = 1.5
            
            run = p.add_run(text)
            run.font.name = 'Times New Roman'
            run.font.size = Pt(13)
            run.font.italic = True
            run.font.color.rgb = RGBColor(0, 0, 0)
            continue
            
        # 7. Paragraphs (Văn bản thường)
        clean_line = line.strip()
        if clean_line.startswith('$$') and clean_line.endswith('$$'):
            paragraph_text = line
            i += 1
        else:
            paragraph_text = line
            i += 1
            # Read consecutive lines as part of same paragraph unless it starts a new block
            while i < len(lines):
                next_line = lines[i].strip()
                if not next_line or next_line.startswith('#') or next_line.startswith('|') or next_line.startswith('*') or next_line.startswith('-') or next_line.startswith('>') or re.match(r'^\d+\.', next_line) or next_line.startswith('$$'):
                    break
                paragraph_text += ' ' + next_line
                i += 1
            
        clean_para = paragraph_text.strip()
        if clean_para.startswith('$$') and clean_para.endswith('$$'):
            tag_match = re.match(r'^\$\$(.*?)\\tag\{(.*?)\}\s*\$\$$', clean_para, re.DOTALL)
            if tag_match:
                formula_content = tag_match.group(1).strip()
                tag_content = tag_match.group(2).strip()
                add_numbered_equation(doc, formula_content, tag_content)
            else:
                formula_content = clean_para[2:-2].strip()
                add_unnumbered_equation(doc, formula_content)
            continue
            
        p = doc.add_paragraph()
        p.paragraph_format.space_before = Pt(6)
        p.paragraph_format.space_after = Pt(6)
        p.paragraph_format.line_spacing = 1.5 # Giãn dòng 1.5 lines theo quy chuẩn
        p.paragraph_format.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY # Canh đều (Justify)
        
        # Apply paragraph indentation (First-line indent for body, hanging indent for references)
        should_indent = False
        if current_section:
            match = re.match(r'^([1-8])\.', current_section)
            if match:
                should_indent = True
                
        # Do not indent keywords
        clean_text = paragraph_text.strip()
        if clean_text.startswith(('Từ khóa:', 'Keywords:', '**Từ khóa:**', '**Keywords:**')):
            should_indent = False
            
        if current_section in ["REFERENCES", "TÀI LIỆU THAM KHẢO"]:
            p.paragraph_format.left_indent = Inches(0.5)
            p.paragraph_format.first_line_indent = Inches(-0.5)
        elif should_indent:
            p.paragraph_format.first_line_indent = Inches(0.5)
            
        add_formatted_text(p, paragraph_text)
        
        # Add Bookmark if Table Caption
        clean_para = paragraph_text.strip().strip('*').strip('_').strip()
        table_cap_match = re.match(r'^(?:Table|Bảng)\s+([A-Za-z0-9.]+):?\s*(.*)$', clean_para)
        if table_cap_match:
            tbl_num = table_cap_match.group(1)
            bm_name = make_bookmark_name("RefTable", tbl_num)
            bookmark_id_counter += 1
            add_bookmark(p, bm_name, bookmark_id_counter)
            
        # Check if keywords line to insert the 4 pages
        is_keywords_line = clean_text.startswith(('Từ khóa:', 'Keywords:', '**Từ khóa:**', '**Keywords:**'))
        if is_keywords_line:
            doc.add_page_break()
            
            # 1. Table of Contents
            toc_title = "MỤC LỤC" if not is_english else "TABLE OF CONTENTS"
            add_table_of_contents(doc, toc_title)
            doc.add_page_break()
            
            # 2. List of Abbreviations
            add_list_of_abbreviations(doc, is_english, content)
            doc.add_page_break()
            
            # 3. List of Tables
            add_list_of_tables(doc, tables_list, is_english)
            doc.add_page_break()
            
            # 4. List of Figures
            add_list_of_figures(doc, figures_list, is_english)
        
    set_update_fields_on_open(doc)

    try:
        doc.save(docx_path)
        print(f"Successfully converted {md_path} to {docx_path}")
    except PermissionError:
        print(f"\n[CẢNH BÁO/WARNING]: Không thể ghi đè file {docx_path} do tệp đang được mở trong một chương trình khác (ví dụ: Microsoft Word).")
        print("Vui lòng đóng tệp Word này lại và thực hiện chạy lại câu lệnh.\n")
        sys.exit(1)

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python markdown_to_docx.py <input_md_path> <output_docx_path>")
        sys.exit(1)
    
    input_md = sys.argv[1]
    output_docx = sys.argv[2]
    
    os.makedirs(os.path.dirname(os.path.abspath(output_docx)), exist_ok=True)
    
    parse_markdown_to_docx(input_md, output_docx)
