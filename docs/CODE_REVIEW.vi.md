# Đánh giá mã nguồn và phạm vi phát hành

Ngày đánh giá: 08/11/2026. Phạm vi: source Python/R, cấu hình, test hiện có và khả
năng chạy bản phân phối nguồn trên máy mới. Đây không phải thẩm định toàn bộ kết
quả thực nghiệm hay phản biện học thuật hoàn chỉnh.

## Nhận định

Dự án có giá trị làm portfolio về lập trình nghiên cứu tài chính: kết nối xử lý
dữ liệu, kinh tế lượng, ML và truy vết đầu ra. Phần kiểm soát thời điểm quan sát
nhãn được triển khai rõ hơn nhiều prototype thông thường. Mức sẵn sàng phù hợp
hiện tại là **research prototype có bản demo kiểm chứng được**. Chưa đủ bằng chứng
để mô tả đây là hệ thống giao dịch hay kết quả dự báo có thể triển khai thực tế.

Điểm mạnh được xác nhận trực tiếp trong mã:

- Nhãn huấn luyện phải có `window_end < decision_date`; inner split cũng loại
  nhãn chưa hoàn tất tại ngày bắt đầu validation.
- Imputation và scaling nằm trong pipeline được fit theo tập huấn luyện.
- DCC/aDCC ước lượng toàn mẫu không được dùng làm benchmark dự báo ngoài mẫu.
- Ngày lợi suất tương lai lấy từ ngày giao dịch thật của input thay vì tự tạo
  lịch giả; xác suất lớp được ánh xạ theo `classes_` của estimator.
- Run cuối có thư mục riêng, snapshot source, hash input và trạng thái manifest.

## Vấn đề đã sửa và đóng góp trong 0.1.0

| Mức | Vấn đề | Thay đổi |
|---|---|---|
| Cao | Ngày thiếu/không hợp lệ hoặc trùng trong input có thể lọt vào ghép panel | Kiểm tra từng CSV trước ghép, xác nhận quan hệ một-một |
| Cao | Thiếu cột lõi hoặc giá trị vô hạn gây lỗi sâu trong feature/model | Báo lỗi có tên cột ngay tại bước nạp input |
| Vừa | Một utility dùng đường dẫn tuyệt đối theo máy cá nhân | Tìm gốc dự án theo vị trí file |
| Vừa | Thiếu hợp đồng môi trường và cách chạy độc lập với dữ liệu thị trường | Thêm requirements, demo giả lập, hướng dẫn tái lập, kiểm tra CI |
| Vừa | Workspace lẫn dữ liệu, output và repo tham khảo tải về | Thêm ignore rules và gói ZIP theo danh sách file được chọn rõ ràng |

Các thay đổi kiểm tra input không thay công thức mô hình hoặc sửa trực tiếp dữ
liệu nghiên cứu. Dữ liệu, bài nộp, thư viện PDF và output cũ được giữ tại máy.

## Vấn đề còn lại cần ưu tiên

1. **Diễn giải lợi suất và wealth.** `03_adaptive_hedge_ml.py` dùng đầu vào
   `100*log(P_t/P_{t-1})`, tạo chuỗi `r_target - beta*r_hedge`, trong khi các hàm
   `max_drawdown`, `summarize_strategies` và `04_adaptive_hedge_ml_upgrade.py::summarize_strategies`
   ghép wealth theo dạng simple return `prod(1+r/100)`. Đây là xấp xỉ; tổ hợp
   log-return cũng chưa tự định nghĩa được danh mục tự tài trợ. Cần thống nhất
   quy ước vốn, trọng số và simple return trước khi báo cáo lợi suất tích lũy hay
   drawdown như kết quả giao dịch. Bản phát hành giữ nguyên để không làm thay đổi
   ngầm kết quả nghiên cứu cũ.
2. **Dữ liệu point-in-time.** Lag theo ngày và nhãn chín chưa xác nhận timestamp
   công bố vĩ mô, múi giờ đóng cửa tài sản hoặc phiên bản dữ liệu lịch sử. Các
   kiểm tra hiện tại không chứng minh toàn bộ feature đã sẵn có lúc giao dịch.
3. **Chi phí và tìm kiếm mô hình.** Switch penalty/sensitivity chưa thay thế được
   mô hình turnover, spread và khả năng giao dịch SJC. HAC/block bootstrap chưa
   tự hiệu chỉnh toàn bộ quá trình thử nhiều cấu hình nghiên cứu.
4. **Khả năng bảo trì.** Các script ML phiên bản 03/04/05 phụ thuộc nhau bằng
   dynamic import; nhiều tham số thời gian/candidate còn là hằng số trong Python,
   ngoài cấu hình YAML. Nên tách module theo data/validation/estimators/reporting
   ở bước tiếp theo, kèm regression tests trước refactor.
5. **Bao phủ kiểm thử.** Test hiện có tập trung vào Python ML. R được kiểm tra
   cú pháp; chưa có bộ kiểm thử số học/fixture bao phủ GARCH, DCC và các extension.
   Dependency R chưa được khóa bằng lockfile.

## Bằng chứng kiểm tra

- 21 test gốc đã chạy thành công trước sửa đổi.
- Bổ sung regression tests cho ngày lỗi/trùng, cột lõi thiếu và infinity.
- Demo có preflight, smoke fitting và kiểm tra writers bằng dữ liệu giả lập.
- Kiểm tra cú pháp R không gọi ước lượng; không thể suy ra hội tụ mô hình từ việc
  parse thành công.

Kết quả kiểm tra cuối của bản đóng gói được ghi trong `docs/VALIDATION.md`.
