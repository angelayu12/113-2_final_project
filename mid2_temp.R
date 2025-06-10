library(ggplot2)

# 1. 設定學生資料夾的根目錄：與專案資料夾同層
base_dir <- "./"

# 2. 學生資料夾名稱 S1～S10
students <- paste0("S", 1:10)

# 3. 建立一個空的資料框
all_data <- data.frame()

# 4. 逐一讀取 TEMP.csv 並處理
for (student in students) {
  # 組出完整路徑
  temp_path <- file.path(base_dir, student, "Midterm 2", "TEMP.csv")
  
  # 檢查檔案是否存在
  if (!file.exists(temp_path)) {
    message("找不到檔案：", temp_path)
    next
  }
  
  # 嘗試讀取資料
  df <- tryCatch(
    read.csv(temp_path, header = FALSE, stringsAsFactors = FALSE),
    error = function(e) {
      message("讀檔錯誤：", temp_path)
      return(NULL)
    }
  )
  if (is.null(df)) next
  
  # 轉換第 3 列之後的欄位為數值
  temps <- suppressWarnings(as.numeric(df[-c(1, 2), 1]))
  
  # 如果全部轉換成 NA，代表資料可能有問題
  if (all(is.na(temps))) {
    message("資料無效或格式錯誤：", temp_path)
    next
  }
  
  # 建立樣本資料框
  temp_df <- data.frame(
    SampleIndex = seq_along(temps) - 1,
    Temperature = temps,
    Student = student,
    stringsAsFactors = FALSE
  )
  
  # 合併進總資料框
  all_data <- rbind(all_data, temp_df)
}

# 5. 確認資料存在
if (nrow(all_data) == 0) {
  stop("沒有有效資料可繪圖，請確認檔案是否存在且格式正確。")
}

# 指定學生順序
student_order <- paste0("S", 1:10)

# 將 Student 欄位轉換成有順序的 factor
all_data$Student <- factor(all_data$Student, levels = student_order)

# 6. 繪圖
p <- ggplot(data = all_data, aes(x = SampleIndex, y = Temperature, color = Student)) +
  geom_line(size = 0.7) +
  scale_x_continuous(breaks = seq(0, max(all_data$SampleIndex), by = 10000)) +
  labs(
    title = "Midterm 2 Temperature Time Series",
    x = "Sample Index",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# 7. 儲存圖檔
ggsave("midterm2_temperature_plot.png", plot = p, width = 15, height = 6, dpi = 300)

# 8. 顯示圖表
print(p)