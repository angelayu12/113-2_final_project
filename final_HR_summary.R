library(tidyverse)
library(lubridate)
library(writexl)

# 學生編號
students <- paste0("S", 1:10)

# 儲存所有資料
all_data <- list()

# 讀取每位學生的 HR 資料
for (student in students) {
  file_path <- file.path(student, "Final", "HR.csv")
  
  if (!file.exists(file_path)) {
    warning(paste("找不到檔案：", file_path))
    next
  }
  
  lines <- readLines(file_path)
  start_time <- as.numeric(lines[1])
  sample_rate <- as.numeric(lines[2])
  hr_data <- read.csv(file_path, skip = 2, header = FALSE)
  colnames(hr_data) <- c("HR")
  
  time_seq <- start_time + seq(0, by = 1/sample_rate, length.out = nrow(hr_data))
  time_seq <- as.POSIXct(time_seq, origin = "1970-01-01", tz = "America/Chicago")
  
  df <- data.frame(Time = time_seq, HR = hr_data$HR, Student = student)
  all_data[[student]] <- df
}

# 合併所有學生資料
combined_data <- bind_rows(all_data)
combined_data$Student <- factor(combined_data$Student, levels = students)

# 考試時間範圍：取每位學生當天的 10:55 起 + 3 小時
reference_day <- as.Date(min(combined_data$Time))
exam_start <- as.POSIXct(paste0(reference_day, " 10:55:00"), tz = "America/Chicago")
exam_end <- exam_start + hours(3)

# 篩選時間區間內的資料，並計算經過分鐘數
filtered_data <- combined_data %>%
  filter(Time >= exam_start & Time <= exam_end) %>%
  mutate(Time_Minutes = as.numeric(difftime(Time, exam_start, units = "mins")))

# 🔹繪圖：小分面圖，每位學生一格
ggplot(filtered_data, aes(x = Time_Minutes, y = HR)) +
  geom_line(color = "red") +
  facet_wrap(~ Student, scales = "free_x") +
  labs(
    title = "Heart Rate from Final Exam - Each Student",
    x = "Time since exam start (min)",
    y = "Heart Rate (BPM)"
  ) +
  theme_minimal()

# 🔹統計計算（含 HR_diff）
final_summary <- filtered_data %>%
  group_by(Student) %>%
  summarise(
    HR_max = max(HR, na.rm = TRUE),
    HR_min = min(HR, na.rm = TRUE),
    HR_mean = mean(HR, na.rm = TRUE),
    HR_diff = HR_max - HR_min,
    .groups = "drop"
  ) %>%
  mutate(
    stu_no = Student,
    exam_type = "final"
  ) %>%
  select(stu_no, exam_type, HR_max, HR_min, HR_mean, HR_diff)

# 匯出成 Excel
write_xlsx(final_summary, "Final_HR_Summary.xlsx")