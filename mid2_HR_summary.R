library(tidyverse)
library(lubridate)
library(writexl)

students <- paste0("S", 1:10)
summary_data <- list()
plot_data_all <- list()

for (student in students) {
  file_path <- file.path(student, "Midterm 2", "HR.csv")
  
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
  
  valid_idx <- hour(time_seq) >= 9 & hour(time_seq) <= 10 &
    ((hour(time_seq) == 9) | (hour(time_seq) == 10 & minute(time_seq) <= 30))
  
  filtered_hr <- hr_data$HR[valid_idx]
  filtered_time <- time_seq[valid_idx]
  
  plot_data_all[[student]] <- data.frame(
    Student = student,
    Time = filtered_time,
    HR = filtered_hr
  )
  
  # 統計值：加入 HR_diff
  max_hr <- max(filtered_hr, na.rm = TRUE)
  min_hr <- min(filtered_hr, na.rm = TRUE)
  mean_hr <- mean(filtered_hr, na.rm = TRUE)
  hr_diff <- max_hr - min_hr
  
  summary_data[[student]] <- data.frame(
    Student = student,
    Exam = "Mid2",
    HR_max = max_hr,
    HR_min = min_hr,
    HR_mean = mean_hr,
    HR_diff = hr_diff
  )
}

plot_df <- bind_rows(plot_data_all)
summary_df <- bind_rows(summary_data)

# 畫圖：每位學生一格
ggplot(plot_df, aes(x = Time, y = HR)) +
  geom_line(color = "red") +
  facet_wrap(~ Student, scales = "free_x") +
  labs(title = "Heart Rate from Midterm2 - Each Student",
       x = "Time", y = "Heart Rate (HR)") +
  theme_minimal()

# 匯出統計值
write_xlsx(summary_df, "Midterm2_HR_Summary.xlsx")