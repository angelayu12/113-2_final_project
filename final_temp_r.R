library(readr)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(tidyr)

# ------------------ 使用者設定 ------------------
root_dir   <- "."                      
students   <- sprintf("S%d", 1:10)     # S1 ~ S10
tz_local   <- "America/Chicago"        # US/Central (含 DST)
exam_dur   <- 180                      # ✅ 3 小時（Final）
plot_title <- "Final Exam Skin Temperature"  # ✅ 圖標題

# ------------------ 讀檔＋切片 ------------------
extract_exam <- function(sid) {
  # ✅ 資料夾名稱改為 "Final"
  f <- file.path(root_dir, sid, "Final", "TEMP.csv")
  if (!file.exists(f)) {
    warning("❌ 找不到檔案：", f)
    return(NULL)
  }
  
  hdr <- read_lines(f, n_max = 2)
  start_utc <- suppressWarnings(as.numeric(hdr[1]))
  fs        <- suppressWarnings(as.numeric(hdr[2]))
  if (is.na(start_utc) || is.na(fs)) {
    warning("⚠️ 首兩列格式異常 (", sid, ")")
    return(NULL)
  }
  
  temp <- read_csv(f, skip = 2, col_names = FALSE,
                   show_col_types = FALSE)$X1
  
  start_local <- with_tz(as_datetime(start_utc, tz = "UTC"), tz_local)
  exam_start  <- update(start_local, hour = 10, minute = 55, second = 0)
  if (exam_start < start_local) exam_start <- exam_start + days(1)
  
  idx_start <- round(as.numeric(difftime(exam_start, start_local,
                                         units = "secs")) * fs)
  idx_end   <- idx_start + exam_dur * 60 * fs
  idx_start <- max(idx_start, 0)
  idx_end   <- min(idx_end, length(temp))
  if (idx_start >= idx_end) {
    warning("⚠️ ", sid, " 完全沒有覆蓋 9:00–12:00 區段")
    return(NULL)
  }
  
  segment <- temp[(idx_start + 1):idx_end]
  t_min   <- seq(0, by = 1/fs, length.out = length(segment)) / 60
  
  tibble(student  = sid,
         time_min = t_min,
         temp     = segment,
         complete = length(segment) == exam_dur * 60 * fs)
}

# ------------------ 執行並畫圖 ------------------
plot_df <- map_dfr(students, extract_exam)

if (nrow(plot_df) == 0) stop("❌ 沒讀到任何資料，請檢查資料夾結構")

ggplot(plot_df, aes(time_min, temp, colour = student)) +
  scale_color_discrete(breaks = sprintf("S%d", 1:10)) +
  geom_line() +
  scale_x_continuous(
    breaks  = c(0, 60, 120, 180),   # 刻度改成 0‒180
    limits  = c(0, 180),            # x 軸 0‒180 min
    expand  = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(20, 40),
    expand = c(0, 0)
  ) +
  labs(
    x      = "Time since exam start (min)",
    y      = "Skin temperature (°C)",
    title  = plot_title,
    colour = "Student"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# 各學生是否擷取完整 3 小時
plot_df %>% distinct(student, complete)

library(writexl)  # 用來寫出 Excel 檔

# 計算每位學生的統計值
summary_df <- plot_df %>%
  group_by(student) %>%
  summarise(
    stu_no    = student[1],
    exam_type = "final",
    temp_max  = max(temp, na.rm = TRUE),
    temp_min  = min(temp, na.rm = TRUE),
    temp_mean = mean(temp, na.rm = TRUE),
    temp_diff = max(temp, na.rm = TRUE) - min(temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(stu_no, exam_type, temp_max, temp_min, temp_mean, temp_diff)

# 寫出成 Excel 檔案
write_xlsx(summary_df, "final_temp.xlsx")