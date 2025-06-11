library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 讀取資料
df <- read_excel("exam_stress.xlsx")

# 標準化成績
df <- df %>%
  mutate(
    norm_grade = case_when(
      exam_type %in% c("mid1", "mid2") ~ grades,
      exam_type == "final" ~ grades / 2,
      TRUE ~ NA_real_
    )
  )

# 設定學生順序
student_levels <- df %>%
  distinct(stu_no) %>%
  mutate(
    stu_no_num = as.numeric(gsub("[^0-9]", "", stu_no))
  ) %>%
  arrange(stu_no_num) %>%
  pull(stu_no)

df <- df %>%
  mutate(
    stu_no = factor(stu_no, levels = student_levels)
  )

# exam_type 順序 & 百分比
df <- df %>%
  mutate(
    norm_pct = norm_grade / 100,
    exam_type = factor(exam_type, levels = c("mid1", "mid2", "final"))
  )

# ➤ 找出每位學生的趨勢
df_wide <- df %>%
  select(stu_no, exam_type, norm_grade) %>%
  pivot_wider(names_from = exam_type, values_from = norm_grade)

df_trend <- df_wide %>%
  mutate(
    diff1 = mid2 - mid1,
    diff2 = final - mid2,
    trend = case_when(
      diff1 >= 0 & diff2 >= 0 ~ "improve",
      diff1 <= 0 & diff2 <= 0 ~ "decline",
      TRUE ~ "mix type"
    )
  ) %>%
  select(stu_no, trend)

# ➤ 把 trend 加回原始長格式資料
df_plot <- df %>%
  left_join(df_trend, by = "stu_no")

# ➤ 用 trend 當作顏色分組畫圖
# 產生 final 的資料並加入一點 x 偏移
df_final <- df_plot %>%
  filter(exam_type == "final") %>%
  mutate(x_nudge = as.numeric(exam_type) + 0.1)  # 偏移 0.1，數值可調整

p <- ggplot(df_plot, aes(x = exam_type, y = norm_grade, group = stu_no, color = trend)) +
  geom_line(alpha = 0.8, size = 1) +
  geom_point(size = 2) +
  geom_text(
    data = df_final,
    aes(x = x_nudge, label = stu_no),  # 用偏移後的 x 值
    hjust = 0,  # 左對齊，讓文字更靠右顯示
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Student Exam Score Trends (by Improvement Type)",
    x     = NULL,
    y     = "Score (out of 100)",
    color = "Trend"
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.3))) +  # 加大右側空間
  theme_minimal(base_size = 13)

print(p)