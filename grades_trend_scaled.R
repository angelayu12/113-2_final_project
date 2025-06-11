library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read_excel("exam_stress.xlsx")

# norm_grade 計算
df <- df %>%
  mutate(
    norm_grade = case_when(
      exam_type %in% c("mid1", "mid2") ~ grades,
      exam_type == "final" ~ grades / 2,
      TRUE ~ NA_real_
    )
  )

# ➤ 設定 stu_no 的排序（避免 duplicated levels）
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

# 繼續處理 norm_pct 與 exam_type 順序
df <- df %>%
  mutate(
    norm_pct = norm_grade / 100,
    exam_type = factor(exam_type, levels = c("mid1", "mid2", "final"))
  )

# 畫圖
ggplot(df, aes(x = exam_type, y = norm_grade, group = stu_no, color = stu_no)) +
  geom_line(alpha = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Each Student’s Trend of Three Exam Scores",
    x     = NULL,
    y     = "Score (out of 100)"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_discrete(name = "student")

# 把三次考試橫向展開
df_wide <- df %>%
  select(stu_no, exam_type, norm_grade) %>%
  pivot_wider(names_from = exam_type, values_from = norm_grade)

# 計算變化
df_progress <- df_wide %>%
  mutate(
    diff_mid1_mid2 = mid2 - mid1,
    diff_mid2_final = final - mid2,
    total_change   = final - mid1,
    trend = case_when(
      diff_mid1_mid2 >= 0 & diff_mid2_final >= 0 ~ "進步",
      diff_mid1_mid2 <= 0 & diff_mid2_final <= 0 ~ "退步",
      TRUE                                       ~ "混合"
    )
  )