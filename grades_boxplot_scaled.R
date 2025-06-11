library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

df <- read_excel("exam_stress.xlsx")

df <- df %>%
  mutate(
    norm_grade = case_when(
      exam_type %in% c("mid1", "mid2") ~ grades,    # 本來就是 0–100
      exam_type == "final"                      ~ grades / 2, # 將 0–200 轉成 0–100
      TRUE                                       ~ NA_real_
    )
  )

df <- df %>%
  mutate(
    norm_pct = norm_grade / 100  # 轉成 0–1 之間
  )

# 確保 exam_type 的順序還是 midterm1 → midterm2 → final
df <- df %>%
  mutate(
    exam_type = factor(exam_type, 
                       levels = c("mid1", "mid2", "final"))  
  )

ggplot(df, aes(x = exam_type, y = norm_grade)) +
  geom_boxplot(fill = "#87CEFA", color = "#1E90FF", outlier.color = "red") +
  labs(
    title = "Distribution of Exam Scores (Scaled to 100)",
    x     = NULL,
    y     = "Score (out of 100)"
  ) +
  theme_minimal(base_size = 14)

summary_stats <- df %>%
  group_by(exam_type) %>%
  summarise(
    min = min(norm_grade, na.rm = TRUE),
    Q1 = quantile(norm_grade, 0.25, na.rm = TRUE),
    median = median(norm_grade, na.rm = TRUE),
    Q3 = quantile(norm_grade, 0.75, na.rm = TRUE),
    max = max(norm_grade, na.rm = TRUE),
    IQR = IQR(norm_grade, na.rm = TRUE),
    sd  = sd(norm_grade, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

