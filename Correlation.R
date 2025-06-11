# 套件
library(readxl)
library(dplyr)
library(broom)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)

# 1. 讀取資料
df <- read_excel("exam_stress.xlsx")

# 2. 整理格式
df <- df %>%
  mutate(
    exam_type = factor(exam_type, levels = c("mid1", "mid2", "final")),
    stu_no = as.factor(stu_no)
  )

# 3. 要分析的變數（排除 stu_no, exam_type, grades, grades_rescaled）
vars_to_analyze <- df %>%
  select(-stu_no, -exam_type, -grades, -grades_rescaled) %>%
  select(where(is.numeric)) %>%
  names()

# 4. 儲存所有相關性結果
all_corr_results <- list()

# 5. 主迴圈
for (var in vars_to_analyze) {
  message("處理變數: ", var)
  
  # === 相關分析 ===
  # 整體
  cor_overall <- cor.test(df[[var]], df$grades_rescaled, method = "pearson")
  tbl_overall <- broom::tidy(cor_overall) %>%
    mutate(exam_type = "all", variable = var) %>%
    select(variable, exam_type, estimate, p.value, conf.low, conf.high)
  
  # 分組
  tbl_grouped <- df %>%
    group_by(exam_type) %>%
    summarise(
      result = list(broom::tidy(cor.test(.data[[var]], grades_rescaled, method = "pearson"))),
      .groups = "drop"
    ) %>%
    unnest(result) %>%
    mutate(variable = var) %>%
    select(variable, exam_type, estimate, p.value, conf.low, conf.high)
  
  # 合併
  corr_tbl <- bind_rows(tbl_overall, tbl_grouped)
  all_corr_results[[var]] <- corr_tbl
  
  # === 圖形 ===
  # 整體相關結果
  p_raw <- tbl_overall$p.value
  sig_label <- ifelse(p_raw < 0.001, "***",
                      ifelse(p_raw < 0.01, "**",
                             ifelse(p_raw < 0.05, "*", "ns")))
  r_val <- round(tbl_overall$estimate, 2)
  p_val <- format.pval(p_raw, digits = 2, eps = 0.001)
  annotation_text <- paste0("r = ", r_val, ", p = ", p_val, " ", sig_label)
  
  # 畫圖
  p <- ggplot(df, aes_string(x = var, y = "grades_rescaled", color = "exam_type")) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(aes(group = 1), method = "lm", color = "black", linetype = "solid") +
    annotate("text", x = Inf, y = Inf, label = annotation_text,
             hjust = 1.1, vjust = 1.5, size = 4.5, fontface = "italic") +
    labs(
      title = paste("Rescaled Grades vs", var),
      x = var,
      y = "Rescaled Grades",
      color = "Exam Type"
    ) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # 儲存圖檔
  ggsave(
    filename = paste0("scatter_", str_replace_all(var, "[^A-Za-z0-9_]", "_"), ".png"),
    plot = p,
    width = 6, height = 5, dpi = 300,
    bg = "white"
  )
}

# 6. 合併所有統計表
final_corr_df <- bind_rows(all_corr_results)

# 7. 顯示結果
print(final_corr_df)

# 8. 可選：輸出為 CSV
write.csv(final_corr_df, "correlation_summary.csv", row.names = FALSE)