library(readxl)
library(dplyr)
library(tidyr)
library(ez)
library(emmeans)
library(ggplot2)
library(ggpubr)

# 2. 讀入資料
df <- read_excel("exam_stress.xlsx")

# 3. 整理資料格式
df <- df %>%
  mutate(
    exam_type = factor(exam_type, levels = c("mid1", "mid2", "final"), labels = c("Mid1", "Mid2", "Final")),
    stu_no = as.factor(stu_no)
  )

# 4. 選取數值變數（加入 grades_rescaled）
numeric_vars <- df %>%
  select(-stu_no, -exam_type, -grades) %>%  # 保留 grades_rescaled
  select(where(is.numeric)) %>%
  names()

# 5. 分析與畫圖
anova_results <- list()
significant_vars <- c()

format_num <- function(x) {
  if (is.numeric(x)) format(round(x, 3), nsmall = 3, scientific = FALSE) else x
}

for (var in numeric_vars) {
  cat("\n==============================\n")
  cat("變數:", var, "\n")
  
  df_temp <- df %>% rename(DV = all_of(var))
  
  result <- ezANOVA(
    data = df_temp,
    dv = DV,
    wid = stu_no,
    within = exam_type,
    detailed = TRUE,
    type = 3
  )
  
  # 儲存格式化 ANOVA 結果
  anova_table <- result$ANOVA
  anova_table[] <- lapply(anova_table, format_num)
  print(anova_table)
  anova_results[[var]] <- anova_table
  
  # 如果顯著，加入清單
  raw_p <- as.numeric(result$ANOVA$p[2])
  if (!is.na(raw_p) && raw_p < 0.05) {
    significant_vars <- c(significant_vars, var)
  }
  
  # 取得 ANOVA p 值並格式化
  anova_p <- paste0("ANOVA, p = ", format(round(raw_p, 2), nsmall = 2))
  
  # 繪製 boxplot
  plot <- ggboxplot(df, x = "exam_type", y = var, color = "exam_type",
                    palette = "jco", add = "jitter", width = 0.3, title = var) +
    annotate("text",
             x = 3.2,
             y = max(df[[var]], na.rm = TRUE) * 1.15,
             label = anova_p,
             hjust = 0, size = 4, fontface = "italic") +
    stat_compare_means(comparisons = list(c("Mid1", "Mid2"), c("Mid2", "Final"), c("Mid1", "Final")),
                       method = "t.test", paired = TRUE, p.adjust.method = "holm") +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
    theme(
      legend.position = "none",              # 移除 exam_type 圖例
      axis.text.x = element_text(size = 11),
      plot.title = element_text(size = 14, face = "bold"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  
  # 儲存圖檔
  ggsave(filename = paste0("boxplot_", var, ".png"), plot = plot, width = 12, height = 6, dpi = 300)
}