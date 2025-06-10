#修改第13行，可以看不同考試、不同生理指標的開始記錄時間

library(readr)
library(lubridate)
library(dplyr)
library(purrr)

root_dir <- "."
students <- sprintf("S%d", 1:10)
tz_local <- "America/Chicago"

check_files <- function(sid) {
  f <- file.path(root_dir, sid, "Final", "temp.csv")
  
  if (!file.exists(f)) {
    return(tibble(
      student = sid,
      file_exists = FALSE,
      start_local = NA
    ))
  }
  
  hdr <- tryCatch(read_lines(f, n_max = 1), error = function(e) NA_character_)
  ts_num <- suppressWarnings(as.numeric(hdr[1]))
  
  start_time <- if (!is.na(ts_num)) {
    tryCatch(as_datetime(ts_num, tz = "UTC") %>% with_tz(tz_local), error = function(e) NA)
  } else {
    NA
  }
  
  tibble(
    student = sid,
    file_exists = TRUE,
    start_local = start_time
  )
}

result <- map_dfr(students, check_files)
print(result)
