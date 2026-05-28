library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# 读取数据
data <- read_excel("D:/fyp/analysis_final2.xlsx") %>% 
  as.data.frame()

# 污染物变量
pollutants <- c(
  "pm25_1113",
  "pm10_1113",
  "pm1_1113",
  "NO2_1113",
  "SO2_1113",
  "CO_1113",
  "O3_1113"
)

# 污染物显示名称
pollutant_labels <- c(
  pm25_1113 = "PM$_{2.5}$",
  pm10_1113 = "PM$_{10}$",
  pm1_1113  = "PM$_1$",
  NO2_1113  = "NO$_2$",
  SO2_1113  = "SO$_2$",
  CO_1113   = "CO",
  O3_1113   = "O$_3$"
)

# 整理 outcome 分组
df <- data %>%
  mutate(
    htn_group = case_when(
      htn_follow == 0 ~ "No hypertension",
      htn_follow == 1 ~ "Incident hypertension",
      TRUE ~ NA_character_
    ),
    htn_group = factor(
      htn_group,
      levels = c("No hypertension", "Incident hypertension")
    )
  )

# mean (SD) 格式函数
mean_sd <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  sprintf("%.2f (%.2f)", m, s)
}

# 生成污染物描述统计表
pollutant_table <- lapply(pollutants, function(v) {
  
  overall <- mean_sd(df[[v]])
  
  no_htn <- df %>%
    filter(htn_group == "No hypertension") %>%
    summarise(value = mean_sd(.data[[v]])) %>%
    pull(value)
  
  htn <- df %>%
    filter(htn_group == "Incident hypertension") %>%
    summarise(value = mean_sd(.data[[v]])) %>%
    pull(value)
  
  # t-test p value
  p_value <- tryCatch({
    t.test(df[[v]] ~ df$htn_group)$p.value
  }, error = function(e) NA)
  
  p_fmt <- case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "< 0.001",
    TRUE ~ sprintf("%.3f", p_value)
  )
  
  data.frame(
    Pollutant = pollutant_labels[[v]],
    Overall = overall,
    `No hypertension` = no_htn,
    `Incident hypertension` = htn,
    P = p_fmt,
    check.names = FALSE
  )
}) %>%
  bind_rows()

# 查看表格
print(pollutant_table)

n_all <- nrow(df)
n_no <- sum(df$htn_group == "No hypertension", na.rm = TRUE)
n_yes <- sum(df$htn_group == "Incident hypertension", na.rm = TRUE)

latex_pollutant_table <- pollutant_table %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    col.names = c(
      "Pollutant",
      paste0("Overall \\\\ N = ", format(n_all, big.mark = ",")),
      paste0("No hypertension \\\\ N = ", format(n_no, big.mark = ",")),
      paste0("Incident hypertension \\\\ N = ", format(n_yes, big.mark = ",")),
      "P"
    ),
    caption = "Three-year average air pollutant concentrations by incident hypertension status."
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 8
  )

latex_pollutant_table
