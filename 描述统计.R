# ==============================
# Baseline characteristics table
# ==============================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(knitr)
library(kableExtra)

# 读取数据
data <- read_excel("D:/fyp/analysis_final2.xlsx") %>% 
  as.data.frame()

# ==============================
# 1. 数据整理
# ==============================

df <- data %>%
  mutate(
    # outcome group
    htn_group = case_when(
      htn_follow == 0 ~ "No hypertension",
      htn_follow == 1 ~ "Hypertension",
      TRUE ~ NA_character_
    ),
    htn_group = factor(htn_group, levels = c("No hypertension", "Hypertension")),
    
    # Age category
    age_cat = case_when(
      age < 65 ~ "< 65",
      age >= 65 ~ "≥ 65",
      TRUE ~ NA_character_
    ),
    age_cat = factor(age_cat, levels = c("< 65", "≥ 65")),
    
    # Sex
    sex_cat = case_when(
      sex == 0 ~ "Female",
      sex == 1 ~ "Male",
      TRUE ~ NA_character_
    ),
    sex_cat = factor(sex_cat, levels = c("Female", "Male")),
    
    # BMI category
    bmi_cat = case_when(
      bmi < 18.5 ~ "< 18.5",
      bmi >= 18.5 & bmi < 24 ~ "18.5–24",
      bmi >= 24 & bmi < 28 ~ "24–28",
      bmi >= 28 ~ "≥ 28",
      TRUE ~ NA_character_
    ),
    bmi_cat = factor(bmi_cat, levels = c("< 18.5", "18.5–24", "24–28", "≥ 28")),
    
    # Ethnicity / nation
    ethnicity_cat = case_when(
      nation == 1 ~ "Han",
      !is.na(nation) ~ "Minority",
      TRUE ~ NA_character_
    ),
    ethnicity_cat = factor(ethnicity_cat, levels = c("Minority", "Han")),
    
    # Education
    education_cat = case_when(
      education == 1 ~ "No formal education",
      education == 2 ~ "Primary school",
      education >= 3 ~ "High school or above",
      TRUE ~ NA_character_
    ),
    education_cat = factor(
      education_cat,
      levels = c("No formal education", "Primary school", "High school or above")
    ),
    
    # Marital status
    marital_cat = case_when(
      marry == 1 ~ "Married",
      marry == 2 ~ "Divorced",
      !is.na(marry) ~ "Other",
      TRUE ~ NA_character_
    ),
    marital_cat = factor(marital_cat, levels = c("Married", "Divorced", "Other")),
    
    # Residential area
    residence_cat = case_when(
      hrural == 0 ~ "Urban",
      hrural == 1 ~ "Rural",
      TRUE ~ NA_character_
    ),
    residence_cat = factor(residence_cat, levels = c("Urban", "Rural")),
    
    # Smoking status: three-level variable
    smoking_status = case_when(
      smokev == 0 & smoken == 0 ~ "Never smoker",
      smokev == 1 & smoken == 0 ~ "Former smoker",
      smoken == 1 ~ "Current smoker",
      TRUE ~ NA_character_
    ),
    smoking_status = factor(
      smoking_status,
      levels = c("Never smoker", "Former smoker", "Current smoker")
    ),
    
    # Alcohol drinking status: three-level variable
    alcohol_status = case_when(
      drinkev == 0 & drinkl == 0 ~ "Never drinker",
      drinkev == 1 & drinkl == 0 ~ "Former drinker",
      drinkl == 1 ~ "Current drinker",
      TRUE ~ NA_character_
    ),
    alcohol_status = factor(
      alcohol_status,
      levels = c("Never drinker", "Former drinker", "Current drinker")
    ),
    
    # Sleep duration
    sleep_cat = case_when(
      sleep_night < 6 ~ "< 6",
      sleep_night >= 6 & sleep_night < 8 ~ "6–8",
      sleep_night >= 8 ~ "≥ 8",
      TRUE ~ NA_character_
    ),
    sleep_cat = factor(sleep_cat, levels = c("< 6", "6–8", "≥ 8")),
    
    # Number of chronic diseases
    chronic_cat = case_when(
      chronic == 0 ~ "0",
      chronic == 1 ~ "1",
      chronic >= 2 ~ "≥ 2",
      TRUE ~ NA_character_
    ),
    chronic_cat = factor(chronic_cat, levels = c("0", "1", "≥ 2")),
    
    # Binary diseases
    diabetes_cat = case_when(
      diabete == 0 ~ "No",
      diabete == 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    heart_cat = case_when(
      heart == 0 ~ "No",
      heart == 1 ~ "Yes",
      TRUE ~ NA_character_
    ),
    stroke_cat = case_when(
      stroke == 0 ~ "No",
      stroke == 1 ~ "Yes",
      TRUE ~ NA_character_
    )
  )

# ==============================
# 2. 定义生成 n (%) 的函数
# ==============================

format_n_pct <- function(n, denom) {
  pct <- round(n / denom * 100, 1)
  paste0(format(n, big.mark = ","), " (", pct, "%)")
}

# 单个变量生成表格
make_cat_table <- function(data, var, var_label) {
  
  x <- data[[var]]
  group <- data$htn_group
  
  # 总人数
  n_total <- nrow(data %>% filter(!is.na(.data[[var]])))
  n_no <- nrow(data %>% filter(htn_group == "No hypertension", !is.na(.data[[var]])))
  n_yes <- nrow(data %>% filter(htn_group == "Hypertension", !is.na(.data[[var]])))
  
  # p value
  tab_p <- table(x, group, useNA = "no")
  
  p_value <- tryCatch({
    if (all(dim(tab_p) > 1)) {
      chisq.test(tab_p)$p.value
    } else {
      NA
    }
  }, error = function(e) NA)
  
  p_value_fmt <- case_when(
    is.na(p_value) ~ "",
    p_value < 0.001 ~ "< 0.001",
    p_value > 0.9 ~ "> 0.9",
    TRUE ~ sprintf("%.3f", p_value)
  )
  
  # 每个 level 的人数和百分比
  levels_x <- levels(as.factor(x))
  
  out <- map_dfr(levels_x, function(lv) {
    
    n_overall <- sum(x == lv, na.rm = TRUE)
    n_no_htn <- sum(x == lv & group == "No hypertension", na.rm = TRUE)
    n_htn <- sum(x == lv & group == "Hypertension", na.rm = TRUE)
    
    tibble(
      Variable = lv,
      Overall = format_n_pct(n_overall, n_total),
      `No hypertension` = format_n_pct(n_no_htn, n_no),
      Hypertension = format_n_pct(n_htn, n_yes),
      P = ""
    )
  })
  
  # 插入变量名行
  header <- tibble(
    Variable = var_label,
    Overall = "",
    `No hypertension` = "",
    Hypertension = "",
    P = p_value_fmt
  )
  
  bind_rows(header, out)
}

# ==============================
# 3. 生成所有变量的 baseline table
# ==============================

baseline_table <- bind_rows(
  make_cat_table(df, "age_cat", "Age, years"),
  make_cat_table(df, "sex_cat", "Sex"),
  make_cat_table(df, "bmi_cat", "BMI, kg/m²"),
  make_cat_table(df, "ethnicity_cat", "Ethnicity"),
  make_cat_table(df, "education_cat", "Education"),
  make_cat_table(df, "marital_cat", "Marital status"),
  make_cat_table(df, "residence_cat", "Residential area"),
  make_cat_table(df, "smoking_status", "Smoking status"),
  make_cat_table(df, "alcohol_status", "Alcohol drinking status"),
  make_cat_table(df, "sleep_cat", "Sleep duration, h/night"),
  make_cat_table(df, "chronic_cat", "Number of chronic diseases"),
  make_cat_table(df, "diabetes_cat", "Diabetes"),
  make_cat_table(df, "heart_cat", "Heart disease"),
  make_cat_table(df, "stroke_cat", "Stroke")
)

# 查看结果
baseline_table
