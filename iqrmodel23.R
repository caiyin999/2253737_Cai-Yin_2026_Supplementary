library(lme4)
library(dplyr)

#-----------------------------
# 1. 变量准备
#-----------------------------
airpollutants <- c(
  "pm25_1113", "pm10_1113", "pm1_1113",
  "NO2_1113", "SO2_1113", "CO_1113", "O3_1113"
)

#-----------------------------
# 2. Model 2: per IQR increase
#-----------------------------
run_model2_iqr <- function(pollutant_name) {
  
  dat <- analysis %>%
    select(
      htn_follow, age, sex, bmi, nation, education, marry, hrural, city,
      all_of(pollutant_name)
    ) %>%
    na.omit()
  
  iqr_value <- IQR(dat[[pollutant_name]], na.rm = TRUE)
  
  new_var <- paste0(pollutant_name, "_iqr")
  dat[[new_var]] <- dat[[pollutant_name]] / iqr_value
  
  model2_iqr <- glmer(
    as.formula(
      paste0(
        "htn_follow ~ 1 + ", new_var,
        " + age + sex + bmi + nation + education + marry + hrural + (1 | city)"
      )
    ),
    data = dat,
    family = binomial(link = "logit")
  )
  
  coef_table <- summary(model2_iqr)$coefficients
  
  beta <- coef_table[new_var, "Estimate"]
  se   <- coef_table[new_var, "Std. Error"]
  p    <- coef_table[new_var, "Pr(>|z|)"]
  
  OR  <- exp(beta)
  LCL <- exp(beta - 1.96 * se)
  UCL <- exp(beta + 1.96 * se)
  
  out <- data.frame(
    Pollutant = pollutant_name,
    N = nrow(dat),
    IQR = round(iqr_value, 3),
    OR = round(OR, 3),
    CI_lower = round(LCL, 3),
    CI_upper = round(UCL, 3),
    P_value = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    check.names = FALSE
  )
  
  return(out)
}

#-----------------------------
# 3. 批量运行 7 个污染物
#-----------------------------
result_model2_iqr <- do.call(
  rbind,
  lapply(airpollutants, run_model2_iqr)
)

#-----------------------------
# 4. 整理成论文表格格式
#-----------------------------
result_model2_iqr$`OR (95% CI)` <- paste0(
  sprintf("%.3f", result_model2_iqr$OR),
  " (",
  sprintf("%.3f", result_model2_iqr$CI_lower),
  ", ",
  sprintf("%.3f", result_model2_iqr$CI_upper),
  ")"
)

result_model2_iqr$Pollutant <- c("PM2.5", "PM10", "PM1", "NO2", "SO2", "CO", "O3")

result_table_model2 <- result_model2_iqr %>%
  select(Pollutant, N, IQR, `OR (95% CI)`, P_value)

print(result_table_model2)




iqr_co <- IQR(analysis$CO_1113, na.rm = TRUE)
analysis$CO_1113_iqr <- analysis$CO_1113 / iqr_co

model2_co_iqr <- glmer(
  htn_follow ~ 1 + CO_1113_iqr + age + sex + bmi + nation + education + marry + hrural + (1 | city),
  data = analysis,
  family = binomial(link = "logit")
)

summary(model2_co_iqr)

# 提取固定效应结果
sm <- summary(model2_co_iqr)$coefficients

# 计算 OR 和 95% CI
result <- data.frame(
  Variable = rownames(sm),
  OR = exp(sm[, "Estimate"]),
  CI_lower = exp(sm[, "Estimate"] - 1.96 * sm[, "Std. Error"]),
  CI_upper = exp(sm[, "Estimate"] + 1.96 * sm[, "Std. Error"]),
  p_value = sm[, "Pr(>|z|)"]
)

print(result)



#-----------------------------
# 2. Model 3: per IQR increase
#-----------------------------
run_model3_iqr <- function(pollutant_name) {
  
  dat <- analysis %>%
    select(
      htn_follow, age, sex, bmi, nation, education, marry, hrural,
      smokev, smoken, drinkev, drinkl, pollutant, sleep_night,
      diabete, heart, stroke, chronic, city,
      all_of(pollutant_name)
    ) %>%
    na.omit()
  
  iqr_value <- IQR(dat[[pollutant_name]], na.rm = TRUE)
  
  new_var <- paste0(pollutant_name, "_iqr")
  dat[[new_var]] <- dat[[pollutant_name]] / iqr_value
  
  model3_iqr <- glmer(
    as.formula(
      paste0(
        "htn_follow ~ 1 + ", new_var,
        " + age + sex + bmi + nation + education + marry + hrural",
        " + smokev + smoken + drinkev + drinkl + pollutant",
        " + sleep_night + diabete + heart + stroke + chronic",
        " + (1 | city)"
      )
    ),
    data = dat,
    family = binomial(link = "logit")
  )
  
  coef_table <- summary(model3_iqr)$coefficients
  
  beta <- coef_table[new_var, "Estimate"]
  se   <- coef_table[new_var, "Std. Error"]
  p    <- coef_table[new_var, "Pr(>|z|)"]
  
  OR  <- exp(beta)
  LCL <- exp(beta - 1.96 * se)
  UCL <- exp(beta + 1.96 * se)
  
  out <- data.frame(
    Pollutant = pollutant_name,
    N = nrow(dat),
    IQR = round(iqr_value, 3),
    OR = round(OR, 3),
    CI_lower = round(LCL, 3),
    CI_upper = round(UCL, 3),
    P_value = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    check.names = FALSE
  )
  
  return(out)
}

#-----------------------------
# 3. 批量运行 7 个污染物
#-----------------------------
result_model3_iqr <- do.call(
  rbind,
  lapply(airpollutants, run_model3_iqr)
)

#-----------------------------
# 4. 整理成论文表格格式
#-----------------------------
result_model3_iqr$`OR (95% CI)` <- paste0(
  sprintf("%.3f", result_model3_iqr$OR),
  " (",
  sprintf("%.3f", result_model3_iqr$CI_lower),
  ", ",
  sprintf("%.3f", result_model3_iqr$CI_upper),
  ")"
)

result_model3_iqr$Pollutant <- c("PM2.5", "PM10", "PM1", "NO2", "SO2", "CO", "O3")

result_table_model3 <- result_model3_iqr %>%
  select(Pollutant, N, IQR, `OR (95% CI)`, P_value)

print(result_table_model3)
