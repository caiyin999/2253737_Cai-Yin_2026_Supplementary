library(lme4)
library(dplyr)

airpollutants <- c("pm25_1113", "pm10_1113", "pm1_1113", "NO2_1113", "SO2_1113", "CO_1113", "O3_1113")

# 计算 O3 的 IQR
iqr_pm25 <- IQR(analysis$pm25_1113, na.rm = TRUE)

# 构造每增加一个 IQR 的变量
analysis$pm25_1113_iqr <- analysis$pm25_1113 / iqr_pm25

# 建模
simple_model_iqr <- glmer(
  htn_follow ~ 1 + pm25_1113_iqr + age + sex + bmi + (1 | city),
  data = analysis,
  family = binomial(link = "logit")
)

summary(simple_model_iqr)


# 提取固定效应系数
beta <- fixef(simple_model_iqr)

# 计算 OR
OR <- exp(beta)

OR

# 提取标准误
se <- sqrt(diag(vcov(simple_model_iqr)))

# 计算95%CI
lower <- exp(beta - 1.96 * se)
upper <- exp(beta + 1.96 * se)

# 合并结果
result <- data.frame(
  Variable = names(beta),
  OR = OR,
  CI_lower = lower,
  CI_upper = upper
)

result




library(lme4)
library(dplyr)

#-----------------------------
# 1. 先处理 analysis
#-----------------------------
analysis$city <- as.factor(analysis$city)
analysis$sex  <- as.factor(analysis$sex)

airpollutants <- c(
  "pm25_1113", "pm10_1113", "pm1_1113",
  "NO2_1113", "SO2_1113", "CO_1113", "O3_1113"
)

#-----------------------------
# 2. 只用 analysis 的函数
#-----------------------------
run_glmer_iqr <- function(pollutant) {
  
  dat <- analysis %>%
    select(htn_follow, age, sex, bmi, city, all_of(pollutant)) %>%
    na.omit()
  
  iqr_value <- IQR(dat[[pollutant]], na.rm = TRUE)
  
  new_var <- paste0(pollutant, "_iqr")
  dat[[new_var]] <- dat[[pollutant]] / iqr_value
  
  fml <- as.formula(
    paste0("htn_follow ~ ", new_var, " + age + sex + bmi + (1 | city)")
  )
  
  model <- glmer(
    fml,
    data = dat,
    family = binomial(link = "logit")
  )
  
  coef_table <- summary(model)$coefficients
  
  beta <- coef_table[new_var, "Estimate"]
  se   <- coef_table[new_var, "Std. Error"]
  p    <- coef_table[new_var, "Pr(>|z|)"]
  
  OR  <- exp(beta)
  LCL <- exp(beta - 1.96 * se)
  UCL <- exp(beta + 1.96 * se)
  
  p_show <- ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  
  out <- data.frame(
    Pollutant = pollutant,
    N = nrow(dat),
    IQR = round(iqr_value, 3),
    OR = round(OR, 3),
    CI_lower = round(LCL, 3),
    CI_upper = round(UCL, 3),
    P_value = p_show,
    check.names = FALSE
  )
  
  return(out)
}

#-----------------------------
# 3. 批量运行
#-----------------------------
result_iqr <- do.call(
  rbind,
  lapply(airpollutants, run_glmer_iqr)
)

#-----------------------------
# 4. 生成表格格式
#-----------------------------
result_iqr$`OR (95% CI)` <- paste0(
  sprintf("%.3f", result_iqr$OR),
  " (",
  sprintf("%.3f", result_iqr$CI_lower),
  ", ",
  sprintf("%.3f", result_iqr$CI_upper),
  ")"
)

result_iqr$Pollutant <- c("PM2.5", "PM10", "PM1", "NO2", "SO2", "CO", "O3")

result_table <- result_iqr %>%
  select(Pollutant, N, IQR, `OR (95% CI)`, P_value)

print(result_table)

write.csv(result_table, "IQR_GLMM_results.csv", row.names = FALSE)