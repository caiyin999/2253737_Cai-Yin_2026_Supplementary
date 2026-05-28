# 加载包
library(tmle3)
library(sl3)
library(data.table)
library(readxl)    
library(dplyr)     
library(broom)     
library(glmnet)
library(ranger)
library(nnls)
library(tmle3shift)

data <- read_excel("D:/fyp/analysis_final2.xlsx") %>% as.data.frame()

# 数据预处理
data_clean <- data %>%
  # 确保变量类型正确
  mutate(
    city = as.factor(city),
    sex = as.factor(sex), 
    htn_follow = as.integer(htn_follow),
    pm25_1113 = as.numeric(pm25_1113),
    age = as.numeric(age),
    hrural = as.factor(hrural))
data_clean=na.omit(data_clean)

# 定义节点
node_list <- list(
  W =c("age", "sex", "hrural", "city",
       "smokev", "smoken"), # 协变量
  A = "O3_1113",         # 暴露
  Y = "htn_follow"    # 结局
)

# ==============================
# 5. 为 Y（结局）定义 learners：二分类更合适
# ==============================
# 这里 Y 是 0/1，所以 glm 和 glmnet 都设成 binomial
lrnr_Y_mean  <- make_learner(Lrnr_mean)

lrnr_Y_glm <- make_learner(
  Lrnr_glm,
  family = binomial()
)

lrnr_Y_lasso <- make_learner(
  Lrnr_glmnet,
  family = "binomial"
)

# RF 对 0/1 outcome 也可以作为回归器使用（预测条件均值）
lrnr_Y_rf <- make_learner(
  Lrnr_ranger,
  num.trees = 500
)

# 如果你确认 Lrnr_nnet 在你环境里能跑，可以打开这一行
lrnr_Y_mlp <- make_learner(Lrnr_nnet, size = 5, maxit = 100, decay = 0.01)

sl_Y <- Lrnr_sl$new(
  learners = list(
    lrnr_Y_mean,
    lrnr_Y_glm,
    lrnr_Y_lasso,
    lrnr_Y_rf
    , lrnr_Y_mlp
  ),
  metalearner = Lrnr_nnls$new()
)

# ==============================
# 6. 为 A（暴露）定义 learners：连续变量更合适
# ==============================
# 这里 A 是连续暴露，所以 glm 和 glmnet 都设成 gaussian
lrnr_A_mean  <- make_learner(Lrnr_mean)

lrnr_A_glm <- make_learner(
  Lrnr_glm,
  family = gaussian()
)

lrnr_A_lasso <- make_learner(
  Lrnr_glmnet,
  family = "gaussian"
)

lrnr_A_rf <- make_learner(
  Lrnr_ranger,
  num.trees = 500
)

# 如果你确认 Lrnr_nnet 在你环境里能跑，可以打开这一行
lrnr_A_mlp <- make_learner(Lrnr_nnet, size = 5, maxit = 100, decay = 0.01)

sl_A <- Lrnr_sl$new(
  learners = list(
    lrnr_A_mean,
    lrnr_A_glm,
    lrnr_A_lasso,
    lrnr_A_rf
    , lrnr_A_mlp
  ),
  metalearner = Lrnr_nnls$new()
)

# ==============================
# 7. 分开定义 learner_list
# ==============================
learner_list <- list(
  Y = sl_Y,   # 二分类 outcome 模型
  A = sl_A    # 连续 exposure 模型
)

# ==============================
# 8. 定义 shift intervention
# ==============================
delta <- IQR(data_clean$O3_1113, na.rm = TRUE)

tmle_spec <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta
)

# ==============================
# 9. 拟合 TMLE
# ==============================
tmle_fit <- tmle3(
  tmle_spec,
  data = data_clean,
  node_list = node_list,
  learner_list = learner_list
)

# ==============================
# 10. 输出结果
# ==============================
tmle_summary <- tmle_fit$summary
print(tmle_summary)





# 获取ATE（平均处理效应）
ate_estimate <- tmle_summary$tmle_est
ate_se <- tmle_summary$se
ate_ci_lower <- tmle_summary$lower
ate_ci_upper <- tmle_summary$upper

cat("\n=== TMLE 分析结果 ===\n")
cat(sprintf("平均处理效应 (ATE): %.4f\n", ate_estimate))
cat(sprintf("标准误: %.4f\n", ate_se))
cat(sprintf("95%% 置信区间: [%.4f, %.4f]\n", ate_ci_lower, ate_ci_upper))

# 计算相对风险（可选）
cat("\n=== 相对风险估计 ===\n")

# 提取各组的数据
diabetes_group <- data_clean[data_clean$ever_had_diabetes == 1, ]
no_diabetes_group <- data_clean[data_clean$ever_had_diabetes == 0, ]

# 计算粗发病率
risk_diabetes <- mean(diabetes_group$ever_had_heart_problems)
risk_no_diabetes <- mean(no_diabetes_group$ever_had_heart_problems)
relative_risk <- risk_diabetes / risk_no_diabetes

cat(sprintf("糖尿病组心脑血管病发病率: %.2f%%\n", risk_diabetes * 100))
cat(sprintf("非糖尿病组心脑血管病发病率: %.2f%%\n", risk_no_diabetes * 100))
cat(sprintf("粗相对风险 (RR): %.2f\n", relative_risk))

# 解释ATE结果
if (ate_estimate > 0) {
  cat("\n解释：在调整了性别和生物加速年龄后，患有糖尿病会使心脑血管病的绝对风险增加",
      round(ate_estimate * 100, 2), "个百分点")
} else if (ate_estimate < 0) {
  cat("\n解释：在调整了性别和生物加速年龄后，患有糖尿病会使心脑血管病的绝对风险降低",
      round(abs(ate_estimate) * 100, 2), "个百分点")
} else {
  cat("\n解释：未发现糖尿病与心脑血管病之间的显著关联")
}



# 可选：绘制结果的可视化
if (require(ggplot2)) {
  results_df <- data.frame(
    Parameter = "ATE",
    Estimate = ate_estimate,
    SE = ate_se,
    CI_lower = ate_ci_lower,
    CI_upper = ate_ci_upper
  )
  
  p <- ggplot(results_df, aes(x = Parameter, y = Estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "糖尿病对心脑血管病的因果效应 (TMLE估计)",
      subtitle = "调整了性别和生物加速年龄",
      y = "平均处理效应 (ATE)",
      x = ""
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
  ggsave("tmle_ate_plot.png", plot = p, width = 8, height = 6)
}

# 敏感性分析：尝试不同的机器学习算法
cat("\n=== 敏感性分析 ===\n")
cat("尝试不同的机器学习算法组合...\n")

# 使用更简单的模型进行敏感性分析
lrnr_glm_only <- Lrnr_glm$new()
lrnr_mean_only <- Lrnr_mean$new()

learner_list_simple <- list(
  Y = lrnr_glm_only,
  A = lrnr_glm_only
)

# 使用简单模型重新运行TMLE
tmle_fit_simple <- tmle3(
  tmle_spec,
  data = data_clean,
  node_list = node_list,
  learner_list = learner_list_simple
)

cat("简单模型 (仅GLM) 结果:\n")
print(tmle_fit_simple$summary)