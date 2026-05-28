# 选择污染物列
airpollutants <- data_clean %>% select(pm25_1113, pm1_1113, pm10_1113, CO_1113, SO2_1113, NO2_1113, O3_1113)

# 计算相关系数矩阵
cor_matrix <- cor(airpollutants, use = "complete.obs", method = "pearson")
print(cor_matrix)

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
    bmi = as.numeric(bmi),
    O3_1113 = as.numeric(O3_1113),
    age = as.numeric(age),
    chronic = as.numeric(chronic),
    sleep_night = as.numeric(sleep_night),
    nation = as.factor(nation),
    education = as.factor(education),
    marry = as.factor(marry),
    hrural = as.factor(hrural))
data_clean=na.omit(data_clean)

# 定义节点
node_list <- list(
  W =c("age", "sex", "hrural", "city",
       "smokev", "smoken", "CO_1113"), # 协变量
  A = "O3_1113",         # 暴露
  Y = "htn_follow"    # 结局
)

# 为 Y（结局）定义 learners：二分类更合适
# Y 是 0/1，所以 glm 和 glmnet 都设成 binomial
lrnr_Y_mean  <- make_learner(Lrnr_mean)

lrnr_Y_glm <- make_learner(
  Lrnr_glm,
  family = binomial()
)

lrnr_Y_lasso <- make_learner(
  Lrnr_glmnet,
  family = "binomial"
)

lrnr_Y_rf <- make_learner(
  Lrnr_ranger,
  num.trees = 500
)

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

# 为 A（暴露）定义 learners：连续变量更合适
# A 是连续暴露，所以 glm 和 glmnet 都设成 gaussian
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

# 分开定义 learner_list
learner_list <- list(
  Y = sl_Y,   # 二分类 outcome 模型
  A = sl_A    # 连续 exposure 模型
)

# 定义 shift intervention
delta <- IQR(data_clean$O3_1113, na.rm = TRUE)

tmle_spec <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta
)

# 拟合 TMLE
tmle_fit <- tmle3(
  tmle_spec,
  data = data_clean,
  node_list = node_list,
  learner_list = learner_list
)

tmle_summary <- tmle_fit$summary
print(tmle_summary)

