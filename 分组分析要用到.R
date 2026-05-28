# -----------------------------
# 1️⃣ 筛选女性数据
# -----------------------------
# 假设 sex=0 是女性，sex=1 是男性；根据你数据实际编码修改
data_female <- data_clean %>% filter(sex == 0)

# -----------------------------
# 2️⃣ 定义节点
# -----------------------------
node_list_female <- list(
  W =c("age", "hrural", "city",
       "smokev", "smoken", "drinkev", "drinkl"),
  A = "CO_1113",
  Y = "htn_follow"
)

# -----------------------------
# 3️⃣ 使用同样的 learner_list
# -----------------------------
# 这里用之前定义好的 learner_list 不需要改
# learner_list 已经包含 sl_Y 和 sl_A

# -----------------------------
# 4️⃣ 定义 shift intervention（同样用 IQR）
# -----------------------------
delta_female <- IQR(data_female$CO_1113, na.rm = TRUE)

tmle_spec_female <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_female
)

# -----------------------------
# 5️⃣ 拟合 TMLE
# -----------------------------
tmle_fit_female <- tmle3(
  tmle_spec_female,
  data = data_female,
  node_list = node_list_female,
  learner_list = learner_list
)

# -----------------------------
# 6️⃣ 输出结果
# -----------------------------
tmle_summary_female <- tmle_fit_female$summary
print(tmle_summary_female)


# -----------------------------
# 1️⃣ 筛选男性数据
# -----------------------------
data_male <- data_clean %>% filter(sex == 1)  # 根据你数据 sex=1 表示男性

# -----------------------------
# 2️⃣ 定义节点
# -----------------------------
node_list_male <- list(
  W =c("age", "hrural", "city",
       "smokev", "smoken", "drinkev", "drinkl"),  # 协变量
  A = "CO_1113",
  Y = "htn_follow"
)

# -----------------------------
# 3️⃣ shift intervention
# -----------------------------
delta_male <- IQR(data_male$CO_1113, na.rm = TRUE)

tmle_spec_male <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_male
)

# -----------------------------
# 4️⃣ 拟合 TMLE
# -----------------------------
tmle_fit_male <- tmle3(
  tmle_spec_male,
  data = data_male,
  node_list = node_list_male,
  learner_list = learner_list  # 使用之前定义好的 learners
)

# -----------------------------
# 5️⃣ 输出结果
# -----------------------------
tmle_summary_male <- tmle_fit_male$summary
print(tmle_summary_male)





# -----------------------------
# 1️⃣ 筛选 Non-smoker 和 Smoker
# -----------------------------
data_non_smoke <- data_clean %>% filter(smokev == 0)
data_smoke <- data_clean %>% filter(smokev == 1)

# -----------------------------
# 2️⃣ 定义节点（保持原来 W、A、Y）
# -----------------------------
node_list <- list(
  W =c("age", "hrural", "city",
       "sex", "smoken", "drinkev", "drinkl"),
  A = "CO_1113",
  Y = "htn_follow"
)

# -----------------------------
# 3️⃣ shift intervention
# -----------------------------
delta_non <- IQR(data_non_smoke$CO_1113, na.rm = TRUE)
delta_sm <- IQR(data_smoke$CO_1113, na.rm = TRUE)

tmle_spec_non <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_non
)
tmle_spec_sm <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_sm
)

# -----------------------------
# 4️⃣ 拟合 TMLE
# -----------------------------
tmle_fit_non <- tmle3(
  tmle_spec_non,
  data = data_non_smoke,
  node_list = node_list,
  learner_list = learner_list
)
tmle_fit_sm <- tmle3(
  tmle_spec_sm,
  data = data_smoke,
  node_list = node_list,
  learner_list = learner_list
)

# -----------------------------
# 5️⃣ 输出结果
# -----------------------------
tmle_summary_non <- tmle_fit_non$summary
tmle_summary_sm <- tmle_fit_sm$summary

print(tmle_summary_non)
print(tmle_summary_sm)


# -----------------------------
# 1️⃣ 按年龄分组
# -----------------------------
data_age_lt65 <- data_clean %>% filter(age < 65)
data_age_ge65 <- data_clean %>% filter(age >= 65)

# -----------------------------
# 2️⃣ 定义节点
# -----------------------------
node_list <- list(
  W = c("sex","hrural","city",
        "smokev","smoken","drinkev","drinkl"),
  A = "CO_1113",
  Y = "htn_follow"
)

# -----------------------------
# 3️⃣ shift intervention
# -----------------------------
delta_lt65 <- IQR(data_age_lt65$CO_1113, na.rm = TRUE)
delta_ge65 <- IQR(data_age_ge65$CO_1113, na.rm = TRUE)

tmle_spec_lt65 <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_lt65
)
tmle_spec_ge65 <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_ge65
)

# -----------------------------
# 4️⃣ 拟合 TMLE
# -----------------------------
tmle_fit_lt65 <- tmle3(
  tmle_spec_lt65,
  data = data_age_lt65,
  node_list = node_list,
  learner_list = learner_list
)
tmle_fit_ge65 <- tmle3(
  tmle_spec_ge65,
  data = data_age_ge65,
  node_list = node_list,
  learner_list = learner_list
)

# -----------------------------
# 5️⃣ 输出结果
# -----------------------------
tmle_summary_lt65 <- tmle_fit_lt65$summary
tmle_summary_ge65 <- tmle_fit_ge65$summary

print(tmle_summary_lt65)
print(tmle_summary_ge65)




# -----------------------------
# 1️⃣ 定义 BMI 分组
# -----------------------------
data_bmi_lt18 <- data_clean %>% filter(bmi < 18.5)
data_bmi_18_24 <- data_clean %>% filter(bmi >= 18.5 & bmi < 24)
data_bmi_ge24 <- data_clean %>% filter(bmi >= 24)

# -----------------------------
# 2️⃣ 定义节点
# -----------------------------
node_list <- list(
  W = c("sex","age", "hrural","city",
        "smokev","smoken","drinkev","drinkl"),
  A = "CO_1113",
  Y = "htn_follow"
)

# -----------------------------
# 3️⃣ shift intervention
# -----------------------------
delta_lt18 <- IQR(data_bmi_lt18$CO_1113, na.rm = TRUE)
delta_18_24 <- IQR(data_bmi_18_24$CO_1113, na.rm = TRUE)
delta_ge24 <- IQR(data_bmi_ge24$CO_1113, na.rm = TRUE)

tmle_spec_lt18 <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_lt18
)
tmle_spec_18_24 <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_18_24
)
tmle_spec_ge24 <- tmle3shift::tmle_shift(
  shift_fun = function(a, w) a + delta_ge24
)

# -----------------------------
# 4️⃣ 拟合 TMLE
# -----------------------------
tmle_fit_lt18 <- tmle3(
  tmle_spec_lt18,
  data = data_bmi_lt18,
  node_list = node_list,
  learner_list = learner_list
)
tmle_fit_18_24 <- tmle3(
  tmle_spec_18_24,
  data = data_bmi_18_24,
  node_list = node_list,
  learner_list = learner_list
)
tmle_fit_ge24 <- tmle3(
  tmle_spec_ge24,
  data = data_bmi_ge24,
  node_list = node_list,
  learner_list = learner_list
)

# -----------------------------
# 5️⃣ 输出结果
# -----------------------------
tmle_summary_lt18 <- tmle_fit_lt18$summary
tmle_summary_18_24 <- tmle_fit_18_24$summary
tmle_summary_ge24 <- tmle_fit_ge24$summary

print(tmle_summary_lt18)
print(tmle_summary_18_24)
print(tmle_summary_ge24)


# -----------------------------
# 1️⃣ 整理所有分组 TMLE 结果
# -----------------------------
forest_df <- bind_rows(
  data.frame(
    Group = c("Female", "Male"),
    Subgroup = "Sex",
    Estimate = c(tmle_summary_female$tmle_est, tmle_summary_male$tmle_est),
    Lower = c(tmle_summary_female$lower, tmle_summary_male$lower),
    Upper = c(tmle_summary_female$upper, tmle_summary_male$upper)
  ),
  data.frame(
    Group = c("Non-smoker", "Smoker"),
    Subgroup = "Smoke",
    Estimate = c(tmle_summary_non$tmle_est, tmle_summary_sm$tmle_est),
    Lower = c(tmle_summary_non$lower, tmle_summary_sm$lower),
    Upper = c(tmle_summary_non$upper, tmle_summary_sm$upper)
  ),
  data.frame(
    Group = c("<65", ">=65"),
    Subgroup = "Age",
    Estimate = c(tmle_summary_lt65$tmle_est, tmle_summary_ge65$tmle_est),
    Lower = c(tmle_summary_lt65$lower, tmle_summary_ge65$lower),
    Upper = c(tmle_summary_lt65$upper, tmle_summary_ge65$upper)
  ),
  data.frame(
    Group = c("<18.5", "18.5-24", ">=24"),
    Subgroup = "BMI",
    Estimate = c(tmle_summary_lt18$tmle_est, tmle_summary_18_24$tmle_est, tmle_summary_ge24$tmle_est),
    Lower = c(tmle_summary_lt18$lower, tmle_summary_18_24$lower, tmle_summary_ge24$lower),
    Upper = c(tmle_summary_lt18$upper, tmle_summary_18_24$upper, tmle_summary_ge24$upper)
  )
)

# -----------------------------
# 2️⃣ 为每个分组生成顺序（确保分组在森林图中有间距）
# -----------------------------
forest_df <- forest_df %>%
  mutate(Group_Order = paste0(Subgroup, " - ", Group))


library(ggplot2)
library(dplyr)

# 假设 forest_df 已经整理好：
forest_df <- forest_df %>%
  mutate(
    Label = paste0(round(Estimate,2), " [", round(Lower,2), ",", round(Upper,2), "]"),
    # 用于y轴排序和分组显示
    Group_Order = factor(paste0(Subgroup, " - ", Group), levels = rev(unique(paste0(Subgroup, " - ", Group))))
  )

ggplot(forest_df, aes(x = Estimate, y = Group_Order)) +
  # 绘制点和横线
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.25, color = "black") +
  # 红色虚线
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  # 左侧数字放在y轴左侧，与分组一起
  geom_text(aes(x = min(Lower) - 0.1*diff(range(Lower)), label = Label),
            hjust = 1, vjust = 0.5, size = 3.5) +
  # 分组显示
  facet_grid(Subgroup ~ ., scales = "free_y", space = "free_y") +
  xlab("Shift Effect of CO on Hypertension (ATE)") +
  ylab("") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold")
  ) +
  # 扩展横轴，给数字留出空间
  scale_x_continuous(expand = expansion(mult = c(0.25, 0.05)))

