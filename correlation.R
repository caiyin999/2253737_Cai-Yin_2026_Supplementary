library(dplyr)
library(tidyr)
library(ggplot2)

# 1. 选择变量
vars <- c("pm25_1113", "pm10_1113", "pm1_1113",
          "NO2_1113", "SO2_1113", "CO_1113", "O3_1113")

# 2. 图里显示的名字
var_labels <- c("PM2.5", "PM10", "PM1", "NO2", "SO2", "CO", "O3")

# 3. 提取数据并计算相关矩阵
cor_dat <- analysis[, vars]
cor_mat <- cor(cor_dat, use = "pairwise.complete.obs", method = "spearman")

# 改矩阵的行名列名，方便后面画图
colnames(cor_mat) <- var_labels
rownames(cor_mat) <- var_labels

# 4. 转成长数据
cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
names(cor_df) <- c("row_var", "col_var", "corr")

# 5. 标记上三角、下三角、对角线
cor_df$row_id <- match(cor_df$row_var, var_labels)
cor_df$col_id <- match(cor_df$col_var, var_labels)

cor_df$part <- case_when(
  cor_df$row_id < cor_df$col_id ~ "upper",
  cor_df$row_id > cor_df$col_id ~ "lower",
  TRUE ~ "diag"
)

# 6. 设置因子顺序（y轴反过来，这样左上角是第一项）
cor_df$row_var <- factor(cor_df$row_var, levels = rev(var_labels))
cor_df$col_var <- factor(cor_df$col_var, levels = var_labels)

# 7. 画图
p <- ggplot(cor_df, aes(x = col_var, y = row_var)) +
  # 底层方格
  geom_tile(fill = "white", color = "grey75", linewidth = 0.6) +
  
  # 上三角：圆圈
  geom_point(
    data = subset(cor_df, part == "upper"),
    aes(size = abs(corr), color = corr),
    shape = 16
  ) +
  
  # 下三角：数字
  geom_text(
    data = subset(cor_df, part == "lower"),
    aes(label = sprintf("%.2f", corr), color = corr),
    fontface = "bold",
    size = 6
  ) +
  
  # 对角线：变量名
  geom_text(
    data = subset(cor_df, part == "diag"),
    aes(label = col_var),
    color = "black",
    fontface = "bold",
    size = 5.5
  ) +
  
  # 颜色
  scale_color_gradientn(
    colours = c("#0047CC", "#1E90FF", "#F3E7B3", "#FF8C00", "#B30000"),
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +
  
  # 圆圈大小
  scale_size_continuous(range = c(5, 20), guide = "none") +
  
  coord_fixed() +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  guides(
    color = guide_colorbar(
      barheight = unit(4, "cm"),
      barwidth = unit(0.5, "cm")
    )
  )

p
