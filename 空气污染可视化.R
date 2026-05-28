library(haven)
library(sf)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(geosphere)
library(sf)  
library(data.table)
library(zoo)
library(splancs)
library(spdep)
library(dplyr)
library(patchwork)


county <- st_read("D:/fyp/地图/city84.shp")
city_list <- c(
  "库尔勒市", "安庆市", "安阳市", "鞍山市", "宝鸡市", "保定市", "保山市",
  "北京市", "本溪市", "滨州市", "阜阳市", "沧州市", "常德市", "巢湖市", "朝阳市", "潮州市", "成都市",
  "承德市", "赤峰市", "楚雄市", "大连市", "德州市", "定西县", "恩施市",
  "佛山市", "福州市", "阜阳市", "康定县", "赣州市", "南充市", "广州市", "桂林市",
  "哈尔滨市", "平安县", "汉中市", "杭州市", "河池市", "呼和浩特市", "海拉尔市", "湖州市",
  "淮南市", "麻城市", "鸡西市", "吉安市", "吉林市", "济南市", "佳木斯市", "嘉兴市", "江门市",
  "焦作市", "锦州市", "荆门市", "景德镇市", "九江市", "昆明市", "兰州市", "丽江市", "丽水市",
  "连云港市", "西昌市", "聊城市", "临沧", "临汾市", "临沂市", "六安市", "娄底市",
  "洛阳市", "茂名市", "乐山市", "绵阳市", "南昌市", "南充市", "南宁市", "内江市", "宁波市",
  "宁德市", "平顶山市", "平凉市", "莆田市", "濮阳市", "齐齐哈尔市", "凯里市",
  "都匀市", "青岛市", "清远市", "上海市", "上饶市", "邵阳市", "深圳市", "石家庄市",
  "四平市", "苏州市", "宿迁市", "宿州市", "台州市", "泰州市", "天津市", "威海市", "潍坊市",
  "渭南市", "锡林浩特市", "襄樊市", "忻州市", "信阳市", "乌兰浩特市", "徐州市",
  "盐城市", "扬州市", "阳泉市", "宜宾市", "宜春市", "益阳市", "榆林市", "玉林市",
  "岳阳市", "运城市", "枣庄市", "张掖市", "漳州市", "长沙市", "昭通市", "郑州市",
  "重庆市", "周口市", "资阳市"
)

library(dplyr)
library(sf)
library(ggplot2)

################pm25
library(readr)
pm25 <- read_csv("D:/fyp/city_pm25_2000_2023.csv")

pm25_filtered <- pm25 %>%
  filter(name %in% city_list) %>%                     
  rowwise() %>%                                       
  mutate(avg_pm25 = mean(c_across(Y2011:Y2020),       
                         na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_pm25)                              


county_sel <- county %>%
  filter(ct_中文名 %in% city_list)

county_points2 <- county_sel %>%
  left_join(pm25_filtered, by = c("ct_中文名" = "name")) %>%  
  st_centroid() %>%                                           
  mutate(panel = "PM[2.5]")                                   

county2 <- county %>%
  mutate(panel = "PM[2.5]")

ggplot() +
  geom_sf(data = county2,
          fill = NA,
          color = "grey40",
          size  = 0.2) +
  geom_sf(data = county_points2,
          aes(color = avg_pm25),
          size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",
                                    color = "black",
                                    linewidth = 0.8),
    strip.text       = element_text(size = 16, face = "bold"),
    panel.border     = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.spacing    = unit(0, "lines"),
    axis.title       = element_blank()
  )


################pm10
pm10 <- read_csv("D:/fyp/city_pm10_2000_2023.csv")

pm10_filtered <- pm10 %>%
  filter(name %in% city_list) %>%
  rowwise() %>%
  mutate(avg_pm10 = mean(c_across(Y2011:Y2020), na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_pm10)

county_points10 <- county %>%
  filter(ct_中文名 %in% city_list) %>%
  left_join(pm10_filtered, by = c("ct_中文名" = "name")) %>%
  st_centroid() %>%
  mutate(panel = "PM[10]")

county_base10 <- county %>%
  mutate(panel = "PM[10]")

ggplot() +
  geom_sf(data = county_base10, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = county_points10, aes(color = avg_pm10), size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0, "lines"),
    axis.title = element_blank()
  )

#######pm1
pm1 <- read_csv("D:/fyp/city_pm1_2000_2021.csv")

pm1_filtered <- pm1 %>%
  filter(name %in% city_list) %>%
  rowwise() %>%
  mutate(avg_pm1 = mean(c_across(Y2011:Y2020), na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_pm1)

county_points1 <- county %>%
  filter(ct_中文名 %in% city_list) %>%
  left_join(pm1_filtered, by = c("ct_中文名" = "name")) %>%
  st_centroid() %>%
  mutate(panel = "PM[1]")

county_base1 <- county %>%
  mutate(panel = "PM[1]")

ggplot() +
  geom_sf(data = county_base1, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = county_points1, aes(color = avg_pm1), size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0, "lines"),
    axis.title = element_blank()
  )


########o3
o3 <- read_csv("D:/fyp/city_O3_2000_2023.csv")

o3_filtered <- o3 %>%
  filter(name %in% city_list) %>%
  rowwise() %>%
  mutate(avg_o3 = mean(c_across(Y2011:Y2020), na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_o3)

county_pointso3 <- county %>%
  filter(ct_中文名 %in% city_list) %>%
  left_join(o3_filtered, by = c("ct_中文名" = "name")) %>%
  st_centroid() %>%
  mutate(panel = "O[3]")

county_baseo3 <- county %>%
  mutate(panel = "O[3]")

ggplot() +
  geom_sf(data = county_baseo3, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = county_pointso3, aes(color = avg_o3), size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8", color = "black", linewidth = 0.8),
    strip.text = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(0, "lines"),
    axis.title = element_blank()
  )


#####so2
so2 <- read_csv("D:/fyp/city_SO2_2013_2023.csv")

so2_filtered <- so2 %>%
  filter(name %in% city_list) %>%
  rowwise() %>%
  mutate(avg_so2 = mean(c_across(Y2013:Y2020), na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_so2)

county_points_so2 <- county %>%
  filter(ct_中文名 %in% city_list) %>%
  left_join(so2_filtered, by = c("ct_中文名" = "name")) %>%
  st_centroid() %>%
  mutate(panel = "SO[2]")

county_base_so2 <- county %>%
  mutate(panel = "SO[2]")

ggplot() +
  geom_sf(data = county_base_so2, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = county_points_so2, aes(color = avg_so2), size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8", color = "black", linewidth = 0.8),
    strip.text       = element_text(size = 16, face = "bold"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.spacing    = unit(0, "lines"),
    axis.title       = element_blank()
  )



######no2
no2 <- read_csv("D:/fyp/city_NO2_2013_2023.csv")

no2_filtered <- no2 %>%
  filter(name %in% city_list) %>%
  rowwise() %>%
  mutate(avg_no2 = mean(c_across(Y2013:Y2020), na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_no2)

county_points_no2 <- county %>%
  filter(ct_中文名 %in% city_list) %>%
  left_join(no2_filtered, by = c("ct_中文名" = "name")) %>%
  st_centroid() %>%
  mutate(panel = "NO[2]")

county_base_no2 <- county %>%
  mutate(panel = "NO[2]")

ggplot() +
  geom_sf(data = county_base_no2, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = county_points_no2, aes(color = avg_no2), size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8", color = "black", linewidth = 0.8),
    strip.text       = element_text(size = 16, face = "bold"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.spacing    = unit(0, "lines"),
    axis.title       = element_blank()
  )



#######co
co <- read_csv("D:/fyp/city_CO_2013_2023.csv")

co_filtered <- co %>%
  filter(name %in% city_list) %>%
  rowwise() %>%
  mutate(avg_co = mean(c_across(Y2013:Y2020), na.rm = TRUE)) %>%
  ungroup() %>%
  select(name, avg_co)

county_points_co <- county %>%
  filter(ct_中文名 %in% city_list) %>%
  left_join(co_filtered, by = c("ct_中文名" = "name")) %>%
  st_centroid() %>%
  mutate(panel = "CO")

county_base_co <- county %>%
  mutate(panel = "CO")

ggplot() +
  geom_sf(data = county_base_co, fill = NA, color = "grey40", size = 0.2) +
  geom_sf(data = county_points_co, aes(color = avg_co), size = 2) +
  facet_wrap(~panel, labeller = label_parsed) +
  labs(color = "Level") +
  scale_color_viridis_c() +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8", color = "black", linewidth = 0.8),
    strip.text       = element_text(size = 16, face = "bold"),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.spacing    = unit(0, "lines"),
    axis.title       = element_blank()
  )



######时间序列
pm25 <- read_csv("D:/fyp/city_pm25_2000_2023.csv")

ts_pm25 <- pm25 %>%
  filter(name %in% city_list) %>%
  select(name, Y2011:Y2020) %>%
  pivot_longer(cols = Y2011:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "PM[2.5]")   

ggplot(ts_pm25, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2011:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) + 
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",  
                                    color = "black",
                                    linewidth = 0.8),
    strip.text = element_text(size = 16, face = "bold"),  
    panel.border = element_rect(color = "black", fill = NA),
    axis.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

pm10 <- read_csv("C:/Users/Administrator/Desktop/2026thesis/city_pm10_2000_2023.csv")

ts_pm10 <- pm10 %>%
  filter(name %in% city_list) %>%
  select(name, Y2011:Y2020) %>%
  pivot_longer(cols = Y2011:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "PM[10]")

ggplot(ts_pm10, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2011:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",
                                    color = "black",
                                    linewidth = 0.8),
    strip.text   = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title   = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

pm1 <- read_csv("C:/Users/Administrator/Desktop/2026thesis/city_pm1_2000_2021.csv")

ts_pm1 <- pm1 %>%
  filter(name %in% city_list) %>%
  select(name, Y2011:Y2020) %>%
  pivot_longer(cols = Y2011:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "PM[1]")   # ⭐标题（带下标）

ggplot(ts_pm1, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2011:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) +   # ⭐蓝色标题框方式
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",  # ⭐浅蓝色 strip
                                    color = "black",
                                    linewidth = 0.8),
    strip.text   = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title   = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )



o3 <- read_csv("C:/Users/Administrator/Desktop/2026thesis/city_o3_2000_2023.csv")

ts_o3 <- o3 %>%
  filter(name %in% city_list) %>%
  select(name, Y2011:Y2020) %>%
  pivot_longer(cols = Y2011:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "O[3]")

ggplot(ts_o3, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2011:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",
                                    color = "black",
                                    linewidth = 0.8),
    strip.text   = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title   = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )


so2 <- read_csv("C:/Users/Administrator/Desktop/2026thesis/city_so2_2013_2023.csv")

ts_so2 <- so2 %>%
  filter(name %in% city_list) %>%
  select(name, Y2013:Y2020) %>%
  pivot_longer(cols = Y2013:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "SO[2]")

ggplot(ts_so2, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2013:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",
                                    color = "black",
                                    linewidth = 0.8),
    strip.text   = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title   = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )


no2 <- read_csv("C:/Users/Administrator/Desktop/2026thesis/city_NO2_2013_2023.csv")

ts_no2 <- no2 %>%
  filter(name %in% city_list) %>%
  select(name, Y2013:Y2020) %>%
  pivot_longer(cols = Y2013:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "NO[2]")

ggplot(ts_no2, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2013:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",
                                    color = "black",
                                    linewidth = 0.8),
    strip.text   = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title   = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )




co <- read_csv("C:/Users/Administrator/Desktop/2026thesis/city_CO_2013_2023.csv")

ts_co <- co %>%
  filter(name %in% city_list) %>%
  select(name, Y2013:Y2020) %>%
  pivot_longer(cols = Y2013:Y2020,
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(sub("Y", "", year))) %>%
  group_by(year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(panel = "CO")

ggplot(ts_co, aes(year, mean_value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2013:2020) +
  labs(x = "Year", y = "Conc.") +
  facet_wrap(~panel, labeller = label_parsed) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#7EC9E8",
                                    color = "black",
                                    linewidth = 0.8),
    strip.text   = element_text(size = 16, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.title   = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )
