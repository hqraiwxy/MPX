require(ggpubr)
require(dplyr)
require(tidyr)
require(stringr)
require(readr)
require(ggplot2)
require(lemon)
require(zoo)
require(sqldf)
require(networkD3)
require(d3Network)
require(ggrepel)
require(maps)
require(RColorBrewer)
require(tidyverse)
require(reshape2)
require(runner)
require(lubridate)


setwd("C:/Users/wxyrc/Desktop/MPX")
time_to_num <- read.csv("time to num.csv")
time_to_num$date = as.Date(time_to_num$date) #调整日期格式
colnames(time_to_num) <- c("time", "date")

# STEP 0  汇总处理最终Risk数据
MPX_result <- read.csv("MPX_result.csv")
MPX_result %>%
  group_by(country, iso3, time) %>%
  summarise(Risk_p_mean = mean(Risk_posterior),
            Risk_p_sd = sd(Risk_posterior)) -> MPX_result_m_sd

stringency <- read.csv("stringency_adjusted.csv")
stringency %>%
  dplyr::select(iso_code, location, date, stringency_index) -> stringency
stringency$date = as.Date(stringency$date) #调整日期格式
stringency %>%
  mutate(date_num = unclass(as.Date(date))) %>%
  select(location,
         iso_code,
         date,
         date_num,
         stringency_index) -> stringency
colnames(stringency) <-
  c("location", "iso3", "date", "date_num", "stringency_index")

risk_post_pior = left_join(MPX_result_m_sd, stringency, by = "iso3")
risk_post_pior %>%
  subset(time == date_num) -> risk_post_pior
risk_post_pior %>%
  mutate(
    risk_mean = Risk_p_mean * (100 - stringency_index) / 100,
    risk_sd = Risk_p_sd * (100 - stringency_index) / 100
  ) %>%
  select(country,
         iso3,
         time,
         risk_mean,
         risk_sd,
         Risk_p_mean,
         Risk_p_sd,
         stringency_index) -> risk_mean_sd
# 带上时间
risk_mean_sd = left_join(time_to_num, risk_mean_sd, by = "time")

write.csv(risk_mean_sd, "risk_mean_sd.csv")

# STEP 1  处理每个国家的风险数据

risk_mean_sd %>%
  dplyr::select(country, iso3, time, risk_mean, risk_sd) -> risk_in_each_country
colnames(risk_in_each_country) <-
  c("departure_country", "iso3", "time", "risk_mean", "risk_sd")


# 需要将国家名调整一致
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Bolivia (Plurinational State of)"] <- "Bolivia"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Cura鑾絘o"] <- "Curacao"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Democratic Republic of the Congo"] <-
  "Democratic Republic of Congo"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Iran (Islamic Republic of)"] <- "Iran"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Czechia"] <- "Czech"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "New Caledonia"] <- "new Caledonia"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "New Zealand"] <- "new Zealand"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Philippines"] <- "the Philippines"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Republic of Korea"] <- "South Korea"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Russian Federation"] <- "Russia"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Saint Martin"] <- "st martin"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "The United Kingdom"] <- "U.K."
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "United States of America"] <- "U.S."
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "T鐪塺kiye"] <- "Turkey"
risk_in_each_country$departure_country[risk_in_each_country$departure_country ==
                                         "El Salvador"] <- "salvador"


# population_GHS_risk <-
#   read.csv("population_GHS_risk.csv")
#
# aa = merge(
#   risk_in_each_country,
#   population_GHS_risk ,
#   by = "departure_country",
#   all.x = TRUE
# )
# aa  -> population_GHS_risk

# STEP 2 处理每个国家的航班数据
flight_actual_3month <-
  read.csv("flight_actual_3month.csv") #载入3个月的航班数据

flight_actual_3month %>%
  group_by(departure_country, land_city) %>%
  mutate(avg_flight = mean(flights_in_month) / 30) %>%  #每日平均航班数
  select(departure_country, land_city, avg_flight) -> flight_actual_3month
flight_actual_3month = unique(flight_actual_3month)

bb = merge(risk_in_each_country,
           flight_actual_3month,
           by = "departure_country",
           all.x = TRUE)
risk_flight_result = na.omit(bb)


# STEP 3 航班相关国内结果汇总：包括各个城市和全国
risk_flight_result %>%
  group_by(departure_country, time, risk_mean, risk_sd) %>%
  mutate(flight_to_China = sum(avg_flight)) %>%
  mutate(
    risk_importation_mean =   flight_to_China * risk_mean,
    risk_importation_sd =   flight_to_China * risk_sd,
    risk_city_mean = avg_flight * risk_mean,
    risk_city_sd = avg_flight * risk_sd
  ) -> Importation_Risk_Result
Importation_Risk_Result = left_join(time_to_num, Importation_Risk_Result, by = "time")
write.csv(Importation_Risk_Result, "第二部分国内结果.csv")


##世界各国输入我国的总体风险
Importation_Risk_Result %>%
  dplyr::select(
    departure_country,
    time,
    risk_mean,
    risk_sd,
    iso3,
    flight_to_China,
    risk_importation_mean,
    risk_importation_sd
  ) -> Importation_Risk_country2China
Importation_Risk_country2China <-
  unique(Importation_Risk_country2China)
Importation_Risk_country2China = left_join(time_to_num, Importation_Risk_country2China, by = "time")
write.csv(Importation_Risk_country2China, "每个日期世界各国输入我国的风险.csv")
# Importation_Risk_Result %>%
#   mutate(population_scale = case_when(
#     population <= 10000000 ~ 1,
#     (population > 10000000 & population <= 100000000) ~ 2,
#     population > 100000000 ~ 3
#   )) -> Importation_Risk_Result
##################Fig 1 绘制全球风险图###########
#地图绘制
population_GHS_risk %>%
  mutate(post.risk = risk) -> population_GHS_risk
population_GHS_risk %>%
  mutate(risk = (1 - GHS / 100) * post.risk) -> population_GHS_risk
population_GHS_risk[, c(1, 2)]  -> country_risk

# 需要将国家名调整一致
#salvador    El Salvador
#the Philippines  Philippines
# U.K. UK
# U.S. USA
# st martin     Saint Martin
# new caledonia   New Caledonia
# new Zealand            New Zealand
country_risk$departure_country[country_risk$departure_country == "salvador"] <-
  "El Salvador"
country_risk$departure_country[country_risk$departure_country == "the Philippines"] <-
  "Philippines"
country_risk$departure_country[country_risk$departure_country == "U.K."] <-
  "UK"
country_risk$departure_country[country_risk$departure_country == "U.S."] <-
  "USA"
country_risk$departure_country[country_risk$departure_country == "st martin"] <-
  "Saint Martin"
country_risk$departure_country[country_risk$departure_country == "new caledonia"] <-
  "New Caledonia"
#  载入世界地图
WorldData <-
  map_data('world') %>% filter(region != "Antarctica") %>% fortify
#  将国家risk数据并入worldmap
WorldData %>%
  left_join(country_risk, by = c("region" = "departure_country")) -> aiv_world_map
#绘制世界部分


#绘制欧洲部分 coord_cartesian(xlim=c(-25,45),ylim=c(36,72))+ #欧洲区域
ep <-
  ggplot(aiv_world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = risk), colour = "white") +
  scale_x_continuous(
    breaks = seq(-180, 210, 45),
    labels = function(x) {
      paste0(x, "°")
    }
  ) +
  scale_y_continuous(
    breaks = seq(-60, 100, 30),
    labels = function(x) {
      paste0(x, "°")
    }
  ) +
  scale_fill_gradient(low = "steel blue",
                      high = "dark red",
                      na.value = "#e5ecc9") +
  # scale_fill_gradient(low = "#33ace1", high="#1e7090",na.value = "#e5ecc9") +
  coord_cartesian(xlim = c(-25, 45), ylim = c(36, 72)) + #欧洲区域
  labs(#title="Importation Risk of Monkeypox to China Around the World on September 15",
    y = " ", x = " ") +
  theme_light()
ep








###############Fig.3 各国输入我国的风险############################
#去除城市相关列
Importation_Risk_country2China %>%
  subset(time == 19258) -> Fig3_data #2022.09.23

stringency %>%
  subset(date_num == 19258) -> stringency_fig3 #2022.09.23

stringency_population = left_join(population, stringency_fig3, by = "iso3")

Fig3_data = left_join(Fig3_data, stringency_population, by = "iso3")

Fig3_data = Fig3_data[, c(
  "departure_country.x",
  "risk_importation_mean",
  "stringency_index",
  "population",
  "iso3",
  "who_region"
)]
colnames(Fig3_data) = c("Country",
                        "Importation Risk",
                        "Stringency Index",
                        "Population",
                        "iso3",
                        "WHO_region")


# Fig3_data %>%
#   mutate(population_scale = case_when(
#                                       population_scale==10 ~ 1,
#                                       population_scale==50 ~ 2,
#                                       population_scale==100 ~ 3))  -> Fig3_data
write.csv(Fig3_data, "Fig3_data.csv")
p = ggscatter(
  Fig3_data,
  x = "Stringency Index",
  y = "Importation Risk",
  color = "WHO_region",
  palette = "jama",
  label = "iso3",
  # 添加点的标签
  size = "Population",
  alpha = 0.5,
  #透明度
  repel = T,
  #  ggtheme = theme_bw()
)   + scale_size(range = c(5, 30))
p
ggpar(
  p,
  ylim = c(0.0001, 40),
  xlim = c(0, 60),
  yscale = "log10",
  format.scale = TRUE,
  legend = 'right'
)

# ggsave(fig3.pdf, plot = last_plot(), device = NULL,
#         scale = 1,
#        width = 16, height = 9, units = c("in", "cm", "mm"),
#        dpi = 300)
# # 部分参数解释
# filename # 设置保存图片的文件名及格式，可选格式有ps、tex、jpeg、pdf、tiff、png、
# # bmp、svg或wmf等。wmf文件仅限在装有Windows系统的计算机中保存。
# plot # 默认保存最后创建的图片
# path # 图片保存的位置，结合文件名来保存，默认保存在工作目录
# width, height, units # 保存图片的宽度、高度和单位，默认单位为英寸。
# dpi # 设置图片分辨率，接受字符串设置："retina"(320), "print"(300)或"screen"(72)
# limitsize # 逻辑词，默认TRUE，表示ggsave不会保存大于50*50英寸的图片
# compression = "lzw" # tiff格式压缩类型

###############Fig 4 绘制城市桑吉图##################################
# city=Importation_Risk_Result$land_city
#
# city=unique(city)
#
# city=as.data.frame(city)

city_GDP = read.csv("city_GDP.csv")
abc <- Importation_Risk_Result
Fig4_data = merge(abc,
                  city_GDP ,
                  by = "land_city",
                  all.x = TRUE)

Fig4_data %>%
  mutate(importation_risk_city = (1 - GHS / 100) * avg_flight * risk) -> Fig4_data

Fig4_data %>%
  subset(importation_risk_city != 0) -> Fig4_data

################## sankey diagram

Fig4_data %>%
  select(departure_country, land_city, importation_risk_city) -> Sankeylinks

Sankeynodes <-
  data.frame(name = unique(c(
    Sankeylinks$departure_country, Sankeylinks$land_city
  )), stringsAsFactors = FALSE)

Sankeynodes$index <- 0:(nrow(Sankeynodes) - 1)

Sankeylinks <-
  merge(Sankeylinks, Sankeynodes, by.x = "departure_country", by.y = "name") #将第一列的index并入
Sankeylinks <-
  merge(Sankeylinks, Sankeynodes, by.x = "land_city", by.y = "name")

Sankeydata <-
  Sankeylinks[, c(4, 5, 3)] # 重新用index代替name
names(Sankeydata) <-
  c("departure_country", "land_city", "importation_risk_city") # 重命名

Sankeyname <- Sankeynodes[, 1, drop = FALSE]

network <- sankeyNetwork(
  Links = Sankeydata,
  Nodes = Sankeyname,
  Source = "departure_country",
  Target = "land_city",
  Value = "importation_risk_city",
  NodeID = "name",
  #units = "TWh",
  fontSize = 12,
  nodeWidth = 30
)
network
#NetworkD3包其实提供了一个专门的输出函数saveNetwork
saveNetwork(network, "network.html", selfcontained = TRUE)
saveNetwork(network, "network.html", selfcontained = TRUE)




##################Fig 2 绘制前10主要国家的风险变化################
write.csv(risk_result, "risk_result1004.csv")

risk_in_11_country <-
  read.csv("10_country_risk_1004.csv", fileEncoding = 'GBK')
risk_in_11_country$date = as.Date(risk_in_11_country$date) #调整日期格式

risk_in_11_country %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%y")) -> risk_in_11_country



risk_in_11_country %>%
  mutate(
    USA_risk = (1 - 75.9 / 100) * USA,
    Spain_risk = (1 - 60.9 / 100) * Spain,
    Brazil_risk = (1 - 51.2 / 100) * Brazil,
    France_risk = (1 - 61.9 / 100) * France,
    Germany_risk = (1 - 65.5 / 100) * Germany,
    UK_risk = (1 - 67.2 / 100) * UK,
    Canada_risk = (1 - 69.8 / 100) * Canada,
    Netherlands_risk = (1 - 64.7 / 100) * Netherlands,
    Peru_risk = (1 - 54.9 / 100) * Peru,
    Portugal_risk = (1 - 54.7 / 100) * Portugal,
    Italy_risk = (1 - 51.9 / 100) * Italy
  ) -> risk_in_11_country

risk_in_11_country[, c(1, 13, 14, 15, 16, 17, 18, 21, 19, 20)]  -> risk_in_9_country
# risk_in_9_country %>%
#   mutate(USA_risk7=sum_run(USA_risk,7)) -> risk_in_9_country1

write.csv(risk_in_11_country, "risk_in_9_country.csv")
colnames(risk_in_9_country) <-
  c(
    "Date",
    "USA",
    "Spain",
    "Brazil",
    "France",
    "Germany",
    "UK",
    "Peru",
    "Canada",
    "Netherlands"
  )
risk_in_9_country[, c(1, 2, 4, 8, 9)]  -> Region_Americas
risk_in_9_country[, c(1, 3, 5, 6, 7, 10)]  -> Region_European
Region_Americas = melt(Region_Americas, id = "Date")
Region_European = melt(Region_European, id = "Date")
colnames(Region_Americas) <- c("Date", "Country", "Risk")
colnames(Region_European) <- c("Date", "Country", "Risk")

Region_Americas %>%
  subset(Risk != 0) -> Region_Americas

fig2.A <-
  ggplot(Region_Americas ,
         aes(
           x = Date,
           y = Risk,
           group = Country,
           color = Country
         )) +
  #geom_point()+
  geom_line(size = 1) +
  xlab("Date") +
  ylab("Risk index") +
  # geom_smooth(method ="loess", span = 0.3, se = FALSE)+
  scale_x_datetime(
    date_breaks = "1 week",
    minor_breaks = as.POSIXct("06/15", format = "%m/%d") ,
    date_labels = "%m/%d"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor =  element_blank(),
    #以上代码用于去除网格线且保留坐标轴边框
    #  legend.position = c(.060, .850),    #更改图例位置，放至图内部的左上角
    legend.box.background = element_rect(color = NA) #为图例加边框线
  )
fig2.A
#   ggpar(fig2.A,  ylim = c(0, 0.4)  )


# qplot(Region_Americas$Date,  Region_Americas$Risk, aes(group = Region_Americas$Country, color = Region_Americas$Country),geom='smooth', span =0.5)+
#   theme_classic()+
#   theme(panel.grid.major = element_line(colour = NA),
#         panel.background = element_rect(fill = "transparent", colour = NA),
#         plot.background = element_rect(fill = "transparent", colour = NA),
#         panel.grid.minor =  element_blank(),    #以上代码用于去除网格线且保留坐标轴边框
#         #  legend.position = c(.060, .850),    #更改图例位置，放至图内部的左上角
#         legend.box.background = element_rect(color = NA) #为图例加边框线
#   )


Region_European %>%
  subset(Risk != 0) -> Region_European

fig2.B = ggplot(Region_European ,
                mapping = aes(
                  x = Date,
                  y = Risk,
                  group = Country,
                  color = Country
                )) +
  geom_point() +
  geom_line(size = 1) +
  #geom_smooth(method ="loess", span = 0.3, se = FALSE)+
  xlab("Date") +
  ylab("Risk index") +
  scale_x_datetime(
    date_breaks = "1 week",
    minor_breaks = as.POSIXct("06/15", format = "%m/%d") ,
    date_labels = "%m/%d"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor =  element_blank(),
    #以上代码用于去除网格线且保留坐标轴边框
    legend.position = c(.060, .850),
    #更改图例位置，放至图内部的左上角
    legend.box.background = element_rect(color = NA)
  )   #为图例加边框线
# fig2.B
ggpar(fig2.B,  ylim = c(0, 0.4))


#ggsave(fig2, filename = "fig2.pdf")
################Fig 2 in another way########
risk_result %>%
  select(country, time, Risk_posterior)   %>%
  mutate(Date = as.Date(time + 44560, origin = '1900-1-1')) %>%
  subset(
    country == "United States of America" |
      country == "Spain" |
      country == "Brazil" |
      country == "France" |
      country == "Germany" |
      country == "The United Kingdom" |
      country == "Peru" |
      country == "Canada" |
      country == "Netherlands"
  ) -> risk_result_fig2
risk_result_fig2 %>%
  subset(Risk_posterior > 0) -> risk_result_fig2
risk_result_fig2 %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%y")) -> risk_result_fig2

##美洲国家
risk_result_fig2 %>%
  subset(country == "United States of America") %>%
  mutate(Risk_posterior = (1 - 75.9 / 100) * Risk_posterior) -> risk_result_USA
risk_result_fig2 %>%
  subset(country == "Canada") %>%
  mutate(Risk_posterior = (1 - 69.8 / 100) * Risk_posterior) -> risk_result_Canada
risk_result_fig2 %>%
  subset(country == "Brazil") %>%
  mutate(Risk_posterior = (1 - 51.2 / 100) * Risk_posterior) -> risk_result_Brazil
risk_result_fig2 %>%
  subset(country == "Peru") %>%
  mutate(Risk_posterior = (1 - 54.9 / 100) * Risk_posterior) -> risk_result_Peru

bind_rows(risk_result_USA,
          risk_result_Canada,
          risk_result_Brazil,
          risk_result_Peru) -> risk_result_America

##欧洲国家
risk_result_fig2 %>%
  subset(country == "The United Kingdom") %>%
  mutate(Risk_posterior = (1 - 67.2 / 100) * Risk_posterior) -> risk_result_UK
risk_result_fig2 %>%
  subset(country == "Spain") %>%
  mutate(Risk_posterior = (1 - 60.9 / 100) * Risk_posterior) -> risk_result_Spain
risk_result_fig2 %>%
  subset(country == "France") %>%
  mutate(Risk_posterior = (1 - 61.9 / 100) * Risk_posterior) -> risk_result_France
risk_result_fig2 %>%
  subset(country == "Germany") %>%
  mutate(Risk_posterior = (1 - 65.5 / 100) * Risk_posterior) -> risk_result_Germany
risk_result_fig2 %>%
  subset(country == "Netherlands") %>%
  mutate(Risk_posterior = (1 - 64.7 / 100) * Risk_posterior) -> risk_result_Netherlands


bind_rows(
  risk_result_UK,
  risk_result_Spain,
  risk_result_France,
  risk_result_Germany,
  risk_result_Netherlands
) -> risk_result_Europe


fig2.America = ggplot(
  risk_result_America ,
  mapping = aes(
    x = Date,
    y = Risk_posterior,
    group = country,
    color = country
  )
) +
  geom_point() +
  geom_line(size = 1) +
  #geom_smooth(method ="loess", span = 0.3, se = FALSE)+
  xlab("Date") +
  ylab("Risk index") +
  scale_x_datetime(
    date_breaks = "1 week",
    minor_breaks = as.POSIXct("06/15", format = "%m/%d") ,
    date_labels = "%m/%d"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor =  element_blank(),
    #以上代码用于去除网格线且保留坐标轴边框
    legend.position = c(.060, .850),
    #更改图例位置，放至图内部的左上角
    legend.box.background = element_rect(color = NA)
  )   #为图例加边框线
fig2.America
ggpar(fig2.America,  ylim = c(0, 0.4))

fig2.Europe = ggplot(
  risk_result_Europe ,
  mapping = aes(
    x = Date,
    y = Risk_posterior,
    group = country,
    color = country
  )
) +
  geom_point() +
  geom_line() +
  #geom_smooth(method ="loess", span = 0.3, se = FALSE)+
  xlab("Date") +
  ylab("Risk index") +
  scale_x_datetime(
    date_breaks = "1 week",
    minor_breaks = as.POSIXct("06/15", format = "%m/%d") ,
    date_labels = "%m/%d"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.minor =  element_blank(),
    #以上代码用于去除网格线且保留坐标轴边框
    legend.position = c(.060, .850),
    #更改图例位置，放至图内部的左上角
    legend.box.background = element_rect(color = NA)
  )   #为图例加边框线
# fig2.Europe
ggpar(fig2.Europe,  ylim = c(0, 0.4))
