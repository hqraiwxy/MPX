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
############自定义函数###################################################
myfill <- function(df) {
  df2 <- df %>%
    mutate(across(where(is.character), function(x) {
      replace(x, is.na(x), "0")
    })) %>%
    mutate(across(where(is.numeric), function(x) {
      replace(x, is.na(x), 0)
    }))
  return(df2)
}

myfill_1 <- function(df) {
  df2 <- df %>%
    mutate(across(where(is.character), function(x) {
      replace(x, is.na(x), "1")
    })) %>%
    mutate(across(where(is.numeric), function(x) {
      replace(x, is.na(x), 1)
    }))
  return(df2)
}

standarize <- function(x) {
  #最大最小标准化函数,按列进行标准化
  rowmax <- apply(x, 2, max)
  rowmin <- apply(x, 2, min)
  rv <- sweep(x, 2, rowmin, "-")  #表达量-min
  rv <- sweep(rv, 2, (rowmax - rowmin), "/")  #再除以max-min
  return(rv)
}
##########初始化参数###################################################
##载入数据
# 人口数据1
# df_data <- read.csv("owid-covid-data.csv")
# df_data %>%
#   dplyr::select(iso_code, continent, location, population) -> df_data
# population_data <- unique(df_data)
# 人口数据2
population <- read.csv("population_GHS_risk.csv") #载入各个国家人口数据
population <- population[, c(1, 4, 5, 6)]
# MSM数据
MSM <- read.csv("MSM Proportions by week of report.csv")
MSM$week_start = as.Date(MSM$week_start) #调整日期格式
MSM %>%
  mutate(week_start_num = unclass(as.Date(week_start))) -> MSM
MSM <- read.csv("MSM Proportions by week of report (10.17).csv")
MSM$week_start = as.Date(MSM$week_start) #调整日期格式
MSM %>%
  mutate(
    week_start_num = unclass(as.Date(week_start)),
    MSM_prop = runif(nrow(MSM), ci_lower_msm, ci_upper_msm)
  ) -> MSM
## 将所有国家按报道病例天数划分为两类
New_cases <-
  read.csv("new_cases.csv")
New_cases %>%
  subset(new_cases != 0) -> New_cases
cnt = table(New_cases$iso3)  # 分组计数
cnt <- as.data.frame(cnt)
colnames(cnt) <- c("iso3", "Freq")
cnt$iso3 <- as.character(cnt$iso3)
cnt %>%
  subset(Freq > 20) -> country_above_20
cnt %>%
  subset(Freq <= 20) -> country_below_20

## MPX数据
MPX_data <-
  read.csv("dataframe.csv")
#read.csv("Monkeypox cases by country as of 27 October 2022.csv") #载入结果文件
MPX_data <- unique(MPX_data)
MPX_data$date = as.Date(MPX_data$date) #调整日期格式
MPX_data %>%
  mutate(date_num = unclass(as.Date(date))) %>%
  select(country,
         iso3,
         date,
         date_num,
         new_cases) -> MPX_data_daily
MPX_each_country_final = split(MPX_data_daily, MPX_data_daily$iso3) #所有的结果以列表的形式存储

##起止时间  t = ymd(t)
t_start <- as.Date("2022-05-07")
t_start = unclass(as.Date(t_start))
t_end <- as.Date("2022-10-26")
t_end = unclass(as.Date(t_end))

##以均数21，标准差7 用以随机的period数列
set.seed(1)
ts_MPX_period = round(rnorm(100, mean = 21, sd = 7))
ts_MPX_period = abs(ts_MPX_period)
###########开始100次sim######################
sim = 1
MPX_result <- NULL
for (MPX_period in ts_MPX_period) {
  MPX_each_country <-  MPX_each_country_final#每次计算重新载入，避免在其上累积
  
  risk_result <- NULL # 存储每日的结果数据
  ####计算每个国家的MPX_period累积病例数################################
  res <-
    NULL #res为数据总集: country iso3 date  date_num  new_cases daily_case_accumulated
  for (i in 1:length(MPX_each_country)) {
    MPX_daily_accumulated <- NULL # 用来记录计算出的每日累积数据
    MPX_in_country = MPX_each_country[[i]]
    MPX_daily_accumulated <- MPX_in_country
    
    for (j in 1:nrow(MPX_in_country)) {
      MPX_in_country %>%
        subset(date_num <= MPX_in_country$date_num[j]) %>%
        subset(date_num > (MPX_in_country$date_num[j] - MPX_period)) -> MPX_accumulated
      MPX_daily_accumulated$daily_cases_accumulated[j] = sum(MPX_accumulated$new_cases)
    }
    
    MPX_each_country[[i]] = MPX_daily_accumulated # 添加MPX_period天累积病例的最终数据
    
    res %>%
      bind_rows(MPX_daily_accumulated) -> res
  }
  
  #########################时间循环开始#########################
  for (t in t_start:t_end) {
    MPX_each_country = split(res, res$iso3) #list存储各个国家的数据
    vt <- NULL   #存储评估向量，每次循环前清零
    
    ######国家循环开始######
    for (i in 1:length(MPX_each_country)) {
      MPX_daily_accumulated = MPX_each_country[[i]]
      
      if (MPX_daily_accumulated$iso3[1] %in% country_above_20$iso3) {
        #####################计算每一天的增长率############################
        MPX_daily_grow <-  #复制一个MPX_daily_grow来计算28天增长率
          MPX_daily_accumulated
        MPX_daily_grow$daily_cases_accumulated[1] = 0 #将第一天的增长率设为0
        
        for (k in 2:nrow(MPX_daily_grow)) {
          if (MPX_daily_accumulated$daily_cases_accumulated[k]  != 0) {
            MPX_daily_grow$daily_cases_accumulated[k] = (
              MPX_daily_accumulated$daily_cases_accumulated[k] - MPX_daily_accumulated$daily_cases_accumulated[k - 1]
            ) / #delt(t)=1
              (MPX_daily_accumulated$daily_cases_accumulated[k])
          }
        }
        
        MPX_daily_accumulated %>%
          cbind("daily_grow_rate" = MPX_daily_grow$daily_cases_accumulated)  -> MPX_daily_accumulated #存储每日增长率
        #plot(MPX_daily_accumulated$date_num, MPX_daily_accumulated$daily_grow_rate)
        MPX_each_country[[i]] = MPX_daily_accumulated # 时间和每日增长率
        
        #拟合增长率的直线 γ(t)=at+b
        fit = lm(formula = daily_grow_rate ~ date_num, data = MPX_daily_accumulated) #利用每日数据拟合一阶近似γ(t)=at+b
        a = fit[[1]][[2]]
        b = fit[[1]][[1]]
        
        
        t_peak = -b / a     # 峰值
        
        day_to_inflection = -abs(t_peak - t) # 参数1 距离峰值越远，风险越小
        
        # day_to_inflection = max (-200, day_to_inflection)
        
        
        
        up_rate = a * t + b  #参数2 当天增长率
        #up_rate = max (0,up_rate)
        # up_rate = a2 * t + b2  #参数2 当天增长率
      }
      
      else {
        day_to_inflection = 100000
        up_rate = 100000
      }
      ###################################################################
      # # 获取N(0)
      # MPX_daily_accumulated %>%
      #   subset(date_num == t_start) -> N_0
      # N0 = as.numeric(N_0$new_cases)
      
      # 计算Nt
      #if (t %in% MPX_daily_accumulated$date_num) {
      
      # }
      # else{
      #   #Nt = N0 * exp(a/2 * t^2 + b*t)
      #   Nt_lasttime = max(MPX_daily_accumulated$date_num)
      #   Nt = 0
      #   if (t_to_t0 >= 0 & t_to_t0 <= Nt_lasttime) {
      #     MPX_daily_accumulated %>%
      #       subset(date_num < t_to_t0) -> Nt_last
      #     Nt = Nt_last$daily_cases_accumulated[length(Nt_last$daily_cases_accumulated)] #取new_cases最后一个元素
      #   }
      #   if (t_to_t0 > Nt_lasttime) {
      #     if (t_to_t0 - 28 <=  Nt_lasttime & t_to_t0 - 28 > 0) {
      #       MPX_daily_accumulated %>%
      #         subset(date_num < t_to_t0 - 28) -> Nt_last
      #       Nt = Nt_last$daily_cases_accumulated[length(Nt_last$daily_cases_accumulated)] #取new_cases最后一个元素
      #     }
      #     Nt = as.numeric(Nt)
      #   }
      # }
      ########################################################################
      
      MPX_daily_accumulated %>%
        subset(date_num == t) -> N_t
      Nt = as.numeric(N_t$daily_cases_accumulated) #参数3 当天累积period天病例数
      # 获取MSM占比
      MSM %>%
        subset(week_start_num <= t) -> MSM_week_prop
      if (nrow(MSM_week_prop) == 0) {
        MSM_prop = 1
      }
      else{
        MSM_prop = MSM_week_prop$MSM_prop[length(MSM_week_prop$MSM_prop)] #取最后一个元素
        MSM_prop = as.numeric(MSM_prop)
      }
      
      
      vt %>%
        rbind(
          c(
            MPX_daily_accumulated$country[1],
            MPX_daily_accumulated$iso3[1],
            day_to_inflection,
            up_rate,
            Nt,
            MSM_prop
          )
        ) -> vt
    }
    #######国家循环结束############
    
    colnames(vt) <-
      c(
        "country",
        "iso3",
        "day_to_inflection",
        "time_dependent_coefficient",
        "Nt",
        "MSM_prop"
      )
    # 将vt转换为数值
    vt %>%
      transform (
        day_to_inflection = as.numeric(day_to_inflection),
        time_dependent_coefficient = as.numeric(time_dependent_coefficient),
        Nt = as.numeric(Nt),
        MSM_prop = as.numeric(MSM_prop)
      ) -> vt
    # vt = myfill(vt)# 将vt中的NA替换为0
    vt$day_to_inflection[vt$day_to_inflection == 100000] <-
      min(vt$day_to_inflection)
    vt$time_dependent_coefficient[vt$time_dependent_coefficient == 100000] <-
      min(vt$time_dependent_coefficient)
    
    Nt_population = merge(population, vt)
    Nt_population %>%
      mutate(
        Nt_p = as.numeric(Nt) / as.numeric(population),
        NonMSM = as.numeric(Nt) * (1 - as.numeric(MSM_prop)),
        MSM = as.numeric(Nt) * as.numeric(MSM_prop)
      ) -> Nt_population
    Nt_population %>%
      dplyr::select(day_to_inflection,
                    time_dependent_coefficient,
                    NonMSM,
                    MSM,
                    Nt_p) %>%
      transform(
        day_to_inflection = as.numeric(day_to_inflection),
        time_dependent_coefficient = as.numeric(time_dependent_coefficient),
        NonMSM = as.numeric(NonMSM),
        MSM = as.numeric(MSM),
        Nt_p = as.numeric(Nt_p)
      )  -> Nt_standarize1
    #Nt_standarized = scale(Nt_standarize1)
    #Nt_standarized <- as.data.frame(Nt_standarized)
    
    
    Nt_standarized = standarize(Nt_standarize1)
    
    Nt_standarized[is.na(Nt_standarized)] = 0 #将df中的NAN替换为0
    
    Nt_standarized %>%
      mutate(
        Risk_posterior = 0.15 * day_to_inflection + 0.15 * time_dependent_coefficient
        + 0.3 * Nt_p
        + 0.1 * NonMSM  + 0.3 * MSM
      ) %>%
      mutate(country = Nt_population$country,
             iso3 = Nt_population$iso3,
             time = t) -> Nt_population_scale1
    
    risk_result %>%
      rbind(Nt_population_scale1) -> risk_result #存储每一天的结果
  } #时间循环结束
  
  risk_result %>%
    cbind(MPX_preiod = MPX_period,
          sim = sim) -> risk_result
  sim = sim + 1
  MPX_result %>%
    rbind(risk_result) -> MPX_result #存储每一天的结果
}
#############处理数据
write.csv(MPX_result, "MPX_result.csv")

time_to_num <- read.csv("time to num.csv")
time_to_num$date = as.Date(time_to_num$date) #调整日期格式
colnames(time_to_num) <- c("time","date")

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
    risk_mean = Risk_p_mean * (100 - stringency_index)/100,
    risk_sd = Risk_p_sd * (100 - stringency_index)/100
  ) %>%
  select(country,
         iso3,
         time,
         risk_mean,
         risk_sd,
         Risk_p_mean,
         Risk_p_sd,
         stringency_index) -> risk_mean_sd
write.csv(risk_mean_sd, "risk_mean_sd.csv")

# STEP 1  处理每个国家的风险数据

risk_mean_sd %>%
  dplyr::select(country, iso3, time, risk_mean, risk_sd) -> risk_in_each_country
colnames(risk_in_each_country) <- c("departure_country","iso3", "time", "risk_mean", "risk_sd")


# 需要将国家名调整一致
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Bolivia (Plurinational State of)"] <- "Bolivia"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Cura鑾絘o"] <- "Curacao"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Democratic Republic of the Congo"] <- "Democratic Republic of Congo"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Iran (Islamic Republic of)"] <- "Iran"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Czechia"] <- "Czech"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="New Caledonia"] <- "new Caledonia"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="New Zealand"] <- "new Zealand"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Philippines"] <- "the Philippines"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Republic of Korea"] <- "South Korea"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Russian Federation"] <- "Russia"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Saint Martin"] <- "st martin"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="The United Kingdom"] <- "U.K."
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="United States of America"] <- "U.S."
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="Venezuela (Bolivarian Republic of)"] <- "Venezuela"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="T鐪塺kiye"] <- "Turkey"
risk_in_each_country$departure_country[risk_in_each_country$departure_country=="El Salvador"] <- "salvador"


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
  mutate(avg_flight = mean(flights_in_month)) %>%
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
  mutate(risk_importation_mean =   flight_to_China * risk_mean,
         risk_importation_sd =   flight_to_China * risk_sd,
         risk_city_mean = avg_flight * risk_mean,
         risk_city_sd = avg_flight * risk_sd
  ) -> Importation_Risk_Result
Importation_Risk_Result = left_join(time_to_num, Importation_Risk_Result, by = "time")
write.csv(Importation_Risk_Result,"第二部分国内结果.csv")


##世界各国输入我国的总体风险
Importation_Risk_Result %>%
  dplyr::select(departure_country,
                risk_mean, 
                risk_sd,
                iso3,
                flight_to_China,
                risk_importation_mean,
                risk_importation_sd) -> Importation_Risk_country2China
Importation_Risk_country2China <- unique(Importation_Risk_country2China)
Importation_Risk_country2China = left_join(time_to_num, Importation_Risk_country2China, by = "time")
write.csv(Importation_Risk_country2China,"每个日期世界各国输入我国的风险.csv")