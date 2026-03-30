# install.packages("ggpmisc")  # 安装ggpmisc包
# install.packages("rlang")
# update.packages(ask = FALSE)
setwd("D:/人口密度论文投稿/data&R")
library(lme4)
library(rlang)
library(ggplot2)
library(ggpmisc)   # 加载ggpmisc
# # 安装并加载ggpubr包
# if (!require("ggpubr")) install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(MuMIn)
library(writexl)  # 用于导出 Excel

township_data = read.table("./data1/twon_pop2010_all_class7.txt",header = TRUE,sep = ',')

township_data = na.omit(township_data)

city_min = min(township_data$city_lv)
city_max = max(township_data$city_lv)
town_min = min(township_data$town_lv)
town_max = max(township_data$town_lv)
gj_min = min(township_data$gj_lv)
gj_max = max(township_data$gj_lv)
crop_min = min(township_data$crop_lv)
crop_max = max(township_data$crop_lv)
resid_min = min(township_data$resid_lv)
resid_max =max(township_data$resid_lv)
bare_min = min(township_data$bare_lv)
bare_max = max(township_data$bare_lv)
forest_min = min(township_data$forest_lv)
forest_max = max(township_data$forest_lv)
lake_min = min(township_data$lake_lv)
lake_max = max(township_data$lake_lv)
gaia_min = min(township_data$gaia_c_lv)
gaia_max = max(township_data$gaia_c_lv)
black_min = min(township_data$black_revise)
black_max = max(township_data$black_revise)

# 假设 township_data 已经加载并包含目标变量
normalize_min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
# 对每个指定变量进行归一化
township_data <- township_data %>%
  mutate(
    city_lv = normalize_min_max(city_lv),
    town_lv = normalize_min_max(town_lv),
    gj_lv = normalize_min_max(gj_lv),
    crop_lv = normalize_min_max(crop_lv),
    resid_lv = normalize_min_max(resid_lv),
    bare_lv = normalize_min_max(bare_lv),
    forest_lv = normalize_min_max(forest_lv),
    lake_lv = normalize_min_max(lake_lv),
    gaia_c_lv = normalize_min_max(gaia_c_lv),
    black_revise = normalize_min_max(black_revise)
  )

pop_predict <- read.csv("./data1/pop_predict_bnlearn_Lat55f_2010_2.txt",header = TRUE,sep = ',')
pop_predict

pop_predict = na.omit(pop_predict)
population =pop_predict

population$city_lv <- (population$city_lv-city_min)/(city_max-city_min)
population$town_lv <- (population$town_lv - town_min)/(town_max - town_min)
population$crop_lv <- (population$crop_lv - crop_min)/(crop_max-crop_min)
population$gj_lv <- (population$gj_lv - gj_min)/(gj_max-gj_min)
population$resid_lv <-(population$resid_lv - resid_min)/(resid_max-resid_min)
population$bare_lv <-(population$bare_lv - bare_min)/(bare_max-bare_min)
population$forest_lv <-(population$forest_lv-forest_min)/(forest_max-forest_min)
population$lake_lv <-(population$lake_lv-lake_min)/(lake_max-lake_min)
population$gaia_c_lv <-(population$gaia_c_lv-gaia_min)/(gaia_max-gaia_min)
population$black_revise <-(population$black_revise-black_min)/(black_max-black_min)
population$class9
population$xiang_id1
# population$lat55_area <- population$prediction


set.seed(12386)#0.750(1+lat55_area),0.73(1+city_lv  + gj_lv + gaia_c_lv + black_revise)

# 初始化一个列表来存储训练结果
results_list <- list()
township_data$lat55_area
for (i in 1:10){

  # filtered_data <- township_data
  # 过滤掉pop_tw小于等于0的数据
  filtered_data <- township_data %>%
    filter(pop2010_tw != 0) %>%
    filter(lat55_area != 0)

  # 抽取每个class1类别的十分之一数据
  sampled_data <- filtered_data %>%
    group_by(shen, class9) %>%
    slice_sample(prop = 0.1) %>%
    ungroup()

  
  # 假设predicted_population是bnlearn得到的预测值，township_data是乡镇级数据，group_var表示乡镇的分组变量，township_variable：乡镇级别的自变量。
  # 假设predicted_population是bnlearn得到的预测值，township_data是乡镇级数据，group_var表示乡镇的分组变量，township_variable：乡镇级别的自变量。
  model <-
    lmer(pop2010_tw ~ lat55_area + (1+lat55_area|
                                  class9),
         data = sampled_data)
  
  
  # 结果摘要
  summary(model)



  pop_pre <- predict(model, newdata = population)
  pop_pre

  # 保存模型结果
  results_list[[i]] <- pop_pre
  r_squared <- r.squaredGLMM(model)
  print(paste("i=",i,":",r_squared))
  
}

# 将列表转换为数据框
results_df <- do.call(cbind, results_list)

# 计算每个样本的平均预测值
avg_predict <- rowMeans(results_df, na.rm = TRUE)
# avg_predict
# 查看结果
print("每次预测的结果表格：")
print(results_df)
print("平均预测结果：")
print(avg_predict)
pop_pre = avg_predict

predictpop = as.data.frame(pop_pre)
# predictpop
predictpop$pop_old = pop_predict$prediction
# predictpop
predictpop$IIDD = pop_predict$IIDD
predictpop$lat = pop_predict$Lat
predictpop$lon = pop_predict$Lon
predictpop$xian = pop_predict$xian
predictpop$class9 = pop_predict$class9
predictpop$world = pop_predict$world
predictpop$land = pop_predict$land
predictpop$RF = pop_predict$RF
predictpop$pop_xian = pop_predict$pop
predictpop$xiang_id = pop_predict$xiang_id1


# predictpop
write.table(predictpop, file = "./predict/predictpop_bnlearn_Lat55f_2010_1.csv",sep = ",",row.names = F,quote = F)


#根据xian进行统计pop_pre0的和
predictpop$pop_pre0 = predictpop$pop_pre
predictpop['pop_pre0'][predictpop['pop_pre0']< 0] <- 0
pre_sum = predictpop %>%
  group_by(xian) %>%
  summarise(pop_pre0_sum = sum(pop_pre0,na.rm = TRUE))
pre_sum
predictpop = predictpop %>%
  left_join(pre_sum,by = 'xian')

predictpop = predictpop %>%
  mutate(
    pop_re = ifelse(pop_pre0_sum != 0, pop_pre0*(pop_xian/pop_pre0_sum),NA)
  )

write.table(predictpop, file = "./predict/predictpop_bnlearn_Lat55f_2010_1_suiji.csv",sep = ",",row.names = F,quote = F)

#根据xian进行统计pop_pre0的和(predictpop$pop_pre0 = predictpop$pop_pre)
predictpop$pop_old0 = predictpop$pop_old
predictpop['pop_old0'][predictpop['pop_old0']< 0] <- 0
pre_old_sum = predictpop %>%
  group_by(xian) %>%
  summarise(pop_old0_sum = sum(pop_old0,na.rm = TRUE))
pre_old_sum
predictpop = predictpop %>%
  left_join(pre_old_sum,by = 'xian')
#predictpop
predictpop = predictpop %>%
  mutate(
    pop_old_re = ifelse(pop_old0_sum != 0, pop_old0*(pop_xian/pop_old0_sum),NA)
  )

pre_xiang = predictpop %>%
  group_by(xiang_id) %>%
  summarise(pre_xiang = sum(pop_re,na.rm = TRUE))

pre_xiang_old = predictpop %>%
  group_by(xiang_id) %>%
  summarise(pre_xiang_old = sum(pop_old_re,na.rm = TRUE))

write.table(pre_xiang, file = "./predict/predictpop_bnlearn_Lat55f_2010_1_id.csv",sep = ",",row.names = F,quote = F)

pre_xiang$id = pre_xiang$xiang_id
pre_xiang_old$id = pre_xiang_old$xiang_id

town_Y = read.table("./data1/twon_pop2010_all_class7.txt",header = TRUE,sep = ',')

town_Y = town_Y %>%
  left_join(pre_xiang,by = 'id')


town_Y = town_Y %>%
  left_join(pre_xiang_old,by = 'id')


town_Y1 <- na.omit(town_Y)
town_Y1 <- town_Y1 %>%
  filter(pop2010>0)


town_Y2 <- town_Y1
town_Y2$pop2010 = town_Y2$pop2010/10000
town_Y2$pre_xiang_old = town_Y2$pre_xiang_old/10000
town_Y2$pre_xiang = town_Y2$pre_xiang/10000
town_Y2$RF = town_Y2$RF/10000
town_Y2$world = town_Y2$world/10000
town_Y2$land = town_Y2$land/10000

#差异率
variance_yq = function(pop, world){
  var = (world - pop)/pop
  return(var)
}

mae <-function(actual, predicted){
  mean(abs(actual - predicted))
}

mse <- function(actual, predicted) {
  print(mean((actual - predicted) ^ 2))
}

rmse <- function(actual, predicted) {
  sqrt(mse(actual, predicted))
}

r_squared <- function(actual, predicted) {
  correlation <- cor(actual, predicted)
  
  print(correlation^2)
}

adjusted_r_squared <- function(actual, predicted, n, p) {
  r2 <- r_squared(actual, predicted)
  1 - ((1 - r2) * (n - 1) / (n - p - 1))
}
#R2计算
R2_train =function(actual,pre){
  ss_res <- sum((actual - pre)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 = 1 - (ss_res / ss_tot)
  print(r2)
}


R3_train = function(train,pre){
  tss <- sum((train - mean(train))^2)
  rss <- sum((train - pre)^2)
  r_s <- 1-(rss/tss)
  print(r_s)
}

# set.seed(12322)
# 存储所有省份的结果
final_results2010 <- data.frame(shen = character(), 
                                land = numeric(), world = numeric(), tree = numeric(), 
                                pre_xiang = numeric(), pre_xiang_old = numeric())

#先按照省份分组
groups <- split(town_Y2, town_Y2$shen)
for(group_id in names(groups)){
  print(group_id)
  group_data <- groups[[group_id]]
  print(group_data)
  results <- data.frame(iteration = 1:10, 
                        land = numeric(10), world = numeric(10), tree = numeric(10), 
                        pre_xiang = numeric(10),pre_xiang_old = numeric(10))
  # 循环10次
  for (i in 1:10) {
    # 按照 "省代码" 字段进行分组，并在每个省份内随机抽取10%的数据
    sampled_town <- town_Y2 %>%
      group_by(shen) %>%  # 按照省代码分组
      sample_frac(0.1) %>%   # 每个组抽取10%的数据
      ungroup()              # 去掉分组信息

    results$land[i] <- r_squared(sampled_town$pop2010, sampled_town$land)

    results$world[i] <- r_squared(sampled_town$pop2010, sampled_town$world)

    results$tree[i] <- r_squared(sampled_town$pop2010, sampled_town$RF)

    results$pre_xiang[i] <- r_squared(sampled_town$pop2010, sampled_town$pre_xiang)

    results$pre_xiang_old[i] <- r_squared(sampled_town$pop2010, sampled_town$pre_xiang_old)

  }
  # 计算 10 次预测值的均值
  mean_results <- results %>%
    summarise(across(land:pre_xiang_old, mean, na.rm = TRUE)) %>%
    mutate(shen = group_id) %>%  
    relocate(shen, .before = land)  # 确保省份名称在第一列
  
  # 合并到最终表格
  final_results2010 <- bind_rows(final_results2010, mean_results)
  
}
mean(final_results2010$pre_xiang)
# 保存为 Excel，每个省份作为一个 Sheet
write_xlsx(final_results2010, "./predict/province_results2010.xlsx")

# set.seed(12322)
# 存储所有省份的结果
final_results2010xz <- data.frame(classid = character(), 
                                land = numeric(), world = numeric(), tree = numeric(), 
                                pre_xiang = numeric(), pre_xiang_old = numeric())

# 假设 town_Y2 是你的数据框，首先创建 classid 列
town_Y2 <- town_Y2 %>%
  mutate(classid = ifelse(class1 == 3, 2, class1))  # 将class1列中值为3的行，classid列设置为2

#先按照省份分组
groups <- split(town_Y2, town_Y2$classid)
for(group_id in names(groups)){
  group_data <- groups[[group_id]]
  
  results <- data.frame(iteration = 1:10, 
                        land = numeric(10), world = numeric(10), tree = numeric(10), 
                        pre_xiang = numeric(10),pre_xiang_old = numeric(10))
  # 循环10次
  for (i in 1:10) {
    # 按照 "省代码" 字段进行分组，并在每个省份内随机抽取10%的数据
    sampled_town <- town_Y2 %>%
      group_by(classid) %>%  # 按照省代码分组
      sample_frac(0.1) %>%   # 每个组抽取10%的数据
      ungroup()              # 去掉分组信息
    
    # 计算各个指标，并存储到相应的变量中
    results$land[i] <- r_squared(sampled_town$pop2010, sampled_town$land)
    results$world[i] <- r_squared(sampled_town$pop2010, sampled_town$world)

    results$tree[i] <- r_squared(sampled_town$pop2010, sampled_town$RF)

    results$pre_xiang[i] <- r_squared(sampled_town$pop2010, sampled_town$pre_xiang)

    results$pre_xiang_old[i] <- r_squared(sampled_town$pop2010, sampled_town$pre_xiang_old)

  }
  # 计算 10 次预测值的均值
  mean_resultsxz <- results %>%
    summarise(across(land:pre_xiang_old, mean, na.rm = TRUE)) %>%
    mutate(classid = group_id) %>%  
    relocate(classid, .before = land)  # 确保省份名称在第一列
  
  # 合并到最终表格
  final_results2010xz <- bind_rows(final_results2010xz, mean_resultsxz)
  
}


mean(final_results2010xz$pre_xiang)

# 保存为 Excel，每个省份作为一个 Sheet
write_xlsx(final_results2010xz, "./predict/province_results2010xz.xlsx")
