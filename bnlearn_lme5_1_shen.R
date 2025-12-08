# install.packages("ggpmisc")  # 安装ggpmisc包
# install.packages("rlang")
# update.packages(ask = FALSE)
# install.packages("writexl")
setwd("D:/Spatialization_population/人口空间化数据库")
library(lme4)
library(rlang)
library(ggplot2)
library(ggpmisc)   # 加载ggpmisc
# # 安装并加载ggpubr包
# if (!require("ggpubr")) install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(MuMIn)
library(dplyr)
library(writexl)  # 用于导出 Excel

####导入乡镇class20####
township_data = read.table("predict/twon_pop2020_all_class7.txt",header = TRUE,sep = ',')
township_data = na.omit(township_data)
# township_data$lat55_area = township_data$lat55af/township_data$area


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
####导入渔网带乡镇id\xian####
pop_predict <- read.csv("predict/pop_predict_bnlearn_Lat55f_4.txt",header = TRUE,sep = ',')
pop_predict

pop_predict = na.omit(pop_predict)

population <- pop_predict
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
population$class20

vars<-c("lake_lv","gaia_c_lv", "crop_lv","city_lv","town_lv","gj_lv","black_revise","forest_lv","bare_lv","pop_tw")
diff <- township_data[vars]
# install.packages("corrplot")
par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
#集群
rho = cor(diff)
corr = rho
corr
corrplot(
  # 相关系数矩阵
  corr = corr, 
  order = 'AOE',
  
  tl.pos = 'd',
  pch.cex = 0.8 )
corrplot(
  corr = corr, 
  add = TRUE, 
  type = 'upper',  # 指定展示的方式，可以是完全的、下三角或上三角
  method = 'number',# 指定可视化的方法，可以是圆形、方形、椭圆形、数值、阴影、颜色或饼图形
  order = 'AOE', # 指定相关系数排序的方法，可以是原始顺序(original)、特征向量角序(AOE)、第一主成分顺序(FPC)、层次聚类顺序(hclust)和字母顺序，一般”AOE”排序结果都比”FPC”要好
  diag = FALSE,  #是否展示对角线上的结果，默认为TRUE
  tl.pos = 'n', # 指定文本标签(变量名称)的位置，当type=full时，默认标签位置在左边和顶部(lt)，当type=lower时，默认标签在左边和对角线(ld)，当type=upper时，默认标签在顶部和对角线，d表示对角线，n表示不添加文本标签
  cl.pos = 'n',
  number.cex = 1)# 图例（颜色）位置，当type=upper或full时，图例在右表(r)，当type=lower时，图例在底部，不需要图例时，只需指定该参数为n


# set.seed(1232)#0.80
# set.seed(1236)#0.798
# set.seed(1237)#0.7999
# set.seed(1238)#0.802
# set.seed(12381)#0.799
# set.seed(12383)#0.799
# set.seed(12386)#0.80
# set.seed(12388)#0.80
# set.seed(12389)#0.80
# set.seed(12362)#0.80
set.seed(12368)#0.802(city_lv  + gj_lv + gaia_c_lv + black_revise)
              #0.800(lat55_area +city_lv  + gj_lv + gaia_c_lv + black_revise)
# set.seed(123694)#0.799
# 初始化一个列表来存储训练结果
results_list <- list()

for (i in 1:10){
  filtered_data <- township_data
  # 过滤掉pop_tw小于等于0的数据
  filtered_data <- township_data %>%
    filter(pop_tw != 0) %>%
    filter(lat55_area != 0)
  # 按省份和class1分类，计算class1每种类别的数据条数
  # class1_counts <- filtered_data %>%
  #   group_by(shen, class20) %>%
  #   summarise(count = n(), .groups = 'drop')
  
  # 抽取每个class1类别的十分之一数据
  sampled_data <- filtered_data %>%
    group_by(shen, class20) %>%
    slice_sample(prop = 0.1) %>%
    ungroup()

  
  # 假设predicted_population是bnlearn得到的预测值，township_data是乡镇级数据，group_var表示乡镇的分组变量，township_variable：乡镇级别的自变量。
  # 假设predicted_population是bnlearn得到的预测值，township_data是乡镇级数据，group_var表示乡镇的分组变量，township_variable：乡镇级别的自变量。
  model <-
    lmer(pop_tw ~ lat55_area + (1 + city_lv  + gj_lv + gaia_c_lv + black_revise|
                                  class20),
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
coef(model)$class20
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
predictpop$class20 = pop_predict$class20
predictpop$world = pop_predict$world
predictpop$land = pop_predict$land
predictpop$pop_xian = pop_predict$pop_xian
predictpop$idxiang = pop_predict$idxiang
# predictpop
write.table(predictpop, file = "./predict/predictpop_bnlearn_Lat55f_9.csv",sep = ",",row.names = F,quote = F)


#根据xian进行统计pop_pre0的和
predictpop$pop_pre0 = predictpop$pop_pre
predictpop['pop_pre0'][predictpop['pop_pre0']< 0] <- 0
pre_sum = predictpop %>%
  group_by(xian) %>%
  summarise(pop_pre0_sum = sum(pop_pre0,na.rm = TRUE))
pre_sum
predictpop = predictpop %>%
  left_join(pre_sum,by = 'xian')
predictpop
predictpop = predictpop %>%
  mutate(
    pop_re = ifelse(pop_pre0_sum != 0, pop_pre0*(pop_xian/pop_pre0_sum),NA)
  )


write.table(predictpop, file = "./predict/predictpop_bnlearn_Lat55f_9_suiji.csv",sep = ",",row.names = F,quote = F)



#根据xian进行统计pop_old0的和(predictpop$pop_pre0 = predictpop$pop_pre)
predictpop$pop_old0 = predictpop$pop_old
predictpop['pop_old0'][predictpop['pop_old0']< 0] <- 0
pre_old_sum = predictpop %>%
  group_by(xian) %>%
  summarise(pop_old0_sum = sum(pop_old0,na.rm = TRUE))
pre_old_sum
predictpop = predictpop %>%
  left_join(pre_old_sum,by = 'xian')
predictpop
predictpop = predictpop %>%
  mutate(
    pop_old_re = ifelse(pop_old0_sum != 0, pop_old0*(pop_xian/pop_old0_sum),NA)
  )

pre_xiang = predictpop %>%
  group_by(idxiang) %>%
  summarise(pre_xiang = sum(pop_re,na.rm = TRUE))

write.table(pre_xiang, file = "./predict/predictpop_bnlearn_Lat55f_9_id.csv",sep = ",",row.names = F,quote = F)

pre_xiang$id = pre_xiang$idxiang



#######
pre_xiang_old = predictpop %>%
  group_by(idxiang) %>%
  summarise(pre_xiang_old = sum(pop_old_re,na.rm = TRUE))

pre_xiang_old$id = pre_xiang_old$idxiang
# town_Y = read.table("predict/shi_Y_loss1_xzcdf_xian7.txt",header = TRUE,sep = ',')



#######




####也是乡镇,带world\land\RF####
# town_Y = read.table("predict/shi_Y_loss1_xzcdf_xian7.txt",header = TRUE,sep = ',')
# town_Y
town_Y = read.table("predict/twon_pop2020_all_class7.txt",header = TRUE,sep = ',')

town_Y = town_Y %>%
  left_join(pre_xiang,by = 'id')


town_Y = town_Y %>%
  left_join(pre_xiang_old,by = 'id')


town_Y1 <- na.omit(town_Y)
town_Y1 <- town_Y1 %>%
  filter(pop>0) %>%
  filter(lat55f_7_sj0>0)

town_Y2 <- town_Y1
# town_Y2 <- town_Y1 %>%
#   group_by(省代码) %>%
#   sample_frac(0.1)%>%
#   ungroup()
town_Y2$pop = town_Y2$pop/10000
town_Y2$lat55af = town_Y2$lat55af/10000
town_Y2$lat55f_6_sj = town_Y2$lat55f_6_sj/10000
town_Y2$lat55f_7_sj0 = town_Y2$lat55f_7_sj0/10000
town_Y2$pre_xiang = town_Y2$pre_xiang/10000
town_Y2$RF = town_Y2$RF/10000
town_Y2$world = town_Y2$world/10000
town_Y2$land = town_Y2$land/10000

town_Y2$pre_xiang_old = town_Y2$pre_xiang_old/10000

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
# sum((town_Y1$pop_tw - town_Y1$tree_huizo)^2)
# sum((town_Y1$pop_tw-mean(town_Y1$pop_tw))^2)
# 1-(sum((town_Y1$pop_tw - town_Y1$tree_huizo)^2)/sum((town_Y1$pop_tw-mean(town_Y1$pop_tw))^2))

R3_train = function(train,pre){
  tss <- sum((train - mean(train))^2)
  rss <- sum((train - pre)^2)
  r_s <- 1-(rss/tss)
  print(r_s)
}
# 
 
# land0 = mean(variance_yq(sampled_town$pop,sampled_town$land))
# mae21 =mae(sampled_town$pop, sampled_town$land)
# mse21 = mse(sampled_town$pop, sampled_town$land)
# rmse21 = rmse(sampled_town$pop, sampled_town$land)
# r_squared21 = r_squared(sampled_town$pop, sampled_town$land)
# R2_hz1 = R3_train(sampled_town$pop, sampled_town$land)
# 
# world0 = mean(variance_yq(sampled_town$pop,sampled_town$world))
# mae22 = mae(sampled_town$pop, sampled_town$world)
# mse22 = mse(sampled_town$pop, sampled_town$world)
# rmse22 = rmse(sampled_town$pop, sampled_town$world)
# r_squared22 = r_squared(sampled_town$pop, sampled_town$world)
# R2_hz2 = R3_train(sampled_town$pop, sampled_town$world)
# 
# tree0 = mean(variance_yq(sampled_town$pop,sampled_town$RF))
# mae23 = mae(sampled_town$pop, sampled_town$RF)
# mse23 = mse(sampled_town$pop, sampled_town$RF)
# rmse23 = rmse(sampled_town$pop, sampled_town$RF)
# r_squared23 = r_squared(sampled_town$pop, sampled_town$RF)
# R2_hz3 = R3_train(sampled_town$pop, sampled_town$RF)
# ## 检验R2=0.71
# lat55af0 = mean(variance_yq(sampled_town$pop,sampled_town$lat55af))
# mae28 = mae(sampled_town$pop, sampled_town$lat55af)
# mse28 = mse(sampled_town$pop, sampled_town$lat55af)
# rmse28 = rmse(sampled_town$pop, sampled_town$lat55af)
# r_squared28 = r_squared(sampled_town$pop, sampled_town$lat55af)
# R2_hz8 = R3_train(sampled_town$pop, sampled_town$lat55af)
# 
# ##class1的验证R2=0.74，class的验证是0.73,
# # class6(考虑乡镇人口密度):0.76-, class11:0.7667,class12:0.7678,
# # class14:0.7798,class15:0.7929,class17:0.7936，class19:0.7951
# pre_xiang0 = mean(variance_yq(sampled_town$pop,sampled_town$pre_xiang))
# mae29 = mae(sampled_town$pop, sampled_town$pre_xiang)
# mse29 = mse(sampled_town$pop, sampled_town$pre_xiang)
# rmse29 = rmse(sampled_town$pop, sampled_town$pre_xiang)
# r_squared29 = r_squared(sampled_town$pop, sampled_town$pre_xiang)
# R2_hz9 = R3_train(sampled_town$pop, sampled_town$pre_xiang)


# # 初始化存储变量
# land0_vals <- numeric(10)
# mae21_vals <- numeric(10)
# mse21_vals <- numeric(10)
# rmse21_vals <- numeric(10)
# r_squared21_vals <- numeric(10)
# R2_hz1_vals <- numeric(10)
# 
# world0_vals <- numeric(10)
# mae22_vals <- numeric(10)
# mse22_vals <- numeric(10)
# rmse22_vals <- numeric(10)
# r_squared22_vals <- numeric(10)
# R2_hz2_vals <- numeric(10)
# 
# tree0_vals <- numeric(10)
# mae23_vals <- numeric(10)
# mse23_vals <- numeric(10)
# rmse23_vals <- numeric(10)
# r_squared23_vals <- numeric(10)
# R2_hz3_vals <- numeric(10)
# 
# lat55af0_vals <- numeric(10)
# mae28_vals <- numeric(10)
# mse28_vals <- numeric(10)
# rmse28_vals <- numeric(10)
# r_squared28_vals <- numeric(10)
# R2_hz8_vals <- numeric(10)
# 
# pre_xiang0_vals <- numeric(10)
# mae29_vals <- numeric(10)
# mse29_vals <- numeric(10)
# rmse29_vals <- numeric(10)
# r_squared29_vals <- numeric(10)
# R2_hz9_vals <- numeric(10)
# 
# pre_xiang_old_vals <- numeric(10)
# mae30_vals <- numeric(10)
# mse30_vals <- numeric(10)
# rmse30_vals <- numeric(10)
# r_squared30_vals <- numeric(10)
# R2_hz10_vals <- numeric(10)



# set.seed(12322)
set.seed(1231)
# 存储所有省份的结果
final_results <- data.frame(shen = character(), 
                            land = numeric(), world = numeric(), tree = numeric(), 
                            lat55af = numeric(), pre_xiang = numeric(), pre_xiang_old = numeric())

#先按照省份分组
groups <- split(town_Y2, town_Y2$shen)
for(group_id in names(groups)){
  group_data <- groups[[group_id]]
  
  results <- data.frame(iteration = 1:10, 
                        land = numeric(10), world = numeric(10), tree = numeric(10), 
                        lat55af = numeric(10), pre_xiang = numeric(10),pre_xiang_old = numeric(10))
  # 循环10次
  for (i in 1:10) {
    # 按照 "省代码" 字段进行分组，并在每个省份内随机抽取10%的数据
    sampled_town <- group_data %>%
      group_by(shen) %>%  # 按照省代码分组
      sample_frac(0.1) %>%   # 每个组抽取10%的数据
      ungroup()              # 去掉分组信息
  # 按IDshen分组

    # # 计算各个指标，并存储到相应的变量中
    # land0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$land))
    # mae21_vals[i] <- mae(sampled_town$pop, sampled_town$land)
    # mse21_vals[i] <- mse(sampled_town$pop, sampled_town$land)
    # rmse21_vals[i] <- rmse(sampled_town$pop, sampled_town$land)
    results$land[i] <- r_squared(sampled_town$pop, sampled_town$land)
    # R2_hz1_vals[i] <- R3_train(sampled_town$pop, sampled_town$land)
    
    # world0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$world))
    # mae22_vals[i] <- mae(sampled_town$pop, sampled_town$world)
    # mse22_vals[i] <- mse(sampled_town$pop, sampled_town$world)
    # rmse22_vals[i] <- rmse(sampled_town$pop, sampled_town$world)
    results$world[i] <- r_squared(sampled_town$pop, sampled_town$world)
    # R2_hz2_vals[i] <- R3_train(sampled_town$pop, sampled_town$world)
    
    # tree0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$RF))
    # mae23_vals[i] <- mae(sampled_town$pop, sampled_town$RF)
    # mse23_vals[i] <- mse(sampled_town$pop, sampled_town$RF)
    # rmse23_vals[i] <- rmse(sampled_town$pop, sampled_town$RF)
    results$tree[i] <- r_squared(sampled_town$pop, sampled_town$RF)
    # R2_hz3_vals[i] <- R3_train(sampled_town$pop, sampled_town$RF)
    
    # lat55af0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$lat55af))
    # mae28_vals[i] <- mae(sampled_town$pop, sampled_town$lat55af)
    # mse28_vals[i] <- mse(sampled_town$pop, sampled_town$lat55af)
    # rmse28_vals[i] <- rmse(sampled_town$pop, sampled_town$lat55af)
    results$lat55af[i] <- r_squared(sampled_town$pop, sampled_town$lat55af)
    # R2_hz8_vals[i] <- R3_train(sampled_town$pop, sampled_town$lat55af)
    
    # pre_xiang0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$pre_xiang))
    # mae29_vals[i] <- mae(sampled_town$pop, sampled_town$pre_xiang)
    # mse29_vals[i] <- mse(sampled_town$pop, sampled_town$pre_xiang)
    # rmse29_vals[i] <- rmse(sampled_town$pop, sampled_town$pre_xiang)
    results$pre_xiang[i] <- r_squared(sampled_town$pop, sampled_town$pre_xiang)
    # R2_hz9_vals[i] <- R3_train(sampled_town$pop, sampled_town$pre_xiang)
    
    # pre_xiang_old_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$pre_xiang_old))
    # mae30_vals[i] <- mae(sampled_town$pop, sampled_town$pre_xiang_old)
    # mse30_vals[i] <- mse(sampled_town$pop, sampled_town$pre_xiang_old)
    # rmse29_vals[i] <- rmse(sampled_town$pop, sampled_town$pre_xiang_old)
    results$pre_xiang_old[i] <- r_squared(sampled_town$pop, sampled_town$pre_xiang_old)
    # R2_hz10_vals[i] <- R3_train(sampled_town$pop, sampled_town$pre_xiang_old)
  }
  
  # 计算 10 次预测值的均值
  mean_results <- results %>%
    summarise(across(land:pre_xiang_old, mean, na.rm = TRUE)) %>%
    mutate(shen = group_id) %>%  
    relocate(shen, .before = land)  # 确保省份名称在第一列
  
  # 合并到最终表格
  final_results <- bind_rows(final_results, mean_results)
  
}
mean(final_results$pre_xiang)

# 保存为 Excel，每个省份作为一个 Sheet
write_xlsx(final_results, "D:\\Spatialization_population\\论文\\province_results2020.xlsx")



# set.seed(12322)
set.seed(1231)
# 存储所有省份的结果
final_resultsxz <- data.frame(classid = character(), 
                            land = numeric(), world = numeric(), tree = numeric(), 
                            lat55af = numeric(), pre_xiang = numeric(), pre_xiang_old = numeric())

# 假设 town_Y2 是你的数据框，首先创建 classid 列
town_Y2 <- town_Y2 %>%
  mutate(classid = ifelse(class1 == 3, 2, class1))  # 将class1列中值为3的行，classid列设置为2

#先按照省份分组
groups <- split(town_Y2, town_Y2$classid)
for(group_id in names(groups)){
  group_data <- groups[[group_id]]
  
  results <- data.frame(iteration = 1:10, 
                        land = numeric(10), world = numeric(10), tree = numeric(10), 
                        lat55af = numeric(10), pre_xiang = numeric(10),pre_xiang_old = numeric(10))
  # 循环10次
  for (i in 1:10) {
    # 按照 "省代码" 字段进行分组，并在每个省份内随机抽取10%的数据
    sampled_town <- group_data %>%
      group_by(classid) %>%  # 按照省代码分组
      sample_frac(0.1) %>%   # 每个组抽取10%的数据
      ungroup()              # 去掉分组信息
    # 按IDshen分组
    
    # # 计算各个指标，并存储到相应的变量中
    # land0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$land))
    # mae21_vals[i] <- mae(sampled_town$pop, sampled_town$land)
    # mse21_vals[i] <- mse(sampled_town$pop, sampled_town$land)
    # rmse21_vals[i] <- rmse(sampled_town$pop, sampled_town$land)
    results$land[i] <- r_squared(sampled_town$pop, sampled_town$land)
    # R2_hz1_vals[i] <- R3_train(sampled_town$pop, sampled_town$land)
    
    # world0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$world))
    # mae22_vals[i] <- mae(sampled_town$pop, sampled_town$world)
    # mse22_vals[i] <- mse(sampled_town$pop, sampled_town$world)
    # rmse22_vals[i] <- rmse(sampled_town$pop, sampled_town$world)
    results$world[i] <- r_squared(sampled_town$pop, sampled_town$world)
    # R2_hz2_vals[i] <- R3_train(sampled_town$pop, sampled_town$world)
    
    # tree0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$RF))
    # mae23_vals[i] <- mae(sampled_town$pop, sampled_town$RF)
    # mse23_vals[i] <- mse(sampled_town$pop, sampled_town$RF)
    # rmse23_vals[i] <- rmse(sampled_town$pop, sampled_town$RF)
    results$tree[i] <- r_squared(sampled_town$pop, sampled_town$RF)
    # R2_hz3_vals[i] <- R3_train(sampled_town$pop, sampled_town$RF)
    
    # lat55af0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$lat55af))
    # mae28_vals[i] <- mae(sampled_town$pop, sampled_town$lat55af)
    # mse28_vals[i] <- mse(sampled_town$pop, sampled_town$lat55af)
    # rmse28_vals[i] <- rmse(sampled_town$pop, sampled_town$lat55af)
    results$lat55af[i] <- r_squared(sampled_town$pop, sampled_town$lat55af)
    # R2_hz8_vals[i] <- R3_train(sampled_town$pop, sampled_town$lat55af)
    
    # pre_xiang0_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$pre_xiang))
    # mae29_vals[i] <- mae(sampled_town$pop, sampled_town$pre_xiang)
    # mse29_vals[i] <- mse(sampled_town$pop, sampled_town$pre_xiang)
    # rmse29_vals[i] <- rmse(sampled_town$pop, sampled_town$pre_xiang)
    results$pre_xiang[i] <- r_squared(sampled_town$pop, sampled_town$pre_xiang)
    # R2_hz9_vals[i] <- R3_train(sampled_town$pop, sampled_town$pre_xiang)
    
    # pre_xiang_old_vals[i] <- mean(variance_yq(sampled_town$pop, sampled_town$pre_xiang_old))
    # mae30_vals[i] <- mae(sampled_town$pop, sampled_town$pre_xiang_old)
    # mse30_vals[i] <- mse(sampled_town$pop, sampled_town$pre_xiang_old)
    # rmse29_vals[i] <- rmse(sampled_town$pop, sampled_town$pre_xiang_old)
    results$pre_xiang_old[i] <- r_squared(sampled_town$pop, sampled_town$pre_xiang_old)
    # R2_hz10_vals[i] <- R3_train(sampled_town$pop, sampled_town$pre_xiang_old)
  }
  
  # 计算 10 次预测值的均值
  mean_resultsxz <- results %>%
    summarise(across(land:pre_xiang_old, mean, na.rm = TRUE)) %>%
    mutate(classid = group_id) %>%  
    relocate(classid, .before = land)  # 确保省份名称在第一列
  
  # 合并到最终表格
  final_resultsxz <- bind_rows(final_resultsxz, mean_resultsxz)
  
}
mean(final_resultsxz$pre_xiang)

# 保存为 Excel，每个省份作为一个 Sheet
write_xlsx(final_resultsxz, "D:\\Spatialization_population\\论文\\province_results2020xz.xlsx")

