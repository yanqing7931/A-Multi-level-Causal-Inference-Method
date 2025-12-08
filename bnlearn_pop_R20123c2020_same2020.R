#install.packages("gplots")
# install.packages("ggpubr")
library(ggpubr)
library(bnlearn)
library(Rgraphviz)
library(grid)
library(gplots)
library(corrplot)
library(dplyr)

setwd("D:/Spatialization_population/人口空间化数据库")
#加载数据
temp = read.table("city_pop_Y6_Lat_geo9.txt",header = TRUE,sep = ',')
temp

# 检查 变量呈正态分布
par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))
for (var in c("pop_lv","dem", "road","gaia_c_lv", "crop_lv","city_lv","town_lv","gj_lv","bare_lv","black_revise","forest_lv")) {

  x = temp[, var]
  hist(x, prob = TRUE, xlab = var, ylab = "", main = var, col = "ivory")
  lines(density(x), lwd = 2, col = "tomato")
  curve(dnorm(x, mean = mean(x), sd = sd(x)), from = min(x), to = max(x),
        add = TRUE,lwd = 2, col = "steelblue")
}

vars<-c("dem","lake_lv","gaia_c_lv", "crop_lv","city_lv","town_lv","gj_lv","black_revise","forest_lv","bare_lv","geo_subdivision6","pop_lv")
diff <- temp[vars]
diff

mean_pop =mean(diff$pop_lv)
sd_pop =sd(diff$pop_lv)
diff$pop_lv = scale(diff$pop_lv)
diff$pop_lv = log(1+diff$pop_lv)
# pop_max = max(diff$pop_lv)
# pop_min = min(diff$pop_lv)
# diff$pop_lv = (diff$pop_lv-pop_min)/(pop_max-pop_min)

# lake_lv_max = max(diff$lake_lv)
# lake_lv_min = min(diff$lake_lv)
# diff$lake_lv = (diff$lake_lv-lake_lv_min)/(lake_lv_max-lake_lv_min)

gaia_max = max(diff$gaia_c_lv)
gaia_min = min(diff$gaia_c_lv)
diff$gaia_c_lv = (diff$gaia_c_lv-gaia_min)/(gaia_max-gaia_min)
# diff$gaia_c_lv = log(1+diff$gaia_c_lv)
# diff$crop_lv = log(1+diff$crop_lv)
# diff$city_lv = log(1+diff$city_lv)
# diff$town_lv = log(1+diff$town_lv)
# diff$gj_lv = log(1+diff$gj_lv)
# diff$forest_lv = log(1+diff$forest_lv)
# crop_max = max(diff$crop_lv)
# crop_min = min(diff$crop_lv)
# diff$crop_lv = (diff$crop_lv-crop_min)/(crop_max-crop_min)

lake_lv_max = max(diff$lake_lv)
lake_lv_min = min(diff$lake_lv)
diff$lake_lv = (diff$lake_lv-lake_lv_min)/(lake_lv_max-lake_lv_min)

dem_max = max(diff$dem)
dem_min = min(diff$dem)
diff$dem = (diff$dem-dem_min)/(dem_max-dem_min)
# city_max = max(diff$city_lv)
# city_min = min(diff$city_lv)
# diff$city_lv = (diff$city_lv-city_min)/(city_max-city_min)
# 
# town_max = max(diff$town_lv)
# town_min = min(diff$town_lv)
# diff$town_lv = (diff$town_lv-town_min)/(town_max-town_min)
# diff$town_lv = log(1+diff$town_lv)
# 
# gj_max = max(diff$gj_lv)
# gj_min = min(diff$gj_lv)
# diff$gj_lv = (diff$gj_lv-gj_min)/(gj_max-gj_min)
# 
# forest_max = max(diff$forest_lv)
# forest_min = min(diff$forest_lv)
# diff$forest_lv = (diff$forest_lv-forest_min)/(forest_max-forest_min)

black_max = max(diff$black_revise)
black_min = min(diff$black_revise)
diff$black_revise = (diff$black_revise-black_min)/(black_max-black_min)


diff_cv = diff
pairs(diff[, setdiff(names(diff),c("pop_lv"))],
      upper.panel = function(x, y, ...) {
        points(x = x, y = y, col = "grey",cex = 0.8)
        abline(coef(lm(y ~ x)), col = "tomato", lwd = 1)
      },
      lower.panel = function(x, y, ...) {
        par(usr = c(0, 1, 0, 1))
        text(x = 0.5, y = 0.5, round(cor(x, y), 2), cex = 1)
      }
)

diff1 = diff
diff1= diff1[-9]
diff1

# install.packages("corrplot")
par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
#集群
rho = cor(diff1)
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

#通过par参数，合理的调整绘图区大小，内边距和外边距的大小，能更好的展示图形
palette.breaks = seq(0, 1, 0.1)
par(oma=c(2,2,2,2))
heatmap.2(rho, scale = "none", trace = "none", revC = TRUE, breaks = palette.breaks)
print("3")

#按照相关系数
ug = empty.graph(colnames(rho))
ug
amat(ug) = (abs(rho) > 0.4) + 0L - diag(1L, nrow(rho))
amat(ug)
par(mgp=c(20,20,20))
graphviz.plot(ug, layout = "fdp", shape = "ellipse")
print("4")

#set.seed(12345)d
# set.seed(123456)e
# f
set.seed(123458)
diff <- diff%>%
  group_by(geo_subdivision6)%>%
  mutate(data_spilt = if_else(runif(n())<0.7,'Train','Test'))

train_data = diff%>%filter(data_spilt == "Train")
test_data = diff%>%filter(data_spilt =="Test")


# 检查 train_data 的类型
print(class(train_data))

# 如果 train_data 不是数据框，则将其转换为数据框
if (!is.data.frame(train_data)) {
  train_data <- as.data.frame(train_data)
}
class(train_data)
# 现在尝试从数据框中删除 'data_spilt' 列
train_data <- train_data %>% select(-data_spilt)

train_data$pop_lv = as.numeric(train_data$pop_lv)
train_data$dem = as.numeric(train_data$dem)
# train_data$road = as.numeric(train_data$road)
train_data$lake_lv = as.numeric(train_data$lake_lv)
train_data$gaia_c_lv=as.numeric(train_data$gaia_c_lv)
train_data$crop_lv = as.numeric(train_data$crop_lv)
train_data$city_lv = as.numeric(train_data$city_lv)
train_data$town_lv = as.numeric(train_data$town_lv)
train_data$gj_lv = as.numeric(train_data$gj_lv)
train_data$forest_lv = as.numeric(train_data$forest_lv)
# train_data$bare_lv = as.numeric(train_data$bare_lv)
train_data$black_revise=as.numeric(train_data$black_revise)
# train_data$geo_subdivision6 = as.factor(train_data$geo_subdivision6)
train_data$geo_subdivision6 = as.factor(train_data$geo_subdivision6)
print("2")

# 检查 train_data 的数据类型
str(train_data)

# 将所有整数型列转换为数值型
train_data <- data.frame(lapply(train_data, function(x) {
  if (class(x) == "integer") {
    as.numeric(x)
  } else {
    x
  }
}))

bl = tiers2blacklist(list("gaia_c_lv", c("crop_lv","city_lv","town_lv","gj_lv","black_revise","forest_lv","pop_lv","geo_subdivision6")))
# 黑名单
bl = rbind(bl, c("geo_subdivision6","lake_lv"),c("geo_subdivision6","dem"),
           c("geo_subdivision6","city_lv"),c("geo_subdivision6","bare_lv"),
           c("geo_subdivision6","crop_lv"),c("geo_subdivision6","black_revise"),
           c("geo_subdivision6","town_lv"),c("geo_subdivision6","gj_lv"),
           c("geo_subdivision6","forest_lv"),c("gj_lv","forest_lv"),
           c("city_lv","forest_lv"),c("gaia_c_lv","forest_lv"),c("black_revise","forest_lv"),
           c("crop_lv","black_revise"),c("crop_lv","city_lv"),c("crop_lv","gj_lv"),c("gj_lv","crop_lv"),
           c("lake_lv","black_revise")
)
bl

wl = matrix(c("forest_lv","pop_lv",
              "dem","gaia_c_lv",
              "gaia_c_lv","pop_lv",
              "crop_lv","pop_lv",
              "city_lv","pop_lv",
              "town_lv","pop_lv",
              "black_revise","pop_lv",
              "geo_subdivision6","pop_lv",
              "lake_lv","pop_lv",
              "town_lv","black_revise"
              
),
ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))
wl
str.raw = boot.strength(train_data, R = 200, algorithm = "hc",
                        algorithm.args = list(whitelist = wl, blacklist = bl))
str.raw
typeof(str.raw)
attr(str.raw, "threshold")

avg.raw.full = averaged.network(str.raw)
avg.raw.full
par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
strength.plot(avg.raw.full, str.raw, shape = "ellipse", highlight = list(arcs = wl))

#threshold判断电弧强度
avg.raw.simpler = averaged.network(str.raw, threshold = 0.80)
strength.plot(avg.raw.simpler, str.raw,layout = "dot", shape = "ellipse", highlight = list(arcs = wl))

# hc(train_data, whitelist = wl, blacklist = bl,restart = 0, perturb = 1, max.iter = 5, maxp = Inf, optimized = TRUE)


bnNew = bn.fit(avg.raw.simpler,data=train_data, method = "mle-cg")
bnNew
print("5")

train_diff = predict(object = bnNew, node = "pop_lv",
                   data = train_data[,1:11],method = "bayes-lw",
                   prob = FALSE)



# 残差平方和(SSE)、回归平方和(SSR)、总偏离平方和(SST)、均方差(MSE)
# R2计算
# R2_train =function(train,pre){
#   y_mean = mean(train)
#   SSR = sum((pre-y_mean)^2)
#   SSE = sum((pre-train)^2)
#   SST = sum((train-y_mean)^2)
#   R2 = SSR/SST
#   print(R2)
# }
R2_train =function(train,pre){
  y_mean = mean(train)
  SSR = sum((pre-y_mean)^2)
  SSE = sum((train-pre)^2)
  SST = sum((train-y_mean)^2)
  R2 = 1-SSE/SST
  print(R2)
}

train_R2 = R2_train(train_data$pop_lv,train_diff)
train_R2
train_mse <- mean((train_diff - train_data$pop_lv)^2)
train_rmse <- sqrt(train_mse)
train_mae <- mean(abs(train_diff - train_data$pop_lv))
train_cor <- cor(train_diff, train_data$pop_lv)^2
print(list(MSE=train_mse, RMSE=train_rmse,MAE=train_mae, COR=train_cor,R2=train_R2)) 
# 残差的标准差
residuals_sd= sd(train_diff-train_data$pop_lv)
print(list(Residuals_Sd = residuals_sd))

# 检查 test_data 的类型
print(class(test_data))
# 如果 train_data 不是数据框，则将其转换为数据框
if (!is.data.frame(test_data)) {
  test_data <- as.data.frame(test_data)
}

# 现在尝试从数据框中删除 'data_spilt' 列
test_data <- test_data %>% select(-data_spilt)
test_data$pop_lv = as.numeric(test_data$pop_lv)
test_data$dem = as.numeric(test_data$dem)
# test_data$road = as.numeric(test_data$road)
test_data$lake_lv = as.numeric(test_data$lake_lv)
test_data$gaia_c_lv=as.numeric(test_data$gaia_c_lv)
test_data$crop_lv = as.numeric(test_data$crop_lv)
test_data$city_lv = as.numeric(test_data$city_lv)
test_data$town_lv = as.numeric(test_data$town_lv)
test_data$gj_lv = as.numeric(test_data$gj_lv)
test_data$forest_lv = as.numeric(test_data$forest_lv)
# test_data$bare_lv = as.numeric(test_data$bare_lv)
test_data$black_revise=as.numeric(test_data$black_revise)
# test_data$geo_subdivision6 = as.factor(test_data$geo_subdivision6)
test_data$geo_subdivision6 = as.factor(test_data$geo_subdivision6)
print("2")

# 检查 train_data 的数据类型
str(test_data)
# 将所有整数型列转换为数值型
test_data <- data.frame(lapply(test_data, function(x) {
  if (class(x) == "integer") {
    as.numeric(x)
  } else {
    x
  }
}))

test_diff = predict(object = bnNew, node = "pop_lv",
                     data = test_data[,1:11],method = "bayes-lw",
                     prob = FALSE)
test_R2 = R2_train(test_data$pop_lv,test_diff)
test_R2

test_mse <- mean((test_diff - test_data$pop_lv)^2)
test_rmse <- sqrt(test_mse)
test_mae <- mean(abs(test_diff - test_data$pop_lv))
test_cor <- cor(test_diff, test_data$pop_lv)
print(list(MSE=test_mse, RMSE=test_rmse,MAE=test_mae, COR=test_cor,R2 = test_R2))


#检验交叉验证mse
err1 = numeric(10)
for(i in 1:10){
  xval1 = bn.cv(train_data,bn="hc",algorithm.args = list(blacklist=bl,whitelist=wl),
                loss = "mse-lw-cg",loss.args = list(target = "pop_lv",n=200),runs=10)
  
  err1[i]=mean(sapply(xval1, function(x) attr(x, "mean")))
}
xval1
round(err1, digits = 3)
mean(err1)

#检验交叉验证cor
err = numeric(10)
for(i in 1:10){
  xval = bn.cv(train_data,bn="hc",algorithm.args = list(blacklist=bl,whitelist=wl),
               loss = "cor-lw-cg",loss.args = list(target = "pop_lv",n=200),runs=10)
  
  err[i]=mean(sapply(xval, function(x) attr(x, "mean")))
}
xval
round(err, digits = 3)
mean(err)
# 负的 BIC 值可能表明模型对数据有很好的拟合，但需要注意的是，
# 较低（或较负）的 BIC 值不总是意味着模型是最佳选择。需要平衡模型的复杂度和拟合优度。
#检验BIC
# BIC=ln(n)k-2ln(L)
score(avg.raw.simpler, train_data, type = "bic-cg", by.node = TRUE)

predcor =structure(numeric(7),
                   names = c("crop_lv","black_revise", "city_lv","town_lv","gj_lv","gaia_c_lv","forest_lv"))
for (var in names(predcor)) {
  
  xval_other = bn.cv(train_data, bn = "hc",
                     algorithm.args = list(blacklist = bl, whitelist = wl), loss = "cor-lw-cg",
                     loss.args = list(target = var, n = 200), runs = 10)
  
  predcor[var] = mean(sapply(xval_other, function(x) attr(x, "mean")))
  
}#FOR

round(predcor, digits = 3)

mean(predcor)

round(coefficients(bnNew$pop_lv), digits = 4)
coefficients(bnNew$pop_lv)
dcdfit<-bnNew$pop_lv
dcdfit$parents



#预测数据
# newdata = read.table("China1000Asia.txt",header = TRUE,sep = ',')
# newdata = read.table("China1000Asia_Latp_bf4_geo10.txt",header = TRUE,sep = ',')
newdata = read.table("China1000Asia_Latp_bf4_geo10_1.txt",header = TRUE,sep = ',')
# newdata = read.table("China1000Asia_Latp_bf4_geo8.txt",header = TRUE,sep = ',')

colMeans(newdata)

vars<-c("dem","lake_lv","black_revise","gaia_c_lv", "crop_lv","city_lv","town_lv","gj_lv","bare_lv","forest_lv","geo_subdivision6")
diff_pre <- newdata[vars]
diff_pre


scale_pre =function(data,mean1,sd1){
  data_scale = (data-mean1)/sd1
}

maxmin_pre = function(data,min1,max1){
  data_minmax = (data-min1)/(max1-min1)
  return(data_minmax)
}


diff_pre$dem = maxmin_pre(diff_pre$dem,dem_min,dem_max)
# diff_pre$road = maxmin_pre(diff_pre$road,road_min,dem_max)
diff_pre$lake_lv = maxmin_pre(diff_pre$lake_lv,lake_lv_min,lake_lv_max)
diff_pre$gaia_c_lv = maxmin_pre(diff_pre$gaia_c_lv,gaia_min,gaia_max)
# diff_pre$gaia_c_lv = log(1+diff_pre$gaia_c_lv)
# diff$crop_lv = log(1+diff$crop_lv)
# diff_pre$city_lv = log(1+diff_pre$city_lv)
# diff_pre$town_lv = log(1+diff_pre$town_lv)
# diff_pre$gj_lv = log(1+diff_pre$gj_lv)
# diff_pre$forest_lv = log(1+diff_pre$forest_lv)
# diff_pre$crop_lv = maxmin_pre(diff_pre$crop_lv,crop_min,crop_max)
# diff_pre$city_lv = maxmin_pre(diff_pre$city_lv,city_min,city_max)
# diff_pre$town_lv = maxmin_pre(diff_pre$town_lv,town_min,town_max)
# diff_pre$town_lv = log(1+diff_pre$town_lv)
# 
# diff_pre$gj_lv = maxmin_pre(diff_pre$gj_lv,gj_min,gj_max)
# diff_pre$forest_lv = maxmin_pre(diff_pre$forest_lv,forest_min,forest_max)
# diff_pre$bare_lv = maxmin_pre(diff_pre$bare_lv,bare_min,bare_max)
diff_pre$black_revise = maxmin_pre(diff_pre$black_revise,black_min,black_max)


diff_pre$dem = as.numeric(diff_pre$dem)
# diff_pre$road = as.numeric(diff_pre$road)
diff_pre$lake_lv = as.numeric(diff_pre$lake_lv)
diff_pre$gaia_c_lv=as.numeric(diff_pre$gaia_c_lv)
diff_pre$crop_lv = as.numeric(diff_pre$crop_lv)
diff_pre$city_lv =as.numeric(diff_pre$city_lv)
diff_pre$town_lv =as.numeric(diff_pre$town_lv)
diff_pre$gj_lv =as.numeric(diff_pre$gj_lv)
diff_pre$forest_lv =as.numeric(diff_pre$forest_lv)
diff_pre$bare_lv = as.numeric(diff_pre$bare_lv)
diff_pre$black_revise=as.numeric(diff_pre$black_revise)


diff_pre$geo_subdivision6 = as.factor(diff_pre$geo_subdivision6)
print("6")

#预测模型
set.seed(123)
data1 = rbn(bnNew, n=1000)
prediction <- predict(object = bnNew, node = "pop_lv",
                       data = diff_pre,method = "bayes-lw",
                       prob = FALSE)
print("7")
pop_predict = as.data.frame(prediction)

pop_predict$prediction = exp(pop_predict$prediction)-1
pop_predict$prediction = pop_predict$prediction*sd_pop+mean_pop
# pop_predict$prediction = pop_predict$prediction*(pop_max-pop_min)+pop_min





#dataframe添加列
pop_predict$IIDD = newdata$IIDD
pop_predict10 = pop_predict
pop_predict$xian = newdata$xian
pop_predict$Lat = newdata$Lat
pop_predict$Lon = newdata$Lon
pop_predict$world = newdata$world
pop_predict$land = newdata$land
pop_predict$class = newdata$class_1
# row.names = F表示index省略
write.table(pop_predict, file = "./predict/pop_predict_bnlearn_Lat55f_1.csv",sep = ",",row.names = F,quote = F)

