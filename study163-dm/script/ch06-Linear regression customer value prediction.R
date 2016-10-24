#第6讲 线性回归模型与诊断
##数据说明：本数据是一份汽车贷款违约数据
##名称	中文含义
#id	id
#Acc	是否开卡(1=已开通)
#avg_exp	月均信用卡支出（元）
#avg_exp_ln	月均信用卡支出的自然对数
#gender	性别(男=1)
#Age	年龄
#Income	年收入（万元）
#Ownrent	是否自有住房（有=1；无=0)
#Selfempl	是否自谋职业(1=yes, 0=no)
#dist_home_val	所住小区房屋均价(万元)
#dist_avg_income	当地人均收入
#high_avg	高出当地平均收入
#edu_class	教育等级：小学及以下开通=0，中学=1，本科=2，研究生=3
#—————————————————————————————————————————————————————————————————————————————————————————————————————————————###

setwd('D:\\R_edu\\data\\')
#导入数据和数据清洗
creditcard_exp<-read.csv("creditcard_exp.csv")
creditcard_exp<-na.omit(creditcard_exp)
attach(creditcard_exp)
creditcard_exp$gender<-as.factor(creditcard_exp$gender)
#6.1 相关性分析
#散点图
plot(Income,avg_exp)

#Correlation:"spearman","pearson" and "kendall",
cor.test(Income,avg_exp,method="pearson")
cor.test(Income,avg_exp,method="spearman")

#6.2 线性回归
#6.2.1简单线性回归
lm_s<-lm(avg_exp~Income)

coef(lm_s)
summary(lm_s)
#Predict
creditcard_exp$Pred1_avg_exp<-predict(lm_s)
creditcard_exp$Pred2_avg_exp<-fitted(lm_s)
creditcard_exp$resid_avg_exp<-resid(lm_s)

#6.2.2多元线性回归
lm_m<-lm(avg_exp~Age+Income+dist_home_val+dist_avg_income,data=creditcard_exp)
coef(lm_m)
summary(lm_m)

#6.2.3多元线性回归的变量筛选
lm_m<-lm(avg_exp~Age+Income+dist_home_val+dist_avg_income,data=creditcard_exp)
summary(lm_m)
#lm_ms<-step(lm_m,direction = "forward")
#step(lm_m,direction = "backward")
lm_ms<-step(lm_m,direction = "both")
summary(lm_ms)

#6.3线性回归的诊断
#6.3.1残差分析
exp <- read.csv("creditcard_exp.csv")
names(exp)
exp <-na.omit(exp)
ana1<-lm(avg_exp~Income,data=exp)
summary(ana1)
exp$pred<-predict(ana1)
exp$res<-resid(ana1)
#Why use pred?
plot(exp$res~exp$pred)

library(lmtest)
bptest(ana1)

#遇到异方差情况,教科书上会介绍使用加权最小二乘法
#但是实际上最常用的是对被解释变量取对数
ana2<-lm(avg_exp_ln~Income,data=exp)
summary(ana2)
exp$pred<-predict(ana2)
exp$res<-resid(ana2)
#Why use pred?
plot(exp$res~exp$pred)
library(lmtest)
bptest(ana2)

#但是这样会使模型失去解释意义
exp$Income_ln<-log(exp$Income)
ana3<-lm(avg_exp_ln~Income_ln,data=exp)
summary(ana3)
exp$pred<-predict(ana3)
exp$res<-resid(ana3)
plot(exp$res~exp$pred)
library(lmtest)
bptest(ana3)


#寻找最优的模型
Rsq<-c(summary(ana1)$r.squared,summary(ana2)$r.squared,summary(ana3)$r.squared)
names(Rsq)<-c("EXP~Income","LN(EXP)~Income","LN(EXP)~LN(Income)")
Rsq



#6.3.2强影响点分析
exp$res_t<-(exp$res-mean(exp$res))/sd(exp$res)
#Find outlier
exp$res_t[abs(exp$res_t)>2]
#Drop outlier
exp2<-subset(exp,abs(res_t)<=2,select=id:Income_ln)
ana4<-lm(avg_exp_ln~Income_ln,data=exp2)
exp2$pred<-predict(ana4)
exp2$res<-resid(ana4)
plot(exp2$res~exp2$pred)
summary(ana4)


#6.3.3多重共线性分析
names(exp2)
exp2$dist_avg_income_ln<-log(exp2$dist_avg_income)
ana5<-lm(avg_exp~Income+Age+dist_home_val+dist_avg_income)
library(car)
vif(ana5)
#The most easy way is to use step regression. However, it is not always work.
ana6<-step(ana5,direction = "both")
summary(ana6)
vif(ana6)
ana7<-lm(avg_exp~dist_home_val+dist_avg_income+high_avg)
vif(ana7)

#################################################################################
#6.4 正则算法
#6.4.1岭回归
#在R中实现岭回归的包有很多，比如glmnet和MASS，本节使用两种方法分别做演示

library(MASS)
lmr<-lm.ridge(avg_exp~.,data=creditcard_exp[,c(3,6,7,10,11)],lambda=seq(0,10,1))
plot(lmr)
select(lmr)

names(creditcard_exp)
x <- as.matrix(creditcard_exp[,c(6,7,10,11) ])
y <- as.matrix(creditcard_exp[,c(3) ])
library(glmnet)
# alpha = 0表示岭回归
r1 <- glmnet(x = x, y = y, family = "gaussian", alpha = 0)
plot(r1, xvar = "lambda",label=TRUE)
r1.cv <- cv.glmnet(x = x, y = y, family = "gaussian", alpha = 0, nfold = 10)
plot(r1.cv)
# 选择lambda的原则
#lambda.min:value of lambda that gives minimum cvm(交叉验证误差最小)
r1.cv$lambda.min
r1.min <- glmnet(x = x, y = y, family = "gaussian", alpha = 0, lambda = r1.cv$lambda.min)
coef(r1.min)
#lambda.1se:largest value of lambda such that error is within 1 standard error of the minimum.
#r1.cv$lambda.1se
#r1.1se <- glmnet(x = x, y = y, family = "gaussian", alpha = 0, lambda = r1.cv$lambda.1se)
#coef(r1.1se)


#6.4.2 LASSO算法
# alpha = 1表示lasso
r2 <- glmnet(x = x, y = y, family = "gaussian", alpha = 1)
plot(r2, xvar = "lambda")
#plot(r2)
r2.cv <- cv.glmnet(x = x, y = y, family = "gaussian", alpha = 1, nfold = 10)
plot(r2.cv)
# 选择lambda的原则
r2.cv$lambda.min
r2.min <- glmnet(x = x, y = y, family = "gaussian", alpha = 1, lambda = r2.cv$lambda.min)
coef(r2.min)

r2.cv$lambda.1se
r2.1se <- glmnet(x = x, y = y, family = "gaussian", alpha = 1, lambda = 50)
coef(r2.1se)






