#第5讲 统计推断基础

##数据说明：本数据是
##名称	中文含义
#dis_name   小区名称
#rate       房价同比增长率
setwd('D:\\R_edu\\data')
house_price_gr<-read.csv("house_price_gr.csv")
attach(house_price_gr)
#5.1 参数估计
#进行描述性统计分析
summary(rate)

#Histograph
hist(rate,prob=T,main="Distrabution of fico_score")
lines(density(rate))
##Q-Q
qqnorm(rate);qqline(rate)

#Box Plots
boxplot(rate)

#如果你要求的只是95%的置信度的话，t.test(),这个函数本来是用于做单样本T检验的
t.test(rate)
mean(rate)
se<-sd(rate)/sqrt(length(rate))
LB<-mean(rate)-1.98*se
UB<-mean(rate)+1.98*se
c(LB,UB)
#如果要求任意置信度下的置信区间的话，就需要自己编一个函数了。
confint<-function(x,sigma=-1,alpha=0.05)
{
  n<-length(x)
  xb<-mean(x)
  tmp<-(sd(x)/sqrt(n))*qt(1-alpha/2,n-1);df<- n-1
  data.frame(mean=xb,df=df,LB=xb-tmp,UB=xb+tmp)
}
#这个函数的使用：
confint(rate,0.05)  

#5.2 假设检验与单样本T检验
#一般认为FICO高于690的客户信誉较高，请检验该产品的客户整体信用是否高于690
t.test(rate,mu=0.1)




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

#导入数据和数据清洗
creditcard_exp<-read.csv("creditcard_exp.csv")
creditcard_exp<-na.omit(creditcard_exp)


#5.3 两样本T检验
#tapply
#根据性别比较支出.
attach(creditcard_exp)
tapply(avg_exp, gender, summary)


#第一步:
var.test(avg_exp~gender)
#第二步:
t.test(avg_exp~gender,var.equal=T)

#5.4 方差分析
#单因素方差分析
creditcard_exp$edu_class<-as.factor(creditcard_exp$edu_class)
creditcard_exp$gender<-as.factor(creditcard_exp$gender)
attach(creditcard_exp)#在原始数据更新后，需要重新执行该语句
#attach(creditcard_exp)
tapply(avg_exp, edu_class, summary)
#oneway.test(avg_exp~edu_class,var.equal=F)
anova(lm(creditcard_exp$avg_exp~creditcard_exp$edu_class))

#多因素方差分析
anova(lm(avg_exp~edu_class+gender))

ana<-lm(avg_exp~edu_class+gender)
summary(ana)

anova(lm(avg_exp~edu_class+gender+edu_class*gender))
ana<-lm(avg_exp~edu_class+gender+edu_class*gender)
summary(ana)


#5.5 相关分析
#散点图
plot(Income,avg_exp)

#相关性分析:“spearman”,“pearson” 和 "kendall",
cor.test(Income,avg_exp,method="pearson")
cor.test(Income,avg_exp,method="spearman")


#5.6卡方检验
accepts<-read.csv("accepts.csv")
#accepts<-na.omit(accepts)

table(accepts$bankruptcy_ind)
m<-matrix(table(accepts$bankruptcy_ind))
prop.table(m, 2)

#barplot(table(accepts$bankruptcy_ind))
table(accepts$bankruptcy_ind,accepts$bad_ind)

#对于两分类变量的列联表分析，使用prettyR包中的xtab函数
library(prettyR)
xtab(~ bankruptcy_ind + bad_ind, data=accepts)

chisq.test(x=accepts$bankruptcy_ind,y=accepts$bad_ind)
xtab(~ bankruptcy_ind + bad_ind, data=accepts, chisq = TRUE)

