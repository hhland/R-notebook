###################################
###1.描述性统计
###################################
###数据集描述与属性说明###：
##数据说明：本数据是一份汽车贷款违约数据,因变量为是否违约(bad_ind)
##[T]因变量,[N]数值变量,[C]分类变量,[S]序数变量,[D]时间
##application_id	申请者ID
##account_number	帐户号
##[T]bad_ind	是否违约
##[D]vehicle_year	汽车购买时间
##[C]vehicle_make	汽车制造商
##[C]bankruptcy_ind	曾经破产标识
##[N]tot_derog	五年内信用不良事件数量(比如手机欠费消号)
##[N]tot_tr	全部帐户数量
##[N]age_oldest_tr	最久账号存续时间(月)
##[N]tot_open_tr	在使用帐户数量
##[N]tot_rev_tr	在使用可循环贷款帐户数量(比如信用卡)
##[N]tot_rev_debt	在使用可循环贷款帐户余额(比如信用卡欠款)
##[N]tot_rev_line	可循环贷款帐户限额(信用卡授权额度)
##[N]rev_util	可循环贷款帐户使用比例(余额/限额)
##[N]fico_score	FICO打分
##[N]purch_price	汽车购买金额(元)
##[N]msrp	建议售价
##[N]down_pyt	分期付款的首次交款
##[N]loan_term	贷款期限(月)
##[N]loan_amt	贷款金额
##[N]ltv	贷款金额/建议售价*100
##[N]tot_income	月均收入(元)
##[N]veh_mileage	行使历程(Mile)
##[C]used_ind	是否使用
##[N]weight	样本权重
##读取数据
setwd("D:/R_edu/data")
accepts<-read.csv("data/accepts.csv")
##数据预处理
sapply(accepts,class)#查看数据类型
summary(accepts)
accepts$bad_ind=as.factor(accepts$bad_ind)
#accepts$bankruptcy_ind=as.factor(accepts$bankruptcy_ind)
accepts$used_ind=as.factor(accepts$used_ind)

#################################################################
#3.1 描述性统计与探索型数据分析
##3.1.1.分类变量分析
table(accepts$bad_ind)
m<-matrix(table(accepts$bankruptcy_ind))
prop.table(m, 2)
#对于两分类变量，也可以使用prettyR包中的xtab函数
barplot(table(accepts$bankruptcy_ind))

##3.1.2.连续变量分析
##3.1.2.2.1.数据的集中趋势2
#FICO_SCORE的均值与中位数
fs=accepts$fico_score
mean(fs,na.rm=T)#求fs的均值
median(fs,na.rm=T)#求fs的中位数
quantile(fs,probs=c(0.25,0.5,0.75),na.rm=T)#求a的上下四分位数与中位数
hist(fs,nclass=20)#绘制fs的直方图
##3.1.2.2.数据的离散程度
fs=accepts$fico_score
mad=function(x){
  mean(abs(x-mean(x,na.rm=T)),na.rm=T)
  }#定义平均绝对偏差函数mad()
max(fs,na.rm=T)-min(fs,na.rm=T)#极差
mad(fs)#平均绝对偏差
var(fs,na.rm=T)#求方差
sd(fs,na.rm=T)#求标准差
IQR(fs,na.rm=T)
#正态分布的变量使用变异系数,标准化后的变异，适用于描述波动情况
CV=100*sd(fs,na.rm=T)/mean(fs,na.rm=T);CV
#非正态分布的变量以下两个指标,标准化后的变异，适用于描述波动情况
ind1<-100*mad(fs)/median(fs,na.rm=T);ind1
ind2<-100*IQR(fs,na.rm=T)/median(fs,na.rm=T);ind2

##3.1.2.3.数据的偏度与峰度
#fico_score:正态
library(GLDEX)
fs=accepts$fico_score
hist(fs,breaks=15)
fs_s=skewness(fs,na.rm=T);fs_s
fs_k=kurtosis(fs,na.rm=T);fs_k
#purch_price:右偏
p=accepts$purch_price
hist(p,nclass=20)
p_s=skewness(p,na.rm=T);p_s
p_k=kurtosis(p,na.rm=T);p_k
m=matrix(c(fs_s,fs_k,p_s,p_k),nrow=2)
colnames(m)=c('fs-正态','p-右偏')
rownames(m)=c('偏度','峰度')
print(m)

#3.1.3常见分布
a <- rnorm(1000)#随机正态分布
hist(a)#绘制随机正态分布直方图

b###################################
###3.2 APPLY函数族
###################################
##1.LAPPLY(对列表进行循环)
#列表循环求均值
x=list(a = 1:5, b = rnorm(10))#生成两组向量
lapply(x, mean)

#产生四组向量，求均值
x=list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, sum)
##2.SAPPLY
#SAPPLY与LAPPLY的对比
x=list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
l<-lapply(x, mean)
s<-sapply(x, mean)
print(l)
print(s)
mean(x)#报错
class(l)#LAPPLY返回列表
class(s)#SAPPLY返回数字(只有一个元素)
##3.APPLY
#对行与列进行汇总
x=matrix(rnorm(24), 4, 6)
x=as.matrix(x)
apply(x, 2, mean)#对矩阵中的列进行循环
apply(x, 1, sum)#对矩阵中的行进行循环
rowSums=apply(x, 1, sum)
rowMeans=apply(x, 1, mean)
colSums=apply(x, 2, sum)
colMeans=apply(x, 2, mean)
#对矩阵每一列求四分位数
x=matrix(rnorm(24), 4, 6)
x=as.data.frame(x)
apply(x,2,quantile,probs=c(0.25,0.5,0.75))

##4.TAPPLY(数据分组循环)
x=c(rnorm(10), runif(10), rnorm(10, 1))
f=gl(3, 10)
data=data.frame('x'=x,'group'=f)#分组
tapply(data$x, data$group, mean)
tapply(data$x, data$group, mean, simplify = FALSE)#不简化结果
tapply(data$x, data$group, range)

##按照违约指示变量进行分组汇总
vmean<-mean(accepts$fico_score,na.rm=TRUE)
accepts[is.na(accepts$fico_score),]$fico_score<-vmean
tapply(accepts$fico_score, accepts$bad_ind,summary)


##5.SPLIT(数据分组循环)
#split用于分组
x=c(rnorm(10), runif(10), rnorm(10, 1))
f=gl(3, 10)
data=data.frame('x'=x,'group'=f)#分组
class(split(data$x, data$group))#类型为列表
lapply(split(data$x, data$group),mean)
sapply(split(data$x, data$group),mean)
#split用于数据框
#使用违约数据集,对比违约与不违约用户
#在fico_score和purch_price的均值差异
accepts_s=accepts[,c('bad_ind','fico_score','purch_price')]#使用违约数据集
s=split(accepts_s,accepts$bad_ind)#按照是否违约进行分组
#构造两个循环，第一个对s进行循环，第二个对数据框的指定列进行循环
lapply(s,function(x) lapply(x[,2:3],
                            function(col) 
                              mean(col,na.rm=T)))
sapply(s,function(x) sapply(x[,2:3],function(col) mean(col,na.rm=T)))
#在fico_score和purch_price的分布的差异
par(mfrow=c(2,2))
lapply(s,function(x) lapply(x[,2:3],
                            function(col) 
                              hist(col,nclass=20,main="直方图")))

#在fico_score和purch_price的分布的差异

pdf(file="pair.pdf")
lapply(s,function(x) lapply(x[,2:3],
    function(col) 
    hist(col,nclass=20)))
dev.off()
    


###################################
###3.4 R基础绘图包
###################################
#3.4.1饼图
paint.sales=c(0.27, 0.1, 0.14, 0.24, 0.13, 0.07, 0.03)
pie(paint.sales, col = gray(seq(0.5,1.0,length=7)))
names(paint.sales)=c("Red", "Green", "Blue","White", "Brown", "Gray", "Purple")
pie(paint.sales, col = names(paint.sales)) 

#使用灰度
par(mfrow=c(1,1))
table(accepts$bankruptcy_ind)
pie(table(accepts$bankruptcy_ind), col = gray(seq(0.1,1.0,length=3)))
#自定义颜色
pie(table(accepts$bankruptcy_ind), col = c("Green", "blue", "black"))
#使用预定义的彩虹色
pie(table(accepts$bankruptcy_ind), col = rainbow(3))


#3.4.2直方图
x=accepts$fico_score
hist(x, freq = T,main="fico_score", 
     sub ="source:汽车贷款数据", xlab="fico_score打分", 
     ylab="频数",nclass=50)

#3.4.3箱线图
##FICO打分的分布
boxplot(accepts$fico_score)
##破产标识和FICO打分之间的关系
boxplot(accepts$fico_score ~ accepts$bankruptcy_ind)

#3.4.4散点图
#试图找出purch_price与loan_amt的关系，汽车价格vs贷款金额
x=accepts$purch_price
y=accepts$loan_amt
plot(x=x,y=y,type="p")#散点图
text(30000,80000,"汽车价格vs贷款金额")#加标签
plot(accepts$ltv,accepts$purch_price)#散点图

###################################
###3.5 ggplot2制图
###################################
library("ggplot2")
#分析购买汽车价格和贷款数量之间的关系
ggplot(data=accepts,aes(x=purch_price, y=loan_amt)) + 
  geom_point(size=2)#图基数据(指定轴)+类型(点)

qplot(accepts$purch_price,accepts$loan_amt)#散点图


