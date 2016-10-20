#*********************************************************************************************/
##读取数据
setwd('D:\\R_edu\\data')
accepts<-read.csv("accepts.csv")
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

#*********************************************************************************************/
#4.6 脏数据或数据不正确
#发现脏数据或数据不正确
#要区分开错误数据和异常值：错误数据应该替换为缺失值，而异常数据应该使用盖帽法或分箱处理
summary(accepts$purch_price)
hist(accepts$purch_price)
quantile(accepts$purch_price, probs = c(0,0.01,0.25,0.75,0.99,1))
accepts[accepts$purch_price<100,]$purch_price<-NA

#4.7 数据重复
accepts$dup<-duplicated(accepts) #生成重复标识变量
accepts_dup<-accepts[accepts$dup==TRUE,] #把有重复的数据保存出来，以备核查
accepts_nodup1<-accepts[accepts$dup==FALSE,] #以下三种方式效果一样
accepts_nodup2<-accepts[!duplicated(accepts),] 
accepts_nodup3<-unique(accepts) 

accepts$dup1<-duplicated(accepts$account_number) #按照主键进行重复记录标识
accepts$dup1<-duplicated(accepts$fico_score) #这只是演示，没有实际意义

#4.8缺失值处理
summary(accepts)
##tot_derog	2、缺失代表无记录,使用均值或中位数填补
vmean<-mean(accepts$tot_derog,na.rm=TRUE)
accepts$tot_derog_empflag<-is.na(accepts$tot_derog)
accepts[is.na(accepts$tot_derog),]$tot_derog<-vmean

#其他有缺失变量请自行填补，找到一个有缺失的分类变量，使用众数进行填补

#多重插补：mice包中的mice函数，用于多重插补
library(mice)
#library(mitools)#该包用于多重插补方法的数据分析
#names(accepts)
#多重插补的处理有两个要点：1、被解释变量有缺失值的观测不能填补，只能删除；2、只对放入模型的解释变量进行插补。
y<-accepts[,c(1,2,3,25)]
x<-accepts[,c(-1,-2,-3,-25)]
imp<-mice(x,met="cart",m=1)#该方法只对数值变量进行插补，分类变量的缺失值保留
x_imp<-complete(imp)
result<-cbind(y,x_imp)
summary(result)
#summary(accepts)

#4.9噪声值处理
#4.9.1盖帽法
#整行去掉数据框里99%以上和1%以下的点
#result为进行多重插补后的数据，这只是为了保证数据没有缺失值。
summary(result$tot_derog)#盖帽法之前，查看数据情况
hist(result$tot_derog)
q1<-quantile(result$tot_derog, 0.01)
q99<-quantile(result$tot_derog, 0.99)
result[result$tot_derog<q1,]$tot_derog<-q1
result[result$tot_derog>q99,]$tot_derog<-q99
summary(result$tot_derog)#盖帽法之后，查看数据情况

#k-means聚类法、分箱（等深，等宽）、盖帽法

#4.9.2分箱法
#分箱法——等宽分箱
ewtd<-cut(result$age_oldest_tr,4)#这里以age_oldest_tr字段等宽分为4段
table(ewtd)
levels(ewtd)<-paste("L",0:3,sep="")#将连续变量转化成定序变量
result$age_oldest_tr_1<-ewtd
table(result$age_oldest_tr_1)
#accepts$age_oldest_tr_2<-tapply(orgDate1$age_oldest_tr,ewtd,mean)[ewtd]#使用箱均值进行光滑处理

#分箱法——等深分箱
parts<-4
xiaoz<-0.00001#极小值
eRat<-quantile(result$age_oldest_tr,probs = seq(0,1,1/parts))#这里以age_oldest_tr字段等比分为4段
table(eRat)
veRat<-mapply(function(x){
  for (i in 1:(parts-1)) 
  {
    if(x>=(eRat[i]-xiaoz)&x<eRat[i+1])
    {
      return(i)
    }
  }
  if(x+xiaoz>eRat[parts])
  {
    return(parts)
  }
  return(-1)
},result$age_oldest_tr)
table(veRat)
result$age_oldest_tr_3<-paste("L",veRat,sep="")#将连续变量转化成定序变量
table(result$age_oldest_tr_3)
#result$age_oldest_tr_4<-tapply(result$age_oldest_tr,result$age_oldest_tr_3,mean)[result$age_oldest_tr_3]#使用箱均值进行光滑处理
head(result)









