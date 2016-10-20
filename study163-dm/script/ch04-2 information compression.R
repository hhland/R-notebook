#*********************************************************************************************/
#4.2 分类变量的压缩
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
setwd('D:\\R_edu\\data')
accepts <- read.csv("accepts.csv")
#*********************************************************************************************/
#4.4 分类变量的压缩
#*********************************************************************************************/
#4.4.1 分类变量重编码（概化）
#查看数据，确认不存在似不完整数据问题
table(accepts$vehicle_make,accepts$bad_ind)

library(sqldf)
freq<-data.frame(table(accepts$vehicle_make))
names(freq)<-c("vehicle_make","freq")
accepts1<-sqldf("select a.*,b.freq
                from accepts as a
                left join freq as b on a.vehicle_make=b.vehicle_make")
accepts1$vehicle_make<-as.character(accepts1$vehicle_make)
accepts1[accepts1$freq<10,]$vehicle_make<-"other"

#查看数据，确认不存在似不完整数据问题
table(accepts1$vehicle_make,accepts1$bad_ind)
#4.4.2 基于目标变量的转换-WOE
freq<-data.frame(table(accepts1$vehicle_make,accepts$bad_ind))
freq$Var2<-paste("group",freq$Var2,sep="")
library(reshape)
freq_w<- cast(freq,Var1~Var2)
freq_sum<-sqldf("select sum(group0) as group0_sum,
                sum(group1)  as group1_sum
                from freq_w as a")
freq_p<-sqldf("select a.*,a.group0/(b.group0_sum+0.0) as group0_p,
              a.group1/(b.group1_sum+0.0) as group1_p
              from freq_w as a, freq_sum as b")
freq_p$perc<-freq_p$group1/(freq_p$group0+freq_p$group1)
freq_p$woe<-log(freq_p$group1_p/freq_p$group0_p)
accepts2<-sqldf("select a.*,b.woe as vehicle_make_woe
                from accepts1 as a
                left join freq_p as b on a.vehicle_make=b.Var1")

#*********************************************************************************************/
#4.5 连续变量的压缩
#*********************************************************************************************/
setwd("D:\\R_edu\\data")
#加载数据集，使用barroData数据集
orgData<-read.csv("profile_telecom.csv")
names(orgData)
orgData<-orgData[,2:5]
#数据描述：
#该数据记录电信客户业务使用行为信息；
#属性说明：
#ID:客户编码
#cnt_call:打电话次数
#cnt_msg:发短信次数
#cnt_wei:发微信次数
#cnt_web:浏览网站次数
###################################################################
#4.5.1 主成分分析
#princomp()
#cor是逻辑变量 
#当cor=TRUE表示用样本的相关矩阵R做主成分分析
#当cor=FALSE表示用样本的协方差阵S做主成分分析
##方法1
pr1<-princomp(orgData,cor=TRUE)
pr1
#loading是逻辑变量 
#当loading=TRUE时表示显示loading 的内容
#loadings的输出结果为载荷 是主成分对应于原始变量的系数即Q矩阵
summary(pr1,loadings=TRUE)

#分析结果含义
#----Standard deviation 标准差   其平方为方差=特征值
#----Proportion of Variance  方差贡献率
#----Cumulative Proportion  方差累计贡献率

#画主成分的碎石图
screeplot(pr1,type="lines")

#由结果显示 前5个主成分的累计贡献率已经达到82%，碎石图在第5个点后趋于平稳 可以舍去另外9个主成分 达到降维的目的

#得到主成分的值
p<-predict(pr1)[,1:2]

##方法2
##http://blog.csdn.net/lilanfeng1991/article/details/36190841#
library(psych)
pr2<-principal(orgData,nfactors=3,rotate="none",covar=F,score=TRUE) 
pr2
head(pr2$scores) 
##方法3：使用主成分法，进行因子旋转
library(psych)
fc1<-principal(orgData,nfactors=3,rotate="varimax",covar=F,score=TRUE) 
fc1
head(fc1$scores)
orgData<-read.csv("profile_telecom.csv")
anadata<-cbind(orgData,fc1$scores)
names(anadata)<-c("ID","cnt_call","cnt_msg","cnt_wei","cnt_web","wei_web","msg","call")


#4.5.2 变量聚类
setwd("D:\\R_edu\\data")
#加载数据集
orgData<-read.csv("profile_telecom.csv")
names(orgData)
orgData<-orgData[,2:5]

library(ClustOfVar)
tree <- hclustvar(orgData)
plot(tree)
#choice of the number of clusters
stability(tree,B=40)
part <- cutreevar(tree,3,matsim = T)
#print(part)
summary(part)
part$var
part$sim
#手工计算1 – R2 Ratio 
test<-cbind(orgData[,3:4],part$scores)
cor(test)
cnt_wei_1_R2<-(1-0.98*0.98)/(1-0.51*0.51)
cnt_web_1_R2<-(1-0.98*0.98)/(1-0.739*0.739)

#对accepts数据集进行变量聚类
library(ClustOfVar)
setwd("D:\\R_edu\\data")
accepts <- read.csv("accepts.csv")
orgData<-accepts[,c(-1,-2,-3,-5,-6)]
tree <- hclustvar(orgData)
plot(tree)
#choice of the number of clusters
stability(tree,B=30)
part <- cutreevar(tree,7,matsim = T)
print(part)
summary(part)
part$sim

part <- cutreevar(tree,16,matsim = T)
print(part)
summary(part)
part$sim










