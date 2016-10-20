accepts<- read.csv("data/accepts.csv")
library(sampling)

t_bad_ind<-table(accepts$bad_ind)

n_bad0<-as.integer(t_bad_ind["0"])
n_bad1<-as.integer(t_bad_ind["1"])

nc_bad_ind<-c(n_bad1,n_bad0)

accepts_target<- strata(accepts,stratanames = "bad_ind",size=as.integer(nc_bad_ind*0.3),method = "srswor") 
accepts_train<- strata(accepts,stratanames = "bad_ind",size=as.integer(nc_bad_ind*0.7),method = "srswor" )

#将accepts数据集中的loan_term作为分类变量处理，进行分类变量概化处理，并且进行WOE转换

library(reshape)

loan_term<-data.frame(table(accepts$loan_term))
names(loan_term)<-c("loan_term","freq")

accepts_1<-merge(accepts,loan_term,by = "loan_term")
accepts_1$loan_term<-as.character(accepts_1$loan_term)
accepts_1[accepts_1$freq<10,]$loan_term<-"other"

table(accepts_1$loan_term,accepts_1$freq)
freq<-data.frame(table(accepts_1$loan_term,accepts_1$bad_ind))
freq$Var2<-paste("group",freq$Var2,sep="")


freq_w<- cast(freq,Var1~Var2)
freq_sum<-sqldf("select sum(group0) as group0_sum,
                sum(group1)  as group1_sum
                from freq_w as a")
freq_p<-sqldf("select a.*,a.group0/(b.group0_sum+0.0) as group0_p,
              a.group1/(b.group1_sum+0.0) as group1_p
              from freq_w as a, freq_sum as b")
freq_p$perc<-freq_p$group1/(freq_p$group0+freq_p$group1)
freq_p$woe<-log(freq_p$group1_p/freq_p$group0_p)
accepts_2<-sqldf("select a.*,b.woe as loan_term_woe
                from accepts_1 as a
                left join freq_p as b on a.loan_term=b.Var1")


#
使用“profile_bank”进行主成分分析，变量的含义如下：
ID,客户ID
CNT_TBM,柜台办理业务的次数
CNT_ATM,ATM办理业务的次数
CNT_POS,POS办理业务的次数
CNT_CSC,有偿服务的次数
CNT_TOT，总的业务办理次数
使用第2至4个变量进行主成分分析，并给出以下答案：
1、保留几个主成分合适；
2、进行主成分旋转之后，每个主成分分别代表原始的哪些变量？

profile_bank <- read.csv("data/profile_bank.csv")
orgdata <- profile_bank[,2:4]
pri1 <- princomp(orgdata,cor = T)
summary(pri1,loadings=T)

screeplot(pri1,type="lines")
p<-predict(pri1)[,1:2]

library(psych)
fc1<-principal(orgdata,nfactors=3,rotate="varimax",covar=F,score=TRUE) 
fc1

比较主成分分析和变量聚类的异同点。
使用“accepts”中的连续变量进行主成分分析，比较聚类后的结果是否和变量聚类的结果是否一致。

library(ClustOfVar)
tree<- hclustvar(orgdata)
plot(tree)
