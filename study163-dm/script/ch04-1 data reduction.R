#*********************************************************************************************/
library(sqldf)
#4.1 FRM提取行为变量
setwd('D:\\R_edu\\data')
rfm_trad_flow<-read.csv("data/rfm_trad_flow.csv")
rfm<-sqldf("select  cust_id,type,
           max(time) as Recency,count(*) as freq,sum(amount) as Monetary
           from  rfm_trad_flow
           where type='Special_offer' or type='Normal'
           group by cust_id,type")

#4.2 数据重组
#4.2.1 拆分列
rfm<-sqldf("select  cust_id,type,sum(amount) as Monetary
           from  rfm_trad_flow
           where type='Special_offer' or type='Normal'
           group by cust_id,type")
library(reshape)
#https://www.r-statistics.com/tag/transpose/
rfm_w<- cast(rfm,cust_id~type)
rfm_w[is.na(rfm_w$Special_offer),]$Special_offer<-0 #进行缺失值替换
rfm_w$Special_offer_ratio<-rfm_w$Special_offer/(rfm_w$Special_offer+rfm_w$Normal)

#4.2.2 堆叠列
rfm_l<-melt(rfm_w,id="cust_id")

#4.3 抽样
clients<-read.csv("clients.csv")
#4.3.1简单随机抽样:随机选取100个客户用于调研
set.seed(100)
select<-sample(1:nrow(clients),100)
sample_clinet<-clients[select,]

#简单随机抽样:随机选取10%个客户用于调研
set.seed(100)
select<-sample(1:nrow(clients),length(clients$client_id)*0.1)
sample_clinet<-clients[select,]
other_clinet<-clients[-select,]

#4.3.2分层抽样（STR）
library(sampling)
table(clients$district_id)
sample_clinet_stra=strata(clients,stratanames="district_id",
                          size=rep(5,times=77) ,method="srswor")
