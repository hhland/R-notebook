library(sqldf)
#导入数据
setwd('D:\\R_edu\\data')

accounts <- read.csv("accounts.csv",stringsAsFactors =F)
card <- read.csv("card.csv",stringsAsFactors =F)
clients <- read.csv("clients.csv",stringsAsFactors =F)
disp <- read.csv("disp.csv",stringsAsFactors =F)
district<- read.csv("district.csv",stringsAsFactors =F)
loans <- read.csv("loans.csv",stringsAsFactors =F)
order <- read.csv("order.csv",stringsAsFactors =F)
trans <- read.csv("trans.csv",stringsAsFactors =F)

#*********************************************************************************************/
#使用“loans”数据，根据“status”变量生成违约标识变量（bad_good），
#其中当等于status=A时，#取值为bad_good=0，
#其中当等于status in (B,D)时，取值为bad_good=1，
#等于status=C时，bad_good为缺失值。*/
summary(loans)
loans$bad_good<-ifelse(loans$status=="B" | loans$status=="D",1,
                       ifelse(loans$status=="A",0,NA))

#是否违约与借款人的年龄是否有关系？
    #根据“birth_date”变量和“date”变量每个客户贷款时的年龄（age）变量
data<-sqldf("select  a.*,c.sex,c.birth_date,c.district_id,
             c.client_id  from  loans as a
       left join disp as b on a.account_id=b.account_id
       left join clients as c on b.client_id=c.client_id
       where b.type='所有者'")
data$age<-floor((as.Date(data$date)-as.Date(data$birth_date))/365)
    #年龄和是否违约关系很弱
tapply(data$age, data$bad_good,mean)
data$bad_good<-as.factor(data$bad_good)
data$age<-as.numeric(data$age)
boxplot(data$age ~ data$bad_good)


#是否违约与借款人的资产否有关系？
  #账户余额在“trans”表中，因此要将“loan”和“trans”表连接
data2<-sqldf("select  a.*,b.balance,b.date as t_date  
              from  loans as a
              left join trans as b on a.account_id=b.account_id
              order by a.account_id,b.date")

  #只需要贷款前一年内的账户余额
data2$date2<-as.Date(data2$date)
data2$t_date2<-as.Date(data2$t_date)
data2$balance2<-as.numeric(substr(gsub(",","",data2$balance),
                                  2,nchar(data2$balance)))

data3<-data2[data2$date2>data2$t_date2 
              & data2$date2<=data2$t_date2+365,]
   #sd<-data.frame(tapply(data3$balance2, data3$account_id,sd))
data4<-sqldf("select  a.account_id,a.status,a.amount,
              avg(balance2) as avg_balance,
              stdev(balance2) as stdev_balance2
             from  data3 as a
             group by a.account_id
             order by a.account_id")
data4$bad_good<-ifelse(data4$status=="B" 
                       | data4$status=="D",1,
                       ifelse(data4$status=="A",0,NA))
data4$bad_good<-as.factor(data4$bad_good)


  #资产低的客人违约的可能性较高
tapply(data4$avg_balance, data4$bad_good,mean)
boxplot(data4$avg_balance ~ data4$bad_good)
  #贷款数额高的客人违约的可能性较高
tapply(data4$amount, data4$bad_good,mean)
boxplot(data4$amount ~ data4$bad_good)
  #贷款额度超出资产越高的客人违约的可能性较高
data4$rate<-data4$amount/data4$avg_balance
tapply(data4$rate, data4$bad_good,mean)
boxplot(data4$rate ~ data4$bad_good)

  #资产波动越高的客人违约的可能性较高,不过不明显
tapply(data4$std, data4$bad_good,mean)
boxplot(data4$std ~ data4$bad_good)

  #资产波动越高的客人违约的可能性较高,不过不明显
data4$cv<-data4$std/data4$avg_balance
tapply(data4$cv, data4$bad_good,mean)
boxplot(data4$cv ~ data4$bad_good)

