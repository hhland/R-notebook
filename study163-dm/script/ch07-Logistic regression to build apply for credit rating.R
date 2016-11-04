#第7讲 逻辑回归
##数据说明：本数据是一份汽车贷款违约数据
##名称	中文含义
##application_id	申请者ID
##account_number	帐户号
##bad_ind	是否违约
##vehicle_year	汽车购买时间
##vehicle_make	汽车制造商
##bankruptcy_ind	曾经破产标识
##tot_derog	五年内信用不良事件数量(比如手机欠费消号)
##tot_tr	全部帐户数量
##age_oldest_tr	最久账号存续时间(月)
##tot_open_tr	在使用帐户数量
##tot_rev_tr	在使用可循环贷款帐户数量(比如信用卡)
##tot_rev_debt	在使用可循环贷款帐户余额(比如信用卡欠款)
##tot_rev_line	可循环贷款帐户限额(信用卡授权额度)
##rev_util	可循环贷款帐户使用比例(余额/限额)
##fico_score	FICO打分
##purch_price	汽车购买金额(元)
##msrp	建议售价
##down_pyt	分期付款的首次交款
##loan_term	贷款期限(月)
##loan_amt	贷款金额
##ltv	贷款金额/建议售价*100
##tot_income	月均收入(元)
##veh_mileage	行使历程(Mile)
##used_ind	是否二手车
##weight	样本权重
#—————————————————————————————————————————————————————————————————————————————————————————————————————————————###
setwd('D:\\R_edu\\data')
#导入数据和数据清洗
accepts<-read.csv("accepts.csv")
accepts<-na.omit(accepts)
attach(accepts)
#7.1 分类变量的相关关系
#曾经破产标识与是否违约是否有关系?
table(bad_ind)
table(bankruptcy_ind)
table(bankruptcy_ind,bad_ind)
#对于两分类变量的列联表分析，使用prettyR包中的xtab函数
library(prettyR)
xtab(~ bankruptcy_ind + bad_ind, data=accepts)

chisq.test(x=accepts$bankruptcy_ind,y=accepts$bad_ind)
xtab(~ bankruptcy_ind + bad_ind, data=accepts, chisq = TRUE)

#7.2 逻辑回归
#随机抽样，建立训练集与测试集
set.seed(100)
select<-sample(1:nrow(accepts),length(accepts$application_id)*0.7)
train=accepts[select,]
test=accepts[-select,]
attach(train)
plot(fico_score,bad_ind)

lg<-glm(bad_ind ~fico_score,family=binomial(link='logit'))
summary(lg)

lg<-glm(bad_ind ~fico_score+bankruptcy_ind+tot_derog+age_oldest_tr+rev_util+ltv+veh_mileage,family=binomial(link='logit'))
summary(lg)
lg_ms<-step(lg,direction = "both")
summary(lg_ms)
train$lg_p<-predict(lg_ms, train) 
summary(train$lg_p)
train$p<-1/(1+exp(-1*train$lg_p))
summary(train$p)

library(car)
vif(lg)
#7.3 模型评估
#对分类数据预测需要加上bad_ind参数 
test$lg_p<-predict(lg_ms, test) 
test$p<-(1/(1+exp(-1*test$lg_p)))
summary(test$p)
test$out<-1
test[test$p<0.2,]$out<-0
table(test$bad_ind,test$out)

#7.3.1计算准确率
rate2<-sum(test$out==test$bad_ind)/length(test$bad_ind)

#7.3.2绘制ROC曲线
#旧方法
library(ROCR)
pred_Te <- prediction(test$p, test$bad_ind)
perf_Te <- performance(pred_Te,"tpr","fpr")
pred_Tr <- prediction(train$p, train$bad_ind)
perf_Tr <- performance(pred_Tr,"tpr","fpr")
plot(perf_Te, col='blue',lty=1);
plot(perf_Tr, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')

lr_m_auc<-round(as.numeric(performance(pred_Tr,'auc')@y.values),3)
lr_m_str<-paste("Mode_Train-AUC:",lr_m_auc,sep="")
legend(0.3,0.4,c(lr_m_str),2:8)

lr_m_auc<-round(as.numeric(performance(pred_Te,'auc')@y.values),3)
lr_m_ste<-paste("Mode_Test-AUC:",lr_m_auc,sep="")
legend(0.3,0.2,c(lr_m_ste),2:8)

#新方法
#library(pROC)
#plot.roc(bad_ind~p,train,col="1")->r1
#rocobjtr<- roc(train$bad_ind, train$p)
#auc(rocobjtr)
#lines.roc(bad_ind~p,test,col='2')->r2
#rocobjte <- roc(test$bad_ind, test$p)
#auc(rocobjte)
#roc.test(r1,r2)

#7.3.3绘制洛伦兹曲线
pred_Tr <- prediction(train$p, train$bad_ind)
tpr <- performance(pred_Tr,measure='tpr')@y.values[[1]]
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,tpr,type='l',main='Lorenz图',ylab='查全率(tpr)',xlab='深度(depth)')


#7.3.4绘制累积提升度曲线
library(ROCR)
pred_Tr <- prediction(train$p, train$bad_ind)
lift <- performance(pred_Tr,measure='lift')@y.values[[1]]
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,lift,type='l',main='lift图',ylab='lift',xlab='depth')

#7.3.5绘制K-S曲线
pred_Tr <- prediction(train$p, train$bad_ind)
tpr <- performance(pred_Tr,measure='tpr')@y.values[[1]]
fpr <- performance(pred_Tr,measure='fpr')@y.values[[1]]
ks<-(tpr-fpr)
depth <- performance(pred_Tr,measure='rpp')@y.values[[1]]
plot(depth,ks,type='l',main='K-S曲线',ylab='KS值',xlab='深度(depth)')
kslable<-paste("KS统计量:",max(ks),sep="")
legend(0.3,0.2,c(kslable),2:8)

















