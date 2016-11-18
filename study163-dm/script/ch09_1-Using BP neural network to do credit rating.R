#读取german数据
#该数据从german_data数据改写，将编码转换为数值
#german.data是经过脱敏的数据，因此从变量中完全看不出变量的意义，仅知道这些变量是连续变量分箱后的顺序值
setwd('D:\\R_edu\\data')
#导入数据和数据清洗
data<-read.csv("german.csv")
data[data$y==1,'y']=0
data[data$y==2,'y']=1

#随机抽样，建立训练集与测试集
set.seed(10)
select<-sample(1:nrow(data),700)
train=data[select,]
test=data[-select,]
#进行中心标准化
train[,1:24]=scale(train[,1:24])
test[,1:24]=scale(test[,1:24])

#------------------------使用nnet包实现BP神经网络--------------------------
library(nnet)
help(package="nnet")
model_nnet<-nnet(y~., linout = F,size = 8, decay = 0.01, maxit = 300,data = train) 
#对分类数据预测需要加上y参数 
pre.forest=predict(model_nnet, test) 
out=pre.forest
out[out<0.5]=0
out[out>=0.5]=1
#计算准确率
rate2<-sum(out==test$y)/length(test$y)

#做出概率预测
train$lg_nnet_p<-predict(model_nnet, train) 
test$lg_nnet_p<-predict(model_nnet, test) 

#绘制ROC曲线
library(ROCR)
pred_nnet_Tr <- prediction(train$lg_nnet_p, train$y)
perf_nnet_Tr <- performance(pred_nnet_Tr,"tpr","fpr")

pred_nnet_Te <- prediction(test$lg_nnet_p, test$y)
perf_nnet_Te <- performance(pred_nnet_Te,"tpr","fpr")

plot(perf_nnet_Tr,col='green',main="ROC of Models")
plot(perf_nnet_Te, col='black',lty=2,add=TRUE);
abline(0,1,lty=2,col='red')


lr_m_auc<-round(as.numeric(performance(pred_nnet_Tr,'auc')@y.values),3)
lr_m_str<-paste("Tran-AUC:",lr_m_auc,sep="")
legend(0.3,0.45,c(lr_m_str),2:8)

lr_m_auc<-round(as.numeric(performance(pred_nnet_Te,'auc')@y.values),3)
lr_m_ste<-paste("Test-AUC:",lr_m_auc,sep="")
legend(0.3,0.25,c(lr_m_ste),2:8)



#查看模型中两个重要参数的情况:只是为了得到取得优良效果的最小值
#要求输入的为数据框，全都为数字，不需要进行标准化
#最大迭代次数maxNum,公式formula,隐藏层大小sizeNum,输入的为数据框DataSet,训练集比例samplerate，神经网络类型ntype,
#1、对隐藏结点（size）的观察
Network<-function(maxNum,formula,sizeNum,DataSet,samplerate){
  library(nnet)
  library(ROCR)
  set.seed(10)
  select<-sample(1:nrow(data),ceiling(nrow(data)*samplerate))
  train=data[select,]
  test=data[-select,]
  #进行中心标准化
  train[,1:24]=scale(train[,1:24])
  test[,1:24]=scale(test[,1:24])
  
  ROC<-data.frame()
  for (i in seq(from =1,to =sizeNum+1,by =2)){
    model_nnet<-nnet(formula, linout = F,size = i, decay = 0.01, maxit = maxNum,trace = F,data = train) 
    train$lg_nnet_p<-predict(model_nnet, train) 
    test$lg_nnet_p<-predict(model_nnet, test) 
    pred_nnet_Tr <- prediction(train$lg_nnet_p, train$y)
    perf_nnet_Tr <- performance(pred_nnet_Tr,"tpr","fpr")
    pred_nnet_Te <- prediction(test$lg_nnet_p, test$y)
    perf_nnet_Te <- performance(pred_nnet_Te,"tpr","fpr")
    lr_m_auc_Tr<-round(as.numeric(performance(pred_nnet_Tr,'auc')@y.values),3)
    lr_m_auc_Te<-round(as.numeric(performance(pred_nnet_Te,'auc')@y.values),3)
    out<-data.frame(i,lr_m_auc_Tr,lr_m_auc_Te)
    ROC<-rbind(ROC,out)
  }  
  return(ROC)
}
 
data<-read.csv("german.csv")
data[data$y==1,'y']=0
data[data$y==2,'y']=1
Roc<-Network(maxNum=100,formula=y~.,sizeNum=20,DataSet=data,samplerate=0.7)
names(Roc)<-c("size","Index_Train","Index_Test")
plot(Roc$size,Roc$Index_Train,type="l",main="训练集的ROC INDEX")
plot(Roc$size,Roc$Index_Test,type="l",main="验证集的ROC INDEX")
#plot(Roc$maxit,Roc$Index_Test,type="l",col='red',lty=2)

#2、对迭代次数（maxit）的观察
Network<-function(maxNum,formula,sizeNum,DataSet,samplerate){
  library(nnet)
  library(ROCR)
  set.seed(10)
  select<-sample(1:nrow(data),ceiling(nrow(data)*samplerate))
  train=data[select,]
  test=data[-select,]
  #进行中心标准化
  train[,1:24]=scale(train[,1:24])
  test[,1:24]=scale(test[,1:24])
  
  model_nnet<-nnet(formula, linout = F,size = sizeNum, decay = 0.01, maxit = maxNum,trace = F,data = train) 
  train$lg_nnet_p<-predict(model_nnet, train) 
  test$lg_nnet_p<-predict(model_nnet, test) 
  pred_nnet_Tr <- prediction(train$lg_nnet_p, train$y)
  perf_nnet_Tr <- performance(pred_nnet_Tr,"tpr","fpr")
  
  pred_nnet_Te <- prediction(test$lg_nnet_p, test$y)
  perf_nnet_Te <- performance(pred_nnet_Te,"tpr","fpr")
  lr_m_auc_Tr<-round(as.numeric(performance(pred_nnet_Tr,'auc')@y.values),3)
  lr_m_auc_Te<-round(as.numeric(performance(pred_nnet_Te,'auc')@y.values),3)
  #print("ROC Index of Train:",lr_m_auc_Tr)
  #print("ROC Index of Test:",lr_m_auc_Te)
  ROC<-data.frame()
  for (i in seq(from =1,to =maxNum,by =2)){
    model_nnet<-nnet(formula, linout = F,size = sizeNum, decay = 0.01, maxit = i,trace = F,data = train) 
    train$lg_nnet_p<-predict(model_nnet, train) 
    test$lg_nnet_p<-predict(model_nnet, test) 
    pred_nnet_Tr <- prediction(train$lg_nnet_p, train$y)
    perf_nnet_Tr <- performance(pred_nnet_Tr,"tpr","fpr")
    pred_nnet_Te <- prediction(test$lg_nnet_p, test$y)
    perf_nnet_Te <- performance(pred_nnet_Te,"tpr","fpr")
    lr_m_auc_Tr<-round(as.numeric(performance(pred_nnet_Tr,'auc')@y.values),3)
    lr_m_auc_Te<-round(as.numeric(performance(pred_nnet_Te,'auc')@y.values),3)
    out<-data.frame(i,lr_m_auc_Tr,lr_m_auc_Te)
    ROC<-rbind(ROC,out)
  }  
  return(ROC)
}

data<-read.csv("german.csv")
data[data$y==1,'y']=0
data[data$y==2,'y']=1
Roc<-Network(maxNum=150,formula=y~.,sizeNum=5,DataSet=data,samplerate=0.7)
names(Roc)<-c("maxit","Index_Train","Index_Test")
plot(Roc$maxit,Roc$Index_Train,type="l",main="训练集的ROC INDEX")
plot(Roc$maxit,Roc$Index_Test,type="l",main="验证集的ROC INDEX")
