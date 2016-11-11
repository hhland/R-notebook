#加载数据集
setwd("D:\\R_edu\\data")
orgData<-read.csv("Allelectronics.csv")
#其中age的1代表“<=30”，2代表“31…40”，3代表“>40”，并且为因子变量
#进行该编码的目的是C50不能处理"..."和"<="之类的特殊字符
orgData$age<-as.factor(orgData$age)

summary(orgData)#经检查，没有缺失值问题
names(orgData)
###################################
## Section 1: C50算法
###################################
#1.建立C50模型
##建模
library(C50)
#调整树大小的主要变量
#事前剪枝设置
#minCases结点如果要被分割，要求的最小样本量，值越大，模型就越小
#winnow事先是否筛选变量，设置为T的话，模型一般比较小
#事后剪枝设置
##CF置信因子越小，要求模型的置信度越高，模型就越小
  #affects the way that error rates are estimated and hence the severity of pruning; 
  #values smaller than the default (25%) cause more of the initial tree to be pruned, 
  #while larger values result in less pruning.
#noGlobalPruning是否进行第二轮基于全局性的事后剪枝操作，设置为F的话，模型一般比较小
  #disables this second pruning component and generally results in larger 
  #decision tees and rulesets. 
  #Turning off global pruning can be beneficial for some applications, 
  #particularly when rulesets are generated.
tc<-C5.0Control(minCases = 2,winnow=F,CF=0.99,noGlobalPruning=T)

model <- C5.0(buys_computer ~age+income+student+credit_rating,data=orgData,
              trials=1,rules=F,control =tc )
#图形展示
plot(model)

#生成规则
rule <- C5.0(buys_computer ~age+income+student+credit_rating,data=orgData,
              trials=1,rules=T)
summary( rule )
###################################
## Section 2: CART算法(分类树)
###################################
#1.建立CART模型
##建模
#CART在R中的实现
#rpart包中有针对CART决策树算法提供的函数，比如rpart函数
#以及用于剪枝的prune函数
#rpart函数的基本形式：rpart(formula,data,subset,na.action=na.rpart,method.parms,control,...)
library(rpart)
#1.建立CART模型
#1.1 设置前向剪枝的条件
tc <- rpart.control(minsplit=1,minbucket=1,maxdepth=10,xval=5,cp=0.005)
## rpart.control对树进行一些设置  
## minsplit是最小分支节点数，这里指大于等于5，那么该节点会继续分划下去，否则停止  
## minbucket：树中叶节点包含的最小样本数  
## maxdepth：决策树最大深度 
## xval:交叉验证的次数
## cp全称为complexity parameter，指某个点的复杂度，对每一步拆分,模型的拟合优度必须提高的程度  
#1.2 建模
rpart.mod=rpart(buys_computer ~age+income+student+credit_rating,data=orgData, 
                parms = list(split = "gini"),method="class",control=tc)
summary(rpart.mod)
#1.3图形展示
library(rpart.plot)
rpart.plot(rpart.mod,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8)

#2.cart剪枝方法
#cp是每次分割对应的复杂度系数
rpart.mod$cp
plotcp(rpart.mod)
#prune函数可以实现最小代价复杂度剪枝法，对于CART的结果，每个节点均输出一个对应的cp
#prune函数通过设置cp参数来对决策树进行修剪,cp为复杂度系数
## 我们可以用下面的办法选择具有最小xerror的cp的办法：  
rpart.mod.pru<-prune(rpart.mod, cp= rpart.mod$cptable[which.min(rpart.mod$cptable[,"xerror"]),"CP"]) 
rpart.mod.pru$cp
library(rpart.plot)
rpart.plot(rpart.mod.pru,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="决策树")
