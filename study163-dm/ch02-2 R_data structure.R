#data structure
a<-c(1,2,3)
a
i<-c(1:100)
i
i[1]
matrix1<-matrix(c(1,2,3,4,5,6),nrow=2,ncol=3)
matrix1
matrix2<-matrix(c(1,2,3,4,5,6),nrow=2,byrow=TRUE)
matrix2
matrix3<-matrix(c("a","b","c","d"),nrow=2)
matrix3
matrix4 <- matrix(nrow=2,ncol =2)
matrix4
matrix4[1,1] <- 3
matrix4[2,1] <- 5
matrix4[1,2] <- 10
matrix4[2,2] <- 14
matrix4
matrix5<-matrix(1:15,nrow=3)
matrix5

matrix5[2,]
matrix5[,4]
matrix5[2,5]
matrix5[c(1,2),]
matrix5[,c(3:5)]

array1<-array(1:18,c(3,3,2))
array1

dim1<-c("A1","A2","A3")
dim2<-c("B1","B2","B3")
dim3<-c("C1","C2")

array3<-array(1:18,c(3,3,2),dimnames=list(dim1,dim2,dim3))
array3

a2<-array(1:6,6)
a2

v<-c(1:6)
v

is.array(a2)
is.vector(a2)
is.vector(v)
is.array(v)

array1<-array(1:18,c(3,3,2))
array1

array1[2,,]
array1[,1:2,]
array1[,,2]
array1[3,3,2]

performance<-c("bad","good","good","bad", "excellent","bad")
class(performance)

f1<-factor(performance)
f1
class(f1)

levels(f1)
mode(performance)
mode(f1)

f2<- factor(performance,labels="a")
f2

f3<-factor(performance,levels=c("bad","good","excellent"),ordered=TRUE)
f3

f4<-factor(performance,exclude="bad")
f4

performance<-c("bad","good","good","bad","excellent","bad")
f1<-factor(performance)
f1
f1[2]
f1[6]

name<-c("Jane","Bob","Elena","Lily","Max")
English<-c(84,86,78,90,88)
Math<-c(80,85,90,87,85)
Art<-c(78,80,80,85,86)
Score<-data.frame(name,English,Math,Art)
Score

v2<-c(3,4)
v3<-c("a","b")
df1<-data.frame(v2,v3)
df1

class(df1$v3)
m3<-matrix(1:6,nrow=2)
m4<-matrix(5:10,nrow=2)
m5<-c(T,F)
v4<-c(TRUE,FALSE)
dm2<-data.frame(m3,m4,v3)
dm2
str(dm2)
dm2[1,]
dm2[,4]
Score[[1]]
Score$Math

v5<-c(2:8)
v5
v6<-c("aa","bb","cc")
v6

m6<-matrix(c(1:9),nrow=3)
m6

f2<-factor(c("high","low","low","high"))
f2

mylist<-list(v5,v6,m6,f2)
mylist

list2<-list(num11=v5,cha=v6,matrix=m6,factor=f2)
list2

list2[[1]]
list2[[3]]
list2[["matrix"]]
list2$num11
