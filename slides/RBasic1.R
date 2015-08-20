library(MASS)
#介紹引入套件
data(Boston)
Boston=subset(Boston,,-c(zn,chas,black,rad,ptratio,age,nox,tax))
#刪減欄位
head(Boston)
summary(Boston)
library(knitr)
kable(summary(Boston[c("crim", "indus")]))

kable(summary(Boston[,c("rm", "dis")]))

kable(summary(Boston[,c("lstat", "medv")]))
summary(Boston$medv)
#解釋1非常正相關 -1是非常負相關
cor(Boston$medv,Boston)
barplot(cor(Boston$medv,Boston))

plot(Boston$rm,Boston$medv)
#x軸為房間數 y軸為房價
LM=lm(Boston$medv~Boston$rm)
#找出線性關係
abline(LM,col=2)
#畫出圖
## plot(Boston$rm,Boston$medv)
## #x軸為房間數 y軸為房價
## LM=lm(Boston$medv~Boston$rm)
## #找出線性關係
## abline(LM,col=2)
## #畫出圖
quantile(Boston$medv, probs = c(0.95, 0.9))
hist(Boston$medv,breaks=20)
hist(Boston$medv, breaks = 20)
abline(v = 43.4, col="red")
MEDVhight = subset(Boston,Boston$medv>=43.4)
MEDVOther = subset(Boston,Boston$medv< 43.4)
# MEDVhight 為房價高的資料
# MEDVOther 其他資料
colMeans(MEDVhight)
colMeans(MEDVOther)
rbind(colMeans(MEDVhight),colMeans(MEDVOther))
View(Boston)

Boston2=Boston
Boston2$Income=cut(Boston2$lstat,breaks=3)
#將低收入戶比例分成三類別
View(Boston2)
Boston2$Income= cut(Boston2$lstat,breaks=3,labels=c("H","M","L"))
table(Boston2$Income)
str(Boston2)
x = c(1, 2)
Month1=c(1,2,3,4,5,6,7,8,9,10,11,12)
Month2=1:12 # 建立連續向量的簡易寫法
TaipeiTem=c(16.1,16.5,18.5 ,  21.9 ,	25.2 ,	27.7 ,	29.6 ,	29.2,27.4,24.5 ,	21.5 ,	17.9)
HengchunTem=c(20.7 , 21.4 , 23.2 ,  25.2 ,	27.0 ,	27.9 ,	28.4 ,	28.1 ,	27.4 ,	26.3 ,	24.3 ,	21.7)
names(TaipeiTem) = Month1

names(HengchunTem) = Month2
TaipeiTem
HengchunTem
c(1, 2, 3) + c(3, 2, 1)
c(1, 2, 3) + 1
c(1, 2, 3) + 1
c(1, 2, 3) + 1
guess <- c(1:10) + c(2, 4)
guess
1:3 * 2
x <- c(1, 2, "3")
x
x <- c("F","M","F","F")
x
x <- factor(c("F","M","F","F"), levels=c("F","M"))
x
as.integer(x)
levels(x)
x <- factor(c("F","M","F","F"), levels=c("F"))
x
#農業社會 男尊女卑
Argri <- factor(c("F","M","F","F"),order=TRUE,levels=c("F","M"))
#阿美族 女尊男卑
Amis <- factor(c("F","M","F","F"),order=FALSE,levels=c("F","M"))

#應該要用
Amis <- factor(c("F","M","F","F"),order=TRUE,levels=c("M","F"))

rank=factor(c("C","A","B","B","C","C"),order=TRUE,level=c("C","B","A"))
rank
rank[1]<rank[2]

x <- "abc";y <- "dbbbe"
paste(x, y, sep=",")
strsplit(x, "b")
name1<-"郭雪芙"
substring(name1, 1, 1)
Jolin = c(age=34,height=158,weight=40)
Jolin
Jolin = c(age=34,height=158,weight=40,Location="Taipei",Gender="F")
Jolin
x<-list(1,2,"3")
Jolin = c(age=34,height=158,weight=40,Location="Taipei",Gender="F")
Jolin
#答案
Jolin = list(age=34,height=158,weight=40,Location="Taipei",Gender="F")
Jolin
View(Boston)
Boston[1,]
View(Boston)
Boston[,1]
Serial=c(1,2,3)
RBasicTeacher=c("Dboy", "Ning", "Noah")
RBasicRbind=rbind(Serial,RBasicTeacher)
Serial=c(1,2,3)
RBasicTeacher=c("Dboy", "Ning", "Noah")
RBasicCbind=cbind(Serial,RBasicTeacher)
RBasic=cbind(c(1,2,3),c("Dboy", "Ning", "Noha"))
factor_example=factor(c("A", "B", "A"))
summary(factor_example)
character_example=c("a","b","c")
summary(character_example)
list_example <- list(1, 2.0, "3")
summary(list_example)

summary(Boston2)
x <- 1:5
x[2:3]
x <- 1:5
x > 3
x[x > 3]
TaipeiTem
HengchunTem
TaipeiTem>20
TaipeiHotMonths = TaipeiTem[TaipeiTem>20]
SelectMonth= HengchunTem>20
HengchunHotMonths = HengchunTem[SelectMonth==TRUE]
 TaipeiTem[7:9]
mean(TaipeiTem[7:9])
HengchunTem[7:9]
mean(HengchunTem[7:9])
# TaipeiTem[1,2,12]
mean(TaipeiTem[c(1,2,12)])
mean(HengchunTem[c(1,2,12)])
TemTable=rbind(TaipeiTem,HengchunTem)
TemTable
#選取台北二月的溫度
TemTable[1,2]
#選取二月的溫度
TemTable[,2]
TemTable[1,]

head(Boston)
head(Boston[["crim"]]) # head(Boston$crim)
as.numeric("2")
as.integer("a")
x <- c(1, 2, 3, 2, 3, 2, 1)
as.character(x) # 字串
factor(x) 
x <- c("1", "2", "3", "2", "a")
as.numeric(x)
