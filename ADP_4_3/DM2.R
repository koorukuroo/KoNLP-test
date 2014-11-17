#=================#
# 02. 데이터 가공 #
#=================#

######################
# 1.Data Exploration #
######################

## (1) head
data(tips,package="reshape")
head(tips) # 최초 6 row 조회

## (2) Summary
summary(tips)

## (3) tabplot
library(tabplot)
tableplot(tips, cex=1.8)

#################
# 2. 변수중요도 #
#################

## (2) klaR 패키지의 greedy.wilks
library(klaR)
data(wine,package="HDclassif")
head(wine)

wine$class<-factor(wine$class)
gw_obj <- greedy.wilks(class ~ ., data = wine, niveau = 0.1)
gw_obj

## (3) klaR 패키지의 plineplot
wine_impVar <- wine[, c("V7","V10","class")]
plineplot(class ~ ., data = wine_impVar, method = "lda", x = wine_impVar$V7, xlab = "V7")

summary(wine$V7)
V7_breaks<-c(0,1,2,3,6)
wine$V7_bin<-cut(wine$V7,V7_breaks)
table(wine$class,wine$V7_bin)

plineplot(class ~ ., data = wine_impVar, method = "lda", x = wine_impVar$V10, xlab = "V10")

summary(wine$V10)
V10_breaks<-c(1,2,4,6,8,13)
wine$V10_bin<-cut(wine$V10,V10_breaks)
table(wine$class,wine$V10_bin)

## (4) klaR 패키지의 NaiveBayes
mN <- NaiveBayes(class ~ ., data = wine[,c(1,8,11,14,2)])
par(mfrow=c(2,2))
plot(mN)
par(mfrow=c(1,1))

## (5) Binning
library(party)
data(wine,package="HDclassif")
wine$class<-factor(wine$class)
summary(wine$V7)
wine$V7_c <- cut(wine$V7,10)

wine_ctr<-ctree(class ~.,data=wine)
plot(wine_ctr)

wine_ctr
