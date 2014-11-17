#========================================#
# 05. 연관성 분석 (Association Analysis) #
#========================================#

#############################
# 5. R을 이용한 연관성 분석 #
#############################

## (1) arules 패키지를 이용한 연관성 분석

# (a) data 탐색
install.packages("arules")
library(arules)
data(Groceries)

str(Groceries)
table(Groceries@data@i)
table(Groceries@data@Dim)
head(Groceries@itemInfo)
summary(Groceries)
inspect(head(Groceries))

length(Groceries)
nrow(Groceries)
ncol(Groceries)

# (b) apriori function을 이용해 연관규칙 도출
G_arules<-apriori(Groceries, parameter=list(support=0.01, confidence=0.3))
summary(G_arules)
str(G_arules)
inspect(head(G_arules))
summary(G_arules@quality$lift)

# (c) 도출한 125개의 규칙 중 특정한 조건에 만족하는 규칙만 도출
(G_arules_yogurt <- subset(G_arules, subset = rhs %in% "yogurt" & lift > 2))
inspect(G_arules_yogurt)
inspect(G_arules_yogurt@lhs)

(G_arules_root_vegetables <- subset(G_arules, subset = rhs %in% "root vegetables" & lift > 2))
inspect(G_arules_root_vegetables)

inspect(head(sort(G_arules_yogurt, by = "confidence"), n = 3))
inspect(head(sort(G_arules_yogurt, by = "support"), n = 3))


## (2) arulesSequences 패키지를 이용한 순차연관성 분석

# (a) data 탐색
install.packages("arulesSequences")
library(arulesSequences)
data(zaki)
? zaki
str(zaki)

summary(zaki)
inspect(head(zaki))
as(zaki,"data.frame")

length(zaki)
ncol(zaki)

# (b) cspade function을 이용해 순차연관규칙 도출
z_arules_Seq <- cspade(zaki, parameter = list(support=0.4),control=list(verbose=TRUE))
str(z_arules_Seq)
summary(z_arules_Seq)
as(z_arules_Seq,"data.frame")

## (3) arulesViz패키지를 이용한 시각화

# (a) 그래프로 표시할 연관규칙 모델링
install.packages("arulesViz")
library(arulesViz)
G_arulesViz<-apriori(Groceries, parameter=list(support=0.005, confidence=0.3))

# (b) 데이터 탐색
G_arulesViz
inspect(head(G_arulesViz,by="confidence"))

# (c) 모델 시각화
plot(G_arulesViz)

# (d) 모델 시각화: interactive mode와 축 바꾸기
plot(G_arulesViz,interactive=TRUE)
plot(G_arulesViz, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

# (d) 모델 subset
G_arulesViz_sub1 <- G_arulesViz[quality(G_arulesViz)$confidence > 0.5]
G_arulesViz_sub1

# (e) 모델 시각화: 규칙의 matrix화, lift만을 measure로 하여 시각화
plot(G_arulesViz_sub1, method="matrix", measure="lift")

# (f) 모델 시각화: 규칙의 matrix화, lift와 confidence를 measure로 하여 시각화
plot(G_arulesViz_sub1, method="matrix", measure=c("lift", "confidence"))

# (g) 모델 시각화: 규칙의 3차원 matrix화, lift를 한 축으로 하여 시각화
plot(G_arulesViz_sub1, method="matrix3D", measure="lift")

# (h) 모델 시각화: 그룹 옵션으로 시각화
plot(G_arulesViz_sub1, method="grouped")

# (i) 모델 시각화: lift 상위 10개에 대해 그래프 옵션으로 시각화
G_arulesViz_sub2 <- head(sort(G_arulesViz, by="lift"),10)
plot(G_arulesViz_sub2, method="graph", control=list(type="items"))
