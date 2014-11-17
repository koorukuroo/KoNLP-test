#==============================#
# 03. 기초 분석 및 데이터 관리 #
#==============================#

#####################################
# 2. Missing Data 확인 및 제외 방법 #
#####################################

Final4_team <- c("GER","ARG","NED","BRA",NA) # 입력
is.na(Final4_team) # 확인 방법

Final4<-data.frame(team=c("GER","ARG","NED","BRA","GER","BRA","ARG","NED"),stage=c(rep("F",2),rep("34",2),rep("QF",4)),res=c("1","0","0","3","1","7","0(2)","0(4)"))
Final4

Final4[Final4$stage=="QF","res"] <- NA # 대체 방법
Final4

mean(as.numeric(as.character(Final4$res)))
mean(as.numeric(as.character(Final4$res)), na.rm=T)

Final4[!complete.cases(Final4),]


########################################
# 3. Amelia를 이용한 Missing Data 처리 #
########################################

# install.packages("Amelia")
library(Amelia)
data(africa)
head(africa)

# trade에 5개의 NA 존재
summary(africa)

# 변수들의 관계를 이용한 imputation
a.out <- amelia(africa, m = 3, ts = "year", cs = "country")

# imputation 전
missmap(a.out)

# imputation 후
africa$trade <- a.out$imputation[[3]]$trade
missmap(africa)


########################
# 4. Outlier Detection #
########################

## (6) outlier가 있는 row를 파악할 수 있는 방법
library(DMwR)
data(wine,package="HDclassif")
head(wine)

wine_od <- wine[,c(8,11,14,2)]
outlier.scores <- lofactor(wine_od, k = 5)
plot(density(outlier.scores))

outliers <- order(outlier.scores, decreasing = T)[1:5]
print(outliers)

summary(wine_od)

n <- nrow(wine_od)
labels <- 1:n
labels[-outliers] <- "."
biplot(prcomp(wine_od), cex = 0.8, xlabs = labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(wine_od, pch = pch, col = col)
