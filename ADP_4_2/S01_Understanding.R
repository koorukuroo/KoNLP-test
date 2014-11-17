#====================#
# 01.통계분석의 이해 #
#====================#

###############
# 7. Sampling #
###############
sample(1:100,5) # 비복원추출(default)
sample(1:100,5,replace=T) # 복원추출
sample(1:100,5,replace=T,prob=1:100) # 가중치 부여

# install.packages("sampling")
library(sampling)
data(iris)

# 비복원 층화추출
x<-strata(c("Species"),size=c(2,2,2),method="srswor",data=iris)
getdata(iris,x)

# 복원 층화추출
x<-strata(c("Species"),size=c(2,2,2),method="srswr",data=iris)
getdata(iris,x)

# install.packages("doBy")
library(doBy)

# 계통 샘플 추출
x<-data.frame(x=1:100)
sampleBy(~1, frac=.3, data=x,systematic=T)


###############
# 9. 확률분포 #
###############
# 난수발생: 평균이 10 이고, 표준편차가256인 정규분포 5개 난수 발생
rnorm(5, mean=10, sd=25)

# 확률계산: 평균이 0 이고, 표준편차가 1인 표준정규분포에서 X<=0의 확률
pnorm(0,mean=0,sd=1)

# 표준정규분포에서 누적확률이 0.9인 x
qnorm(0.9, mean=0,sd=1)

# 표준정규분포에서 누적확률이 0.95인 x
qnorm(0.95, mean=0,sd=1)

# 표준정규분포에서 누적확률이 0.99인 x
qnorm(0.99, mean=0,sd=1)

# 확률밀도함수: 표준정규분포 확률밀도함수에서 x=0의 확률
dnorm(0, mean=0, sd=1)
dnorm(0)

# 표준정규분포 그리기
plot(density(rnorm(100000,0,1)))


#########################
# 11.추정과 검정의 사례 #
#########################
# (1) 단일 표본 평균에 대한 추정 및 검정사례
x<-rnorm(1000)
t.test(x)

# (2) 2개 표본 평균에 대한 추정 및 검정 사례
data(sleep)
t.test(extra~group, data=sleep, paired=F, var.equal=T)
t.test(extra~group, data=sleep, paired=T, var.equal=T)

# (3) 기타 사례
#이표본분산
data(iris)
var.test(iris$Sepal.Width,iris$Sepal.Length)

# 일표본비율
prop.test(42,100)
binom.test(42,100)

# 이표본비율(귀무가설: 성공,실패의 확률이 같다)
prop.test(c(44,55),c(100,90))

# 상관계수 검정: 양측검정에서 0.05이하면 귀무가설 기각
cor.test(c(1,2,3,4,5),c(1,0,3,4,5),method="pearson")

# 독립성 검정
s.tab<-xtabs(~Sex+Exer,data=survey) # 분할표
s.tab
chisq.test(s.tab)

# 정규분포의 값인지 검정
shapiro.test(rnorm(1000))

# 정규분포인지 검정
ks.test(rnorm(100),runif(100))
