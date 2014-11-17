#=================#
# 05. 주성분 분석 #
#=================#

###########################
# 2. R을 이용한 분석 사례 #
###########################

## (1) data load와 좋은 방향 일치

# (a) heptathlon

library(HSAUR)
data(heptathlon)
head(heptathlon)
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

# (b) 산점도 확인
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
print(heptathlon_pca)

# (c) 상관계수 확인
round(cor(heptathlon[,-score]), 2)

# (d) prcomp 함수 적용
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
print(heptathlon_pca)

summary(heptathlon_pca)
(a1 <- heptathlon_pca$rotation[,1])
(a2 <- heptathlon_pca$rotation[,2])

cor(heptathlon$score,heptathlon_pca$x[,1])
plot(heptathlon_pca)

# (e) rescale을 위해 center와 scale값 추출
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale

# (f) 첫번째 주성분 추출
hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, scale = scale) %*% heptathlon_pca$rotation[,1])

predict(heptathlon_pca)[,1]

# (g) 두 개의 주성분을 이용한 시각화(biplot)
biplot(heptathlon_pca, col = c("gray", "black"))

# (h) 첫번째 주성분과 7종경기 점수와의 상관관계
plot(heptathlon$score, heptathlon_pca$x[,1])
cor(heptathlon$score, heptathlon_pca$x[,1])
