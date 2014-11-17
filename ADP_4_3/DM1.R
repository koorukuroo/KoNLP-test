#===========================#
# 01. 데이터의 변경 및 요약 #
#===========================#

###################
# 6. Reshape 사례 #
###################

# install.packages("reshape")
library(reshape)
data(tips)
head(tips)
tips$no<-1:nrow(tips)

# melt
tips_melt <- melt(tips, id = c("no","sex","smoker","day","time"), na.rm = TRUE)
head(tips_melt,2)
tail(tips_melt,2)
str(tips_melt)
table(tips_melt$variable)

# cast
tips_cast_time <- cast(tips_melt, time ~ variable, mean)
tips_cast_time

cast(tips_melt, time ~ variable, c(mean,length))
cast(tips_melt, day ~ variable ~ time,c(mean, length))
cast(tips_melt, day ~ . |variable, mean)
cast(tips_melt, day ~ variable, mean, margins=c("grand_row", "grand_col"))
cast(tips_melt, day ~ time, mean, subset=variable=="tip")
cast(tips_melt, day ~ variable, range)


############
# 7. sqldf #
############

# install.packages("sqldf")
library(sqldf)
data(french_fries ,package="reshape")
head(french_fries)

table(french_fries$treatment)

sqldf("select * from french_fries limit 6")

sqldf("select count(*) from french_fries where treatment=1")

names(french_fries)[2]<-"oil.type"
head(french_fries)

sqldf("select * from french_fries limit 6")

sqldf("select count(*) from french_fries where oil_type=1")


####################
# 8. plyr #
####################

# install.packages("plyr")
library(plyr)
data(baseball)
head(baseball)

bb_subset<-baseball[,c(1,2,5,13)]
head(bb_subset)

ddply(bb_subset[1:100,], ~ year, nrow)

ddply(bb_subset[1:100,], .(year), nrow)

ddply(bb_subset[1:100,], "year", nrow)

ddply(bb_subset, .(lg), c("nrow", "ncol"))

rbi_s <- ddply(bb_subset, .(year), summarise, mean_rbi = mean(rbi, na.rm = TRUE))
head(rbi_s)

rbi_t <- ddply(bb_subset, .(year), transform, mean_rbi = mean(rbi, na.rm = TRUE))
head(rbi_t)

##################
# 10. Data Table #
##################

# install.packages("data.table")
library(data.table)
Final4<-data.table(team=c("GER","ARG","NED","BRA","GER","BRA","ARG","NED"),
                   stage=c(rep("F",2),rep("34",2),rep("QF",4)),
                   res=rep(c(1,0),4),
                   score=c("1","0","0","3","7","1","0(4)","0(2)"))
Final4

data(wine,package="HDclassif")
head(wine)

dt_wine <- data.table(wine)
head(dt_wine)

# 둘 간의 차이
tables()

sapply(dt_wine,class)

sapply(Final4,class)

Final4

Final4[1,]   # 1번째 행 조회

Final4[Final4$team=="GER",]   # team값이 "GER"인 데이터 조회

# data table에 key를 지정해서 해당 값으로 조회
setkey(Final4,team)
Final4 # team에 의해 ordering되어 있음. 

tables()

# “GER”가 들어간 모든 데이터, 첫번째 결과, 마지막 결과, 모든 결과
Final4["GER",]
Final4["GER",mult="first"]
Final4["GER",mult="last"]

# 검색조건 다음에 “,”는 선택사항
Final4["GER"]
