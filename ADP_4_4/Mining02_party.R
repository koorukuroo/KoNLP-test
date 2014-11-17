#===============================#
# 02. 분류분석 (Classification) #
#===============================#

#############################
# 5. party 패키지 분석 사례 #
#############################

# (a) data load
install.packages("HDclassif")
library(HDclassif)
data(wine)
head(wine)
str(wine)

# (b) factorize the target variable
wine$class<-factor(wine$class)
table(wine$class)

# (c) data 분리
set.seed(2020)
ind<-sample(2,nrow(wine),replace=TRUE, prob=c(0.7,0.3))
table(ind)

tr_wine<-wine[ind==1,]
val_wine<-wine[ind==2,]

str(tr_wine)

# (d) 패키지 설치 및 load
install.packages("party")
library(party)

# (e) Training model
mdl_party<-ctree(class~., data=tr_wine)
str(mdl_party)

mdl_party@tree # model (text)

mdl_party@where # node of train set

mdl_party@get_where(newdata=val_wine) # node of validation set

# (f) Visulization
plot(mdl_party)
plot(mdl_party,terminal_panel = node_barplot)
plot(mdl_party,type = "simple")
plot(mdl_party,terminal_panel = node_terminal)

# (g) performance check - train set
res_tr<-table(predict(mdl_party),tr_wine$class)
res_tr/sum(res_tr)

# Accuracy
sum(diag(res_tr))/sum(res_tr)

# precision
res_tr[1,1]/sum(res_tr[1,])
res_tr[2,2]/sum(res_tr[2,])
res_tr[3,3]/sum(res_tr[3,])

# detect rate(sensitivity)
res_tr[1,1]/sum(res_tr[,1])
res_tr[2,2]/sum(res_tr[,2])
res_tr[3,3]/sum(res_tr[,3])

# (h) performance - validation set
res_val<-table(predict(mdl_party,newdata=val_wine),val_wine$class)
res_val/sum(res_val)

# Accuracy
sum(diag(res_val))/sum(res_val)

# precision
res_val[1,1]/sum(res_val[1,])
res_val[2,2]/sum(res_val[2,])
res_val[3,3]/sum(res_val[3,])

# detect rate
res_val[1,1]/sum(res_val[,1])
res_val[2,2]/sum(res_val[,2])
res_val[3,3]/sum(res_val[,3])
