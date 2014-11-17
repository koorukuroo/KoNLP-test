#========================#
# 04. Parallel 처리 방안 #
#========================#

# for Windows
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=3)

# for Mac
install.packages("doMC")
library(doMC)
registerDoMC(cores=3)
