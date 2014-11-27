##requires
require(KoNLP)
require(tm)
require(Rcpp)
require(doParallel)
registerDoParallel(7)

## ready for korean scinario in one forder as text files.
## 시나리오를 txt 파일로 한 폴더내에 넣어주세요.(파일명은 영어가 좋습니다.거기까지 신경쓰지 못했어요.) 
## create variable using in for loop

##고쳐야 겠지만 파일을 불러올 폴더에서 txt 파일 이름을 불러옵니다.
movieList<-list.files('./data/movie')  

##불러와야할 txt 파일의 갯수를 확인합니다.
numMovieList<-length(movieList)        

##폴더에 spacing 폴더가 있어 같이 이름이 들어가므로 제거 합니다.
##remvoe 'spacing' forder 
movieList<-movieList[-numMovieList]

##파일이름을 변수로 사용하기 위해 .txt를 뗴어냅니다.
movieNames<-gsub(".txt","",movieList)  


## for loop using readLines and encoding change to UTF-8 for Korean
## I want to use data as a variable because of using this -> """ eval(parse(test=paste0())) """
## 포문은 파일 이름인 놈을 변수로 txt를 readLines 명령으로 불러와 list로 저장한다.

for (i in 1:numMovieList){
  assign(eval(parse(text=paste0('movieNames[',i,']'))),eval(parse(text=paste0('readLines("./data/movie/',movieList[i],'")'))))
  assign(eval(parse(text=paste0('movieNames[',i,']'))),iconv(eval(parse(text=paste0(eval(parse(text=paste0('movieNames[',i,']')))))), "UTF-8", "CP949"))
}





#sentence spacing algorithm with HMM  
#setwd("C:/work/RWork/spacing")
library(HMM)
 
#문자열 코딩, 00101 
makeCorpus <- function(str){
  strv <- strsplit(str,split="")[[1]]
  lenstrv <- length(strv)
  spacev <- vector(mode="character",length=lenstrv)
  charv  <- vector(mode="character",length=lenstrv)
  vidx <- 1
  for(i in 1:lenstrv){
    if(strv[i] == " ") {
      next
    }
    if(i + 1 <= lenstrv && strv[i + 1] == " "){
      #spacev <- append(spacev, "1")
      spacev[vidx] <- "1"
    }else{
      if(i == lenstrv){
        spacev[vidx] <- "1"
      }else{
        spacev[vidx] <- "0"
      }
    }
    charv[vidx] <- strv[i]
    vidx <- vidx + 1
  }
  return(data.frame(status=spacev,char=charv))
}
 
 
#전이확률 계산 
getTransprob <- function(str){
  charv <- strsplit(str,split="")[[1]]
  status <- unique(charv)
  transMat <- matrix(0,nrow=length(status),ncol=length(status), 
                     dimnames=list(status, status))
  for(i in 1:(length(charv) -1)){
    rowidx <- which(rownames(transMat) == charv[i])
    colidx <- which(colnames(transMat) == charv[i + 1])
    transMat[rowidx, colidx] <- transMat[rowidx, colidx] + 1
  }
  prop.table(transMat,margin=1)
}
 
#관측확률 계산
observProb <- function(rawcorpus){
  statecntdf <- aggregate(rawcorpus[[1]], by=list(rawcorpus[[2]]), table)  
  statecnts <- as.data.frame(statecntdf$x)
  prob0 <- statecnts$`0`/rowSums(statecnts)
  prob1 <- statecnts$`1`/rowSums(statecnts)
  cbind(char=statecntdf[,1], statecnts, prob0, prob1)
}
 
 
# 모델을 가지고 하는 띄어쓰기
spacing <- function(hmm, str){
  strsp <- strsplit(str, split="")[[1]]
  emissions <- viterbi(hmm, strsp)
  if(length(strsp) != length(emissions)){
    stop("lengths are not match!")
  }
  strspacing <- vector(mode="character",length=length(strsp) * 2)
  spacingidx <- 1
  for(i in 1:length(emissions)){
    strspacing[spacingidx] <- strsp[i]
    if(emissions[i] == "1"){
      spacingidx <- spacingidx + 1
      strspacing[spacingidx] <- " "
    }
    spacingidx <- spacingidx + 1
  }
  return(paste(strspacing[1:spacingidx-1], collapse=""))
}
 

################ 실행 ########################################################
 #코퍼스 파일 정재
gsub("]>","",gsub("\t","",(gsub("<(.|\n)*?>","",lines))))->line
linel<-line[nchar(line)!=0]


#코퍼스 파일 읽음 
fp <- file("./data/movie/spacing/corpus.txt", encoding="UTF-8")
lines <- readLines(fp)
close(fp)

fullstr <- paste(line, collapse="")
#띄어쓰기 코딩 
fullrawcorpus <- makeCorpus(fullstr)
#관측확률 계산 
oprob <- observProb(fullrawcorpus)
#전이확률 계산 
trprob <- getTransprob(paste(fullrawcorpus$status,collapse=""))
 
#HMM 모델 빌드
hmm <- initHMM(c("0", "1"), oprob$char, 
        startProbs=c(.9,.1), transProbs=trprob, emissionProbs=t(as.matrix(oprob[,c(5,6)])))







# using score.sentiment() source(https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/R/sentiment.R)
##<-

#' 
#' score.sentiment() implements a very simple algorithm to estimate
#' sentiment, assigning a integer score by subtracting the number 
#' of occurrences of negative words from that of positive words.
#' 
#' @param sentences vector of text to score
#' @param pos.words vector of words of postive sentiment
#' @param neg.words vector of words of negative sentiment
#' @param .progress passed to <code>laply()</code> to control of progress bar.
#' @returnType data.frame
#' @return data.frame of text and corresponding sentiment scores
#' @author Jefrey Breen <jbreen@cambridge.aero>
score.sentiment = function(sentences, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = lapply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)

    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)

    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
  
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)

    return(score)
  }, .progress=.progress )

  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

##->


