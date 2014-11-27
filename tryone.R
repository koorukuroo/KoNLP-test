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

## HMM 알고리즘으로 학습한 모델 불러오기(source : http://freesearch.pe.kr/archives/2746)
load("./data/movie/spacing/hmms.RData")

## for loop using readLines and encoding change to UTF-8 for Korean
## I want to use data as a variable because of using this -> """ eval(parse(test=paste0())) """
## 포문은 파일 이름인 놈을 변수로 txt를 readLines 명령으로 불러와 list로 저장한다.

for (i in 1:numMovieList){
	assign(eval(parse(text=paste0('movieNames[',i,']'))),eval(parse(text=paste0('readLines("./data/movie/',movieList[i],'")'))))
	assign(eval(parse(text=paste0('movieNames[',i,']'))),iconv(eval(parse(text=paste0(eval(parse(text=paste0('movieNames[',i,']')))))), "UTF-8", "CP949"))
}

load("./data/movie/spacing/hmms.RData")

spacing(hmm,)







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


