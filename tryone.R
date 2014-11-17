movieList<-list.files('./data/movie')
numMovieList<-length(movieList)
movieNames<-gsub(".txt","",movieList)

for (i in 1:numMovieList){
	assign(eval(parse(text=paste0('movieNames[',i,']'))),eval(parse(text=paste0('readLines("./data/movie/',movieList[i],'")'))))
	assign(eval(parse(text=paste0('movieNames[',i,']'))),iconv(eval(parse(text=paste0(eval(parse(text=paste0('movieNames[',i,']')))))), "UTF-8", "CP949"))
}
