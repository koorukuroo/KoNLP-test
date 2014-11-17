movieList<-list.files('./data/movie')
numMovieList<-length(movieList)
movieNames<-gsub(".txt","",movieList)

for (i in 1:numMovieList){
	assign(eval(parse(text=paste0('movieNames[',i,']'))),eval(parse(text=paste0('readLines("./data/movie/',movieList[i],'")'))))
	assign(Encoding(eval(parse(text=paste0(eval(parse(text=paste0('movieNames[',i,']'))))))),"UTF-8")
}

head(iconv(eval(parse(text=paste0(eval(parse(text=paste0('movieNames[',i,']')))))),from="CP1252",to="UTF-8"),10)