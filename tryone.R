## ready for korean scinario in one forder as text files.
## 시나리오를 txt 파일로 한 폴더내에 넣어주세요.(파일명은 영어가 좋습니다.거기까지 신경쓰지 못했어요.) 
## create variable using in for loop

movieList<-list.files('./data/movie')  ##고쳐야 겠지만 파일을 불러올 폴더에서 txt 파일 이름을 불러옵니다.
numMovieList<-length(movieList)        ##불러와야할 txt 파일의 갯수를 확인합니다.
movieNames<-gsub(".txt","",movieList)  ##파일이름을 변수로 사용하기 위해 .txt를 뗴어냅니다.

## for loop using readLines and encoding change to UTF-8 for Korean
## I want to use data as a variable because of using this -> """ eval(parse(test=paste0())) """
## 포문은 파일 이름인 놈을 변수로 txt를 readLines 명령으로 불러와 list로 저장한다.

for (i in 1:numMovieList){
	assign(eval(parse(text=paste0('movieNames[',i,']'))),eval(parse(text=paste0('readLines("./data/movie/',movieList[i],'")'))))
	assign(eval(parse(text=paste0('movieNames[',i,']'))),iconv(eval(parse(text=paste0(eval(parse(text=paste0('movieNames[',i,']')))))), "UTF-8", "CP949"))
}



