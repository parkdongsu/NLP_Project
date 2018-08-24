
SourceString <- strsplit(doc.df$NOTE_TEXT[1],' ')[[1]][1]
TargetString <- dictionary

#getOption("sd_num_thread")
word <- stringdist::stringdist(SourceString, TargetString, method = c("jaccard"), useBytes = FALSE,
                             weight = c(d = 1, i = 1, s = 1, t = 1), q = 1,nthread = 1)

min(word)

nchar(SourceString)

# 한글 벡터를 가져온 후 ?자 이상의 숫자를 가진 단어 대해서 min이 1/(글자 갯수) 이하 인 녀석들의 min값을 구함. (min이 0< min값 <1/글자갯수 일 경우 바꿔줌)



library(progress)


strsplit(doc.df$NOTE_TEXT[5],' ')[[1]]
length(strsplit(doc.df$NOTE_TEXT[2],' ')[[1]])
min_distence <- c()
min_word <- c()

min_word_dictionary <- list()

pb <- progress_bar$new(total=((nrow(doc.df))))
for(i in 1:nrow(doc.df)){
    print(i)
    #pb$tick()
    for(k in 1:length(strsplit(doc.df$NOTE_TEXT[i],' ')[[1]]))
        
        word <- strsplit(doc.df$NOTE_TEXT[1],' ')[[1]][2]
        
        word_distence <- min(stringdist::stringdist(word, dictionary, method = c("jaccard"), useBytes = FALSE,
                                   weight = c(d = 1, i = 1, s = 1, t = 1), q = 1,nthread = 7))
        min_distence <- c(min_distence,word_distence)
        min_word     <- c(min_word,paste(i,'_',word))
        #min으로 나온 단어랑 비교했을때 0보다 크고 한글자 이상 틀리면 안됨
        if(min_distence >= 0 & min_distence <= (1/min_distence) ){
            which_word <- which((1/min_distence) > stringdist::stringdist(word, dictionary, method = c("jaccard"), useBytes = FALSE,
                                                            weight = c(d = 1, i = 1, s = 1, t = 1), q = 1,nthread = 7))
            dictionary[which_word]
            
        }
}






doc.df$NOTE_TEXT[4]

length(strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
doc.df$NOTE_TEXT[2]
library(progress)


min_distence <- c()
min_word <- c()

min_word_dictionary <- vector('list',nrow(doc.df))

pb <- progress_bar$new(total=((nrow(doc.df))))
for(i in 1:nrow(doc.df)){
    i = 1
    #pb$tick()
    for(k in 1:length(strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])){
        
        word <- strsplit(doc.df$NOTE_TEXT[1],' ')[[1]][k]
    
    word_distence <- min(stringdist::stringdist(word, dictionary, method = c("jaccard"), useBytes = FALSE,
                                                weight = c(d = 1, i = 1, s = 1, t = 1), q = 1,nthread = 7))
    min_distence <- c(min_distence,word_distence)
    min_word     <- c(min_word,paste(i,'_',word))
    #min으로 나온 단어랑 비교했을때 0보다 크고 한글자 이상 틀리면 안됨
    if(min_distence >= 0 & min_distence <= (1/min_distence) ){
        which_word <- which((1/min_distence) > stringdist::stringdist(word, dictionary, method = c("jaccard"), useBytes = FALSE,
                                                                      weight = c(d = 1, i = 1, s = 1, t = 1), q = 1,nthread = 7))
        min_word_dictionary[[i]][k] <- dictionary[which_word]
        
        }
    }
}
min_word_dictionary[[2]]

