
#영어
library(progress)

#사전 생성
dictionary_eng <-read.table('D:/Dongsu/NLP_Sample/R_CODE/DIC/dic_english.txt',stringsAsFactors = FALSE)
dictionary_eng <- dictionary_eng$V1
#소문자로 바꿔줌. 
dictionary_eng <- tolower(dictionary_eng)

#진단서의 모든 영어 단어 추출(한글 제거)
eng_tmp_word <- c()
eng_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    only_eng <-gsub('[가-힣]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
    only_eng <-unique(only_eng)
    only_eng <- only_eng[-which(only_eng == "")]
    

    eng_tmp_word <- c(eng_tmp_word,only_eng)
    
    if(i%%100 == 0){
        eng_word <- c(eng_word,eng_tmp_word)
        eng_tmp_word <- c()
    }
}
eng_word <- c(eng_word,eng_tmp_word)
eng_word <- unique(eng_word)
length(eng_word)

#사전에 있으면 워드 제거 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    exist_tmp_word <- dictionary_eng[which(dictionary_eng == eng_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)



#오탈자 찾기.
tmp_similar_word_eng <-c()
similar_word_eng <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    word <- eng_word[i]

    if (nchar(word)>2){ # 3글자 이상만 찾음.
        
    levenshtein_eng_list <- RecordLinkage::levenshteinDist(word,dictionary_eng)
    
    word_distence <- min(levenshtein_eng_list)
    
    if(length(which(levenshtein_eng_list == 1)) != 0){
        tmp_similar_word_eng <- c(tmp_similar_word_eng,paste(word,':',dictionary_eng[which(levenshtein_eng_list == 1)]))
    }
    }
    
    if(i%%100 == 0){
        similar_word_eng <- c(similar_word_eng,tmp_similar_word_eng)
        tmp_similar_word_eng <- c()
    }
    
}
similar_word_eng <- c(similar_word_eng,tmp_similar_word_eng)

length(similar_word_eng)


write.csv(similar_word_eng,'D:/levenshtein.csv')

similar_word_eng_df <- data.frame(similar_word_eng,stringsAsFactors = FALSE)

#영어------------------------------------------------------------
library(Rcpp)
library(digest)

#한글
N_gram_length = 3
word_storage_N1 <- c()
word_storage_N2 <- c()
word_storage_N3 <- c()

for(count in 2:N_gram_length){
    

    #사전 생성
    #dictionary. R 파일에 있음
    
    #진단서의 모든 한글 단어 추출(영어 제거)
    if(count == 1){
        kor_tmp_word <- c()
        kor_word <- c()

        for(i in 1:nrow(doc.df)){
            
            only_kor <-gsub('[a-zA-Z]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
            only_kor <-unique(only_kor)
            only_kor <- only_kor[-which(only_kor == "")]
            
            
            kor_tmp_word <- c(kor_tmp_word,only_kor)
            
            if(i%%100 == 0){
                kor_word <- c(kor_word,kor_tmp_word)
                kor_tmp_word <- c()
            }
        }
        kor_word <- c(kor_word,kor_tmp_word)
        kor_word_unique <- unique(kor_word)
    }
    
    else if(count == 2){
        kor_tmp_word <- c()
        kor_word <- c()

        for(i in 1:nrow(doc.df)){
 
            only_kor <-strsplit(doc.df$NOTE_TEXT_N2[i],' ')
            only_kor <-unique(only_kor)
            only_kor <- only_kor[-which(only_kor == "")]
            
            
            kor_tmp_word <- c(kor_tmp_word,only_kor)
            
            if(i%%100 == 0){
                kor_word <- c(kor_word,kor_tmp_word)
                kor_tmp_word <- c()
            }
        }
        kor_word <- c(kor_word,kor_tmp_word)
        kor_word_unique <- unique(kor_word)
    }
    else if(count == 3){
        kor_tmp_word <- c()
        kor_word <- c()

        for(i in 1:nrow(doc.df)){

            only_kor <-strsplit(doc.df$NOTE_TEXT_N3[i],' ')
            only_kor <-unique(only_kor)
            only_kor <- only_kor[-which(only_kor == "")]
            
            
            kor_tmp_word <- c(kor_tmp_word,only_kor)
            
            if(i%%100 == 0){
                kor_word <- c(kor_word,kor_tmp_word)
                kor_tmp_word <- c()
            }
        }
        kor_word <- c(kor_word,kor_tmp_word)
        kor_word_unique <- unique(kor_word)
    }
    
    #사전에 있으면 워드 제거 
    exist_word <- c()
    exist_tmp_word <- c()

    for(i in 1:length(kor_word_unique)){

        exist_tmp_word <- dictionary[which(dictionary == kor_word_unique[i])]
        exist_word <- c(exist_word,exist_tmp_word)
    }
    kor_word_unique <- setdiff(kor_word_unique,exist_word)
    
    
    #오탈자 찾기
    
    
    #사전-한글 단어 비교------------------------------------------------------------
    
    #단어들의 '집합(Set)'을 한 단어 단위로 끊어 String 값으로 저장
    kor_word_string <- as.String(kor_word_unique)
    
    #\n 단위로 나눠 각 단어의 위치를 저장 
    spacing <- as.vector(gregexpr('\n',kor_word_string)[[1]])    
    spacing <- c(0,spacing)
    
    #혹시 몰라 df 로 저장 (단어 위치, 단어)
    word_location_df <- data.frame(location = c(spacing), word = c(kor_word_unique),stringsAsFactors = FALSE)
    
    #spacing과 동일
    word_location <- word_location_df$location
    
    #각 단어를
    
    word_storage <- c()
    for(i in 1:length(dictionary)){
      
        #str 단어에서 사전 단어 하나당 단어가 들어간다면 몇번째 자리에 나오는지 구함.
        diction_location <- as.vector(gregexpr(dictionary[i],kor_word_string)[[1]])
        
        #diction_location의 값이 없을때는 무시
        if(diction_location[1] != -1){
            for(k in 1:length(diction_location)){
                #각 사전단어에서 제일 가까운 \n의 위치 값을 구함.(절대값이라 +,- 구분이 안되서 변수 두개에 둘다 저장)
                high_dic <- diction_location[k] + min(abs((word_location)-diction_location[k]))
                low_dic <-  diction_location[k] - min(abs((word_location)-diction_location[k]))
                #둘중의 하나의 값엔 이상한 값, 하나는 정상 값이 들어있음.
                high_dic_word <- word_location_df[word_location_df$location == high_dic,2]
                low_dic_word <- word_location_df[word_location_df$location == low_dic,2]   
                #단어가 사전에 있는지 파악해서 넣어줌.
                if(length(high_dic_word) != 0){
                    if(length(grep(dictionary[i],high_dic_word)) == TRUE){
                        word_storage <- c(word_storage,high_dic_word)
                    }
                }
                
                if(length(low_dic_word) != 0){
                    if(length(grep(dictionary[i],low_dic_word)) == TRUE){
                        word_storage <- c(word_storage,low_dic_word)
                    }
                }
                
                
            }
        }
    }
    
    #사전-한글 단어 비교------------------------------------------------------------
    if(count == 1 ){
        word_storage_N1 <- c(word_storage_N1,word_storage)
    }
    else if(count == 2 ){
        word_storage_N2 <- c(word_storage_N2,word_storage)
    }
    else if(count == 3 ){
        word_storage_N3 <- c(word_storage_N3,word_storage)
    }
}

write.csv(word_storage_N1,'D:/N1.csv')



