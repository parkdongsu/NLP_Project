
#����
library(progress)

#���� ����
dictionary_eng <-read.table('D:/Dongsu/NLP_Sample/R_CODE/DIC/dic_english.txt',stringsAsFactors = FALSE)
dictionary_eng <- dictionary_eng$V1
#�ҹ��ڷ� �ٲ���. 
dictionary_eng <- tolower(dictionary_eng)

#���ܼ��� ��� ���� �ܾ� ����(�ѱ� ����)
eng_tmp_word <- c()
eng_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    only_eng <-gsub('[��-�R]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
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

#������ ������ ���� ���� 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    exist_tmp_word <- dictionary_eng[which(dictionary_eng == eng_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)



#��Ż�� ã��.
tmp_similar_word_eng <-c()
similar_word_eng <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    word <- eng_word[i]

    if (nchar(word)>2){ # 3���� �̻� ã��.
        
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

#����------------------------------------------------------------
library(Rcpp)
library(digest)

#�ѱ�
N_gram_length = 3
word_storage_N1 <- c()
word_storage_N2 <- c()
word_storage_N3 <- c()

for(count in 2:N_gram_length){
    

    #���� ����
    #dictionary. R ���Ͽ� ����
    
    #���ܼ��� ��� �ѱ� �ܾ� ����(���� ����)
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
    
    #������ ������ ���� ���� 
    exist_word <- c()
    exist_tmp_word <- c()

    for(i in 1:length(kor_word_unique)){

        exist_tmp_word <- dictionary[which(dictionary == kor_word_unique[i])]
        exist_word <- c(exist_word,exist_tmp_word)
    }
    kor_word_unique <- setdiff(kor_word_unique,exist_word)
    
    
    #��Ż�� ã��
    
    
    #����-�ѱ� �ܾ� ��------------------------------------------------------------
    
    #�ܾ���� '����(Set)'�� �� �ܾ� ������ ���� String ������ ����
    kor_word_string <- as.String(kor_word_unique)
    
    #\n ������ ���� �� �ܾ��� ��ġ�� ���� 
    spacing <- as.vector(gregexpr('\n',kor_word_string)[[1]])    
    spacing <- c(0,spacing)
    
    #Ȥ�� ���� df �� ���� (�ܾ� ��ġ, �ܾ�)
    word_location_df <- data.frame(location = c(spacing), word = c(kor_word_unique),stringsAsFactors = FALSE)
    
    #spacing�� ����
    word_location <- word_location_df$location
    
    #�� �ܾ
    
    word_storage <- c()
    for(i in 1:length(dictionary)){
      
        #str �ܾ�� ���� �ܾ� �ϳ��� �ܾ ���ٸ� ���° �ڸ��� �������� ����.
        diction_location <- as.vector(gregexpr(dictionary[i],kor_word_string)[[1]])
        
        #diction_location�� ���� �������� ����
        if(diction_location[1] != -1){
            for(k in 1:length(diction_location)){
                #�� �����ܾ�� ���� ����� \n�� ��ġ ���� ����.(���밪�̶� +,- ������ �ȵǼ� ���� �ΰ��� �Ѵ� ����)
                high_dic <- diction_location[k] + min(abs((word_location)-diction_location[k]))
                low_dic <-  diction_location[k] - min(abs((word_location)-diction_location[k]))
                #������ �ϳ��� ���� �̻��� ��, �ϳ��� ���� ���� �������.
                high_dic_word <- word_location_df[word_location_df$location == high_dic,2]
                low_dic_word <- word_location_df[word_location_df$location == low_dic,2]   
                #�ܾ ������ �ִ��� �ľ��ؼ� �־���.
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
    
    #����-�ѱ� �ܾ� ��------------------------------------------------------------
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


