
#¿µ¾î
library(progress)

#»çÀü »ı¼º
dictionary_eng <-read.table('D:/Dongsu/NLP_Sample/R_CODE/DIC/dic_english.txt',stringsAsFactors = FALSE)
dictionary_eng <- dictionary_eng$V1
#¼Ò¹®ÀÚ·Î ¹Ù²ãÁÜ. 
dictionary_eng <- tolower(dictionary_eng)

#Áø´Ü¼­ÀÇ ¸ğµç ¿µ¾î ´Ü¾î ÃßÃâ(ÇÑ±Û Á¦°Å)
eng_tmp_word <- c()
eng_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    only_eng <-gsub('[°¡-ÆR]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
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

#»çÀü¿¡ ÀÖÀ¸¸é ¿öµå Á¦°Å 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    exist_tmp_word <- dictionary_eng[which(dictionary_eng == eng_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)



#¿ÀÅ»ÀÚ Ã£±â.
tmp_similar_word_eng <-c()
similar_word_eng <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    word <- eng_word[i]

    if (nchar(word)>2){ # 3±ÛÀÚ ÀÌ»ó¸¸ Ã£À½.
        
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

#¿µ¾î------------------------------------------------------------
library(Rcpp)
library(digest)

#ÇÑ±Û
N_gram_length = 3
word_storage_N1 <- c()
word_storage_N2 <- c()
word_storage_N3 <- c()

for(count in 2:N_gram_length){
    

    #»çÀü »ı¼º
    #dictionary. R ÆÄÀÏ¿¡ ÀÖÀ½
    
    #Áø´Ü¼­ÀÇ ¸ğµç ÇÑ±Û ´Ü¾î ÃßÃâ(¿µ¾î Á¦°Å)
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
    
    #»çÀü¿¡ ÀÖÀ¸¸é ¿öµå Á¦°Å 
    exist_word <- c()
    exist_tmp_word <- c()

    for(i in 1:length(kor_word_unique)){

        exist_tmp_word <- dictionary[which(dictionary == kor_word_unique[i])]
        exist_word <- c(exist_word,exist_tmp_word)
    }
    kor_word_unique <- setdiff(kor_word_unique,exist_word)
    
    
    #¿ÀÅ»ÀÚ Ã£±â
    
    
    #»çÀü-ÇÑ±Û ´Ü¾î ºñ±³------------------------------------------------------------
    
    #´Ü¾îµéÀÇ 'ÁıÇÕ(Set)'À» ÇÑ ´Ü¾î ´ÜÀ§·Î ²÷¾î String °ªÀ¸·Î ÀúÀå
    kor_word_string <- as.String(kor_word_unique)
    
    #\n ´ÜÀ§·Î ³ª´² °¢ ´Ü¾îÀÇ À§Ä¡¸¦ ÀúÀå 
    spacing <- as.vector(gregexpr('\n',kor_word_string)[[1]])    
    spacing <- c(0,spacing)
    
    #È¤½Ã ¸ô¶ó df ·Î ÀúÀå (´Ü¾î À§Ä¡, ´Ü¾î)
    word_location_df <- data.frame(location = c(spacing), word = c(kor_word_unique),stringsAsFactors = FALSE)
    
    #spacing°ú µ¿ÀÏ
    word_location <- word_location_df$location
    
    #°¢ ´Ü¾î¸¦
    
    word_storage <- c()
    for(i in 1:length(dictionary)){
      
        #str ´Ü¾î¿¡¼­ »çÀü ´Ü¾î ÇÏ³ª´ç ´Ü¾î°¡ µé¾î°£´Ù¸é ¸î¹øÂ° ÀÚ¸®¿¡ ³ª¿À´ÂÁö ±¸ÇÔ.
        diction_location <- as.vector(gregexpr(dictionary[i],kor_word_string)[[1]])
        
        #diction_locationÀÇ °ªÀÌ ¾øÀ»¶§´Â ¹«½Ã
        if(diction_location[1] != -1){
            for(k in 1:length(diction_location)){
                #°¢ »çÀü´Ü¾î¿¡¼­ Á¦ÀÏ °¡±î¿î \nÀÇ À§Ä¡ °ªÀ» ±¸ÇÔ.(Àı´ë°ªÀÌ¶ó +,- ±¸ºĞÀÌ ¾ÈµÇ¼­ º¯¼ö µÎ°³¿¡ µÑ´Ù ÀúÀå)
                high_dic <- diction_location[k] + min(abs((word_location)-diction_location[k]))
                low_dic <-  diction_location[k] - min(abs((word_location)-diction_location[k]))
                #µÑÁßÀÇ ÇÏ³ªÀÇ °ª¿£ ÀÌ»óÇÑ °ª, ÇÏ³ª´Â Á¤»ó °ªÀÌ µé¾îÀÖÀ½.
                high_dic_word <- word_location_df[word_location_df$location == high_dic,2]
                low_dic_word <- word_location_df[word_location_df$location == low_dic,2]   
                #´Ü¾î°¡ »çÀü¿¡ ÀÖ´ÂÁö ÆÄ¾ÇÇØ¼­ ³Ö¾îÁÜ.
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
    
    #»çÀü-ÇÑ±Û ´Ü¾î ºñ±³------------------------------------------------------------
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



