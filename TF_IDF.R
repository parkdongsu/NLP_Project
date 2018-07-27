#1:1비율
#Frequency Matrix
#TF-IDF Maxtrix
#Rank(ALL)
#뽑기에 해당되지 않은 워드 벡터 10개

#아직 미완성

#0과 1 인 경우 몇십개씩만뽑자################################################################
#doc_random_0[60010:60060]
#doc_random_1[10010:10060]
#doc.list_0[doc_random_0[60010:60060]]
#doc.list_1[doc_random_1[10010:10060]]

#write.csv(doc.list_0[doc_random_0[60010:60060]],"D:/Dongsu/NLP_Sample/Not-admission.csv")
#write.csv(doc.list_1[doc_random_1[10010:10060]],"D:/Dongsu/NLP_Sample/Re-admission.csv")

#for(i in 1:)

ad_list <- list()
for(i in 1:50){
    ad_list[i] <- strsplit(doc.list_0[doc_random_0[i]][[1]]," ")
}

write.csv(doc.list_0[doc_random_0[1:10]],"D:/Dongsu/NLP_Sample/Not-admission.csv")
write.csv(doc.list_1[doc_random_1[1:10]],"D:/Dongsu/NLP_Sample/Re-admission.csv")
############################################################################################

doc.list_1["295866"]
search_df$NOTE_ID[295866]
z <- result_xml_df_1[result_xml_df_1$`<MN>`=='현병력',]
z$NOTE_ID["295866"]

library(tm)
library(SnowballC)
library(progress)
library(Matrix)

#랜덤 변수 지정
set.seed(1)

#랜덤으로 숫자 다 뽑아 놓고 뽑아서 쓰기.
doc_random_0 <- sample(length(doc.list_0))
doc_random_1 <- sample(length(doc.list_1))

#몇개씩 꺼내쓸지 정하기.
option <- 1000
#몇번 돌릴지 정하기
count<- 1

#TOP_100 저장할 DF 생성 
TOP_100_DF <- data.frame(stringsAsFactors = FALSE)

pb <- progress_bar$new(total=count)
first_doc <- 1
for(tic in 1:count){ #count개씩 끊어서 진행.
    pb$tick()
    
    last_doc <- tic*option
    if(tic != 1){
        first_doc <- first_doc+option
    }
    #0
    None_Target_note <- doc.list_0[doc_random_0[first_doc:last_doc]]
    #1
    Target_note <-      doc.list_1[doc_random_1[first_doc:last_doc]]
    
    #두가지 경우를 합침. #위의 n개가 타겟, 아래의 n개가 논타겟
    Total_Target_note <- c(Target_note,None_Target_note)
    
    word_piece_list <- list()
    for (i in 1:length(Total_Target_note)){
        word_piece_list[i] <- strsplit(Total_Target_note[[i]]," ")
        word_piece_list[[i]] <- word_piece_list[[i]][-1]
    }
    
    #DF 생성용 
    word_piece <- unlist(word_piece_list)
    unique_word_piece <- unique(word_piece)
    
    #빈도수 체크
    frequency_Matrix <- matrix(length(Total_Target_note),length(unique_word_piece),data=0)
    rownames(frequency_Matrix) <- names(Total_Target_note)
    colnames(frequency_Matrix) <- unique_word_piece
    
    #DTM에 문서에 단어 카운트 할당
    for(i in 1:nrow(frequency_Matrix)){
        for(k in word_piece_list[[i]]){
            frequency_Matrix[i,k] <- frequency_Matrix[i,k] + 1
        }
    }
    Path <- paste('D:/Dongsu/NLP_Sample/Frequency_Matrix/Frequency_Matrix_',tic,'.csv')
    write.csv(frequency_Matrix,file = Path)
    
    
    for (i in 1:nrow(frequency_Matrix)){
        for(k in 1:ncol(frequency_Matrix)){
            if(frequency_Matrix[i,k] != 0){
                frequency_Matrix[i,k] = 1 
            }
        }
    }
    
    #Poission(TF 전처리) : 하나의 문서에서 지나치게 반복 되어 나온 데이터를 정리하는 과정.
    write.csv(frequency_Matrix,'D:/Dongsu/NLP_Sample/z.csv')
    
    
    
    
    
    #Numbering
    num <- rep(1:length(unique_word_piece))
    
    #mapping DF 생성
    mapping_df <- data.frame(unique_word_piece,num,stringsAsFactors = FALSE)
    
    tmp_piece_list = list()
    for(i in 1:length(word_piece_list)){
        for(k in 1:length(word_piece_list[[i]])){
            word_piece_list[[i]][k] <- mapping_df[mapping_df$unique_word_piece == word_piece_list[[i]][k],2]
        }
        tmp_piece_list[i] <- gsub("\n"," ",as.String(word_piece_list[[i]]))
        Total_Target_note[i] <- as.vector(tmp_piece_list[i])
    }
    
    my_docs <- VectorSource(Total_Target_note)
    my_corpus <- Corpus(my_docs)
    DTM <- DocumentTermMatrix(my_corpus)
    
    #인식하지 못하는 문서를 없애 줌.
    exception <- c()
    corpus_vector <- rep(1:length(my_corpus))
    if (length(unique(DTM$i)) != length(corpus_vector)){
        diff_vector <-  sort(setdiff(corpus_vector,unique(DTM$i)),decreasing = TRUE)
        
        for(i in diff_vector){
            my_corpus <- my_corpus[-i]
            exception <- c(exception,i)
        }
    }
    DTM <- DocumentTermMatrix(my_corpus)
    
    #매핑.
    for (i in 1:length(DTM$dimnames$Terms)){
        DTM$dimnames$Terms[i] <- mapping_df[mapping_df$num == DTM$dimnames$Terms[i],1]
    }
    
    #TF-IDF function
    DTM_tfxidf<-weightTfIdf(DTM)
    
    
    val <- length(exception[exception<=option])
    
    #TF_IDF중 재방문 한경우의 Matrix
    TOPIC<-as.matrix(DTM_tfxidf[1:(option-val),])
    Path <- paste('D:/Dongsu/NLP_Sample/TF-IDF_Matrix/TF-IDF_Matrix_only_1_',tic,'.csv')
    write.csv(TOPIC,file = Path)  
    #TF_IDF중 전체 경우의 Matrix
    #TOPIC<-as.matrix(DTM_tfxidf[1:nrow(DTM_tfxidf),])
    #write.csv(TOPIC,file = 'D:/Dongsu/NLP_Sample/TF-IDF_Matrix/TF-IDF_Matrix_total',i,'.csv')  
    
    #Topic_rating DF 생성
    Filt_TOPIC <- data.frame(stringsAsFactors = FALSE)
    for (i in 1:length(Target_note)){
        TOPIC<-as.matrix(DTM_tfxidf[i,])
        #한 진단서 내용에 TF-IDF값이 1개만 차있으면 names()가 NULL이 되어 나눠서 처리. 
        if (length(as.vector(TOPIC[,TOPIC[1,]>0])) == 1){
            for(k in 1:length(TOPIC)){
                if(TOPIC[,TOPIC[1,]>0] == TOPIC[1,k]){
                    tmp_Filt_TOPIC<-data.frame('TOPIC' = names(TOPIC[1,][k]),'RATIO' = as.vector(TOPIC[,TOPIC[1,]>0]),stringsAsFactors = FALSE)
                }
            }  
        }
        else{
            tmp_Filt_TOPIC<-data.frame('TOPIC' = names(TOPIC[,TOPIC[1,]>0]),'RATIO' = as.vector(TOPIC[,TOPIC[1,]>0]),stringsAsFactors = FALSE)
            Filt_TOPIC <- rbind(Filt_TOPIC,tmp_Filt_TOPIC)
        }
    }
    
    
    #TOP LEVEL 지정
    
    unique_Filt_TOPIC <- unique(Filt_TOPIC[,1])
    
    Topic_level_df <- data.frame('TOPIC' = unique_Filt_TOPIC,'RATIO' = 0 ,stringsAsFactors = FALSE)
    #Ratio 값의 합을 Topic_level_df에 저장해 Top_level 추출 
    for(i in 1:nrow(Topic_level_df)){
        ratio_vector <- sum(Filt_TOPIC[Filt_TOPIC$TOPIC == Topic_level_df$TOPIC[i],2])
        Topic_level_df[i,2] <- ratio_vector
    }
    Topic_level_df <- Topic_level_df[c(order(-Topic_level_df$RATIO)),]
    Topic_level_df <- Topic_level_df[1:100,]
    
    #첫번째 루프에서는 첫 Topic_level_df를 TOP_100_DF로 설정 
    if(tic ==1){
        TOP_100_DF <- Topic_level_df
    }
    #TOP_100 저장
    TOP_100_DF <- cbind(TOP_100_DF,Topic_level_df)
    
    
}


Path <- paste('D:/Dongsu/NLP_Sample/TOP-RANK_DataFrame/TOP-RANK_DataFrame_',tic,'.csv')
write.csv(TOP_100_DF,file = Path)  




#정규분포(DF 전처리) : 중요하지 않은 단어를 제거하는 과정.
#for (i in 1:nrow(frequency_Matrix)){
#    for(k in 1:ncol(frequency_Matrix)){
#        if(frequency_Matrix[i,k] != 0){
#            frequency_Matrix[i,k] = 1 
#        }
#    }
#}
#plot_y <- c()
#for(i in 1:ncol(frequency_Matrix)){
#    plot_y <- c(plot_y,sum(frequency_Matrix[,i]))
#}
#plot_x <- c(1:length(plot_y))

#plot(plot_x,plot_y)
#plot_x


#tmp_list[i]는 단어 숫자를 저장
tmp = c()
for(i in 1:ncol(frequency_Matrix)){
    tmp <- c(tmp,sum(frequency_Matrix[,i]))
}
#빈도를 저장할 Value
value <- rep(0,max(tmp))
length(value)
for(i in 1:length(tmp)){
    value[tmp[i]] <- value[tmp[i]]+1
}
length(tmp)
tmp
value

hist(x = value,breaks = length(value))
?hist

#for(i in 1:ncol(frequency_Matrix)){
#    plot_y <- c(plot_y,sum(frequency_Matrix[,i]))
#}

#value_list <- rep(0,max(frequency_Matrix))
#for (i in 1:nrow(frequency_Matrix)){
#    for(k in 1:ncol(frequency_Matrix)){
#        value_list[frequency_Matrix[i,k]] <- value_list[frequency_Matrix[i,k]]+1
#    }
#}

value_list <- rep(0,max(frequency_Matrix))

frequency_Matrix(1,0)

for (i in 1:nrow(frequency_Matrix)){
    for(k in 1:ncol(frequency_Matrix)){
        value_list[frequency_Matrix[i,k]] <- value_list[frequency_Matrix[i,k]]+1
    }
}
name <- rep(1:length(value_list))
name

plot(name,value_list)

value_list

