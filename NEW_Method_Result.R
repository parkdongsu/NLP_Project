
library(progress)

#랜덤 변수 지정
set.seed(1)

#doc_list 합침
total_doc.list <- c(doc.list_0,doc.list_1)

doc_random <- sample(length(total_doc.list))

option <- 100
count <- 10
first_doc <- 1

pb <- progress_bar$new(total=count)
#count 번 돌림
for(tic in 1:count){
    pb$tick()
    last_doc <- tic*option
    if(tic != 1){
        first_doc <- first_doc+option
    }
    
    #100개씩 임의로 뽑아옴
    Target_note <- total_doc.list[doc_random[first_doc:last_doc]]
    
    #입원/입원x를 구분해주는 벡터 
    TF_vec <- doc_random[first_doc:last_doc] > length(doc.list_0)
    TF_vec <- gsub('FALSE','F',TF_vec)
    TF_vec <- gsub('TRUE','T',TF_vec)
    #" "을 기준으로 나눠 word_piece_list라는 리스트로 저장 
    word_piece_list <- list()
    for (i in 1:length(Target_note)){
        word_piece_list[i] <- strsplit(Target_note[[i]]," ")
        word_piece_list[[i]] <- word_piece_list[[i]][-1]
    }
    length(word_piece_list)
    #word_piece_list에 문자 -> 숫자 매핑.
    for(i in 1:length(word_piece_list)){
        for(k in 1:length(word_piece_list[[i]])){
            word_piece_list[[i]][k] <- RESULT_DF[RESULT_DF$T_WORD == word_piece_list[[i]][k],2]
        }
    }
    
    Value_df <- data.frame('Score' = NA,'Class' = NA, stringsAsFactors = FALSE)
    Value_df
    for(i in 1:length(word_piece_list)){
        tmp_vec <- c(sum(as.double(word_piece_list[[i]])),TF_vec[i])
        Value_df <- rbind(Value_df,tmp_vec)
    }
    Value_df <- Value_df[-1,]
    
    if (tic ==1){
        Final_Value_df <- Value_df
    }
    else{
        Final_Value_df <- cbind(Final_Value_df,Value_df)
    }
    sum(as.double(word_piece_list[[1]]))
}


Path <- paste('D:/Dongsu/NLP_Sample/new_method_3.csv')
write.csv(Final_Value_df,file = Path,row.names = FALSE) 

