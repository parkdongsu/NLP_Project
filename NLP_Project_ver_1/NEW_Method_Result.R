
library(progress)

#���� ���� ����
set.seed(1)

#doc_list ��ħ
total_doc.list <- c(doc.list_0,doc.list_1)

doc_random <- sample(length(total_doc.list))

option <- 100
count <- 10
first_doc <- 1

pb <- progress_bar$new(total=count)
#count �� ����
for(tic in 1:count){
    pb$tick()
    last_doc <- tic*option
    if(tic != 1){
        first_doc <- first_doc+option
    }
    
    #100���� ���Ƿ� �̾ƿ�
    Target_note <- total_doc.list[doc_random[first_doc:last_doc]]
    
    #�Կ�/�Կ�x�� �������ִ� ���� 
    TF_vec <- doc_random[first_doc:last_doc] > length(doc.list_0)
    TF_vec <- gsub('FALSE','F',TF_vec)
    TF_vec <- gsub('TRUE','T',TF_vec)
    #" "�� �������� ���� word_piece_list��� ����Ʈ�� ���� 
    word_piece_list <- list()
    for (i in 1:length(Target_note)){
        word_piece_list[i] <- strsplit(Target_note[[i]]," ")
        word_piece_list[[i]] <- word_piece_list[[i]][-1]
    }
    length(word_piece_list)
    #word_piece_list�� ���� -> ���� ����.
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
