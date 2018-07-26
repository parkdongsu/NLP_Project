library(tm)
library(SnowballC)
#랜덤 변수 지정 
set.seed(1)

#0에서 1/100개 뽑음
None_Target_note <- sample(doc.list_0,as.integer(length(doc.list_0)/100))
#1에서 1/100개 뽑음 
Target_note <- sample(doc.list_1,as.integer(length(doc.list_1)/100))

#두가지 경우를 합침. 
Total_Target_note <- c(Target_note,None_Target_note)

#매핑하기 위해 띄어쓰기로 리스트 화.
word_piece_list <- list()
for (i in 1:length(Total_Target_note)){
    word_piece_list[i] <- strsplit(Total_Target_note[[i]]," ")
    word_piece_list[[i]] <- word_piece_list[[i]][-1]
}
#DF 생성용 
word_piece <- unlist(word_piece_list)
word_piece <- unique(word_piece)

#Numbering
num <- rep(1:length(word_piece))

#mapping DF 생성
mapping_df <- data.frame(word_piece,num,stringsAsFactors = FALSE)

#매핑.
tmp_piece_list = list()
for(i in 1:length(word_piece_list)){
    for(k in 1:length(word_piece_list[[i]])){
        word_piece_list[[i]][k] <- mapping_df[mapping_df$word_piece == word_piece_list[[i]][k],2]
    }
    tmp_piece_list[i] <- gsub("\n"," ",as.String(word_piece_list[[i]]))
    Total_Target_note[i] <- as.vector(tmp_piece_list[i])
}

my_docs <- VectorSource(Total_Target_note)
my_corpus <- Corpus(my_docs)
DTM <- DocumentTermMatrix(my_corpus)

#인식하지 못하는 문서를 없애 줌.
corpus_vector <- rep(1:length(my_corpus))
diff_vector <-  sort(setdiff(corpus_vector,unique(DTM$i)),decreasing = TRUE)

for(i in diff_vector){
    my_corpus <- my_corpus[-i]
}

DTM <- DocumentTermMatrix(my_corpus)


#매핑.
for (i in 1:length(DTM$dimnames$Terms)){
    DTM$dimnames$Terms[i] <- mapping_df[mapping_df$num == DTM$dimnames$Terms[i],1]
}

#TF-IDF function

DTM_tfxidf<-weightTfIdf(DTM)
inspect(DTM_tfxidf)

#DTM_matrix <- as.matrix(DTM_tfxidf)

TOPIC<-as.matrix(DTM_tfxidf[1,])
Filt_TOPIC<-as.data.frame(TOPIC[,TOPIC[1,]>0])
Filt_TOPIC

Filt_TOPIC <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(Target_note)){
    TOPIC<-as.matrix(DTM_tfxidf[i,])
    tmp_Filt_TOPIC<-as.data.frame(TOPIC[,TOPIC[1,]>0])
    Filt_TOPIC <- rbind(Filt_TOPIC,tmp_Filt_TOPIC)
}
























