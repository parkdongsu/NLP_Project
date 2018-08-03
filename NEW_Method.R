
#0
None_Target_note <- doc.list_0
#1
Target_note <-      doc.list_1

#두가지 경우를 합침. #위의 n개가 타겟, 아래의 n개가 논타겟
Total_Target_note <- c(Target_note,None_Target_note)

#" "을 기준으로 나눠 word_piece_list라는 리스트로 저장 
word_piece_list <- list()
for (i in 1:length(Total_Target_note)){
    word_piece_list[i] <- strsplit(Total_Target_note[[i]]," ")
    word_piece_list[[i]] <- word_piece_list[[i]][-1]
}
#unique_word를 모두 집계
WORD <- unique(unlist(word_piece_list))
#WORD 와 합쳐 만들 NUM 생성
NUM <- rep(0,length(WORD))

#대입할 F,T값을 vector로 만들어서 대입할 준비를 함. 
word_piece_vec_F <- as.vector(unlist(word_piece_list[1:length(doc.list_0)]))
word_piece_vec_T <- as.vector(unlist(word_piece_list[(length(doc.list_0)+1):(length(doc.list_0)+length(doc.list_1))]))


#빈도수 DF를 만들어줌 
WORD_NUM_DF_T <- data.frame(table(word_piece_vec_T),stringsAsFactors = FALSE)
WORD_NUM_DF_F <- data.frame(table(word_piece_vec_F),stringsAsFactors = FALSE)

#전체 워드에서 빈도수 DF와의 차집합을 구해줌 -> 새로운 DF를 만들어서 빈도수를 0으로 하고 붙여주는 용도. 
diffWORD_T <-setdiff(WORD,as.vector(WORD_NUM_DF_T$word_piece_vec_T))
diffWORD_F <-setdiff(WORD,as.vector(WORD_NUM_DF_F$word_piece_vec_F))

#빈도수 0 
NUM_T <- rep(0,length(diffWORD_T))
NUM_F <- rep(0,length(diffWORD_F))

# 차집합의 단어와 빈도수를 0으로 한 DF 완성
add_diffWORD_T <- data.frame(word_piece_vec_T = diffWORD_T,Freq = NUM_T,stringsAsFactors = FALSE)
add_diffWORD_F <- data.frame(word_piece_vec_F = diffWORD_F,Freq = NUM_F,stringsAsFactors = FALSE)

#DF를 합쳐줌.
WORD_NUM_DF_T <- rbind(WORD_NUM_DF_T,add_diffWORD_T)
WORD_NUM_DF_F <- rbind(WORD_NUM_DF_F,add_diffWORD_F)

#RANK
Rank_Freq_T <- rank(WORD_NUM_DF_T$Freq)
Rank_Freq_F <- rank(WORD_NUM_DF_F$Freq)

#RANK 부여
WORD_NUM_DF_T$Freq <- Rank_Freq_T
WORD_NUM_DF_F$Freq <- Rank_Freq_F

#MERGE 하기위해 Colname 수정
colnames(WORD_NUM_DF_T) <- c('word_piece_vec','Freq')
colnames(WORD_NUM_DF_F) <- c('word_piece_vec','Freq')

#MERGE
MERGE_DF <- merge(WORD_NUM_DF_T,WORD_NUM_DF_F, by = 'word_piece_vec')

#CSV에 적합하게 하기위해 나눔.
WORD_TMP_DF <- MERGE_DF[,1]

COUNT_DF <- data.frame(scale((MERGE_DF[,2]-MERGE_DF[,3]),center = 0))
colnames(COUNT_DF) <- c("value")

COUNT_DF <- transform(COUNT_DF,value = (value - min(value))/(max(value) - min(value)))
COUNT_DF <- COUNT_DF*2-1


#DF 재조합
RESULT_DF <- data.frame('T_WORD' = WORD_TMP_DF, 'COUNT_DF' = COUNT_DF,stringsAsFactors = FALSE )
Path <- "D:/Dongsu/NLP_Sample/new_method_2.csv"
write.csv(RESULT_DF,file = Path,row.names = FALSE)









