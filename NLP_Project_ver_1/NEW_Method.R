
#0
None_Target_note <- doc.list_0
#1
Target_note <-      doc.list_1

#�ΰ��� ��츦 ��ħ. #���� n���� Ÿ��, �Ʒ��� n���� ��Ÿ��
Total_Target_note <- c(Target_note,None_Target_note)

#" "�� �������� ���� word_piece_list��� ����Ʈ�� ���� 
word_piece_list <- list()
for (i in 1:length(Total_Target_note)){
    word_piece_list[i] <- strsplit(Total_Target_note[[i]]," ")
    word_piece_list[[i]] <- word_piece_list[[i]][-1]
}
#unique_word�� ��� ����
WORD <- unique(unlist(word_piece_list))

#WORD �� ���� ���� NUM ����
NUM <- rep(0,length(WORD))

#������ F,T���� vector�� ���� ������ �غ� ��. 
word_piece_vec_F <- as.vector(unlist(word_piece_list[1:length(doc.list_0)]))
word_piece_vec_T <- as.vector(unlist(word_piece_list[(length(doc.list_0)+1):(length(doc.list_0)+length(doc.list_1))]))


#�󵵼� DF�� ������� 
WORD_NUM_DF_T <- data.frame(table(word_piece_vec_T),stringsAsFactors = FALSE)
WORD_NUM_DF_F <- data.frame(table(word_piece_vec_F),stringsAsFactors = FALSE)

#��ü ���忡�� �󵵼� DF���� �������� ������ -> ���ο� DF�� ���� �󵵼��� 0���� �ϰ� �ٿ��ִ� �뵵. 
diffWORD_T <-setdiff(WORD,as.vector(WORD_NUM_DF_T$word_piece_vec_T))
diffWORD_F <-setdiff(WORD,as.vector(WORD_NUM_DF_F$word_piece_vec_F))

#�󵵼� 0 
NUM_T <- rep(0,length(diffWORD_T))
NUM_F <- rep(0,length(diffWORD_F))

# �������� �ܾ�� �󵵼��� 0���� �� DF �ϼ�
add_diffWORD_T <- data.frame(word_piece_vec_T = diffWORD_T,Freq = NUM_T,stringsAsFactors = FALSE)
add_diffWORD_F <- data.frame(word_piece_vec_F = diffWORD_F,Freq = NUM_F,stringsAsFactors = FALSE)

#DF�� ������.
WORD_NUM_DF_T <- rbind(WORD_NUM_DF_T,add_diffWORD_T)
WORD_NUM_DF_F <- rbind(WORD_NUM_DF_F,add_diffWORD_F)

#RANK
Rank_Freq_T <- rank(WORD_NUM_DF_T$Freq)
Rank_Freq_F <- rank(WORD_NUM_DF_F$Freq)

#RANK �ο�
WORD_NUM_DF_T$Freq <- Rank_Freq_T
WORD_NUM_DF_F$Freq <- Rank_Freq_F

#MERGE �ϱ����� Colname ����
colnames(WORD_NUM_DF_T) <- c('word_piece_vec','Freq')
colnames(WORD_NUM_DF_F) <- c('word_piece_vec','Freq')

#MERGE
MERGE_DF <- merge(WORD_NUM_DF_T,WORD_NUM_DF_F, by = 'word_piece_vec')

#CSV�� �����ϰ� �ϱ����� ����.
WORD_TMP_DF <- MERGE_DF[,1]

COUNT_DF <- data.frame(scale((MERGE_DF[,2]-MERGE_DF[,3]),center = 0))
colnames(COUNT_DF) <- c("value")

COUNT_DF <- transform(COUNT_DF,value = (value - min(value))/(max(value) - min(value)))
COUNT_DF <- COUNT_DF*2-1


#DF ������
RESULT_DF <- data.frame('T_WORD' = WORD_TMP_DF, 'COUNT_DF' = COUNT_DF,stringsAsFactors = FALSE )
Path <- "D:/Dongsu/NLP_Sample/new_method_2.csv"
write.csv(RESULT_DF,file = Path,row.names = FALSE)








