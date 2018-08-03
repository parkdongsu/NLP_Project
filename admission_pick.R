#admission 뽑기

library(tm)
library(SnowballC)
library(progress)
library(Matrix)

#랜덤 변수 지정
set.seed(1)

#랜덤으로 숫자 다 뽑아 놓고 뽑아서 쓰기.
doc_random_0 <- sample(length(doc.list_0))
doc_random_1 <- sample(length(doc.list_1))



#몇 번 미만의 단어를 지울지 정하기.
remove_words = 2
#중복된 단어의 최대 갯수 정하기.
overlap_words = 10


#0
None_Target_note <- doc.list_0[doc_random_0[60001:60060]]
#1
Target_note <-      doc.list_1[doc_random_1[10001:10060]]

#두가지 경우를 합침. #위의 n개가 타겟, 아래의 n개가 논타겟
Total_Target_note <- c(Target_note,None_Target_note)

#" "을 기준으로 나눠 word_piece_list라는 리스트로 저장 
word_piece_list <- list()
for (i in 1:length(Total_Target_note)){
    word_piece_list[i] <- strsplit(Total_Target_note[[i]]," ")
    word_piece_list[[i]] <- word_piece_list[[i]][-1]
}

#NOTE_ID를 유지하기 위해 저장
Total_Target_note_name <- names(Total_Target_note)

#DF 생성용 
word_piece <- unlist(word_piece_list)
unique_word_piece <- unique(word_piece)

#Numbering
num <- rep(1:length(unique_word_piece))

#n번 이하로 나온 단어를 제거하기 위한 DF의 열
count <- rep(0,length(unique_word_piece))

#mapping DF 생성
mapping_df <- data.frame(unique_word_piece,num,count,stringsAsFactors = FALSE)

#단어 나온 갯수 count 열에 추가.
for (i in 1:length(word_piece)){
    mapping_df[mapping_df$unique_word_piece == word_piece[i],3] <- mapping_df[mapping_df$unique_word_piece == word_piece[i],3] + 1
}

#word_piece_list에 문자 -> 숫자 매핑.
for(i in 1:length(word_piece_list)){
    for(k in 1:length(word_piece_list[[i]])){
        word_piece_list[[i]][k] <- mapping_df[mapping_df$unique_word_piece == word_piece_list[[i]][k],2]
    }
}

#remove_words번 미만 나온 단어들의 매핑숫자들을 벡터로 만듦 
rare_mapping_vec <- mapping_df[mapping_df[,3]<remove_words,2]

#rare_mapping_vec와 word_piece_list의 교집합을 구해 n번 이하로 나온 단어만 없애줌.
empty_vec <- c()
for(i in 1:length(word_piece_list)){
    erase_word <- intersect(word_piece_list[[i]],rare_mapping_vec)
    for(k in erase_word){
        word_piece_list[[i]] <- gsub(k,NA,word_piece_list[[i]])
    }
    word_piece_list[[i]] <- as.vector(na.omit(word_piece_list[[i]]))
    if(is.na(word_piece_list[[i]][1]) == TRUE){
        empty_vec <- c(empty_vec,i)  
    }
}
if(length(empty_vec) > 0){
    for(i in length(empty_vec):1){
        word_piece_list <- word_piece_list[-empty_vec[i]]
        Total_Target_note_name <- Total_Target_note_name[-empty_vec[i]]# NOTE_ID도 같이 제거
    }
}

#한 문서에서 overlap_words번 이상 나온 단어는 overlap_words번으로 고정 시킴.
for (i in 1:length(word_piece_list)){
    reduce_df <- subset(data.frame(COUNT = table(word_piece_list[i])),COUNT.Freq >= overlap_words+1)
    if(nrow(reduce_df) > 0 ){
        wpl <- unlist(word_piece_list[i])
        for(k in 1:nrow(reduce_df)){
            remove_word <- which(wpl == as.vector(reduce_df[k,1]))
            remove_word <- remove_word[-1:-overlap_words]
            for(m in remove_word){
                wpl[m] <- NA
            }
            wpl <- as.vector(na.omit(wpl))
        }
        word_piece_list[[i]] <- wpl   
    }
}


#word_piece_list를 vecor로 만들어 TOtal_Target_note에 저장하기 위함.
tmp_piece_list = list()
tmp_Total_Target_note <- list()
for(i in 1:length(word_piece_list)){
    tmp_piece_list[i] <- gsub("\n"," ",as.String(word_piece_list[[i]]))
    tmp_Total_Target_note[i] <- as.vector(tmp_piece_list[i])
}
names(tmp_Total_Target_note) <- Total_Target_note_name
##
Total_Target_note <- tmp_Total_Target_note

test_list <- list()

for (i in 1:length(Total_Target_note)){
    test_list[i] <- strsplit(Total_Target_note[[i]]," ")
}

for(i in 1:length(test_list)){
    for(k in 1:length(test_list[[i]])){
        test_list[[i]][k] <- mapping_df[mapping_df$num == test_list[[i]][k],1]
    }
}



tmp_test_list = list()
tmp_Total_test_note <- list()
for(i in 1:length(test_list)){
    tmp_test_list[i] <- gsub("\n"," \t",as.String(test_list[[i]]))
    tmp_Total_test_note[i] <- as.vector(tmp_test_list[i])
}
tmp_Total_test_note[[3]]

zz<- data.frame(stringsAsFactors = FALSE)
length(NOTE_TEXT,tmp_Total_test_note)
for(i in 1:length(tmp_Total_test_note)){
    zz[i,1]<-as.vector(tmp_Total_test_note[[i]])
}
z<- rep(0,59)
c<-rep(1,60)
z
c
cc<- c(z,c)
xx<- cbind(cc,zz)





write.csv(xx,'D:/Dongsu/NLP_Sample/admisson.csv')