#1.공란처리 -> /n /t, 2칸떨어진것 모두 한칸으로 바꿔줌
#2.대소문자 통일 -> 의대생분과 상의 후 작업
#3.숫자표현 -> 숫자들을 모두 _number_로 바꿔 인식 or 숫자 그대로 인식 or 숫자와 뒤의 단어를 합쳐 저장 ex) 3회, 3번 등 어떻게 할 것인지
#4.문장부호 및 특수문자 제거 -> 특이 케이스만 분류해 합쳐줘 저장.  만약 그렇다면 의대생분과 작업, 아니라면 어떻게 할 것인지
#5.불용단어 제거 ->로직 생각해서 적용 한글은 필요없는 품사 제거 / (명사는 명사로 남기고 다른 품사는 또 어떻게 처리할것인지 고민) 
#6.어근 동일화 처리 -> 한글은 적용X,  영어는 어떻게 할것인지
#7.엔그램(n번 연이어 등장하는 단어들의 연쇄) ->하나의 의학용어는 붙여야하는지 따로 할 것인지 붙인다면 의대생분과 상의후 작업 #방법은 지정된 영어문자로
#변경후 ex) Unique_word1 나중에 POS_EXTRACTION함수 실행하면 영어니까 그대로 나옴 그 후 list에서 찾아서 다시 바꿔줌 
#8.영어 접두사, 접미사 제거해야할지?  맞다면 그것만 하면 되는지 # 일단 끝나고 해야함 지정 용어외엔 단어 정립후 앞뒤로 뗄지??

#ubuntu 환경에서 topicmodels install 시 ctm.c:29:25: fatal error: gsl/gsl_rng.h: No such file or directory 오류 나오면
#sudo apt-get install libgsl0-dev 로 해결

#사전 준비사항########################################
#XML_Parsing_Pro7에서 file=""에 RDS 파일 경로를 써주고 실행한 후 실행 할 것.
#C:\Program Files\R\R-3.5.1\library\base\R\RProfile에 options(java.parameters = c("-Xmx16384m","-Dfile.encoding=UTF-8")) 추가 # KoNLP에러 방지 
#options(java.parameters = c("-Xmx16384m","-Dfile.encoding=UTF-8"))
#options("java.parameters")$java.parameters
######################################################

#POS 추출 함수
K_POS_EXTRACTION <- function(wordlist){
    #5.불용 단어 제거
    wordlist <- gsub('/F+','/CW+',wordlist)
    wordlist <- gsub('/NC+','/CW+',wordlist)
    
    pos_start <- as.vector(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]]) # 정규표현식을 통해 걸러서 사용 
    pos_length <- as.vector(attr(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]],'match.length'))
    
    pos_end <- pos_start+pos_length-5
    
    word_data = rep(NA,length(pos_start))
    word <- c()
    for(i in 1:length(pos_start)){
        word_data[i] <- substr(wordlist,pos_start[i],pos_end[i])
        word <- paste(word,word_data[i])
    }
    return(word)
}

#문장중 제거할 부분을 선처리해주는 함수  
NLP_PROCESSING <- function(xmldf){
    #4.특수 문자 변경 및 제거
    xmldf <- gsub('&#x0D;', " ", xmldf) # 띄어쓰기는 마지막에 무조건 하나로 통일 해주는 부분이 있음. 
    xmldf <- gsub('&lt;', " ", xmldf)
    xmldf <- gsub('&gt;', " ", xmldf)
    xmldf <- gsub('&amp;', " ", xmldf)
    xmldf <- gsub('&quot;', " ", xmldf)
    
    xmldf <- gsub("[ㄱ-ㅎ]","", xmldf)#자음 오타 제거
    xmldf <- gsub("[ㅏ-ㅣ]","", xmldf)#모음 오타 제거 
    
    xmldf <- gsub("[\\‘]","", xmldf)#특수문자 제거
    xmldf <- gsub("[\\’]","", xmldf)#특수문자 제거
    xmldf <- gsub("[\\“]","", xmldf)#특수문자 제거
    xmldf <- gsub("[\\”]","", xmldf)#특수문자 제거
    
    xmldf <- gsub("[\\]","", xmldf)#특수문자 제거
    xmldf <- gsub("[\\+]|[\\{]|[\\}]|[\\(]|[\\)]|[\\<]|[\\>]|[\\']|[\\.]"," ", xmldf)#특수문자 제거
    xmldf <- gsub('[\\"]'," ", xmldf)#특수문자 제거
    xmldf <- gsub("\\["," ", xmldf)#특수문자 제거
    xmldf <- gsub("\\]"," ", xmldf)#특수문자 제거
    xmldf <- gsub("\\/"," ", xmldf)#특수문자 제거
    xmldf <- gsub("\\'"," ", xmldf)#특수문자 제거
    xmldf <- gsub('\\"'," ", xmldf)#특수문자 제거
    xmldf <- gsub("[\\`]|[\\_]|[\\~]|[\\!]|[\\@]|[\\#]|[\\$]|[\\>]|[\\<]|[\\%]|[\\≥]|[\\=]|[\\^]|[\\&]|[\\×]|[\\*]|[\\-]|[\\:]|[\\●]|[\\★]|[\\¤]|[\\±]"," ", xmldf)
    xmldf <- gsub("[\\|]"," ", xmldf)#특수문자 제거
    xmldf <-xmldf <- gsub(',', " ", xmldf) # 콤마는 한칸 떨어뜨려줌.
    
    #2.대소문자 통일(선택가능으로 만들 것)
    #xmldf<- toupper(xmldf) # 대문자 
    xmldf<- tolower(xmldf)# 소문자
    
    #6.어근 동일화 처리
    #xmldf <- gsub(' are ',' be ',xmldf)
    #xmldf <- gsub(' are ',' be ',xmldf)
    #xmldf <- gsub(' is ',' be ',xmldf)
    
    #또는 중요하지않은 단어들을 빼고 싶다
    #xmldf <- gsub('and|of|as|in',"",xmldf)# 선처리 후 공란 처리 할 것.
    
    #7.엔그램
    #xmldf <- sub('[^A-Za-z 가-힣]*graphic[ _-]variant[^A-Za-z 가-힣]*','graphicvariant',xmldf) # KoNLP 처리시 영어문장은 그대로 나오기 때문에 한단어로 바로 나옴.
    
    #한글, 영어가 붙어있는 경우에 떨어뜨려줌.
    pos_start <- as.vector(gregexpr('[^가-힣 ]*[A-Za-z]+[^가-힣 ]*',xmldf)[[1]]) # 정규표현식을 통해 걸러서 사용 
    pos_length <- as.vector(attr(gregexpr('[^가-힣 ]*[A-Za-z]+[^가-힣 ]*',xmldf)[[1]],'match.length'))
    pos_end <- pos_start+pos_length-1
    
    word_data <- c()
    if(length(pos_start) > 0){  
        for(i in 1:length(pos_start)){
            word_data[i] <- substr(xmldf,pos_start[i],pos_end[i])
        }
        
        new_word_data <- paste("",toupper(word_data),"")
        
        for(i in 1:length(word_data)){
            xmldf <- sub(word_data[i],new_word_data[i],xmldf)
        }
    }
    xmldf<- tolower(xmldf)# 다시 소문자 처리를 해줌.
    
    
    #1.공란처리
    xmldf <- stringr::str_replace_all(xmldf,"[[:space:]]{1,}"," ")# 한칸이상의 띄어쓰기를 한칸으로 통일
    
    #바꿀 형식
    xmldf <- paste(xmldf,'.',sep = '')#문장이 아닌 경우 날짜, 약명 등 중요한 정보가 잘리는 경우가 있음. ex) 12-02-02 단어 하나있으면 12-02-0 과 2로 나뉨.
    #어차피 마지막 . 추가해주면 따로 나눠지고 정규표현식에서 거르지 않으니 괜찮을거라고 생각함.
    
    return(xmldf)
}

#품사 분석부
POS_ANALYSIS <- function(word_df){
    word_list <- KoNLP::SimplePos22(word_df)
    if(length(word_list) ==1){
        word_vector <- word_list[[1]]
        result_word_list <- c(word_vector)
    } 
    else{
        word_vector <- word_list[[1]]
        for (k in 2:length(word_list)){
            word_vector <- paste(word_vector,'+',word_list[[k]],sep = '')
        }
        result_word_list <- c(word_vector)
    }
    return(result_word_list)
}


# load packages
if(!require(rJava)) {
    install.packages('rJava')
}
if(!require(KoNLP)) {
    install.packages('KoNLP')
}
if(!require(devtools)) {
    install.packages('devtools')
}
#library(devtools)
#install_github('haven-jeon/NIADic/NIAdic', build_vignettes = TRUE)
if(!require(topicmodels)) {
    install.packages('topicmodels')
}
if(!require(openNLP)) {
    install.packages('openNLP')
}
if(!require(NLP)) {
    install.packages('NLP')
}
if(!require(parallel)) {
    install.packages("parallel")
}

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java/jdk1.8.0_171") 
library(KoNLP)
library(rJava)
library(topicmodels)
library(stringr)
library(parallel)

# 코어 개수 획득
numCores <- parallel::detectCores() - 1
# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores)

########MAIN CODE##############################################################

for (count in 1:2){
    #search###############################################
    if(count ==1){
        search_df <- result_xml_df_0[result_xml_df_0$`<MN>`=='현병력',] # 태그, 검색어 지정 ex) <MN>, '약명'
    }
    if(count ==2){
        search_df <- result_xml_df_1[result_xml_df_1$`<MN>`=='현병력',] # 태그, 검색어 지정 ex) <MN>, '약명'
    }
    ######################################################
    
    tag ='<TD>' # NLP 처리하고 싶은 tag 입력
    
    #검색 후 tag가 NA인 행을 삭제
    search_df[,tag][is.na(search_df[,tag])] <- ""
    
    for (i in nrow(search_df):1){# 뒤에서부터 삭제해 행이 밀려서 삭제 되지 않도록 처리함.
        if(search_df[i,tag] == ""){
            search_df <- search_df[-i,]
        }
    }
    
    
    Sys.time()
    
    #NLP 용 df 만들기
    xml_df <- search_df[tag]
    
    #NLP_PROCESSING 함수를 통한 초기 설정(병렬처리)
    word_df <- data.frame(parApply(myCluster,xml_df,1,NLP_PROCESSING),stringsAsFactors = FALSE)
    
    #형태소 분석후 합치기
    result_word_list <- apply(word_df,1,POS_ANALYSIS)
    
    #result_word_list <- parApply(myCluster,word_df,1,POS_ANALYSIS)
    result_word_list<- unlist(result_word_list)
    
    #원하는 품사 추출 후 하나의 문장으로 합쳐줌. (병렬처리)
    doc.list <- parallel::parLapply(myCluster,result_word_list,K_POS_EXTRACTION)
    
    N.docs<-length(doc.list)
    
    #note_id를 리스트의 이름으로 붙여줌 
    names(doc.list)<- search_df$NOTE_ID
    
    if(count ==1){
        doc.list_0 <- doc.list
    }
    if(count ==2){
        doc.list_1 <- doc.list
    }
}
