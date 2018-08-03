#사전 준비사항########################################
#PatientLevelPrediction.R 실행 후 실행할 것.
######################################################
library(progress)

#XML_parser -> 정규표현식을 통해 파싱 진행

XML_PARSING <- function(xmlList){
    pattern_start <- as.vector(gregexpr('<[^/<>]+>[^<>]+<\\/[^<>]+>',xmlList)[[1]])
    pattern_length <- as.vector(attr(gregexpr('<[^/<>]+>[^<>]+<\\/[^<>]+>',xmlList)[[1]],'match.length'))
    pattern_end <- pattern_start+pattern_length-1
    
    xml_data = rep(NA,length(pattern_start))
    for(i in 1:length(pattern_start)){
        xml_data[i] <- substr(xmlList,pattern_start[i],pattern_end[i])
    }
    
    return(xml_data)
}

#diag_Processer(맨 아랫단의 태그의 정보만 뽑고 첫 태그부터 다음 첫태그로 구분함.)
#ex) EMR 약명   약명    ##약
#    EMR 진단명 진단명  감기

#파싱하며 각 약명은 ?? 진단명은 ??를 구분하기 위한 함수.
DIAG_PROCESSING <- function(diag_list){
    #첫번째 > 를 기준으로 태그를 추출
    tag_vector  <- as.vector(regexpr('>',diag_list))
    text_vector <- as.vector(regexpr('</',diag_list))
    
    #tag와 text를 구별해 담아줌
    tag_data_vector <- substr(diag_list,1,tag_vector)
    text_data_vector <- substr(diag_list,tag_vector+1,text_vector-1)
    
    #첫번째 태그부터 다음 첫 태그까지 구간 나눠 할당
    first_tag_vector <- as.vector(regexpr(tag_data_vector[1],tag_data_vector))# 첫번째 패턴이 나오면 1로 변환
    
    #결과 저장할 df 생성
    final_xml_df <- data.frame(stringsAsFactors = FALSE) 
    
    #1의 위치를 찾아 위치 정보 넣어줌
    data =c()
    for (i in 1:length(first_tag_vector)){
        if (first_tag_vector[i] == 1){
            data[i] <- i
        }
    }
    #NA 제거
    data <- data[!is.na(data)]
    
    #마지막 첫번째 태그의 나머지 값
    data[length(data)+1] <- (length(first_tag_vector)+1)
    
    #tag 중복 제거
    tag_data_set <- unique(tag_data_vector)
    
    #tag를 행이름 설정
    df <- data.frame(stringsAsFactors = FALSE)
    for (i in tag_data_set){
        df[i] <- character(0)
    }
    #rbind용 df Setting
    tmp_df <- df
    xml_df<- df
    
    #df에 값 넣기
    cnt <- 1
    for (i in 1:(length(data)-1)){
        val <- (data[i+1])-(data[i])
        for (k in 1:val){
            df[1,tag_data_vector[cnt]] <- text_data_vector[cnt]
            cnt <- cnt+1
        }
        xml_df <- rbind(xml_df, df)
        df <- tmp_df
    }
    final_xml_df <- rbind(final_xml_df,xml_df)
    
    return(final_xml_df)
}


########MAIN CODE##############################################################




# load packages
if(!require(parallel)) {
    install.packages("parallel")
}
library(parallel)

# 코어 개수 획득
numCores <- parallel::detectCores() - 1
# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores)

Sys.time()

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="128.1.99.58",
                                                              schema="Dolphin_CDM.dbo",
                                                              user="atlas",
                                                              password="qwer1234!@")
connection <- DatabaseConnector::connect(connectionDetails)
connectionDetails <-connectionDetails
connection <- connection



#
diag_note <- DatabaseConnector::dbGetQuery(conn = connection,statement = "SELECT * FROM DBO.NOTE JOIN COHORT ON NOTE.person_id = COHORT.subject_id AND NOTE.NOTE_DATE = COHORT.COHORT_START_DATE WHERE cohort_definition_id = 747 AND NOTE_TITLE = \'퇴원요약\'") ;

#조건 내에 부합하는 df들의 merge 값 설정###############################################
cohort_outCount_df <- merge(outcomeCount_df,diag_note,by = c("PERSON_ID","NOTE_DATE"))

#######################################################################################

#outCount 값이 0, 1 인 경우로 dataFrame을 나눔 
out_value_0 <- cohort_outCount_df[cohort_outCount_df$outcomeCount == 0,]
out_value_1 <- cohort_outCount_df[cohort_outCount_df$outcomeCount == 1,]



for(count in 1:2){
    #RDS파일을 데이터프레임으로 저장
    #out_value_0 = X 경우, out_value_1 = O인 경우###################
    if(count ==1){
        mediFrame <- out_value_0
    }
    if(count ==2){
        mediFrame <- out_value_1
    }
    ################################################################
    #mediFrame <- readRDS(file,refhook = NULL)
    
    #list 생성 및 ID, TEXT 저장
    medi_list = list(NOTE_ID=c(NA),NOTE_TEXT=c(NA))
    medi_list['NOTE_ID'] <- mediFrame['NOTE_ID']
    medi_list['NOTE_TEXT'] <- mediFrame['NOTE_TEXT']
    
    #XML Parsing을 위해 ROOT 추가.
    medi_list[['NOTE_TEXT']] <- paste("<xml>",medi_list[['NOTE_TEXT']],"</xml>")
    #XML Parsing을 위해 ><사이에 아무것도 없거나 공백이있다면 엔터 처리해준다.
    medi_list[['NOTE_TEXT']] <- gsub('>[ ]*<','>\n<',medi_list[['NOTE_TEXT']])
    
    #결과 저장할 df 생성
    final_xml_df <- data.frame(stringsAsFactors = FALSE) 
    
    #XML 파서로 나눔(병렬처리)
    diagnosis_list <- parallel::parLapply(cl = myCluster, X = medi_list[['NOTE_TEXT']], fun = XML_PARSING)
    
    #진단서 하나의 DataFrame을 list에 저장(병렬처리)
    result_xml_list <- parallel::parLapply(cl = myCluster, X = diagnosis_list, fun = DIAG_PROCESSING)
    
    #한개의 진단서당 NOTE_ID 삽입
    for (i in 1:length(result_xml_list)){
        result_xml_list[[i]][,'NOTE_ID'] <- medi_list[['NOTE_ID']][i]
    }
    
    
    
    #dataFrame에 결과를 정리해서 넣어줌 모든 항목이 NA인 부분은 아예 dataframe에 추가하지 않음.
    #rbind 하는 시간이 오래걸려 div번씩 끊고 한번에 합쳐줌.
    div= 1000
    result_tmp_df <- result_xml_list[[1]]
    flag = 0
    pb <- progress_bar$new(total=(length(result_xml_list)-1))
    for (i in 2:length(result_xml_list)){
        if (length(result_xml_list[[i]]) == length(result_xml_list[[1]])){
            result_tmp_df <- rbind(result_tmp_df,result_xml_list[[i]])
            if(i%%div == 0 & i>=div){
                if(flag == 0){
                    result_xml_df <- result_tmp_df
                    result_tmp_df <- data.frame(stringsAsFactors = FALSE)
                    flag = 1
                }
                else{
                    result_xml_df <- rbind(result_xml_df,result_tmp_df)
                    if(i != length(result_xml_list)){
                        result_tmp_df <- data.frame(stringsAsFactors = FALSE)
                    }
                }
            }
        }
        pb$tick()
    }
    if(div > length(result_xml_list)){
        result_xml_df <- result_tmp_df
    }
    
    
    #결과값 저장
    if(count ==1){
        result_xml_df_0 <- result_xml_df
    }
    if(count ==2){
        result_xml_df_1 <- result_xml_df
    }
    
    
    
}
Sys.time()

# 클러스터 중지
parallel::stopCluster(myCluster)
#################################################################################
























