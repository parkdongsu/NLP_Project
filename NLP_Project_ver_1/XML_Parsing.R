#���� �غ����########################################
#PatientLevelPrediction.R ���� �� ������ ��.
######################################################
library(progress)

#XML_parser -> ����ǥ������ ���� �Ľ� ����

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

#diag_Processer(�� �Ʒ����� �±��� ������ �̰� ù �±׺��� ���� ù�±׷� ������.)
#ex) EMR ���   ���    ##��
#    EMR ���ܸ� ���ܸ�  ����

#�Ľ��ϸ� �� ����� ?? ���ܸ��� ??�� �����ϱ� ���� �Լ�.
DIAG_PROCESSING <- function(diag_list){
    #ù��° > �� �������� �±׸� ����
    tag_vector  <- as.vector(regexpr('>',diag_list))
    text_vector <- as.vector(regexpr('</',diag_list))
    
    #tag�� text�� ������ �����
    tag_data_vector <- substr(diag_list,1,tag_vector)
    text_data_vector <- substr(diag_list,tag_vector+1,text_vector-1)
    
    #ù��° �±׺��� ���� ù �±ױ��� ���� ���� �Ҵ�
    first_tag_vector <- as.vector(regexpr(tag_data_vector[1],tag_data_vector))# ù��° ������ ������ 1�� ��ȯ
    
    #��� ������ df ����
    final_xml_df <- data.frame(stringsAsFactors = FALSE) 
    
    #1�� ��ġ�� ã�� ��ġ ���� �־���
    data =c()
    for (i in 1:length(first_tag_vector)){
        if (first_tag_vector[i] == 1){
            data[i] <- i
        }
    }
    #NA ����
    data <- data[!is.na(data)]
    
    #������ ù��° �±��� ������ ��
    data[length(data)+1] <- (length(first_tag_vector)+1)
    
    #tag �ߺ� ����
    tag_data_set <- unique(tag_data_vector)
    
    #tag�� ���̸� ����
    df <- data.frame(stringsAsFactors = FALSE)
    for (i in tag_data_set){
        df[i] <- character(0)
    }
    #rbind�� df Setting
    tmp_df <- df
    xml_df<- df
    
    #df�� �� �ֱ�
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

# �ھ� ���� ȹ��
numCores <- parallel::detectCores() - 1
# Ŭ������ �ʱ�ȭ
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
diag_note <- DatabaseConnector::dbGetQuery(conn = connection,statement = "SELECT TOP 10000 * FROM DBO.NOTE JOIN COHORT ON NOTE.person_id = COHORT.subject_id AND NOTE.NOTE_DATE = COHORT.COHORT_START_DATE WHERE cohort_definition_id = 747 AND NOTE_TITLE = \'������\'") ;

#���� ���� �����ϴ� df���� merge �� ����###############################################
cohort_outCount_df <- merge(outcomeCount_df,diag_note,by = c("PERSON_ID","NOTE_DATE"))

#######################################################################################

#outCount ���� 0, 1 �� ���� dataFrame�� ���� 
out_value_0 <- cohort_outCount_df[cohort_outCount_df$outcomeCount == 0,]
out_value_1 <- cohort_outCount_df[cohort_outCount_df$outcomeCount == 1,]



for(count in 1:2){
    #RDS������ ���������������� ����
    #out_value_0 = X ���, out_value_1 = O�� ���###################
    if(count ==1){
        mediFrame <- out_value_0
    }
    if(count ==2){
        mediFrame <- out_value_1
    }
    ################################################################
    #mediFrame <- readRDS(file,refhook = NULL)
    
    #list ���� �� ID, TEXT ����
    medi_list = list(NOTE_ID=c(NA),NOTE_TEXT=c(NA))
    medi_list['NOTE_ID'] <- mediFrame['NOTE_ID']
    medi_list['NOTE_TEXT'] <- mediFrame['NOTE_TEXT']
    
    #XML Parsing�� ���� ROOT �߰�.
    medi_list[['NOTE_TEXT']] <- paste("<xml>",medi_list[['NOTE_TEXT']],"</xml>")
    #XML Parsing�� ���� ><���̿� �ƹ��͵� ���ų� �������ִٸ� ���� ó�����ش�.
    medi_list[['NOTE_TEXT']] <- gsub('>[ ]*<','>\n<',medi_list[['NOTE_TEXT']])
    
    #��� ������ df ����
    final_xml_df <- data.frame(stringsAsFactors = FALSE) 
    
    #XML �ļ��� ����(����ó��)
    diagnosis_list <- parallel::parLapply(cl = myCluster, X = medi_list[['NOTE_TEXT']], fun = XML_PARSING)
    
    #���ܼ� �ϳ��� DataFrame�� list�� ����(����ó��)
    result_xml_list <- parallel::parLapply(cl = myCluster, X = diagnosis_list, fun = DIAG_PROCESSING)
    
    #�Ѱ��� ���ܼ��� NOTE_ID ����
    for (i in 1:length(result_xml_list)){
        result_xml_list[[i]][,'NOTE_ID'] <- medi_list[['NOTE_ID']][i]
    }
    
    
    
    #dataFrame�� ����� �����ؼ� �־��� ��� �׸��� NA�� �κ��� �ƿ� dataframe�� �߰����� ����.
    #rbind �ϴ� �ð��� �����ɷ� div���� ���� �ѹ��� ������.
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
    
    
    #����� ����
    if(count ==1){
        result_xml_df_0 <- result_xml_df
    }
    if(count ==2){
        result_xml_df_1 <- result_xml_df
    }
    
    
    
}
Sys.time()

# Ŭ������ ����
parallel::stopCluster(myCluster)
#################################################################################
















