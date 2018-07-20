Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_171") 
library(rJava)

install.packages("devtools")
library(devtools)
install.packages("drat")
drat::addRepo("OHDSI")
install.packages("FeatureExtration")

install.packages("drat")
drat::addRepo("OHDSI")
install_github("ohdsi/FeatureExtraction", args = "--no-multiarch") 
install_github("ohdsi/PatientLevelPrediction", args = "--no-multiarch") 






library(SqlRender)
library(FeatureExtraction)
library(PatientLevelPrediction)

workingFolder<-"D:/Dongsu/R_code/sql"
setwd(workingFolder)


connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="###.###.###.###",
                                                              schema="#########.dbo",
                                                              user="#########",
                                                              password="########")
connection <- DatabaseConnector::connect(connectionDetails)
connectionDetails <-connectionDetails
connection <- connection

cdmDatabaseSchema<-"#######.dbo"
targetDatabaseSchema<-"#######.dbo"
targetCohortTable<-"#######"
targetCohortId <- 747
outcomeCohortId <- 748
cdmversion <- "5"

#CREATE TABLE & INPUT VALUE
#7일이상 입원한 환자들
#===========================================
# (T)
#===========================================

sql <- SqlRender::readSql(file.path(workingFolder,"all_admission.sql")) #local
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=targetCohortId
                            
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
DatabaseConnector::executeSql(connection,sql)

#모든 응급실입원 환자들
#===========================================
# (O)
#===========================================

sql <- SqlRender::readSql("ed_visit.sql") #local
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=outcomeCohortId
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
DatabaseConnector::executeSql(connection,sql)

#30일 이내에 재입원한 환자를 DF로 뽑아내는 함수들
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = FALSE,
                                                                useDemographicsAge = FALSE, useDemographicsAgeGroup = FALSE,
                                                                useDemographicsRace = FALSE, useDemographicsEthnicity = FALSE,
                                                                useConditionOccurrenceLongTerm = FALSE,
                                                                useDrugExposureLongTerm = FALSE,
                                                                useProcedureOccurrenceLongTerm = FALSE,
                                                                useMeasurementLongTerm = FALSE,
                                                                useObservationLongTerm = FALSE,
                                                                useDistinctConditionCountLongTerm =FALSE,
                                                                useVisitCountLongTerm = FALSE,
                                                                longTermStartDays = 0,
                                                                endDays = 30)

plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              #oracleTempSchema = oracleTempSchema,
                                              cohortDatabaseSchema = targetDatabaseSchema,
                                              cohortTable = "cohort",
                                              cohortId = targetCohortId,
                                              washoutPeriod = 0,
                                              covariateSettings = covariateSettings,
                                              outcomeDatabaseSchema = targetDatabaseSchema,
                                              outcomeTable = "cohort",
                                              outcomeIds = outcomeCohortId,
                                              cdmVersion = cdmversion)

population <- PatientLevelPrediction::createStudyPopulation(plpData, population = NULL, 
                                                            outcomeId=outcomeCohortId, 
                                                            binary = T,
                                                            includeAllOutcomes = T, 
                                                            firstExposureOnly = FALSE, 
                                                            washoutPeriod = 0,
                                                            removeSubjectsWithPriorOutcome = FALSE, 
                                                            priorOutcomeLookback = 99999,
                                                            requireTimeAtRisk = T, 
                                                            minTimeAtRisk = 1, 
                                                            riskWindowStart = 1,
                                                            addExposureDaysToStart = FALSE, 
                                                            riskWindowEnd = 30,
                                                            addExposureDaysToEnd = F)



outcomeCount_df <- data.frame(c(population["subjectId"],population["cohortStartDate"],population["outcomeCount"]))
colnames(outcomeCount_df) <-c("PERSON_ID","NOTE_DATE","outcomeCount")








