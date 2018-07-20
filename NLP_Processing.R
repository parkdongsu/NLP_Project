#1.°ø¶õÃ³¸® -> /n /t, 2Ä­¶³¾îÁø°Í ¸ğµÎ ÇÑÄ­À¸·Î ¹Ù²ãÁÜ
#2.´ë¼Ò¹®ÀÚ ÅëÀÏ -> ÀÇ´ë»ıºĞ°ú »óÀÇ ÈÄ ÀÛ¾÷
#3.¼ıÀÚÇ¥Çö -> ¼ıÀÚµéÀ» ¸ğµÎ _number_·Î ¹Ù²ã ÀÎ½Ä or ¼ıÀÚ ±×´ë·Î ÀÎ½Ä or ¼ıÀÚ¿Í µÚÀÇ ´Ü¾î¸¦ ÇÕÃÄ ÀúÀå ex) 3È¸, 3¹ø µî ¾î¶»°Ô ÇÒ °ÍÀÎÁö
#4.¹®ÀåºÎÈ£ ¹× Æ¯¼ö¹®ÀÚ Á¦°Å -> Æ¯ÀÌ ÄÉÀÌ½º¸¸ ºĞ·ùÇØ ÇÕÃÄÁà ÀúÀå.  ¸¸¾à ±×·¸´Ù¸é ÀÇ´ë»ıºĞ°ú ÀÛ¾÷, ¾Æ´Ï¶ó¸é ¾î¶»°Ô ÇÒ °ÍÀÎÁö
#5.ºÒ¿ë´Ü¾î Á¦°Å ->·ÎÁ÷ »ı°¢ÇØ¼­ Àû¿ë ÇÑ±ÛÀº ÇÊ¿ä¾ø´Â Ç°»ç Á¦°Å / (¸í»ç´Â ¸í»ç·Î ³²±â°í ´Ù¸¥ Ç°»ç´Â ¶Ç ¾î¶»°Ô Ã³¸®ÇÒ°ÍÀÎÁö °í¹Î) 
#6.¾î±Ù µ¿ÀÏÈ­ Ã³¸® -> ÇÑ±ÛÀº Àû¿ëX,  ¿µ¾î´Â ¾î¶»°Ô ÇÒ°ÍÀÎÁö
#7.¿£±×·¥(n¹ø ¿¬ÀÌ¾î µîÀåÇÏ´Â ´Ü¾îµéÀÇ ¿¬¼â) ->ÇÏ³ªÀÇ ÀÇÇĞ¿ë¾î´Â ºÙ¿©¾ßÇÏ´ÂÁö µû·Î ÇÒ °ÍÀÎÁö ºÙÀÎ´Ù¸é ÀÇ´ë»ıºĞ°ú »óÀÇÈÄ ÀÛ¾÷ #¹æ¹ıÀº ÁöÁ¤µÈ ¿µ¾î¹®ÀÚ·Î
#º¯°æÈÄ ex) Unique_word1 ³ªÁß¿¡ POS_EXTRACTIONÇÔ¼ö ½ÇÇàÇÏ¸é ¿µ¾î´Ï±î ±×´ë·Î ³ª¿È ±× ÈÄ list¿¡¼­ Ã£¾Æ¼­ ´Ù½Ã ¹Ù²ãÁÜ 
#8.¿µ¾î Á¢µÎ»ç, Á¢¹Ì»ç Á¦°ÅÇØ¾ßÇÒÁö?  ¸Â´Ù¸é ±×°Í¸¸ ÇÏ¸é µÇ´ÂÁö # ÀÏ´Ü ³¡³ª°í ÇØ¾ßÇÔ ÁöÁ¤ ¿ë¾î¿Ü¿£ ´Ü¾î Á¤¸³ÈÄ ¾ÕµÚ·Î ¶¿Áö??


#»çÀü ÁØºñ»çÇ×########################################
#XML_Parsing_Pro7¿¡¼­ file=""¿¡ RDS ÆÄÀÏ °æ·Î¸¦ ½áÁÖ°í ½ÇÇàÇÑ ÈÄ ½ÇÇà ÇÒ °Í.
######################################################

# load packages
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


library(KoNLP)
#library(rJava)
library(topicmodels)
library(stringr)

#search###############################################
search_df <- result_xml_df[result_xml_df$`<MN>`=='Çöº´·Â',] # ÅÂ±×, °Ë»ö¾î ÁöÁ¤ ex) <MN>, '¾à¸í'
######################################################

tag ='<TD>' # NLP Ã³¸®ÇÏ°í ½ÍÀº tag ÀÔ·Â

#°Ë»ö ÈÄ tag°¡ NAÀÎ ÇàÀ» »èÁ¦
search_df[,tag][is.na(search_df[,tag])] <- ""

for (i in nrow(search_df):1){# µÚ¿¡¼­ºÎÅÍ »èÁ¦ÇØ ÇàÀÌ ¹Ğ·Á¼­ »èÁ¦ µÇÁö ¾Êµµ·Ï Ã³¸®ÇÔ.
    if(search_df[i,tag] == ""){
        search_df <- search_df[-i,]
    }
}

#POS ÃßÃâ ÇÔ¼ö
K_POS_EXTRACTION <- function(wordlist){
    #5.ºÒ¿ë ´Ü¾î Á¦°Å
    wordlist <- gsub('/F+','/CW+',wordlist)
    wordlist <- gsub('/NC+','/CW+',wordlist)
    
    pos_start <- as.vector(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]]) # Á¤±ÔÇ¥Çö½ÄÀ» ÅëÇØ °É·¯¼­ »ç¿ë 
    pos_length <- as.vector(attr(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]],'match.length'))
    
    pos_end <- pos_start+pos_length-5
    
    
    
    #ÇÕÄ¡µÇ ¼ø¼­´Â À¯ÁöÇØ¾ßÇÔ. -> ºñÈ¿À²
    
    # /ÀÇ À§Ä¡¸¦ ¾ò¾î¼­ µÚ¸¦ »èÁ¦ -> ºñÈ¿À²Àû
    
    
    word_data = rep(NA,length(pos_start))
    for(i in 1:length(pos_start)){
        word_data[i] <- substr(wordlist,pos_start[i],pos_end[i])
    }
    return(word_data)
}

#
NLP_PROCESSING <- function(xmldf){
    #4.Æ¯¼ö ¹®ÀÚ º¯°æ ¹× Á¦°Å
    xmldf <- gsub('&#x0D;', " ", xmldf) # ¶ç¾î¾²±â´Â ¸¶Áö¸·¿¡ ¹«Á¶°Ç ÇÏ³ª·Î ÅëÀÏ ÇØÁÖ´Â ºÎºĞÀÌ ÀÖÀ½.
    xmldf <- gsub('&lt;', " ", xmldf)
    xmldf <- gsub('&gt;', " ", xmldf)
    xmldf <- gsub('&amp;', " ", xmldf)
    xmldf <- gsub('&quot;', " ", xmldf)
    
    xmldf <- gsub("[~!@#$%^&*()]"," ", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    
    
    #2.´ë¼Ò¹®ÀÚ ÅëÀÏ(¼±ÅÃ°¡´ÉÀ¸·Î ¸¸µé °Í)
    #xmldf<- toupper(xmldf) # ´ë¹®ÀÚ 
    xmldf<- tolower(xmldf)# ¼Ò¹®ÀÚ
    
    #6.¾î±Ù µ¿ÀÏÈ­ Ã³¸®
    #xmldf <- gsub(' are ',' be ',xmldf)
    #xmldf <- gsub(' are ',' be ',xmldf)
    #xmldf <- gsub(' is ',' be ',xmldf)
    
    #¶Ç´Â Áß¿äÇÏÁö¾ÊÀº ´Ü¾îµéÀ» »©°í ½Í´Ù
    #xmldf <- gsub('and|of|as|in',"",xmldf)# ¼±Ã³¸® ÈÄ °ø¶õ Ã³¸® ÇÒ °Í.
    
    #7.¿£±×·¥
    #xmldf <- sub('[^A-Za-z °¡-ÆR]*graphic[ _-]variant[^A-Za-z °¡-ÆR]*','graphicvariant',xmldf) # KoNLP Ã³¸®½Ã ¿µ¾î¹®ÀåÀº ±×´ë·Î ³ª¿À±â ¶§¹®¿¡ ÇÑ´Ü¾î·Î ¹Ù·Î ³ª¿È.
    
    
    #1.°ø¶õÃ³¸®
    xmldf <- str_replace_all(xmldf,"[[:space:]]{1,}"," ")
    
    #¹Ù²Ü Çü½Ä
    xmldf <- paste(xmldf,'.',sep = '')#¹®ÀåÀÌ ¾Æ´Ñ °æ¿ì ³¯Â¥, ¾à¸í µî Áß¿äÇÑ Á¤º¸°¡ Àß¸®´Â °æ¿ì°¡ ÀÖÀ½. ex) 12-02-02 ´Ü¾î ÇÏ³ªÀÖÀ¸¸é 12-02-0 °ú 2·Î ³ª´¸.
    #¾îÂ÷ÇÇ ¸¶Áö¸· . Ãß°¡ÇØÁÖ¸é µû·Î ³ª´²Áö°í Á¤±ÔÇ¥Çö½Ä¿¡¼­ °Å¸£Áö ¾ÊÀ¸´Ï ±¦ÂúÀ»°Å¶ó°í »ı°¢ÇÔ.
    
}

########MAIN CODE##############################################################
#NLP ¿ë df ¸¸µé±â
xml_df <- search_df[tag]

#NLP_PROCESSING ÇÔ¼ö¸¦ ÅëÇÑ ÃÊ±â ¼³Á¤
word_df <- apply(xml_df,2,NLP_PROCESSING)

#ÇüÅÂ¼Ò ºĞ¼®ÈÄ ÇÕÄ¡±â
result_word_list <- list()
for (i in 1:nrow(word_df)){
    word_list<-SimplePos22(word_df[i])
    if(length(word_list) ==1){
        word_vector <- word_list[[1]]
        result_word_list[[i]] <- c(word_vector)
    } 
    else{
        word_vector <- word_list[[1]]
        for (k in 2:length(word_list)){
            word_vector <- paste(word_vector,'+',word_list[[k]],sep = '')
        }
        result_word_list[[i]] <- c(word_vector)
    }
}

#¿øÇÏ´Â Ç°»ç ÃßÃâ 
pos_word <- lapply(result_word_list,K_POS_EXTRACTION)


#´Ü¾î ¸®½ºÆ® -> ÁıÇÕ 
unique_pos_vector <- unique(unlist(pos_word))
unique_pos_vector
#""ÀÏ°æ¿ì Áö¿öÁà¾ßÇÔ 
for (NULLValue in 1:(length(unique_pos_vector))){
    if (unique_pos_vector[NULLValue] == ""){
        unique_pos_vector = unique_pos_vector[-NULLValue]
        break
    }
}


#ÇÑ±Û Ç°»ç ÃßÃâµÚ ¿µ¾î ÁøÇà
#8.¿µ¾îÀÇ °æ¿ì´Â ÁöÁ¤ ¿ë¾î¿Ü¿£ ´Ü¾î Á¤¸³ÈÄ ¾ÕµÚ·Î ¶¿Áö??
###ÇÑ±Û Ç°»ç ÃßÃâÈÄ unique_pos_vector ¼³Á¤ÈÄ¿¡ ±× ´Ü¾îµé·Î ÃßÃâÇÏ´Â°Ô ¾î¶³±î

#library('NLP')
#library('openNLP')
#library('tm')


#unique_pos_vector <- gsub("[°¡-ÆR]+",NA,unique_pos_vector)
#unique_pos_vector
#¶ç¾î¾²±â·Î ±¸ºĞÇØ¼­ ÇÕÄ¡ÀÚ
#unique_pos_word <- unique_pos_vector[1]
#for (i in 2:length(unique_pos_vector)){
#    unique_pos_word <- paste(unique_pos_word,unique_pos_vector[i], sep = " ")
#}
#unique_pos_word

#sent <- annotate(unique_pos_word,Maxent_Sent_Token_Annotator())
#word <- annotate(unique_pos_word,Maxent_Word_Token_Annotator(),sent)
#PosTag <- annotate(unique_pos_word,Maxent_POS_Tag_Annotator(),word)
#PosTag

########MAIN CODE##############################################################

##Ãß°¡ÀûÀÎ ÅäÇÈ¸ğÇü ÀÛ¾÷###############################

#pos_word°¡ ºñ¾îÀÖ´Â ºÎºĞÀº pos_word¿Í search_df¿¡¼­µµ »èÁ¦
for (i in length(pos_word):1){
    if (pos_word[[i]][1] == ""){
        pos_word <- pos_word[-i]
        search_df <- search_df[-i,]
    }
}


#DTM Matrix ¸¸µé±â
DTM <- matrix(nrow=nrow(search_df),ncol=length(unique_pos_vector),data=0)
#ROW,COL ÀÌ¸§ ÁÖ±â
NOTE_ID_list <- c(search_df['NOTE_ID'])
rownames(DTM) <- NOTE_ID_list[['NOTE_ID']]
colnames(DTM) <- unique_pos_vector
dim(DTM)

#DTM¿¡ ¹®¼­¿¡ ´Ü¾î Ä«¿îÆ® ÇÒ´ç
for(i in 1:nrow(DTM)){
    for(k in pos_word[[i]]){
        DTM[i,k] <- DTM[i,k] + 1
    }
}

#Topic ¿¹Ãø ¿¹Á¦
lda.out <- LDA(DTM,control = list(seed=11),k=5)
dim(lda.out@gamma)
dim(lda.out@beta)
terms(lda.out,12)

posterior_lda <- posterior(lda.out)
round(posterior_lda$topics,3)
########################################################

