
setwd("Your\\Working\\Directory\\Duh")
getwd()
install.packages("wordcloud") 
install.packages("stringr")

library(stringr)
library(KoNLP)  
library(wordcloud)

useSejongDic()

data1 <- readLines("MICRODUST.txt",encoding="UTF-8")
head(data1,10)

data1 <- unique(data1)
length(data1)

data2 <- str_replace_all(data1, "[^[:alpha:][:digit:][:blank:]]" , " " )
data2 <- str_replace_all(data2, ".*제목: 미세먼지.*" , " " )

data3 <- extractNoun(data2)
head(data3,5)

data4 <- unlist(data3)
data5 <- gsub("\\^", "", data4) 
data5 <- gsub("Q[0-9]", "", data5)
data5 <- gsub("의해", "", data5)
data5 <- gsub("먼지", "", data5)
data5 <- gsub(".*힘[든들].*","",data5)
data5 <- gsub("후에[는]?","",data5)
data5 <- gsub("확확","화학",data5)
data5 <- gsub(".*해결방.*","해결방법",data5)
data5 <- gsub(".*호흡기.*","호흡기",data5)
data5 <- gsub(".*호흡[^기]*","호흡",data5)
data5 <- gsub(paste(c("인한","인해"),collapse='|'), "", data5) 
data5 <- gsub(paste(c("[있잇]습니[다]?","[있잇]는[데]?","[있잇]다[면고]?","[있잇][지어었으][만실시는고나니며면므]?[길로데]?"),collapse='|'), "", data5)
data5 <- gsub(paste(c("[했햇]습니[다]?","[했햇]는[데]?","[했햇]다[면고]?","[했햇][지어었으][만실시는고나니며면므]?[길로데]?"),collapse='|'), "", data5)
data5 <- gsub("아토", "아토피", data5)
data5 <- gsub(paste(c("미세","미세먼지"),collapse='|'),"미세먼지", data5)
data5 <- gsub(paste(c("초미세","초미세먼지"),collapse='|'),"초미세먼지", data5)
data5 <- gsub(".*위닉.*", "위닉스", data5)
data5 <- gsub(".*중국.*", "중국", data5)
data5 <- gsub(paste(c("산호", "호수","산호수"),collapse='|'), "산호수", data5)
data5 <- gsub(".*콧털.*", "콧털", data5)
data5 <- gsub(".*피부.*", "피부", data5)
data5 <- gsub(".*결막염.*", "결막염", data5)
data5 <- gsub("[^초]*미세먼지.*", "미세먼지", data5)
data5 <- gsub(".*초미세먼지.*", "초미세먼지", data5)
data5 <- gsub(".*중증질환.*", "중증질환", data5)
data5 <- gsub(".*호두유.*", "호두유", data5)
data5 <- gsub(".*히비스커스.*", "히비스커스", data5)
data5 <- gsub(paste(c(".*흡입.*",".*흡수.*",".*흡착.*"),collapse='|'), "흡입", data5)
data5 <- gsub(".*후비루.*", "후비루", data5)
data5 <- gsub(paste(c(".*효용.*",".*효율.*",".*효과.*"),collapse='|'), "효율", data5)
data5 <- gsub(paste(c(".*인정.*",".*인증.*"),collapse='|'), "인증", data5)
data5 <- gsub("KF80", "KF8.0", data5)
data5 <- gsub("KF94", "KF9.4", data5)
data5 <- gsub("KF99", "K9F", data5)
data5 <- gsub("[^[:alpha:]][0-9]+", "", data5)
data5 <- gsub("KF8", "KF80", data5)
data5 <- gsub("KF9", "KF94", data5)
data5 <- gsub("K9F", "KF99", data5)
data5 <- gsub("미새물", "미생물", data5)
data5 <- gsub(".*유근피.*", "참유근피", data5)
data5 <- gsub(paste(c(".*세계.*",".*보건기구.*",".*세계보건기구.*",".*[Ww][Hh][Oo].*"),collapse='|'), "WHO", data5)
data5 <- gsub(".*황사.*", "황사", data5)
data5 <- gsub(".*환경.*", "환경", data5)
data5 <- gsub(".*화학.*", "화학", data5)
data5 <- gsub(".*탄소.*", "탄소", data5)
data5 <- gsub(".*탄닌.*", "탄닌", data5)
data5 <- gsub(".*염증.*", "염증", data5)
data5 <- gsub(".*예방.*", "예방", data5)
data5 <- gsub(".*오미자.*", "오미자", data5)
data5 <- gsub(".*오메가.*", "오메가", data5)
data5 <- gsub(".*오염.*", "오염", data5)
data5 <- gsub(".*염증.*", "염증", data5)
data5 <- gsub(".*오존.*", "오존", data5)
data5 <- gsub(".*에어코리아.*", "에어코리아", data5)
data5 <- gsub(".*PM[^2]*", "PM10", data5)
data5 <- gsub(".*PM102.*", "PM2.5", data5)
data5 <- gsub("ㅠ+", "", data5)
data5 <- gsub(".*배기.*", "배기가스", data5)
data5 <- gsub(".*코점막.*", "코점막", data5)
data5 <- gsub(".*mm.*", "mm", data5)
data5 <- gsub(".*합니.*","",data5)
data5 <- gsub(paste(c(".*구회.*",".*죽염.*",".*구회죽염.*"),collapse='|'),"구회죽염",data5)

head(data5,5)

length(data5)

data6 <- Filter(function(y) {nchar(y) <= 10 & nchar(y) >1 },data5)
wordcount <- table(unlist(data6))
head(sort(wordcount, decreasing=TRUE),100)

txt <- readLines("MICRODUSTgsub.txt",encoding="UTF-8")
txt
cnt_txt <- length(txt)
cnt_txt
for ( i in 1:cnt_txt ){
      data6 <- gsub((txt[i]),"", data6)   
      }

data7 <- Filter(function(y) {nchar(y) <= 10 && nchar(y) >1 },data6)
wordcount2 <- table(data7)
head(sort(wordcount2, decreasing=TRUE),100)

par(oma=c(0.1,0.1,0.1,0.1))
palete <- brewer.pal(7,"Set1")
wordcloud(names(wordcount2),freq=wordcount2,scale=c(5,1),rot.per=0.25,min.freq=10,
random.order=FALSE,random.color=TRUE,colors=palete)

getwd( )
savePlot("MICRODUST_Wordcloud.png" , type="png")





