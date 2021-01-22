
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
data2 <- str_replace_all(data2, ".*����: �̼�����.*" , " " )

data3 <- extractNoun(data2)
head(data3,5)

data4 <- unlist(data3)
data5 <- gsub("\\^", "", data4) 
data5 <- gsub("Q[0-9]", "", data5)
data5 <- gsub("����", "", data5)
data5 <- gsub("����", "", data5)
data5 <- gsub(".*��[���].*","",data5)
data5 <- gsub("�Ŀ�[��]?","",data5)
data5 <- gsub("ȮȮ","ȭ��",data5)
data5 <- gsub(".*�ذ��.*","�ذ���",data5)
data5 <- gsub(".*ȣ����.*","ȣ����",data5)
data5 <- gsub(".*ȣ��[^��]*","ȣ��",data5)
data5 <- gsub(paste(c("����","����"),collapse='|'), "", data5) 
data5 <- gsub(paste(c("[����]����[��]?","[����]��[��]?","[����]��[���]?","[����][�������][���ǽô°����ϸ���]?[��ε�]?"),collapse='|'), "", data5)
data5 <- gsub(paste(c("[����]����[��]?","[����]��[��]?","[����]��[���]?","[����][�������][���ǽô°����ϸ���]?[��ε�]?"),collapse='|'), "", data5)
data5 <- gsub("����", "������", data5)
data5 <- gsub(paste(c("�̼�","�̼�����"),collapse='|'),"�̼�����", data5)
data5 <- gsub(paste(c("�ʹ̼�","�ʹ̼�����"),collapse='|'),"�ʹ̼�����", data5)
data5 <- gsub(".*����.*", "���н�", data5)
data5 <- gsub(".*�߱�.*", "�߱�", data5)
data5 <- gsub(paste(c("��ȣ", "ȣ��","��ȣ��"),collapse='|'), "��ȣ��", data5)
data5 <- gsub(".*����.*", "����", data5)
data5 <- gsub(".*�Ǻ�.*", "�Ǻ�", data5)
data5 <- gsub(".*�ḷ��.*", "�ḷ��", data5)
data5 <- gsub("[^��]*�̼�����.*", "�̼�����", data5)
data5 <- gsub(".*�ʹ̼�����.*", "�ʹ̼�����", data5)
data5 <- gsub(".*������ȯ.*", "������ȯ", data5)
data5 <- gsub(".*ȣ����.*", "ȣ����", data5)
data5 <- gsub(".*����Ŀ��.*", "����Ŀ��", data5)
data5 <- gsub(paste(c(".*����.*",".*����.*",".*����.*"),collapse='|'), "����", data5)
data5 <- gsub(".*�ĺ��.*", "�ĺ��", data5)
data5 <- gsub(paste(c(".*ȿ��.*",".*ȿ��.*",".*ȿ��.*"),collapse='|'), "ȿ��", data5)
data5 <- gsub(paste(c(".*����.*",".*����.*"),collapse='|'), "����", data5)
data5 <- gsub("KF80", "KF8.0", data5)
data5 <- gsub("KF94", "KF9.4", data5)
data5 <- gsub("KF99", "K9F", data5)
data5 <- gsub("[^[:alpha:]][0-9]+", "", data5)
data5 <- gsub("KF8", "KF80", data5)
data5 <- gsub("KF9", "KF94", data5)
data5 <- gsub("K9F", "KF99", data5)
data5 <- gsub("�̻���", "�̻���", data5)
data5 <- gsub(".*������.*", "��������", data5)
data5 <- gsub(paste(c(".*����.*",".*���Ǳⱸ.*",".*���躸�Ǳⱸ.*",".*[Ww][Hh][Oo].*"),collapse='|'), "WHO", data5)
data5 <- gsub(".*Ȳ��.*", "Ȳ��", data5)
data5 <- gsub(".*ȯ��.*", "ȯ��", data5)
data5 <- gsub(".*ȭ��.*", "ȭ��", data5)
data5 <- gsub(".*ź��.*", "ź��", data5)
data5 <- gsub(".*ź��.*", "ź��", data5)
data5 <- gsub(".*����.*", "����", data5)
data5 <- gsub(".*����.*", "����", data5)
data5 <- gsub(".*������.*", "������", data5)
data5 <- gsub(".*���ް�.*", "���ް�", data5)
data5 <- gsub(".*����.*", "����", data5)
data5 <- gsub(".*����.*", "����", data5)
data5 <- gsub(".*����.*", "����", data5)
data5 <- gsub(".*�����ڸ���.*", "�����ڸ���", data5)
data5 <- gsub(".*PM[^2]*", "PM10", data5)
data5 <- gsub(".*PM102.*", "PM2.5", data5)
data5 <- gsub("��+", "", data5)
data5 <- gsub(".*���.*", "��Ⱑ��", data5)
data5 <- gsub(".*������.*", "������", data5)
data5 <- gsub(".*mm.*", "mm", data5)
data5 <- gsub(".*�մ�.*","",data5)
data5 <- gsub(paste(c(".*��ȸ.*",".*�׿�.*",".*��ȸ�׿�.*"),collapse='|'),"��ȸ�׿�",data5)

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




