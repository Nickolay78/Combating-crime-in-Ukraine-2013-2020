#Скрипт для отримання та очищення статистичних даних Державної судової адміністрації

library (xlsx)
library (stringr)
library(readxl)

colT13_17<-"numeric text text text"
for (ct in 1:61) colT13_17<-paste (colT13_17,"numeric", sep = " ")

colT18_20<-"numeric text text text"
for (ct in 1:44) colT18_20<-paste (colT18_20,"numeric", sep = " ")

TNames13_17<-"Year Article Part Chapter CRTOT CONVIC ACQUIT CMED CCLOSE CNOTCR CONFES RECONC CIRCUMS SPONS CEDU AMNESTY DEATH COTHER LIFEIMP IMP IMP1 IMP12 IMP23 IMP35 IMP510 IMP1015 IMP1525 RESTOL DISBAT ARREST CORRW SRVRSTR PUBLW FINE DEPR PNOTHR PROB RELAMN RELOTHR ADDFINE ADDRANK ADDDEPR ADDCONF SETCR SETSEN x2LIFEIMP x2IMP x2IMP1 x2IMP12 x2IMP23 x2IMP35 x2IMP510 x2IMP1015 x2IMP1525 x2RESTOL x2DISBAT x2ARREST x2CORRW x2SRVRSTR x2PUBLW x2FINE x2DEPR x2PNOTHR ART69 COMTREAT"
TNames18_20<-"Year Article Part Chapter CRTOT CONVIC ACQUIT CMED CCLOSE SAMEVERD CONFES RECONC CIRCUMS SPONS DENOPR AMNESTY DEATH COTHER x3LIFEIMP x3IMP x3IMP1 x3IMP12 x3IMP23 x3IMP35 x3IMP510 x3IMP1015 x3IMP1525 x3RESTOL x3DISBAT x3ARREST x3CORRW x3SRVRSTR x3PUBLW x3FINE x3DEPR x3PNOTHR PROB RELAMN RELOTHR ADDFINE ADDRANK ADDDEPR ADDCONF SETCR SETSEN ART69 COMTREAT SPECCNF"


chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")   
ymin<-2013
ymax<-2020


special_court<-data.frame (
  Year=ymin:ymax,
  http=c("https://court.gov.ua/userfiles/6_2013.xls",
         "https://court.gov.ua/userfiles/6_2014.xls",
         "https://court.gov.ua/userfiles/6_2015.xls",
         "https://court.gov.ua/userfiles/file/DSA/DSA_2017_all_docs/FEBRUARY_17/statistika_16/Copi_6_2016.xls",
         "https://court.gov.ua/userfiles/file/DSA/2018_DSA_docs/6_2017.xlsx",
         "https://court.gov.ua/userfiles/media/media/6_2018.xlsx",
         "https://court.gov.ua/userfiles/media/dsa_pres_slujba_2019/dsa_pres_slujba_2020/6_2019.xlsx",
          "https://court.gov.ua/userfiles/media/new_folder_for_uploads/main_site/6_2020.xlsx"),
  Trange=c("A14:BM889",
           "A14:BM909",
           "A14:BM962",
           "A14:BM965",
           "A14:BM967",
           "A13:AV971",
           "A13:AV990",
           "A13:AV1013"),
  Trange2=c("A917:BM917",
           "A969:BM969",
           "A963:BM963",
           "A1051:BM1051",
           "A968:BM968",
           "A1046:AV1046",
           "A991:AV991",
           "A1014:AV1014"),
  TNames=c(TNames13_17,
           TNames13_17,
           TNames13_17,
           TNames13_17,
           TNames13_17,
           TNames18_20,
           TNames18_20,
           TNames18_20),
  colTypes=c(colT13_17,
             colT13_17,
             colT13_17,
             colT13_17,
             colT13_17,
             colT18_20,
             colT18_20,
             colT18_20)
        )


 
#Завантаження статистичних звітів з офіційних джерел


for (yr in ymin:ymax) 
{
if (yr<2017) {ext<-".xls"} else {ext<-".xlsx"}
  
    filename<-paste("RAW_COURT/raw_court",as.character(yr),ext,sep="")
    if (!file.exists(filename))
      {
    fileURL <-special_court$http[special_court$Year==yr] 
    download.file(fileURL, filename, method = "curl")
      }
}


#Очищення даних, генерація CSV файлів

listcourt<-list.files(path="RAW_COURT/")

for (iy in 1:length(listcourt))
{

readxl<-paste("RAW_COURT/",listcourt[iy],sep="")
Year<-special_court$Year[iy]
names_y<-unlist(str_split(special_court$TNames[iy], pattern = " "))
colT<-unlist(str_split(special_court$colTypes[iy],pattern = " "))

  
court_y <- read_excel(readxl, sheet = "Форма 6", 
                      range = special_court$Trange[iy], col_names = FALSE, 
                      col_types = colT) 

court_y2 <- read_excel(readxl, sheet = "Форма 6", 
                      range = special_court$Trange2[iy], col_names = FALSE, 
                     col_types = colT) 

court_y<-rbind (court_y,court_y2)


                        
names(court_y)<-names_y
court_y$Year<-Year
court_y$Article[nrow(court_y)]<-"Criminal Code 1960"
court_y$Part[nrow(court_y)]<-0
court_y$Chapter[nrow(court_y)]<-0
ch<-1
for (i in 2:nrow(court_y))
{
  if (str_detect(court_y$Article[i],"ст.")&!str_detect(court_y$Article[i-1],"ст."))
  {ch<-ch+1
      } else
    
  {
    if ((str_detect(court_y$Article[i],"ч.")))
              {
      court_y$Article[i]<-sub ("ч.","",court_y$Article[i])
      court_y$Part[i]<-word (court_y$Article[i],-1)
              }
    
    if (i<nrow(court_y)) 
        {court_y$Article[i]<-paste ("Article ",word (court_y$Article[i],1))
    court_y$Chapter[i]<-chapter_ord[ch]}
    
  }
}

court_y<-subset(court_y,!str_detect(court_y$Article,"ст."))
court_y$Part<-as.numeric(court_y$Part)
court_y[is.na(court_y)]<-0



court_y$Part[nrow(court_y)]<-NA
court_y$Chapter[nrow(court_y)]<-NA

fileread<-paste("DATA_COURT/",as.character(Year),"Court.csv",sep="")
write.csv(court_y,file=fileread,row.names = FALSE,col.names = TRUE)
}


