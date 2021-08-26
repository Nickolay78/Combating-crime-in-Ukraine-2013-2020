#Скрипт для отримання та очищення статистичних даних Офісу Герального Прокурора


library (xlsx)
library (stringr)
library(readxl)



chapter_ord<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
               "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")   
ymin<-2013
ymax<-2020


special_pgo<-data.frame (
  Year=ymin:ymax,
  http=c("https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=185282",
         "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=198199",
         "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=198191",
         "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=200945",
         "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=203952",
         "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=205797",
         "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=208205",
          "https://www.gp.gov.ua/ua/stat_n_st?_m=fslib&_t=fsfile&_c=download&file_id=210855"),
  Trange=c("A78:X770",
           "A78:X784",
           "A78:X786",
           "A78:X784",
           "A84:Y786",
           "A84:Y786",
           "A84:Y789",
           "A224:AB1090"),
  TNames=c("Year Article Chapter ACC SUSP STPIL STPLOC STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPSDR STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPSDR STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPSDR STPFOR INDICM REL MED EDU RECID GROUP INTOX JUVEN CLOSE CL1-2 NODEC",
           "Year Article Chapter ACC SUSP STPIL STPLOC STPSDR STPFOR TOCRT INDICM REL MED EDU CL3-1 RECID GROUP INTOX JUVEN CLOSE CL1-2 CL10 NODEC"),
  colTypes=c(
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric",
    "text text text skip skip skip skip skip numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric numeric"
      )
        )


 
#Завантаження звітів з офіційного сайту


for (yr in ymin:ymax) 

{
  filename<-paste("RAW_PGO/raw_pgo",as.character(yr),".xlsx",sep="")
    if (!file.exists(filename)){
    fileURL <-special_pgo$http[special_pgo$Year==yr] 
    download.file(fileURL, filename, method = "curl")
    
  }
  
  
}

#Очищення даних

listpgo<-list.files(path="RAW_PGO/")

for (iy in 1:length(listpgo))
{

readxl<-paste("RAW_PGO/",listpgo[iy],sep="")
Year<-special_pgo$Year[iy]
names_y<-unlist(str_split(special_pgo$TNames[iy], pattern = " "))
colT<-unlist(str_split(special_pgo$colTypes[iy],pattern = " "))
  
pgo_y <- read_excel(readxl, sheet = "1", 
                      range = special_pgo$Trange[iy], col_names = FALSE, 
                      col_types = colT) 
                        
names(pgo_y)<-names_y
pgo_y$Article[nrow(pgo_y)]<-"Інші"
pgo_y<-subset(pgo_y,!is.na(pgo_y$Article))
pgo_y<-subset(pgo_y,!(pgo_y$Article=="з них"|
                pgo_y$Article=="у т.ч."|pgo_y$Article=="у   т. ч."|pgo_y$Article=="у тому числі" ))
pgo_y<-subset(pgo_y,!str_detect(pgo_y$Article,"з рядка"))

pgo_y$Year<-Year

ch<-1
for (i in 1:nrow(pgo_y))
{
  if (pgo_y$Article[i]=="Інші"|pgo_y$Article[i]=="Іінші"|pgo_y$Article[i]=="інші")
  {
    pgo_y$Article[i]<-"Others"
    pgo_y$Chapter[i]<-chapter_ord[ch]
    ch<-ch+1
  } else

      {
    pgo_y$Article[i]<-paste ("Article ",word (pgo_y$Article[i],-1))
    pgo_y$Chapter[i]<-chapter_ord[ch]
    
      }
  if (str_detect(pgo_y$Article[i], "ст.")) 
  {pgo_y$Article[i]<-sub ("ст.","",pgo_y$Article[i])
  pgo_y$Article[i]<-sub (" ","",pgo_y$Article[i])
  }  
  
  }


fileread<-paste("DATA_PGO/PGO",as.character(Year),".csv",sep="")
write.csv(pgo_y,file=fileread,row.names = FALSE,col.names = TRUE)
}



