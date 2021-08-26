#Скрипт для завантаження оброблених даних до робочих баз


library (tidyverse)
library (scales)
chapter_levels<-c("I","II","III","IV","V","VI","VII","VIII","IX","X",
                  "XI","XII","XIII","XIV","XV","XVI","XVII","XVIII","XIX","XX")
#Визначення змінних для характеристики фізичного розміру візуалізацій
rozmir=data.frame(
  first=c(8,9.23),
  A5=c(5.83, 8.27))
dpi.a<-900
graph.k<-1
graphw<-graph.k*rozmir$A5[2]
graphh<-graph.k*rozmir$A5[1]
#Локальна функція запису файлів візуалізацій
mysave<-function (name)
{ggsave (paste("visualization-",name,".png",sep=""), width = graphw, height = graphh, path="GRAPH/",dpi=dpi.a)}

vis<-1
varcount<-0

court_raw<-data.frame()
y=1
  list1<-list.files(path="DATA_COURT/") 
for (y in 1:length (list1))
{
  rr<-read.csv(paste("DATA_COURT/",list1[y],sep=""))
  varcount<-varcount+nrow(rr)*ncol(rr)
    
    
    rr_t<-select  (rr,Year:CONVIC,ACQUIT,CMED,CCLOSE,
               CONFES,RECONC,CIRCUMS,SPONS,AMNESTY,DEATH,COTHER,
               PROB,RELAMN,RELOTHR)
#court verdict
  rr_t$SAMEVERD<-NA
  rr_t$CNOTCR<-NA
  rr_t$DENOPR<-NA
  rr_t$CEDU<-NA
  if (rr$Year[1]<2018)
  {rr_t$CNOTCR<-rr$CNOTCR
  rr_t$CEDU<-rr$CEDU} 
  else 
  {rr_t$SAMEVERD<-rr$SAMEVERD
    rr_t$DENOPR<-rr$DENOPR}
 #Sentence
  if (rr$Year[1]<2018)
  {
  rr_t$LIFEIMP<-rr$LIFEIMP
  rr_t$IMP<-rr$IMP
  rr_t$IMP1<-rr$IMP1
  rr_t$IMP12<-rr$IMP12
  rr_t$IMP23<-rr$IMP23
  rr_t$IMP35<-rr$IMP35
  rr_t$IMP510<-rr$IMP510
  rr_t$IMP1015<-rr$IMP1015
  rr_t$IMP1525<-rr$IMP1525
  rr_t$RESTOL<-rr$RESTOL
  rr_t$DISBAT<-rr$DISBAT
  rr_t$ARREST<-rr$ARREST
  rr_t$CORRW<-rr$CORRW
  rr_t$SRVRSTR<-rr$SRVRSTR
  rr_t$PUBLW<-rr$PUBLW
  rr_t$FINE<-rr$FINE
  rr_t$DEPR<-rr$DEPR}
  else
  {
    rr_t$LIFEIMP<-rr$x3LIFEIMP
    rr_t$IMP<-rr$x3IMP
    rr_t$IMP1<-rr$x3IMP1
    rr_t$IMP12<-rr$x3IMP12
    rr_t$IMP23<-rr$x3IMP23
    rr_t$IMP35<-rr$x3IMP35
    rr_t$IMP510<-rr$x3IMP510
    rr_t$IMP1015<-rr$x3IMP1015
    rr_t$IMP1525<-rr$x3IMP1525
    rr_t$RESTOL<-rr$x3RESTOL
    rr_t$DISBAT<-rr$x3DISBAT
    rr_t$ARREST<-rr$x3ARREST
    rr_t$CORRW<-rr$x3CORRW
    rr_t$SRVRSTR<-rr$x3SRVRSTR
    rr_t$PUBLW<-rr$x3PUBLW
    rr_t$FINE<-rr$x3FINE
    rr_t$DEPR<-rr$x3DEPR}
  
  rr<-rr_t
  court_raw<-rbind(court_raw,rr)} 

  
pgo_raw<-data.frame()
  y=1
  list1<-list.files(path="DATA_PGO/") 
  for (y in 1:length (list1))
  {
    rr<-read.csv(paste("DATA_PGO/",list1[y],sep=""))
    varcount<-varcount+nrow(rr)*ncol(rr)
    rr<-select  (rr,Year:SUSP,INDICM,REL,MED,EDU)
    
    
    pgo_raw<-rbind(pgo_raw,rr)} 
  
  pgo_raw$Article[str_detect(pgo_raw$Article,".252")]<-"Article 252"
  pgo_raw$Article[str_detect(pgo_raw$Article,"cle 32-2")]<-"Article 332-2"
  pgo_raw$Chapter<-as.roman(pgo_raw$Chapter)
  court_raw$Chapter<-as.roman(court_raw$Chapter)
  
  
  
  