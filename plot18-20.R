# Візуалізації 18-20

library (tidyverse)


court_raw2<-data.frame()
y=1
list1<-list.files(path="DATA_COURT/") 
for (y in 1:length (list1))
{
  rr<-read.csv(paste("DATA_COURT/",list1[y],sep=""))
  rr<-select  (rr,Year:CONVIC,PROB,RELAMN,RELOTHR)
    court_raw2<-rbind(court_raw2,rr)} 

court_raw2$Chapter<-as.roman(court_raw2$Chapter)




court_year5<- summarise(group_by(court_raw2, Year, Chapter), 
                       PUNISH=sum(CONVIC)-sum(PROB)-sum(RELAMN)-sum(RELOTHR),
                       CONVIC=sum(CONVIC))
court_year5<-subset(court_year5,!is.na(Chapter))
court_year5$Chapter_n<-as.numeric(court_year5$Chapter)

court_year6<-summarise (group_by(court_year5,Chapter_n),PUNISH1=sum(PUNISH),
                        CONVIC1=sum(CONVIC),RATIO=(sum(PUNISH)/sum(CONVIC))*100)

gr5<-ggplot(court_year5, aes(x=Chapter_n, y=PUNISH/CONVIC*100))
gr5+geom_point()+ geom_line()+ facet_wrap(ncol=2,Year~., dir="v")+
labs(x="Розділ Особливої частини КК",y="частка репресивної форми (%)",
     title = "Використання репресивної форми реалізації кримінальної відповідальності")
  
mysave (vis)
vis<-vis+1

gr6<-ggplot(court_year5, aes(x=Year, y=PUNISH/CONVIC*100))
gr6+geom_point()+ geom_line()+ 
  facet_wrap(ncol=4,.~factor(as.character(Chapter),
                             levels=chapter_levels))+
  labs(y="частка репресивної форми (%)",
       title = 
"Використання репресивної форми реалізації кримінальної відповідальності 
за розділами Особливої частини КК")+
  theme (axis.title.x=element_blank())
mysave (vis)
vis<-vis+1

abl2<-(sum(court_year6$PUNISH1)/sum(court_year6$CONVIC1))*100
gr7<-ggplot(court_year6,aes(x=factor(Chapter_n),y=RATIO,fill=Chapter_n))
gr7+geom_histogram(stat="identity",show.legend = FALSE)+
  geom_hline(aes(yintercept=abl2,size=1,alpha=1/2,), color="chartreuse",
             show.legend = FALSE)+
  geom_text(aes(x=11.5,y=abl2,label=paste ("середній рівень ",round (abl2,1),"%")    ))+
  labs(x="Розділ Особливої частини КК", y="частка репресивної форми реалізації",
       title="Використання репресивної форми реалізації кримінальної відповідальності",
       subtitle = "середні значення за період з 2013 по 2020 рік")
mysave (vis)
vis<-vis+1