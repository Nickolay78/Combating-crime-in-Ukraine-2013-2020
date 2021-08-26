# Візуалізації 4-6


library (tidyverse)


to_court<-summarise (group_by (pgo_raw, Year, Chapter),
                     ChIn=as.integer(Chapter),
                     INDICM=sum(INDICM),
                     REL=sum(REL),
                     MED=sum(MED),
                     EDU=sum(EDU),
                     OTHER=sum (REL,MED,EDU),
                     RATIO=(1-sum(INDICM)/sum(INDICM,REL,MED,EDU)))

data3_1<-select (to_court, Year, ChIn, RATIO)


in_court<-summarise (group_by (court_raw,Year, Chapter),
                     ChIn=as.integer(Chapter),
                     RATIO=(1-sum(CONVIC)/sum(CRTOT)),
                     Type="частка незасуджених (ДСА)")
data3_1$Type<-"частка клопотань серед направлених матеріалів (ОГП)"
in_court<-select (in_court, Year, ChIn, RATIO,Type)
data3_1_1<-rbind (data3_1,in_court)

total<-summarise (group_by(data3_1_1, Year,Type), S=mean(RATIO, na.rm=TRUE))
total$Type[total$Type=="частка клопотань серед направлених матеріалів (ОГП)"]<-"частка матеріалів, що не містять обвинувальних актів, серед всіх матеріалів, надісланих до суду"
total$Type[total$Type=="частка незасуджених (ДСА)"]<-"частка осіб, щодо яких прийнято рішення не пов'язане з засудженням, у загальній кількості осіб щодо яких судами прийнято рішення"




totgr<-ggplot(total, aes(x=Year, y=S*100, label=round(S*100,1)))
totgr+geom_bar (aes(fill=Type),stat="identity",position="dodge")+
  geom_label()+
  labs(y="%",
       title = "Частка матеріалів, що надіслані до суду та не містять обвинувального акту.
Частка судових рішень, не пов'язаних із засудженням")+
  theme (legend.title = element_blank(), legend.position="bottom", 
         legend.direction = "vertical", axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),axis.title.x =element_blank() )

mysave (vis)
vis<-vis+1



grtc<-ggplot(data3_1, aes(
  x=ChIn, y=RATIO*100))
grtc+geom_point()+ geom_line(stat="identity")+ 
  facet_grid(Year~.)+
  labs(x="Розділ Особливої частини КК",y="%",
       title = 
"Частка клопоптань про звільнення від кримінальної відповідальності, клопотань 
про застосування примусових заходів медичного або виховного характеру
серед направлених до суду матеріалів (дані ОГП)")

mysave (vis)
vis<-vis+1




grtc<-ggplot(data3_1_1, aes(
  x=ChIn, y=RATIO*100))
grtc+geom_point()+ geom_line(stat="identity")+ 
  facet_grid(Year~Type)+
  labs(x="Розділ Особливої частини КК",
       title = 
"Частка необвинувальних рішень серед всіх видів судових рішень (дані ДСА)
Частка клопоптань про звільнення від кримінальної відповідальності та про застосування
примусових або виховних заходів серед направлених до суду матеріалів (дані ОГП)")+
  theme (axis.title.y=element_blank())

mysave (vis)
vis<-vis+1





