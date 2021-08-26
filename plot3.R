# Візуалізація 3

library (tidyverse)


merge_year2<-data.frame()


pgo_year<- summarise(group_by(pgo_raw, Year), 
                     QNT=sum(INDICM,REL,MED,EDU)/sum(ACC),
                     Type="провадження направлені до суду / обліковані провадження")
merge_year2<-pgo_year

court_year<- summarise(group_by(court_raw, Year), 
                       QNT=sum(CONVIC)/sum(CRTOT),
                       Type="засуджені особи / особи, щодо яких судом прийнято рішення")
merge_year2<-rbind(merge_year2,court_year)




mean_y2<-summarise(group_by(merge_year2, Type), 
                   mean_v=mean(QNT)*100)

gr2<-ggplot(merge_year2,aes(x=factor(Year), y=QNT*100, fill=Type))
gr2+geom_histogram(stat = "identity",show.legend = FALSE)+
  geom_hline(data = mean_y2, color="chartreuse", show.legend = FALSE,
             aes(yintercept=mean_v,
                 size=1,
                 alpha=1/2))+
  geom_text(data=mean_y2,aes(x=8,
            y=mean_v,
            label=paste(round (mean_v,1),"%")))+facet_grid(.~Type)+
labs(y="%",
     title = 
"Частки обвинувальних вироків та направлених до суду проваджень")+
  theme (axis.title.x=element_blank())


mysave (vis)
vis<-vis+1
