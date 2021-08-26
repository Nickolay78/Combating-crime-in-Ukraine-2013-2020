# Візуалізації 1-2

library (tidyverse)

  
merge_year<-data.frame()


pgo_year<- summarise(group_by(pgo_raw, Year), 
                         QNT=sum(ACC),
                         Type="обліковано проваджень")
merge_year<-pgo_year

pgo_year<- summarise(group_by(pgo_raw, Year), 
                     QNT=sum(INDICM,REL,MED,EDU),
                     Type="проваджень направлено до суду")
merge_year<-rbind(merge_year,pgo_year)

court_year<- summarise(group_by(court_raw, Year), 
                       QNT=sum(CONVIC),
                       Type="засуджено осіб")
merge_year<-rbind(merge_year,court_year)


gr<-ggplot(merge_year, 
           aes(x=factor(Year), y=QNT, 
           fill=factor(Type, levels=c("засуджено осіб",
                                      "проваджень направлено до суду",
                                      "обліковано проваджень") )))
gr+geom_histogram(stat="identity")+scale_fill_brewer(palette = "Set1")+
labs(title = "Обліковані правопорушення, провадження направлені до суду,
кількість засуджених")+
theme (axis.title=element_blank(), 
       legend.title = element_blank(), legend.position="bottom")

mysave (vis)
vis<-vis+1

require(scales)
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

mean_merge<-summarise(group_by(merge_year,Type),
                      mean_v=mean(QNT))

gr2<-ggplot (merge_year, aes(x=factor(Year), y=QNT,fill=Type))
gr2+geom_histogram(stat="identity",show.legend = FALSE)+
  scale_fill_brewer(palette = "Set1")+
  geom_hline(data = mean_merge, color="chartreuse", show.legend = FALSE,
             aes(yintercept=mean_v,
                 size=1,
                 alpha=1/2))+
  geom_text(data=mean_merge,aes(x=7,
            y=mean_v, label=round (mean_v)))+
    facet_grid(.~factor(Type,levels = c("обліковано проваджень",
                                      "проваджень направлено до суду",
                                      "засуджено осіб")))+
  labs(title = 
"Обліковані правопорушення, провадження направлені до суду,кількість засуджених")+
  theme(axis.text=element_text(), axis.title=element_blank())+
  scale_y_continuous(labels = point)   


mysave (vis)
vis<-vis+1


