# Візуалізації 10-17

library (tidyverse)


merge_year3<-data.frame()

pgo_year<- summarise(group_by(pgo_raw, Year, Chapter), 
                     QNT=sum(ACC),
                     Type="обліковані провадження")
merge_year3<-pgo_year

court_year<- summarise(group_by(court_raw, Year, Chapter), 
                       QNT=sum(CONVIC),
                       Type="засуджено осіб")


court_year_s<- summarise(group_by(court_raw, Year, Chapter), 
                       QNT1=sum(CONVIC),
                       Type1="засуджено осіб")
pgo_year_ind<-summarise(group_by(pgo_raw,Year, Chapter),
                       QNT=sum(INDICM))
#Data for convicted/ registed
merge_year_s<-merge(pgo_year,court_year_s)
merge_year_s$ratio<-(merge_year_s$QNT1/merge_year_s$QNT)*100

#Data for convicted/indictment
merge_year_s1<-merge(pgo_year_ind,court_year_s)
merge_year_s1$ratio<-(merge_year_s1$QNT1/merge_year_s1$QNT)*100



merge_year3<-rbind(merge_year3,court_year)
merge_year3<-subset(merge_year3,!is.na(Chapter))
merge_year3$Chapter_n<-as.numeric(merge_year3$Chapter)

gr3<-ggplot(merge_year3, aes(
  x=Chapter_n, y=log(QNT)))
gr3+geom_point()+ geom_line(stat="identity")+ 
  facet_grid(Year~Type)+
    labs(x="Розділ Особливої частини КК",y="логарифм абсолютного значення",
     title = 
"Розподіл облікованих проваджень та кількості засуджених 
за розділами Особливої частини КК")

mysave (vis)
vis<-vis+1

point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

gr3<-ggplot(merge_year3, aes(
  x=Chapter_n, y=QNT))
gr3+geom_point()+ geom_line(stat="identity")+ 
  facet_grid(Year~Type)+
  labs(x="Розділ Особливої частини КК",y="абсолютне значення",
       title = 
"Розподіл облікованих проваджень та кількості засуджених 
за розділами Особливої частини КК")+
  scale_y_continuous(labels = point)    

mysave (vis)
vis<-vis+1



gr4<-ggplot(subset(merge_year3,Type=="обліковані провадження"), aes(x=Year, y=QNT))
gr4+geom_point()+ geom_smooth(method = "lm", alpha=1/2)+geom_line()+
facet_wrap(ncol=4,.~factor(as.character(Chapter),
                      levels=chapter_levels),
             scales="free_y")+
      labs(y="абсолютне значення",
       title = "Кількість облікованих проваджень за розділами Особливої частини КК")+
  theme (axis.title.x=element_blank())+ scale_y_continuous(labels = point)    


mysave (vis)
vis<-vis+1



gr4<-ggplot(subset(merge_year3,Type=="засуджено осіб"), aes(x=Year, y=QNT))
gr4+geom_point()+geom_smooth(method = "lm",alpha=1/2)+geom_line()+ 
  facet_wrap(ncol=4,
             .~factor(as.character(Chapter),
                      levels=chapter_levels),
             scales="free_y")+
  labs(y="абсолютне значення",
       title = "Кількість засуджених за розділами Особливої частини КК")+
  theme (axis.title.x=element_blank())+ scale_y_continuous(labels = point)    

mysave (vis)
vis<-vis+1

#Circles


pgo_rawn<-subset(pgo_raw,!is.na(Chapter))

pgo_yeart<- summarise(group_by(pgo_rawn, Year, Chapter), 
                      QNT=sum(ACC))
for (i in 2013:2020){
  pgo_yeart$Sumch[pgo_yeart$Year==i]<-sum(pgo_yeart$QNT[pgo_yeart$Year==i])
}


tidy_pgo_y<-data.frame()
pgo_yeart$proc<-pgo_yeart$QNT/pgo_yeart$Sumch*100
draft<-arrange(pgo_yeart,desc(proc))
draft<-arrange(draft,Year)
draft$Chapter<-as.character(draft$Chapter)
for (i in 2013:2020)
{
  
  ss<-subset (draft, draft$Year==i)
  ss1<-ss[1:9,]
  tidy_pgo_y<-rbind(tidy_pgo_y,ss1)
  ss2<-data.frame (Year=i,
                   Chapter="Інші",
                   QNT=ss1$Sumch[1]-sum(ss1$QNT),
                   Sumch=ss1$Sumch[1], proc=100-sum(ss1$proc))
  tidy_pgo_y<-rbind(tidy_pgo_y,ss2)
  
}


grn<-ggplot(tidy_pgo_y, aes(x=factor(1),
                            y=proc,fill=factor(Chapter)))
grn+geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Paired")+
  facet_wrap(nrow=2,ncol=4,Year~.)+
  theme (axis.ticks=element_blank(), axis.text = element_blank(), legend.position = "bottom")+
  guides(fill=guide_legend("Розділи Особливої частини КК"))+
  labs(x=NULL,y=NULL,
       title = 
         "Розподіл облікованих правопорушень за розділами Особливої частини КК")


mysave (vis)
vis<-vis+1



court_rawn<-subset(court_raw,!is.na(Chapter))

court_yeart<- summarise(group_by(court_rawn, Year, Chapter), 
                        QNT=sum(CONVIC))
for (i in 2013:2020){
  court_yeart$Sumch[court_yeart$Year==i]<-sum(court_yeart$QNT[pgo_yeart$Year==i])
}


tidy_court_y<-data.frame()
court_yeart$proc<-court_yeart$QNT/court_yeart$Sumch*100
draftc<-arrange(court_yeart,desc(proc))
draftc<-arrange(draftc,Year)
draftc$Chapter<-as.character(draftc$Chapter)
for (i in 2013:2020)
{
  ss<-subset (draftc, draftc$Year==i)
  ss1<-ss[1:9,]
  tidy_court_y<-rbind(tidy_court_y,ss1)
  ss2<-data.frame (Year=i,
                   Chapter="Інші",
                   QNT=ss1$Sumch[1]-sum(ss1$QNT),
                   Sumch=ss1$Sumch[1], proc=100-sum(ss1$proc))
  tidy_court_y<-rbind(tidy_court_y,ss2)
  
}


grn<-ggplot(tidy_court_y, aes(x=factor(1),
                              y=proc,fill=factor(Chapter)))
grn+geom_bar(width = 1,stat="identity")+coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Paired")+
  facet_wrap(nrow=2,ncol=4,Year~.)+
  theme (axis.ticks = element_blank(),
         axis.text = element_blank(), legend.position = "bottom")+
  guides(fill=guide_legend("Розділи Особливої частини КК"))+
  labs(x=NULL,y=NULL,
       title = 
         "Розподіл засуджених за розділами Особливої частини КК")


mysave (vis)
vis<-vis+1













#Ratios ACC/CRTOT

gr4_1<-ggplot(merge_year_s, aes(x=Year, y=ratio))
gr4_1+geom_bar(aes(fill=Year),stat = "identity",show.legend = FALSE)+
  geom_smooth(method = "lm",alpha=1/2)+ 
  facet_wrap(ncol=4,
             .~factor(as.character(Chapter),
                      levels=chapter_levels),
             scales="free_y")+
  labs(y="%",
       title = 
"Відношення кількості засуджених до кількості облікованих проваджень
за розділами Особливої частини КК")+
  theme (axis.title.x=element_blank())+ scale_y_continuous(labels = point)    


mysave (vis)
vis<-vis+1




gr4_2<-ggplot(merge_year_s1, aes(x=Year, y=ratio))
gr4_2+geom_bar(aes(fill=Year),stat = "identity",show.legend = FALSE)+
  geom_smooth(method = "lm",alpha=1/2)+ 
  facet_wrap(ncol=4,
             .~factor(as.character(Chapter),
                      levels=chapter_levels),
             scales="free_y")+
  labs(y="%",
       title = 
"Відношення кількості засуджених до кількості переданих до суду матеріалів
з обвинувальним актом за розділами Особливої частини КК")+
  theme (axis.title.x=element_blank())+ scale_y_continuous(labels = point)    


mysave (vis)
vis<-vis+1




