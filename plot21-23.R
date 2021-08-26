# Візуалізації 21-23
library (tidyverse)



#Застосовані покарання

court_sub<-court_raw

court_sub<-summarise (group_by(court_sub,Year),
                      LIFEIMP=sum(LIFEIMP),
                      IMP=sum(IMP),
                      IMP1=sum(IMP1),
                      IMP12=sum(IMP12),
                      IMP23=sum(IMP23),
                      IMP35=sum(IMP35),
                      IMP510=sum(IMP510),
                      IMP1015=sum(IMP1015),
                      IMP1525=sum(IMP1525),
                      RESTOL=sum(RESTOL),
                      DISBAT=sum(DISBAT),
                      ARREST=sum(ARREST),
                      CORRW=sum(CORRW),
                      SRVRSTR=sum(SRVRSTR),
                      PUBLW=sum(PUBLW),
                      FINE=sum(FINE),
                      DEPR=sum(DEPR))


court_sub<-select(court_sub,Year:IMP,RESTOL:DEPR)
court_sub<-as.data.frame(court_sub)


court_sub<-reshape(court_sub,
                   timevar = "Comment", 
                   times=c("LIFEIMP","IMP","RESTOL","DISBAT", "ARREST", "CORRW","SRVRSTR","PUBLW","FINE","DEPR"), 
                   v.names = "QNT", 
                   varying = c("LIFEIMP","IMP","RESTOL","DISBAT", "ARREST", "CORRW","SRVRSTR","PUBLW","FINE","DEPR"), 
                   direction = "long")
punish_ord<-c(
  "довічне позбавлення волі",
  "позбавлення волі",
  "обмеження волі",			
  "тримання в дисциплінарному 
батальйоні",			
  "арешт",		
  "виправні роботи",			
  "службове обмеження для
військовослужбовців",			
  "громадські роботи",			
  "штраф",			
  "позбавлення права займати певні
посади або займатися певною
діяльністю")			




court_sub$Comment[court_sub$Comment=="LIFEIMP"]<-"довічне позбавлення волі"
court_sub$Comment[court_sub$Comment=="IMP"]<-"позбавлення волі"
court_sub$Comment[court_sub$Comment=="RESTOL"]<-"обмеження волі"
court_sub$Comment[court_sub$Comment=="DISBAT"]<-"тримання в дисциплінарному
батальйоні"
court_sub$Comment[court_sub$Comment=="ARREST"]<-"арешт"
court_sub$Comment[court_sub$Comment=="CORRW"]<-"виправні роботи"
court_sub$Comment[court_sub$Comment=="SRVRSTR"]<-"службове обмеження для
військовослужбовців"
court_sub$Comment[court_sub$Comment=="PUBLW"]<-"громадські роботи"
court_sub$Comment[court_sub$Comment=="FINE"]<-"штраф"
court_sub$Comment[court_sub$Comment=="DEPR"]<-"позбавлення права займати певні
посади або займатися певною
діяльністю"
court_sub[court_sub==0]<-NA


top_5<-ggplot (court_sub,aes(x=factor(Comment,levels=punish_ord), y=QNT,fill=Comment))
top_5+scale_fill_discrete(breaks=punish_ord)+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=4,dir="h")+
    theme (legend.position = "bottom",legend.title=element_blank(),axis.ticks.x = element_blank(),
         legend.text.align=0,legend.text=element_text(size = 7), axis.text.x=element_blank())+
  labs(title="Застосовані покарання",x="Види покарань",y="кількість")


mysave (vis)
vis<-vis+1
#Розподіл інтенсивності позбавлення волі

court_sub<-court_raw
court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
                      IMP1=sum(IMP1),
                      IMP12=sum(IMP12),
                      IMP23=sum(IMP23),
                      IMP35=sum(IMP35),
                      IMP510=sum(IMP510),
                      IMP1015=sum(IMP1015),
                      IMP1525=sum(IMP1525))



court_sub<-as.data.frame(court_sub)


court_sub<-reshape(court_sub,
                   timevar = "Comment", 
                   times=c("IMP1","IMP12","IMP23","IMP35", "IMP510", "IMP1015","IMP1525"), 
                   v.names = "QNT", 
                   varying = c("IMP1","IMP12","IMP23","IMP35", "IMP510", "IMP1015","IMP1525"), 
                   direction = "long")
imp_ord<-c(
  "1 рік",	
  "понад 1 рік
до 2 років включно",	
  "понад 2 роки
до 3 років включно",	
  "понад 3 роки
до 5 років включно",	
  "понад 5 років
до 10 років включно",	
  "понад 10 років
до 15 років включно",	
  "понад 15 років
до 25 років включно")	

court_sub$Comment[court_sub$Comment=="IMP1"]<-"1 рік"
court_sub$Comment[court_sub$Comment=="IMP12"]<-"понад 1 рік
до 2 років включно"
court_sub$Comment[court_sub$Comment=="IMP23"]<-"понад 2 роки
до 3 років включно"
court_sub$Comment[court_sub$Comment=="IMP35"]<-"понад 3 роки
до 5 років включно"
court_sub$Comment[court_sub$Comment=="IMP510"]<-"понад 5 років
до 10 років включно"
court_sub$Comment[court_sub$Comment=="IMP1015"]<-"понад 10 років
до 15 років включно"
court_sub$Comment[court_sub$Comment=="IMP1525"]<-"понад 15 років
до 25 років включно"

court_sub[court_sub==0]<-NA

court_sub<-subset(court_sub,!is.na(QNT))



top_6<-ggplot (court_sub,aes(x=factor(Comment,levels=imp_ord), y=QNT,fill=Comment))
top_6+scale_fill_discrete(breaks=imp_ord,type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  geom_bar(stat="identity")+facet_wrap(.~Year,ncol=4,dir="h")+
    theme (legend.position = "bottom",axis.ticks.x=element_blank(), legend.title=element_blank(),
         legend.text=element_text(size = 8), axis.text.x=element_blank())+
  labs(title="Розподіл строків призначеного покарання у вигляді позбавлення волі",x="Строки",y="кількість")


mysave (vis)
vis<-vis+1




court_s<-data.frame()
y=1
list1<-list.files(path="DATA_COURT/") 
for (y in 1:length (list1))
{
  rr<-read.csv(paste("DATA_COURT/",list1[y],sep=""))
  if (y<=5) 
  {
    rr<-select  (rr,Year:CONVIC,LIFEIMP,IMP, RESTOL, PUBLW, FINE, PROB,RELAMN,RELOTHR)
    rrn<-names(rr)}
  else
  {
    rr<-select  (rr,Year:CONVIC,x3LIFEIMP,x3IMP, x3RESTOL,x3PUBLW, x3FINE, PROB,RELAMN,RELOTHR)
    names (rr)<-rrn}
  court_s<-rbind(court_s,rr)
} 

court_s$Chapter<-as.roman(court_s$Chapter)

court_s4<- summarise(group_by(court_s, Year, Chapter), 
                     PUNISH=sum(CONVIC)-sum(PROB,RELAMN,RELOTHR),
                     REL=sum(PROB,RELAMN,RELOTHR),
                     CONVIC=sum(CONVIC),
                     IMP=sum(LIFEIMP,IMP),
                     
                     PUBLW=sum(PUBLW),
                     FINE=sum(FINE),
                     OTHR=sum(CONVIC)-sum(PROB,RELAMN,RELOTHR,PUBLW,LIFEIMP,IMP,FINE))

court_s4<-subset(court_s4,!is.na(Chapter))
court_s4$Chapter_n<-as.numeric(court_s4$Chapter)




gname<-c("Year","Chapter","QNT","Comment")
s6_2<-select (court_s4,Year,Chapter,PUNISH)
s6_2$Comment<-"застосовано покарання"
names(s6_2)<-gname

s6_11<-select (court_s4,Year, Chapter,REL)
s6_11$Comment<-"звільнено від покарання"
names(s6_11)<-gname
s6_2<-rbind(s6_2,s6_11)

s6_11<-select (court_s4,Year, Chapter,IMP)
s6_11$Comment<-"позбавлення волі"
names(s6_11)<-gname
s6_2<-rbind(s6_2,s6_11)

s6_11<-select (court_s4,Year, Chapter,FINE)
s6_11$Comment<-"штраф"
names(s6_11)<-gname
s6_2<-rbind(s6_2,s6_11)

s6_11<-select (court_s4,Year, Chapter,PUBLW)
s6_11$Comment<-"громадські роботи"
names(s6_11)<-gname
s6_2<-rbind(s6_2,s6_11)

s6_11<-select (court_s4,Year, Chapter, Chapter,OTHR)
s6_11$Comment<-"інше покарання"
names(s6_11)<-gname
s6_2<-rbind(s6_2,s6_11)

gr6_1<-ggplot(s6_2,aes(x=factor(Comment), y=QNT, fill=Comment))
gr6_1+geom_bar(stat="identity")+
  facet_wrap(ncol=5,
             .~factor(as.character(Chapter),
                      levels=chapter_levels),
             scales="free_y")+
   scale_fill_discrete(breaks=c("застосовано покарання",
                               "звільнено від покарання",
                               "позбавлення волі",
                               "штраф",
                               "громадські роботи",
                               "інше покарання"),
                       type = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33"))+
  scale_x_discrete(limits=c("застосовано покарання",
                            "звільнено від покарання",
                            "позбавлення волі",
                            "штраф",
                            "громадські роботи",
                            "інше покарання"))+
  theme (axis.title = element_blank(),
         legend.title=element_blank(),
         axis.text.x = element_blank(),axis.ticks.x = element_blank(),
         legend.position = "bottom")+
  labs(title = "Розподіл форм реалізації кримінальної відповідальності та 
застосованих покарань за розділами Особливої частини КК",
       subtitle = "загальна кількість за період з 2013 по 2020 рік")


mysave (vis)
vis<-vis+1