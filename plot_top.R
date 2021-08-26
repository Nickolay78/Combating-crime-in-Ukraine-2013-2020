#Візуалізації щодо найбільш поширених кримінальних правопорушень 
#(144-288, 1 загальна та по 6 щодо кожної статті

top_art_names<-c(
  "Крадіжка, ст. 185",
  "Незаконне виробництво, виготовлення, придбання, зберігання, перевезення 
чи пересилання наркотичних засобів, психотропних речовин або їх аналогів
без мети збуту, ст. 309",
  "Грабіж, ст. 186",
  "Умисне легке тілесне ушкодження, ст. 125",
  "Незаконне поводження зі зброєю, бойовими припасами 
або вибуховими речовинами, ст. 263",
  "Порушення правил безпеки дорожнього  руху або експлуатації транспорту 
особами,  які керують транспортними засобами, ст. 286",                                              
  "Шахрайство, ст. 190",                                                                                                                                                      
  "Незаконне заволодіння транспортним засобом, ст. 289",                                                                                                                      
  "Самовільне залишення військової частини або місця служби, ст. 407",                                                                                                        
  "Умисне тяжке тілесне ушкодження, ст. 121",                                                                                                                                 
  "Хуліганство, ст. 296",                                                                                                                                                     
  "Ухилення від сплати аліментів на утримання дітей,  ст. 164",                                                                                                               
  "Незаконне виробництво, виготовлення, придбання, зберігання, перевезення, 
пересилання чи збут  наркотичних засобів, психотропних речовин  
або їх аналогів, ст. 307",        
  "Розбій, ст. 187",                                                                                                                                                          
  "Підроблення документів, печаток, штампів та бланків, збут чи  використання 
підроблених документів, печаток, штампів, ст. 358",                                             
  "Умисне середньої тяжкості тілесне ушкодження, ст. 122",                                                                                                                    
  "Посів або вирощування снотворного маку чи конопель, ст. 310",                                                                                                              
  "Ухилення від покарання, не пов’язаного з позбавленням волі, ст. 389",                                                                                                      
  "Привласнення, розтрата майна або заволодіння ним 
шляхом зловживання службовим становищем, ст. 191",                                                                        
  "Умисне вбивство, ст. 115",                                                                                                                                                 
  "Порушення правил адміністративного нагляду, ст. 395",                                                                                                                      
  "Порушення порядку здійснення операцій з металобрухтом, ст. 213",                                                                                                           
  "Незаконне зайняття рибним, звіриним або іншим водним  
добувним промислом , ст. 249",                                                                                       
  "Організація або утримання місць для незаконного вживання, виробництва 
чи виготовлення  наркотичних засобів,  психотропних речовин 
або їх аналогів, ст. 317")


top<-summarise(group_by(court_raw,Article),
               CONVIC_T=sum(CONVIC),
               prob_art=0,
               round_p=0)
top$prob_art<-(top$CONVIC_T/sum(top$CONVIC_T))*100
top$round_p<-round(top$prob_art,2)





top_a<-arrange(subset(top, prob_art > quantile (prob_art, prob = 0.945)), desc (prob_art))

top_a$Article<-sub("Article ","",top_a$Article)
top_g<-ggplot(top_a,aes(x=Article,y=CONVIC_T))
top_g+geom_bar(aes(fill=Article), show.legend = FALSE, stat = "identity")+
  labs (x="Стаття Особливої частини КК", y="кількість засуджених",
        title=paste("Кримінальні правопорушення, за вчинення яких засуджено", round ((sum(top_a$prob_art)/sum(top$prob_art))*100), "% осіб,
визнаних винними протягом 2013-2020 років"))+
  scale_y_continuous(labels = point)    
mysave (vis)
vis<-vis+1

for (i in 1:length(top_a$Article))
{
  print (paste("Working... ",i,"/",length (top_a$Article)))
  art_top<-sub(" ","",top_a$Article[i])

  pgo_sub<-subset (pgo_raw,(str_detect(pgo_raw$Article,art_top)&!str_detect(pgo_raw$Article,"-")))
  pgo_sub$Article<-art_top
  pgo_sub$TO_COURT<-apply (pgo_sub[,6:9],1,sum)
  pgo_sub1<-select(pgo_sub,"Year","Chapter","Article","ACC","TO_COURT")
  
  court_sub<-subset (court_raw,str_detect(court_raw$Article,art_top))
  court_sub$Article<-art_top
  court_sub_art<-summarise (group_by(court_sub,Year,Chapter,Article),
                            CRTOT=sum(CRTOT),
                            CONVIC=sum(CONVIC))
  court_sub_art$Chapter<-as.roman(court_sub_art$Chapter)
  pgo_sub1<-merge (pgo_sub1,court_sub_art)
  pgo_sub1<-pgo_sub1[,c("Year","Chapter","Article","ACC","TO_COURT","CONVIC")]
  pgo_sub1<-reshape(pgo_sub1,
          timevar = "Comment", 
          times=c("ACC","TO_COURT","CONVIC"), 
          v.names = "QNT", 
          varying = c("ACC","TO_COURT","CONVIC"), 
          direction = "long")
  
  pgo_sub1$Comment[pgo_sub1$Comment=="ACC"]<-"обліковано проваджень"
  pgo_sub1$Comment[pgo_sub1$Comment=="TO_COURT"]<-"передано матеріалів до суду"
  pgo_sub1$Comment[pgo_sub1$Comment=="CONVIC"]<-"засуджено осіб"
  
  
  
  

  
  top_1<-ggplot (pgo_sub1,aes(x=Year,y=QNT))
  top_1+geom_bar(aes(fill=Comment),stat = "identity",show.legend = FALSE)+
    facet_grid(.~factor(Comment, levels=c("обліковано проваджень",
                                          "передано матеріалів до суду",
                                        "засуджено осіб")))+
    labs (y="кількість", title = "Кількість облікованих проваджень, переданих до суду матеріалів та засуджених осіб",
          subtitle=paste (top_art_names[i]))+
    theme (axis.title.x = element_blank())+scale_y_continuous(labels = point)   
  
          
  mysave (vis)
  vis<-vis+1  
  
  pgo_sub2<-select(pgo_sub,"Year","Article","Chapter","INDICM","REL","MED","EDU")
    pgo_sub2<-reshape(pgo_sub2,
          timevar = "Comment", 
          times=c("INDICM","REL","MED","EDU"), 
          v.names = "QNT", 
          varying = c("INDICM","REL","MED","EDU"), 
          direction = "long",new.row.names = 1:32)
  pgo_sub2$Comment[pgo_sub2$Comment=="INDICM"]<-"з обвинувальним актом"
  pgo_sub2$Comment[pgo_sub2$Comment=="REL"]<-"клопотання про звільнення"
  pgo_sub2$Comment[pgo_sub2$Comment=="MED"]<-"примусові заходи
медичного характеру"
  pgo_sub2$Comment[pgo_sub2$Comment=="EDU"]<-"примусові заходи
виховного характеру"
  
  pgo_sub2[pgo_sub2==0]<-NA
pgo_sub2<-subset(pgo_sub2,!is.na(pgo_sub2$QNT))
  top_2<-ggplot(pgo_sub2, aes(x=Year,y=QNT, label=QNT))
  top_2+geom_bar (stat="identity",aes(fill=Comment),show.legend = FALSE)+
    facet_wrap(.~factor (Comment, 
levels=c("з обвинувальним актом",
         "клопотання про звільнення",
         "примусові заходи
медичного характеру",
         "примусові заходи
виховного характеру")),scales = "free_y")+ 
    scale_fill_brewer(palette = "Set1")+
    labs(title = "Структура матеріалів, що передаються до суду",
         subtitle=paste (top_art_names[i]))+
    geom_text(size=3,position = position_stack(vjust = 0.5))+
    theme(axis.title=element_blank(),axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  mysave (vis)
  vis<-vis+1
  
  court_sub<-subset (court_raw,(str_detect(court_raw$Article,art_top)&!str_detect(court_raw$Article,"-")))
  court_sub$Article<-art_top
  court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
                            CONVIC=sum(CONVIC),
                            ACQUIT=sum(ACQUIT),
                            CMED=sum(CMED),
                            CCLOSE=sum(CCLOSE))
  court_sub<-court_sub[,c("Year","Chapter","Article","CONVIC","ACQUIT","CMED","CCLOSE")]
  court_sub<-as.data.frame(court_sub)
  court_sub<-reshape(court_sub,
                    timevar = "Comment", 
                    times=c("CONVIC","ACQUIT","CMED","CCLOSE"), 
                    v.names = "QNT", 
                    varying = c("CONVIC","ACQUIT","CMED","CCLOSE"), 
                    direction = "long")
  
  
  court_sub$Comment[court_sub$Comment=="CONVIC"]<-"засуджено осіб"
  court_sub$Comment[court_sub$Comment=="ACQUIT"]<-"виправдано осіб"
  court_sub$Comment[court_sub$Comment=="CMED"]<-"неосудних осіб"
  court_sub$Comment[court_sub$Comment=="CCLOSE"]<-"закрито справ"
  court_sub[court_sub==0]<-NA
  court_sub<-subset(court_sub,!is.na(court_sub$QNT))
  top_3<-ggplot(court_sub, aes(x=Year,y=QNT, label=QNT))
  top_3+geom_bar (stat="identity",aes(fill=Comment),show.legend = FALSE)+
    facet_wrap(.~factor (Comment, 
                         levels=c("засуджено осіб",
                                  "виправдано осіб",
                                  "неосудних осіб",
                                  "закрито справ")),scales = "free_y")+ 
    labs(title = "Структура судових рішень",
         subtitle=paste (top_art_names[i]))+
    geom_text(size=3,position = position_stack(vjust = 0.5))+
    theme(axis.title=element_blank(),axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  mysave (vis)
  vis<-vis+1  
  
  #Розподіл підстав закриття справ  
  
  court_sub<-subset (court_raw,(str_detect(court_raw$Article,art_top)&!str_detect(court_raw$Article,"-")))
  
  court_sub$Article<-art_top
  court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
                        CONFES=sum(CONFES),
                        RECONC=sum(RECONC),
                        CIRCUMS=sum(CIRCUMS),
                        SPONS=sum(SPONS),
                        AMNESTY=sum(AMNESTY),
                        DEATH=sum(DEATH),
                        COTHER=sum(COTHER),
                        CNOTCR=sum(CNOTCR),
                        CEDU=sum(CEDU),
                        SAMEVERD=sum(SAMEVERD),
                        DENOPR=sum(DENOPR))
                        
                        
  court_sub<-as.data.frame(court_sub)
  court_sub<-reshape(court_sub,
                     timevar = "Comment", 
                     times=c("CONFES","RECONC","CIRCUMS","SPONS","AMNESTY","DEATH","COTHER",
                             "CNOTCR","CEDU","SAMEVERD","DENOPR"), 
                     v.names = "QNT", 
                     varying = c("CONFES","RECONC","CIRCUMS","SPONS","AMNESTY","DEATH","COTHER",
                                 "CNOTCR","CEDU","SAMEVERD","DENOPR"), 
                     direction = "long")
  
  court_sub$Comment[court_sub$Comment=="CONFES"]<-"дійове каяття"
  court_sub$Comment[court_sub$Comment=="RECONC"]<-"примирення винного з потерпілим"
  court_sub$Comment[court_sub$Comment=="CIRCUMS"]<-"зміна обстановки"
  court_sub$Comment[court_sub$Comment=="SPONS"]<-"передача на поруки"
  court_sub$Comment[court_sub$Comment=="AMNESTY"]<-"амністія"
  court_sub$Comment[court_sub$Comment=="DEATH"]<-"смерть"
  court_sub$Comment[court_sub$Comment=="COTHER"]<-"інші підстави"
  court_sub$Comment[court_sub$Comment=="CNOTCR"]<-"недоведеність обвинувачення 
(до 2017)"
  court_sub$Comment[court_sub$Comment=="CEDU"]<-"примусові заходи виховного характеру
(до 2017)"
  court_sub$Comment[court_sub$Comment=="SAMEVERD"]<-"рішення по ідентичному
обвинуваченню (з 2018)"
  court_sub$Comment[court_sub$Comment=="DENOPR"]<-"відмова від обвинувачення 
(з 2018)"
  court_sub[court_sub==0]<-NA
  order_p<-c("дійове каяття",
             "примирення винного з потерпілим",
             "зміна обстановки",
             "передача на поруки",
             "амністія",
             "смерть",
             "недоведеність обвинувачення 
(до 2017)",
             "примусові заходи виховного характеру
(до 2017)",
             "рішення по ідентичному
обвинуваченню (з 2018)",
             "відмова від обвинувачення 
(з 2018)",
             "інші підстави")
  court_sub<-subset(court_sub,!is.na(QNT))
  
  top_4<-ggplot (court_sub,aes(x=factor(Comment,levels=order_p), y=QNT,fill=Comment, label=QNT))
  top_4+scale_fill_discrete(breaks=order_p)+
    geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=4,dir="h")+
    geom_text(size=2,vjust = 0, nudge_y = 0.5)+
    theme (legend.position = "bottom",legend.title=element_blank(),
           axis.ticks.x=element_blank(),legend.direction="horizontal",
           legend.text=element_text(size = 7), axis.text.x=element_blank())+
    labs(title="Розподіл підстав закриття справ",x="Підстави",y="кількість",
         subtitle=paste (top_art_names[i]))
    
    
  mysave (vis)
  vis<-vis+1
  
#Застосовані покарання

court_sub<-subset (court_raw,(str_detect(court_raw$Article,art_top)&!str_detect(court_raw$Article,"-")))

court_sub$Article<-art_top
court_sub<-summarise (group_by(court_sub,Year,Chapter,Article),
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
посади або займатися певною діяльністю")			




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
посади або займатися певною діяльністю"
court_sub[court_sub==0]<-NA

court_sub<-subset(court_sub,!is.na(QNT))

top_5<-ggplot (court_sub,aes(x=factor(Comment,levels=punish_ord), y=QNT,fill=Comment,label=QNT))
top_5+scale_fill_discrete(breaks=punish_ord)+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=4,dir="h")+
  geom_text(size=2,vjust = 0, nudge_y = 0.5)+
  theme (legend.position = "bottom",axis.ticks.x=element_blank(), legend.title=element_blank(),
         legend.text.align=0,legend.text=element_text(size = 7), axis.text.x=element_blank())+
  labs(title="Застосовані покарання",x="Види покарань",y="кількість",
       subtitle=paste (top_art_names[i]))


mysave (vis)
vis<-vis+1



#Розподіл інтенсивності позбавлення волі

court_sub<-subset (court_raw,(str_detect(court_raw$Article,art_top)&!str_detect(court_raw$Article,"-")))

court_sub$Article<-art_top
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



top_6<-ggplot (court_sub,aes(x=factor(Comment,levels=imp_ord), y=QNT,fill=Comment,label=QNT))
top_6+scale_fill_discrete(breaks=imp_ord,type=c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"))+
  geom_bar(stat="identity",)+facet_wrap(.~Year,ncol=4,dir="h")+
  geom_text(size=2,vjust = 0, nudge_y = 0.5)+
  theme (legend.position = "bottom",axis.ticks.x=element_blank(), legend.title=element_blank(),
         legend.text=element_text(size = 7), axis.text.x=element_blank())+
  labs(title="Розподіл строків призначеного покарання у вигляді позбавлення волі",x="Строки",y="кількість",
       subtitle=paste (top_art_names[i]))


mysave (vis)
vis<-vis+1

}







