# Скрипт визначає послідовність виконання всіх скриптів проєкту

library (beepr)


exlist<-c("get-proceed-COURT.R",
          "get-proceed-PG0.R",
          "load2.R",
          "plot1-2.R",
          "plot3.R",
          "plot4-6.R",
          "plot7-9.R",
          "plot10-17.R", 
          "plot18-20.R",
          "plot21-23.R",      
          "plot_chapter.R",
          "plot_top.R")                    
start<-Sys.time()
  
for (pow in 1:length(exlist))
    {source(exlist[pow],encoding = "UTF-8",echo = TRUE)}

beep(5)
print (c(paste("Роботу розпочато: ",start," Роботу закінчено ",Sys.time())))
