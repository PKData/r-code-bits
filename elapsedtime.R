df<-read.csv("E:/ex1.csv")


library(dplyr)



df$time<-as.POSIXct(df$time,format="%d/%m/%y %H:%M")
df$goodstart<-ifelse(df$To=="green",as.POSIXct(df$time),0)
df$endtime<-ifelse(df$From=="green",as.POSIXct(df$time),0)

df1<-df %>%
  group_by(Host) %>%
  mutate(starttime=lag(goodstart))

df1$elapsedtime<-df1$endtime-df1$starttime

df2<-subset(df1, endtime > 0 & starttime > 0)


  