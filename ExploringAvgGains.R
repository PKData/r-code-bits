ranker<-anothermod
rank2<-ranker %>%
  arrange(symbol) %>%
  group_by(symbol) %>%
  mutate(rank=rank(-low))

library(rpart)
library(rpart.plot)


rankalt<-rank2 %>%
  arrange(symbol) %>%
  group_by(symbol) %>%
  mutate(l1=lead(close,order_by=date), l2=lead(close,2,order_by=date),l3=lead(close,3,order_by=date),l4=lead(close,4,order_by=date),l5=lead(close,5,order_by=date))

rankalt2<-rankalt %>%
  arrange(symbol) %>%
  group_by(symbol) %>%
  mutate(g1=(l1-low)/low,g2=(l2-low)/low,g3=(l3-low)/low,g4=(l4-low)/low,g5=(l5-low)/low)

rankalt3<-rankalt2 %>%
  arrange(symbol) %>%
  group_by(symbol) %>%
  mutate(gweighted=g1*5+g2*4+g3*3+g2*2+g1, gavg=(g1+g2+g3+g4+g5)/5)


rdat<-rankalt3

rdat$symbol<-NULL
rdat$date<-NULL
rdat$open<-NULL
rdat$high<-NULL
rdat$low<-NULL
rdat$close<-NULL
rdat$volume<-NULL
rdat$group_by<-NULL
rdat$fut1<-NULL
rdat$l1<-NULL
rdat$l2<-NULL
rdat$l3<-NULL
rdat$l4<-NULL
rdat$l5<-NULL
rdat$g1<-NULL
rdat$g2<-NULL
rdat$g3<-NULL
rdat$g4<-NULL
rdat$g5<-NULL
rdat$gweighted<-NULL

rdat<-na.omit(rdat)
goodmod<-rpart(gavg~.,data=rdat,control=rpart.control(minbucket=.05*nrow(rdat), cp=0.003, minsplit = 1))
rpart.plot(goodmod)

rdat2<-subset(rankalt3, rankalt3$rank > 58 & daily.returns > 0.012)
rdat2$date<-NULL
rdat2$open<-NULL
rdat2$high<-NULL
rdat2$low<-NULL
rdat2$close<-NULL
rdat2$volume<-NULL
rdat2$group_by<-NULL
rdat2$l1<-NULL
rdat2$l2<-NULL
rdat2$l3<-NULL
rdat2$l4<-NULL
rdat2$l5<-NULL
rdat2$g1<-NULL
rdat2$g2<-NULL
rdat2$g3<-NULL
rdat2$g4<-NULL
rdat2$g5<-NULL
rdat2$gweighted<-NULL
rdat2$gavg<-NULL

rank3<-rdat2 %>%
  arrange(symbol) %>%
  group_by(symbol) %>%
  mutate(bigups=rank(fut1))

rdat3<-rank3
rdat3$symbol<-NULL
rdat3$fut1<-NULL


#goodmod4<-rpart(bigups~.,data=rdat3,control=rpart.control(minbucket=.03*nrow(rdat3), cp=0.002, minsplit = 1))

s5<-s4 %>%
  group_by(date) %>%
  top_n(n=5,wt=-daily.returns)


