df<-read.csv("E:/categorical.csv",header=TRUE)


#transforming text into logical true/false presence values
south<-apply(df,1,function(x)ifelse(sum(x=="south")>0,1,0))
north<-apply(df,1,function(x)ifelse(sum(x=="north")>0,1,0))
east<-apply(df,1,function(x)ifelse(sum(x=="east")>0,1,0))
west<-apply(df,1,function(x)ifelse(sum(x=="west")>0,1,0))
central<-apply(df,1,function(x)ifelse(sum(x=="central")>0,1,0))
unclassified<-apply(df,1,function(x)ifelse(sum(x=="unclassified")>0,1,0))


#forming new table
df1<-cbind(south,north,east,west,central,unclassified)

#counting the number of different values present & adding the column to a new table
diffvals<-apply(df1,1,sum)
df2<-cbind(df1,diffvals)


#classifying
classification<-ifelse(unclassified >=1,"Unclassified",
                           ifelse(diffvals>2,"Mixed",
                              ifelse(south == 1 & east == 1, "South_East",
                              ifelse(south == 1 & west == 1, "South_West",
                              ifelse(south == 1 & central == 1, "South_Central",
                              ifelse(south == 1 & north == 1, "NOT_SURE",
                              ifelse(south==1,"South",
                                  ifelse(north == 1 & east == 1, "North_East",
                                  ifelse(north == 1 & west == 1, "North_West",
                                  ifelse(north == 1 & central == 1, "North_Central",
                                  ifelse(north == 1, "North",
                                         ifelse(east == 1 & west == 1, "NOT_SURE",
                                         ifelse(east == 1 & central == 1, "East_Central",
                                         ifelse(east == 1, "East",
                                                ifelse(west==1 & central == 1, "West_Central",
                                                  ifelse(west==1,"West",
                                                         ifelse(central==1,"Central",
                                                                "NONE")))))))))))))))))
cbind(df,classification)