library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
library(tidyr)
library(mlr)  # to utlize summarizeColumns

#------------------------TrainData loading
#system.time(train_cat <- fread("train_categorical.csv",stringsAsFactors=T))
#system.time(train_dat <- fread("train_date.csv",stringsAsFactors=T))
#system.time(train_num <- fread("train_numeric.csv",stringsAsFactors=T))

####---------------------feature statitics
system.time(r<-summarizeColumns(train_num))  # Numiric Features
system.time(s<-summarizeColumns(train_dat))  # Date features
system.time(j1<-summarizeColumns(train_cat[,.SD,.SDcols=c(2:1050)])) # for full colum's it crashes
system.time(j2<-summarizeColumns(train_cat[,.SD,.SDcols=c(1051:2141)]))
setkey(train_cat,L0_S1_F25)
system.time(g<-summarizeLevels(train_cat[,.SD,.SDcols=c(2:2140)]))  # to find level in each colum
t<-rbind(j1,j2)  # r s t takes 40 minutes to run
features_stat<-NULL
features_stat<-rbind(r[-c(1,970),],s[-c(1),],t) # remove Id and response colums

# split feature to numeric-> line, station and feature no 
x<-features_stat  %>% separate(name,c("Line","Station","Fno"),"_")
features_stat$Line<-as.numeric(gsub("L","",x$Line))
features_stat$Station<-as.numeric(gsub("S","",x$Station))
features_stat$Fno<-as.numeric(gsub("[FD]","",x$Fno))
features_stat$Ftype<-c(rep("Fnum",(nrow(r)-2)),rep("Ddat",(nrow(s)-1)),rep("Fcat",(nrow(t))))
features_stat$NApc<-features_stat$na*100/nrow(train_num)  # percentage of NA in each colum
features_stat<-tbl_df(features_stat)

## ploting order of features with line and station and feature type
feature_plot<-ggplot(data = features_stat,aes(x=Station))+
  geom_point(aes(y=Fno,colour=factor(Ftype),pch=factor(Ftype)),size=1.5)+
  geom_vline(xintercept = c(-0.5,23.5,25.5,28.5,51.5), color = "red", size=.5)+
  scale_x_continuous(name="Station No", breaks=seq(0,51,3))+
  ylab("Features number")+
  annotate("text", x=c(10,24.5,27,40), y=4200, label= c("Line:L1","L2","L3","L4"),color = "blue")

ggplotly(feature_plot)

### feature statics extraction
y<-features_stat%>% group_by(Station,Ftype)%>% summarise(Fno=n())
features_stat%>% group_by(Station)%>% filter(Ftype=="Fnum")%>%tally() # for total features
y<-features_stat%>%
  filter( NApc==100)%>% #Station==24&& Ftype="Dat"
  arrange(desc(NApc))
y$NApc<-1
pp<-ggplot(data = y,aes(x=Fno))+
    geom_point(aes(y=round(NApc,0),pch=factor(Ftype)),size=2)
    #geom_line(aes(y=max,colour=factor(Line)),size=1)

ggplotly(pp)

table(round(y$NApc))
h<-prop.table(table(round(features_stat$NApc)))*100
#####################--
train_num[,.(L0_S0_F8)]
train_num[,.SD,.SDcols=c(1:2)]
b[,.SD,.SDcols=-2] # remove colum

conn <- unz("train_numeric.csv.zip","train_numeric.csv","r")
columns <- scan(conn,what=character(0),sep=",",nlines=1)
close(conn)

conn <- unz("train_date.csv.zip","train_date.csv","r")
columns <- scan(conn,what=character(0),sep=",",nlines=3)
close(conn)
m=merge(train_cat,train_dat,train_num,by="Id")