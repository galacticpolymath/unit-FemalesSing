#Install/load pacman
if(!require(pacman)){install.packages("pacman");require(pacman)}
#Install/load tons of packages
p_load(ggplot2,tidyverse,lubridate,ggpubr)

#bring in code for enciphering graph elements
source("https://raw.githubusercontent.com/drwilkins/ciphR/master/ciphR/enciphR.R")
#bring in GP styling
source("https://raw.githubusercontent.com/galacticpolymath/ggGalactic/master/ggGalactic.R")


#--------------------------
#**************************************************************************
###### Look at Timing of singing in both sexes ############################
####
##

j<-read.csv("data/temporal_singing_data_for_fig2.csv")
j$year=factor(j$year)
j$RecDt<-ymd(gsub("2011","2012", j$RecDate,fixed=T))
k=subset(j,!is.na(year))
#Calculate male song recording phenology
(songsPerDay<-tapply(subset(j,sex=="M")$N,subset(j,sex=="M")$RelRec,sum))
plot(sort(unique(subset(j,sex=="M")$RelRec)),songsPerDay)
weighted.mean(subset(j,sex=="M")$RelRec,subset(j,sex=="M")$N,na.rm=T)
#Calculate female song recording phenology
(songsPerDay<-tapply(subset(j,sex=="F")$N,subset(j,sex=="F")$RelRec,sum))
plot(sort(unique(subset(j,sex=="F")$RelRec)),songsPerDay)
weighted.mean(subset(j,sex=="F")$RelRec,subset(j,sex=="F")$N,na.rm=T)





#Males+Female #songs produced by recording date
show.gpPal(2)
femalecol="#BD0000"
malecol="#6812D1"
k$sex<-factor(k$sex,labels=c("Female","Male"))
G0<-ggplot(data=k,aes(x=RecDt,y=N,col=sex,shape=sex))+geom_point(size=3,stroke=.5,alpha=1)+geom_point(aes(fill=sex),size=3,alpha=.3)+scale_x_date()+xlab("Recording Date")+ylab("# Songs Recorded")+geom_smooth(method="loess",aes(group=sex),span=2,size=1.1,show.legend=F,se=F)+scale_shape_manual(values=gpShps[1:2])+scale_fill_manual(values=c(malecol,femalecol))+scale_colour_manual(values=c(malecol,femalecol))

#Level 1 puzzle
(Gl1 <- G0+ggGalactic()+guides(shape = guide_legend(override.aes = list(size = 4)))+theme(legend.title=element_text(face="bold")))
ggsave("NITgraph_level1_original.png",width=8,height=6)


#Level 2 puzzle: (+1) shift
(Gl2 <- ggCiphR(Gl1,1))
ggsave("NITgraph_level2_coded-axis-labels.png",width=8,height=6)

#Level 3 (-3) shift
(Gl3 <- ggCiphR(Gl1,-3))
ggsave("NITgraph_level3_difficult-coded-axis-labels.png",width=8,height=6)

#Level 4 
K <- k
enciphR("sex",3)
K$sex<-factor(k$sex,labels=sapply(levels(k$sex),enciphR,3))

(Gl4.0<-ggplot(data=K,aes(x=RecDt,y=N,col=sex,shape=sex))+geom_point(size=3,stroke=.5,alpha=1)+geom_point(aes(fill=sex),size=3,alpha=.3)+scale_x_date()+xlab("Recording Date")+ylab("# Songs Recorded")+geom_smooth(method="loess",aes(group=sex),span=2,size=1.1,show.legend=F,se=F)+scale_shape_manual(values=gpShps[1:2])+scale_fill_manual(values=c(malecol,femalecol))+scale_colour_manual(values=c(malecol,femalecol))+ggGalactic()+guides(shape = guide_legend(override.aes = list(size = 5))))
(Gl4<-ggCiphR(Gl4.0,3))
ggsave("NITgraph_level4_Impossible-coded-axis-labels+key.png",width=8,height=6)



###########
## Make histograms of female singing
k2<-melt(k[,c("ID","sex","N")],id=c("ID","sex"))
k3<-dcast(k2,formula=ID+sex~variable,fun.aggregate=sum,na.rm=T)

g2<-ggplot(k3,aes(x=N))+geom_histogram(breaks=seq(0,100,10),col=1,fill=gpPal2[12])+facet_grid(~sex,labeller=labeller(sex=c(Female="?____________",Male="?____________")))+ggGalactic()+labs(x="X:______________________________",y="Y:______________________",title="Title:______________________")+theme(strip.background=element_rect(fill="gray90"),strip.text=element_text(colour=1,size=22))
g2
ggsave("songOutputHistograms_blank.png",width=8,height=6)
#Axis, labeled, no facets or title
g2+labs(x="Total Songs Recorded per Individual",y="Count of Individuals",title="")
ggsave("songOutputHistograms_AxisLabels.png",width=8,height=6)
#Axis, facets labeled, no title
g2+labs(x="Total Songs Recorded per Individual",y="Count of Individuals",title="")+ggGalactic()+facet_grid(~sex)
ggsave("songOutputHistograms_Axis+FacetLabels.png",width=8,height=6)



##### 
## 