#Install/load pacman
if(!require(pacman)){install.packages("pacman");require(pacman)}
#Install/load tons of packages
p_load(ggplot2,tidyverse,lubridate,ggpubr,reshape2,colorspace,patchwork)

#bring in code for enciphering graph elements
source("https://raw.githubusercontent.com/drwilkins/ciphR/master/ciphR/enciphR.R")
#bring in GP styling
# source("https://raw.githubusercontent.com/galacticpolymath/ggGalactic/master/ggGalactic.R")


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
femalecol="#BD0000" #gpPal2[14] 
malecol= "#6812D1" #gpPal[4]
k$sex<-factor(k$sex,labels=c("Female","Male"))
G0<-ggplot(data=k,aes(x=RecDt,y=N,col=sex,shape=sex))+geom_point(size=3,stroke=.5,alpha=1)+geom_point(aes(fill=sex),size=3,alpha=.3)+scale_x_date()+xlab("Recording Date")+ylab("# Songs Recorded")+geom_smooth(method="loess",aes(group=sex),span=2,size=1.1,show.legend=F,se=F)+scale_shape_manual(values=gpShps[1:2])+scale_fill_manual(values=c(malecol,femalecol))+scale_colour_manual(values=c(malecol,femalecol))


#Level A puzzle
#quartz()
(Gl1 <- G0+ggGalactic()+guides(shape = guide_legend(override.aes = list(size = 4)))+theme(legend.title=element_text(face="bold")))+ggtitle("Figure 1. A Mysterious Scatter Plot")

ggsave("NITgraph_level0_original.png",width=10,height=6)

Gl1+ggtitle("")+theme(plot.background = element_blank())
ggsave("scatterplot_forLessonBanner.png",bg="transparent",width=9,height=6)


#Level A&B puzzle: (+1) shift -1 decipher
(Gl2 <- ggCiphR(Gl1,1))+ggtitle("Figure 1. A Mysterious Scatter Plot")
ggsave("NITgraph_levelA&B_coded-axis-labels(-1_Decipher).png",width=10,height=6)

#Level C (-3) shift +3 decipher
(Gl3 <- ggCiphR(Gl1,-3)+ggtitle("Figure 1. A Mysterious Scatter Plot"))
ggsave("NITgraph_levelC_difficult-coded-axis-labels(+3_Decipher).png",width=10,height=6)

# #Level 4 
# K <- k
# enciphR("sex",3)
# K$sex<-factor(k$sex,labels=sapply(levels(k$sex),enciphR,3))
# 
(Gl4.0<-ggplot(data=K,aes(x=RecDt,y=N,col=sex,shape=sex))+geom_point(size=3,stroke=.5,alpha=1)+geom_point(aes(fill=sex),size=3,alpha=.3)+scale_x_date()+xlab("Recording Date")+ylab("# Songs Recorded")+geom_smooth(method="loess",aes(group=sex),span=2,size=1.1,show.legend=F,se=F)+scale_shape_manual(values=gpShps[1:2])+scale_fill_manual(values=c(malecol,femalecol))+scale_colour_manual(values=c(malecol,femalecol))+ggGalactic()+guides(shape = guide_legend(override.aes = list(size = 5)))+ggtitle("Figure 1. A Mysterious Scatter Plot"))
(Gl4<-ggCiphR(Gl4.0,3))
ggsave("NITgraph_levelD_Impossible-coded-axis-labels+key.png",width=10,height=6)



#############################
## Make HISTOGRAMS of female singing
#
#Data reorganization
k2<-melt(k[,c("ID","sex","N")],id=c("ID","sex"))
k3<-dcast(k2,formula=ID+sex~variable,fun.aggregate=sum,na.rm=T) %>% arrange(sex,N) #%>% filter(N>0)
k3
k3_anon<-k3[,c(1,3)]
names(k3_anon)[1]<-"BandNumber"
k3_anon$sex=factor(k3$sex,levels=c("Female","Male") ,labels=c("one","two"))
write.csv(k3_anon[,c(1,3,2)],"data/Table 1.SongOutputByID&Sex.csv",row.names = F)

#Make graphs for the answer key
xLabel="Number of Songs Recorded (N)"
yLabel="Count of Birds Who\nSang N Songs"
#blend color with white. Loooks niiiice ::)
fillCol=mixcolor( alpha=.7,hex2RGB(gpPal[1]),RGB(1,1,1)) %>% hex() #for pretty graphic
fillCol2<-gpPal2[2] #for high contrast (opaque)

######################################
# LESSON BANNER Histogram version
ggplot(k3,aes(x=N))+geom_histogram(breaks=seq(0,100,10),col=outlineCol,fill=fillCol,size=.6)+facet_grid(~sex)+ggGalactic()+labs(x=xLabel,y=yLabel,title="Figure 2. Histograms of Singing Output by Sex")+theme(strip.text=element_text(colour=1,size=22))+theme(panel.grid=element_line(size=2),plot.background=element_blank(),strip.background=element_rect(fill="gray92"),strip.text=element_text(colour=gpPal[6],size=22))+scale_x_continuous(minor_breaks=seq(0,100,10),breaks=seq(0,100,10))+scale_y_continuous(minor_breaks=seq(0,20,1),breaks=seq(0,20,5))+ggtitle("")+scale_x_continuous(breaks=seq(0,100,20),minor_breaks=NULL)+scale_y_continuous(breaks=seq(0,14,5),minor_breaks=NULL)+theme(panel.grid=element_line(size=.5))
ggsave("songOutputHistograms_bothplots_for banner.png",width=9,height=6,bg="transparent")
######################################


g2<-ggplot(k3,aes(x=N))+geom_histogram(breaks=seq(0,100,10),col=1,fill=fillCol2,size=.6)+facet_grid(~sex)+ggGalactic()+labs(x=xLabel,y=yLabel,title="Figure 2. Histograms of Singing Output by Sex")+theme(strip.text=element_text(colour=1,size=22))+theme(panel.grid=element_line(size=2),plot.background=element_blank(),strip.background=element_rect(fill="gray92"),strip.text=element_text(colour=gpPal[6],size=22))+scale_x_continuous(minor_breaks=seq(0,100,10),breaks=seq(0,100,10))+scale_y_continuous(minor_breaks=seq(0,20,1),breaks=seq(0,20,5))
g2+facet_grid(~sex,labeller=labeller(sex=c(Female="Sex 'One'",Male="Sex 'Two'")))
#The both plots (coded facets)

ggsave("songOutputHistograms_bothplots_coded facets.png",width=12,height=6,bg="transparent")





#now, w/ facets labeling One and two as Fem and Male
#More explicit answer with facets labeled

g2+
  facet_grid(~sex,labeller=labeller(sex=c(Female="Sex 'One' (Females)",Male="Sex 'Two' (Males)")))+scale_fill_manual(values="red")
ggsave("songOutputHistograms_bothplots+answer.png",width=12,height=6)


#Blank Plot
ggplot(k3,aes(x=N))+geom_histogram(breaks=seq(0,100,10),col="transparent",fill="transparent")+facet_grid(~sex,labeller=labeller(sex=c(Female="Sex 'One'",Male="Sex 'Two'")))+ggGalactic()+labs(x=xLabel,y=yLabel,title="Figure 2. Histograms of Singing Output by Sex")+theme(panel.grid=element_line(size=1),strip.background=element_rect(fill="gray90"),strip.text=element_text(colour=1,size=22))+scale_x_continuous(minor_breaks=seq(0,100,10),breaks=seq(0,100,10))+scale_y_continuous(minor_breaks=seq(0,20,1),breaks=seq(0,20,5))
ggsave("songOutputHistograms_blank.png",width=12,height=6)

#Blank Female, Male printed
ggplot(k3,aes(x=N,col=sex,fill=sex))+geom_histogram(breaks=seq(0,100,10),show.legend=F)+facet_grid(~sex,labeller=labeller(sex=c(Female="Sex 'One'",Male="Sex 'Two'")))+ggGalactic()+labs(x=xLabel,y=yLabel,title="Figure 2. Histograms of Singing Output by Sex")+theme(panel.grid=element_line(size=1),strip.background=element_rect(fill="gray90"),strip.text=element_text(colour=1,size=22))+scale_x_continuous(minor_breaks=seq(0,100,10),breaks=seq(0,100,10))+scale_y_continuous(minor_breaks=seq(0,20,1),breaks=seq(0,20,5))+scale_fill_manual(values=c("transparent",gpPal2[2]))+scale_colour_manual(values=c("transparent",1))
ggsave("songOutputHistograms_blankFem.png",width=12,height=6)




############
# Make combined histogram, scatterplot

#vertical orientation
(Gl1+ggtitle("Figure 1: A Scatter Plot")+theme(plot.title=element_text(size=18,face="plain")))/(g2+ggtitle("Figure 2: Histograms")+theme(plot.title=element_text(size=18,face="plain")))
ggsave("combined_vert_scatterplot+hist.png",width=12,height=12)

#horizontal orientation
(Gl1+ggtitle("Figure 1: A Scatter Plot")+theme(plot.title=element_text(size=18,face="plain")))+(g2+scale_x_continuous(minor_breaks=seq(0,100,10),breaks=seq(0,100,20))+ggtitle("Figure 2: Histograms")+theme(plot.title=element_text(size=18,face="plain")))
ggsave("combined_horiz_scatterplot+hist.png",width=18,height=6)
