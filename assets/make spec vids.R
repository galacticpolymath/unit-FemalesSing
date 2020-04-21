source_url("https://raw.githubusercontent.com/drwilkins/rspectVid/master/rspectVid.R")
fspec<-testSpec("/Users/mattwilkins/GDrive/GP/Lessons/Assumptions Matter/assets/mw661183_nice female rec.WAV",yLim=c(.2,.8)) 

mspec<-testSpec("/Users/mattwilkins/GDrive/GP/Lessons/Assumptions Matter/assets/mw661252_nice male bout.WAV",yLim=c(0,8),filter=c(0,.6),wl=800,crop=F,xLim=3.6,ampTrans=3.,min_dB = -38)
rspectVid(mspec,vidName = "male barn swallow bout.mp4",delTemps=F)
