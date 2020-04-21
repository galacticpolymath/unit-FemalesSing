source_url("https://raw.githubusercontent.com/drwilkins/rspectVid/master/rspectVid.R")
#Little tinamou
ti<-testSpec("https://www.xeno-canto.org/sounds/uploaded/UOMSQHKZSS/XC344299-Crypturellus%20soui.mp3",xLim=3.5,crop=16,ampTrans=3,dest_folder = "/Users/mattwilkins/GDrive/GP/Lessons/Assumptions Matter/assets/OZtourAssets")
rspectVid(ti,vidName = "littleTinamou_XC344299")
