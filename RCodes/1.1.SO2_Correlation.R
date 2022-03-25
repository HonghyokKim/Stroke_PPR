'%!in%' <- function(x,y)!('%in%'(x,y))

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}
REFI_INFO<-read.csv("ADD FOLDER NAME HERE\\file\\REFI_INFO.csv")
SO2_AQS_INFO<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_AQS_INFO.csv")
SO2_AQS_REFI_DISTANCE<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_AQS_REFI_DISTANCE.csv")
SO2_AQS_REFI_DISTANCE<-merge(SO2_AQS_REFI_DISTANCE,REFI_INFO[,c("REFI_FID","AD_Mbpd_2015","AD_Mbpd_2016","AD_Mbpd_2017","Estimated_Oil_Production_2015","Estimated_Oil_Production_2016","Estimated_Oil_Production_2017")],by="REFI_FID")



ALABAMA_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_ALABAMA_2015.csv")
ALABAMA_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_ALABAMA_2016.csv")
ALABAMA_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_ALABAMA_2017.csv")

ARKANSAS_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_ARKANSAS_2015.csv")
ARKANSAS_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_ARKANSAS_2016.csv")
ARKANSAS_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_ARKANSAS_2017.csv")


LOUISIANA_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_LOUISIANA_2015.csv")
LOUISIANA_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_LOUISIANA_2016.csv")
LOUISIANA_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_LOUISIANA_2017.csv")


MISSISSIPPI_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_MISSISSIPPI_2015.csv")
MISSISSIPPI_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_MISSISSIPPI_2016.csv")
MISSISSIPPI_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_MISSISSIPPI_2017.csv")


NEWMEXICO_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_NEWMEXICO_2015.csv")
NEWMEXICO_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_NEWMEXICO_2016.csv")
NEWMEXICO_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_NEWMEXICO_2017.csv")


OKLAHOMA_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_OKLAHOMA_2015.csv")
OKLAHOMA_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_OKLAHOMA_2016.csv")
OKLAHOMA_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_OKLAHOMA_2017.csv")

TEXAS_SO2_2015<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_TEXAS_2015.csv")
TEXAS_SO2_2016<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_TEXAS_2016.csv")
TEXAS_SO2_2017<-read.csv("ADD FOLDER NAME HERE\\file\\SO2_TEXAS_2017.csv")




SO2_2015 <- rbind(
  ALABAMA_SO2_2015,
  ARKANSAS_SO2_2015,
  LOUISIANA_SO2_2015,
  MISSISSIPPI_SO2_2015,
  NEWMEXICO_SO2_2015,
  OKLAHOMA_SO2_2015,
  TEXAS_SO2_2015
)
SO2_2015$Date <- as.Date(SO2_2015$Date,format="%m/%d/%Y")
SO2_2015$SO2<-SO2_2015$Daily.Max.1.hour.SO2.Concentration
SO2_2015$SO2<-ifelse(SO2_2015$SO2<0,0,SO2_2015$SO2)
SO2_2015<-merge(SO2_2015,SO2_AQS_INFO[,c("AQS_FID","Site.ID")],by="Site.ID")   ## SOME AQS STATION WILL BE DELETED because too small number of measurements ###
SO2_2015_Site<-aggregate(SO2_2015$SO2,by=list(SO2_2015$AQS_FID),FUN=mean)
colnames(SO2_2015_Site)<-c("AQS_FID","SO2_2015")  ##ppb

SO2_2015_Site<-merge(SO2_2015_Site,SO2_AQS_INFO[,c("AQS_FID","Site.ID","STATE","COUNTY")],by="AQS_FID")

SO2_2016 <- rbind(
  ALABAMA_SO2_2016,
  ARKANSAS_SO2_2016,
  LOUISIANA_SO2_2016,
  MISSISSIPPI_SO2_2016,
  NEWMEXICO_SO2_2016,
  OKLAHOMA_SO2_2016,
  TEXAS_SO2_2016
)
SO2_2016$Date <- as.Date(SO2_2016$Date,format="%m/%d/%Y")
SO2_2016$SO2<-SO2_2016$Daily.Max.1.hour.SO2.Concentration
SO2_2016$SO2<-ifelse(SO2_2016$SO2<0,0,SO2_2016$SO2)
SO2_2016<-merge(SO2_2016,SO2_AQS_INFO[,c("AQS_FID","Site.ID")],by="Site.ID")   ## SOME AQS STATION WILL BE DELETED because too small number of measurements ###
SO2_2016_Site<-aggregate(SO2_2016$SO2,by=list(SO2_2016$AQS_FID),FUN=mean)
colnames(SO2_2016_Site)<-c("AQS_FID","SO2_2016")  ##ppb
SO2_2016_Site<-merge(SO2_2016_Site,SO2_AQS_INFO[,c("AQS_FID","Site.ID","STATE","COUNTY")],by="AQS_FID")


SO2_2017 <- rbind(
  ALABAMA_SO2_2017,
  ARKANSAS_SO2_2017,
  LOUISIANA_SO2_2017,
  MISSISSIPPI_SO2_2017,
  NEWMEXICO_SO2_2017,
  OKLAHOMA_SO2_2017,
  TEXAS_SO2_2017
)
SO2_2017$Date <- as.Date(SO2_2017$Date,format="%m/%d/%Y")
SO2_2017$SO2<-SO2_2017$Daily.Max.1.hour.SO2.Concentration
SO2_2017$SO2<-ifelse(SO2_2017$SO2<0,0,SO2_2017$SO2)
SO2_2017<-merge(SO2_2017,SO2_AQS_INFO[,c("AQS_FID","Site.ID")],by="Site.ID")   ## SOME AQS STATION WILL BE DELETED because too small number of measurements ###
SO2_2017_Site<-aggregate(SO2_2017$SO2,by=list(SO2_2017$AQS_FID),FUN=mean)
colnames(SO2_2017_Site)<-c("AQS_FID","SO2_2017")  ##ppb
SO2_2017_Site<-merge(SO2_2017_Site,SO2_AQS_INFO[,c("AQS_FID","Site.ID","STATE","COUNTY")],by="AQS_FID")





SO2_AQS_REFI_DISTANCE_2.5km <- subset(SO2_AQS_REFI_DISTANCE,DISTANCE<2500)
SO2_AQS_REFI_DISTANCE_5km <- subset(SO2_AQS_REFI_DISTANCE,DISTANCE<5000)
SO2_AQS_REFI_DISTANCE_10km <- subset(SO2_AQS_REFI_DISTANCE,DISTANCE<10000)
SO2_AQS_REFI_DISTANCE_25km <- subset(SO2_AQS_REFI_DISTANCE,DISTANCE<20000)
SO2_AQS_REFI_DISTANCE_50km <- subset(SO2_AQS_REFI_DISTANCE,DISTANCE<50000)


AD_MBPD_2.5km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_2.5km$AD_Mbpd_2015/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
AD_MBPD_2.5km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
AD_MBPD_2.5km_2015<-merge(AD_MBPD_2.5km_2015,AD_MBPD_2.5km_2015_DIST_SQ,by="Group.1")
AD_MBPD_2.5km_2015$AD_Mbpd_2015<-AD_MBPD_2.5km_2015$x.x/AD_MBPD_2.5km_2015$x.y
AD_MBPD_2.5km_2015<-AD_MBPD_2.5km_2015[,c(1,4)]
colnames(AD_MBPD_2.5km_2015)[1]<-"AQS_FID"
AD_MBPD_2.5km_2015 <- merge(AD_MBPD_2.5km_2015,SO2_2015_Site,by="AQS_FID")

AD_MBPD_5km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_5km$AD_Mbpd_2015/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
AD_MBPD_5km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
AD_MBPD_5km_2015<-merge(AD_MBPD_5km_2015,AD_MBPD_5km_2015_DIST_SQ,by="Group.1")
AD_MBPD_5km_2015$AD_Mbpd_2015<-AD_MBPD_5km_2015$x.x/AD_MBPD_5km_2015$x.y
AD_MBPD_5km_2015<-AD_MBPD_5km_2015[,c(1,4)]
colnames(AD_MBPD_5km_2015)[1]<-"AQS_FID"
AD_MBPD_5km_2015 <- merge(AD_MBPD_5km_2015,SO2_2015_Site,by="AQS_FID")

AD_MBPD_10km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_10km$AD_Mbpd_2015/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
AD_MBPD_10km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
AD_MBPD_10km_2015<-merge(AD_MBPD_10km_2015,AD_MBPD_10km_2015_DIST_SQ,by="Group.1")
AD_MBPD_10km_2015$AD_Mbpd_2015<-AD_MBPD_10km_2015$x.x/AD_MBPD_10km_2015$x.y
AD_MBPD_10km_2015<-AD_MBPD_10km_2015[,c(1,4)]
colnames(AD_MBPD_10km_2015)[1]<-"AQS_FID"
AD_MBPD_10km_2015 <- merge(AD_MBPD_10km_2015,SO2_2015_Site,by="AQS_FID")


AD_MBPD_25km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_25km$AD_Mbpd_2015/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
AD_MBPD_25km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
AD_MBPD_25km_2015<-merge(AD_MBPD_25km_2015,AD_MBPD_25km_2015_DIST_SQ,by="Group.1")
AD_MBPD_25km_2015$AD_Mbpd_2015<-AD_MBPD_25km_2015$x.x/AD_MBPD_25km_2015$x.y
AD_MBPD_25km_2015<-AD_MBPD_25km_2015[,c(1,4)]
colnames(AD_MBPD_25km_2015)[1]<-"AQS_FID"
AD_MBPD_25km_2015 <- merge(AD_MBPD_25km_2015,SO2_2015_Site,by="AQS_FID")


AD_MBPD_50km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_50km$AD_Mbpd_2015/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
AD_MBPD_50km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
AD_MBPD_50km_2015<-merge(AD_MBPD_50km_2015,AD_MBPD_50km_2015_DIST_SQ,by="Group.1")
AD_MBPD_50km_2015$AD_Mbpd_2015<-AD_MBPD_50km_2015$x.x/AD_MBPD_50km_2015$x.y
AD_MBPD_50km_2015<-AD_MBPD_50km_2015[,c(1,4)]
colnames(AD_MBPD_50km_2015)[1]<-"AQS_FID"
AD_MBPD_50km_2015 <- merge(AD_MBPD_50km_2015,SO2_2015_Site,by="AQS_FID")






AD_MBPD_2.5km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_2.5km$AD_Mbpd_2016/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
AD_MBPD_2.5km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
AD_MBPD_2.5km_2016<-merge(AD_MBPD_2.5km_2016,AD_MBPD_2.5km_2016_DIST_SQ,by="Group.1")
AD_MBPD_2.5km_2016$AD_Mbpd_2016<-AD_MBPD_2.5km_2016$x.x/AD_MBPD_2.5km_2016$x.y
AD_MBPD_2.5km_2016<-AD_MBPD_2.5km_2016[,c(1,4)]
colnames(AD_MBPD_2.5km_2016)[1]<-"AQS_FID"
AD_MBPD_2.5km_2016 <- merge(AD_MBPD_2.5km_2016,SO2_2016_Site,by="AQS_FID")

AD_MBPD_5km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_5km$AD_Mbpd_2016/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                              by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
AD_MBPD_5km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                      by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
AD_MBPD_5km_2016<-merge(AD_MBPD_5km_2016,AD_MBPD_5km_2016_DIST_SQ,by="Group.1")
AD_MBPD_5km_2016$AD_Mbpd_2016<-AD_MBPD_5km_2016$x.x/AD_MBPD_5km_2016$x.y
AD_MBPD_5km_2016<-AD_MBPD_5km_2016[,c(1,4)]
colnames(AD_MBPD_5km_2016)[1]<-"AQS_FID"
AD_MBPD_5km_2016 <- merge(AD_MBPD_5km_2016,SO2_2016_Site,by="AQS_FID")

AD_MBPD_10km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_10km$AD_Mbpd_2016/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
AD_MBPD_10km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
AD_MBPD_10km_2016<-merge(AD_MBPD_10km_2016,AD_MBPD_10km_2016_DIST_SQ,by="Group.1")
AD_MBPD_10km_2016$AD_Mbpd_2016<-AD_MBPD_10km_2016$x.x/AD_MBPD_10km_2016$x.y
AD_MBPD_10km_2016<-AD_MBPD_10km_2016[,c(1,4)]
colnames(AD_MBPD_10km_2016)[1]<-"AQS_FID"
AD_MBPD_10km_2016 <- merge(AD_MBPD_10km_2016,SO2_2016_Site,by="AQS_FID")


AD_MBPD_25km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_25km$AD_Mbpd_2016/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
AD_MBPD_25km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
AD_MBPD_25km_2016<-merge(AD_MBPD_25km_2016,AD_MBPD_25km_2016_DIST_SQ,by="Group.1")
AD_MBPD_25km_2016$AD_Mbpd_2016<-AD_MBPD_25km_2016$x.x/AD_MBPD_25km_2016$x.y
AD_MBPD_25km_2016<-AD_MBPD_25km_2016[,c(1,4)]
colnames(AD_MBPD_25km_2016)[1]<-"AQS_FID"
AD_MBPD_25km_2016 <- merge(AD_MBPD_25km_2016,SO2_2016_Site,by="AQS_FID")


AD_MBPD_50km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_50km$AD_Mbpd_2016/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
AD_MBPD_50km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
AD_MBPD_50km_2016<-merge(AD_MBPD_50km_2016,AD_MBPD_50km_2016_DIST_SQ,by="Group.1")
AD_MBPD_50km_2016$AD_Mbpd_2016<-AD_MBPD_50km_2016$x.x/AD_MBPD_50km_2016$x.y
AD_MBPD_50km_2016<-AD_MBPD_50km_2016[,c(1,4)]
colnames(AD_MBPD_50km_2016)[1]<-"AQS_FID"
AD_MBPD_50km_2016 <- merge(AD_MBPD_50km_2016,SO2_2016_Site,by="AQS_FID")






AD_MBPD_2.5km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_2.5km$AD_Mbpd_2017/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
AD_MBPD_2.5km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
AD_MBPD_2.5km_2017<-merge(AD_MBPD_2.5km_2017,AD_MBPD_2.5km_2017_DIST_SQ,by="Group.1")
AD_MBPD_2.5km_2017$AD_Mbpd_2017<-AD_MBPD_2.5km_2017$x.x/AD_MBPD_2.5km_2017$x.y
AD_MBPD_2.5km_2017<-AD_MBPD_2.5km_2017[,c(1,4)]
colnames(AD_MBPD_2.5km_2017)[1]<-"AQS_FID"
AD_MBPD_2.5km_2017 <- merge(AD_MBPD_2.5km_2017,SO2_2017_Site,by="AQS_FID")

AD_MBPD_5km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_5km$AD_Mbpd_2017/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                              by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
AD_MBPD_5km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                      by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
AD_MBPD_5km_2017<-merge(AD_MBPD_5km_2017,AD_MBPD_5km_2017_DIST_SQ,by="Group.1")
AD_MBPD_5km_2017$AD_Mbpd_2017<-AD_MBPD_5km_2017$x.x/AD_MBPD_5km_2017$x.y
AD_MBPD_5km_2017<-AD_MBPD_5km_2017[,c(1,4)]
colnames(AD_MBPD_5km_2017)[1]<-"AQS_FID"
AD_MBPD_5km_2017 <- merge(AD_MBPD_5km_2017,SO2_2017_Site,by="AQS_FID")

AD_MBPD_10km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_10km$AD_Mbpd_2017/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
AD_MBPD_10km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
AD_MBPD_10km_2017<-merge(AD_MBPD_10km_2017,AD_MBPD_10km_2017_DIST_SQ,by="Group.1")
AD_MBPD_10km_2017$AD_Mbpd_2017<-AD_MBPD_10km_2017$x.x/AD_MBPD_10km_2017$x.y
AD_MBPD_10km_2017<-AD_MBPD_10km_2017[,c(1,4)]
colnames(AD_MBPD_10km_2017)[1]<-"AQS_FID"
AD_MBPD_10km_2017 <- merge(AD_MBPD_10km_2017,SO2_2017_Site,by="AQS_FID")


AD_MBPD_25km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_25km$AD_Mbpd_2017/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
AD_MBPD_25km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
AD_MBPD_25km_2017<-merge(AD_MBPD_25km_2017,AD_MBPD_25km_2017_DIST_SQ,by="Group.1")
AD_MBPD_25km_2017$AD_Mbpd_2017<-AD_MBPD_25km_2017$x.x/AD_MBPD_25km_2017$x.y
AD_MBPD_25km_2017<-AD_MBPD_25km_2017[,c(1,4)]
colnames(AD_MBPD_25km_2017)[1]<-"AQS_FID"
AD_MBPD_25km_2017 <- merge(AD_MBPD_25km_2017,SO2_2017_Site,by="AQS_FID")


AD_MBPD_50km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_50km$AD_Mbpd_2017/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
AD_MBPD_50km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
AD_MBPD_50km_2017<-merge(AD_MBPD_50km_2017,AD_MBPD_50km_2017_DIST_SQ,by="Group.1")
AD_MBPD_50km_2017$AD_Mbpd_2017<-AD_MBPD_50km_2017$x.x/AD_MBPD_50km_2017$x.y
AD_MBPD_50km_2017<-AD_MBPD_50km_2017[,c(1,4)]
colnames(AD_MBPD_50km_2017)[1]<-"AQS_FID"
AD_MBPD_50km_2017 <- merge(AD_MBPD_50km_2017,SO2_2017_Site,by="AQS_FID")














Estimated_Oil_Production_2.5km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_2.5km$Estimated_Oil_Production_2015/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_2.5km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_2.5km_2015<-merge(Estimated_Oil_Production_2.5km_2015,Estimated_Oil_Production_2.5km_2015_DIST_SQ,by="Group.1")
Estimated_Oil_Production_2.5km_2015$Estimated_Oil_Production_2015<-Estimated_Oil_Production_2.5km_2015$x.x/Estimated_Oil_Production_2.5km_2015$x.y
Estimated_Oil_Production_2.5km_2015<-Estimated_Oil_Production_2.5km_2015[,c(1,4)]
colnames(Estimated_Oil_Production_2.5km_2015)[1]<-"AQS_FID"
Estimated_Oil_Production_2.5km_2015 <- merge(Estimated_Oil_Production_2.5km_2015,SO2_2015_Site,by="AQS_FID")

Estimated_Oil_Production_5km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_5km$Estimated_Oil_Production_2015/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                              by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_5km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                      by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_5km_2015<-merge(Estimated_Oil_Production_5km_2015,Estimated_Oil_Production_5km_2015_DIST_SQ,by="Group.1")
Estimated_Oil_Production_5km_2015$Estimated_Oil_Production_2015<-Estimated_Oil_Production_5km_2015$x.x/Estimated_Oil_Production_5km_2015$x.y
Estimated_Oil_Production_5km_2015<-Estimated_Oil_Production_5km_2015[,c(1,4)]
colnames(Estimated_Oil_Production_5km_2015)[1]<-"AQS_FID"
Estimated_Oil_Production_5km_2015 <- merge(Estimated_Oil_Production_5km_2015,SO2_2015_Site,by="AQS_FID")

Estimated_Oil_Production_10km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_10km$Estimated_Oil_Production_2015/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
Estimated_Oil_Production_10km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
Estimated_Oil_Production_10km_2015<-merge(Estimated_Oil_Production_10km_2015,Estimated_Oil_Production_10km_2015_DIST_SQ,by="Group.1")
Estimated_Oil_Production_10km_2015$Estimated_Oil_Production_2015<-Estimated_Oil_Production_10km_2015$x.x/Estimated_Oil_Production_10km_2015$x.y
Estimated_Oil_Production_10km_2015<-Estimated_Oil_Production_10km_2015[,c(1,4)]
colnames(Estimated_Oil_Production_10km_2015)[1]<-"AQS_FID"
Estimated_Oil_Production_10km_2015 <- merge(Estimated_Oil_Production_10km_2015,SO2_2015_Site,by="AQS_FID")


Estimated_Oil_Production_25km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_25km$Estimated_Oil_Production_2015/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
Estimated_Oil_Production_25km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
Estimated_Oil_Production_25km_2015<-merge(Estimated_Oil_Production_25km_2015,Estimated_Oil_Production_25km_2015_DIST_SQ,by="Group.1")
Estimated_Oil_Production_25km_2015$Estimated_Oil_Production_2015<-Estimated_Oil_Production_25km_2015$x.x/Estimated_Oil_Production_25km_2015$x.y
Estimated_Oil_Production_25km_2015<-Estimated_Oil_Production_25km_2015[,c(1,4)]
colnames(Estimated_Oil_Production_25km_2015)[1]<-"AQS_FID"
Estimated_Oil_Production_25km_2015 <- merge(Estimated_Oil_Production_25km_2015,SO2_2015_Site,by="AQS_FID")


Estimated_Oil_Production_50km_2015 <- aggregate(SO2_AQS_REFI_DISTANCE_50km$Estimated_Oil_Production_2015/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
Estimated_Oil_Production_50km_2015_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
Estimated_Oil_Production_50km_2015<-merge(Estimated_Oil_Production_50km_2015,Estimated_Oil_Production_50km_2015_DIST_SQ,by="Group.1")
Estimated_Oil_Production_50km_2015$Estimated_Oil_Production_2015<-Estimated_Oil_Production_50km_2015$x.x/Estimated_Oil_Production_50km_2015$x.y
Estimated_Oil_Production_50km_2015<-Estimated_Oil_Production_50km_2015[,c(1,4)]
colnames(Estimated_Oil_Production_50km_2015)[1]<-"AQS_FID"
Estimated_Oil_Production_50km_2015 <- merge(Estimated_Oil_Production_50km_2015,SO2_2015_Site,by="AQS_FID")






Estimated_Oil_Production_2.5km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_2.5km$Estimated_Oil_Production_2016/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_2.5km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_2.5km_2016<-merge(Estimated_Oil_Production_2.5km_2016,Estimated_Oil_Production_2.5km_2016_DIST_SQ,by="Group.1")
Estimated_Oil_Production_2.5km_2016$Estimated_Oil_Production_2016<-Estimated_Oil_Production_2.5km_2016$x.x/Estimated_Oil_Production_2.5km_2016$x.y
Estimated_Oil_Production_2.5km_2016<-Estimated_Oil_Production_2.5km_2016[,c(1,4)]
colnames(Estimated_Oil_Production_2.5km_2016)[1]<-"AQS_FID"
Estimated_Oil_Production_2.5km_2016 <- merge(Estimated_Oil_Production_2.5km_2016,SO2_2016_Site,by="AQS_FID")

Estimated_Oil_Production_5km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_5km$Estimated_Oil_Production_2016/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                              by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_5km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                      by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_5km_2016<-merge(Estimated_Oil_Production_5km_2016,Estimated_Oil_Production_5km_2016_DIST_SQ,by="Group.1")
Estimated_Oil_Production_5km_2016$Estimated_Oil_Production_2016<-Estimated_Oil_Production_5km_2016$x.x/Estimated_Oil_Production_5km_2016$x.y
Estimated_Oil_Production_5km_2016<-Estimated_Oil_Production_5km_2016[,c(1,4)]
colnames(Estimated_Oil_Production_5km_2016)[1]<-"AQS_FID"
Estimated_Oil_Production_5km_2016 <- merge(Estimated_Oil_Production_5km_2016,SO2_2016_Site,by="AQS_FID")

Estimated_Oil_Production_10km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_10km$Estimated_Oil_Production_2016/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
Estimated_Oil_Production_10km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
Estimated_Oil_Production_10km_2016<-merge(Estimated_Oil_Production_10km_2016,Estimated_Oil_Production_10km_2016_DIST_SQ,by="Group.1")
Estimated_Oil_Production_10km_2016$Estimated_Oil_Production_2016<-Estimated_Oil_Production_10km_2016$x.x/Estimated_Oil_Production_10km_2016$x.y
Estimated_Oil_Production_10km_2016<-Estimated_Oil_Production_10km_2016[,c(1,4)]
colnames(Estimated_Oil_Production_10km_2016)[1]<-"AQS_FID"
Estimated_Oil_Production_10km_2016 <- merge(Estimated_Oil_Production_10km_2016,SO2_2016_Site,by="AQS_FID")


Estimated_Oil_Production_25km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_25km$Estimated_Oil_Production_2016/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
Estimated_Oil_Production_25km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
Estimated_Oil_Production_25km_2016<-merge(Estimated_Oil_Production_25km_2016,Estimated_Oil_Production_25km_2016_DIST_SQ,by="Group.1")
Estimated_Oil_Production_25km_2016$Estimated_Oil_Production_2016<-Estimated_Oil_Production_25km_2016$x.x/Estimated_Oil_Production_25km_2016$x.y
Estimated_Oil_Production_25km_2016<-Estimated_Oil_Production_25km_2016[,c(1,4)]
colnames(Estimated_Oil_Production_25km_2016)[1]<-"AQS_FID"
Estimated_Oil_Production_25km_2016 <- merge(Estimated_Oil_Production_25km_2016,SO2_2016_Site,by="AQS_FID")


Estimated_Oil_Production_50km_2016 <- aggregate(SO2_AQS_REFI_DISTANCE_50km$Estimated_Oil_Production_2016/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
Estimated_Oil_Production_50km_2016_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
Estimated_Oil_Production_50km_2016<-merge(Estimated_Oil_Production_50km_2016,Estimated_Oil_Production_50km_2016_DIST_SQ,by="Group.1")
Estimated_Oil_Production_50km_2016$Estimated_Oil_Production_2016<-Estimated_Oil_Production_50km_2016$x.x/Estimated_Oil_Production_50km_2016$x.y
Estimated_Oil_Production_50km_2016<-Estimated_Oil_Production_50km_2016[,c(1,4)]
colnames(Estimated_Oil_Production_50km_2016)[1]<-"AQS_FID"
Estimated_Oil_Production_50km_2016 <- merge(Estimated_Oil_Production_50km_2016,SO2_2016_Site,by="AQS_FID")






Estimated_Oil_Production_2.5km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_2.5km$Estimated_Oil_Production_2017/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_2.5km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_2.5km$DISTANCE)^2,
                                        by=list(SO2_AQS_REFI_DISTANCE_2.5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_2.5km_2017<-merge(Estimated_Oil_Production_2.5km_2017,Estimated_Oil_Production_2.5km_2017_DIST_SQ,by="Group.1")
Estimated_Oil_Production_2.5km_2017$Estimated_Oil_Production_2017<-Estimated_Oil_Production_2.5km_2017$x.x/Estimated_Oil_Production_2.5km_2017$x.y
Estimated_Oil_Production_2.5km_2017<-Estimated_Oil_Production_2.5km_2017[,c(1,4)]
colnames(Estimated_Oil_Production_2.5km_2017)[1]<-"AQS_FID"
Estimated_Oil_Production_2.5km_2017 <- merge(Estimated_Oil_Production_2.5km_2017,SO2_2017_Site,by="AQS_FID")

Estimated_Oil_Production_5km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_5km$Estimated_Oil_Production_2017/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                              by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_5km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_5km$DISTANCE)^2,
                                      by=list(SO2_AQS_REFI_DISTANCE_5km$AQS_FID),FUN=sum)
Estimated_Oil_Production_5km_2017<-merge(Estimated_Oil_Production_5km_2017,Estimated_Oil_Production_5km_2017_DIST_SQ,by="Group.1")
Estimated_Oil_Production_5km_2017$Estimated_Oil_Production_2017<-Estimated_Oil_Production_5km_2017$x.x/Estimated_Oil_Production_5km_2017$x.y
Estimated_Oil_Production_5km_2017<-Estimated_Oil_Production_5km_2017[,c(1,4)]
colnames(Estimated_Oil_Production_5km_2017)[1]<-"AQS_FID"
Estimated_Oil_Production_5km_2017 <- merge(Estimated_Oil_Production_5km_2017,SO2_2017_Site,by="AQS_FID")

Estimated_Oil_Production_10km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_10km$Estimated_Oil_Production_2017/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
Estimated_Oil_Production_10km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_10km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_10km$AQS_FID),FUN=sum)
Estimated_Oil_Production_10km_2017<-merge(Estimated_Oil_Production_10km_2017,Estimated_Oil_Production_10km_2017_DIST_SQ,by="Group.1")
Estimated_Oil_Production_10km_2017$Estimated_Oil_Production_2017<-Estimated_Oil_Production_10km_2017$x.x/Estimated_Oil_Production_10km_2017$x.y
Estimated_Oil_Production_10km_2017<-Estimated_Oil_Production_10km_2017[,c(1,4)]
colnames(Estimated_Oil_Production_10km_2017)[1]<-"AQS_FID"
Estimated_Oil_Production_10km_2017 <- merge(Estimated_Oil_Production_10km_2017,SO2_2017_Site,by="AQS_FID")


Estimated_Oil_Production_25km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_25km$Estimated_Oil_Production_2017/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
Estimated_Oil_Production_25km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_25km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_25km$AQS_FID),FUN=sum)
Estimated_Oil_Production_25km_2017<-merge(Estimated_Oil_Production_25km_2017,Estimated_Oil_Production_25km_2017_DIST_SQ,by="Group.1")
Estimated_Oil_Production_25km_2017$Estimated_Oil_Production_2017<-Estimated_Oil_Production_25km_2017$x.x/Estimated_Oil_Production_25km_2017$x.y
Estimated_Oil_Production_25km_2017<-Estimated_Oil_Production_25km_2017[,c(1,4)]
colnames(Estimated_Oil_Production_25km_2017)[1]<-"AQS_FID"
Estimated_Oil_Production_25km_2017 <- merge(Estimated_Oil_Production_25km_2017,SO2_2017_Site,by="AQS_FID")


Estimated_Oil_Production_50km_2017 <- aggregate(SO2_AQS_REFI_DISTANCE_50km$Estimated_Oil_Production_2017/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                               by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
Estimated_Oil_Production_50km_2017_DIST_SQ <- aggregate(1/(SO2_AQS_REFI_DISTANCE_50km$DISTANCE)^2,
                                       by=list(SO2_AQS_REFI_DISTANCE_50km$AQS_FID),FUN=sum)
Estimated_Oil_Production_50km_2017<-merge(Estimated_Oil_Production_50km_2017,Estimated_Oil_Production_50km_2017_DIST_SQ,by="Group.1")
Estimated_Oil_Production_50km_2017$Estimated_Oil_Production_2017<-Estimated_Oil_Production_50km_2017$x.x/Estimated_Oil_Production_50km_2017$x.y
Estimated_Oil_Production_50km_2017<-Estimated_Oil_Production_50km_2017[,c(1,4)]
colnames(Estimated_Oil_Production_50km_2017)[1]<-"AQS_FID"
Estimated_Oil_Production_50km_2017 <- merge(Estimated_Oil_Production_50km_2017,SO2_2017_Site,by="AQS_FID")

































Estimated_Oil_Production_2.5km_2015<-subset(Estimated_Oil_Production_2.5km_2015,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_5km_2015<-subset(Estimated_Oil_Production_5km_2015,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_10km_2015<-subset(Estimated_Oil_Production_10km_2015,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_25km_2015<-subset(Estimated_Oil_Production_25km_2015,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_50km_2015<-subset(Estimated_Oil_Production_50km_2015,AQS_FID %!in% c(24, 55,56))




Estimated_Oil_Production_2.5km_2016<-subset(Estimated_Oil_Production_2.5km_2016,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_5km_2016<-subset(Estimated_Oil_Production_5km_2016,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_10km_2016<-subset(Estimated_Oil_Production_10km_2016,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_25km_2016<-subset(Estimated_Oil_Production_25km_2016,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_50km_2016<-subset(Estimated_Oil_Production_50km_2016,AQS_FID %!in% c(24, 55,56))




Estimated_Oil_Production_2.5km_2017<-subset(Estimated_Oil_Production_2.5km_2017,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_5km_2017<-subset(Estimated_Oil_Production_5km_2017,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_10km_2017<-subset(Estimated_Oil_Production_10km_2017,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_25km_2017<-subset(Estimated_Oil_Production_25km_2017,AQS_FID %!in% c(24, 55,56))
Estimated_Oil_Production_50km_2017<-subset(Estimated_Oil_Production_50km_2017,AQS_FID %!in% c(24, 55,56))



tiff("ADD FOLDER NAME HERE\\file\\SO2.tiff",units="in",res=300,width=12,height=8,compression="zip")
par(mfrow=c(3,5))
plot(Estimated_Oil_Production_2.5km_2015$Estimated_Oil_Production_2015,Estimated_Oil_Production_2.5km_2015$SO2_2015,xlab="Estimated_Oil_Production 2015",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_2.5km_2015$SO2_2015~Estimated_Oil_Production_2.5km_2015$Estimated_Oil_Production_2015)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="Alabama")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="red",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="green",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2015,STATE=="Texas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="blue",pch=16)
lines(x=Estimated_Oil_Production_2.5km_2015$Estimated_Oil_Production_2015,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("2.5km (2015)")

plot(Estimated_Oil_Production_5km_2015$Estimated_Oil_Production_2015,Estimated_Oil_Production_5km_2015$SO2_2015,xlab="Estimated_Oil_Production 2015",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_5km_2015$SO2_2015~Estimated_Oil_Production_5km_2015$Estimated_Oil_Production_2015)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="Alabama")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="red",pch=16)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="green",pch=16)
d<-subset(Estimated_Oil_Production_5km_2015,STATE=="Texas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="blue",pch=16)
lines(x=Estimated_Oil_Production_5km_2015$Estimated_Oil_Production_2015,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("5km (2015)")


plot(Estimated_Oil_Production_10km_2015$Estimated_Oil_Production_2015,Estimated_Oil_Production_10km_2015$SO2_2015,xlab="Estimated_Oil_Production 2015",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_10km_2015$SO2_2015~Estimated_Oil_Production_10km_2015$Estimated_Oil_Production_2015)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="Alabama")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="red",pch=16)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="green",pch=16)
d<-subset(Estimated_Oil_Production_10km_2015,STATE=="Texas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="blue",pch=16)
lines(x=Estimated_Oil_Production_10km_2015$Estimated_Oil_Production_2015,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("10km (2015)")



plot(Estimated_Oil_Production_25km_2015$Estimated_Oil_Production_2015,Estimated_Oil_Production_25km_2015$SO2_2015,xlab="Estimated_Oil_Production 2015",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_25km_2015$SO2_2015~Estimated_Oil_Production_25km_2015$Estimated_Oil_Production_2015)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="Alabama")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="red",pch=16)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="green",pch=16)
d<-subset(Estimated_Oil_Production_25km_2015,STATE=="Texas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="blue",pch=16)
lines(x=Estimated_Oil_Production_25km_2015$Estimated_Oil_Production_2015,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("25km (2015)")



plot(Estimated_Oil_Production_50km_2015$Estimated_Oil_Production_2015,Estimated_Oil_Production_50km_2015$SO2_2015,xlab="Estimated_Oil_Production 2015",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_50km_2015$SO2_2015~Estimated_Oil_Production_50km_2015$Estimated_Oil_Production_2015)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="Alabama")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="red",pch=16)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="green",pch=16)
d<-subset(Estimated_Oil_Production_50km_2015,STATE=="Texas")
points(d$Estimated_Oil_Production_2015,d$SO2_2015,col="blue",pch=16)
lines(x=Estimated_Oil_Production_50km_2015$Estimated_Oil_Production_2015,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("50km (2015)")

















plot(Estimated_Oil_Production_2.5km_2016$Estimated_Oil_Production_2016,Estimated_Oil_Production_2.5km_2016$SO2_2016,xlab="Estimated_Oil_Production 2016",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_2.5km_2016$SO2_2016~Estimated_Oil_Production_2.5km_2016$Estimated_Oil_Production_2016)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="Alabama")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="red",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="green",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2016,STATE=="Texas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="blue",pch=16)
lines(x=Estimated_Oil_Production_2.5km_2016$Estimated_Oil_Production_2016,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("2.5km (2016)")







plot(Estimated_Oil_Production_5km_2016$Estimated_Oil_Production_2016,Estimated_Oil_Production_5km_2016$SO2_2016,xlab="Estimated_Oil_Production 2016",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_5km_2016$SO2_2016~Estimated_Oil_Production_5km_2016$Estimated_Oil_Production_2016)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="Alabama")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="red",pch=16)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="green",pch=16)
d<-subset(Estimated_Oil_Production_5km_2016,STATE=="Texas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="blue",pch=16)
lines(x=Estimated_Oil_Production_5km_2016$Estimated_Oil_Production_2016,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("5km (2016)")


plot(Estimated_Oil_Production_10km_2016$Estimated_Oil_Production_2016,Estimated_Oil_Production_10km_2016$SO2_2016,xlab="Estimated_Oil_Production 2016",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_10km_2016$SO2_2016~Estimated_Oil_Production_10km_2016$Estimated_Oil_Production_2016)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="Alabama")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="red",pch=16)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="green",pch=16)
d<-subset(Estimated_Oil_Production_10km_2016,STATE=="Texas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="blue",pch=16)
lines(x=Estimated_Oil_Production_10km_2016$Estimated_Oil_Production_2016,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("10km (2016)")


plot(Estimated_Oil_Production_25km_2016$Estimated_Oil_Production_2016,Estimated_Oil_Production_25km_2016$SO2_2016,xlab="Estimated_Oil_Production 2016",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_25km_2016$SO2_2016~Estimated_Oil_Production_25km_2016$Estimated_Oil_Production_2016)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="Alabama")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="red",pch=16)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="green",pch=16)
d<-subset(Estimated_Oil_Production_25km_2016,STATE=="Texas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="blue",pch=16)
lines(x=Estimated_Oil_Production_25km_2016$Estimated_Oil_Production_2016,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("25km (2016)")



plot(Estimated_Oil_Production_50km_2016$Estimated_Oil_Production_2016,Estimated_Oil_Production_50km_2016$SO2_2016,xlab="Estimated_Oil_Production 2016",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_50km_2016$SO2_2016~Estimated_Oil_Production_50km_2016$Estimated_Oil_Production_2016)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="Alabama")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="red",pch=16)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="green",pch=16)
d<-subset(Estimated_Oil_Production_50km_2016,STATE=="Texas")
points(d$Estimated_Oil_Production_2016,d$SO2_2016,col="blue",pch=16)
lines(x=Estimated_Oil_Production_50km_2016$Estimated_Oil_Production_2016,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("50km (2016)")







plot(Estimated_Oil_Production_2.5km_2017$Estimated_Oil_Production_2017,Estimated_Oil_Production_2.5km_2017$SO2_2017,xlab="Estimated_Oil_Production 2017",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_2.5km_2017$SO2_2017~Estimated_Oil_Production_2.5km_2017$Estimated_Oil_Production_2017)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="Alabama")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="red",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="green",pch=16)
d<-subset(Estimated_Oil_Production_2.5km_2017,STATE=="Texas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="blue",pch=16)
lines(x=Estimated_Oil_Production_2.5km_2017$Estimated_Oil_Production_2017,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("2.5km (2017)")







plot(Estimated_Oil_Production_5km_2017$Estimated_Oil_Production_2017,Estimated_Oil_Production_5km_2017$SO2_2017,xlab="Estimated_Oil_Production 2017",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_5km_2017$SO2_2017~Estimated_Oil_Production_5km_2017$Estimated_Oil_Production_2017)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="Alabama")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="red",pch=16)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="green",pch=16)
d<-subset(Estimated_Oil_Production_5km_2017,STATE=="Texas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="blue",pch=16)
lines(x=Estimated_Oil_Production_5km_2017$Estimated_Oil_Production_2017,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("5km (2017)")


plot(Estimated_Oil_Production_10km_2017$Estimated_Oil_Production_2017,Estimated_Oil_Production_10km_2017$SO2_2017,xlab="Estimated_Oil_Production 2017",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_10km_2017$SO2_2017~Estimated_Oil_Production_10km_2017$Estimated_Oil_Production_2017)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="Alabama")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="red",pch=16)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="green",pch=16)
d<-subset(Estimated_Oil_Production_10km_2017,STATE=="Texas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="blue",pch=16)
lines(x=Estimated_Oil_Production_10km_2017$Estimated_Oil_Production_2017,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("10km (2017)")


plot(Estimated_Oil_Production_25km_2017$Estimated_Oil_Production_2017,Estimated_Oil_Production_25km_2017$SO2_2017,xlab="Estimated_Oil_Production 2017",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_25km_2017$SO2_2017~Estimated_Oil_Production_25km_2017$Estimated_Oil_Production_2017)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="Alabama")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="red",pch=16)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="green",pch=16)
d<-subset(Estimated_Oil_Production_25km_2017,STATE=="Texas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="blue",pch=16)
lines(x=Estimated_Oil_Production_25km_2017$Estimated_Oil_Production_2017,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("25km (2017)")



plot(Estimated_Oil_Production_50km_2017$Estimated_Oil_Production_2017,Estimated_Oil_Production_50km_2017$SO2_2017,xlab="Estimated_Oil_Production 2017",ylab="SO2 (ppb)", bty="l",pch=16,col="orange")
fit<-lm(Estimated_Oil_Production_50km_2017$SO2_2017~Estimated_Oil_Production_50km_2017$Estimated_Oil_Production_2017)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="Alabama")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="red",pch=16)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="Arkansas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkred",pch=16)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="Louisiana")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="orange",pch=16)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="Mississippi")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="yellow",pch=16)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="New Mexico")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="darkgreen",pch=16)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="Oklahoma")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="green",pch=16)
d<-subset(Estimated_Oil_Production_50km_2017,STATE=="Texas")
points(d$Estimated_Oil_Production_2017,d$SO2_2017,col="blue",pch=16)
lines(x=Estimated_Oil_Production_50km_2017$Estimated_Oil_Production_2017,y=fitted.values(fit),col="red",lwd=2)
  legend("topright",paste0("Corr: ",round(sqrt(summary(fit)$r.squared),2)), bty="n")
title("50km (2017)")
add_legend("top",c("Alabama","Arkansas","Louisiana","Mississippi","New Mexico","Oklahoma","Texas"),col=c("red","darkred","orange","yellow","darkgreen","green","blue"),bty="n",pch=16,ncol=7)
dev.off()












