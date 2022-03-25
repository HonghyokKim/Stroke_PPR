
Distance<-read.csv("ADD FOLDER NAME HERE/file/CensusTract_RefinaryDistance.csv",header=T)
##INPUT_FID = CensusTract Original FID
##NEAR_FID = Refinary FID

library(foreign)

CensusTract<-read.csv("ADD FOLDER NAME HERE/file/CensusTractCentroid.csv")
RefinaryInfo<-read.csv("ADD FOLDER NAME HERE/file/REFI_INFO.csv")
Pop<-read.csv("ADD FOLDER NAME HERE/file/POP.csv")

colnames(RefinaryInfo)[1]<-"NEAR_FID"
colnames(CensusTract)[11]<-"INPUT_FID"

Distance_dat<-merge(Distance,RefinaryInfo,by="NEAR_FID")
Distance_dat<-merge(Distance_dat,CensusTract,by="INPUT_FID")

Distance_dat$County_FIPS<-ifelse(Distance_dat$COUNTYFP<10,paste0(Distance_dat$STATEFP,"00",Distance_dat$COUNTYFP),
                                 ifelse(Distance_dat$COUNTYFP<100,paste0(Distance_dat$STATEFP,"0",Distance_dat$COUNTYFP),paste0(Distance_dat$STATEFP,Distance_dat$COUNTYFP)))
Distance_dat$County_FIPS<-as.numeric(Distance_dat$County_FIPS)

Distance_dat$STATE_FIPS<-ifelse(substr(Distance_dat$STATEFP,1,1)=="0",substr(Distance_dat$STATEFP,2,2),substr(Distance_dat$STATEFP,1,2))
Distance_dat$STATE_FIPS<-as.numeric(Distance_dat$STATE_FIPS)


CensusTract$County_FIPS<-ifelse(CensusTract$COUNTYFP<10,paste0(CensusTract$STATEFP,"00",CensusTract$COUNTYFP),
                                ifelse(CensusTract$COUNTYFP<100,paste0(CensusTract$STATEFP,"0",CensusTract$COUNTYFP),paste0(CensusTract$STATEFP,CensusTract$COUNTYFP)))
CensusTract$County_FIPS<-as.numeric(CensusTract$County_FIPS)


Distance_dat$Oil_Est<-(Distance_dat$Estimated_Oil_Production_2015+Distance_dat$Estimated_Oil_Production_2016+Distance_dat$Estimated_Oil_Production_2017)/3


Distance_dat$InverseSq_Distance <- 1/(Distance_dat$DISTANCE)^2

Distance_dat_50km <- subset(Distance_dat,DISTANCE<=50000)
Distance_dat_25km <- subset(Distance_dat,DISTANCE<=25000)
Distance_dat_11.1km <- subset(Distance_dat,DISTANCE<=11111)
Distance_dat_10km <- subset(Distance_dat,DISTANCE<=10000)
Distance_dat_5km <- subset(Distance_dat,DISTANCE<=5000)
Distance_dat_2.5km <- subset(Distance_dat,DISTANCE<=2500)

ExposureMetric_50km_TOP<-aggregate(Distance_dat_50km$Oil_Est * 1/(Distance_dat_50km$DISTANCE)^2 , by=list(Distance_dat_50km$INPUT_FID),FUN=sum)
ExposureMetric_50km_BOTTOM<-aggregate(1/(Distance_dat_50km$DISTANCE)^2, by=list(Distance_dat_50km$INPUT_FID),FUN=sum)
ExposureMetric_50km<-merge(ExposureMetric_50km_TOP,ExposureMetric_50km_BOTTOM,by="Group.1")
colnames(ExposureMetric_50km) <-c("INPUT_FID","TOP","BOTTOM")
ExposureMetric_50km$Metric_50km<-ExposureMetric_50km$TOP/ExposureMetric_50km$BOTTOM
ExposureMetric_50km<-ExposureMetric_50km[,c("INPUT_FID","Metric_50km")]

ExposureMetric_25km_TOP<-aggregate(Distance_dat_25km$Oil_Est * 1/(Distance_dat_25km$DISTANCE)^2 , by=list(Distance_dat_25km$INPUT_FID),FUN=sum)
ExposureMetric_25km_BOTTOM<-aggregate(1/(Distance_dat_25km$DISTANCE)^2, by=list(Distance_dat_25km$INPUT_FID),FUN=sum)
ExposureMetric_25km<-merge(ExposureMetric_25km_TOP,ExposureMetric_25km_BOTTOM,by="Group.1")
colnames(ExposureMetric_25km) <-c("INPUT_FID","TOP","BOTTOM")
ExposureMetric_25km$Metric_25km<-ExposureMetric_25km$TOP/ExposureMetric_25km$BOTTOM
ExposureMetric_25km<-ExposureMetric_25km[,c("INPUT_FID","Metric_25km")]


ExposureMetric_11.1km_TOP<-aggregate(Distance_dat_11.1km$Oil_Est * 1/(Distance_dat_11.1km$DISTANCE)^2 , by=list(Distance_dat_11.1km$INPUT_FID),FUN=sum)
ExposureMetric_11.1km_BOTTOM<-aggregate(1/(Distance_dat_11.1km$DISTANCE)^2, by=list(Distance_dat_11.1km$INPUT_FID),FUN=sum)
ExposureMetric_11.1km<-merge(ExposureMetric_11.1km_TOP,ExposureMetric_11.1km_BOTTOM,by="Group.1")
colnames(ExposureMetric_11.1km) <-c("INPUT_FID","TOP","BOTTOM")
ExposureMetric_11.1km$Metric_11.1km<-ExposureMetric_11.1km$TOP/ExposureMetric_11.1km$BOTTOM
ExposureMetric_11.1km<-ExposureMetric_11.1km[,c("INPUT_FID","Metric_11.1km")]


ExposureMetric_10km_TOP<-aggregate(Distance_dat_10km$Oil_Est * 1/(Distance_dat_10km$DISTANCE)^2 , by=list(Distance_dat_10km$INPUT_FID),FUN=sum)
ExposureMetric_10km_BOTTOM<-aggregate(1/(Distance_dat_10km$DISTANCE)^2, by=list(Distance_dat_10km$INPUT_FID),FUN=sum)
ExposureMetric_10km<-merge(ExposureMetric_10km_TOP,ExposureMetric_10km_BOTTOM,by="Group.1")
colnames(ExposureMetric_10km) <-c("INPUT_FID","TOP","BOTTOM")
ExposureMetric_10km$Metric_10km<-ExposureMetric_10km$TOP/ExposureMetric_10km$BOTTOM
ExposureMetric_10km<-ExposureMetric_10km[,c("INPUT_FID","Metric_10km")]

ExposureMetric_5km_TOP<-aggregate(Distance_dat_5km$Oil_Est * 1/(Distance_dat_5km$DISTANCE)^2 , by=list(Distance_dat_5km$INPUT_FID),FUN=sum)
ExposureMetric_5km_BOTTOM<-aggregate(1/(Distance_dat_5km$DISTANCE)^2, by=list(Distance_dat_5km$INPUT_FID),FUN=sum)
ExposureMetric_5km<-merge(ExposureMetric_5km_TOP,ExposureMetric_5km_BOTTOM,by="Group.1")
colnames(ExposureMetric_5km) <-c("INPUT_FID","TOP","BOTTOM")
ExposureMetric_5km$Metric_5km<-ExposureMetric_5km$TOP/ExposureMetric_5km$BOTTOM
ExposureMetric_5km<-ExposureMetric_5km[,c("INPUT_FID","Metric_5km")]

ExposureMetric_2.5km_TOP<-aggregate(Distance_dat_2.5km$Oil_Est * 1/(Distance_dat_2.5km$DISTANCE)^2 , by=list(Distance_dat_2.5km$INPUT_FID),FUN=sum)
ExposureMetric_2.5km_BOTTOM<-aggregate(1/(Distance_dat_2.5km$DISTANCE)^2, by=list(Distance_dat_2.5km$INPUT_FID),FUN=sum)
ExposureMetric_2.5km<-merge(ExposureMetric_2.5km_TOP,ExposureMetric_2.5km_BOTTOM,by="Group.1")
colnames(ExposureMetric_2.5km) <-c("INPUT_FID","TOP","BOTTOM")
ExposureMetric_2.5km$Metric_2.5km<-ExposureMetric_2.5km$TOP/ExposureMetric_2.5km$BOTTOM
ExposureMetric_2.5km<-ExposureMetric_2.5km[,c("INPUT_FID","Metric_2.5km")]

ExposureMetric<-merge(CensusTract,ExposureMetric_50km,by="INPUT_FID",all.x=T)
ExposureMetric<-merge(ExposureMetric,ExposureMetric_25km,by="INPUT_FID",all.x=T)
ExposureMetric<-merge(ExposureMetric,ExposureMetric_11.1km,by="INPUT_FID",all.x=T)
ExposureMetric<-merge(ExposureMetric,ExposureMetric_10km,by="INPUT_FID",all.x=T)
ExposureMetric<-merge(ExposureMetric,ExposureMetric_5km,by="INPUT_FID",all.x=T)
ExposureMetric<-merge(ExposureMetric,ExposureMetric_2.5km,by="INPUT_FID",all.x=T)

ExposureMetric$Metric_50km[is.na(ExposureMetric$Metric_50km)]<-0
ExposureMetric$Metric_25km[is.na(ExposureMetric$Metric_25km)]<-0
ExposureMetric$Metric_11.1km[is.na(ExposureMetric$Metric_11.1km)]<-0
ExposureMetric$Metric_10km[is.na(ExposureMetric$Metric_10km)]<-0
ExposureMetric$Metric_5km[is.na(ExposureMetric$Metric_5km)]<-0
ExposureMetric$Metric_2.5km[is.na(ExposureMetric$Metric_2.5km)]<-0


ExposureMetric$STATE_FIPS<-as.numeric(as.character(ExposureMetric$STATEFP))

ExposureMetric$LocationName<-ExposureMetric$GEOID
write.csv(ExposureMetric,"ADD FOLDER NAME HERE/file/Exposure_IDW.csv")













