
ExposureMetric<-read.csv("ADD FOLDER NAME HERE/file/Exposure_IDW.csv")

PLACES<-read.csv("ADD FOLDER NAME HERE/file/CDC_PLACES.csv")
PLACES<-PLACES[,-1]


variables<-read.csv("ADD FOLDER NAME HERE/file/CensusTractDat.csv")

findat<-merge(variables,ExposureMetric,by="LocationName")
findat<-merge(findat,PLACES,by="LocationName",all.x=T)

findat<-findat[!is.na(findat$Female_pt),]
findat<-findat[!is.na(findat$Poverty_18over_pt),]
findat<-findat[!is.na(findat$STROKE),]

write.csv(findat,"ADD FOLDER NAME HERE/file/findat.csv")
