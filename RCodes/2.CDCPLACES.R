

CDC_PLACES_CT<-read.csv("ADD FOLDER NAME HERE\\file\\PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2020_release.csv",header=T)


Measure<-CDC_PLACES_CT[!duplicated(CDC_PLACES_CT$Measure),]

HIGHCHOL
BPMED
BPHIGH
CSMOKING
BINGE	
DIABETES
OBESITY

HIGHCHOL <- subset(CDC_PLACES_CT,MeasureId=="HIGHCHOL" & Data_Value_Type=="Crude prevalence")
BPMED <- subset(CDC_PLACES_CT,MeasureId=="BPMED" & Data_Value_Type=="Crude prevalence")
BPHIGH <- subset(CDC_PLACES_CT,MeasureId=="BPHIGH" & Data_Value_Type=="Crude prevalence")
BINGE <- subset(CDC_PLACES_CT,MeasureId=="BINGE" & Data_Value_Type=="Crude prevalence")
CSMOKING <- subset(CDC_PLACES_CT,MeasureId=="CSMOKING" & Data_Value_Type=="Crude prevalence")
OBESITY <- subset(CDC_PLACES_CT,MeasureId=="OBESITY" & Data_Value_Type=="Crude prevalence")
DIABETES <- subset(CDC_PLACES_CT,MeasureId=="DIABETES" & Data_Value_Type=="Crude prevalence")
CASTHMA <- subset(CDC_PLACES_CT,MeasureId=="CASTHMA" & Data_Value_Type=="Crude prevalence")
COPD <- subset(CDC_PLACES_CT,MeasureId=="COPD" & Data_Value_Type=="Crude prevalence")
KIDNEY <- subset(CDC_PLACES_CT,MeasureId=="KIDNEY" & Data_Value_Type=="Crude prevalence")
CHD <- subset(CDC_PLACES_CT,MeasureId=="CHD" & Data_Value_Type=="Crude prevalence")
STROKE <- subset(CDC_PLACES_CT,MeasureId=="STROKE" & Data_Value_Type=="Crude prevalence")


HIGHCHOL<-HIGHCHOL[,c("CountyFIPS","LocationName","Data_Value")]
BPMED<-BPMED[,c("LocationName","Data_Value")]
BPHIGH<-BPHIGH[,c("LocationName","Data_Value")]
BINGE<-BINGE[,c("LocationName","Data_Value")]
CSMOKING<-CSMOKING[,c("LocationName","Data_Value")]
OBESITY<-OBESITY[,c("LocationName","Data_Value")]
DIABETES<-DIABETES[,c("LocationName","Data_Value")]
CASTHMA<-CASTHMA[,c("LocationName","Data_Value")]
COPD<-COPD[,c("LocationName","Data_Value")]
KIDNEY<-KIDNEY[,c("LocationName","Data_Value")]
CHD<-CHD[,c("LocationName","Data_Value")]
STROKE<-STROKE[,c("LocationName","Data_Value")]


colnames(HIGHCHOL)[3]<-"HIGHCHOL"
colnames(BPMED)[2]<-"BPMED"
colnames(BPHIGH)[2]<-"BPHIGH"
colnames(BINGE)[2]<-"BINGE"
colnames(CSMOKING)[2]<-"CSMOKING"
colnames(OBESITY)[2]<-"OBESITY"
colnames(DIABETES)[2]<-"DIABETES"
colnames(CASTHMA)[2]<-"CASTHMA"
colnames(COPD)[2]<-"COPD"
colnames(KIDNEY)[2]<-"KIDNEY"
colnames(CHD)[2]<-"CHD"
colnames(STROKE)[2]<-"STROKE"



PLACES<-merge(HIGHCHOL,BPMED,by="LocationName")
PLACES<-merge(PLACES,BPHIGH,by="LocationName")
PLACES<-merge(PLACES,BINGE,by="LocationName")
PLACES<-merge(PLACES,CSMOKING,by="LocationName")
PLACES<-merge(PLACES,OBESITY,by="LocationName")
PLACES<-merge(PLACES,DIABETES,by="LocationName")

PLACES<-merge(PLACES,CASTHMA,by="LocationName")
PLACES<-merge(PLACES,COPD,by="LocationName")
PLACES<-merge(PLACES,KIDNEY,by="LocationName")
PLACES<-merge(PLACES,CHD,by="LocationName")
PLACES<-merge(PLACES,STROKE,by="LocationName")


write.csv(PLACES,"ADD FOLDER NAME HERE\\file\\CDC_PLACES.csv")



