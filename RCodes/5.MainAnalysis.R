library(wCorr)
library(dplyr)
library(gnm)
library(corrplot)
library(mgcv)
#devtools::install_github("HonghyokKim/CGPSspatialmatch")
library(CGPSspatialmatch)
'%>%' <- dplyr::'%>%'
mutate<- dplyr::mutate
group_by<- dplyr::group_by
first<-dplyr::first
slice<-dplyr::slice

findat<-read.csv("ADD FOLDER NAME HERE/file/Findat.csv")

findat$Pop_65_84_18over_pt<-findat$Pop_65_74_18over_pt+findat$Pop_75_84_18over_pt


all_vari<-c("PM25","Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
            "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","HIGHCHOL","BINGE","CSMOKING","BEDS","Latitude","Longitude")
cor(findat[,all_vari],use="complete.obs")


cor.data<-findat[,c(all_vari,"Metric_2.5km","Metric_5km","Metric_10km","Metric_25km","Metric_50km")]


cor.pvalue <- cor.mtest(cor.data)$p
cor.mat <- cor(cor.data, method="pearson", use="complete.obs")

cor.mat[lower.tri(cor.mat)]<-NA
cor.pvalue[upper.tri(cor.pvalue)]<-NA
rownames(cor.pvalue)<-rownames(cor.mat)<-colnames(cor.pvalue)<-colnames(cor.mat)


dat.run<-findat
dat.run$exposure<-ifelse(dat.run$Metric_2.5km>0,1,0)
dat.run$exposure_5km<-ifelse(dat.run$Metric_5km>0,1,0)
dat.run$exposure_10km<-ifelse(dat.run$Metric_10km>0,1,0)
table(subset(dat.run,exposure==1)$RUCACODE_2010)

dat.run$exposure_CC<-ifelse(dat.run$Metric_2.5km>0,1,ifelse(dat.run$Metric_5km==0,0,NA))

dat.run2<-subset(dat.run,exposure_CC>=0)

dat.run3<-subset(dat.run,exposure==0)

dat.run3$logMetric_5km<-log(dat.run3$Metric_5km)
dat.run2$logMetric_2.5km<-log(dat.run2$Metric_2.5km)
dat.run$logMetric_5km<-log(dat.run$Metric_5km)

PSdataset<-subset(dat.run2,Metric_10km>0)


Stroke_2.5km_GAM_Nearest_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                        long="Longitude",lat="Latitude",
                                                        PS.method="mgcv.GAM",
                                                        PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                        CGPS.formula="logMetric_2.5km~NH_Black_pt+Hispanic_pt+CSMOKING+s(Longitude,Latitude)",
                                                        smethod="nearest",smethod.replace=FALSE,
                                                        formulaDisease="STROKE~Metric_2.5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                        varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                   "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_2.5km_GAM_NearestReplacement_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                              long="Longitude",lat="Latitude",
                                              PS.method="mgcv.GAM",
                                              PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                              CGPS.formula="logMetric_2.5km~NH_Black_pt+Hispanic_pt+CSMOKING+s(Longitude,Latitude)",
                                              smethod="nearest",smethod.replace=TRUE,
                                              formulaDisease="STROKE~Metric_2.5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                              varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                         "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))




Stroke_2.5km_GAM_NearestCaliper0.5_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                              long="Longitude",lat="Latitude",
                                              PS.method="mgcv.GAM",
                                              PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                              CGPS.formula="logMetric_2.5km~NH_Black_pt+Hispanic_pt+CSMOKING+s(Longitude,Latitude)",
                                              smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=FALSE,
                                              formulaDisease="STROKE~Metric_2.5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                              varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                         "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_2.5km_GAM_NearestCaliper0.5Replacement_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                         long="Longitude",lat="Latitude",
                                                         PS.method="mgcv.GAM",
                                                         PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                         CGPS.formula="logMetric_2.5km~NH_Black_pt+Hispanic_pt+CSMOKING+s(Longitude,Latitude)",
                                                         smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=TRUE,
                                                         formulaDisease="STROKE~Metric_2.5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                         varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                    "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))









Stroke_2.5km_Binary_GAM_Nearest_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                              long="Longitude",lat="Latitude",
                                              PS.method="mgcv.GAM",
                                              PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                              CGPS.formula="logMetric_2.5km~1",
                                              smethod="nearest",smethod.replace=FALSE,
                                              formulaDisease="STROKE~exposure_CC",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                              varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                         "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



Stroke_2.5km_Binary_GAM_NearestReplacement_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                     long="Longitude",lat="Latitude",
                                                     PS.method="mgcv.GAM",
                                                     PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                     CGPS.formula="logMetric_2.5km~1",
                                                     smethod="nearest",smethod.replace=TRUE,
                                                     formulaDisease="STROKE~exposure_CC",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                     varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))


Stroke_2.5km_Binary_GAM_NearestCaliper0.2_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                     long="Longitude",lat="Latitude",
                                                     PS.method="mgcv.GAM",
                                                     PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                     CGPS.formula="logMetric_2.5km~1",
                                                     smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=FALSE,
                                                     formulaDisease="STROKE~exposure_CC",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                     varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))


Stroke_2.5km_Binary_GAM_NearestCaliper0.2Replacement_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                               long="Longitude",lat="Latitude",
                                                               PS.method="mgcv.GAM",
                                                               PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                               CGPS.formula="logMetric_2.5km~1",
                                                               smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=TRUE,
                                                               formulaDisease="STROKE~exposure_CC",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                               varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                          "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))


Stroke_2.5km_Binary_GAM_NearestCaliper0.5_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                               long="Longitude",lat="Latitude",
                                                               PS.method="mgcv.GAM",
                                                               PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                               CGPS.formula="logMetric_2.5km~1",
                                                               smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=FALSE,
                                                               formulaDisease="STROKE~exposure_CC",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                               varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                          "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_2.5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1<-estimateATT(dataset=dat.run2,PSdataset=PSdataset,bexp="exposure_CC",cexp="logMetric_2.5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                               long="Longitude",lat="Latitude",
                                                               PS.method="mgcv.GAM",
                                                               PS.formula="exposure_CC~Pop_20_24_18over_pt+Pop_25_44_18over_pt+Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                               CGPS.formula="logMetric_2.5km~1",
                                                               smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=TRUE,
                                                               formulaDisease="STROKE~exposure_CC",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                               varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                          "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))












PSdataset<-subset(dat.run,Metric_10km>0)


Stroke_5km_GAM_Nearest_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                            long="Longitude",lat="Latitude",
                                            PS.method="mgcv.GAM",
                                            PS.formula="exposure_5km~Pop_65_84_18over_pt +
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                            CGPS.formula="logMetric_5km~s(Longitude,Latitude)",
                                            smethod="nearest",caliper_bw=1,smethod.replace=FALSE,
                                            formulaDisease="STROKE~Metric_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                            varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                       "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))




Stroke_5km_GAM_NearestReplacement_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                            long="Longitude",lat="Latitude",
                                            PS.method="mgcv.GAM",
                                            PS.formula="exposure_5km~Pop_65_84_18over_pt +
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                            CGPS.formula="logMetric_5km~s(Longitude,Latitude)",
                                            smethod="nearest",caliper_bw=1,smethod.replace=TRUE,
                                            formulaDisease="STROKE~Metric_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                            varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                       "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))









Stroke_5km_GAM_NearestCaliper0.2_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                            long="Longitude",lat="Latitude",
                                            PS.method="mgcv.GAM",
                                            PS.formula="exposure_5km~Pop_65_84_18over_pt+Pop_85over_18over_pt +
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                            CGPS.formula="logMetric_5km~s(Longitude,Latitude)",
                                            smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=FALSE,
                                            formulaDisease="STROKE~Metric_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                            varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                       "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))




Stroke_5km_GAM_NearestCaliper0.2Replacement_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                      long="Longitude",lat="Latitude",
                                                      PS.method="mgcv.GAM",
                                                      PS.formula="exposure_5km~Pop_65_84_18over_pt+Pop_85over_18over_pt +
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                      CGPS.formula="logMetric_5km~s(Longitude,Latitude)",
                                                      smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=TRUE,
                                                      formulaDisease="STROKE~Metric_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                      varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                 "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))






Stroke_5km_GAM_NearestCaliper0.5_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                      long="Longitude",lat="Latitude",
                                                      PS.method="mgcv.GAM",
                                                      PS.formula="exposure_5km~Pop_65_84_18over_pt+Pop_85over_18over_pt +
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                      CGPS.formula="logMetric_5km~s(Longitude,Latitude)",
                                                      smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=FALSE,
                                                      formulaDisease="STROKE~Metric_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                      varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                 "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_5km_GAM_NearestCaliper0.5Replacement_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                                 long="Longitude",lat="Latitude",
                                                                 PS.method="mgcv.GAM",
                                                                 PS.formula="exposure_5km~Pop_65_84_18over_pt+Pop_85over_18over_pt +
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                                 CGPS.formula="logMetric_5km~s(Longitude,Latitude)",
                                                                 smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=TRUE,
                                                                 formulaDisease="STROKE~Metric_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                                 varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                            "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



Stroke_5km_Binary_GAM_Nearest_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                            long="Longitude",lat="Latitude",
                                            PS.method="mgcv.GAM",
                                            PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+Insured_19over_pt+Poverty_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                            CGPS.formula="logMetric_5km~1",
                                            smethod="nearest",caliper_bw=1,smethod.replace=FALSE,
                                            formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                            varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                       "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_5km_Binary_GAM_NearestReplacement_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                   long="Longitude",lat="Latitude",
                                                   PS.method="mgcv.GAM",
                                                   PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+Insured_19over_pt+Poverty_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                   CGPS.formula="logMetric_5km~1",
                                                   smethod="nearest",caliper_bw=1,smethod.replace=TRUE,
                                                   formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                   varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                              "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



Stroke_5km_Binary_GAM_NearestCaliper0.2_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                             long="Longitude",lat="Latitude",
                                                             PS.method="mgcv.GAM",
                                                             PS.formula="exposure_5km~Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Poverty_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                             CGPS.formula="logMetric_5km~1",
                                                             smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=FALSE,
                                                             formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                             varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                        "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_5km_Binary_GAM_NearestCaliper0.2Replacement_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                             long="Longitude",lat="Latitude",
                                                             PS.method="mgcv.GAM",
                                                             PS.formula="exposure_5km~Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Poverty_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                             CGPS.formula="logMetric_5km~1",
                                                             smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=TRUE,
                                                             formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                             varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                        "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))







Stroke_5km_Binary_GAM_NearestCaliper0.5_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                             long="Longitude",lat="Latitude",
                                                             PS.method="mgcv.GAM",
                                                             PS.formula="exposure_5km~Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Poverty_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                             CGPS.formula="logMetric_5km~1",
                                                             smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=FALSE,
                                                             formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                             varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                        "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

Stroke_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1<-estimateATT(dataset=dat.run,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                                        long="Longitude",lat="Latitude",
                                                                        PS.method="mgcv.GAM",
                                                                        PS.formula="exposure_5km~Pop_65_84_18over_pt+
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Poverty_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                                        CGPS.formula="logMetric_5km~1",
                                                                        smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=TRUE,
                                                                        formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                                        varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                                   "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))





PSdataset<-subset(dat.run3,Metric_10km>0)

Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1<-estimateATT(dataset=dat.run3,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                       long="Longitude",lat="Latitude",
                                                       PS.method="mgcv.GAM",
                                                       PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                       CGPS.formula="logMetric_5km~1",
                                                       smethod="nearest",caliper_bw=1,smethod.replace=FALSE,
                                                       formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                       varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                  "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



Stroke_2.5_5km_Binary_GAM_NearestReplacement_dist0.1<-estimateATT(dataset=dat.run3,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                       long="Longitude",lat="Latitude",
                                                       PS.method="mgcv.GAM",
                                                       PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                       CGPS.formula="logMetric_5km~1",
                                                       smethod="nearest",caliper_bw=1,smethod.replace=TRUE,
                                                       formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                       varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                  "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))







Stroke_2.5_5km_Binary_GAM_NearestCaliper0.2_dist0.1<-estimateATT(dataset=dat.run3,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                       long="Longitude",lat="Latitude",
                                                       PS.method="mgcv.GAM",
                                                       PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+Poverty_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                       CGPS.formula="logMetric_5km~1",
                                                       smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=FALSE,
                                                       formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                       varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                  "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



Stroke_2.5_5km_Binary_GAM_NearestCaliper0.2Replacement_dist0.1<-estimateATT(dataset=dat.run3,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                                  long="Longitude",lat="Latitude",
                                                                  PS.method="mgcv.GAM",
                                                                  PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                                  CGPS.formula="logMetric_5km~1",
                                                                  smethod="nearestcaliper",caliper_bw=0.2,smethod.replace=TRUE,
                                                                  formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                                  varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                             "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))







Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5_dist0.1<-estimateATT(dataset=dat.run3,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                                 long="Longitude",lat="Latitude",
                                                                 PS.method="mgcv.GAM",
                                                                 PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+Poverty_18over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                                 CGPS.formula="logMetric_5km~1",
                                                                 smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=FALSE,
                                                                 formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                                 varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                            "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1<-estimateATT(dataset=dat.run3,PSdataset=PSdataset,bexp="exposure_5km",cexp="logMetric_5km",fmethod.replace=TRUE,distbuf=0.1,exp.included=FALSE,
                                                                            long="Longitude",lat="Latitude",
                                                                            PS.method="mgcv.GAM",
                                                                            PS.formula="exposure_5km~
                                                         Hispanic_pt+NH_Black_pt+EduLHigh_18over_pt+Insured_19over_pt+MHI+CSMOKING+s(Longitude,Latitude)",
                                                                            CGPS.formula="logMetric_5km~1",
                                                                            smethod="nearestcaliper",caliper_bw=0.5,smethod.replace=TRUE,
                                                                            formulaDisease="STROKE~exposure_5km",family="gaussian",bs.N=1000,bs.replace=TRUE,
                                                                            varilist=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                                                       "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))




A2.5B<-Stroke_2.5km_Binary_GAM_Nearest_dist0.1$summary
B2.5B<-Stroke_2.5km_Binary_XGB_Nearest_dist0.1$summary
C2.5B<-Stroke_2.5km_Binary_GAM_NearestCaliper0.2_dist0.1$summary

A2.5_5B<-Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$summary
B2.5_5B<-Stroke_2.5_5km_Binary_XGB_Nearest_dist0.1$summary
C2.5_5B<-Stroke_2.5_5km_Binary_GAM_NearestCaliper0.2_dist0.1$summary


A5B<-Stroke_5km_Binary_GAM_Nearest_dist0.1$summary
B5B<-Stroke_5km_Binary_XGB_Nearest_dist0.1$summary
C5B<-Stroke_5km_Binary_GAM_NearestCaliper0.2_dist0.1$summary



cbind(rbind(round(A2.5B,2),
            round(B2.5B,2),
            round(C2.5B,2),
            round(A2.5_5B,2),
            round(B2.5_5B,2),
            round(C2.5_5B,2),
            round(A5B,2),
            round(B5B,2),
            round(C5B,2)),
      
      
      rbind(
        round(sum(abs(Stroke_2.5km_Binary_GAM_Nearest_dist0.1$smd.matched)),1),
        round(sum(abs(Stroke_2.5km_Binary_XGB_Nearest_dist0.1$smd.matched)),1),
        round(sum(abs(Stroke_2.5km_Binary_GAM_NearestCaliper0.2_dist0.1$smd.matched)),1),
        
        round(sum(abs(Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$smd.matched)),1),
        round(sum(abs(Stroke_2.5_5km_Binary_XGB_Nearest_dist0.1$smd.matched)),1),
        round(sum(abs(Stroke_2.5_5km_Binary_GAM_NearestCaliper0.2_dist0.1$smd.matched)),1),
        
        round(sum(abs(Stroke_5km_Binary_GAM_Nearest_dist0.1$smd.matched)),1),
        round(sum(abs(Stroke_5km_Binary_XGB_Nearest_dist0.1$smd.matched)),1),
        round(sum(abs(Stroke_5km_Binary_GAM_NearestCaliper0.2_dist0.1$smd.matched)),1)),
      
      rbind(round(100-Stroke_2.5km_Binary_GAM_Nearest_dist0.1$match.info[3],1),
            round(100-Stroke_2.5km_Binary_XGB_Nearest_dist0.1$match.info[3],1),
            round(100-Stroke_2.5km_Binary_GAM_NearestCaliper0.2_dist0.1$match.info[3],1),
            
            round(100-Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$match.info[3],1),
            round(100-Stroke_2.5_5km_Binary_XGB_Nearest_dist0.1$match.info[3],1),
            round(100-Stroke_2.5_5km_Binary_GAM_NearestCaliper0.2_dist0.1$match.info[3],1),
            
            round(100-Stroke_5km_Binary_GAM_Nearest_dist0.1$match.info[3],1),
            round(100-Stroke_5km_Binary_XGB_Nearest_dist0.1$match.info[3],1),
            round(100-Stroke_5km_Binary_GAM_NearestCaliper0.2_dist0.1$match.info[3],1))
)





###GET ASSOCIATION ESTIMATES (PER 1SD INCREASE of Xct) 

round(
  rbind(
    Stroke_2.5km_GAM_Nearest_dist0.1$summary*sd(subset(dat.run,exposure==1)$Metric_2.5km),
    Stroke_2.5km_GAM_NearestReplacement_dist0.1$summary*sd(subset(dat.run,exposure==1)$Metric_2.5km),
    Stroke_2.5km_GAM_NearestCaliper0.5_dist0.1$summary*sd(subset(dat.run,exposure==1)$Metric_2.5km),
    Stroke_2.5km_GAM_NearestCaliper0.5Replacement_dist0.1$summary*sd(subset(dat.run,exposure==1)$Metric_2.5km)),2)

round(
  rbind(
    Stroke_5km_GAM_Nearest_dist0.1$summary*sd(subset(dat.run,exposure_5km==1)$Metric_5km),
    Stroke_5km_GAM_NearestReplacement_dist0.1$summary*sd(subset(dat.run,exposure_5km==1)$Metric_5km),
    Stroke_5km_GAM_NearestCaliper0.5_dist0.1$summary*sd(subset(dat.run,exposure_5km==1)$Metric_5km),
    Stroke_5km_GAM_NearestCaliper0.5Replacement_dist0.1$summary*sd(subset(dat.run,exposure_5km==1)$Metric_5km)),2)

###GET AVERAGE SMD (COVARIATE BALANCE)

round(rbind(
sum(abs(Stroke_2.5km_GAM_Nearest_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5km_GAM_NearestReplacement_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5km_GAM_NearestCaliper0.5_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5km_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched)/14)),2)

round(
rbind(
sum(abs(Stroke_5km_GAM_Nearest_dist0.1$smd.matched)/14),
sum(abs(Stroke_5km_GAM_NearestReplacement_dist0.1$smd.matched)/14),
sum(abs(Stroke_5km_GAM_NearestCaliper0.5_dist0.1$smd.matched)/14),
sum(abs(Stroke_5km_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched)/14)),2)

###GET MATCHING INFORMATION

round(rbind(
  Stroke_2.5km_GAM_Nearest_dist0.1$match.info,
  Stroke_2.5km_GAM_NearestReplacement_dist0.1$match.info,
  Stroke_2.5km_GAM_NearestCaliper0.5_dist0.1$match.info,
  Stroke_2.5km_GAM_NearestCaliper0.5Replacement_dist0.1$match.info),1)



round(rbind(
  Stroke_5km_GAM_Nearest_dist0.1$match.info,
  Stroke_5km_GAM_NearestReplacement_dist0.1$match.info,
  Stroke_5km_GAM_NearestCaliper0.5_dist0.1$match.info,
  Stroke_5km_GAM_NearestCaliper0.5Replacement_dist0.1$match.info),1)


###GET ASSOCIATION ESTIMATES (PROXIMITY BINARY)

rbind(
round(
  rbind(
    Stroke_2.5km_Binary_GAM_Nearest_dist0.1$summary,
    Stroke_2.5km_Binary_GAM_NearestReplacement_dist0.1$summary,
    Stroke_2.5km_Binary_GAM_NearestCaliper0.5_dist0.1$summary,
    Stroke_2.5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$summary),2),

round(
  rbind(
    Stroke_5km_Binary_GAM_Nearest_dist0.1$summary,
    Stroke_5km_Binary_GAM_NearestReplacement_dist0.1$summary,
    Stroke_5km_Binary_GAM_NearestCaliper0.5_dist0.1$summary,
    Stroke_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$summary),2),

round(
  rbind(
    Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$summary,
    Stroke_2.5_5km_Binary_GAM_NearestReplacement_dist0.1$summary,
    Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5_dist0.1$summary,
    Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$summary),2))


###GET AVERAGE SMD (COVARIATE BALANCE)

round(
rbind(
sum(abs(Stroke_2.5km_Binary_GAM_Nearest_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5km_Binary_GAM_NearestReplacement_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5km_Binary_GAM_NearestCaliper0.5_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched)/14),


sum(abs(Stroke_5km_Binary_GAM_Nearest_dist0.1$smd.matched)/14),
sum(abs(Stroke_5km_Binary_GAM_NearestReplacement_dist0.1$smd.matched)/14),
sum(abs(Stroke_5km_Binary_GAM_NearestCaliper0.5_dist0.1$smd.matched)/14),
sum(abs(Stroke_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched)/14),


sum(abs(Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5_5km_Binary_GAM_NearestReplacement_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5_dist0.1$smd.matched)/14),
sum(abs(Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched)/14)),2)




###GET MATCHING INFORMATION
round(
  rbind(
    Stroke_2.5km_Binary_GAM_Nearest_dist0.1$match.info,
    Stroke_2.5km_Binary_GAM_NearestReplacement_dist0.1$match.info,
    Stroke_2.5km_Binary_GAM_NearestCaliper0.5_dist0.1$match.info,
    Stroke_2.5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$match.info,
    
    
    Stroke_5km_Binary_GAM_Nearest_dist0.1$match.info,
    Stroke_5km_Binary_GAM_NearestReplacement_dist0.1$match.info,
    Stroke_5km_Binary_GAM_NearestCaliper0.5_dist0.1$match.info,
    Stroke_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$match.info,
    
    
    Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$match.info,
    Stroke_2.5_5km_Binary_GAM_NearestReplacement_dist0.1$match.info,
    Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5_dist0.1$match.info,
    Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$match.info),1)



### COVARIATE BALANCE PLOTS
smdorg_2.5km<-CGPSspatialmatch::smd(subset(dat.run2,MHI>0),"exposure_CC",
                              varinames=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                          "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))



jpeg("ADD FOLDER NAME HERE/file/SMD2.5km_Continuous.jpg",width=10,height=6,pointsize=10,units="in",res=300)
par(mar=c(4,15,1,1))
plot(x=smdorg_2.5km,y=seq(14),bty="n",yaxt="n",ylab="",xlab="Standardized mean difference",xlim=c(-1,1),pch=1,col="magenta")
axis(2,at=seq(14),las = 2,
     
     c("% Female", "% 20-24 yrs","% 25-44 yrs","% 45-64 yrs","% 65-84 yrs","% 85+ yrs",
       "% Hispanic","% Non-Hispanic White","% Non-Hispanic Black",
       "% Low education","% Poverty", "% Insured","Median Household Income",
       "Smoking"),)
abline(v=c(-0.25,0.25),col="grey",lty=3)
abline(v=c(-0.1,0.1),col="grey",lty=2)
points(x=Stroke_2.5km_GAM_Nearest_dist0.1$smd.matched,y=seq(14),col="red",pch=2)
points(x=Stroke_2.5km_GAM_NearestReplacement_dist0.1$smd.matched,y=seq(14),col="orange",pch=4)
points(x=Stroke_2.5km_GAM_NearestCaliper0.5_dist0.1$smd.matched,y=seq(14),col="blue",pch=2)
points(x=Stroke_2.5km_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched,y=seq(14),col="skyblue",pch=4)
legend("bottomright",c("Original (Unmatched)",
                       "NNWoR",
                       "NNWR",
                       "NNCWoR",
                       "NNCWR"
),col=c("magenta","red","orange","blue","skyblue"),pch=c(1,2,4,2,4,2))
dev.off()



jpeg("ADD FOLDER NAME HERE/file/SMD2.5km_Binary.jpg",width=10,height=6,pointsize=10,units="in",res=300)
par(mar=c(4,15,1,1))
plot(x=smdorg_2.5km,y=seq(14),bty="n",yaxt="n",ylab="",xlab="Standardized mean difference",xlim=c(-1,1),pch=1,col="magenta")
axis(2,at=seq(14),las = 2,
     
     c("% Female", "% 20-24 yrs","% 25-44 yrs","% 45-64 yrs","% 65-84 yrs","% 85+ yrs",
       "% Hispanic","% Non-Hispanic White","% Non-Hispanic Black",
       "% Low education","% Poverty", "% Insured","Median Household Income",
       "Smoking"),)
abline(v=c(-0.25,0.25),col="grey",lty=3)
abline(v=c(-0.1,0.1),col="grey",lty=2)
points(x=Stroke_2.5km_Binary_GAM_Nearest_dist0.1$smd.matched,y=seq(14),col="red",pch=2)
points(x=Stroke_2.5km_Binary_GAM_NearestReplacement_dist0.1$smd.matched,y=seq(14),col="orange",pch=4)
points(x=Stroke_2.5km_Binary_GAM_NearestCaliper0.5_dist0.1$smd.matched,y=seq(14),col="blue",pch=2)
points(x=Stroke_2.5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched,y=seq(14),col="skyblue",pch=4)
legend("bottomright",c("Original (Unmatched)",
                       "NNWoR",
                       "NNWR",
                       "NNCWoR",
                       "NNCWR"
),col=c("magenta","red","orange","blue","skyblue"),pch=c(1,2,4,2,4,2))
dev.off()







smdorg_5km<-CGPSspatialmatch::smd(subset(dat.run,MHI>0),"exposure_5km",
                                    varinames=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                                "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))

jpeg("ADD FOLDER NAME HERE/file/SMD5km_Continuous.jpg",width=10,height=6,pointsize=10,units="in",res=300)
par(mar=c(4,15,1,1))
plot(x=smdorg_5km,y=seq(14),bty="n",yaxt="n",ylab="",xlab="Standardized mean difference",xlim=c(-1,1),pch=1,col="magenta")
axis(2,at=seq(14),las = 2,
     
     c("% Female", "% 20-24 yrs","% 25-44 yrs","% 45-64 yrs","% 65-84 yrs","% 85+ yrs",
       "% Hispanic","% Non-Hispanic White","% Non-Hispanic Black",
       "% Low education","% Poverty", "% Insured","Median Household Income",
       "Smoking"),)
abline(v=c(-0.25,0.25),col="grey",lty=3)
abline(v=c(-0.1,0.1),col="grey",lty=2)
points(x=Stroke_5km_GAM_Nearest_dist0.1$smd.matched,y=seq(14),col="red",pch=2)
points(x=Stroke_5km_GAM_NearestReplacement_dist0.1$smd.matched,y=seq(14),col="orange",pch=4)
points(x=Stroke_5km_GAM_NearestCaliper0.5_dist0.1$smd.matched,y=seq(14),col="blue",pch=2)
points(x=Stroke_5km_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched,y=seq(14),col="skyblue",pch=4)
legend("bottomright",c("Original (Unmatched)",
                       "NNWoR",
                       "NNWR",
                       "NNCWoR",
                       "NNCWR"
),col=c("magenta","red","orange","blue","skyblue"),pch=c(1,2,4,2,4,2))
dev.off()



jpeg("ADD FOLDER NAME HERE/file/SMD5km_Binary.jpg",width=10,height=6,pointsize=10,units="in",res=300)
par(mar=c(4,15,1,1))
plot(x=smdorg_5km,y=seq(14),bty="n",yaxt="n",ylab="",xlab="Standardized mean difference",xlim=c(-1,1),pch=1,col="magenta")
axis(2,at=seq(14),las = 2,
     
     c("% Female", "% 20-24 yrs","% 25-44 yrs","% 45-64 yrs","% 65-84 yrs","% 85+ yrs",
       "% Hispanic","% Non-Hispanic White","% Non-Hispanic Black",
       "% Low education","% Poverty", "% Insured","Median Household Income",
       "Smoking"),)
abline(v=c(-0.25,0.25),col="grey",lty=3)
abline(v=c(-0.1,0.1),col="grey",lty=2)
points(x=Stroke_5km_Binary_GAM_Nearest_dist0.1$smd.matched,y=seq(14),col="red",pch=2)
points(x=Stroke_5km_Binary_GAM_NearestReplacement_dist0.1$smd.matched,y=seq(14),col="orange",pch=4)
points(x=Stroke_5km_Binary_GAM_NearestCaliper0.5_dist0.1$smd.matched,y=seq(14),col="blue",pch=2)
points(x=Stroke_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched,y=seq(14),col="skyblue",pch=4)
legend("bottomright",c("Original (Unmatched)",
                       "NNWoR",
                       "NNWR",
                       "NNCWoR",
                       "NNCWR"
),col=c("magenta","red","orange","blue","skyblue"),pch=c(1,2,4,2,4,2))
dev.off()




smdorg_2.5_5km<-CGPSspatialmatch::smd(subset(dat.run3,MHI>0),"exposure_5km",
                                  varinames=c("Female_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
                                              "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING"))




jpeg("ADD FOLDER NAME HERE/file/SMD2.5_5km_Binary.jpg",width=10,height=6,pointsize=10,units="in",res=300)
par(mar=c(4,15,1,1))
plot(x=smdorg_2.5_5km,y=seq(14),bty="n",yaxt="n",ylab="",xlab="Standardized mean difference",xlim=c(-1,1),pch=1,col="magenta")
axis(2,at=seq(14),las = 2,
     
     c("% Female", "% 20-24 yrs","% 25-44 yrs","% 45-64 yrs","% 65-84 yrs","% 85+ yrs",
       "% Hispanic","% Non-Hispanic White","% Non-Hispanic Black",
       "% Low education","% Poverty", "% Insured","Median Household Income",
       "Smoking"),)
abline(v=c(-0.25,0.25),col="grey",lty=3)
abline(v=c(-0.1,0.1),col="grey",lty=2)
points(x=Stroke_2.5_5km_Binary_GAM_Nearest_dist0.1$smd.matched,y=seq(14),col="red",pch=2)
points(x=Stroke_2.5_5km_Binary_GAM_NearestReplacement_dist0.1$smd.matched,y=seq(14),col="orange",pch=4)
points(x=Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5_dist0.1$smd.matched,y=seq(14),col="blue",pch=2)
points(x=Stroke_2.5_5km_Binary_GAM_NearestCaliper0.5Replacement_dist0.1$smd.matched,y=seq(14),col="skyblue",pch=4)
legend("bottomright",c("Original (Unmatched)",
                       "NNWoR",
                       "NNWR",
                       "NNCWoR",
                       "NNCWR"
),col=c("magenta","red","orange","blue","skyblue"),pch=c(1,2,4,2,4,2))
dev.off()


