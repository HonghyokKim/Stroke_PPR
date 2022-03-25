library(mgcv)

findat<-read.csv("ADD FOLDER NAME HERE/file/Findat.csv")


findat$Pop_65_84_18over_pt<-findat$Pop_65_74_18over_pt+findat$Pop_75_84_18over_pt

findat$Male_18over_pt <- 100-findat$Female_18over_pt 
findat$Pop_18_19_18over_pt <- 100-
  (findat$Pop_20_24_18over_pt+findat$Pop_25_44_18over_pt+
  findat$Pop_45_64_18over_pt+findat$Pop_65_84_18over_pt+
  findat$Pop_85over_18over_pt)

all_vari<-c("Male_18over_pt","Female_18over_pt","Pop_18_19_18over_pt","Pop_20_24_18over_pt","Pop_25_44_18over_pt","Pop_45_64_18over_pt","Pop_65_84_18over_pt","Pop_85over_18over_pt",
            "Hispanic_pt","NH_White_pt","NH_Black_pt","EduLHigh_18over_pt","Poverty_18over_pt","Insured_19over_pt","MHI","CSMOKING","STROKE")
cor(findat[,all_vari],use="complete.obs")



findat$Buffer <- ifelse(findat$Metric_2.5km>0,1,ifelse(findat$Metric_5km>0,2,ifelse(findat$Metric_11.1km>0,3,ifelse(findat$Metric_25km>0,4,5))))


### Table 1 Mean
asd<-data.frame(round(
  rbind(colMeans(subset(findat,Buffer==1)[,all_vari],na.rm=T),
        colMeans(subset(findat,Buffer==2)[,all_vari],na.rm=T),
        colMeans(subset(findat,Buffer==3)[,all_vari],na.rm=T),
        colMeans(subset(findat,Buffer==4)[,all_vari],na.rm=T),
        colMeans(subset(findat,Buffer==5)[,all_vari],na.rm=T)
  ),1))

asd

### Table 1 Standard deviation
cbind(round(sqrt(diag(cov(findat[findat$Buffer==1,all_vari],use="complete.obs"))),1),
round(sqrt(diag(cov(findat[findat$Buffer==2,all_vari],use="complete.obs"))),1),
round(sqrt(diag(cov(findat[findat$Buffer==3,all_vari],use="complete.obs"))),1),
round(sqrt(diag(cov(findat[findat$Buffer==4,all_vari],use="complete.obs"))),1),
round(sqrt(diag(cov(findat[findat$Buffer==5,all_vari],use="complete.obs"))),1))



### Statistical Tests

x1<-summary(lm(Female_18over_pt~as.factor(Buffer),data=findat))
x2<-summary(lm(Pop_20_24_18over_pt~as.factor(Buffer),data=findat))
x3<-summary(lm(Pop_25_44_18over_pt~as.factor(Buffer),data=findat))
x4<-summary(lm(Pop_45_64_18over_pt~as.factor(Buffer),data=findat))
x5<-summary(lm(Pop_65_84_18over_pt~as.factor(Buffer),data=findat))
x6<-summary(lm(Pop_85over_18over_pt~as.factor(Buffer),data=findat))
x7<-summary(lm(Hispanic_pt~as.factor(Buffer),data=findat))
x8<-summary(lm(NH_White_pt~as.factor(Buffer),data=findat))
x9<-summary(lm(NH_Black_pt~as.factor(Buffer),data=findat))
x10<-summary(lm(EduLHigh_18over_pt~as.factor(Buffer),data=findat))
x11<-summary(lm(Poverty_18over_pt~as.factor(Buffer),data=findat))
x12<-summary(lm(Insured_19over_pt~as.factor(Buffer),data=findat))
x13<-summary(lm(MHI~as.factor(Buffer),data=findat))
x14<-summary(lm(CSMOKING~as.factor(Buffer),data=findat))
x15<-summary(lm(STROKE~as.factor(Buffer),data=findat))



round(rbind(pf(x1$fstatistic[1],x1$fstatistic[2],x1$fstatistic[3],lower.tail=FALSE),
pf(x2$fstatistic[1],x2$fstatistic[2],x2$fstatistic[3],lower.tail=FALSE),
pf(x3$fstatistic[1],x3$fstatistic[2],x3$fstatistic[3],lower.tail=FALSE),
pf(x4$fstatistic[1],x4$fstatistic[2],x4$fstatistic[3],lower.tail=FALSE),
pf(x5$fstatistic[1],x5$fstatistic[2],x5$fstatistic[3],lower.tail=FALSE),
pf(x6$fstatistic[1],x6$fstatistic[2],x6$fstatistic[3],lower.tail=FALSE),
pf(x7$fstatistic[1],x7$fstatistic[2],x7$fstatistic[3],lower.tail=FALSE),
pf(x8$fstatistic[1],x8$fstatistic[2],x8$fstatistic[3],lower.tail=FALSE),
pf(x9$fstatistic[1],x9$fstatistic[2],x9$fstatistic[3],lower.tail=FALSE),
pf(x10$fstatistic[1],x10$fstatistic[2],x10$fstatistic[3],lower.tail=FALSE),

pf(x11$fstatistic[1],x11$fstatistic[2],x11$fstatistic[3],lower.tail=FALSE),
pf(x12$fstatistic[1],x12$fstatistic[2],x12$fstatistic[3],lower.tail=FALSE),
pf(x13$fstatistic[1],x13$fstatistic[2],x13$fstatistic[3],lower.tail=FALSE),
pf(x14$fstatistic[1],x14$fstatistic[2],x14$fstatistic[3],lower.tail=FALSE),
pf(x15$fstatistic[1],x15$fstatistic[2],x15$fstatistic[3],lower.tail=FALSE)),3)


