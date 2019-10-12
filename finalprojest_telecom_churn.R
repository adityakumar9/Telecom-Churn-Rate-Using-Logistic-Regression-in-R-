# install.packages("lpSolve")
# install.packages("matrix")

library(dplyr)
library(irr)
library(car)
library(ggplot2)
library(caret)
library(gains)
library(ROCR)
library(prediction)


# set location ------------------------------------------------------------

setwd("C:\\Users\\Shubham\\Downloads\\data _Science\\09_Final Project\\")
getwd()

telecom<- read.csv("sampletelecomfinal.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE)

# set Iteration -----------------------------------------------------------

names(telecom)

?checkDataQuality
?paste
?tempdir()

num.file <- paste(tempdir(), "/dqr_num.csv", sep= "")

cat.file <- paste(tempdir(), "/dqr_cat.csv", sep= "")

checkDataQuality(data= telecom,  out.file.num= num.file, out.file.cat= cat.file)

summary(telecom)

head(telecom)

str(telecom)

class(telecom)




# find churn rate for categorical variable -----------------------------------------------------
names(telecom)
options(scipen = 999)
summary(telecom)


## income 
telecom%>%count(churn,levels=income)%>%filter(churn==1)->datC1
datC1$N<-unclass(telecom%>%filter(income%in%datC1$levels)%>%count(income))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("income",nrow(datC1))
datC1


## models
telecom%>%count(churn,levels=models)%>%filter(churn==1)->datC2
datC2$N<-unclass(telecom%>%filter(models%in%datC2$levels)%>%count(models))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("models",nrow(datC2))
datC2


## actvsubs
telecom%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC3
datC3$N<-unclass(telecom%>%filter(actvsubs%in%datC3$levels)%>%count(actvsubs))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("actvsubs",nrow(datC3))
datC3


## uniqsubs 
telecom%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC31
datC31$N<-unclass(telecom%>%filter(uniqsubs%in%datC31$levels)%>%count(uniqsubs))[[2]]
datC31$ChurnPerc<-datC31$n/datC31$N
datC31$Var.Name<-rep("uniqsubs",nrow(datC31))
datC31


## forgntvl  
telecom%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC4
datC4$N<-unclass(telecom%>%filter(forgntvl%in%datC4$levels)%>%count(forgntvl))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N
datC4$Var.Name<-rep("forgntvl",nrow(datC4))
datC4


## mtrcycle 
telecom%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC5
datC5$N<-unclass(telecom%>%filter(mtrcycle%in%datC5$levels)%>%count(mtrcycle))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N
datC5$Var.Name<-rep("mtrcycle",nrow(datC5))
datC5


## numbcars 
telecom%>%count(churn,levels=numbcars)%>%filter(churn==1)->datC6
datC6$N<-unclass(telecom%>%filter(numbcars%in%datC6$levels)%>%count(numbcars))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N
datC6$Var.Name<-rep("numbcars",nrow(datC6))
datC6

## truck  
telecom%>%count(churn,levels=truck)%>%filter(churn==1)->datC7
datC7$N<-unclass(telecom%>%filter(truck%in%datC7$levels)%>%count(truck))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N
datC7$Var.Name<-rep("truck",nrow(datC7))
datC7



## crclscod
telecom%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC8
datC8$N<-unclass(telecom%>%filter(crclscod%in%datC8$levels)%>%count(crclscod))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N
datC8$Var.Name<-rep("crclscod",nrow(datC8))
datC8


## asl_flag =  IN
telecom%>%count(churn,levels=asl_flag)%>%filter(churn==1)->datC9
datC9$N<-unclass(telecom%>%filter(asl_flag%in%datC9$levels)%>%count(asl_flag))[[2]]
datC9$ChurnPerc<-datC9$n/datC9$N
datC9$Var.Name<-rep("asl_flag",nrow(datC9))
datC9


## prizm_social_one
telecom%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->datC10
datC10$N<-unclass(telecom%>%filter(prizm_social_one%in%datC10$levels)%>%count(prizm_social_one))[[2]]
datC10$ChurnPerc<-datC10$n/datC10$N
datC10$Var.Name<-rep("prizm_social_one",nrow(datC10))
datC10


## area =  IN
telecom%>%count(churn,levels=area)%>%filter(churn==1)->datC11
datC11$N<-unclass(telecom%>%filter(area%in%datC11$levels)%>%count(area))[[2]]
datC11$ChurnPerc<-datC11$n/datC11$N
datC11$Var.Name<-rep("area",nrow(datC11))
datC11

                        ## cut function in area varibale
datC11$area<-cut(datC11$ChurnPerc,4, labels = c("East","West", "North", "South"))


## refurb_new =  IN
telecom%>%count(churn,levels=refurb_new)%>%filter(churn==1)->datC12
datC12$N<-unclass(telecom%>%filter(refurb_new%in%datC12$levels)%>%count(refurb_new))[[2]]
datC12$ChurnPerc<-datC12$n/datC12$N
datC12$Var.Name<-rep("refurb_new",nrow(datC12))
datC12


## hnd_webcap
telecom%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->datC13
datC13$N<-unclass(telecom%>%filter(hnd_webcap%in%datC13$levels)%>%count(hnd_webcap))[[2]]
datC13$ChurnPerc<-datC13$n/datC13$N
datC13$Var.Name<-rep("hnd_webcap",nrow(datC13))
datC13

## marital =  IN
telecom%>%count(churn,levels=marital)%>%filter(churn==1)->datC14
datC14$N<-unclass(telecom%>%filter(marital%in%datC14$levels)%>%count(marital))[[2]]
datC14$ChurnPerc<-datC14$n/datC14$N
datC14$Var.Name<-rep("marital",nrow(datC14))
datC14


## ethnic
telecom%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC15
datC15$N<-unclass(telecom%>%filter(ethnic%in%datC15$levels)%>%count(ethnic))[[2]]
datC15$ChurnPerc<-datC15$n/datC15$N
datC15$Var.Name<-rep("ethnic",nrow(datC15))
datC15

## car_buy
telecom%>%count(churn,levels=car_buy)%>%filter(churn==1)->datC16
datC16$N<-unclass(telecom%>%filter(car_buy%in%datC16$levels)%>%count(car_buy))[[2]]
datC16$ChurnPerc<-datC16$n/datC16$N
datC16$Var.Name<-rep("car_buy",nrow(datC16))
datC16

## children
telecom%>%count(churn,levels=children)%>%filter(churn==1)->datC17
datC17$N<-unclass(telecom%>%filter(children%in%datC17$levels)%>%count(children))[[2]]
datC17$ChurnPerc<-datC17$n/datC17$N
datC17$Var.Name<-rep("children",nrow(datC17))
datC17


## div_type
telecom%>%count(churn,levels=div_type)%>%filter(churn==1)->datC18
datC18$N<-unclass(telecom%>%filter(div_type%in%datC18$levels)%>%count(div_type))[[2]]
datC18$ChurnPerc<-datC18$n/datC18$N
datC18$Var.Name<-rep("div_type",nrow(datC18))
datC18


## dwlltype
telecom%>%count(churn,levels=dwlltype)%>%filter(churn==1)->datC19
datC19$N<-unclass(telecom%>%filter(dwlltype%in%datC19$levels)%>%count(dwlltype))[[2]]
datC19$ChurnPerc<-datC19$n/datC19$N
datC19$Var.Name<-rep("dwlltype",nrow(datC19))
datC19

## dwllsize
telecom%>%count(churn,levels=dwllsize)%>%filter(churn==1)->datC20
datC20$N<-unclass(telecom%>%filter(dwllsize%in%datC20$levels)%>%count(dwllsize))[[2]]
datC20$ChurnPerc<-datC20$n/datC20$N
datC20$Var.Name<-rep("dwllsize",nrow(datC20))
datC20

## mailordr
telecom%>%count(churn,levels=mailordr)%>%filter(churn==1)->datC21
datC21$N<-unclass(telecom%>%filter(mailordr%in%datC21$levels)%>%count(mailordr))[[2]]
datC21$ChurnPerc<-datC21$n/datC21$N
datC21$Var.Name<-rep("mailordr",nrow(datC21))
datC21


## occu1
telecom%>%count(churn,levels=occu1)%>%filter(churn==1)->datC22
datC22$N<-unclass(telecom%>%filter(occu1%in%datC22$levels)%>%count(occu1))[[2]]
datC22$ChurnPerc<-datC22$n/datC22$N
datC22$Var.Name<-rep("occu1",nrow(datC22))
datC22


## wrkwoman
telecom%>%count(churn,levels=wrkwoman)%>%filter(churn==1)->datC23
datC23$N<-unclass(telecom%>%filter(wrkwoman%in%datC23$levels)%>%count(wrkwoman))[[2]]
datC23$ChurnPerc<-datC23$n/datC23$N
datC23$Var.Name<-rep("wrkwoman",nrow(datC23))
datC23


## proptype
telecom%>%count(churn,levels=proptype)%>%filter(churn==1)->datC24
datC24$N<-unclass(telecom%>%filter(proptype%in%datC24$levels)%>%count(proptype))[[2]]
datC24$ChurnPerc<-datC24$n/datC24$N
datC24$Var.Name<-rep("proptype",nrow(datC24))
datC24

## mailresp
telecom%>%count(churn,levels=mailresp)%>%filter(churn==1)->datC25
datC25$N<-unclass(telecom%>%filter(mailresp%in%datC25$levels)%>%count(mailresp))[[2]]
datC25$ChurnPerc<-datC25$n/datC25$N
datC25$Var.Name<-rep("mailresp",nrow(datC25))
datC25

## cartype
telecom%>%count(churn,levels=cartype)%>%filter(churn==1)->datC26
datC26$N<-unclass(telecom%>%filter(cartype%in%datC26$levels)%>%count(cartype))[[2]]
datC26$ChurnPerc<-datC26$n/datC26$N
datC26$Var.Name<-rep("cartype",nrow(datC26))
datC26



# find churn rate for Continous variable  ------------------------------------------------------

## age
telecom%>%mutate(dec=ntile(age,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(age,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(age,n=10))%>%group_by(dec)%>%summarise(min(age)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(age,n=10))%>%group_by(dec)%>%summarise(max(age)))[[2]]
dat_num1$varname<-rep("age",nrow(dat_num1))
dat_num1


## mou_Mean
telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dat_num1$varname<-rep("mou_Mean",nrow(dat_num1))
dat_num1

## totmrc_Mean
telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dat_num1$varname<-rep("totmrc_Mean",nrow(dat_num1))
dat_num1

## rev_Range
telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dat_num1$varname<-rep("rev_Range",nrow(dat_num1))
dat_num1

## mou_Range
telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dat_num1$varname<-rep("mou_Range",nrow(dat_num1))
dat_num1

## change_mou
telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dat_num1$varname<-rep("change_mou",nrow(dat_num1))
dat_num1

## drop_blk_Mean
telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dat_num1$varname<-rep("drop_blk_Mean",nrow(dat_num1))
dat_num1


dat_num1$lev<- cut(dat_num1$churn_perc, 3, labels = c("L","M","H"))


## drop_vce_Range
telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dat_num1$varname<-rep("drop_vce_Range",nrow(dat_num1))
dat_num1

dat_num1$lev<- cut(dat_num1$churn_perc, 3, labels = c("L","M","H"))

## owylis_vce_Range
telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dat_num1$varname<-rep("owylis_vce_Range",nrow(dat_num1))
dat_num1

## mou_opkv_Range
telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dat_num1$varname<-rep("mou_opkv_Range",nrow(dat_num1))
dat_num1

## months
telecom%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dat_num1$varname<-rep("months",nrow(dat_num1))
dat_num1

## totcalls
telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dat_num1$varname<-rep("totcalls",nrow(dat_num1))
dat_num1

## eqpdays
telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
dat_num1$varname<-rep("eqpdays",nrow(dat_num1))
dat_num1

## custcare_Mean
telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]
dat_num1$varname<-rep("custcare_Mean",nrow(dat_num1))
dat_num1

## callwait_Mean
telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
dat_num1$varname<-rep("callwait_Mean",nrow(dat_num1))
dat_num1

## iwylis_vce_Mean
telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
dat_num1$varname<-rep("iwylis_vce_Mean",nrow(dat_num1))
dat_num1

## callwait_Range
telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
dat_num1$varname<-rep("callwait_Range",nrow(dat_num1))
dat_num1

## ccrndmou_Range
telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
dat_num1$varname<-rep("ccrndmou_Range",nrow(dat_num1))
dat_num1

## adjqty
telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dat_num1$varname<-rep("adjqty",nrow(dat_num1))
dat_num1

## ovrrev_Mean
telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
dat_num1$varname<-rep("ovrrev_Mean",nrow(dat_num1))
dat_num1

## rev_Mean
telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dat_num1$varname<-rep("rev_Mean",nrow(dat_num1))
dat_num1

## ovrmou_Mean
telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
dat_num1$varname<-rep("ovrmou_Mean",nrow(dat_num1))
dat_num1

## comp_vce_Mean
telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dat_num1$varname<-rep("comp_vce_Mean",nrow(dat_num1))
dat_num1

## plcd_vce_Mean
telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dat_num1$varname<-rep("plcd_vce_Mean",nrow(dat_num1))
dat_num1

## avg3mou
telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
dat_num1$varname<-rep("avg3mou",nrow(dat_num1))
dat_num1

## avgmou
telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dat_num1$varname<-rep("avgmou",nrow(dat_num1))
dat_num1


## avg3qty
telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
dat_num1$varname<-rep("avg3qty",nrow(dat_num1))
dat_num1


## avgqty
telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dat_num1$varname<-rep("avgqty",nrow(dat_num1))
dat_num1

## avg6mou
telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
dat_num1$varname<-rep("avg6mou",nrow(dat_num1))
dat_num1

## avg6qty
telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
dat_num1$varname<-rep("avg6qty",nrow(dat_num1))
dat_num1

## age1
telecom%>%mutate(dec=ntile(age1,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(age1,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(age1,n=10))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(age1,n=10))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
dat_num1$varname<-rep("age1",nrow(dat_num1))
dat_num1


## age2
telecom%>%mutate(dec=ntile(age2,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(age2,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(age2,n=10))%>%group_by(dec)%>%summarise(min(age2)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(age2,n=10))%>%group_by(dec)%>%summarise(max(age2)))[[2]]
dat_num1$varname<-rep("age2",nrow(dat_num1))
dat_num1


## hnd_price
telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
dat_num1$varname<-rep("hnd_price",nrow(dat_num1))
dat_num1


## uniqsubs
# telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
# dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(dec)%>%unname())[[2]]
# dat_num1$churn_perc<-dat_num1$n/dat_num1$N
# dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
# dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
# dat_num1$varname<-rep("uniqsubs",nrow(dat_num1))
# dat_num1

# unique(telecom$uniqsubs)

## opk_dat_Mean
telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
dat_num1$varname<-rep("opk_dat_Mean",nrow(dat_num1))
dat_num1


## roam_Mean
telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
dat_num1$varname<-rep("roam_Mean",nrow(dat_num1))
dat_num1

## recv_sms_Mean
telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
dat_num1$varname<-rep("recv_sms_Mean",nrow(dat_num1))
dat_num1

## blck_dat_Mean
telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
dat_num1$varname<-rep("blck_dat_Mean",nrow(dat_num1))
dat_num1


## mou_pead_Mean
telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
dat_num1$varname<-rep("mou_pead_Mean",nrow(dat_num1))
dat_num1

## da_Mean
telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
dat_num1$varname<-rep("da_Mean",nrow(dat_num1))
dat_num1

## da_Range
telecom%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
dat_num1$varname<-rep("da_Range",nrow(dat_num1))
dat_num1

## datovr_Mean
telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
dat_num1$varname<-rep("datovr_Mean",nrow(dat_num1))
dat_num1

## datovr_Range
telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
dat_num1$varname<-rep("datovr_Range",nrow(dat_num1))
dat_num1

## drop_dat_Mean
telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
dat_num1$varname<-rep("drop_dat_Mean",nrow(dat_num1))
dat_num1

## drop_vce_Mean
telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
dat_num1$varname<-rep("drop_vce_Mean",nrow(dat_num1))
dat_num1

## adjmou
telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dat_num1$varname<-rep("adjmou",nrow(dat_num1))
dat_num1

## totrev
telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat_num1$varname<-rep("totrev",nrow(dat_num1))
dat_num1

## adjrev
telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dat_num1$varname<-rep("adjrev",nrow(dat_num1))
dat_num1

## avgrev
telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat_num1
dat_num1$N<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
dat_num1$churn_perc<-dat_num1$n/dat_num1$N
dat_num1$GreaterThan<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dat_num1$LessThan<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dat_num1$varname<-rep("avgrev",nrow(dat_num1))
dat_num1



# relpace zero value from age variable   ----------------------------------
# replace zeros from age1 and age2
# age1
## zeros replace by Mean value
zero<-which(telecom$age1==0)
length(zero)
telecom$age1[zero]<-31
#age2
zero1<-which(telecom$age2==0)
length(zero1)
telecom$age2[zero1]<-21

## missing value imputation for Continuous variable----------------------------------------------

# Missing value imputation for age1 and age2

## age1
##creating groups for churned variable 
telecom%>%mutate(quantile=ntile(age1,10))%>%group_by(churn, quantile)%>%summarize(N=n())%>%filter(
  churn==1)-> age1

## find out churn Rate for each groups including missing values
telecom%>%mutate(quantile=ntile(age1,10))%>%group_by(quantile)%>%summarize(N=n())-> age11

age1$percentage<- age1$N/age11$N
age1

#replace with value in 6th Quantile
# what is 6th Quantile?
quantile(telecom$age1, p=(0:10)/10, na.rm = TRUE)
## 6th decile between 36 and 40 so we have to mean value of these value and put in missing value
(36+40)/2
telecom$age1[is.na(telecom$age1)]<-38

summary(telecom$age1)

## age2
##creating groups for churned variable 
telecom%>%mutate(quantile=ntile(age2,10))%>%group_by(churn, quantile)%>%summarize(N=n())%>%filter(
  churn==1)-> age2

## find out churn Rate for each groups including missing values
telecom%>%mutate(quantile=ntile(age2,10))%>%group_by(quantile)%>%summarize(N=n())-> age21

age2$percentage<- age2$N/age21$N
age2

#replace with value in 10th Quantile
# what is 6th Quantile?
quantile(telecom$age2, p=(0:10)/10, na.rm = TRUE)
## 6th decile between 54 and 98 so we have to mean value of these value and put in missing value
(54+98)/2
telecom$age2[is.na(telecom$age2)]<-76
summary(telecom$age2)

##  concatinate age1 and age2 into age variable
telecom$age<- (telecom$age1+telecom$age2)/2
telecom$age<- round(telecom$age, digits = 0)


## mou_Mean
 
telecom%>%mutate(quantile=ntile(mou_Mean,10))%>%group_by(churn, quantile)%>%summarize(N=n())%>%filter(
  churn==1)->cov1

telecom%>%mutate(quantile=ntile(mou_Mean,10))%>%group_by(quantile)%>%summarize(N=n())-> cov11

cov1$percentage<- cov1$N/cov11$N
cov1

#replace with value Quantile
quantile(telecom$mou_Mean, p=(0:10)/10, na.rm = TRUE)
(55.25+122.25)/2
telecom$mou_Mean[is.na(telecom$mou_Mean)]<- 88.8
summary(telecom$mou_Mean)

## change_mou 
quantile(telecom$change_mou, p=(0:10)/10, na.rm = TRUE)
((-2723.5 )+(-231.5))/2
telecom$change_mou[is.na(telecom$change_mou)]<- -1477.5
summary(telecom$change_mou)

## ovrrev_Mean
quantile(telecom$ovrrev_Mean, p=(0:10)/10, na.rm = TRUE)
(37.5+520.8375)/2
telecom$ovrrev_Mean[is.na(telecom$ovrrev_Mean)]<- 279.1687
summary(telecom$ovrrev_Mean)

## rev_Mean
quantile(telecom$rev_Mean, p=(0:10)/10, na.rm = TRUE)
(-6.1675 + 26.015)/2
telecom$rev_Mean[is.na(telecom$rev_Mean)]<-9.92375
summary(telecom$rev_Mean)

## avg6mou
quantile(telecom$avg6mou, p=(0:10)/10, na.rm = TRUE)
(1173 +	7217)/2
telecom$avg6mou[is.na(telecom$avg6mou)]<-4195
summary(telecom$avg6mou)


## hnd_price
quantile(telecom$hnd_price, p=(0:10)/10, na.rm = TRUE)
(199.98999 + 499.98999)/2
telecom$hnd_price[is.na(telecom$hnd_price)]<-349.99
summary(telecom$hnd_price)




## model iteration for Continuous variable following are-----------------------------------------
 ## mou_Mean,   change_mou,  drop_blk_Mean,  owylis_vce_Range,   mou_opkv_Range,   totcalls,
  # iwylis_vce_Mean,   callwait_Range,   ovrrev_Mean,   rev_Mean,   comp_vce_Mean,   avg6mou,
  # hnd_price,  mou_pead_Mean,   drop_dat_Mean,   totrev,  adjrev,   




## missing value imputation for categorical variable------------------------------

## income
telecom$income<- as.factor(telecom$income)
table_income<- table(telecom$income, telecom$churn)
table_income

incomeChurn_rate<-table_income[,2]/rowSums(table_income)
incomeChurn_rate

total_missingIncome<- which(is.na(telecom$income))
table(telecom$churn[total_missingIncome])/length(total_missingIncome)

telecom$income[total_missingIncome]<-7
summary(telecom$income)


## forgntvl 
table_forgntvl <- table(telecom$forgntvl, telecom$churn)
table_forgntvl 

forgntvlChurn_rate<-table_forgntvl[,2]/rowSums(table_forgntvl)
forgntvlChurn_rate

total_missingforgntvl<- which(is.na(telecom$forgntvl))
table(telecom$churn[total_missingforgntvl])/length(total_missingforgntvl)

telecom$forgntvl[total_missingforgntvl]<-1
summary(telecom$forgntvl)

## mtrcycle 
table_mtrcycle <- table(telecom$mtrcycle, telecom$churn)
table_mtrcycle 

mtrcycleChurn_rate<-table_mtrcycle[,2]/rowSums(table_mtrcycle)
mtrcycleChurn_rate

total_missingMtrcycle<- which(is.na(telecom$mtrcycle))
table(telecom$churn[total_missingMtrcycle])/length(total_missingMtrcycle)

telecom$mtrcycle[total_missingforgntvl]<-0
summary(telecom$mtrcycle)

## numbcars
table_numbcars<- table(telecom$numbcars, telecom$churn)
table_numbcars 

numbcarsChurn_rate<-table_numbcars[,2]/rowSums(table_numbcars)
numbcarsChurn_rate

total_missingNumbcars<- which(is.na(telecom$numbcars))
table(telecom$churn[total_missingNumbcars])/length(total_missingNumbcars)

telecom$numbcars[total_missingNumbcars]<-2
summary(telecom$numbcars)


## truck
telecom$truck[which(is.na(telecom$truck))]<-1
summary(telecom$truck)

## prizm_social_one
telecom$prizm_social_one[which(is.na(telecom$prizm_social_one))]<-"U"
summary(telecom$prizm_social_one)

## marital
telecom$marital[which(is.na(telecom$marital))]<-"S"
summary(telecom$marital)

## car_buy
telecom$car_buy[which(is.na(telecom$car_buy))]<-"New"
summary(telecom$car_buy)

## children
telecom$children[which(is.na(telecom$children))]<-"Y"
summary(telecom$children)

## div_type
telecom$div_type[which(is.na(telecom$div_type))]<-"LTD"
summary(telecom$div_type)

## dwlltype
telecom$dwlltype[which(is.na(telecom$dwlltype))]<-"M"
summary(telecom$dwlltype)


## model iteration for categorical variable following are-----------------------------------------

##  models, actvsubs,  forgntvl,  mtrcycle,  numbcars,  refurb_new,  truck,  asl_flag,  
##  prizm_social_one,  marital,  car_buy,  children,   div_type,   dwlltype,     





# convert the variable class ----------------------------------------------

class(telecom$churn)
telecom$churn<- as.numeric(telecom$churn)

telecom$models<- as.factor(telecom$models)
telecom$actvsubs<- as.factor(telecom$actvsubs)
telecom$forgntvl<- as.factor(telecom$forgntvl)
telecom$mtrcycle<- as.factor(telecom$mtrcycle)
telecom$numbcars<- as.factor(telecom$numbcars)
telecom$truck<- as.factor(telecom$truck)

str(telecom$churn)

#telecom$mou_Mean
telecom$M_mean<- cut(telecom$mou_Mean,  2, labels = c("Low","High"))

summary(telecom$M_mean)      
class(telecom$M_mean)

#telecom$change_mou
telecom$C_mou<- cut(telecom$change_mou, 2, labels = c("Low","High"))


#telecom$drop_blk_Mean
telecom$DB_mean<- cut(telecom$drop_blk_Mean, 2 , labels = c("Low","High"))

#telecom$owylis_vce_Range
telecom$OV_range<- cut(telecom$owylis_vce_Range, 2, labels = c("Low","High"))

#telecom$mou_opkv_Range
telecom$MO_range<-cut(telecom$mou_opkv_Range, 2, labels = c("Low","High"))

#telecom$totcalls
telecom$TT_calls<-cut(telecom$totcalls, 2, labels = c("Low","High"))

#telecom$iwylis_vce_Mean
telecom$IV_mean<-cut(telecom$iwylis_vce_Mean, 2, labels = c("Low","High"))

#telecom$callwait_Range
telecom$CW_range<-cut(telecom$callwait_Range, 2, labels = c("Low","High"))

#telecom$ovrrev_Mean
telecom$OR_mean<-cut(telecom$ovrrev_Mean, 2, labels = c("Low","High"))

#telecom$rev_Mean
telecom$R_mean<-cut(telecom$rev_Mean, 2, labels = c("Low","High"))


#telecom$comp_vce_Mean
telecom$CV_mean<-cut(telecom$comp_vce_Mean, 2, labels = c("Low","High"))


#telecom$avg6mou
telecom$AV6_mou<-cut(telecom$avg6mou, 2, labels = c("Low","High"))


#telecom$hnd_price
telecom$HD_price<-cut(telecom$hnd_price, 2, labels = c("Low","High"))


#telecom$totrev
telecom$TT_rev<-cut(telecom$totrev, 2, labels = c("Low","High"))


#telecom$adjrev
telecom$AD_rev<-cut(telecom$adjrev, 2, labels = c("Low","High"))


#telecom$age
telecom$AGE<-cut(telecom$age, 2, labels = c("Low","High"))


## selecting variables for model iteration -------------------------------------------------------
final_telecom<- data.frame(telecom$M_mean, telecom$C_mou,  telecom$DB_mean, telecom$OV_range,
                           telecom$MO_range,  telecom$TT_calls,  telecom$IV_mean, telecom$CW_range,
                           telecom$OR_mean, telecom$R_mean, telecom$CV_mean,  telecom$AV6_mou,
                           telecom$HD_price, telecom$TT_rev,  telecom$AD_rev, telecom$AGE, 
                           telecom$Customer_ID, telecom$churn, telecom$models, telecom$actvsubs,
                           telecom$forgntvl, telecom$mtrcycle, telecom$numbcars, telecom$truck,
                           telecom$refurb_new, telecom$asl_flag, telecom$prizm_social_one,
                           telecom$marital, telecom$car_buy, telecom$children, telecom$div_type,
                           telecom$dwlltype )


summary(final_telecom)


## spliting data into test and training samples
set.seed(200)
index<- sample(nrow(final_telecom), 0.70*nrow(final_telecom), replace = FALSE)
train<- final_telecom[index,]
test<- final_telecom[-index,]

## build the first model using all the variables

mod<- glm(telecom.churn~., data = train[-17], family = "binomial")
summary(mod)

step(mod, direction = "both")

mod1<- glm(formula = telecom.churn ~ telecom.C_mou + telecom.OV_range + 
             telecom.CW_range + telecom.AV6_mou + telecom.HD_price + telecom.AGE + 
             telecom.models + telecom.actvsubs + telecom.numbcars + telecom.refurb_new + 
             telecom.asl_flag + telecom.prizm_social_one + telecom.children, 
           family = "binomial", data = train)
summary(mod1)


## creating dummies
train$telecom.OV_rangeHigh_D<- ifelse(train$telecom.CW_range=="High", 1,0)
train$telecom.AV6_mouHigh_D<- ifelse(train$telecom.OR_mean=="High", 1,0)
train$telecom.HD_priceHigh_D<- ifelse(train$telecom.HD_price=="High", 1,0)
train$telecom.AGEHigh_D<- ifelse(train$telecom.AGE=="High", 1,0)
train$telecom.models2_D<- ifelse(train$telecom.models=="2", 1,0)
train$telecom.models3_D<- ifelse(train$telecom.models=="3", 1,0)
train$telecom.models5_D<- ifelse(train$telecom.models=="5", 1,0)
train$telecom.models6_D<- ifelse(train$telecom.models=="6", 1,0)
train$telecom.refurb_newR_D<- ifelse(train$telecom.refurb_new=="R", 1,0)
train$telecom.asl_flagY_D<- ifelse(train$telecom.asl_flag=="Y", 1,0)
train$telecom.prizm_social_oneS_D<- ifelse(train$telecom.prizm_social_one=="s", 1,0)
train$telecom.childrenY_D<- ifelse(train$telecom.children=="Y", 1,0) 



test$telecom.OV_rangeHigh_D<- ifelse(test$telecom.CW_range=="High", 1,0)
test$telecom.AV6_mouHigh_D<- ifelse(test$telecom.OR_mean=="High", 1,0)
test$telecom.HD_priceHigh_D<- ifelse(test$telecom.HD_price=="High", 1,0)
test$telecom.AGEHigh_D<- ifelse(test$telecom.AGE=="High", 1,0)
test$telecom.models2_D<- ifelse(test$telecom.models=="2", 1,0)
test$telecom.models3_D<- ifelse(test$telecom.models=="3", 1,0)
test$telecom.models5_D<- ifelse(test$telecom.models=="5", 1,0)
test$telecom.models6_D<- ifelse(test$telecom.models=="6", 1,0)
test$telecom.refurb_newR_D<- ifelse(test$telecom.refurb_new=="R", 1,0)
test$telecom.asl_flagY_D<- ifelse(test$telecom.asl_flag=="Y", 1,0)
test$telecom.prizm_social_oneS_D<- ifelse(test$telecom.prizm_social_one=="s", 1,0)
test$telecom.childrenY_D<- ifelse(test$telecom.children=="Y", 1,0) 


mod2<-glm(formula = telecom.churn ~ telecom.OV_rangeHigh_D + telecom.AV6_mouHigh_D +
            telecom.HD_priceHigh_D +  telecom.HD_priceHigh_D +  telecom.AGEHigh_D +
            telecom.models2_D + telecom.models3_D + telecom.models5_D + telecom.models6_D +
            telecom.refurb_newR_D +  telecom.asl_flagY_D +  telecom.prizm_social_oneS_D + 
            telecom.childrenY_D, family = "binomial", data = train)

summary(mod2)


mod3<-glm(formula = telecom.churn ~ telecom.OV_rangeHigh_D + telecom.HD_priceHigh_D +
            telecom.AGEHigh_D + telecom.models2_D + telecom.models3_D + telecom.models5_D + 
            telecom.models6_D + telecom.refurb_newR_D +  telecom.asl_flagY_D + 
            telecom.childrenY_D, family = "binomial", data = train)

summary(mod3)

mod4<-glm(formula = telecom.churn ~ telecom.HD_priceHigh_D +
            telecom.AGEHigh_D + telecom.models2_D + telecom.models3_D + telecom.models5_D + 
            telecom.models6_D + telecom.refurb_newR_D +  telecom.asl_flagY_D + 
            telecom.childrenY_D, family = "binomial", data = train)

summary(mod4)

hist(mod4$residuals)

qqPlot(mod4$residuals)

vif(mod4)


## create prediction

Pred<- predict(mod4, type = "response", newdata = test)

head(Pred)

table(final_telecom$telecom.churn)/nrow(final_telecom)

Pred<- ifelse(Pred>=0.2352364, 1,0)

kappa2(data.frame(test$telecom.churn, Pred))

library(MASS)
confusionMatrix(Pred, test$telecom.churn, positive = "1")

gains(test$telecom.churn, predict(mod4, type = "response", newdata = test), groups = 10)

test$prob<- predict(mod4, type = "response", newdata = test)

quantile(test$prob, prob= c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

trageted<- test[test$prob>0.2645582 & test$prob<= 0.3108570 ,"telecom.Customer_Id"]
trageted
