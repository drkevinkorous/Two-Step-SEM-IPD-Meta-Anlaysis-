#Load data
library('foreign')
da03255.0001<-read.spss('03255-0001-Data.sav')
#Coded variables
#Participant ID: ID
#Measure Name (not a variable in the dataset): CESD
#Depressive symptom items: 
#Q1703A1,Q1703A2,Q1703A3,Q1703A4,Q1703A5,Q1703A6,Q1703A7,Q1703A8
#Depressive symptom items prior wave: NA
#Academic Achievement: NA
#Academic Achievement prior wave: NA
#Ethnicity: Q14
#Race: Q14
#White: NA
#Black: NA
#Asian or Pacific Islander: NA
#Native American: NA
#More than one race: NA
#Other Race (to determine more than one race): NA
#Sex: Q120
#Age: Q112
#Birth Year: Q110 
#Birth Month: Q108
#Birth Day: NA
#Evaluation Year: NA
#Evaluation Month: NA
#Evaluation Day: NA
#Participant Income: NA  
#Maternal Income: NA
#Paternal Income: NA
#Household Income: Q114
#Participant Education: Q104
#Maternal Education: NA
#Paternal Education: NA
#Household Education: NA
#Participant Occupation: NA 
#Maternal Occupation: NA
#Paternal Occupation: NA
#Household Occupation: NA
#SES: NA
#Year of data collection (not a variable in the dataset): 2001
#Notes: Religion, Aging, and Health Survey, 2001, 2004 [United States] - Wave 1

#Create Dataset ----
attach(da03255.0001)
#Extract relevant variables into a dataframe.
ID3255DS1<-data.frame(id,q1703a1,q1703a2,q1703a3,q1703a4,q1703a5,q1703a6,q1703a7,q1703a8,
                      q14,q120,q112,q110,q108,q114,q104)
detach(da03255.0001)
ID3255DS1$ORIGOBS<-nrow(ID3255DS1)
rm(da03255.0001)

#Check to make sure there is no funny business
head(ID3255DS1)

#Depressive Symptoms ---------
ID3255DS1$q1703a1<-ifelse(ID3255DS1$q1703a1=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a1=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a1=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a1=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a2<-ifelse(ID3255DS1$q1703a2=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a2=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a2=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a2=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a3<-ifelse(ID3255DS1$q1703a3=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a3=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a3=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a3=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a4<-ifelse(ID3255DS1$q1703a4=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a4=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a4=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a4=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a5<-ifelse(ID3255DS1$q1703a5=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a5=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a5=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a5=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a6<-ifelse(ID3255DS1$q1703a6=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a6=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a6=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a6=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a7<-ifelse(ID3255DS1$q1703a7=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a7=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a7=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a7=="Most or All of the Time",3,NA))))
ID3255DS1$q1703a8<-ifelse(ID3255DS1$q1703a8=="Rarely or None of the Time",0,               
                   ifelse(ID3255DS1$q1703a8=="Some or Little of the Time",1,               
                   ifelse(ID3255DS1$q1703a8=="Occasionally or a Moderate Amount of Time",2,
                   ifelse(ID3255DS1$q1703a8=="Most or All of the Time",3,NA))))

#Depressive symptoms prior wave 
#NA

#Assign dataframe that contains all depressive symtpoms (using the column numbers of the items)
dsitems <- ID3255DS1[c(2:9)]
#dsitemspw <- ID3255DS1[c(:)]
#Identify participants missing more than half of dsitems (not dsitemsW1) (1=missing > 50%) - to be used later
ID3255DS1$dsmiss<-ifelse(rowSums(is.na(dsitems))>((ncol(dsitems)/2)),1,0)
#Compute mean symptom score 
ID3255DS1$DEP<-rowMeans(x=dsitems,na.rm=TRUE)
ID3255DS1$DEPPW<-NA#rowMeans(x=dsitemspw,na.rm=TRUE)

#Academic Achievement -----
#Recode Academic Achievement 
ID3255DS1$ACHV<-NA

#Recode Academic Achievement (prior wave)
ID3255DS1$ACHVPW<-NA

#Demographics ----
#Recode race/ethnicity
ID3255DS1$LAT<-ifelse(ID3255DS1$q14=="Hispanic/Latino",1,0)
ID3255DS1$WHT<-ifelse(ID3255DS1$q14=="White/Caucasian",1,0)
ID3255DS1$BLK<-ifelse(ID3255DS1$q14=="Black/African American",1,0)
ID3255DS1$AI<-ifelse(ID3255DS1$q14=="American Indian or Alaskan Native",1,0)
ID3255DS1$AA<-ifelse(ID3255DS1$q14=="Asian or Pacific Islander",1,0)
ID3255DS1$OTH<-ifelse(ID3255DS1$q14=="Other",1,0)

#Compute variable for participants who reported more than one race (mixed/biracial; do not include Latino)
ID3255DS1$MR<-ifelse(((ID3255DS1$q14=="White/Caucasian and some other race")|
                      (ID3255DS1$q14=="Black/African American and some other race")),1,0)
#ID3255DS1$MR<-ifelse((rowSums(x=cbind(ID3255DS1$WHT,ID3255DS1$BLK,ID3255DS1$AI,ID3255DS1$AA,
#                                       ID3255DS1$OTH),na.rm=TRUE)) > 1, 1, 0)

##Compute variables for participants who reported a single race (excluding mixed/biracial & Latino)
#ID3255DS1$WHT<-ifelse(((ID3255DS1$LAT == 0)&(ID3255DS1$MR == 0)&(ID3255DS1$WHT == 1)), 1, 0)
#ID3255DS1$BLK<-ifelse(((ID3255DS1$LAT == 0)&(ID3255DS1$MR == 0)&(ID3255DS1$BLK == 1)), 1, 0)
#ID3255DS1$AI<-ifelse(((ID3255DS1$LAT == 0)&(ID3255DS1$MR == 0)&(ID3255DS1$AI == 1)), 1, 0)
#ID3255DS1$AA<-ifelse(((ID3255DS1$LAT == 0)&(ID3255DS1$MR == 0)&(ID3255DS1$AA == 1)), 1, 0)
#ID3255DS1$OTH<-ifelse(((ID3255DS1$LAT == 0)&(ID3255DS1$MR == 0)&(ID3255DS1$OTH == 1)), 1, 0)
#ID3255DS1$MR<-ifelse(((ID3255DS1$LAT == 0)&(ID3255DS1$MR == 1)), 1, 0)

#Recode sex
ID3255DS1$SEX<-ifelse(ID3255DS1$q120 == "Male",1,
               ifelse(ID3255DS1$q120 == "Female",0,NA))

#Recode age 
ID3255DS1$q112[ID3255DS1$q112==999]<-NA
ID3255DS1$q112[ID3255DS1$q112==998]<-NA
ID3255DS1$AGECALC<-ID3255DS1$q112

#Income, Education, Occupation ----
##Income   
ID3255DS1$INC<-ifelse(ID3255DS1$q114=="Less than $5,000",4999, 
               ifelse(ID3255DS1$q114=="$5,001-$9,999",7499.5,     
               ifelse(ID3255DS1$q114=="$10,000-$14,999",12499.5,   
               ifelse(ID3255DS1$q114=="$15,000-$19,999",17499.5,   
               ifelse(ID3255DS1$q114=="$20,000-$24,999",22499.5,  
               ifelse(ID3255DS1$q114=="$25,000-$29,999",27499.5,   
               ifelse(ID3255DS1$q114=="$30,000-$39,999",34999.5,   
               ifelse(ID3255DS1$q114=="$40,000-$59,999",49999.5,   
               ifelse(ID3255DS1$q114=="60,000-$79,999",69999.5,   
               ifelse(ID3255DS1$q114=="$80,000+",80000,NA))))))))))

##Education 
ID3255DS1$q104[ID3255DS1$q104==99]<-NA
ID3255DS1$q104[ID3255DS1$q104==98]<-NA
ID3255DS1$EDU<-ID3255DS1$q104

##Occupation
#Nam-Powers-Boyd scale (Nam & Boyd, 2008) 
ID3255DS1$OCCS<-NA

#Recode factor into numeric (occupational prestige)
#Nakao & Tres scale (1994)
ID3255DS1$OCCP<-NA

#Save Recoded Dataset ------
#Save database as .csv for OSF 
write.csv(ID3255DS1, file = 'ID3255DS1_recoded.csv')

#Descriptives and Correlations -----
#Exclude participants with more than 50% missing on depressive symptoms items
ID3255DS1 <- ID3255DS1[which(ID3255DS1$dsmiss==0), ]

##Compute mean and sd across all participants within dataset
library('psych')
#Means & SDs of focal variables
dstatsage<-describe(ID3255DS1$AGECALC)
dstatsdep<-describe(ID3255DS1$DEP)
dstatsachv<-NA#describe(ID3255DS1$ACHV)
dstatsinc<-describe(ID3255DS1$INC)
dstatsedu<-describe(ID3255DS1$EDU)
dstatsoccs<-NA#describe(ID3255DS1$OCCS)
dstatsoccp<-NA#describe(ID3255DS1$OCCP)

#Save descriptive stats
dstats<-rbind(dstatsage,dstatsdep,dstatsachv,dstatsinc,dstatsedu,dstatsoccs,dstatsoccp,make.row.names= FALSE)
write.csv(dstats,file = 'ID3255DS1_dstats.csv',
          row.names=c("dstatsage","dstatsdep","dstatsachv","dstatsinc","dstatsedu","dstatsoccs","dstatsoccp"))

##Histograms 
pdf('ID3255DS1_dephist.pdf')
hist(ID3255DS1$DEP, xlab="Depressive Symptoms", main = NULL, col="grey")
dev.off()
#pdf('ID3255DS1_achvhist.pdf')
#hist(ID3255DS1$ACHV, xlab="Academic Achievement", main = NULL, col="grey")
#dev.off()
pdf('ID3255DS1_inchist.pdf')
hist(ID3255DS1$INC, xlab="Income", main = NULL, col="grey")
dev.off()
pdf('ID3255DS1_educhist.pdf')
hist(ID3255DS1$EDU, xlab="Educational Attainment", main = NULL, col="grey")
dev.off()
#pdf('ID3255DS1_occshist.pdf')
#hist(ID3255DS1$OCCS, xlab="Occupational Status", main = NULL, col="grey")
#dev.off()
#pdf('ID3255DS1_occphist.pdf')
#hist(ID3255DS1$OCCP, xlab="Occupational Prestige", main = NULL, col="grey")
#dev.off()

### Extract correlations separately by developmental period, sex, & race/ethnicity
#Developmental period: children(5-11), adolescents(12-17), young adults(18-25), adults(26-64), older adults(65+)
table(ID3255DS1$AGECALC)
#Separate dataset by developmental period, if needed
#CH<-ID3255DS1[which((ID3255DS1$AGECALC >= 5)&(ID3255DS1$AGECALC <= 11)), ] 
#AD<-ID3255DS1[which((ID3255DS1$AGECALC >= 12)&(ID3255DS1$AGECALC <= 17)), ]
#YD<-ID3255DS1[which((ID3255DS1$AGECALC >= 18)&(ID3255DS1$AGECALC <= 25)), ]
#AT<-ID3255DS1[which((ID3255DS1$AGECALC >= 26)&(ID3255DS1$AGECALC <= 64)), ]
OA<-ID3255DS1[which(ID3255DS1$AGECALC >= 65), ]

rm(ID3255DS1)

#Separate developmental datasets by Female and Male 
#CHF <- CH[which(CH$SEX == 0), ] 
#CHM <- CH[which(CH$SEX == 1), ]

#ADF <- AD[which(AD$SEX == 0), ]
#ADM <- AD[which(AD$SEX == 1), ]

#YDF <- YD[which(YD$SEX == 0), ]
#YDM <- YD[which(YD$SEX == 1), ]

#ATF <- AT[which(AT$SEX == 0), ]
#ATM <- AT[which(AT$SEX == 1), ]

OAF <- OA[which(OA$SEX == 0), ]
OAM <- OA[which(OA$SEX == 1), ]

#Separate datasets further by race/ethnicity
#Mean center and square INC,EDU,OCCS,&OCCP within each dataset
#WHTCHF <- CHF[which(CHF$WHT == 1), ]
#WHTCHF$INCMC<-scale(WHTCHF$INC,scale=FALSE)
#WHTCHF$INC2<-WHTCHF$INCMC*WHTCHF$INCMC
#WHTCHF$EDUMC<-scale(WHTCHF$EDU,scale=FALSE)
#WHTCHF$EDU2<-WHTCHF$EDUMC*WHTCHF$EDUMC
#WHTCHF$OCCSMC<-scale(WHTCHF$OCCS,scale=FALSE)
#WHTCHF$OCCS2<-WHTCHF$OCCSMC*WHTCHF$OCCSMC
#WHTCHF$OCCPMC<-scale(WHTCHF$OCCP,scale=FALSE)
#WHTCHF$OCCP2<-WHTCHF$OCCPMC*WHTCHF$OCCPMC

#WHTCHM <- CHM[which(CHM$WHT == 1), ]
#WHTCHM$INCMC<-scale(WHTCHM$INC,scale=FALSE)
#WHTCHM$INC2<-WHTCHM$INCMC*WHTCHM$INCMC
#WHTCHM$EDUMC<-scale(WHTCHM$EDU,scale=FALSE)
#WHTCHM$EDU2<-WHTCHM$EDUMC*WHTCHM$EDUMC
#WHTCHM$OCCSMC<-scale(WHTCHM$OCCS,scale=FALSE)
#WHTCHM$OCCS2<-WHTCHM$OCCSMC*WHTCHM$OCCSMC
#WHTCHM$OCCPMC<-scale(WHTCHM$OCCP,scale=FALSE)
#WHTCHM$OCCP2<-WHTCHM$OCCPMC*WHTCHM$OCCPMC

#WHTADF <- ADF[which(ADF$WHT == 1), ]
#WHTADF$INCMC<-scale(WHTADF$INC,scale=FALSE)
#WHTADF$INC2<-WHTADF$INCMC*WHTADF$INCMC
#WHTADF$EDUMC<-scale(WHTADF$EDU,scale=FALSE)
#WHTADF$EDU2<-WHTADF$EDUMC*WHTADF$EDUMC
#WHTADF$OCCSMC<-scale(WHTADF$OCCS,scale=FALSE)
#WHTADF$OCCS2<-WHTADF$OCCSMC*WHTADF$OCCSMC
#WHTADF$OCCPMC<-scale(WHTADF$OCCP,scale=FALSE)
#WHTADF$OCCP2<-WHTADF$OCCPMC*WHTADF$OCCPMC
#
#WHTADM <- ADM[which(ADM$WHT == 1), ]
#WHTADM$INCMC<-scale(WHTADM$INC,scale=FALSE)
#WHTADM$INC2<-WHTADM$INCMC*WHTADM$INCMC
#WHTADM$EDUMC<-scale(WHTADM$EDU,scale=FALSE)
#WHTADM$EDU2<-WHTADM$EDUMC*WHTADM$EDUMC
#WHTADM$OCCSMC<-scale(WHTADM$OCCS,scale=FALSE)
#WHTADM$OCCS2<-WHTADM$OCCSMC*WHTADM$OCCSMC
#WHTADM$OCCPMC<-scale(WHTADM$OCCP,scale=FALSE)
#WHTADM$OCCP2<-WHTADM$OCCPMC*WHTADM$OCCPMC

#WHTYDF <- YDF[which(YDF$WHT == 1), ]
#WHTYDF$INCMC<-scale(WHTYDF$INC,scale=FALSE)
#WHTYDF$INC2<-WHTYDF$INCMC*WHTYDF$INCMC
#WHTYDF$EDUMC<-scale(WHTYDF$EDU,scale=FALSE)
#WHTYDF$EDU2<-WHTYDF$EDUMC*WHTYDF$EDUMC
#WHTYDF$OCCSMC<-scale(WHTYDF$OCCS,scale=FALSE)
#WHTYDF$OCCS2<-WHTYDF$OCCSMC*WHTYDF$OCCSMC
#WHTYDF$OCCPMC<-scale(WHTYDF$OCCP,scale=FALSE)
#WHTYDF$OCCP2<-WHTYDF$OCCPMC*WHTYDF$OCCPMC
#
#WHTYDM <- YDM[which(YDM$WHT == 1), ]
#WHTYDM$INCMC<-scale(WHTYDM$INC,scale=FALSE)
#WHTYDM$INC2<-WHTYDM$INCMC*WHTYDM$INCMC
#WHTYDM$EDUMC<-scale(WHTYDM$EDU,scale=FALSE)
#WHTYDM$EDU2<-WHTYDM$EDUMC*WHTYDM$EDUMC
#WHTYDM$OCCSMC<-scale(WHTYDM$OCCS,scale=FALSE)
#WHTYDM$OCCS2<-WHTYDM$OCCSMC*WHTYDM$OCCSMC
#WHTYDM$OCCPMC<-scale(WHTYDM$OCCP,scale=FALSE)
#WHTYDM$OCCP2<-WHTYDM$OCCPMC*WHTYDM$OCCPMC

#WHTATF <- ATF[which(ATF$WHT == 1), ]
#WHTATF$INCMC<-scale(WHTATF$INC,scale=FALSE)
#WHTATF$INC2<-WHTATF$INCMC*WHTATF$INCMC
#WHTATF$EDUMC<-scale(WHTATF$EDU,scale=FALSE)
#WHTATF$EDU2<-WHTATF$EDUMC*WHTATF$EDUMC
#WHTATF$OCCSMC<-scale(WHTATF$OCCS,scale=FALSE)
#WHTATF$OCCS2<-WHTATF$OCCSMC*WHTATF$OCCSMC
#WHTATF$OCCPMC<-scale(WHTATF$OCCP,scale=FALSE)
#WHTATF$OCCP2<-WHTATF$OCCPMC*WHTATF$OCCPMC
#
#WHTATM <- ATM[which(ATM$WHT == 1), ]
#WHTATM$INCMC<-scale(WHTATM$INC,scale=FALSE)
#WHTATM$INC2<-WHTATM$INCMC*WHTATM$INCMC
#WHTATM$EDUMC<-scale(WHTATM$EDU,scale=FALSE)
#WHTATM$EDU2<-WHTATM$EDUMC*WHTATM$EDUMC
#WHTATM$OCCSMC<-scale(WHTATM$OCCS,scale=FALSE)
#WHTATM$OCCS2<-WHTATM$OCCSMC*WHTATM$OCCSMC
#WHTATM$OCCPMC<-scale(WHTATM$OCCP,scale=FALSE)
#WHTATM$OCCP2<-WHTATM$OCCPMC*WHTATM$OCCPMC

WHTOAF <- OAF[which(OAF$WHT == 1), ]
WHTOAF$INCMC<-scale(WHTOAF$INC,scale=FALSE)
WHTOAF$INC2<-WHTOAF$INCMC*WHTOAF$INCMC
WHTOAF$EDUMC<-scale(WHTOAF$EDU,scale=FALSE)
WHTOAF$EDU2<-WHTOAF$EDUMC*WHTOAF$EDUMC
#WHTOAF$OCCSMC<-scale(WHTOAF$OCCS,scale=FALSE)
#WHTOAF$OCCS2<-WHTOAF$OCCSMC*WHTOAF$OCCSMC
#WHTOAF$OCCPMC<-scale(WHTOAF$OCCP,scale=FALSE)
#WHTOAF$OCCP2<-WHTOAF$OCCPMC*WHTOAF$OCCPMC

WHTOAM <- OAM[which(OAM$WHT == 1), ]
WHTOAM$INCMC<-scale(WHTOAM$INC,scale=FALSE)
WHTOAM$INC2<-WHTOAM$INCMC*WHTOAM$INCMC
WHTOAM$EDUMC<-scale(WHTOAM$EDU,scale=FALSE)
WHTOAM$EDU2<-WHTOAM$EDUMC*WHTOAM$EDUMC
#WHTOAM$OCCSMC<-scale(WHTOAM$OCCS,scale=FALSE)
#WHTOAM$OCCS2<-WHTOAM$OCCSMC*WHTOAM$OCCSMC
#WHTOAM$OCCPMC<-scale(WHTOAM$OCCP,scale=FALSE)
#WHTOAM$OCCP2<-WHTOAM$OCCPMC*WHTOAM$OCCPMC

#LATCHF <- CHF[which(CHF$LAT == 1), ]
#LATCHF$INCMC<-scale(LATCHF$INC,scale=FALSE)
#LATCHF$INC2<-LATCHF$INCMC*LATCHF$INCMC
#LATCHF$EDUMC<-scale(LATCHF$EDU,scale=FALSE)
#LATCHF$EDU2<-LATCHF$EDUMC*LATCHF$EDUMC
#LATCHF$OCCSMC<-scale(LATCHF$OCCS,scale=FALSE)
#LATCHF$OCCS2<-LATCHF$OCCSMC*LATCHF$OCCSMC
#LATCHF$OCCPMC<-scale(LATCHF$OCCP,scale=FALSE)
#LATCHF$OCCP2<-LATCHF$OCCPMC*LATCHF$OCCPMC

#LATCHM <- CHM[which(CHM$LAT == 1), ]
#LATCHM$INCMC<-scale(LATCHM$INC,scale=FALSE)
#LATCHM$INC2<-LATCHM$INCMC*LATCHM$INCMC
#LATCHM$EDUMC<-scale(LATCHM$EDU,scale=FALSE)
#LATCHM$EDU2<-LATCHM$EDUMC*LATCHM$EDUMC
#LATCHM$OCCSMC<-scale(LATCHM$OCCS,scale=FALSE)
#LATCHM$OCCS2<-LATCHM$OCCSMC*LATCHM$OCCSMC
#LATCHM$OCCPMC<-scale(LATCHM$OCCP,scale=FALSE)
#LATCHM$OCCP2<-LATCHM$OCCPMC*LATCHM$OCCPMC

#LATADF <- ADF[which(ADF$LAT == 1), ]
#LATADF$INCMC<-scale(LATADF$INC,scale=FALSE)
#LATADF$INC2<-LATADF$INCMC*LATADF$INCMC
#LATADF$EDUMC<-scale(LATADF$EDU,scale=FALSE)
#LATADF$EDU2<-LATADF$EDUMC*LATADF$EDUMC
#LATADF$OCCSMC<-scale(LATADF$OCCS,scale=FALSE)
#LATADF$OCCS2<-LATADF$OCCSMC*LATADF$OCCSMC
#LATADF$OCCPMC<-scale(LATADF$OCCP,scale=FALSE)
#LATADF$OCCP2<-LATADF$OCCPMC*LATADF$OCCPMC
#
#LATADM <- ADM[which(ADM$LAT == 1), ]
#LATADM$INCMC<-scale(LATADM$INC,scale=FALSE)
#LATADM$INC2<-LATADM$INCMC*LATADM$INCMC
#LATADM$EDUMC<-scale(LATADM$EDU,scale=FALSE)
#LATADM$EDU2<-LATADM$EDUMC*LATADM$EDUMC
#LATADM$OCCSMC<-scale(LATADM$OCCS,scale=FALSE)
#LATADM$OCCS2<-LATADM$OCCSMC*LATADM$OCCSMC
#LATADM$OCCPMC<-scale(LATADM$OCCP,scale=FALSE)
#LATADM$OCCP2<-LATADM$OCCPMC*LATADM$OCCPMC

#LATYDF <- YDF[which(YDF$LAT == 1), ]
#LATYDF$INCMC<-scale(LATYDF$INC,scale=FALSE)
#LATYDF$INC2<-LATYDF$INCMC*LATYDF$INCMC
#LATYDF$EDUMC<-scale(LATYDF$EDU,scale=FALSE)
#LATYDF$EDU2<-LATYDF$EDUMC*LATYDF$EDUMC
#LATYDF$OCCSMC<-scale(LATYDF$OCCS,scale=FALSE)
#LATYDF$OCCS2<-LATYDF$OCCSMC*LATYDF$OCCSMC
#LATYDF$OCCPMC<-scale(LATYDF$OCCP,scale=FALSE)
#LATYDF$OCCP2<-LATYDF$OCCPMC*LATYDF$OCCPMC
#
#LATYDM <- YDM[which(YDM$LAT == 1), ]
#LATYDM$INCMC<-scale(LATYDM$INC,scale=FALSE)
#LATYDM$INC2<-LATYDM$INCMC*LATYDM$INCMC
#LATYDM$EDUMC<-scale(LATYDM$EDU,scale=FALSE)
#LATYDM$EDU2<-LATYDM$EDUMC*LATYDM$EDUMC
#LATYDM$OCCSMC<-scale(LATYDM$OCCS,scale=FALSE)
#LATYDM$OCCS2<-LATYDM$OCCSMC*LATYDM$OCCSMC
#LATYDM$OCCPMC<-scale(LATYDM$OCCP,scale=FALSE)
#LATYDM$OCCP2<-LATYDM$OCCPMC*LATYDM$OCCPMC

#LATATF <- ATF[which(ATF$LAT == 1), ]
#LATATF$INCMC<-scale(LATATF$INC,scale=FALSE)
#LATATF$INC2<-LATATF$INCMC*LATATF$INCMC
#LATATF$EDUMC<-scale(LATATF$EDU,scale=FALSE)
#LATATF$EDU2<-LATATF$EDUMC*LATATF$EDUMC
#LATATF$OCCSMC<-scale(LATATF$OCCS,scale=FALSE)
#LATATF$OCCS2<-LATATF$OCCSMC*LATATF$OCCSMC
#LATATF$OCCPMC<-scale(LATATF$OCCP,scale=FALSE)
#LATATF$OCCP2<-LATATF$OCCPMC*LATATF$OCCPMC
#
#LATATM <- ATM[which(ATM$LAT == 1), ]
#LATATM$INCMC<-scale(LATATM$INC,scale=FALSE)
#LATATM$INC2<-LATATM$INCMC*LATATM$INCMC
#LATATM$EDUMC<-scale(LATATM$EDU,scale=FALSE)
#LATATM$EDU2<-LATATM$EDUMC*LATATM$EDUMC
#LATATM$OCCSMC<-scale(LATATM$OCCS,scale=FALSE)
#LATATM$OCCS2<-LATATM$OCCSMC*LATATM$OCCSMC
#LATATM$OCCPMC<-scale(LATATM$OCCP,scale=FALSE)
#LATATM$OCCP2<-LATATM$OCCPMC*LATATM$OCCPMC

#LATOAF <- OAF[which(OAF$LAT == 1), ]
#LATOAF$INCMC<-scale(LATOAF$INC,scale=FALSE)
#LATOAF$INC2<-LATOAF$INCMC*LATOAF$INCMC
#LATOAF$EDUMC<-scale(LATOAF$EDU,scale=FALSE)
#LATOAF$EDU2<-LATOAF$EDUMC*LATOAF$EDUMC
#LATOAF$OCCSMC<-scale(LATOAF$OCCS,scale=FALSE)
#LATOAF$OCCS2<-LATOAF$OCCSMC*LATOAF$OCCSMC
#LATOAF$OCCPMC<-scale(LATOAF$OCCP,scale=FALSE)
#LATOAF$OCCP2<-LATOAF$OCCPMC*LATOAF$OCCPMC
#
#LATOAM <- OAM[which(OAM$LAT == 1), ]
#LATOAM$INCMC<-scale(LATOAM$INC,scale=FALSE)
#LATOAM$INC2<-LATOAM$INCMC*LATOAM$INCMC
#LATOAM$EDUMC<-scale(LATOAM$EDU,scale=FALSE)
#LATOAM$EDU2<-LATOAM$EDUMC*LATOAM$EDUMC
#LATOAM$OCCSMC<-scale(LATOAM$OCCS,scale=FALSE)
#LATOAM$OCCS2<-LATOAM$OCCSMC*LATOAM$OCCSMC
#LATOAM$OCCPMC<-scale(LATOAM$OCCP,scale=FALSE)
#LATOAM$OCCP2<-LATOAM$OCCPMC*LATOAM$OCCPMC

#BLKCHF <- CHF[which(CHF$BLK == 1), ]
#BLKCHF$INCMC<-scale(BLKCHF$INC,scale=FALSE)
#BLKCHF$INC2<-BLKCHF$INCMC*BLKCHF$INCMC
#BLKCHF$EDUMC<-scale(BLKCHF$EDU,scale=FALSE)
#BLKCHF$EDU2<-BLKCHF$EDUMC*BLKCHF$EDUMC
#BLKCHF$OCCSMC<-scale(BLKCHF$OCCS,scale=FALSE)
#BLKCHF$OCCS2<-BLKCHF$OCCSMC*BLKCHF$OCCSMC
#BLKCHF$OCCPMC<-scale(BLKCHF$OCCP,scale=FALSE)
#BLKCHF$OCCP2<-BLKCHF$OCCPMC*BLKCHF$OCCPMC

#BLKCHM <- CHM[which(CHM$BLK == 1), ]
#BLKCHM$INCMC<-scale(BLKCHM$INC,scale=FALSE)
#BLKCHM$INC2<-BLKCHM$INCMC*BLKCHM$INCMC
#BLKCHM$EDUMC<-scale(BLKCHM$EDU,scale=FALSE)
#BLKCHM$EDU2<-BLKCHM$EDUMC*BLKCHM$EDUMC
#BLKCHM$OCCSMC<-scale(BLKCHM$OCCS,scale=FALSE)
#BLKCHM$OCCS2<-BLKCHM$OCCSMC*BLKCHM$OCCSMC
#BLKCHM$OCCPMC<-scale(BLKCHM$OCCP,scale=FALSE)
#BLKCHM$OCCP2<-BLKCHM$OCCPMC*BLKCHM$OCCPMC

#BLKADF <- ADF[which(ADF$BLK == 1), ]
#BLKADF$INCMC<-scale(BLKADF$INC,scale=FALSE)
#BLKADF$INC2<-BLKADF$INCMC*BLKADF$INCMC
#BLKADF$EDUMC<-scale(BLKADF$EDU,scale=FALSE)
#BLKADF$EDU2<-BLKADF$EDUMC*BLKADF$EDUMC
#BLKADF$OCCSMC<-scale(BLKADF$OCCS,scale=FALSE)
#BLKADF$OCCS2<-BLKADF$OCCSMC*BLKADF$OCCSMC
#BLKADF$OCCPMC<-scale(BLKADF$OCCP,scale=FALSE)
#BLKADF$OCCP2<-BLKADF$OCCPMC*BLKADF$OCCPMC
#
#BLKADM <- ADM[which(ADM$BLK == 1), ]
#BLKADM$INCMC<-scale(BLKADM$INC,scale=FALSE)
#BLKADM$INC2<-BLKADM$INCMC*BLKADM$INCMC
#BLKADM$EDUMC<-scale(BLKADM$EDU,scale=FALSE)
#BLKADM$EDU2<-BLKADM$EDUMC*BLKADM$EDUMC
#BLKADM$OCCSMC<-scale(BLKADM$OCCS,scale=FALSE)
#BLKADM$OCCS2<-BLKADM$OCCSMC*BLKADM$OCCSMC
#BLKADM$OCCPMC<-scale(BLKADM$OCCP,scale=FALSE)
#BLKADM$OCCP2<-BLKADM$OCCPMC*BLKADM$OCCPMC

#BLKYDF <- YDF[which(YDF$BLK == 1), ]
#BLKYDF$INCMC<-scale(BLKYDF$INC,scale=FALSE)
#BLKYDF$INC2<-BLKYDF$INCMC*BLKYDF$INCMC
#BLKYDF$EDUMC<-scale(BLKYDF$EDU,scale=FALSE)
#BLKYDF$EDU2<-BLKYDF$EDUMC*BLKYDF$EDUMC
#BLKYDF$OCCSMC<-scale(BLKYDF$OCCS,scale=FALSE)
#BLKYDF$OCCS2<-BLKYDF$OCCSMC*BLKYDF$OCCSMC
#BLKYDF$OCCPMC<-scale(BLKYDF$OCCP,scale=FALSE)
#BLKYDF$OCCP2<-BLKYDF$OCCPMC*BLKYDF$OCCPMC
#
#BLKYDM <- YDM[which(YDM$BLK == 1), ]
#BLKYDM$INCMC<-scale(BLKYDM$INC,scale=FALSE)
#BLKYDM$INC2<-BLKYDM$INCMC*BLKYDM$INCMC
#BLKYDM$EDUMC<-scale(BLKYDM$EDU,scale=FALSE)
#BLKYDM$EDU2<-BLKYDM$EDUMC*BLKYDM$EDUMC
#BLKYDM$OCCSMC<-scale(BLKYDM$OCCS,scale=FALSE)
#BLKYDM$OCCS2<-BLKYDM$OCCSMC*BLKYDM$OCCSMC
#BLKYDM$OCCPMC<-scale(BLKYDM$OCCP,scale=FALSE)
#BLKYDM$OCCP2<-BLKYDM$OCCPMC*BLKYDM$OCCPMC

#BLKATF <- ATF[which(ATF$BLK == 1), ]
#BLKATF$INCMC<-scale(BLKATF$INC,scale=FALSE)
#BLKATF$INC2<-BLKATF$INCMC*BLKATF$INCMC
#BLKATF$EDUMC<-scale(BLKATF$EDU,scale=FALSE)
#BLKATF$EDU2<-BLKATF$EDUMC*BLKATF$EDUMC
#BLKATF$OCCSMC<-scale(BLKATF$OCCS,scale=FALSE)
#BLKATF$OCCS2<-BLKATF$OCCSMC*BLKATF$OCCSMC
#BLKATF$OCCPMC<-scale(BLKATF$OCCP,scale=FALSE)
#BLKATF$OCCP2<-BLKATF$OCCPMC*BLKATF$OCCPMC
#
#BLKATM <- ATM[which(ATM$BLK == 1), ]
#BLKATM$INCMC<-scale(BLKATM$INC,scale=FALSE)
#BLKATM$INC2<-BLKATM$INCMC*BLKATM$INCMC
#BLKATM$EDUMC<-scale(BLKATM$EDU,scale=FALSE)
#BLKATM$EDU2<-BLKATM$EDUMC*BLKATM$EDUMC
#BLKATM$OCCSMC<-scale(BLKATM$OCCS,scale=FALSE)
#BLKATM$OCCS2<-BLKATM$OCCSMC*BLKATM$OCCSMC
#BLKATM$OCCPMC<-scale(BLKATM$OCCP,scale=FALSE)
#BLKATM$OCCP2<-BLKATM$OCCPMC*BLKATM$OCCPMC

BLKOAF <- OAF[which(OAF$BLK == 1), ]
BLKOAF$INCMC<-scale(BLKOAF$INC,scale=FALSE)
BLKOAF$INC2<-BLKOAF$INCMC*BLKOAF$INCMC
BLKOAF$EDUMC<-scale(BLKOAF$EDU,scale=FALSE)
BLKOAF$EDU2<-BLKOAF$EDUMC*BLKOAF$EDUMC
#BLKOAF$OCCSMC<-scale(BLKOAF$OCCS,scale=FALSE)
#BLKOAF$OCCS2<-BLKOAF$OCCSMC*BLKOAF$OCCSMC
#BLKOAF$OCCPMC<-scale(BLKOAF$OCCP,scale=FALSE)
#BLKOAF$OCCP2<-BLKOAF$OCCPMC*BLKOAF$OCCPMC

BLKOAM <- OAM[which(OAM$BLK == 1), ]
BLKOAM$INCMC<-scale(BLKOAM$INC,scale=FALSE)
BLKOAM$INC2<-BLKOAM$INCMC*BLKOAM$INCMC
BLKOAM$EDUMC<-scale(BLKOAM$EDU,scale=FALSE)
BLKOAM$EDU2<-BLKOAM$EDUMC*BLKOAM$EDUMC
#BLKOAM$OCCSMC<-scale(BLKOAM$OCCS,scale=FALSE)
#BLKOAM$OCCS2<-BLKOAM$OCCSMC*BLKOAM$OCCSMC
#BLKOAM$OCCPMC<-scale(BLKOAM$OCCP,scale=FALSE)
#BLKOAM$OCCP2<-BLKOAM$OCCPMC*BLKOAM$OCCPMC

#AACHF <- CHF[which(CHF$AA == 1), ]
#AACHF$INCMC<-scale(AACHF$INC,scale=FALSE)
#AACHF$INC2<-AACHF$INCMC*AACHF$INCMC
#AACHF$EDUMC<-scale(AACHF$EDU,scale=FALSE)
#AACHF$EDU2<-AACHF$EDUMC*AACHF$EDUMC
#AACHF$OCCSMC<-scale(AACHF$OCCS,scale=FALSE)
#AACHF$OCCS2<-AACHF$OCCSMC*AACHF$OCCSMC
#AACHF$OCCPMC<-scale(AACHF$OCCP,scale=FALSE)
#AACHF$OCCP2<-AACHF$OCCPMC*AACHF$OCCPMC

#AACHM <- CHM[which(CHM$AA == 1), ]
#AACHM$INCMC<-scale(AACHM$INC,scale=FALSE)
#AACHM$INC2<-AACHM$INCMC*AACHM$INCMC
#AACHM$EDUMC<-scale(AACHM$EDU,scale=FALSE)
#AACHM$EDU2<-AACHM$EDUMC*AACHM$EDUMC
#AACHM$OCCSMC<-scale(AACHM$OCCS,scale=FALSE)
#AACHM$OCCS2<-AACHM$OCCSMC*AACHM$OCCSMC
#AACHM$OCCPMC<-scale(AACHM$OCCP,scale=FALSE)
#AACHM$OCCP2<-AACHM$OCCPMC*AACHM$OCCPMC

#AAADF <- ADF[which(ADF$AA == 1), ]
#AAADF$INCMC<-scale(AAADF$INC,scale=FALSE)
#AAADF$INC2<-AAADF$INCMC*AAADF$INCMC
#AAADF$EDUMC<-scale(AAADF$EDU,scale=FALSE)
#AAADF$EDU2<-AAADF$EDUMC*AAADF$EDUMC
##AAADF$OCCSMC<-scale(AAADF$OCCS,scale=FALSE)
#AAADF$OCCS2<-AAADF$OCCSMC*AAADF$OCCSMC
#AAADF$OCCPMC<-scale(AAADF$OCCP,scale=FALSE)
#AAADF$OCCP2<-AAADF$OCCPMC*AAADF$OCCPMC
#
#AAADM <- ADM[which(ADM$AA == 1), ]
#AAADM$INCMC<-scale(AAADM$INC,scale=FALSE)
#AAADM$INC2<-AAADM$INCMC*AAADM$INCMC
#AAADM$EDUMC<-scale(AAADM$EDU,scale=FALSE)
#AAADM$EDU2<-AAADM$EDUMC*AAADM$EDUMC
#AAADM$OCCSMC<-scale(AAADM$OCCS,scale=FALSE)
#AAADM$OCCS2<-AAADM$OCCSMC*AAADM$OCCSMC
#AAADM$OCCPMC<-scale(AAADM$OCCP,scale=FALSE)
#AAADM$OCCP2<-AAADM$OCCPMC*AAADM$OCCPMC

#AAYDF <- YDF[which(YDF$AA == 1), ]
#AAYDF$INCMC<-scale(AAYDF$INC,scale=FALSE)
#AAYDF$INC2<-AAYDF$INCMC*AAYDF$INCMC
#AAYDF$EDUMC<-scale(AAYDF$EDU,scale=FALSE)
#AAYDF$EDU2<-AAYDF$EDUMC*AAYDF$EDUMC
#AAYDF$OCCSMC<-scale(AAYDF$OCCS,scale=FALSE)
#AAYDF$OCCS2<-AAYDF$OCCSMC*AAYDF$OCCSMC
#AAYDF$OCCPMC<-scale(AAYDF$OCCP,scale=FALSE)
#AAYDF$OCCP2<-AAYDF$OCCPMC*AAYDF$OCCPMC
#
#AAYDM <- YDM[which(YDM$AA == 1), ]
#AAYDM$INCMC<-scale(AAYDM$INC,scale=FALSE)
#AAYDM$INC2<-AAYDM$INCMC*AAYDM$INCMC
#AAYDM$EDUMC<-scale(AAYDM$EDU,scale=FALSE)
#AAYDM$EDU2<-AAYDM$EDUMC*AAYDM$EDUMC
#AAYDM$OCCSMC<-scale(AAYDM$OCCS,scale=FALSE)
#AAYDM$OCCS2<-AAYDM$OCCSMC*AAYDM$OCCSMC
#AAYDM$OCCPMC<-scale(AAYDM$OCCP,scale=FALSE)
#AAYDM$OCCP2<-AAYDM$OCCPMC*AAYDM$OCCPMC

#AAATF <- ATF[which(ATF$AA == 1), ]
#AAATF$INCMC<-scale(AAATF$INC,scale=FALSE)
#AAATF$INC2<-AAATF$INCMC*AAATF$INCMC
#AAATF$EDUMC<-scale(AAATF$EDU,scale=FALSE)
#AAATF$EDU2<-AAATF$EDUMC*AAATF$EDUMC
#AAATF$OCCSMC<-scale(AAATF$OCCS,scale=FALSE)
#AAATF$OCCS2<-AAATF$OCCSMC*AAATF$OCCSMC
#AAATF$OCCPMC<-scale(AAATF$OCCP,scale=FALSE)
#AAATF$OCCP2<-AAATF$OCCPMC*AAATF$OCCPMC
#
#AAATM <- ATM[which(ATM$AA == 1), ]
#AAATM$INCMC<-scale(AAATM$INC,scale=FALSE)
#AAATM$INC2<-AAATM$INCMC*AAATM$INCMC
#AAATM$EDUMC<-scale(AAATM$EDU,scale=FALSE)
#AAATM$EDU2<-AAATM$EDUMC*AAATM$EDUMC
#AAATM$OCCSMC<-scale(AAATM$OCCS,scale=FALSE)
#AAATM$OCCS2<-AAATM$OCCSMC*AAATM$OCCSMC
#AAATM$OCCPMC<-scale(AAATM$OCCP,scale=FALSE)
#AAATM$OCCP2<-AAATM$OCCPMC*AAATM$OCCPMC

#AAOAF <- OAF[which(OAF$AA == 1), ]
#AAOAF$INCMC<-scale(AAOAF$INC,scale=FALSE)
#AAOAF$INC2<-AAOAF$INCMC*AAOAF$INCMC
#AAOAF$EDUMC<-scale(AAOAF$EDU,scale=FALSE)
#AAOAF$EDU2<-AAOAF$EDUMC*AAOAF$EDUMC
#AAOAF$OCCSMC<-scale(AAOAF$OCCS,scale=FALSE)
#AAOAF$OCCS2<-AAOAF$OCCSMC*AAOAF$OCCSMC
#AAOAF$OCCPMC<-scale(AAOAF$OCCP,scale=FALSE)
#AAOAF$OCCP2<-AAOAF$OCCPMC*AAOAF$OCCPMC
#
#AAOAM <- OAM[which(OAM$AA == 1), ]
#AAOAM$INCMC<-scale(AAOAM$INC,scale=FALSE)
#AAOAM$INC2<-AAOAM$INCMC*AAOAM$INCMC
#AAOAM$EDUMC<-scale(AAOAM$EDU,scale=FALSE)
#AAOAM$EDU2<-AAOAM$EDUMC*AAOAM$EDUMC
#AAOAM$OCCSMC<-scale(AAOAM$OCCS,scale=FALSE)
#AAOAM$OCCS2<-AAOAM$OCCSMC*AAOAM$OCCSMC
#AAOAM$OCCPMC<-scale(AAOAM$OCCP,scale=FALSE)
#AAOAM$OCCP2<-AAOAM$OCCPMC*AAOAM$OCCPMC

#AICHF <- CHF[which(CHF$AI == 1), ]
#AICHF$INCMC<-scale(AICHF$INC,scale=FALSE)
#AICHF$INC2<-AICHF$INCMC*AICHF$INCMC
#AICHF$EDUMC<-scale(AICHF$EDU,scale=FALSE)
#AICHF$EDU2<-AICHF$EDUMC*AICHF$EDUMC
#AICHF$OCCSMC<-scale(AICHF$OCCS,scale=FALSE)
#AICHF$OCCS2<-AICHF$OCCSMC*AICHF$OCCSMC
#AICHF$OCCPMC<-scale(AICHF$OCCP,scale=FALSE)
#AICHF$OCCP2<-AICHF$OCCPMC*AICHF$OCCPMC
#
#AICHM <- CHM[which(CHM$AI == 1), ]
#AICHM$INCMC<-scale(AICHM$INC,scale=FALSE)
#AICHM$INC2<-AICHM$INCMC*AICHM$INCMC
#AICHM$EDUMC<-scale(AICHM$EDU,scale=FALSE)
#AICHM$EDU2<-AICHM$EDUMC*AICHM$EDUMC
#AICHM$OCCSMC<-scale(AICHM$OCCS,scale=FALSE)
#AICHM$OCCS2<-AICHM$OCCSMC*AICHM$OCCSMC
#AICHM$OCCPMC<-scale(AICHM$OCCP,scale=FALSE)
#AICHM$OCCP2<-AICHM$OCCPMC*AICHM$OCCPMC

#AIADF <- ADF[which(ADF$AI == 1), ]
#AIADF$INCMC<-scale(AIADF$INC,scale=FALSE)
#AIADF$INC2<-AIADF$INCMC*AIADF$INCMC
#AIADF$EDUMC<-scale(AIADF$EDU,scale=FALSE)
#AIADF$EDU2<-AIADF$EDUMC*AIADF$EDUMC
#AIADF$OCCSMC<-scale(AIADF$OCCS,scale=FALSE)
#AIADF$OCCS2<-AIADF$OCCSMC*AIADF$OCCSMC
#AIADF$OCCPMC<-scale(AIADF$OCCP,scale=FALSE)
#AIADF$OCCP2<-AIADF$OCCPMC*AIADF$OCCPMC
#
#AIADM <- ADM[which(ADM$AI == 1), ]
#AIADM$INCMC<-scale(AIADM$INC,scale=FALSE)
#AIADM$INC2<-AIADM$INCMC*AIADM$INCMC
#AIADM$EDUMC<-scale(AIADM$EDU,scale=FALSE)
#AIADM$EDU2<-AIADM$EDUMC*AIADM$EDUMC
#AIADM$OCCSMC<-scale(AIADM$OCCS,scale=FALSE)
#AIADM$OCCS2<-AIADM$OCCSMC*AIADM$OCCSMC
#AIADM$OCCPMC<-scale(AIADM$OCCP,scale=FALSE)
#AIADM$OCCP2<-AIADM$OCCPMC*AIADM$OCCPMC

#AIYDF <- YDF[which(YDF$AI == 1), ]
#AIYDF$INCMC<-scale(AIYDF$INC,scale=FALSE)
#AIYDF$INC2<-AIYDF$INCMC*AIYDF$INCMC
#AIYDF$EDUMC<-scale(AIYDF$EDU,scale=FALSE)
#AIYDF$EDU2<-AIYDF$EDUMC*AIYDF$EDUMC
#AIYDF$OCCSMC<-scale(AIYDF$OCCS,scale=FALSE)
#AIYDF$OCCS2<-AIYDF$OCCSMC*AIYDF$OCCSMC
#AIYDF$OCCPMC<-scale(AIYDF$OCCP,scale=FALSE)
#AIYDF$OCCP2<-AIYDF$OCCPMC*AIYDF$OCCPMC
#
#AIYDM <- YDM[which(YDM$AI == 1), ]
#AIYDM$INCMC<-scale(AIYDM$INC,scale=FALSE)
#AIYDM$INC2<-AIYDM$INCMC*AIYDM$INCMC
#AIYDM$EDUMC<-scale(AIYDM$EDU,scale=FALSE)
#AIYDM$EDU2<-AIYDM$EDUMC*AIYDM$EDUMC
#AIYDM$OCCSMC<-scale(AIYDM$OCCS,scale=FALSE)
#AIYDM$OCCS2<-AIYDM$OCCSMC*AIYDM$OCCSMC
#AIYDM$OCCPMC<-scale(AIYDM$OCCP,scale=FALSE)
#AIYDM$OCCP2<-AIYDM$OCCPMC*AIYDM$OCCPMC

#AIATF <- ATF[which(ATF$AI == 1), ]
#AIATF$INCMC<-scale(AIATF$INC,scale=FALSE)
#AIATF$INC2<-AIATF$INCMC*AIATF$INCMC
#AIATF$EDUMC<-scale(AIATF$EDU,scale=FALSE)
#AIATF$EDU2<-AIATF$EDUMC*AIATF$EDUMC
#AIATF$OCCSMC<-scale(AIATF$OCCS,scale=FALSE)
#AIATF$OCCS2<-AIATF$OCCSMC*AIATF$OCCSMC
#AIATF$OCCPMC<-scale(AIATF$OCCP,scale=FALSE)
#AIATF$OCCP2<-AIATF$OCCPMC*AIATF$OCCPMC
#
#AIATM <- ATM[which(ATM$AI == 1), ]
#AIATM$INCMC<-scale(AIATM$INC,scale=FALSE)
#AIATM$INC2<-AIATM$INCMC*AIATM$INCMC
#AIATM$EDUMC<-scale(AIATM$EDU,scale=FALSE)
#AIATM$EDU2<-AIATM$EDUMC*AIATM$EDUMC
#AIATM$OCCSMC<-scale(AIATM$OCCS,scale=FALSE)
#AIATM$OCCS2<-AIATM$OCCSMC*AIATM$OCCSMC
#AIATM$OCCPMC<-scale(AIATM$OCCP,scale=FALSE)
#AIATM$OCCP2<-AIATM$OCCPMC*AIATM$OCCPMC

#AIOAF <- OAF[which(OAF$AI == 1), ]
#AIOAF$INCMC<-scale(AIOAF$INC,scale=FALSE)
#AIOAF$INC2<-AIOAF$INCMC*AIOAF$INCMC
#AIOAF$EDUMC<-scale(AIOAF$EDU,scale=FALSE)
#AIOAF$EDU2<-AIOAF$EDUMC*AIOAF$EDUMC
#AIOAF$OCCSMC<-scale(AIOAF$OCCS,scale=FALSE)
#AIOAF$OCCS2<-AIOAF$OCCSMC*AIOAF$OCCSMC
#AIOAF$OCCPMC<-scale(AIOAF$OCCP,scale=FALSE)
#AIOAF$OCCP2<-AIOAF$OCCPMC*AIOAF$OCCPMC
#
#AIOAM <- OAM[which(OAM$AI == 1), ]
#AIOAM$INCMC<-scale(AIOAM$INC,scale=FALSE)
#AIOAM$INC2<-AIOAM$INCMC*AIOAM$INCMC
#AIOAM$EDUMC<-scale(AIOAM$EDU,scale=FALSE)
#AIOAM$EDU2<-AIOAM$EDUMC*AIOAM$EDUMC
#AIOAM$OCCSMC<-scale(AIOAM$OCCS,scale=FALSE)
#AIOAM$OCCS2<-AIOAM$OCCSMC*AIOAM$OCCSMC
#AIOAM$OCCPMC<-scale(AIOAM$OCCP,scale=FALSE)
#AIOAM$OCCP2<-AIOAM$OCCPMC*AIOAM$OCCPMC

#MRCHF <- CHF[which(CHF$MR == 1), ]
#MRCHF$INCMC<-scale(MRCHF$INC,scale=FALSE)
#MRCHF$INC2<-MRCHF$INCMC*MRCHF$INCMC
#MRCHF$EDUMC<-scale(MRCHF$EDU,scale=FALSE)
#MRCHF$EDU2<-MRCHF$EDUMC*MRCHF$EDUMC
#MRCHF$OCCSMC<-scale(MRCHF$OCCS,scale=FALSE)
#MRCHF$OCCS2<-MRCHF$OCCSMC*MRCHF$OCCSMC
#MRCHF$OCCPMC<-scale(MRCHF$OCCP,scale=FALSE)
#MRCHF$OCCP2<-MRCHF$OCCPMC*MRCHF$OCCPMC
#
#MRCHM <- CHM[which(CHM$MR == 1), ]
#MRCHM$INCMC<-scale(MRCHM$INC,scale=FALSE)
#MRCHM$INC2<-MRCHM$INCMC*MRCHM$INCMC
#MRCHM$EDUMC<-scale(MRCHM$EDU,scale=FALSE)
#MRCHM$EDU2<-MRCHM$EDUMC*MRCHM$EDUMC
#MRCHM$OCCSMC<-scale(MRCHM$OCCS,scale=FALSE)
#MRCHM$OCCS2<-MRCHM$OCCSMC*MRCHM$OCCSMC
#MRCHM$OCCPMC<-scale(MRCHM$OCCP,scale=FALSE)
#MRCHM$OCCP2<-MRCHM$OCCPMC*MRCHM$OCCPMC

#MRADF <- ADF[which(ADF$MR == 1), ]
#MRADF$INCMC<-scale(MRADF$INC,scale=FALSE)
#MRADF$INC2<-MRADF$INCMC*MRADF$INCMC
#MRADF$EDUMC<-scale(MRADF$EDU,scale=FALSE)
#MRADF$EDU2<-MRADF$EDUMC*MRADF$EDUMC
#MRADF$OCCSMC<-scale(MRADF$OCCS,scale=FALSE)
#MRADF$OCCS2<-MRADF$OCCSMC*MRADF$OCCSMC
#MRADF$OCCPMC<-scale(MRADF$OCCP,scale=FALSE)
#MRADF$OCCP2<-MRADF$OCCPMC*MRADF$OCCPMC
#
#MRADM <- ADM[which(ADM$MR == 1), ]
#MRADM$INCMC<-scale(MRADM$INC,scale=FALSE)
#MRADM$INC2<-MRADM$INCMC*MRADM$INCMC
#MRADM$EDUMC<-scale(MRADM$EDU,scale=FALSE)
#MRADM$EDU2<-MRADM$EDUMC*MRADM$EDUMC
#MRADM$OCCSMC<-scale(MRADM$OCCS,scale=FALSE)
#MRADM$OCCS2<-MRADM$OCCSMC*MRADM$OCCSMC
#MRADM$OCCPMC<-scale(MRADM$OCCP,scale=FALSE)
#MRADM$OCCP2<-MRADM$OCCPMC*MRADM$OCCPMC

#MRYDF <- YDF[which(YDF$MR == 1), ]
#MRYDF$INCMC<-scale(MRYDF$INC,scale=FALSE)
#MRYDF$INC2<-MRYDF$INCMC*MRYDF$INCMC
#MRYDF$EDUMC<-scale(MRYDF$EDU,scale=FALSE)
#MRYDF$EDU2<-MRYDF$EDUMC*MRYDF$EDUMC
#MRYDF$OCCSMC<-scale(MRYDF$OCCS,scale=FALSE)
#MRYDF$OCCS2<-MRYDF$OCCSMC*MRYDF$OCCSMC
#MRYDF$OCCPMC<-scale(MRYDF$OCCP,scale=FALSE)
#MRYDF$OCCP2<-MRYDF$OCCPMC*MRYDF$OCCPMC
#
#MRYDM <- YDM[which(YDM$MR == 1), ]
#MRYDM$INCMC<-scale(MRYDM$INC,scale=FALSE)
#MRYDM$INC2<-MRYDM$INCMC*MRYDM$INCMC
#MRYDM$EDUMC<-scale(MRYDM$EDU,scale=FALSE)
#MRYDM$EDU2<-MRYDM$EDUMC*MRYDM$EDUMC
#MRYDM$OCCSMC<-scale(MRYDM$OCCS,scale=FALSE)
#MRYDM$OCCS2<-MRYDM$OCCSMC*MRYDM$OCCSMC
#MRYDM$OCCPMC<-scale(MRYDM$OCCP,scale=FALSE)
#MRYDM$OCCP2<-MRYDM$OCCPMC*MRYDM$OCCPMC

#MRATF <- ATF[which(ATF$MR == 1), ]
#MRATF$INCMC<-scale(MRATF$INC,scale=FALSE)
#MRATF$INC2<-MRATF$INCMC*MRATF$INCMC
#MRATF$EDUMC<-scale(MRATF$EDU,scale=FALSE)
#MRATF$EDU2<-MRATF$EDUMC*MRATF$EDUMC
#MRATF$OCCSMC<-scale(MRATF$OCCS,scale=FALSE)
#MRATF$OCCS2<-MRATF$OCCSMC*MRATF$OCCSMC
#MRATF$OCCPMC<-scale(MRATF$OCCP,scale=FALSE)
#MRATF$OCCP2<-MRATF$OCCPMC*MRATF$OCCPMC
#
#MRATM <- ATM[which(ATM$MR == 1), ]
#MRATM$INCMC<-scale(MRATM$INC,scale=FALSE)
#MRATM$INC2<-MRATM$INCMC*MRATM$INCMC
#MRATM$EDUMC<-scale(MRATM$EDU,scale=FALSE)
#MRATM$EDU2<-MRATM$EDUMC*MRATM$EDUMC
#MRATM$OCCSMC<-scale(MRATM$OCCS,scale=FALSE)
#MRATM$OCCS2<-MRATM$OCCSMC*MRATM$OCCSMC
#MRATM$OCCPMC<-scale(MRATM$OCCP,scale=FALSE)
#MRATM$OCCP2<-MRATM$OCCPMC*MRATM$OCCPMC

MROAF <- OAF[which(OAF$MR == 1), ]
MROAF$INCMC<-scale(MROAF$INC,scale=FALSE)
MROAF$INC2<-MROAF$INCMC*MROAF$INCMC
MROAF$EDUMC<-scale(MROAF$EDU,scale=FALSE)
MROAF$EDU2<-MROAF$EDUMC*MROAF$EDUMC
#MROAF$OCCSMC<-scale(MROAF$OCCS,scale=FALSE)
#MROAF$OCCS2<-MROAF$OCCSMC*MROAF$OCCSMC
#MROAF$OCCPMC<-scale(MROAF$OCCP,scale=FALSE)
#MROAF$OCCP2<-MROAF$OCCPMC*MROAF$OCCPMC

MROAM <- OAM[which(OAM$MR == 1), ]
MROAM$INCMC<-scale(MROAM$INC,scale=FALSE)
MROAM$INC2<-MROAM$INCMC*MROAM$INCMC
MROAM$EDUMC<-scale(MROAM$EDU,scale=FALSE)
MROAM$EDU2<-MROAM$EDUMC*MROAM$EDUMC
#MROAM$OCCSMC<-scale(MROAM$OCCS,scale=FALSE)
#MROAM$OCCS2<-MROAM$OCCSMC*MROAM$OCCSMC
#MROAM$OCCPMC<-scale(MROAM$OCCP,scale=FALSE)
#MROAM$OCCP2<-MROAM$OCCPMC*MROAM$OCCPMC

#Which variables should be included in correlation matrix? ----
#c("INCMC","INC2","EDUMC","EDU2","OCCSMC","OCCS2","OCCPMC","OCCP2","DEP","ACHV","DEPPW","ACHVPW")
matrixvars <- c("INCMC","INC2","EDUMC","EDU2","DEP")

detach(package:psych)
#Load packages ----
#install.packages("devtools") 
#devtools::install_github("easystats/correlation")
#Makowski, D., Ben-Shachar, M. S., Patil, I., & Lüdecke, D. (2019). Methods and Algorithms for Correlation Analysis in R. Journal of Open Source Software, 5(51), 2306. 10.21105/joss.02306
library("correlation")
#install.packages('MBESS') #for reliability estimates
library('MBESS')

##Correlations among White Participants -----
##Correlations among White, female, child participants 
##Select variables for correlation matrix
#WHTCHFC <- WHTCHF[matrixvars]
#cordat<-correlation(WHTCHFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTCHFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTCHF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTCHF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTCHFAGGRTED <- cbind(WHTCHFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among White, male, child participants 
##Select variables for correlation matrix
#WHTCHMC <- WHTCHM[matrixvars]
#cordat<-correlation(WHTCHMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTCHMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTCHM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTCHM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTCHMAGGRTED <- cbind(WHTCHMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among White, female, adolescent participants 
##Select variables for correlation matrix
#WHTADFC <- WHTADF[matrixvars]
#cordat<-correlation(WHTADFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTADFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTADF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTADF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTADFAGGRTED <- cbind(WHTADFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among White, male, adolescent participants 
##Select variables for correlation matrix
#WHTADMC <- WHTADM[matrixvars]
#cordat<-correlation(WHTADMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTADMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTADM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTADM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTADMAGGRTED <- cbind(WHTADMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among White, female, young adult participants 
##Select variables for correlation matrix
#WHTYDFC <- WHTYDF[matrixvars]
#cordat<-correlation(WHTYDFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTYDFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTYDF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTYDF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTYDFAGGRTED <- cbind(WHTYDFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among White, male, young adult participants 
##Select variables for correlation matrix
#WHTYDMC <- WHTYDM[matrixvars]
#cordat<-correlation(WHTYDMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTYDMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTYDM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTYDM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTYDMAGGRTED <- cbind(WHTYDMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among White, female, adult participants 
##Select variables for correlation matrix
#WHTATFC <- WHTATF[matrixvars]
#cordat<-correlation(WHTATFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTATFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTATF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTATF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTATFAGGRTED <- cbind(WHTATFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among White, male, adult participants 
##Select variables for correlation matrix
#WHTATMC <- WHTATM[matrixvars]
#cordat<-correlation(WHTATMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#WHTATMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-0
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=WHTATM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=WHTATM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#WHTATMAGGRTED <- cbind(WHTATMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

#Correlations among White, female, older adult participants 
#Select variables for correlation matrix
WHTOAFC <- WHTOAF[matrixvars]
cordat<-correlation(WHTOAFC)
cordfr<-data.frame(t(cordat$r))
names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
cordfn<-data.frame(t(cordat$n_Obs))
names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
WHTOAFAGGRTED<-cbind(cordfr,cordfn)
#Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
DATAID<-'3255'
names(DATAID)<-'DATAID'
#Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
DSID<-'1'
names(DSID)<-'DSID'
#Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
AAMEAS<-'NA'
names(AAMEAS)<-'AAMEAS'
#Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
DEVP<-4
names(DEVP)<-'DEVP'
#Assign sex code (0 = female, 1 = male)
MALE<-0
names(MALE)<-'MALE'
#Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
RACETH<-0
names(RACETH)<-'RACETH'
#Assign name of depressive symptom measure(use acronym with no hyphen)
DEPMEAS<-'CESD'
names(DEPMEAS)<-'DEPMEAS'
#Compute the average year of data collection 
DATAYEAR<-2001
names(DATAYEAR)<-'DATAYEAR'
#Compute reliability estimates
#Only need to change depressive symptoms items 
ALPHAOV <- ci.reliability(data=WHTOAF[2:9], type = "alpha", interval.type = "none")
OMEGAOV <- ci.reliability(data=WHTOAF[2:9], type = "omega", interval.type = "none")
ALPHA <- ALPHAOV$est
OMEGA <- OMEGAOV$est
#Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
WHTOAFAGGRTED <- cbind(WHTOAFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

#Correlations among White, male, older adult participants 
#Select variables for correlation matrix
WHTOAMC <- WHTOAM[matrixvars]
cordat<-correlation(WHTOAMC)
cordfr<-data.frame(t(cordat$r))
names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
cordfn<-data.frame(t(cordat$n_Obs))
names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
WHTOAMAGGRTED<-cbind(cordfr,cordfn)
#Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
DATAID<-'3255'
names(DATAID)<-'DATAID'
#Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
DSID<-'1'
names(DSID)<-'DSID'
#Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
AAMEAS<-'NA'
names(AAMEAS)<-'AAMEAS'
#Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
DEVP<-4
names(DEVP)<-'DEVP'
#Assign sex code (0 = female, 1 = male)
MALE<-1
names(MALE)<-'MALE'
#Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
RACETH<-0
names(RACETH)<-'RACETH'
#Assign name of depressive symptom measure(use acronym with no hyphen)
DEPMEAS<-'CESD'
names(DEPMEAS)<-'DEPMEAS'
#Compute the average year of data collection 
DATAYEAR<-2001
names(DATAYEAR)<-'DATAYEAR'
#Compute reliability estimates
#Only need to change depressive symptoms items 
ALPHAOV <- ci.reliability(data=WHTOAM[2:9], type = "alpha", interval.type = "none")
OMEGAOV <- ci.reliability(data=WHTOAM[2:9], type = "omega", interval.type = "none")
ALPHA <- ALPHAOV$est
OMEGA <- OMEGAOV$est
#Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
WHTOAMAGGRTED <- cbind(WHTOAMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Latino Participants -----
##Correlations among Latino, female, child participants 
##Select variables for correlation matrix
#LATCHFC <- LATCHF[matrixvars]
#cordat<-correlation(LATCHFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATCHFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
#ALPHAOV <- ci.reliability(data=LATCHF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATCHF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATCHFAGGRTED <- cbind(LATCHFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Latino, male, child participants
##Select variables for correlation matrix
#LATCHMC <- LATCHM[matrixvars]
#cordat<-correlation(LATCHMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATCHMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATCHM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATCHM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATCHMAGGRTED <- cbind(LATCHMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Latino, female, adolescent participants 
##Select variables for correlation matrix
#LATADFC <- LATADF[matrixvars]
#cordat<-correlation(LATADFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATADFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
#ALPHAOV <- ci.reliability(data=LATADF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATADF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATADFAGGRTED <- cbind(LATADFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Latino, male, adolescent participants
##Select variables for correlation matrix
#LATADMC <- LATADM[matrixvars]
#cordat<-correlation(LATADMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATADMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATADM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATADM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATADMAGGRTED <- cbind(LATADMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Latino, female, young adult participants 
##Select variables for correlation matrix
#LATYDFC <- LATYDF[matrixvars]
#cordat<-correlation(LATYDFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATYDFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATYDF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATYDF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATYDFAGGRTED <- cbind(LATYDFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Latino, male, young adult participants 
##Select variables for correlation matrix
#LATYDMC <- LATYDM[matrixvars]
#cordat<-correlation(LATYDMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATYDMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATYDM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATYDM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATYDMAGGRTED <- cbind(LATYDMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Latino, female, adult participants 
##Select variables for correlation matrix
#LATATFC <- LATATF[matrixvars]
#cordat<-correlation(LATATFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATATFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATATF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATATF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATATFAGGRTED <- cbind(LATATFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Latino, male, adult participants 
##Select variables for correlation matrix
#LATATMC <- LATATM[matrixvars]
#cordat<-correlation(LATATMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATATMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATATM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATATM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATATMAGGRTED <- cbind(LATATMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Latino, female, older adult participants 
##Select variables for correlation matrix
#LATOAFC <- LATOAF[matrixvars]
#cordat<-correlation(LATOAFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATOAFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-4
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATOAF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATOAF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATOAFAGGRTED <- cbind(LATOAFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Latino, male, older adult participants 
##Select variables for correlation matrix
#LATOAMC <- LATOAM[matrixvars]
#cordat<-correlation(LATOAMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#LATOAMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-4
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-1
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=LATOAM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=LATOAM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#LATOAMAGGRTED <- cbind(LATOAMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Black Participants ----
##Correlations among Black, female, child participants 
##Select variables for correlation matrix
#BLKCHFC <- BLKCHF[matrixvars]
#cordat<-correlation(BLKCHFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKCHFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKCHF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKCHF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKCHFAGGRTED <- cbind(BLKCHFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Black, male, child participants 
##Select variables for correlation matrix
#BLKCHMC <- BLKCHM[matrixvars]
#cordat<-correlation(BLKCHMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKCHMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKCHM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKCHM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKCHMAGGRTED <- cbind(BLKCHMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Black, female, adolescent participants 
##Select variables for correlation matrix
#BLKADFC <- BLKADF[matrixvars]
#cordat<-correlation(BLKADFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKADFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKADF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKADF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKADFAGGRTED <- cbind(BLKADFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Black, male, adolescent participants 
##Select variables for correlation matrix
#BLKADMC <- BLKADM[matrixvars]
#cordat<-correlation(BLKADMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKADMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKADM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKADM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKADMAGGRTED <- cbind(BLKADMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Black, female, young adult participants 
##Select variables for correlation matrix
#BLKYDFC <- BLKYDF[matrixvars]
#cordat<-correlation(BLKYDFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKYDFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKYDF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKYDF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKYDFAGGRTED <- cbind(BLKYDFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Black, male, young adult participants 
##Select variables for correlation matrix
#BLKYDMC <- BLKYDM[matrixvars]
#cordat<-correlation(BLKYDMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKYDMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKYDM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKYDM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKYDMAGGRTED <- cbind(BLKYDMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Black, female, adult participants 
##Select variables for correlation matrix
#BLKATFC <- BLKATF[matrixvars]
#cordat<-correlation(BLKATFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKATFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKATF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKATF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKATFAGGRTED <- cbind(BLKATFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Black, male, adult participants 
##Select variables for correlation matrix
#BLKATMC <- BLKATM[matrixvars]
#cordat<-correlation(BLKATMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#BLKATMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-2
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=BLKATM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=BLKATM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#BLKATMAGGRTED <- cbind(BLKATMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

#Correlations among Black, female, older adult participants 
#Select variables for correlation matrix
BLKOAFC <- BLKOAF[matrixvars]
cordat<-correlation(BLKOAFC)
cordfr<-data.frame(t(cordat$r))
names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
cordfn<-data.frame(t(cordat$n_Obs))
names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
BLKOAFAGGRTED<-cbind(cordfr,cordfn)
#Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
DATAID<-'3255'
names(DATAID)<-'DATAID'
#Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
DSID<-'1'
names(DSID)<-'DSID'
#Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
AAMEAS<-'NA'
names(AAMEAS)<-'AAMEAS'
#Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
DEVP<-4
names(DEVP)<-'DEVP'
#Assign sex code (0 = female, 1 = male)
MALE<-0
names(MALE)<-'MALE'
#Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
RACETH<-2
names(RACETH)<-'RACETH'
#Assign name of depressive symptom measure(use acronym with no hyphen)
DEPMEAS<-'CESD'
names(DEPMEAS)<-'DEPMEAS'
#Compute the average year of data collection 
DATAYEAR<-2001
names(DATAYEAR)<-'DATAYEAR'
#Compute reliability estimates
#Only need to change depressive symptoms items 
ALPHAOV <- ci.reliability(data=BLKOAF[2:9], type = "alpha", interval.type = "none")
OMEGAOV <- ci.reliability(data=BLKOAF[2:9], type = "omega", interval.type = "none")
ALPHA <- ALPHAOV$est
OMEGA <- OMEGAOV$est
#Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
BLKOAFAGGRTED <- cbind(BLKOAFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

#Correlations among Black, male, older adult participants 
#Select variables for correlation matrix
BLKOAMC <- BLKOAM[matrixvars]
cordat<-correlation(BLKOAMC)
cordfr<-data.frame(t(cordat$r))
names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
cordfn<-data.frame(t(cordat$n_Obs))
names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
BLKOAMAGGRTED<-cbind(cordfr,cordfn)
#Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
DATAID<-'3255'
names(DATAID)<-'DATAID'
#Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
DSID<-'1'
names(DSID)<-'DSID'
#Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
AAMEAS<-'NA'
names(AAMEAS)<-'AAMEAS'
#Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
DEVP<-4
names(DEVP)<-'DEVP'
#Assign sex code (0 = female, 1 = male)
MALE<-1
names(MALE)<-'MALE'
#Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
RACETH<-2
names(RACETH)<-'RACETH'
#Assign name of depressive symptom measure(use acronym with no hyphen)
DEPMEAS<-'CESD'
names(DEPMEAS)<-'DEPMEAS'
#Compute the average year of data collection 
DATAYEAR<-2001
names(DATAYEAR)<-'DATAYEAR'
#Compute reliability estimates
#Only need to change depressive symptoms items 
ALPHAOV <- ci.reliability(data=BLKOAM[2:9], type = "alpha", interval.type = "none")
OMEGAOV <- ci.reliability(data=BLKOAM[2:9], type = "omega", interval.type = "none")
ALPHA <- ALPHAOV$est
OMEGA <- OMEGAOV$est
#Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
BLKOAMAGGRTED <- cbind(BLKOAMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Asian American Participants ----
##Correlations among Asian American, female, child participants 
##Select variables for correlation matrix
#AACHFC <- AACHF[matrixvars]
#cordat<-correlation(AACHFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AACHFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AACHF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AACHF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AACHFAGGRTED <- cbind(AACHFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Asian American, male, child participants 
##Select variables for correlation matrix
#AACHMC <- AACHM[matrixvars]
#cordat<-correlation(AACHMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AACHMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AACHMC[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AACHMC[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AACHMAGGRTED <- cbind(AACHMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Asian American, female, adolescent participants 
##Select variables for correlation matrix
#AAADFC <- AAADF[matrixvars]
#cordat<-correlation(AAADFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAADFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAADF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAADF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAADFAGGRTED <- cbind(AAADFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Asian American, male, adolescent participants 
##Select variables for correlation matrix
#AAADMC <- AAADM[matrixvars]
#cordat<-correlation(AAADMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAADMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAADM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAADM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAADMAGGRTED <- cbind(AAADMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Asian American, female, young adult participants 
##Select variables for correlation matrix
#AAYDFC <- AAYDF[matrixvars]
#cordat<-correlation(AAYDFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAYDFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAYDF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAYDF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAYDFAGGRTED <- cbind(AAYDFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Asian American, male, young adult participants 
##Select variables for correlation matrix
#AAYDMC <- AAYDM[matrixvars]
#cordat<-correlation(AAYDMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAYDMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAYDM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAYDM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAYDMAGGRTED <- cbind(AAYDMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Asian American, female, adult participants 
##Select variables for correlation matrix
#AAATFC <- AAATF[matrixvars]
#cordat<-correlation(AAATFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAATFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAATF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAATF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAATFAGGRTED <- cbind(AAATFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Asian American, male, adult participants 
##Select variables for correlation matrix
#AAATMC <- AAATM[matrixvars]
#cordat<-correlation(AAATMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAATMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAATM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAATM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAATMAGGRTED <- cbind(AAATMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Asian American, female, older adult participants 
##Select variables for correlation matrix
#AAOAFC <- AAOAF[matrixvars]
#cordat<-correlation(AAOAFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAOAFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-4
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAOAF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAOAF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAOAFAGGRTED <- cbind(AAOAFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Asian American, male, older adult participants 
##Select variables for correlation matrix
#AAOAMC <- AAOAM[matrixvars]
#cordat<-correlation(AAOAMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AAOAMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-4
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-3
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AAOAM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AAOAM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AAOAMAGGRTED <- cbind(AAOAMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among American Indian Participants ----
##Correlations among American Indian, female, child participants 
##Select variables for correlation matrix
#AICHFC <- AICHF[matrixvars]
#cordat<-correlation(AICHFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AICHFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AICHF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AICHF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AICHFAGGRTED <- cbind(AICHFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among American Indian, male, child participants 
##Select variables for correlation matrix
#AICHMC <- AICHM[matrixvars]
#cordat<-correlation(AICHMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AICHMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AICHM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AICHM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AICHMAGGRTED <- cbind(AICHMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among American Indian, female, adolescent participants 
##Select variables for correlation matrix
#AIADFC <- AIADF[matrixvars]
#cordat<-correlation(AIADFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIADFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIADF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIADF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIADFAGGRTED <- cbind(AIADFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                   DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among American Indian, male, adolescent participants 
##Select variables for correlation matrix
#AIADMC <- AIADM[matrixvars]
#cordat<-correlation(AIADMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIADMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIADM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIADM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIADMAGGRTED <- cbind(AIADMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                   DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among American Indian, female, young adult participants 
##Select variables for correlation matrix
#AIYDFC <- AIYDF[matrixvars]
#cordat<-correlation(AIYDFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIYDFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIYDF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIYDF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIYDFAGGRTED <- cbind(AIYDFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among American Indian, male, young adult participants 
##Select variables for correlation matrix
#AIYDMC <- AIYDM[matrixvars]
#cordat<-correlation(AIYDMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIYDMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIYDM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIYDM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIYDMAGGRTED <- cbind(AIYDMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among American Indian, female, adult participants 
##Select variables for correlation matrix
#AIATFC <- AIATF[matrixvars]
#cordat<-correlation(AIATFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIATFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIATF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIATF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIATFAGGRTED <- cbind(AIATFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among American Indian, male, adult participants 
##Select variables for correlation matrix
#AIATMC <- AIATM[matrixvars]
#cordat<-correlation(AIATMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIATMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIATM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIATM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIATMAGGRTED <- cbind(AIATMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among American Indian, female, older adult participants 
##Select variables for correlation matrix
#AIOAFC <- AIOAF[matrixvars]
#cordat<-correlation(AIOAFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIOAFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-4
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIOAF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIOAF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIOAFAGGRTED <- cbind(AIOAFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among American Indian, male, older adult participants 
##Select variables for correlation matrix
#AIOAMC <- AIOAM[matrixvars]
#cordat<-correlation(AIOAMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#AIOAMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-4
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-4
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=AIOAM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=AIOAM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#AIOAMAGGRTED <- cbind(AIOAMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Mixed Race Participants ----
##Correlations among Mixed Race, female, child participants 
##Select variables for correlation matrix
#MRCHFC <- MRCHF[matrixvars]
#cordat<-correlation(MRCHFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRCHFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRCHF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRCHF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRCHFAGGRTED <- cbind(MRCHFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Mixed Race, male, child participants 
##Select variables for correlation matrix
#MRCHMC <- MRCHM[matrixvars]
#cordat<-correlation(MRCHMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRCHMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-0
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRCHM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRCHM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRCHMAGGRTED <- cbind(MRCHMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Mixed Race, female, adolescent participants 
##Select variables for correlation matrix
#MRADFC <- MRADF[matrixvars]
#cordat<-correlation(MRADFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRADFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRADF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRADF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRADFAGGRTED <- cbind(MRADFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Mixed Race, male, adolescent participants 
##Select variables for correlation matrix
#MRADMC <- MRADM[matrixvars]
#cordat<-correlation(MRADMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRADMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-1
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRADM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRADM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRADMAGGRTED <- cbind(MRADMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Mixed Race, female, young adult participants 
##Select variables for correlation matrix
#MRYDFC <- MRYDF[matrixvars]
#cordat<-correlation(MRYDFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRYDFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRYDF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRYDF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRYDFAGGRTED <- cbind(MRYDFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Mixed Race, male, young adult participants 
##Select variables for correlation matrix
#MRYDMC <- MRYDM[matrixvars]
#cordat<-correlation(MRYDMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRYDMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-2
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRYDM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRYDM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRYDMAGGRTED <- cbind(MRYDMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Correlations among Mixed Race, female, adult participants 
##Select variables for correlation matrix
#MRATFC <- MRATF[matrixvars]
#cordat<-correlation(MRATFC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRATFAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-0
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRATF[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRATF[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRATFAGGRTED <- cbind(MRATFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)
#
##Correlations among Mixed Race, male, adult participants 
##Select variables for correlation matrix
#MRATMC <- MRATM[matrixvars]
#cordat<-correlation(MRATMC)
#cordfr<-data.frame(t(cordat$r))
#names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
#cordfn<-data.frame(t(cordat$n_Obs))
#names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
#MRATMAGGRTED<-cbind(cordfr,cordfn)
##Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
#DATAID<-'3255'
#names(DATAID)<-'DATAID'
##Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
#DSID<-'1'
#names(DSID)<-'DSID'
##Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
#AAMEAS<-'NA'
#names(AAMEAS)<-'AAMEAS'
##Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
#DEVP<-3
#names(DEVP)<-'DEVP'
##Assign sex code (0 = female, 1 = male)
#MALE<-1
#names(MALE)<-'MALE'
##Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
#RACETH<-5
#names(RACETH)<-'RACETH'
##Assign name of depressive symptom measure(use acronym with no hyphen)
#DEPMEAS<-'CESD'
#names(DEPMEAS)<-'DEPMEAS'
##Compute the average year of data collection 
#DATAYEAR<-2001
#names(DATAYEAR)<-'DATAYEAR'
##Compute reliability estimates
##Only need to change depressive symptoms items 
#ALPHAOV <- ci.reliability(data=MRATM[2:9], type = "alpha", interval.type = "none")
#OMEGAOV <- ci.reliability(data=MRATM[2:9], type = "omega", interval.type = "none")
#ALPHA <- ALPHAOV$est
#OMEGA <- OMEGAOV$est
##Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
#MRATMAGGRTED <- cbind(MRATMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
#                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

#Correlations among Mixed Race, female, older adult participants 
#Select variables for correlation matrix
MROAFC <- MROAF[matrixvars]
cordat<-correlation(MROAFC)
cordfr<-data.frame(t(cordat$r))
names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
cordfn<-data.frame(t(cordat$n_Obs))
names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
MROAFAGGRTED<-cbind(cordfr,cordfn)
#Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
DATAID<-'3255'
names(DATAID)<-'DATAID'
#Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
DSID<-'1'
names(DSID)<-'DSID'
#Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
AAMEAS<-'NA'
names(AAMEAS)<-'AAMEAS'
#Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
DEVP<-4
names(DEVP)<-'DEVP'
#Assign sex code (0 = female, 1 = male)
MALE<-0
names(MALE)<-'MALE'
#Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
RACETH<-5
names(RACETH)<-'RACETH'
#Assign name of depressive symptom measure(use acronym with no hyphen)
DEPMEAS<-'CESD'
names(DEPMEAS)<-'DEPMEAS'
#Compute the average year of data collection 
DATAYEAR<-2001
names(DATAYEAR)<-'DATAYEAR'
#Compute reliability estimates
#Only need to change depressive symptoms items 
ALPHAOV <- ci.reliability(data=MROAF[2:9], type = "alpha", interval.type = "none")
OMEGAOV <- ci.reliability(data=MROAF[2:9], type = "omega", interval.type = "none")
ALPHA <- ALPHAOV$est
OMEGA <- OMEGAOV$est
#Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
MROAFAGGRTED <- cbind(MROAFAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

#Correlations among Mixed Race, male, older adult participants 
#Select variables for correlation matrix
MROAMC <- MROAM[matrixvars]
cordat<-correlation(MROAMC)
cordfr<-data.frame(t(cordat$r))
names(cordfr)[1:10]<-c(paste(cordat$Parameter1,cordat$Parameter2,sep = ""))
cordfn<-data.frame(t(cordat$n_Obs))
names(cordfn)[1:10]<-c(paste("N",colnames(cordfr),sep = ""))
MROAMAGGRTED<-cbind(cordfr,cordfn)
#Assign ICPSR ID number (ICPSR Number; e.g., file name = ID3255DS1, then DATAID = 21600)
DATAID<-'3255'
names(DATAID)<-'DATAID'
#Dataset number (ICPSR Dataset number; e.g., file name = ID216DS1, then DSID = 1)
DSID<-'1'
names(DSID)<-'DSID'
#Academic Achievement Measure (0 = GPA, 1 = Grades Recoded, 2 = Both)
AAMEAS<-'NA'
names(AAMEAS)<-'AAMEAS'
#Developmental period: children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
DEVP<-4
names(DEVP)<-'DEVP'
#Assign sex code (0 = female, 1 = male)
MALE<-1
names(MALE)<-'MALE'
#Assign race/ethnicity code (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
RACETH<-5
names(RACETH)<-'RACETH'
#Assign name of depressive symptom measure(use acronym with no hyphen)
DEPMEAS<-'CESD'
names(DEPMEAS)<-'DEPMEAS'
#Compute the average year of data collection 
DATAYEAR<-2001
names(DATAYEAR)<-'DATAYEAR'
#Compute reliability estimates
#Only need to change depressive symptoms items 
ALPHAOV <- ci.reliability(data=MROAM[2:9], type = "alpha", interval.type = "none")
OMEGAOV <- ci.reliability(data=MROAM[2:9], type = "omega", interval.type = "none")
ALPHA <- ALPHAOV$est
OMEGA <- OMEGAOV$est
#Combine columns of aggregated data and computed codes for meta-analysis (do not include individual scores here)
MROAMAGGRTED <- cbind(MROAMAGGRTED[],DATAID,DSID,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,
                    DATAYEAR,ALPHA,OMEGA,row.names=NULL)

##Combine Correlation Dataframes ----
#Create overall dataset with all aggregate correlation datasets
ID3255DS1_aggregated<-dplyr::bind_rows(
#WHTCHFAGGRTED,
#WHTCHMAGGRTED,
#WHTADFAGGRTED,
#WHTADMAGGRTED,
#WHTYDFAGGRTED,
#WHTYDMAGGRTED,
#WHTATFAGGRTED,
#WHTATMAGGRTED,
WHTOAFAGGRTED,
WHTOAMAGGRTED,
#LATCHFAGGRTED,
#LATCHMAGGRTED,
#LATADFAGGRTED,
#LATADMAGGRTED,
#LATYDFAGGRTED,
#LATYDMAGGRTED,
#LATATFAGGRTED,
#LATATMAGGRTED,
#LATOAFAGGRTED,
#LATOAMAGGRTED,
#BLKCHFAGGRTED,
#BLKCHMAGGRTED,
#BLKADFAGGRTED,
#BLKADMAGGRTED,
#BLKYDFAGGRTED,
#BLKYDMAGGRTED,
#BLKATFAGGRTED,
#BLKATMAGGRTED,
BLKOAFAGGRTED,
BLKOAMAGGRTED,
#AACHFAGGRTED,
#AACHMAGGRTED,
#AAADFAGGRTED,
#AAADMAGGRTED,
#AAYDFAGGRTED,
#AAYDMAGGRTED,
#AAATFAGGRTED,
#AAATMAGGRTED,
#AAOAFAGGRTED,
#AAOAMAGGRTED,
#AICHFAGGRTED,
#AICHMAGGRTED,
#AIADFAGGRTED,
#AIADMAGGRTED,
#AIYDFAGGRTED,
#AIYDMAGGRTED,
#AIATFAGGRTED,
#AIATMAGGRTED,
#AIOAFAGGRTED,
#AIOAMAGGRTED,
#MRCHFAGGRTED,
#MRCHMAGGRTED,
#MRADFAGGRTED,
#MRADMAGGRTED,
#MRYDFAGGRTED,
#MRYDMAGGRTED,
#MRATFAGGRTED,
#MRATMAGGRTED,
MROAFAGGRTED,
MROAMAGGRTED
)

#Save aggregated data into a .csv file  ----
#Be sure to change the name (ALL AGGREGRATED FILES)
write.csv(ID3255DS1_aggregated, file = 'ID3255DS1_aggregated.csv',row.names = FALSE)