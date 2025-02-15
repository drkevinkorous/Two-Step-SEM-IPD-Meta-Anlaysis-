#### Descriptive Information ----
## Load Dataset
sesipd.long<-readRDS('sesipd.long.RDS')
load('SESIPD_DSTATCESD.rda')
## Load Packages
library(tidyverse)
#### IPD CHARACTERISTICS ####
## Number of independent studies 
nlevels(as.factor(sesipd.long$IPDID))
## Number of ICPSR datasets (IPD)
nlevels(as.factor(sesipd.long$DATAID))
## Total number of individual datasets
nlevels(as.factor(SESIPD_DSTATCESD$DFID))
# Number of correlation matrices
nlevels(as.factor(sesipd.long$cormid))
## Total number of correlations and sample size for each correlation
samplesize<-data.frame(nes=sesipd.long%>%group_by(cell)%>%summarise(n=n(),ntot=sum(ntot)))
samplesize<-samplesize%>%arrange(factor(nes.cell,
                                        levels=c("INCMCINC2","INCMCEDUMC","INCMCEDU2","INCMCOCCSMC","INCMCOCCS2","INCMCOCCPMC","INCMCOCCP2","INCMCDEP","INCMCDEPPW",
                                        "INC2EDUMC","INC2EDU2","INC2OCCSMC","INC2OCCS2","INC2OCCPMC","INC2OCCP2","INC2DEP","INC2DEPPW",
                                        "EDUMCEDU2","EDUMCOCCSMC","EDUMCOCCS2","EDUMCOCCPMC","EDUMCOCCP2","EDUMCDEP","EDUMCDEPPW",
                                        "EDU2OCCSMC","EDU2OCCS2","EDU2OCCPMC","EDU2OCCP2","EDU2DEP","EDU2DEPPW",
                                        "OCCSMCOCCS2","OCCSMCOCCPMC","OCCSMCOCCP2","OCCSMCDEP","OCCSMCDEPPW",
                                        "OCCS2OCCPMC","OCCS2OCCP2","OCCS2DEP","OCCS2DEPPW",
                                        "OCCPMCOCCP2","OCCPMCDEP","OCCPMCDEPPW",
                                        "OCCP2DEP","OCCP2DEPPW",
                                        "DEPDEPPW")))
write.csv(samplesize,file="samplesize.csv")

## Total sample size
# Code takes largest sample size within each correlation matrix from each dataset
sum(aggregate(ntot~cormid,data=sesipd.long,max)$ntot)
# Sample size for each dataset
totsampsize<-sesipd.long%>%group_by(DFID,cormid)%>%summarise(ntot=max(ntot),DEVP=max(DEVP),
                                                             MALE=max(MALE),RACETH=max(RACETH))
min(data.frame(totsampsize%>%group_by(DFID)%>%summarise(ntot=sum(ntot)))$ntot)
max(data.frame(totsampsize%>%group_by(DFID)%>%summarise(ntot=sum(ntot)))$ntot)
mean(data.frame(totsampsize%>%group_by(DFID)%>%summarise(ntot=sum(ntot)))$ntot)

## AGE & DEVELOPMENTAL PERIOD ----
dsage<-SESIPD_DSTATCESD[which(SESIPD_DSTATCESD$VAR=="dstatsage"),]
# Mean age across datasets
(dsaged<-data.frame(psych::describe(dsage),stats=rownames(psych::describe(dsage))))['mean',]
# Sample size per Developmental Period
# adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
totsampsize%>%group_by(DEVP)%>%summarise(ntot=sum(ntot))

## SEX -----
# Sample size for Males (1) and Females (0)
totsampsize%>%group_by(MALE)%>%summarise(ntot=sum(ntot))

## RACE/ETHNICITY ----
# Sample size and proportion across datasets (WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
totsampsize%>%group_by(RACETH)%>%summarise(ntot=sum(ntot))

## INCOME ----
dsinc<-SESIPD_DSTATCESD[which(SESIPD_DSTATCESD$VAR=="dstatsinc"),]
print(dsincd<-psych::describe(dsinc))

## EDUCATION ----
dsedu<-SESIPD_DSTATCESD[which(SESIPD_DSTATCESD$VAR=="dstatsedu"),]
print(dsedud<-psych::describe(dsedu))

## OCCUPATIONAL STATUS ----
dsoccs<-SESIPD_DSTATCESD[which(SESIPD_DSTATCESD$VAR=="dstatsoccs"),]
print(dsoccsd<-psych::describe(dsoccs))

## OCCUPATIONAL PRESTIGE ----
dsoccp<-SESIPD_DSTATCESD[which(SESIPD_DSTATCESD$VAR=="dstatsoccp"),]
print(dsoccpd<-psych::describe(dsoccp))

## DEPRESSIVE SYMPTOMS ----
# Note that the mean of depressive symptoms is not comparable across all datasets (differing response scales) 
dsdep<-SESIPD_DSTATCESD[which(SESIPD_DSTATCESD$VAR=="dstatsdep"),]
print(dsdepd<-psych::describe(dsdep))
# Confirm Type of Measures is all CESD
table(sesipd.long$DEPMEAS)

#### AGGREGATE CHARACTERISTICS ####
# CORRELATION MATRICES
# Total number of matrices
nrow(sesipd.long)
# Total number of matrices per datasets
data.frame(sesipd.long%>%group_by(DFID)%>%summarise(ncormatrices=nlevels(factor(cormid))))
#Total number of matrices per DEVP
data.frame(sesipd.long%>%group_by(DEVP)%>%summarise(ncormatrices=nlevels(factor(cormid))))
#Total number of matrices per Female and Male
data.frame(sesipd.long%>%group_by(MALE)%>%summarise(ncormatrices=nlevels(factor(cormid))))
#Total number of matrices per race/ethnicity
data.frame(sesipd.long%>%group_by(RACETH)%>%summarise(ncormatrices=nlevels(factor(cormid))))
# By DEVP, MALE, and RACETH 
data.frame(sesipd.long%>%group_by(MALE,DEVP,RACETH)%>%summarise(ncormatrices=nlevels(factor(cormid))))

# Range of correlations between SES components and depressive symptoms
data.frame(sesipd.long%>%group_by(cell)%>%summarise(smallest=min(esr),largest=max(esr)))




