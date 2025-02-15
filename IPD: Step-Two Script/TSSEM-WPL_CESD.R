#---------------------------------------------------------------------------
### SYNTAX FOR TSSEM ANALYSES WITH WPL FOR DEPENDENCIES: RQ #1 and RQ #2 
#---------------------------------------------------------------------------
# Load Packages
library(metaSEM)
library(metafor)
library(corpcor)
library(tidyverse)
#library(semPlot)

# Load Data
load('SESIPD_CESD.rda') 

### REFORMAT DATA ----
## Transform data from wide to long 
# Rename colnames to facilitate pivot_long
names(SESIPD_CESD)[4:48]<-paste("esr",names(SESIPD_CESD[4:48]),sep="_")
names(SESIPD_CESD)[49:93]<-stringr::str_sub(names(SESIPD_CESD[49:93]),2)
names(SESIPD_CESD)[49:93]<-paste("ntot",names(SESIPD_CESD[49:93]),sep="_")

# Add Unique ID for each correlation matrix 
SESIPD_CESD$cormid<-as.numeric(rownames(SESIPD_CESD))

# pivot_long (stacking effect size and sample size for each correlation)
sesipd.long<-SESIPD_CESD%>%pivot_longer(cols=starts_with(c("esr_","ntot_")),
                                        names_to=c(".value","cell"),
                                        names_sep="_",
                                        values_drop_na=T)

# Calculate sampling variance for each effect size
sesipd.long$esr.v<-escalc(measure="COR", ri=esr, ni=ntot, data=sesipd.long, vtype="AV")$vi

# Filter out rows with no effect sizes and is.na(esr.v)-(unnecessary to keep in this file)
sesipd.long<-sesipd.long%>%filter(!is.na(esr),!is.na(esr.v))

# Add Unique ID for each effect size
sesipd.long$esrid<-as.numeric(rownames(sesipd.long))

# Save reformatted data
saveRDS(sesipd.long,file='sesipd.long.RDS')

### MODEL 1A --------------------
## TSSEM: Stage 1 ----
m1astge1<-rma.mv(yi=esr,V=esr.v, 
                 data=filter(sesipd.long,!grepl('INC2|EDU2|OCCS2|OCCP|OCCP2',cell)),
                 random=~1|DFID/esrid,
                 method="ML",
                 mods=~factor(cell)-1,
                 verbose=2,
                 sparse = T) 
summary(m1astge1)
# Save model output
saveRDS(m1astge1,file="m1astge1.RDS")

## TSSEM: Stage 2 ----
m1astge1<-readRDS('m1astge1.RDS')
# Extract asymptotic covariance matrix from model output
m1acov<-m1astge1$vb

# Define correlation matrix
m1acor<-corpcor::vec2sm(vec = c(1,m1astge1$b[7],m1astge1$b[8],m1astge1$b[5],m1astge1$b[6],
                                1,m1astge1$b[4],m1astge1$b[2],m1astge1$b[3],
                                1,m1astge1$b[9],m1astge1$b[10],
                                1,m1astge1$b[1],
                                1),diag=T)

# Add column names to correlation matrix
colnames(m1acor)<-c("INC","EDU","OCCS","DEP","DEPPW")
rownames(m1acor)<-c("INC","EDU","OCCS","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
m1acov<-m1acov[c(7,8,5,6,4,2,3,9,10,1),c(7,8,5,6,4,2,3,9,10,1)]

# Define Matrix A - free direct path parameters
m1aA<-create.mxMatrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        '0.1*inctodep','0.1*edutodep','0.1*occstodep',0,'0.1*deppwtodep',
                        0,0,0,0,0),
                        type="Full",nrow=5,ncol=5,byrow=T,name = "A",
                        dimnames = list(colnames(m1acor), colnames(m1acor)))

# Define Matrix S - free variances and covariances
m1aS<-create.mxMatrix(c(1,
                        '0.1*incWedu',1,
                        '0.1*incWoccs','0.1*eduWoccs',1,
                        0,0,0,'1*Errdep',
                        '0.1*incWdeppw','0.1*eduWdeppw','0.1*occsWdeppw',0,1), 
                        type="Symm",byrow=T,name="S", 
                        dimnames = list(colnames(m1acor),colnames(m1acor)))

# Fit Model 1A
m1astge2<-wls(Cov=m1acor,aCov=m1acov,
              n=sum(aggregate(ntot~cormid,data=sesipd.long,max)$ntot),
              Amatrix=m1aA,Smatrix=m1aS) 
sink(file='m1astge2.txt')
summary(m1astge2)
sink()
saveRDS(m1astge2,file='m1astge2.RDS')

### MODEL 1B --------------------
## TSSEM: Stage 1 -----
m1bstge1<-rma.mv(yi=esr,V=esr.v, 
                 data=filter(sesipd.long,!grepl('INC2|EDU2|OCCP2|OCCS|OCCS2',cell)),
                 random=~1|DFID/esrid,
                 method="ML",
                 mods=~factor(cell)-1,
                 verbose=2,
                 sparse = T) 
summary(m1bstge1)

saveRDS(m1bstge1,file="m1bstge1.RDS")

## TSSEM: Stage 2 ----
m1bstge1<-readRDS('m1bstge1.RDS')
# Extract asymptotic covariance matrix from model output
m1bcov<-m1bstge1$vb

# Define correlation matrix
m1bcor<-corpcor::vec2sm(vec = c(1,m1bstge1$b[7],m1bstge1$b[8],m1bstge1$b[5],m1bstge1$b[6],
                                1,m1bstge1$b[4],m1bstge1$b[2],m1bstge1$b[3],
                                1,m1bstge1$b[9],m1bstge1$b[10],
                                1,m1bstge1$b[1],
                                1),diag=T)

# Add column names to correlation matrix
colnames(m1bcor)<-c("INC","EDU","OCCP","DEP","DEPPW")
rownames(m1bcor)<-c("INC","EDU","OCCP","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
m1bcov<-m1bcov[c(7,8,5,6,4,2,3,9,10,1),c(7,8,5,6,4,2,3,9,10,1)]

# Define Matrix A - free direct path parameters
m1bA<-create.mxMatrix(c(0,0,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0,
                        '0.1*inctodep','0.1*edutodep','0.1*occptodep',0,'0.1*deppwtodep',
                        0,0,0,0,0),
                        type="Full",nrow=5,ncol=5,byrow=T,name = "A",
                        dimnames = list(colnames(m1bcor), colnames(m1bcor)))

# Define Matrix S - free variances and covariances
m1bS<-create.mxMatrix(c(1,
                        '0.1*incWedu',1,
                        '0.1*incWoccp','0.1*eduWoccp',1,
                        0,0,0,'1*Errdep',
                        '0.1*incWdeppw','0.1*eduWdeppw','0.1*occpWdeppw',0,1), 
                        type="Symm",byrow=T,name="S", 
                        dimnames = list(colnames(m1bcor),colnames(m1bcor)))

# Fit Model 1B
m1bstge2<-wls(Cov=m1bcor,aCov=m1bcov,
              n=sum(aggregate(ntot~cormid,data=sesipd.long,max)$ntot),
              Amatrix=m1bA,Smatrix=m1bS) 
sink(file='m1bstge2.txt')
summary(m1bstge2)
sink()
saveRDS(m1bstge2,file='m1bstge2.RDS')

### MODEL 2A --------------------
## TSSEM: Stage 1 ----
m2astge1<-rma.mv(yi=esr,V=esr.v, 
                 data=filter(sesipd.long,!grepl('OCCP|OCCP2',cell)),
                 random=~1|DFID/esrid,
                 method="ML",
                 mods=~factor(cell)-1,
                 verbose=2,
                 sparse = T) 
summary(m2astge1)

saveRDS(m2astge1,file="m2astge1.RDS")

## TSSEM: Stage 2 ----
m2astge1<-readRDS('m2astge1.RDS')
# Extract asymptotic covariance matrix from model output
m2acov<-m2astge1$vb

# Define correlation matrix
m2acor<-corpcor::vec2sm(vec = c(1,m2astge1$b[21],m2astge1$b[20],m2astge1$b[19],m2astge1$b[23],m2astge1$b[22],m2astge1$b[17],m2astge1$b[18],
                                1,m2astge1$b[14],m2astge1$b[13],m2astge1$b[16],m2astge1$b[15],m2astge1$b[11],m2astge1$b[12],
                                1,m2astge1$b[8],m2astge1$b[10],m2astge1$b[9],m2astge1$b[6],m2astge1$b[7],
                                1,m2astge1$b[5],m2astge1$b[4],m2astge1$b[2],m2astge1$b[3],
                                1,m2astge1$b[28],m2astge1$b[26],m2astge1$b[27],
                                1,m2astge1$b[24],m2astge1$b[25],
                                1,m2astge1$b[1],
                                1),diag=T)

# Add column names to correlation matrix
colnames(m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
m2acov<-m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
               c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                        0,0,0,0,0,0,0,0),
                        type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                        dimnames = list(colnames(m2acor), colnames(m2acor)))

# Define Matrix S - free variances and covariances
m2aS<-create.mxMatrix(c(1,
                        '0.1*incWinc2',1,
                        '0.1*incWedu','0.1*inc2Wedu',1,
                        '0.1*incWedu2','0.1*inc2Wedu2','0.1*eduWedu2',1,
                        '0.1*incWoccs','0.1*inc2Woccs','0.1*eduWoccs','0.1*edu2Woccs',1,
                        '0.1*incWoccs2','0.1*inc2Woccs2','0.1*eduWoccs2','0.1*edu2Woccs2','0.1*occsWoccs2',1,
                        0,0,0,0,0,0,'1*Errdep',
                        '0.1*incWdeppw','0.1*inc2Wdeppw','0.1*eduWdeppw','0.1*edu2Wdeppw','0.1*occsWdeppw','0.1*occs2Wdeppw',0,1), 
                        type="Symm",byrow=T,name="S", 
                        dimnames = list(colnames(m2acor),colnames(m2acor)))

# Fit Model 2A
m2astge2<-wls(Cov=m2acor,aCov=m2acov,
              n=sum(aggregate(ntot~cormid,data=sesipd.long,max)$ntot),
              Amatrix=m2aA,Smatrix=m2aS) 
sink(file='m2astge2.txt')
summary(m2astge2)
sink()
saveRDS(m2astge2,file='m2astge2.RDS')

### MODEL 2B --------------------
## TSSEM: Stage 1 ----
m2bstge1<-rma.mv(yi=esr,V=esr.v, 
                 data=filter(sesipd.long,!grepl('OCCS|OCCS2',cell)),
                 random=~1|DFID/esrid,
                 method="ML",
                 mods=~factor(cell)-1,
                 verbose=2,
                 sparse = T) 
summary(m2bstge1)

saveRDS(m2bstge1,file="m2bstge1.RDS")

## TSSEM: Stage 2 ----
m2bstge1<-readRDS('m2bstge1.RDS')
# Extract asymptotic covariance matrix from model output
m2bcov<-m2bstge1$vb

# Define correlation matrix
m2bcor<-corpcor::vec2sm(vec = c(1,m2bstge1$b[21],m2bstge1$b[20],m2bstge1$b[19],m2bstge1$b[23],m2bstge1$b[22],m2bstge1$b[17],m2bstge1$b[18],
                                1,m2bstge1$b[14],m2bstge1$b[13],m2bstge1$b[16],m2bstge1$b[15],m2bstge1$b[11],m2bstge1$b[12],
                                1,m2bstge1$b[8],m2bstge1$b[10],m2bstge1$b[9],m2bstge1$b[6],m2bstge1$b[7],
                                1,m2bstge1$b[5],m2bstge1$b[4],m2bstge1$b[2],m2bstge1$b[3],
                                1,m2bstge1$b[28],m2bstge1$b[26],m2bstge1$b[27],
                                1,m2bstge1$b[24],m2bstge1$b[25],
                                1,m2bstge1$b[1],
                                1),diag=T)

# Add column names to correlation matrix
colnames(m2bcor)<-c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW")
rownames(m2bcor)<-c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
m2bcov<-m2bcov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
               c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
m2bA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        0,0,0,0,0,0,0,0,
                        '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occptodep','0.1*occp2todep',0,'0.1*deppwtodep',
                        0,0,0,0,0,0,0,0),
                        type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                        dimnames = list(colnames(m2bcor), colnames(m2bcor)))

# Define Matrix S - free variances and covariances
m2bS<-create.mxMatrix(c(1,
                        '0.1*incWinc2',1,
                        '0.1*incWedu','0.1*inc2Wedu',1,
                        '0.1*incWedu2','0.1*inc2Wedu2','0.1*eduWedu2',1,
                        '0.1*incWoccp','0.1*inc2Woccp','0.1*eduWoccp','0.1*edu2Woccp',1,
                        '0.1*incWoccp2','0.1*inc2Woccp2','0.1*eduWoccp2','0.1*edu2Woccp2','0.1*occsWoccp2',1,
                        0,0,0,0,0,0,'1*Errdep',
                        '0.1*incWdeppw','0.1*inc2Wdeppw','0.1*eduWdeppw','0.1*edu2Wdeppw','0.1*occpWdeppw','0.1*occp2Wdeppw',0,1), 
                        type="Symm",byrow=T,name="S", 
                        dimnames = list(colnames(m2bcor),colnames(m2bcor)))

# Fit Model 2B
m2bstge2<-wls(Cov=m2bcor,aCov=m2bcov,
              n=sum(aggregate(ntot~cormid,data=sesipd.long,max)$ntot),
              Amatrix=m2bA,Smatrix=m2bS) 
sink(file='m2bstge2.txt')
summary(m2bstge2)
sink()
saveRDS(m2bstge2,file='m2bstge2.RDS')

### MODEL 2A MODERATION ANALYIS: SEX --------------------
## Subset data by sex ----
sesipd.long.f<-sesipd.long%>%filter(MALE==0)
sesipd.long.m<-sesipd.long%>%filter(MALE==1)
## FEMALE PARTICIPANTS: Stage 1 ----
f.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                   data=filter(sesipd.long.f,!grepl('OCCP|OCCP2',cell)),
                   random=~1|DFID/esrid,
                   method="ML",
                   mods=~factor(cell)-1,
                   verbose=2,
                   sparse = T) 
summary(f.m2astge1)

saveRDS(f.m2astge1,file="f.m2astge1.RDS")

## FEMALE PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
f.m2acov<-f.m2astge1$vb

# Define correlation matrix
f.m2acor<-corpcor::vec2sm(vec = c(1,f.m2astge1$b[21],f.m2astge1$b[20],f.m2astge1$b[19],f.m2astge1$b[23],f.m2astge1$b[22],f.m2astge1$b[17],f.m2astge1$b[18],
                                  1,f.m2astge1$b[14],f.m2astge1$b[13],f.m2astge1$b[16],f.m2astge1$b[15],f.m2astge1$b[11],f.m2astge1$b[12],
                                  1,f.m2astge1$b[8],f.m2astge1$b[10],f.m2astge1$b[9],f.m2astge1$b[6],f.m2astge1$b[7],
                                  1,f.m2astge1$b[5],f.m2astge1$b[4],f.m2astge1$b[2],f.m2astge1$b[3],
                                  1,f.m2astge1$b[28],f.m2astge1$b[26],f.m2astge1$b[27],
                                  1,f.m2astge1$b[24],f.m2astge1$b[25],
                                  1,f.m2astge1$b[1],
                                  1),diag=T)

# Add column names to correlation matrix
colnames(f.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(f.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
f.m2acov<-f.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                   c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
f.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                          0,0,0,0,0,0,0,0),
                          type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                          dimnames = list(colnames(f.m2acor), colnames(f.m2acor)))

# Define Matrix S - free variances and covariances
f.m2aS<-create.mxMatrix(c(1,
                          '0.1*incWinc2_f',1,
                          '0.1*incWedu_f','0.1*inc2Wedu_f',1,
                          '0.1*incWedu2_f','0.1*inc2Wedu2_f','0.1*eduWedu2_f',1,
                          '0.1*incWoccs_f','0.1*inc2Woccs_f','0.1*eduWoccs_f','0.1*edu2Woccs_f',1,
                          '0.1*incWoccs2_f','0.1*inc2Woccs2_f','0.1*eduWoccs2_f','0.1*edu2Woccs2_f','0.1*occsWoccs2_f',1,
                          0,0,0,0,0,0,'1*Errdep_f',
                          '0.1*incWdeppw_f','0.1*inc2Wdeppw_f','0.1*eduWdeppw_f','0.1*edu2Wdeppw_f','0.1*occsWdeppw_f','0.1*occs2Wdeppw_f',0,1), 
                          type="Symm",byrow=T,name="S", 
                          dimnames = list(colnames(f.m2acor),colnames(f.m2acor)))

# Fit Model 2A
f.m2astge2<-wls(Cov=f.m2acor,aCov=f.m2acov,
                n=sum(aggregate(ntot~cormid,data=sesipd.long.f,max)$ntot),
                Amatrix=f.m2aA,Smatrix=f.m2aS) 
sink(file='f.m2astge2.txt')
summary(f.m2astge2)
sink()
saveRDS(f.m2astge2,file='f.m2astge2.RDS')

## MALE PARTICIPANTS: Stage 1 ----
m.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                   data=filter(sesipd.long.m,!grepl('OCCP|OCCP2',cell)),
                   random=~1|DFID/esrid,
                   method="ML",
                   mods=~factor(cell)-1,
                   verbose=2,
                   sparse = T) 
summary(m.m2astge1)

saveRDS(m.m2astge1,file="m.m2astge1.RDS")

## MALE PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
m.m2acov<-m.m2astge1$vb

# Define correlation matrix
m.m2acor<-corpcor::vec2sm(vec = c(1,m.m2astge1$b[21],m.m2astge1$b[20],m.m2astge1$b[19],m.m2astge1$b[23],m.m2astge1$b[22],m.m2astge1$b[17],m.m2astge1$b[18],
                                  1,m.m2astge1$b[14],m.m2astge1$b[13],m.m2astge1$b[16],m.m2astge1$b[15],m.m2astge1$b[11],m.m2astge1$b[12],
                                  1,m.m2astge1$b[8],m.m2astge1$b[10],m.m2astge1$b[9],m.m2astge1$b[6],m.m2astge1$b[7],
                                  1,m.m2astge1$b[5],m.m2astge1$b[4],m.m2astge1$b[2],m.m2astge1$b[3],
                                  1,m.m2astge1$b[28],m.m2astge1$b[26],m.m2astge1$b[27],
                                  1,m.m2astge1$b[24],m.m2astge1$b[25],
                                  1,m.m2astge1$b[1],
                                  1),diag=T)

# Add column names to correlation matrix
colnames(m.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(m.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
m.m2acov<-m.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                   c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
m.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          0,0,0,0,0,0,0,0,
                          '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                          0,0,0,0,0,0,0,0),
                          type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                          dimnames = list(colnames(m.m2acor), colnames(m.m2acor)))

# Define Matrix S - free variances and covariances
m.m2aS<-create.mxMatrix(c(1,
                          '0.1*incWinc2_m',1,
                          '0.1*incWedu_m','0.1*inc2Wedu_m',1,
                          '0.1*incWedu2_m','0.1*inc2Wedu2_m','0.1*eduWedu2_m',1,
                          '0.1*incWoccs_m','0.1*inc2Woccs_m','0.1*eduWoccs_m','0.1*edu2Woccs_m',1,
                          '0.1*incWoccs2_m','0.1*inc2Woccs2_m','0.1*eduWoccs2_m','0.1*edu2Woccs2_m','0.1*occsWoccs2_m',1,
                          0,0,0,0,0,0,'1*Errdep_m',
                          '0.1*incWdeppw_m','0.1*inc2Wdeppw_m','0.1*eduWdeppw_m','0.1*edu2Wdeppw_m','0.1*occsWdeppw_m','0.1*occs2Wdeppw_m',0,1), 
                          type="Symm",byrow=T,name="S", 
                          dimnames = list(colnames(m.m2acor),colnames(m.m2acor)))

# Fit Model 2A
m.m2astge2<-wls(Cov=m.m2acor,aCov=m.m2acov,
                n=sum(aggregate(ntot~cormid,data=sesipd.long.m,max)$ntot),
                Amatrix=m.m2aA,Smatrix=m.m2aS) 
sink(file='m.m2astge2.txt')
summary(m.m2astge2)
sink()
saveRDS(m.m2astge2,file='m.m2astge2.RDS')

## EQUALITY CONSTRAINTS TEST: REGRESSION COEFFICIENTS ----
# Define path models by sex
f.m2astge2.eq<-wls(Cov=f.m2acor,aCov=f.m2acov,Amatrix=m2aA,Smatrix=f.m2aS,run=F,model.name="female")
m.m2astge2.eq<-wls(Cov=m.m2acor,aCov=m.m2acov,Amatrix=m2aA,Smatrix=m.m2aS,run=F,model.name="male")

# Define multi-group path model
m2astge2.mf<-mxModel(f.m2astge2.eq,m.m2astge2.eq,model="Equal_Coefficients", 
                     mxFitFunctionMultigroup(c("female","male")))

# Multi-group moderation test
m2astge2.mfeq<-mxRun(m2astge2.mf,intervals=T)
summary(m2astge2.mfeq)

sink(file='m2astge2.mfeq.txt')
summary(m2astge2.mfeq)
sink()
saveRDS(m2astge2.mfeq,file='m2astge2.mfeq.RDS')

# Chi-square test
m2astge2.mfeq.chisq<-data.frame(chisq=m2astge2.mfeq$output$fit,
                                chisq.p=pchisq(m2astge2.mfeq$output$fit,
                                length(m2astge2.mfeq$output$estimate[which(!grepl('_f|_m',names(m2astge2.mfeq$output$estimate)))]),
                                lower.tail=F),
                                df=length(m2astge2.mfeq$output$estimate[which(!grepl('_f|_m',names(m2astge2.mfeq$output$estimate)))]))
sink(file='m2astge2.mfeq.chisq.txt')
m2astge2.mfeq.chisq
sink()

### MODEL 2A MODERATION ANALYIS: DEVELOPMENTAL PERIOD --------------------
## Subset data by developmental period ----
sesipd.long.adol<-sesipd.long%>%filter(DEVP==1)
sesipd.long.yadu<-sesipd.long%>%filter(DEVP==2)
sesipd.long.adu<-sesipd.long%>%filter(DEVP==3)
sesipd.long.oadu<-sesipd.long%>%filter(DEVP==4)

## ADOLESCENT PARTICIPANTS: Stage 1 ----
adol.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                      data=filter(sesipd.long.adol,!grepl('OCCP|OCCP2',cell)),
                      random=~1|DFID/esrid,
                      method="ML",
                      mods=~factor(cell)-1,
                      verbose=2,
                      sparse = T) 
summary(adol.m2astge1)

saveRDS(adol.m2astge1,file="adol.m2astge1.RDS")

## ADOLESCENT PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
adol.m2acov<-adol.m2astge1$vb

# Define correlation matrix
adol.m2acor<-corpcor::vec2sm(vec = c(1,adol.m2astge1$b[21],adol.m2astge1$b[20],adol.m2astge1$b[19],adol.m2astge1$b[23],adol.m2astge1$b[22],adol.m2astge1$b[17],adol.m2astge1$b[18],
                                     1,adol.m2astge1$b[14],adol.m2astge1$b[13],adol.m2astge1$b[16],adol.m2astge1$b[15],adol.m2astge1$b[11],adol.m2astge1$b[12],
                                     1,adol.m2astge1$b[8],adol.m2astge1$b[10],adol.m2astge1$b[9],adol.m2astge1$b[6],adol.m2astge1$b[7],
                                     1,adol.m2astge1$b[5],adol.m2astge1$b[4],adol.m2astge1$b[2],adol.m2astge1$b[3],
                                     1,adol.m2astge1$b[28],adol.m2astge1$b[26],adol.m2astge1$b[27],
                                     1,adol.m2astge1$b[24],adol.m2astge1$b[25],
                                     1,adol.m2astge1$b[1],
                                     1),diag=T)

# Add column names to correlation matrix
colnames(adol.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(adol.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
adol.m2acov<-adol.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                         c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
adol.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                             0,0,0,0,0,0,0,0),
                             type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                             dimnames = list(colnames(adol.m2acor), colnames(adol.m2acor)))

# Define Matrix S - free variances and covariances
adol.m2aS<-create.mxMatrix(c(1,
                            '0.1*incWinc2_adol',1,
                            '0.1*incWedu_adol','0.1*inc2Wedu_adol',1,
                            '0.1*incWedu2_adol','0.1*inc2Wedu2_adol','0.1*eduWedu2_adol',1,
                            '0.1*incWoccs_adol','0.1*inc2Woccs_adol','0.1*eduWoccs_adol','0.1*edu2Woccs_adol',1,
                            '0.1*incWoccs2_adol','0.1*inc2Woccs2_adol','0.1*eduWoccs2_adol','0.1*edu2Woccs2_adol','0.1*occsWoccs2_adol',1,
                            0,0,0,0,0,0,'1*Errdep_adol',
                            '0.1*incWdeppw_adol','0.1*inc2Wdeppw_adol','0.1*eduWdeppw_adol','0.1*edu2Wdeppw_adol','0.1*occsWdeppw_adol','0.1*occs2Wdeppw_adol',0,1), 
                            type="Symm",byrow=T,name="S", 
                            dimnames = list(colnames(adol.m2acor),colnames(adol.m2acor)))

# Fit Model 2A
adol.m2astge2<-wls(Cov=adol.m2acor,aCov=adol.m2acov,
                   n=sum(aggregate(ntot~cormid,data=sesipd.long.adol,max)$ntot),
                   Amatrix=adol.m2aA,Smatrix=adol.m2aS) 
sink(file='adol.m2astge2.txt')
summary(adol.m2astge2)
sink()
saveRDS(adol.m2astge2,file='adol.m2astge2.RDS')

## YOUNG ADULT PARTICIPANTS: Stage 1 ----
yadu.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                      data=filter(sesipd.long.yadu,!grepl('OCCP|OCCP2',cell)),
                      random=~1|DFID/esrid,
                      method="ML",
                      mods=~factor(cell)-1,
                      verbose=2,
                      sparse = T) 
summary(yadu.m2astge1)

saveRDS(yadu.m2astge1,file="yadu.m2astge1.RDS")

## YOUNG ADULT PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
yadu.m2acov<-yadu.m2astge1$vb

# Define correlation matrix
yadu.m2acor<-corpcor::vec2sm(vec = c(1,yadu.m2astge1$b[21],yadu.m2astge1$b[20],yadu.m2astge1$b[19],yadu.m2astge1$b[23],yadu.m2astge1$b[22],yadu.m2astge1$b[17],yadu.m2astge1$b[18],
                                     1,yadu.m2astge1$b[14],yadu.m2astge1$b[13],yadu.m2astge1$b[16],yadu.m2astge1$b[15],yadu.m2astge1$b[11],yadu.m2astge1$b[12],
                                     1,yadu.m2astge1$b[8],yadu.m2astge1$b[10],yadu.m2astge1$b[9],yadu.m2astge1$b[6],yadu.m2astge1$b[7],
                                     1,yadu.m2astge1$b[5],yadu.m2astge1$b[4],yadu.m2astge1$b[2],yadu.m2astge1$b[3],
                                     1,yadu.m2astge1$b[28],yadu.m2astge1$b[26],yadu.m2astge1$b[27],
                                     1,yadu.m2astge1$b[24],yadu.m2astge1$b[25],
                                     1,yadu.m2astge1$b[1],
                                     1),diag=T)

# Add column names to correlation matrix
colnames(yadu.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(yadu.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
yadu.m2acov<-yadu.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                         c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
yadu.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                             0,0,0,0,0,0,0,0),
                             type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                             dimnames = list(colnames(yadu.m2acor), colnames(yadu.m2acor)))

# Define Matrix S - free variances and covariances
yadu.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_yadu',1,
                           '0.1*incWedu_yadu','0.1*inc2Wedu_yadu',1,
                           '0.1*incWedu2_yadu','0.1*inc2Wedu2_yadu','0.1*eduWedu2_yadu',1,
                           '0.1*incWoccs_yadu','0.1*inc2Woccs_yadu','0.1*eduWoccs_yadu','0.1*edu2Woccs_yadu',1,
                           '0.1*incWoccs2_yadu','0.1*inc2Woccs2_yadu','0.1*eduWoccs2_yadu','0.1*edu2Woccs2_yadu','0.1*occsWoccs2_yadu',1,
                           0,0,0,0,0,0,'1*Errdep_yadu',
                           '0.1*incWdeppw_yadu','0.1*inc2Wdeppw_yadu','0.1*eduWdeppw_yadu','0.1*edu2Wdeppw_yadu','0.1*occsWdeppw_yadu','0.1*occs2Wdeppw_yadu',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(yadu.m2acor),colnames(yadu.m2acor)))

# Fit Model 2A
yadu.m2astge2<-wls(Cov=yadu.m2acor,aCov=yadu.m2acov,
                n=sum(aggregate(ntot~cormid,data=sesipd.long.yadu,max)$ntot),
                Amatrix=yadu.m2aA,Smatrix=yadu.m2aS) 
sink(file='yadu.m2astge2.txt')
summary(yadu.m2astge2)
sink()
saveRDS(yadu.m2astge2,file='yadu.m2astge2.RDS')

## MIDDLE-AGED ADULT PARTICIPANTS: Stage 1 ----
adu.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                      data=filter(sesipd.long.adu,!grepl('OCCP|OCCP2',cell)),
                      random=~1|DFID/esrid,
                      method="ML",
                      mods=~factor(cell)-1,
                      verbose=2,
                      sparse = T) 
summary(adu.m2astge1)

saveRDS(adu.m2astge1,file="adu.m2astge1.RDS")

## MIDDLE-AGED ADULT PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
adu.m2acov<-adu.m2astge1$vb

# Define correlation matrix
adu.m2acor<-corpcor::vec2sm(vec = c(1,adu.m2astge1$b[21],adu.m2astge1$b[20],adu.m2astge1$b[19],adu.m2astge1$b[23],adu.m2astge1$b[22],adu.m2astge1$b[17],adu.m2astge1$b[18],
                                     1,adu.m2astge1$b[14],adu.m2astge1$b[13],adu.m2astge1$b[16],adu.m2astge1$b[15],adu.m2astge1$b[11],adu.m2astge1$b[12],
                                     1,adu.m2astge1$b[8],adu.m2astge1$b[10],adu.m2astge1$b[9],adu.m2astge1$b[6],adu.m2astge1$b[7],
                                     1,adu.m2astge1$b[5],adu.m2astge1$b[4],adu.m2astge1$b[2],adu.m2astge1$b[3],
                                     1,adu.m2astge1$b[28],adu.m2astge1$b[26],adu.m2astge1$b[27],
                                     1,adu.m2astge1$b[24],adu.m2astge1$b[25],
                                     1,adu.m2astge1$b[1],
                                     1),diag=T)

# Add column names to correlation matrix
colnames(adu.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(adu.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
adu.m2acov<-adu.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                         c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
adu.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                             0,0,0,0,0,0,0,0),
                             type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                             dimnames = list(colnames(adu.m2acor), colnames(adu.m2acor)))

# Define Matrix S - free variances and covariances
adu.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_adu',1,
                           '0.1*incWedu_adu','0.1*inc2Wedu_adu',1,
                           '0.1*incWedu2_adu','0.1*inc2Wedu2_adu','0.1*eduWedu2_adu',1,
                           '0.1*incWoccs_adu','0.1*inc2Woccs_adu','0.1*eduWoccs_adu','0.1*edu2Woccs_adu',1,
                           '0.1*incWoccs2_adu','0.1*inc2Woccs2_adu','0.1*eduWoccs2_adu','0.1*edu2Woccs2_adu','0.1*occsWoccs2_adu',1,
                           0,0,0,0,0,0,'1*Errdep_adu',
                           '0.1*incWdeppw_adu','0.1*inc2Wdeppw_adu','0.1*eduWdeppw_adu','0.1*edu2Wdeppw_adu','0.1*occsWdeppw_adu','0.1*occs2Wdeppw_adu',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(adu.m2acor),colnames(adu.m2acor)))

# Fit Model 2A
adu.m2astge2<-wls(Cov=adu.m2acor,aCov=adu.m2acov,
                n=sum(aggregate(ntot~cormid,data=sesipd.long.adu,max)$ntot),
                Amatrix=adu.m2aA,Smatrix=adu.m2aS) 
sink(file='adu.m2astge2.txt')
summary(adu.m2astge2)
sink()
saveRDS(adu.m2astge2,file='adu.m2astge2.RDS')

## OLDER ADULT PARTICIPANTS: Stage 1 ----
oadu.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                      data=filter(sesipd.long.oadu,!grepl('OCCP|OCCP2',cell)),
                      random=~1|DFID/esrid,
                      method="ML",
                      mods=~factor(cell)-1,
                      verbose=2,
                      sparse = T) 
summary(oadu.m2astge1)

saveRDS(oadu.m2astge1,file="oadu.m2astge1.RDS")

## OLDER ADULT PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
oadu.m2acov<-oadu.m2astge1$vb

# Define correlation matrix
oadu.m2acor<-corpcor::vec2sm(vec = c(1,oadu.m2astge1$b[21],oadu.m2astge1$b[20],oadu.m2astge1$b[19],oadu.m2astge1$b[23],oadu.m2astge1$b[22],oadu.m2astge1$b[17],oadu.m2astge1$b[18],
                                     1,oadu.m2astge1$b[14],oadu.m2astge1$b[13],oadu.m2astge1$b[16],oadu.m2astge1$b[15],oadu.m2astge1$b[11],oadu.m2astge1$b[12],
                                     1,oadu.m2astge1$b[8],oadu.m2astge1$b[10],oadu.m2astge1$b[9],oadu.m2astge1$b[6],oadu.m2astge1$b[7],
                                     1,oadu.m2astge1$b[5],oadu.m2astge1$b[4],oadu.m2astge1$b[2],oadu.m2astge1$b[3],
                                     1,oadu.m2astge1$b[28],oadu.m2astge1$b[26],oadu.m2astge1$b[27],
                                     1,oadu.m2astge1$b[24],oadu.m2astge1$b[25],
                                     1,oadu.m2astge1$b[1],
                                     1),diag=T)

# Add column names to correlation matrix
colnames(oadu.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(oadu.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
oadu.m2acov<-oadu.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                         c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
oadu.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                             0,0,0,0,0,0,0,0),
                             type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                             dimnames = list(colnames(oadu.m2acor), colnames(oadu.m2acor)))

# Define Matrix S - free variances and covariances
oadu.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_oadu',1,
                           '0.1*incWedu_oadu','0.1*inc2Wedu_oadu',1,
                           '0.1*incWedu2_oadu','0.1*inc2Wedu2_oadu','0.1*eduWedu2_oadu',1,
                           '0.1*incWoccs_oadu','0.1*inc2Woccs_oadu','0.1*eduWoccs_oadu','0.1*edu2Woccs_oadu',1,
                           '0.1*incWoccs2_oadu','0.1*inc2Woccs2_oadu','0.1*eduWoccs2_oadu','0.1*edu2Woccs2_oadu','0.1*occsWoccs2_oadu',1,
                           0,0,0,0,0,0,'1*Errdep_oadu',
                           '0.1*incWdeppw_oadu','0.1*inc2Wdeppw_oadu','0.1*eduWdeppw_oadu','0.1*edu2Wdeppw_oadu','0.1*occsWdeppw_oadu','0.1*occs2Wdeppw_oadu',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(oadu.m2acor),colnames(oadu.m2acor)))

# Fit Model 2A
oadu.m2astge2<-wls(Cov=oadu.m2acor,aCov=oadu.m2acov,
                n=sum(aggregate(ntot~cormid,data=sesipd.long.oadu,max)$ntot),
                Amatrix=oadu.m2aA,Smatrix=oadu.m2aS) 
sink(file='oadu.m2astge2.txt')
summary(oadu.m2astge2)
sink()
saveRDS(oadu.m2astge2,file='oadu.m2astge2.RDS')

## EQUALITY CONSTRAINTS TEST: REGRESSION COEFFICIENTS ----
# Define path models by sex
adol.m2astge2.eq<-wls(Cov=adol.m2acor,aCov=adol.m2acov,Amatrix=m2aA,Smatrix=adol.m2aS,run=F,model.name="adolescent")
yadu.m2astge2.eq<-wls(Cov=yadu.m2acor,aCov=yadu.m2acov,Amatrix=m2aA,Smatrix=yadu.m2aS,run=F,model.name="youngadult")
adu.m2astge2.eq<-wls(Cov=adu.m2acor,aCov=adu.m2acov,Amatrix=m2aA,Smatrix=adu.m2aS,run=F,model.name="adult")
oadu.m2astge2.eq<-wls(Cov=oadu.m2acor,aCov=oadu.m2acov,Amatrix=m2aA,Smatrix=oadu.m2aS,run=F,model.name="olderadult")

## Across all subgroups
# Define multi-group path model
m2astge2.devp<-mxModel(adu.m2astge2.eq,adol.m2astge2.eq,yadu.m2astge2.eq,oadu.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("adult","adolescent","youngadult","olderadult")))

# Multi-group moderation test
m2astge2.devpeq<-mxRun(m2astge2.devp,intervals=T)
summary(m2astge2.devpeq)

sink(file='m2astge2.devpeq.txt')
summary(m2astge2.devpeq)
sink()
saveRDS(m2astge2.devpeq,file='m2astge2.devpeq.RDS')

# Chi-square test
m2astge2.devpeq.chisq<-data.frame(chisq=m2astge2.devpeq$output$fit,
                                  chisq.p=pchisq(m2astge2.devpeq$output$fit,
                                  length(m2astge2.devpeq$output$estimate[which(!grepl('_adol|_yadu|_adu|_oadu',names(m2astge2.devpeq$output$estimate)))])*(length(m2astge2.devpeq$output$matrices[which(grepl('Amatrix',names(m2astge2.devpeq$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.devpeq$output$estimate[which(!grepl('_adol|_yadu|_adu|_oadu',names(m2astge2.devpeq$output$estimate)))])*(length(m2astge2.devpeq$output$matrices[which(grepl('Amatrix',names(m2astge2.devpeq$output$matrices)))])-1))
sink(file='m2astge2.devpeq.chisq.txt')
m2astge2.devpeq.chisq
sink()

## Between subgroups
# Middle-aged adults compared to adolescents
m2astge2.aduadol<-mxModel(adu.m2astge2.eq,adol.m2astge2.eq,
                          model="Equal_Coefficients", 
                          mxFitFunctionMultigroup(c("adult","adolescent")))

# Multi-group moderation test
m2astge2.aduadol<-mxRun(m2astge2.aduadol,intervals=T)
summary(m2astge2.aduadol)

sink(file='m2astge2.aduadol.txt')
summary(m2astge2.aduadol)
sink()
saveRDS(m2astge2.aduadol,file='m2astge2.aduadol.RDS')

# Chi-square test
m2astge2.aduadol.chisq<-data.frame(chisq=m2astge2.aduadol$output$fit,
                                  chisq.p=pchisq(m2astge2.aduadol$output$fit,
                                  length(m2astge2.aduadol$output$estimate[which(!grepl('_adol|_adu',names(m2astge2.aduadol$output$estimate)))])*(length(m2astge2.aduadol$output$matrices[which(grepl('Amatrix',names(m2astge2.aduadol$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.aduadol$output$estimate[which(!grepl('_adol|_adu',names(m2astge2.aduadol$output$estimate)))])*(length(m2astge2.aduadol$output$matrices[which(grepl('Amatrix',names(m2astge2.aduadol$output$matrices)))])-1))
sink(file='m2astge2.aduadol.chisq.txt')
m2astge2.aduadol.chisq
sink()

# Middle-aged adults compared to young adults
m2astge2.aduyadu<-mxModel(adu.m2astge2.eq,yadu.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("adult","youngadult")))

# Multi-group moderation test
m2astge2.aduyadu<-mxRun(m2astge2.aduyadu,intervals=T)
summary(m2astge2.aduyadu)

sink(file='m2astge2.aduyadu.txt')
summary(m2astge2.aduyadu)
sink()
saveRDS(m2astge2.aduyadu,file='m2astge2.aduyadu.RDS')

# Chi-square test
m2astge2.aduyadu.chisq<-data.frame(chisq=m2astge2.aduyadu$output$fit,
                                  chisq.p=pchisq(m2astge2.aduyadu$output$fit,
                                  length(m2astge2.aduyadu$output$estimate[which(!grepl('_yadu|_adu',names(m2astge2.aduyadu$output$estimate)))])*(length(m2astge2.aduyadu$output$matrices[which(grepl('Amatrix',names(m2astge2.aduyadu$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.aduyadu$output$estimate[which(!grepl('_yadu|_adu',names(m2astge2.aduyadu$output$estimate)))])*(length(m2astge2.aduyadu$output$matrices[which(grepl('Amatrix',names(m2astge2.aduyadu$output$matrices)))])-1))
sink(file='m2astge2.aduyadu.chisq.txt')
m2astge2.aduyadu.chisq
sink()

# Middle-aged adults compared to older adults
m2astge2.aduoadu<-mxModel(adu.m2astge2.eq,oadu.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("adult","olderadult")))

# Multi-group moderation test
m2astge2.aduoadu<-mxRun(m2astge2.aduoadu,intervals=T)
summary(m2astge2.aduoadu)

sink(file='m2astge2.aduoadu.txt')
summary(m2astge2.aduoadu)
sink()
saveRDS(m2astge2.aduoadu,file='m2astge2.aduoadu.RDS')

# Chi-square test
m2astge2.aduoadu.chisq<-data.frame(chisq=m2astge2.aduoadu$output$fit,
                                  chisq.p=pchisq(m2astge2.aduoadu$output$fit,
                                  length(m2astge2.aduoadu$output$estimate[which(!grepl('_adu|_oadu',names(m2astge2.aduoadu$output$estimate)))])*(length(m2astge2.aduoadu$output$matrices[which(grepl('Amatrix',names(m2astge2.aduoadu$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.aduoadu$output$estimate[which(!grepl('_adu|_oadu',names(m2astge2.aduoadu$output$estimate)))])*(length(m2astge2.aduoadu$output$matrices[which(grepl('Amatrix',names(m2astge2.aduoadu$output$matrices)))])-1))
sink(file='m2astge2.aduoadu.chisq.txt')
m2astge2.aduoadu.chisq
sink()

# Adolescents compared to young adults
m2astge2.adolyadu<-mxModel(adol.m2astge2.eq,yadu.m2astge2.eq,
                           model="Equal_Coefficients", 
                           mxFitFunctionMultigroup(c("adolescent","youngadult")))

# Multi-group moderation test
m2astge2.adolyadu<-mxRun(m2astge2.adolyadu,intervals=T)
summary(m2astge2.adolyadu)

sink(file='m2astge2.adolyadu.txt')
summary(m2astge2.adolyadu)
sink()
saveRDS(m2astge2.adolyadu,file='m2astge2.adolyadu.RDS')

# Chi-square test
m2astge2.adolyadu.chisq<-data.frame(chisq=m2astge2.adolyadu$output$fit,
                                  chisq.p=pchisq(m2astge2.adolyadu$output$fit,
                                  length(m2astge2.adolyadu$output$estimate[which(!grepl('_adol|_yadu',names(m2astge2.adolyadu$output$estimate)))])*(length(m2astge2.adolyadu$output$matrices[which(grepl('Amatrix',names(m2astge2.adolyadu$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.adolyadu$output$estimate[which(!grepl('_adol|_yadu',names(m2astge2.adolyadu$output$estimate)))])*(length(m2astge2.adolyadu$output$matrices[which(grepl('Amatrix',names(m2astge2.adolyadu$output$matrices)))])-1))
sink(file='m2astge2.adolyadu.chisq.txt')
m2astge2.adolyadu.chisq
sink()

# Adolescents compared to older adults
m2astge2.adoloadu<-mxModel(adol.m2astge2.eq,oadu.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("adolescent","olderadult")))

# Multi-group moderation test
m2astge2.adoloadu<-mxRun(m2astge2.adoloadu,intervals=T)
summary(m2astge2.adoloadu)

sink(file='m2astge2.adoloadu.txt')
summary(m2astge2.adoloadu)
sink()
saveRDS(m2astge2.adoloadu,file='m2astge2.adoloadu.RDS')

# Chi-square test
m2astge2.adoloadu.chisq<-data.frame(chisq=m2astge2.adoloadu$output$fit,
                                  chisq.p=pchisq(m2astge2.adoloadu$output$fit,
                                  length(m2astge2.adoloadu$output$estimate[which(!grepl('_adol|_oadu',names(m2astge2.adoloadu$output$estimate)))])*(length(m2astge2.adoloadu$output$matrices[which(grepl('Amatrix',names(m2astge2.adoloadu$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.adoloadu$output$estimate[which(!grepl('_adol|_oadu',names(m2astge2.adoloadu$output$estimate)))])*(length(m2astge2.adoloadu$output$matrices[which(grepl('Amatrix',names(m2astge2.adoloadu$output$matrices)))])-1))
sink(file='m2astge2.adoloadu.chisq.txt')
m2astge2.adoloadu.chisq
sink()

# Young adults compared to older adults
m2astge2.yaduoadu<-mxModel(yadu.m2astge2.eq,oadu.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("youngadult","olderadult")))

# Multi-group moderation test
m2astge2.yaduoadu<-mxRun(m2astge2.yaduoadu,intervals=T)
summary(m2astge2.yaduoadu)

sink(file='m2astge2.yaduoadu.txt')
summary(m2astge2.yaduoadu)
sink()
saveRDS(m2astge2.yaduoadu,file='m2astge2.yaduoadu.RDS')

# Chi-square test
m2astge2.yaduoadu.chisq<-data.frame(chisq=m2astge2.yaduoadu$output$fit,
                                  chisq.p=pchisq(m2astge2.yaduoadu$output$fit,
                                  length(m2astge2.yaduoadu$output$estimate[which(!grepl('_yadu|_oadu',names(m2astge2.yaduoadu$output$estimate)))])*(length(m2astge2.yaduoadu$output$matrices[which(grepl('Amatrix',names(m2astge2.yaduoadu$output$matrices)))])-1),
                                  lower.tail=F),
                                  df=length(m2astge2.yaduoadu$output$estimate[which(!grepl('_yadu|_oadu',names(m2astge2.yaduoadu$output$estimate)))])*(length(m2astge2.yaduoadu$output$matrices[which(grepl('Amatrix',names(m2astge2.yaduoadu$output$matrices)))])-1))
sink(file='m2astge2.yaduoadu.chisq.txt')
m2astge2.yaduoadu.chisq
sink()

### MODEL 2A MODERATION ANALYIS: RACE/ETHNICITY --------------------
## Subset data by race/ethnicity ----
sesipd.long.aa<-sesipd.long%>%filter(RACETH==3)
sesipd.long.blk<-sesipd.long%>%filter(RACETH==2)
sesipd.long.lat<-sesipd.long%>%filter(RACETH==1)
sesipd.long.ai<-sesipd.long%>%filter(RACETH==4)
sesipd.long.mr<-sesipd.long%>%filter(RACETH==5)
sesipd.long.wht<-sesipd.long%>%filter(RACETH==0)

## ASIAN AMERICAN PARTICIPANTS: Stage 1 ----
aa.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                    data=filter(sesipd.long.aa,!grepl('OCCP|OCCP2',cell)),
                    random=~1|DFID/esrid,
                    method="ML",
                    mods=~factor(cell)-1,
                    verbose=2,
                    sparse = T) 
summary(aa.m2astge1)

saveRDS(aa.m2astge1,file="aa.m2astge1.RDS")

## ASIAN AMERICAN PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
aa.m2acov<-aa.m2astge1$vb

# Define correlation matrix
aa.m2acor<-corpcor::vec2sm(vec = c(1,aa.m2astge1$b[21],aa.m2astge1$b[20],aa.m2astge1$b[19],aa.m2astge1$b[23],aa.m2astge1$b[22],aa.m2astge1$b[17],aa.m2astge1$b[18],
                                   1,aa.m2astge1$b[14],aa.m2astge1$b[13],aa.m2astge1$b[16],aa.m2astge1$b[15],aa.m2astge1$b[11],aa.m2astge1$b[12],
                                   1,aa.m2astge1$b[8],aa.m2astge1$b[10],aa.m2astge1$b[9],aa.m2astge1$b[6],aa.m2astge1$b[7],
                                   1,aa.m2astge1$b[5],aa.m2astge1$b[4],aa.m2astge1$b[2],aa.m2astge1$b[3],
                                   1,aa.m2astge1$b[28],aa.m2astge1$b[26],aa.m2astge1$b[27],
                                   1,aa.m2astge1$b[24],aa.m2astge1$b[25],
                                   1,aa.m2astge1$b[1],
                                   1),diag=T)

# Add column names to correlation matrix
colnames(aa.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(aa.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
aa.m2acov<-aa.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                     c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
aa.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                           0,0,0,0,0,0,0,0),
                           type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                           dimnames = list(colnames(aa.m2acor), colnames(aa.m2acor)))

# Define Matrix S - free variances and covariances
aa.m2aS<-create.mxMatrix(c(1,
                         '0.1*incWinc2_aa',1,
                         '0.1*incWedu_aa','0.1*inc2Wedu_aa',1,
                         '0.1*incWedu2_aa','0.1*inc2Wedu2_aa','0.1*eduWedu2_aa',1,
                         '0.1*incWoccs_aa','0.1*inc2Woccs_aa','0.1*eduWoccs_aa','0.1*edu2Woccs_aa',1,
                         '0.1*incWoccs2_aa','0.1*inc2Woccs2_aa','0.1*eduWoccs2_aa','0.1*edu2Woccs2_aa','0.1*occsWoccs2_aa',1,
                         0,0,0,0,0,0,'1*Errdep_aa',
                         '0.1*incWdeppw_aa','0.1*inc2Wdeppw_aa','0.1*eduWdeppw_aa','0.1*edu2Wdeppw_aa','0.1*occsWdeppw_aa','0.1*occs2Wdeppw_aa',0,1), 
                         type="Symm",byrow=T,name="S", 
                         dimnames = list(colnames(aa.m2acor),colnames(aa.m2acor)))

# Fit Model 2A
aa.m2astge2<-wls(Cov=aa.m2acor,aCov=aa.m2acov,
                 n=sum(aggregate(ntot~cormid,data=sesipd.long.aa,max)$ntot),
                 Amatrix=aa.m2aA,Smatrix=aa.m2aS) 
sink(file='aa.m2astge2.txt')
summary(aa.m2astge2)
sink()
saveRDS(aa.m2astge2,file='aa.m2astge2.RDS')

## BLACK PARTICIPANTS: Stage 1 ----
blk.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                     data=filter(sesipd.long.blk,!grepl('OCCP|OCCP2',cell)),
                     random=~1|DFID/esrid,
                     method="ML",
                     mods=~factor(cell)-1,
                     verbose=2,
                     sparse = T) 
summary(blk.m2astge1)

saveRDS(blk.m2astge1,file="blk.m2astge1.RDS")

## BLACK PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
blk.m2acov<-blk.m2astge1$vb

# Define correlation matrix
blk.m2acor<-corpcor::vec2sm(vec = c(1,blk.m2astge1$b[21],blk.m2astge1$b[20],blk.m2astge1$b[19],blk.m2astge1$b[23],blk.m2astge1$b[22],blk.m2astge1$b[17],blk.m2astge1$b[18],
                                    1,blk.m2astge1$b[14],blk.m2astge1$b[13],blk.m2astge1$b[16],blk.m2astge1$b[15],blk.m2astge1$b[11],blk.m2astge1$b[12],
                                    1,blk.m2astge1$b[8],blk.m2astge1$b[10],blk.m2astge1$b[9],blk.m2astge1$b[6],blk.m2astge1$b[7],
                                    1,blk.m2astge1$b[5],blk.m2astge1$b[4],blk.m2astge1$b[2],blk.m2astge1$b[3],
                                    1,blk.m2astge1$b[28],blk.m2astge1$b[26],blk.m2astge1$b[27],
                                    1,blk.m2astge1$b[24],blk.m2astge1$b[25],
                                    1,blk.m2astge1$b[1],
                                    1),diag=T)

# Add column names to correlation matrix
colnames(blk.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(blk.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
blk.m2acov<-blk.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                       c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
blk.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                            0,0,0,0,0,0,0,0),
                            type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                            dimnames = list(colnames(blk.m2acor), colnames(blk.m2acor)))

# Define Matrix S - free variances and covariances
blk.m2aS<-create.mxMatrix(c(1,
                          '0.1*incWinc2_blk',1,
                          '0.1*incWedu_blk','0.1*inc2Wedu_blk',1,
                          '0.1*incWedu2_blk','0.1*inc2Wedu2_blk','0.1*eduWedu2_blk',1,
                          '0.1*incWoccs_blk','0.1*inc2Woccs_blk','0.1*eduWoccs_blk','0.1*edu2Woccs_blk',1,
                          '0.1*incWoccs2_blk','0.1*inc2Woccs2_blk','0.1*eduWoccs2_blk','0.1*edu2Woccs2_blk','0.1*occsWoccs2_blk',1,
                          0,0,0,0,0,0,'1*Errdep_blk',
                          '0.1*incWdeppw_blk','0.1*inc2Wdeppw_blk','0.1*eduWdeppw_blk','0.1*edu2Wdeppw_blk','0.1*occsWdeppw_blk','0.1*occs2Wdeppw_blk',0,1), 
                          type="Symm",byrow=T,name="S", 
                          dimnames = list(colnames(blk.m2acor),colnames(blk.m2acor)))

# Fit Model 2A
blk.m2astge2<-wls(Cov=blk.m2acor,aCov=blk.m2acov,
                  n=sum(aggregate(ntot~cormid,data=sesipd.long.blk,max)$ntot),
                  Amatrix=blk.m2aA,Smatrix=blk.m2aS) 
sink(file='blk.m2astge2.txt')
summary(blk.m2astge2)
sink()
saveRDS(blk.m2astge2,file='blk.m2astge2.RDS')

## LATINX PARTICIPANTS: Stage 1 ----
lat.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                     data=filter(sesipd.long.lat,!grepl('OCCP|OCCP2',cell)),
                     random=~1|DFID/esrid,
                     method="ML",
                     mods=~factor(cell)-1,
                     verbose=2,
                     sparse = T) 
summary(lat.m2astge1)

saveRDS(lat.m2astge1,file="lat.m2astge1.RDS")

## LATINX PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
lat.m2acov<-lat.m2astge1$vb

# Define correlation matrix
lat.m2acor<-corpcor::vec2sm(vec = c(1,lat.m2astge1$b[21],lat.m2astge1$b[20],lat.m2astge1$b[19],lat.m2astge1$b[23],lat.m2astge1$b[22],lat.m2astge1$b[17],lat.m2astge1$b[18],
                                    1,lat.m2astge1$b[14],lat.m2astge1$b[13],lat.m2astge1$b[16],lat.m2astge1$b[15],lat.m2astge1$b[11],lat.m2astge1$b[12],
                                    1,lat.m2astge1$b[8],lat.m2astge1$b[10],lat.m2astge1$b[9],lat.m2astge1$b[6],lat.m2astge1$b[7],
                                    1,lat.m2astge1$b[5],lat.m2astge1$b[4],lat.m2astge1$b[2],lat.m2astge1$b[3],
                                    1,lat.m2astge1$b[28],lat.m2astge1$b[26],lat.m2astge1$b[27],
                                    1,lat.m2astge1$b[24],lat.m2astge1$b[25],
                                    1,lat.m2astge1$b[1],
                                    1),diag=T)

# Add column names to correlation matrix
colnames(lat.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(lat.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
lat.m2acov<-lat.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                       c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
lat.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                            0,0,0,0,0,0,0,0),
                            type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                            dimnames = list(colnames(lat.m2acor), colnames(lat.m2acor)))

# Define Matrix S - free variances and covariances
lat.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_lat',1,
                           '0.1*incWedu_lat','0.1*inc2Wedu_lat',1,
                           '0.1*incWedu2_lat','0.1*inc2Wedu2_lat','0.1*eduWedu2_lat',1,
                           '0.1*incWoccs_lat','0.1*inc2Woccs_lat','0.1*eduWoccs_lat','0.1*edu2Woccs_lat',1,
                           '0.1*incWoccs2_lat','0.1*inc2Woccs2_lat','0.1*eduWoccs2_lat','0.1*edu2Woccs2_lat','0.1*occsWoccs2_lat',1,
                           0,0,0,0,0,0,'1*Errdep_lat',
                           '0.1*incWdeppw_lat','0.1*inc2Wdeppw_lat','0.1*eduWdeppw_lat','0.1*edu2Wdeppw_lat','0.1*occsWdeppw_lat','0.1*occs2Wdeppw_lat',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(lat.m2acor),colnames(lat.m2acor)))

# Fit Model 2A
lat.m2astge2<-wls(Cov=lat.m2acor,aCov=lat.m2acov,
                  n=sum(aggregate(ntot~cormid,data=sesipd.long.lat,max)$ntot),
                  Amatrix=lat.m2aA,Smatrix=lat.m2aS) 
sink(file='lat.m2astge2.txt')
summary(lat.m2astge2)
sink()
saveRDS(lat.m2astge2,file='lat.m2astge2.RDS')

## NATIVE AMERICAN PARTICIPANTS: Stage 1 ----
ai.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                    data=filter(sesipd.long.ai,!grepl('OCCP|OCCP2',cell)),
                    random=~1|DFID/esrid,
                    method="ML",
                    mods=~factor(cell)-1,
                    verbose=2,
                    sparse = T) 
summary(ai.m2astge1)

saveRDS(ai.m2astge1,file="ai.m2astge1.RDS")

## NATIVE AMERICAN PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
ai.m2acov<-ai.m2astge1$vb

# Define correlation matrix
ai.m2acor<-corpcor::vec2sm(vec = c(1,ai.m2astge1$b[21],ai.m2astge1$b[20],ai.m2astge1$b[19],ai.m2astge1$b[23],ai.m2astge1$b[22],ai.m2astge1$b[17],ai.m2astge1$b[18],
                                   1,ai.m2astge1$b[14],ai.m2astge1$b[13],ai.m2astge1$b[16],ai.m2astge1$b[15],ai.m2astge1$b[11],ai.m2astge1$b[12],
                                   1,ai.m2astge1$b[8],ai.m2astge1$b[10],ai.m2astge1$b[9],ai.m2astge1$b[6],ai.m2astge1$b[7],
                                   1,ai.m2astge1$b[5],ai.m2astge1$b[4],ai.m2astge1$b[2],ai.m2astge1$b[3],
                                   1,ai.m2astge1$b[28],ai.m2astge1$b[26],ai.m2astge1$b[27],
                                   1,ai.m2astge1$b[24],ai.m2astge1$b[25],
                                   1,ai.m2astge1$b[1],
                                   1),diag=T)

# Add column names to correlation matrix
colnames(ai.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(ai.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
ai.m2acov<-ai.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                     c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
ai.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                           0,0,0,0,0,0,0,0),
                           type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                           dimnames = list(colnames(ai.m2acor), colnames(ai.m2acor)))

# Define Matrix S - free variances and covariances
ai.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_ai',1,
                           '0.1*incWedu_ai','0.1*inc2Wedu_ai',1,
                           '0.1*incWedu2_ai','0.1*inc2Wedu2_ai','0.1*eduWedu2_ai',1,
                           '0.1*incWoccs_ai','0.1*inc2Woccs_ai','0.1*eduWoccs_ai','0.1*edu2Woccs_ai',1,
                           '0.1*incWoccs2_ai','0.1*inc2Woccs2_ai','0.1*eduWoccs2_ai','0.1*edu2Woccs2_ai','0.1*occsWoccs2_ai',1,
                           0,0,0,0,0,0,'1*Errdep_ai',
                           '0.1*incWdeppw_ai','0.1*inc2Wdeppw_ai','0.1*eduWdeppw_ai','0.1*edu2Wdeppw_ai','0.1*occsWdeppw_ai','0.1*occs2Wdeppw_ai',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(ai.m2acor),colnames(ai.m2acor)))

# Fit Model 2A
ai.m2astge2<-wls(Cov=ai.m2acor,aCov=ai.m2acov,
                 n=sum(aggregate(ntot~cormid,data=sesipd.long.ai,max)$ntot),
                 Amatrix=ai.m2aA,Smatrix=ai.m2aS) 
sink(file='ai.m2astge2.txt')
summary(ai.m2astge2)
sink()
saveRDS(ai.m2astge2,file='ai.m2astge2.RDS')

## MULTIRACIAL PARTICIPANTS: Stage 1 ----
mr.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                    data=filter(sesipd.long.mr,!grepl('OCCP|OCCP2',cell)),
                    random=~1|DFID/esrid,
                    method="ML",
                    mods=~factor(cell)-1,
                    verbose=2,
                    sparse = T) 
summary(mr.m2astge1)

saveRDS(mr.m2astge1,file="mr.m2astge1.RDS")

## MULTIRACIAL PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
mr.m2acov<-mr.m2astge1$vb

# Define correlation matrix
mr.m2acor<-corpcor::vec2sm(vec = c(1,mr.m2astge1$b[21],mr.m2astge1$b[20],mr.m2astge1$b[19],mr.m2astge1$b[23],mr.m2astge1$b[22],mr.m2astge1$b[17],mr.m2astge1$b[18],
                                   1,mr.m2astge1$b[14],mr.m2astge1$b[13],mr.m2astge1$b[16],mr.m2astge1$b[15],mr.m2astge1$b[11],mr.m2astge1$b[12],
                                   1,mr.m2astge1$b[8],mr.m2astge1$b[10],mr.m2astge1$b[9],mr.m2astge1$b[6],mr.m2astge1$b[7],
                                   1,mr.m2astge1$b[5],mr.m2astge1$b[4],mr.m2astge1$b[2],mr.m2astge1$b[3],
                                   1,mr.m2astge1$b[28],mr.m2astge1$b[26],mr.m2astge1$b[27],
                                   1,mr.m2astge1$b[24],mr.m2astge1$b[25],
                                   1,mr.m2astge1$b[1],
                                   1),diag=T)

# Add column names to correlation matrix
colnames(mr.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(mr.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
mr.m2acov<-mr.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                     c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
mr.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                           0,0,0,0,0,0,0,0),
                           type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                           dimnames = list(colnames(mr.m2acor), colnames(mr.m2acor)))

# Define Matrix S - free variances and covariances
mr.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_mr',1,
                           '0.1*incWedu_mr','0.1*inc2Wedu_mr',1,
                           '0.1*incWedu2_mr','0.1*inc2Wedu2_mr','0.1*eduWedu2_mr',1,
                           '0.1*incWoccs_mr','0.1*inc2Woccs_mr','0.1*eduWoccs_mr','0.1*edu2Woccs_mr',1,
                           '0.1*incWoccs2_mr','0.1*inc2Woccs2_mr','0.1*eduWoccs2_mr','0.1*edu2Woccs2_mr','0.1*occsWoccs2_mr',1,
                           0,0,0,0,0,0,'1*Errdep_mr',
                           '0.1*incWdeppw_mr','0.1*inc2Wdeppw_mr','0.1*eduWdeppw_mr','0.1*edu2Wdeppw_mr','0.1*occsWdeppw_mr','0.1*occs2Wdeppw_mr',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(mr.m2acor),colnames(mr.m2acor)))

# Fit Model 2A
mr.m2astge2<-wls(Cov=mr.m2acor,aCov=mr.m2acov,
                 n=sum(aggregate(ntot~cormid,data=sesipd.long.mr,max)$ntot),
                 Amatrix=mr.m2aA,Smatrix=mr.m2aS) 
sink(file='mr.m2astge2.txt')
summary(mr.m2astge2)
sink()
saveRDS(mr.m2astge2,file='mr.m2astge2.RDS')

## WHITE PARTICIPANTS: Stage 1 ----
wht.m2astge1<-rma.mv(yi=esr,V=esr.v, 
                     data=filter(sesipd.long.wht,!grepl('OCCP|OCCP2',cell)),
                     random=~1|DFID/esrid,
                     method="ML",
                     mods=~factor(cell)-1,
                     verbose=2,
                     sparse = T) 
summary(wht.m2astge1)

saveRDS(wht.m2astge1,file="wht.m2astge1.RDS")

## WHITE PARTICIPANTS: Stage 2 ----
# Extract asymptotic covariance matrix from model output
wht.m2acov<-wht.m2astge1$vb

# Define correlation matrix
wht.m2acor<-corpcor::vec2sm(vec = c(1,wht.m2astge1$b[21],wht.m2astge1$b[20],wht.m2astge1$b[19],wht.m2astge1$b[23],wht.m2astge1$b[22],wht.m2astge1$b[17],wht.m2astge1$b[18],
                                    1,wht.m2astge1$b[14],wht.m2astge1$b[13],wht.m2astge1$b[16],wht.m2astge1$b[15],wht.m2astge1$b[11],wht.m2astge1$b[12],
                                    1,wht.m2astge1$b[8],wht.m2astge1$b[10],wht.m2astge1$b[9],wht.m2astge1$b[6],wht.m2astge1$b[7],
                                    1,wht.m2astge1$b[5],wht.m2astge1$b[4],wht.m2astge1$b[2],wht.m2astge1$b[3],
                                    1,wht.m2astge1$b[28],wht.m2astge1$b[26],wht.m2astge1$b[27],
                                    1,wht.m2astge1$b[24],wht.m2astge1$b[25],
                                    1,wht.m2astge1$b[1],
                                    1),diag=T)

# Add column names to correlation matrix
colnames(wht.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
rownames(wht.m2acor)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")

# Arrange asymptotic covariance matrix to match correlation matrix
wht.m2acov<-wht.m2acov[c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1),
                       c(21,20,19,23,22,17,18,14,13,16,15,11,12,8,10,9,6,7,5,4,2,3,28,26,27,24,25,1)]

# Define Matrix A - free direct path parameters
wht.m2aA<-create.mxMatrix(c(0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,
                            '0.1*inctodep','0.1*inc2todep','0.1*edutodep','0.1*edu2todep','0.1*occstodep','0.1*occs2todep',0,'0.1*deppwtodep',
                            0,0,0,0,0,0,0,0),
                            type="Full",nrow=8,ncol=8,byrow=T,name = "A",
                            dimnames = list(colnames(wht.m2acor), colnames(wht.m2acor)))

# Define Matrix S - free variances and covariances
wht.m2aS<-create.mxMatrix(c(1,
                           '0.1*incWinc2_wht',1,
                           '0.1*incWedu_wht','0.1*inc2Wedu_wht',1,
                           '0.1*incWedu2_wht','0.1*inc2Wedu2_wht','0.1*eduWedu2_wht',1,
                           '0.1*incWoccs_wht','0.1*inc2Woccs_wht','0.1*eduWoccs_wht','0.1*edu2Woccs_wht',1,
                           '0.1*incWoccs2_wht','0.1*inc2Woccs2_wht','0.1*eduWoccs2_wht','0.1*edu2Woccs2_wht','0.1*occsWoccs2_wht',1,
                           0,0,0,0,0,0,'1*Errdep_wht',
                           '0.1*incWdeppw_wht','0.1*inc2Wdeppw_wht','0.1*eduWdeppw_wht','0.1*edu2Wdeppw_wht','0.1*occsWdeppw_wht','0.1*occs2Wdeppw_wht',0,1), 
                           type="Symm",byrow=T,name="S", 
                           dimnames = list(colnames(wht.m2acor),colnames(wht.m2acor)))

# Fit Model 2A
wht.m2astge2<-wls(Cov=wht.m2acor,aCov=wht.m2acov,
                  n=sum(aggregate(ntot~cormid,data=sesipd.long.wht,max)$ntot),
                  Amatrix=wht.m2aA,Smatrix=wht.m2aS) 
sink(file='wht.m2astge2.txt')
summary(wht.m2astge2)
sink()
saveRDS(wht.m2astge2,file='wht.m2astge2.RDS')

## EQUALITY CONSTRAINTS TEST: REGRESSION COEFFICIENTS ----
# Define path models by sex
aa.m2astge2.eq<-wls(Cov=aa.m2acor,aCov=aa.m2acov,Amatrix=m2aA,Smatrix=aa.m2aS,run=F,model.name="asianamerican")
blk.m2astge2.eq<-wls(Cov=blk.m2acor,aCov=blk.m2acov,Amatrix=m2aA,Smatrix=blk.m2aS,run=F,model.name="black")
lat.m2astge2.eq<-wls(Cov=lat.m2acor,aCov=lat.m2acov,Amatrix=m2aA,Smatrix=lat.m2aS,run=F,model.name="latinx")
ai.m2astge2.eq<-wls(Cov=ai.m2acor,aCov=ai.m2acov,Amatrix=m2aA,Smatrix=ai.m2aS,run=F,model.name="nativeamerican")
mr.m2astge2.eq<-wls(Cov=mr.m2acor,aCov=mr.m2acov,Amatrix=m2aA,Smatrix=mr.m2aS,run=F,model.name="multiracial")
wht.m2astge2.eq<-wls(Cov=wht.m2acor,aCov=wht.m2acov,Amatrix=m2aA,Smatrix=wht.m2aS,run=F,model.name="white")

## Across all sub-groups
# Define multi-group path model
m2astge2.re<-mxModel(wht.m2astge2.eq,aa.m2astge2.eq,blk.m2astge2.eq,lat.m2astge2.eq,ai.m2astge2.eq,mr.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("white","asianamerican","black","latinx","nativeamerican","multiracial")))

# Multi-group moderation test
m2astge2.reeq<-mxRun(m2astge2.re,intervals=T)
summary(m2astge2.reeq)

sink(file='m2astge2.reeq.txt')
summary(m2astge2.reeq)
sink()
saveRDS(m2astge2.reeq,file='m2astge2.reeq.RDS')

# Chi-square test
m2astge2.reeq.chisq<-data.frame(chisq=m2astge2.reeq$output$fit,
                                chisq.p=pchisq(m2astge2.reeq$output$fit,
                                length(m2astge2.reeq$output$estimate[which(!grepl('_aa|_blk|_lat|_ai|_mr|_wht',names(m2astge2.reeq$output$estimate)))])*(length(m2astge2.reeq$output$matrices[which(grepl('Amatrix',names(m2astge2.reeq$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.reeq$output$estimate[which(!grepl('_aa|_blk|_lat|_ai|_mr|_wht',names(m2astge2.reeq$output$estimate)))])*(length(m2astge2.reeq$output$matrices[which(grepl('Amatrix',names(m2astge2.reeq$output$matrices)))])-1))
sink(file='m2astge2.reeq.chisq.txt')
m2astge2.reeq.chisq
sink()

## Between sub-groups
# White compared to Black participants
m2astge2.whtblk<-mxModel(wht.m2astge2.eq,blk.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("white","black")))

# Multi-group moderation test
m2astge2.whtblk<-mxRun(m2astge2.whtblk,intervals=T)
summary(m2astge2.whtblk)

sink(file='m2astge2.whtblk.txt')
summary(m2astge2.whtblk)
sink()
saveRDS(m2astge2.whtblk,file='m2astge2.whtblk.RDS')

# Chi-square test
m2astge2.whtblk.chisq<-data.frame(chisq=m2astge2.whtblk$output$fit,
                                chisq.p=pchisq(m2astge2.whtblk$output$fit,
                                length(m2astge2.whtblk$output$estimate[which(!grepl('_blk|_wht',names(m2astge2.whtblk$output$estimate)))])*(length(m2astge2.whtblk$output$matrices[which(grepl('Amatrix',names(m2astge2.whtblk$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.whtblk$output$estimate[which(!grepl('_blk|_wht',names(m2astge2.whtblk$output$estimate)))])*(length(m2astge2.whtblk$output$matrices[which(grepl('Amatrix',names(m2astge2.whtblk$output$matrices)))])-1))
sink(file='m2astge2.whtblk.chisq.txt')
m2astge2.whtblk.chisq
sink()

# White compared to Latinx participants
m2astge2.whtlat<-mxModel(wht.m2astge2.eq,lat.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("white","latinx")))

# Multi-group moderation test
m2astge2.whtlat<-mxRun(m2astge2.whtlat,intervals=T)
summary(m2astge2.whtlat)

sink(file='m2astge2.whtlat.txt')
summary(m2astge2.whtlat)
sink()
saveRDS(m2astge2.whtlat,file='m2astge2.whtlat.RDS')

# Chi-square test
m2astge2.whtlat.chisq<-data.frame(chisq=m2astge2.whtlat$output$fit,
                                chisq.p=pchisq(m2astge2.whtlat$output$fit,
                                length(m2astge2.whtlat$output$estimate[which(!grepl('_lat|_wht',names(m2astge2.whtlat$output$estimate)))])*(length(m2astge2.whtlat$output$matrices[which(grepl('Amatrix',names(m2astge2.whtlat$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.whtlat$output$estimate[which(!grepl('_lat|_wht',names(m2astge2.whtlat$output$estimate)))])*(length(m2astge2.whtlat$output$matrices[which(grepl('Amatrix',names(m2astge2.whtlat$output$matrices)))])-1))
sink(file='m2astge2.whtlat.chisq.txt')
m2astge2.whtlat.chisq
sink()

# White compared to Multiracial participants
m2astge2.whtmr<-mxModel(wht.m2astge2.eq,mr.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("white","multiracial")))

# Multi-group moderation test
m2astge2.whtmr<-mxRun(m2astge2.whtmr,intervals=T)
summary(m2astge2.whtmr)

sink(file='m2astge2.whtmr.txt')
summary(m2astge2.whtmr)
sink()
saveRDS(m2astge2.whtmr,file='m2astge2.whtmr.RDS')

# Chi-square test
m2astge2.whtmr.chisq<-data.frame(chisq=m2astge2.whtmr$output$fit,
                                chisq.p=pchisq(m2astge2.whtmr$output$fit,
                                length(m2astge2.whtmr$output$estimate[which(!grepl('_mr|_wht',names(m2astge2.whtmr$output$estimate)))])*(length(m2astge2.whtmr$output$matrices[which(grepl('Amatrix',names(m2astge2.whtmr$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.whtmr$output$estimate[which(!grepl('_mr|_wht',names(m2astge2.whtmr$output$estimate)))])*(length(m2astge2.whtmr$output$matrices[which(grepl('Amatrix',names(m2astge2.whtmr$output$matrices)))])-1))
sink(file='m2astge2.whtmr.chisq.txt')
m2astge2.whtmr.chisq
sink()

# White compared to Native American participants
m2astge2.whtai<-mxModel(wht.m2astge2.eq,ai.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("white","nativeamerican")))

# Multi-group moderation test
m2astge2.whtai<-mxRun(m2astge2.whtai,intervals=T)
summary(m2astge2.whtai)

sink(file='m2astge2.whtai.txt')
summary(m2astge2.whtai)
sink()
saveRDS(m2astge2.whtai,file='m2astge2.whtai.RDS')

# Chi-square test
m2astge2.whtai.chisq<-data.frame(chisq=m2astge2.whtai$output$fit,
                                chisq.p=pchisq(m2astge2.whtai$output$fit,
                                length(m2astge2.whtai$output$estimate[which(!grepl('_ai|_wht',names(m2astge2.whtai$output$estimate)))])*(length(m2astge2.whtai$output$matrices[which(grepl('Amatrix',names(m2astge2.whtai$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.whtai$output$estimate[which(!grepl('_ai|_wht',names(m2astge2.whtai$output$estimate)))])*(length(m2astge2.whtai$output$matrices[which(grepl('Amatrix',names(m2astge2.whtai$output$matrices)))])-1))
sink(file='m2astge2.whtai.chisq.txt')
m2astge2.whtai.chisq
sink()

# White compared to Asian American participants
m2astge2.whtaa<-mxModel(wht.m2astge2.eq,aa.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("white","asianamerican")))

# Multi-group moderation test
m2astge2.whtaa<-mxRun(m2astge2.whtaa,intervals=T)
summary(m2astge2.whtaa)

sink(file='m2astge2.whtaa.txt')
summary(m2astge2.whtaa)
sink()
saveRDS(m2astge2.whtaa,file='m2astge2.whtaa.RDS')

# Chi-square test
m2astge2.whtaa.chisq<-data.frame(chisq=m2astge2.whtaa$output$fit,
                                chisq.p=pchisq(m2astge2.whtaa$output$fit,
                                length(m2astge2.whtaa$output$estimate[which(!grepl('_aa|_wht',names(m2astge2.whtaa$output$estimate)))])*(length(m2astge2.whtaa$output$matrices[which(grepl('Amatrix',names(m2astge2.whtaa$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.whtaa$output$estimate[which(!grepl('_aa|_wht',names(m2astge2.whtaa$output$estimate)))])*(length(m2astge2.whtaa$output$matrices[which(grepl('Amatrix',names(m2astge2.whtaa$output$matrices)))])-1))
sink(file='m2astge2.whtaa.chisq.txt')
m2astge2.whtaa.chisq
sink()

# Black compared to Latinx participants
m2astge2.blklat<-mxModel(blk.m2astge2.eq,lat.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("black","latinx")))

# Multi-group moderation test
m2astge2.blklat<-mxRun(m2astge2.blklat,intervals=T)
summary(m2astge2.blklat)

sink(file='m2astge2.blklat.txt')
summary(m2astge2.blklat)
sink()
saveRDS(m2astge2.blklat,file='m2astge2.blklat.RDS')

# Chi-square test
m2astge2.blklat.chisq<-data.frame(chisq=m2astge2.blklat$output$fit,
                                chisq.p=pchisq(m2astge2.blklat$output$fit,
                                length(m2astge2.blklat$output$estimate[which(!grepl('_blk|_lat',names(m2astge2.blklat$output$estimate)))])*(length(m2astge2.blklat$output$matrices[which(grepl('Amatrix',names(m2astge2.blklat$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.blklat$output$estimate[which(!grepl('_blk|_lat',names(m2astge2.blklat$output$estimate)))])*(length(m2astge2.blklat$output$matrices[which(grepl('Amatrix',names(m2astge2.blklat$output$matrices)))])-1))
sink(file='m2astge2.blklat.chisq.txt')
m2astge2.blklat.chisq
sink()

# Black compared to Multiracial participants
m2astge2.blkmr<-mxModel(blk.m2astge2.eq,mr.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("black","multiracial")))

# Multi-group moderation test
m2astge2.blkmr<-mxRun(m2astge2.blkmr,intervals=T)
summary(m2astge2.blkmr)

sink(file='m2astge2.blkmr.txt')
summary(m2astge2.blkmr)
sink()
saveRDS(m2astge2.blkmr,file='m2astge2.blkmr.RDS')

# Chi-square test
m2astge2.blkmr.chisq<-data.frame(chisq=m2astge2.blkmr$output$fit,
                                chisq.p=pchisq(m2astge2.blkmr$output$fit,
                                length(m2astge2.blkmr$output$estimate[which(!grepl('_blk|_mr',names(m2astge2.blkmr$output$estimate)))])*(length(m2astge2.blkmr$output$matrices[which(grepl('Amatrix',names(m2astge2.blkmr$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.blkmr$output$estimate[which(!grepl('_blk|_mr',names(m2astge2.blkmr$output$estimate)))])*(length(m2astge2.blkmr$output$matrices[which(grepl('Amatrix',names(m2astge2.blkmr$output$matrices)))])-1))
sink(file='m2astge2.blkmr.chisq.txt')
m2astge2.blkmr.chisq
sink()

# Black compared to Native American participants
m2astge2.blkai<-mxModel(blk.m2astge2.eq,ai.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("black","nativeamerican")))

# Multi-group moderation test
m2astge2.blkai<-mxRun(m2astge2.blkai,intervals=T)
summary(m2astge2.blkai)

sink(file='m2astge2.blkai.txt')
summary(m2astge2.blkai)
sink()
saveRDS(m2astge2.blkai,file='m2astge2.blkai.RDS')

# Chi-square test
m2astge2.blkai.chisq<-data.frame(chisq=m2astge2.blkai$output$fit,
                                chisq.p=pchisq(m2astge2.blkai$output$fit,
                                length(m2astge2.blkai$output$estimate[which(!grepl('_blk|_ai',names(m2astge2.blkai$output$estimate)))])*(length(m2astge2.blkai$output$matrices[which(grepl('Amatrix',names(m2astge2.blkai$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.blkai$output$estimate[which(!grepl('_blk|_ai',names(m2astge2.blkai$output$estimate)))])*(length(m2astge2.blkai$output$matrices[which(grepl('Amatrix',names(m2astge2.blkai$output$matrices)))])-1))
sink(file='m2astge2.blkai.chisq.txt')
m2astge2.blkai.chisq
sink()

# Black compared to Asian American participants
m2astge2.blkaa<-mxModel(aa.m2astge2.eq,blk.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("asianamerican","black")))

# Multi-group moderation test
m2astge2.blkaa<-mxRun(m2astge2.blkaa,intervals=T)
summary(m2astge2.blkaa)

sink(file='m2astge2.blkaa.txt')
summary(m2astge2.blkaa)
sink()
saveRDS(m2astge2.blkaa,file='m2astge2.blkaa.RDS')

# Chi-square test
m2astge2.blkaa.chisq<-data.frame(chisq=m2astge2.blkaa$output$fit,
                                chisq.p=pchisq(m2astge2.blkaa$output$fit,
                                length(m2astge2.blkaa$output$estimate[which(!grepl('_aa|_blk',names(m2astge2.blkaa$output$estimate)))])*(length(m2astge2.blkaa$output$matrices[which(grepl('Amatrix',names(m2astge2.blkaa$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.blkaa$output$estimate[which(!grepl('_aa|_blk',names(m2astge2.blkaa$output$estimate)))])*(length(m2astge2.blkaa$output$matrices[which(grepl('Amatrix',names(m2astge2.blkaa$output$matrices)))])-1))
sink(file='m2astge2.blkaa.chisq.txt')
m2astge2.blkaa.chisq
sink()

# Latinx compared to Multiracial participants
m2astge2.latmr<-mxModel(lat.m2astge2.eq,mr.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("latinx","multiracial")))

# Multi-group moderation test
m2astge2.latmr<-mxRun(m2astge2.latmr,intervals=T)
summary(m2astge2.latmr)

sink(file='m2astge2.latmr.txt')
summary(m2astge2.latmr)
sink()
saveRDS(m2astge2.latmr,file='m2astge2.latmr.RDS')

# Chi-square test
m2astge2.latmr.chisq<-data.frame(chisq=m2astge2.latmr$output$fit,
                                chisq.p=pchisq(m2astge2.latmr$output$fit,
                                length(m2astge2.latmr$output$estimate[which(!grepl('_lat|_mr',names(m2astge2.latmr$output$estimate)))])*(length(m2astge2.latmr$output$matrices[which(grepl('Amatrix',names(m2astge2.latmr$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.latmr$output$estimate[which(!grepl('_lat|_mr',names(m2astge2.latmr$output$estimate)))])*(length(m2astge2.latmr$output$matrices[which(grepl('Amatrix',names(m2astge2.latmr$output$matrices)))])-1))
sink(file='m2astge2.latmr.chisq.txt')
m2astge2.latmr.chisq
sink()

# Latinx compared to Native American participants
m2astge2.latai<-mxModel(lat.m2astge2.eq,ai.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("latinx","nativeamerican")))

# Multi-group moderation test
m2astge2.latai<-mxRun(m2astge2.latai,intervals=T)
summary(m2astge2.latai)

sink(file='m2astge2.latai.txt')
summary(m2astge2.latai)
sink()
saveRDS(m2astge2.latai,file='m2astge2.latai.RDS')

# Chi-square test
m2astge2.latai.chisq<-data.frame(chisq=m2astge2.latai$output$fit,
                                chisq.p=pchisq(m2astge2.latai$output$fit,
                                length(m2astge2.latai$output$estimate[which(!grepl('_lat|_ai',names(m2astge2.latai$output$estimate)))])*(length(m2astge2.latai$output$matrices[which(grepl('Amatrix',names(m2astge2.latai$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.latai$output$estimate[which(!grepl('_lat|_ai',names(m2astge2.latai$output$estimate)))])*(length(m2astge2.latai$output$matrices[which(grepl('Amatrix',names(m2astge2.latai$output$matrices)))])-1))
sink(file='m2astge2.latai.chisq.txt')
m2astge2.latai.chisq
sink()

# Latinx compared to Asian American participants
m2astge2.lataa<-mxModel(aa.m2astge2.eq,lat.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("asianamerican","latinx")))

# Multi-group moderation test
m2astge2.lataa<-mxRun(m2astge2.lataa,intervals=T)
summary(m2astge2.lataa)

sink(file='m2astge2.lataa.txt')
summary(m2astge2.lataa)
sink()
saveRDS(m2astge2.lataa,file='m2astge2.lataa.RDS')

# Chi-square test
m2astge2.lataa.chisq<-data.frame(chisq=m2astge2.lataa$output$fit,
                                chisq.p=pchisq(m2astge2.lataa$output$fit,
                                length(m2astge2.lataa$output$estimate[which(!grepl('_aa|_lat',names(m2astge2.lataa$output$estimate)))])*(length(m2astge2.lataa$output$matrices[which(grepl('Amatrix',names(m2astge2.lataa$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.lataa$output$estimate[which(!grepl('_aa|_lat',names(m2astge2.lataa$output$estimate)))])*(length(m2astge2.lataa$output$matrices[which(grepl('Amatrix',names(m2astge2.lataa$output$matrices)))])-1))
sink(file='m2astge2.lataa.chisq.txt')
m2astge2.lataa.chisq
sink()

# Multiracial compared to Native American participants
m2astge2.mrai<-mxModel(ai.m2astge2.eq,mr.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("nativeamerican","multiracial")))

# Multi-group moderation test
m2astge2.mrai<-mxRun(m2astge2.mrai,intervals=T)
summary(m2astge2.mrai)

sink(file='m2astge2.mrai.txt')
summary(m2astge2.mrai)
sink()
saveRDS(m2astge2.mrai,file='m2astge2.mrai.RDS')

# Chi-square test
m2astge2.mrai.chisq<-data.frame(chisq=m2astge2.mrai$output$fit,
                                chisq.p=pchisq(m2astge2.mrai$output$fit,
                                length(m2astge2.mrai$output$estimate[which(!grepl('_ai|_mr',names(m2astge2.mrai$output$estimate)))])*(length(m2astge2.mrai$output$matrices[which(grepl('Amatrix',names(m2astge2.mrai$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.mrai$output$estimate[which(!grepl('_ai|_mr',names(m2astge2.mrai$output$estimate)))])*(length(m2astge2.mrai$output$matrices[which(grepl('Amatrix',names(m2astge2.mrai$output$matrices)))])-1))
sink(file='m2astge2.mrai.chisq.txt')
m2astge2.mrai.chisq
sink()

# Multiracial compared to Asian American participants
m2astge2.mraa<-mxModel(aa.m2astge2.eq,mr.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("asianamerican","multiracial")))

# Multi-group moderation test
m2astge2.mraa<-mxRun(m2astge2.mraa,intervals=T)
summary(m2astge2.mraa)

sink(file='m2astge2.mraa.txt')
summary(m2astge2.mraa)
sink()
saveRDS(m2astge2.mraa,file='m2astge2.mraa.RDS')

# Chi-square test
m2astge2.mraa.chisq<-data.frame(chisq=m2astge2.mraa$output$fit,
                                chisq.p=pchisq(m2astge2.mraa$output$fit,
                                length(m2astge2.mraa$output$estimate[which(!grepl('_aa|_mr',names(m2astge2.mraa$output$estimate)))])*(length(m2astge2.mraa$output$matrices[which(grepl('Amatrix',names(m2astge2.mraa$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.mraa$output$estimate[which(!grepl('_aa|_mr',names(m2astge2.mraa$output$estimate)))])*(length(m2astge2.mraa$output$matrices[which(grepl('Amatrix',names(m2astge2.mraa$output$matrices)))])-1))
sink(file='m2astge2.mraa.chisq.txt')
m2astge2.mraa.chisq
sink()

# Native American compared to Asian American participants
m2astge2.aiaa<-mxModel(aa.m2astge2.eq,ai.m2astge2.eq,
                       model="Equal_Coefficients", 
                       mxFitFunctionMultigroup(c("asianamerican","nativeamerican")))

# Multi-group moderation test
m2astge2.aiaa<-mxRun(m2astge2.aiaa,intervals=T)
summary(m2astge2.aiaa)

sink(file='m2astge2.aiaa.txt')
summary(m2astge2.aiaa)
sink()
saveRDS(m2astge2.aiaa,file='m2astge2.aiaa.RDS')

# Chi-square test
m2astge2.aiaa.chisq<-data.frame(chisq=m2astge2.aiaa$output$fit,
                                chisq.p=pchisq(m2astge2.aiaa$output$fit,
                                length(m2astge2.aiaa$output$estimate[which(!grepl('_aa|_ai',names(m2astge2.aiaa$output$estimate)))])*(length(m2astge2.aiaa$output$matrices[which(grepl('Amatrix',names(m2astge2.aiaa$output$matrices)))])-1),
                                lower.tail=F),
                                df=length(m2astge2.aiaa$output$estimate[which(!grepl('_aa|_ai',names(m2astge2.aiaa$output$estimate)))])*(length(m2astge2.aiaa$output$matrices[which(grepl('Amatrix',names(m2astge2.aiaa$output$matrices)))])-1))
sink(file='m2astge2.aiaa.chisq.txt')
m2astge2.aiaa.chisq
sink()

## HISTOGRAM PLOT OF CORRELATIONS-----
sesipd.long%>%ggplot()+
       geom_histogram(aes(x=esr),bins=20)+
       facet_wrap(~factor(cell,
                     levels=c("INCMCINC2","INCMCEDUMC","INCMCEDU2","INCMCOCCSMC","INCMCOCCS2","INCMCOCCPMC","INCMCOCCP2","INCMCDEP","INCMCDEPPW",
                              "INC2EDUMC","INC2EDU2","INC2OCCSMC","INC2OCCS2","INC2OCCPMC","INC2OCCP2","INC2DEP","INC2DEPPW",
                              "EDUMCEDU2","EDUMCOCCSMC","EDUMCOCCS2","EDUMCOCCPMC","EDUMCOCCP2","EDUMCDEP","EDUMCDEPPW",
                              "EDU2OCCSMC","EDU2OCCS2","EDU2OCCPMC","EDU2OCCP2","EDU2DEP","EDU2DEPPW",
                              "OCCSMCOCCS2","OCCSMCOCCPMC","OCCSMCOCCP2","OCCSMCDEP","OCCSMCDEPPW",
                              "OCCS2OCCPMC","OCCS2OCCP2","OCCS2DEP","OCCS2DEPPW",
                              "OCCPMCOCCP2","OCCPMCDEP","OCCPMCDEPPW",
                              "OCCP2DEP","OCCP2DEPPW",
                              "DEPDEPPW")),nrow=9)+
      labs(x='Correlation (r)',y='Count')+
      theme_classic()
