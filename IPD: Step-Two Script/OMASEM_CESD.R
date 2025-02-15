### SYNTAX OMASEM ANALYSES: RQ #1 and RQ #2 ####
# Data in this script was derived from OMASEM CESD_Reformatting.R
# Data is imported in each model step
# Load Packages
library(metaSEM)
library(semPlot)

#### MODELS WITH NO CORRELATIONS BETWEEN SES COMPONENTS AND PRIOR DEPRESSIVE SYMPTOMS ----
### MODEL 1A ----
load('model1adata.RData')
# Display the number of correlations
model1ancorrs<-matrix(pattern.na(model1adata$cordat,show.na=FALSE),ncol=5,nrow=5,
                      dimnames = list(c("INC","EDU","OCCS","DEP","DEPPW"),
                                      c("INC","EDU","OCCS","DEP","DEPPW")))
write.csv(model1ancorrs,file='model1ancorrs.csv')
# Display the accumulative sample size for each correlation
model1ans<-matrix(pattern.n(model1adata$cordat,model1adata$n),ncol=5,nrow=5,
                  dimnames = list(c("INC","EDU","OCCS","DEP","DEPPW"),
                                  c("INC","EDU","OCCS","DEP","DEPPW")))
write.csv(model1ans,file='model1ans.csv')
# Sampling covariance matrix of the correlations
model1adf <- Cor2DataFrame(model1adata$cordat, model1adata$n, acov = "weighted")
save(model1adf, file="model1adf.RData")
# Define Model 1a using lavaan syntax 
Model1a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS
          EDU ~~ eduWoccs*OCCS
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model1a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM1a <- lavaan2RAM(Model1a, obs.variables=c("INC","EDU","OCCS","DEP","DEPPW"))
RAM1a

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M1a <- create.vechsR(A0=RAM1a$A, S0=RAM1a$S)

# Heterogeneity Variance-Covariance Matrix
T1a <- create.Tau2(RAM=RAM1a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit1a <- osmasem(model.name="Model_1A", Mmatrix=M1a, Tmatrix=T1a, data=model1adf)
summary(mx.fit1a)
# Save Model 1A Summary
sink("model1aest.txt")
print(summary(mx.fit1a))
sink()  
# Tau 2 estimates
model1avarcorr<-VarCorr(mx.fit1a)
sink("model1ataus.txt")
print(model1avarcorr)
sink() 
# Residuals
Model1aresiduals<-data.frame(mxEval(Smatrix, mx.fit1a$mx.fit))
colnames(Model1aresiduals)<-c("INC","EDU","OCCS","DEP","DEPPW")
sink("Model1aresiduals.txt")
print(Model1aresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model1avmatrix<-mxEval(V,mx.fit1a$mx.fit)
sink("Model1avmatrix.txt")
print(Model1avmatrix)
sink() 
# Implied R Matrix
Model1armatrix<-mxEval(impliedR,mx.fit1a$mx.fit)
colnames(Model1armatrix)<-c("INC","EDU","OCCS","DEP","DEPPW")
sink("Model1armatrix.txt")
print(Model1armatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model1aexpCovmatrix<-mxEval(expCov,mx.fit1a$mx.fit)
sink("Model1aexpCovmatrix.txt")
print(Model1aexpCovmatrix)
sink() 
# Summary with Fit Indices
Model1asumwfit<-summary(mx.fit1a,fitIndices = TRUE)
sink("model1aestwfit.txt")
print(Model1asumwfit)
sink() 
# SRMR
Model1asrmr<-osmasemSRMR(mx.fit1a)
sink("model1asrmr.txt")
print(Model1asrmr)
sink() 
# Save osmasem List
save(mx.fit1a,file="mx_fit1a.RData")

### MODEL 1B ----
load('model1bdata.RData')
# Display the number of correlations
model1bncorrs<-matrix(pattern.na(model1bdata$cordat,show.na=FALSE),ncol=5,nrow=5,
                      dimnames = list(c("INC","EDU","OCCP","DEP","DEPPW"),
                                      c("INC","EDU","OCCP","DEP","DEPPW")))
write.csv(model1bncorrs,file='model1bncorrs.csv')
# Display the accumulative sample size for each correlation
model1bns<-matrix(pattern.n(model1bdata$cordat,model1bdata$n),ncol=5,nrow=5,
                  dimnames = list(c("INC","EDU","OCCP","DEP","DEPPW"),
                                  c("INC","EDU","OCCP","DEP","DEPPW")))
write.csv(model1bns,file='model1bns.csv')
# Sampling covariance matrix of the correlations
model1bdf <- Cor2DataFrame(model1bdata$cordat, model1bdata$n, acov = "weighted")
save(model1bdf, file="model1bdf.RData")
# Define Model 1a using lavaan syntax 
Model1b<-'DEP ~ inctodep*INC + edutodep*EDU + occptodep*OCCP + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccp*OCCP
          EDU ~~ eduWoccp*OCCP
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCP ~~ 1*OCCP
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model1b, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM1b <- lavaan2RAM(Model1b, obs.variables=c("INC","EDU","OCCP","DEP","DEPPW"))
RAM1b

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M1b <- create.vechsR(A0=RAM1b$A, S0=RAM1b$S)

# Heterogeneity Variance-Covariance Matrix
T1b <- create.Tau2(RAM=RAM1b, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit1b <- osmasem(model.name="Model_1B", Mmatrix=M1b, Tmatrix=T1b, data=model1bdf)
summary(mx.fit1b)
# Save Model 1A Summary
sink("model1best.txt")
print(summary(mx.fit1b))
sink()  
# Tau 2 estimates
model1bvarcorr<-VarCorr(mx.fit1b)
sink("model1btaus.txt")
print(model1bvarcorr)
sink() 
#Residuals
Model1bresiduals<-data.frame(mxEval(Smatrix, mx.fit1b$mx.fit))
colnames(Model1bresiduals)<-c("INC","EDU","OCCP","DEP","DEPPW")
sink("Model1bresiduals.txt")
print(Model1bresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model1bvmatrix<-mxEval(V,mx.fit1b$mx.fit)
sink("Model1bvmatrix.txt")
print(Model1bvmatrix)
sink() 
# Implied R Matrix
Model1brmatrix<-mxEval(impliedR,mx.fit1b$mx.fit)
colnames(Model1brmatrix)<-c("INC","EDU","OCCP","DEP","DEPPW")
sink("Model1brmatrix.txt")
print(Model1brmatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model1bexpCovmatrix<-mxEval(expCov,mx.fit1b$mx.fit)
sink("Model1bexpCovmatrix.txt")
print(Model1bexpCovmatrix)
sink() 
# Summary with Fit Indices
Model1bsumwfit<-summary(mx.fit1b,fitIndices = TRUE)
sink("model1bestwfit.txt")
print(Model1bsumwfit)
sink() 
# SRMR
Model1bsrmr<-osmasemSRMR(mx.fit1b)
sink("model1bsrmr.txt")
print(Model1bsrmr)
sink() 
# Save osmasem List
save(mx.fit1b,file="mx_fit1b.RData")

### MODEL 2A (45-90 minutes) ----
rm(list=ls())
load('model2adata.RData')
# Display the number of correlations
model2ancorrs<-matrix(pattern.na(model2adata$cordat,show.na=FALSE),ncol=8,nrow=8,
                      dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                                      c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")))
write.csv(model2ancorrs,file='model2ancorrs.csv')
# Display the accumulative sample size for each correlation
model2ans<-matrix(pattern.n(model2adata$cordat,model2adata$n),ncol=8,nrow=8,
                  dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                                  c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")))
write.csv(model2ans,file='model2ans.csv')
# Sampling covariance matrix of the correlations
model2adf <- Cor2DataFrame(model2adata$cordat, model2adata$n, acov = "weighted")
save(model2adf, file="model2adf.RData")
# Define Model 1a using lavaan syntax 
Model2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2
          INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2
          EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2
          EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2
          OCCS ~~ occsWoccs2*OCCS2
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCS2 ~~ 1*OCCS2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2a <- lavaan2RAM(Model2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2a

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2a <- create.vechsR(A0=RAM2a$A, S0=RAM2a$S)

# Heterogeneity Variance-Covariance Matrix
T2a <- create.Tau2(RAM=RAM2a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2a <- osmasem(model.name="Model_2A", Mmatrix=M2a, Tmatrix=T2a, data=model2adf)
summary(mx.fit2a)
# Save Model 2A Summary
sink("Model2aest.txt")
print(summary(mx.fit2a))
sink()  
# Tau 2 estimates
options(max.print = 10000)
Model2avarcorr<-VarCorr(mx.fit2a)
sink("Model2ataus.txt")
print(Model2avarcorr)
sink() 
# Residuals
options(max.print = 10000)
Model2aresiduals<-data.frame(mxEval(Smatrix, mx.fit2a$mx.fit))
colnames(Model2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Model2aresiduals.txt")
print(Model2aresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model2avmatrix<-mxEval(V,mx.fit2a$mx.fit)
sink("Model2avmatrix.txt")
print(Model2avmatrix)
sink() 
# Implied R Matrix
Model2armatrix<-mxEval(impliedR,mx.fit2a$mx.fit)
colnames(Model2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Model2armatrix.txt")
print(Model2armatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model2aexpCovmatrix<-mxEval(expCov,mx.fit2a$mx.fit)
sink("Model2aexpCovmatrix.txt")
print(Model2aexpCovmatrix)
sink() 
# Summary with Fit Indices
Model2asumwfit<-summary(mx.fit2a,fitIndices = TRUE)
sink("Model2asumwfit.txt")
print(Model2asumwfit)
sink() 
# SRMR
Model2asrmr<-osmasemSRMR(mx.fit2a)
sink("Model2asrmr.txt")
print(Model2asrmr)
sink() 
# Save osmasem List
save(mx.fit2a,file="mx_fit2a.RData")

### MODEL 2B (45-90 minutes) ----
rm(list=ls())
load('model2bdata.RData')
# Sampling covariance matrix of the correlations
model2bdf <- Cor2DataFrame(model2bdata$cordat, model2bdata$n, acov = "weighted")
save(model2bdf, file="model2bdf.RData")
# Define Model 1a using lavaan syntax 
Model2b<-'DEP ~ inutodep*INC + edutodep*EDU + occptodep*OCCP + inc2todep*INC2 + edu2todep*EDU2 + occp2todep*OCCP2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccp*OCCP + incWinc2*INC2 + incWedu2*EDU2 + incWoccp2*OCCP2
          INC2 ~~ icn2Wedu*EDU + inc2Woccp*OCCP + inc2Wedu2*EDU2 + inc2Woccp2*OCCP2
          EDU ~~ eduWoccp*OCCP + eduWedu2*EDU2 + eduWoccp2*OCCP2
          EDU2 ~~ edu2Woccp*OCCP + edu2Woccp2*OCCP2
          OCCP ~~ occpWoccp2*OCCP2
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCP ~~ 1*OCCP
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCP2 ~~ 1*OCCP2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model2b, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2b <- lavaan2RAM(Model2b, obs.variables=c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW"))
RAM2b

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2b <- create.vechsR(A0=RAM2b$A, S0=RAM2b$S)

# Heterogeneity Variance-Covariance Matrix
T2b <- create.Tau2(RAM=RAM2b, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2b <- osmasem(model.name="Model_2B", Mmatrix=M2b, Tmatrix=T2b, data=model2bdf)
summary(mx.fit2b)
# Save Model 2B Summary
sink("Model2best.txt")
print(summary(mx.fit2b))
sink()  
# Tau 2 estimates
Model2bvarcorr<-VarCorr(mx.fit2b)
sink("Model2btaus.txt")
print(Model2bvarcorr)
sink() 
# Residuals
options(max.print = 10000)
Model2bresiduals<-data.frame(mxEval(Smatrix, mx.fit2b$mx.fit))
colnames(Model2bresiduals)<-c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW")
sink("Model2bresiduals.txt")
print(Model2bresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model2bvmatrix<-mxEval(V,mx.fit2b$mx.fit)
sink("Model2bvmatrix.txt")
print(Model2bvmatrix)
sink() 
# Implied R Matrix
Model2brmatrix<-mxEval(impliedR,mx.fit2b$mx.fit)
colnames(Model2brmatrix)<-c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW")
sink("Model2brmatrix.txt")
print(Model2brmatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model2bexpCovmatrix<-mxEval(expCov,mx.fit2b$mx.fit)
sink("Model2bexpCovmatrix.txt")
print(Model2bexpCovmatrix)
sink() 
# Summary with Fit Indices
Model2bsumwfit<-summary(mx.fit2b,fitIndices = TRUE)
sink("Model2bsumwfit.txt")
print(Model2bsumwfit)
sink() 
# SRMR
Model2bsrmr<-osmasemSRMR(mx.fit2b)
sink("Model2bsrmr.txt")
print(Model2bsrmr)
sink() 
# Save osmasem List
save(mx.fit2b,file="mx_fit2b.RData")

### MODEL 2A with Moderators ----
rm(list=ls())
## Sampling covariance matrix of the correlations from Model 2A
load('model2adata.RData')
load("model2adf.RData")
## Add moderators (sex, developmental period) to data set
# Dummy code dev. period with adults as reference group
# children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
# No correlations for children 
model2adata$adol<-ifelse(model2adata$devp==1,1,0)
model2adata$yadu<-ifelse(model2adata$devp==2,1,0)
model2adata$oadu<-ifelse(model2adata$devp==4,1,0)
model2adf$data<- data.frame(model2adf$data, 
                            male=model2adata$male, 
                            adol=model2adata$adol,
                            yadu=model2adata$yadu,
                            oadu=model2adata$oadu,
                            check.names=FALSE)

# Define Model 2a using lavaan syntax 
Model2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2
          INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2
          EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2
          EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2
          OCCS ~~ occsWoccs2*OCCS2
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCS2 ~~ 1*OCCS2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model2a, col="grey")

## MODERATION BY SEX (~45 minutes) ----
# Hypothesis test
# Convert the lavaan syntax into the RAM specification
RAM2a <- lavaan2RAM(Model2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2a

# Define A matrix (paths to be moderated)
Asex<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.male","0*data.male","0*data.male","0*data.male","0*data.male","0*data.male",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2asex <- create.vechsR(A0=RAM2a$A, S0=RAM2a$S, Ax=Asex)

# Heterogeneity Variance-Covariance Matrix
T2a <- create.Tau2(RAM=RAM2a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2abysex <- osmasem(model.name="Model 2A by Sex", Mmatrix=M2asex, Tmatrix=T2a, data=model2adf)
summary(mx.fit2abysex)
# Save Model 2A Summary
sink("Model2abysex.txt")
print(summary(mx.fit2abysex))
sink()  
# Load Model 2a (without moderators for below)
load("mx_fit2a.RData")
# Tau 2 estimates
options(max.print = 10000)
Model2abysextau<-VarCorr(mx.fit2abysex)
sink("Model2abysextaus.txt")
print(Model2abysextau)
sink() 
## Calculate the R2
model2abysexr2<-osmasemR2(mx.fit2abysex, mx.fit2a)
sink("model2abysexr2.txt")
print(model2abysexr2)
sink()
## Compare the models with and without the moderator
model2abysexanova<-anova(mx.fit2abysex, mx.fit2a)
sink("model2abysexanova.txt")
print(model2abysexanova)
sink()
# Residuals
options(max.print = 10000)
model2abysexresiduals<-data.frame(mxEval(Smatrix, mx.fit2abysex$mx.fit))
colnames(model2abysexresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("model2abysexresiduals.txt")
print(model2abysexresiduals)
sink() 
# Save osmasem List
save(mx.fit2abysex,file="mx.fit2abysex.RData")

## MODERATION BY DEVLOPMENTAL PERIOD (8 hours) ----
# Hypothesis test 
# Define A matrix (paths to be moderated)
Aadol<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.adol","0*data.adol","0*data.adol","0*data.adol","0*data.adol","0*data.adol",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Ayadu<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.yadu","0*data.yadu","0*data.yadu","0*data.yadu","0*data.yadu","0*data.yadu",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Aoadu<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.oadu","0*data.oadu","0*data.oadu","0*data.oadu","0*data.oadu","0*data.oadu",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Axdev=list(Aadol,Ayadu,Aoadu)
# Model Implied Correlation Structure
M2adev<-create.vechsR(A0=RAM2a$A,S0=RAM2a$S,Ax=Axdev)

# Heterogeneity Variance-Covariance Matrix
T2a <- create.Tau2(RAM=RAM2a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2abydp <- osmasem(model.name="Model 2A by Dev Perd", Mmatrix=M2adev, Tmatrix=T2a, data=model2adf)
summary(mx.fit2abydp)
# Save Model 2A Summary
sink("Model2abydp.txt")
print(summary(mx.fit2abydp))
sink()  
# Tau 2 estimates
options(max.print = 10000)
Model2abydptau<-VarCorr(mx.fit2abydp)
sink("Model2abydptau.txt")
print(Model2abydptau)
sink() 
## Calculate the R2
model2abydpr2<-osmasemR2(mx.fit2abydp, mx.fit2a)
sink("model2abydpr2.txt")
print(model2abydpr2)
sink()
## Compare the models with and without the moderator
model2abydpanova<-anova(mx.fit2abydp, mx.fit2a)
sink("model2abydpanova.txt")
print(model2abydpanova)
sink()
# Residuals
options(max.print = 10000)
model2abydevpresiduals<-data.frame(mxEval(Smatrix, mx.fit2abydp$mx.fit))
colnames(model2abydevpresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("model2abydevpresiduals.txt")
print(model2abydevpresiduals)
sink() 
# Save osmasem List
save(mx.fit2abydp,file="mx.fit2abydp.RData")

## MODERATION BY RACE/ETHNICITY ----
# Hypothesis test 
# Dummy code race/ethnicity with White samples as the reference group
#(WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
model2adata$lat<-ifelse(model2adata$reth==1,1,0)
model2adata$blk<-ifelse(model2adata$reth==2,1,0)
model2adata$aa<-ifelse(model2adata$reth==3,1,0)
model2adata$ai<-ifelse(model2adata$reth==4,1,0)
model2adata$mr<-ifelse(model2adata$reth==5,1,0)
# Add moderating variables to dataset
model2adf$data<- data.frame(model2adf$data, 
                            lat=model2adata$lat,
                            blk=model2adata$blk,
                            aa=model2adata$aa,
                            ai=model2adata$ai,
                            mr=model2adata$mr,check.names=FALSE)
# Define A matrix (paths to be moderated)
Alat<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Ablk<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Aaa<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Aai<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Amr<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Ax=list(Alat,Ablk,Aaa,Aai,Amr)
# Model Implied Correlation Structure
M2are<-create.vechsR(A0=RAM2a$A,S0=RAM2a$S,Ax=Ax)
mx.fit2abyre <- osmasem(model.name="Model 2A by RE", Mmatrix=M2are, Tmatrix=T2a, data=model2adf)
summary(mx.fit2abyre)
# Save Model 2A Summary
sink("Model2abyre.txt")
print(summary(mx.fit2abyre))
sink()  
# Tau 2 estimates
options(max.print = 10000)
Model2abyretau<-VarCorr(mx.fit2abyre)
sink("Model2abyretau.txt")
print(Model2abyretau)
sink() 
## Calculate the R2
model2abyrer2<-osmasemR2(mx.fit2abyre, mx.fit2a)
sink("model2abyrer2.txt")
print(model2abyrer2)
sink()
## Compare the models with and without the moderator
model2abyreanova<-anova(mx.fit2abyre, mx.fit2a)
sink("model2abyreanova.txt")
print(model2abyreanova)
sink()
# Save osmasem List
save(mx.fit2abyre,file="mx.fit2abyre.RData")

### MODEL 2A among Adolescent Participants (~ 15 minutes) ----
rm(list=ls())
load('adol2adata.RData')
# Sampling covariance matrix of the correlations
adol2adf <- Cor2DataFrame(adol2adata$cordat, adol2adata$n, acov = "weighted")
save(adol2adf, file="adol2adf.RData")
# Define Model 1a using lavaan syntax 
adol2a<-'DEP ~ incd*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errds*DEP'
plot(adol2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aadol <- lavaan2RAM(adol2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aadol

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aadol <- create.vechsR(A0=RAM2aadol$A, S0=RAM2aadol$S)

# Heterogeneity Variance-Covariance Matrix
T2aadol <- create.Tau2(RAM=RAM2aadol, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

adolfit2a <- osmasem(model.name="adol_2A", Mmatrix=M2aadol, Tmatrix=T2aadol, data=adol2adf)
summary(adolfit2a)
# Save Model 2A Summary
sink("adol2aest.txt")
print(summary(adolfit2a))
sink()  
# Residuals
options(max.print = 10000)
adol2aresiduals<-data.frame(mxEval(Smatrix, adolfit2a$mx.fit))
colnames(adol2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adol2aresiduals.txt")
print(adol2aresiduals)
sink() 
# Implied R Matrix
adol2armatrix<-mxEval(impliedR,adolfit2a$mx.fit)
colnames(adol2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adol2armatrix.txt")
print(adol2armatrix)
sink() 
# Summary with Fit Indices
adol2asumwfit<-summary(adolfit2a,fitIndices = TRUE)
sink("adol2asumwfit.txt")
print(adol2asumwfit)
sink() 
# SRMR
adol2asrmr<-osmasemSRMR(adolfit2a)
sink("adol2asrmr.txt")
print(adol2asrmr)
sink() 
# Save osmasem List
save(adolfit2a,file="adolfit2a.RData")

### MODEL 2A among Young Adult Participants (~ 15 minutes) ----
rm(list=ls())
load('yadu2adata.RData')
# Sampling covariance matrix of the correlations
yadu2adf <- Cor2DataFrame(yadu2adata$cordat, yadu2adata$n, acov = "weighted")
save(yadu2adf, file="yadu2adf.RData")
# Define Model 1a using lavaan syntax 
yadu2a<-'DEP ~ incd*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errds*DEP'
plot(yadu2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2ayadu <- lavaan2RAM(yadu2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2ayadu

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2ayadu <- create.vechsR(A0=RAM2ayadu$A, S0=RAM2ayadu$S)

# Heterogeneity Variance-Covariance Matrix
T2ayadu <- create.Tau2(RAM=RAM2ayadu, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

yadufit2a <- osmasem(model.name="yadu_2A", Mmatrix=M2ayadu, Tmatrix=T2ayadu, data=yadu2adf)
summary(yadufit2a)
# Save Model 2A Summary
sink("yadu2aest.txt")
print(summary(yadufit2a))
sink()  
# Residuals
options(max.print = 10000)
yadu2aresiduals<-data.frame(mxEval(Smatrix, yadufit2a$mx.fit))
colnames(yadu2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("yadu2aresiduals.txt")
print(yadu2aresiduals)
sink() 
# Implied R Matrix
yadu2armatrix<-mxEval(impliedR,yadufit2a$mx.fit)
colnames(yadu2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("yadu2armatrix.txt")
print(yadu2armatrix)
sink() 
# Summary with Fit Indices
yadu2asumwfit<-summary(yadufit2a,fitIndices = TRUE)
sink("yadu2asumwfit.txt")
print(yadu2asumwfit)
sink() 
# SRMR
yadu2asrmr<-osmasemSRMR(yadufit2a)
sink("yadu2asrmr.txt")
print(yadu2asrmr)
sink() 
# Save osmasem List
save(yadufit2a,file="yadufit2a.RData")

### MODEL 2A among Middle-Aged Adult Participants (~ 20 minutes) ----
rm(list=ls())
load('adu2adata.RData')
# Sampling covariance matrix of the correlations
adu2adf <- Cor2DataFrame(adu2adata$cordat, adu2adata$n, acov = "weighted")
save(adu2adf, file="adu2adf.RData")
# Define Model 1a using lavaan syntax 
adu2a<-'DEP ~ incd*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errds*DEP'
plot(adu2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aadu <- lavaan2RAM(adu2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aadu

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aadu <- create.vechsR(A0=RAM2aadu$A, S0=RAM2aadu$S)

# Heterogeneity Variance-Covariance Matrix
T2aadu <- create.Tau2(RAM=RAM2aadu, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

adufit2a <- osmasem(model.name="adu_2A", Mmatrix=M2aadu, Tmatrix=T2aadu, data=adu2adf)
summary(adufit2a)
# Save Model 2A Summary
sink("adu2aest.txt")
print(summary(adufit2a))
sink()  
# Residuals
options(max.print = 10000)
adu2aresiduals<-data.frame(mxEval(Smatrix, adufit2a$mx.fit))
colnames(adu2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adu2aresiduals.txt")
print(adu2aresiduals)
sink() 
# Implied R Matrix
adu2armatrix<-mxEval(impliedR,adufit2a$mx.fit)
colnames(adu2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adu2armatrix.txt")
print(adu2armatrix)
sink() 
# Summary with Fit Indices
adu2asumwfit<-summary(adufit2a,fitIndices = TRUE)
sink("adu2asumwfit.txt")
print(adu2asumwfit)
sink() 
# SRMR
adu2asrmr<-osmasemSRMR(adufit2a)
sink("adu2asrmr.txt")
print(adu2asrmr)
sink() 
# Save osmasem List
save(adufit2a,file="adufit2a.RData")

### MODEL 2A among Older Adult Participants (~ 15 minutes) ----
rm(list=ls())
load('oadu2adata.RData')
# Sampling covariance matrix of the correlations
oadu2adf <- Cor2DataFrame(oadu2adata$cordat, oadu2adata$n, acov = "weighted")
save(oadu2adf, file="oadu2adf.RData")
# Define Model 1a using lavaan syntax 
oadu2a<-'DEP ~ incd*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errds*DEP'
plot(oadu2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aoadu <- lavaan2RAM(oadu2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aoadu

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aoadu <- create.vechsR(A0=RAM2aoadu$A, S0=RAM2aoadu$S)

# Heterogeneity Variance-Covariance Matrix
T2aoadu <- create.Tau2(RAM=RAM2aoadu, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

oadufit2a <- osmasem(model.name="oadu_2A", Mmatrix=M2aoadu, Tmatrix=T2aoadu, data=oadu2adf)
summary(oadufit2a)
# Save Model 2A Summary
sink("oadu2aest.txt")
print(summary(oadufit2a))
sink()  
# Residuals
options(max.print = 10000)
oadu2aresiduals<-data.frame(mxEval(Smatrix, oadufit2a$mx.fit))
colnames(oadu2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("oadu2aresiduals.txt")
print(oadu2aresiduals)
sink() 
# Implied R Matrix
oadu2armatrix<-mxEval(impliedR,oadufit2a$mx.fit)
colnames(oadu2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("oadu2armatrix.txt")
print(oadu2armatrix)
sink() 
# Summary with Fit Indices
oadu2asumwfit<-summary(oadufit2a,fitIndices = TRUE)
sink("oadu2asumwfit.txt")
print(oadu2asumwfit)
sink() 
# SRMR
oadu2asrmr<-osmasemSRMR(oadufit2a)
sink("oadu2asrmr.txt")
print(oadu2asrmr)
sink() 
# Save osmasem List
save(oadufit2a,file="oadufit2a.RData")

### MODEL 2A among White Participants (~ 30 minutes) ----
rm(list=ls())
load('Wht2adata.RData')
# Sampling covariance matrix of the correlations
Wht2adf <- Cor2DataFrame(Wht2adata$cordat, Wht2adata$n, acov = "weighted")
save(Wht2adf, file="Wht2adf.RData")
# Define Model 1a using lavaan syntax 
Wht2a<-'DEP ~ incd*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errds*DEP'
plot(Wht2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2awht <- lavaan2RAM(Wht2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2awht

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2awht <- create.vechsR(A0=RAM2awht$A, S0=RAM2awht$S)

# Heterogeneity Variance-Covariance Matrix
T2awht <- create.Tau2(RAM=RAM2awht, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Whtfit2a <- osmasem(model.name="Wht_2A", Mmatrix=M2awht, Tmatrix=T2awht, data=Wht2adf)
summary(Whtfit2a)
# Save Model 2A Summary
sink("Wht2aest.txt")
print(summary(Whtfit2a))
sink()  
# Residuals
options(max.print = 10000)
Wht2aresiduals<-data.frame(mxEval(Smatrix, Whtfit2a$mx.fit))
colnames(Wht2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Wht2aresiduals.txt")
print(Wht2aresiduals)
sink() 
# Implied R Matrix
Wht2armatrix<-mxEval(impliedR,Whtfit2a$mx.fit)
colnames(Wht2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Wht2armatrix.txt")
print(Wht2armatrix)
sink() 
# Summary with Fit Indices
Wht2asumwfit<-summary(Whtfit2a,fitIndices = TRUE)
sink("Wht2asumwfit.txt")
print(Wht2asumwfit)
sink() 
# SRMR
Wht2asrmr<-osmasemSRMR(Whtfit2a)
sink("Wht2asrmr.txt")
print(Wht2asrmr)
sink() 
# Save osmasem List
save(Whtfit2a,file="Whtfit2a.RData")


### MODEL 2A among Latinx Participants (< 30 minutes) ----
rm(list=ls())
load('Lat2adata.RData')
# Sampling covariance matrix of the correlations
Lat2adf <- Cor2DataFrame(Lat2adata$cordat, Lat2adata$n, acov = "weighted")
save(Lat2adf, file="Lat2adf.RData")
# Define Model 1a using lavaan syntax 
Lat2a<-'DEP ~ ind*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(Lat2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2alat <- lavaan2RAM(Lat2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2alat

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2alat <- create.vechsR(A0=RAM2alat$A, S0=RAM2alat$S)

# Heterogeneity Variance-Covariance Matrix
T2alat <- create.Tau2(RAM=RAM2alat, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Latfit2a <- osmasem(model.name="Lat_2A", Mmatrix=M2alat, Tmatrix=T2alat, data=Lat2adf)
summary(Latfit2a)
# Save Model 2A Summary
sink("Lat2aest.txt")
print(summary(Latfit2a))
sink()  
# Residuals
options(max.print = 10000)
Lat2aresiduals<-data.frame(mxEval(Smatrix, Latfit2a$mx.fit))
colnames(Lat2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Lat2aresiduals.txt")
print(Lat2aresiduals)
sink() 
# Implied R Matrix
Lat2armatrix<-mxEval(impliedR,Latfit2a$mx.fit)
colnames(Lat2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Lat2armatrix.txt")
print(Lat2armatrix)
sink() 
# Summary with Fit Indices
Lat2asumwfit<-summary(Latfit2a,fitIndices = TRUE)
sink("Lat2asumwfit.txt")
print(Lat2asumwfit)
sink() 
# SRMR
Lat2asrmr<-osmasemSRMR(Latfit2a)
sink("Lat2asrmr.txt")
print(Lat2asrmr)
sink() 
# Save osmasem List
save(Latfit2a,file="Latfit2a.RData")

### MODEL 2A among Black Participants (< 30 minutes) ----
rm(list=ls())
load('Blk2adata.RData')
# Sampling covariance matrix of the correlations
Blk2adf <- Cor2DataFrame(Blk2adata$cordat, Blk2adata$n, acov = "weighted")
save(Blk2adf, file="Blk2adf.RData")
# Define Model 1a using lavaan syntax 
Blk2a<-'DEP ~ ind*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(Blk2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2ablk <- lavaan2RAM(Blk2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2ablk

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2ablk <- create.vechsR(A0=RAM2ablk$A, S0=RAM2ablk$S)

# Heterogeneity Variance-Covariance Matrix
T2ablk <- create.Tau2(RAM=RAM2ablk, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Blkfit2a <- osmasem(model.name="Blk_2A", Mmatrix=M2ablk, Tmatrix=T2ablk, data=Blk2adf)
summary(Blkfit2a)
# Save Model 2A Summary
sink("Blk2aest.txt")
print(summary(Blkfit2a))
sink()  
# Residuals
options(max.print = 10000)
Blk2aresiduals<-data.frame(mxEval(Smatrix, Blkfit2a$mx.fit))
colnames(Blk2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Blk2aresiduals.txt")
print(Blk2aresiduals)
sink() 
# Implied R Matrix
Blk2armatrix<-mxEval(impliedR,Blkfit2a$mx.fit)
colnames(Blk2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Blk2armatrix.txt")
print(Blk2armatrix)
sink() 
# Summary with Fit Indices
Blk2asumwfit<-summary(Blkfit2a,fitIndices = TRUE)
sink("Blk2asumwfit.txt")
print(Blk2asumwfit)
sink() 
# SRMR
Blk2asrmr<-osmasemSRMR(Blkfit2a)
sink("Blk2asrmr.txt")
print(Blk2asrmr)
sink() 
# Save osmasem List
save(Blkfit2a,file="Blkfit2a.RData")

### MODEL 2A among Asian American Participants (< 15 minutes) ----
rm(list=ls())
load('Aa2adata.RData')
# Sampling covariance matrix of the correlations
Aadep2adf <- Cor2DataFrame(Aadep2adata$cordat, Aadep2adata$n, acov = "weighted")
save(Aadep2adf, file="Aadep2adf.RData")
# Define Model 1a using lavaan syntax 
Aadep2a<-'DEP ~ ind*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
          INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
          INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
          EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
          EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
          OCCS ~~ osWos2*OCCS2
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCS2 ~~ 1*OCCS2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Aadep2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aaa <- lavaan2RAM(Aadep2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aaa

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aaa <- create.vechsR(A0=RAM2aaa$A, S0=RAM2aaa$S)

# Heterogeneity Variance-Covariance Matrix
T2aaa <- create.Tau2(RAM=RAM2aaa, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Aadepfit2a <- osmasem(model.name="AA_2A", Mmatrix=M2aaa, Tmatrix=T2aaa, data=Aadep2adf)
summary(Aadepfit2a)
# Save Model 2A Summary
sink("Aadep2aest.txt")
print(summary(Aadepfit2a))
sink()  
# Residuals
options(max.print = 10000)
Aadep2aresiduals<-data.frame(mxEval(Smatrix, Aadepfit2a$mx.fit))
colnames(Aadep2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Aadep2aresiduals.txt")
print(Aadep2aresiduals)
sink() 
# Implied R Matrix
Aadep2armatrix<-mxEval(impliedR,Aadepfit2a$mx.fit)
colnames(Aadep2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Aadep2armatrix.txt")
print(Aadep2armatrix)
sink() 
# Summary with Fit Indices
Aadep2asumwfit<-summary(Aadepfit2a,fitIndices = TRUE)
sink("Aadep2asumwfit.txt")
print(Aadep2asumwfit)
sink() 
# SRMR
Aadep2asrmr<-osmasemSRMR(Aadepfit2a)
sink("Aadep2asrmr.txt")
print(Aadep2asrmr)
sink() 
# Save osmasem List
save(Aadepfit2a,file="Aadepfit2a.RData")

### MODEL 2A among Native American Participants (< 15 minutes) ----
rm(list=ls())
load('Ai2adata.RData')
# Sampling covariance matrix of the correlations
Ai2adf <- Cor2DataFrame(Ai2adata$cordat, Ai2adata$n, acov = "weighted")
save(Ai2adf, file="Ai2adf.RData")
# Define Model 1a using lavaan syntax 
Ai2a<-'DEP ~ ind*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(Ai2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aai <- lavaan2RAM(Ai2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aai

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aai <- create.vechsR(A0=RAM2aai$A, S0=RAM2aai$S)

# Heterogeneity Variance-Covariance Matrix
T2aai <- create.Tau2(RAM=RAM2aai, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Aifit2a <- osmasem(model.name="AI_2A", Mmatrix=M2aai, Tmatrix=T2aai, data=Ai2adf)
summary(Aifit2a)
# Save Model 2A Summary
sink("Ai2aest.txt")
print(summary(Aifit2a))
sink()  
# Residuals
options(max.print = 10000)
Ai2aresiduals<-data.frame(mxEval(Smatrix, Aifit2a$mx.fit))
colnames(Ai2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Ai2aresiduals.txt")
print(Ai2aresiduals)
sink() 
# Implied R Matrix
Ai2armatrix<-mxEval(impliedR,Aifit2a$mx.fit)
colnames(Ai2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Ai2armatrix.txt")
print(Ai2armatrix)
sink() 
# Summary with Fit Indices
Ai2asumwfit<-summary(Aifit2a,fitIndices = TRUE)
sink("Ai2asumwfit.txt")
print(Ai2asumwfit)
sink() 
# SRMR
Ai2asrmr<-osmasemSRMR(Aifit2a)
sink("Ai2asrmr.txt")
print(Ai2asrmr)
sink() 
# Save osmasem List
save(Aifit2a,file="Aifit2a.RData")

### MODEL 2A among Multiracial Participants (20 mins) ----
rm(list=ls())
load('Mr2adata.RData')
# Sampling covariance matrix of the correlations
Mr2adf <- Cor2DataFrame(Mr2adata$cordat, Mr2adata$n, acov = "weighted")
save(Mr2adf, file="Mr2adf.RData")
# Define Model 1a using lavaan syntax 
Mr2a<-'DEP ~ ind*INC + edd*EDU + osd*OCCS + in2d*INC2 + ed2d*EDU2 + os2d*OCCS2 + dspd*DEPPW
        INC ~~ inWed*EDU + inWos*OCCS + inWin2*INC2 + inWed2*EDU2 + inWos2*OCCS2
        INC2 ~~ in2Wed*EDU + in2Wos*OCCS + in2Wed2*EDU2 + in2Wos2*OCCS2
        EDU ~~ edWos*OCCS + edWed2*EDU2 + edWos2*OCCS2
        EDU2 ~~ ed2Wos*OCCS + ed2Wos2*OCCS2
        OCCS ~~ osWos2*OCCS2
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errds*DEP'
plot(Mr2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2amr <- lavaan2RAM(Mr2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2amr

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2amr <- create.vechsR(A0=RAM2amr$A, S0=RAM2amr$S)

# Heterogeneity Variance-Covariance Matrix
T2amr <- create.Tau2(RAM=RAM2amr, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Mrfit2a <- osmasem(model.name="MR_2A", Mmatrix=M2amr, Tmatrix=T2amr, data=Mr2adf)
summary(Mrfit2a)
# Save Model 2A Summary
sink("Mr2aest.txt")
print(summary(Mrfit2a))
sink()  
# Residuals
options(max.print = 10000)
Mr2aresiduals<-data.frame(mxEval(Smatrix, Mrfit2a$mx.fit))
colnames(Mr2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Mr2aresiduals.txt")
print(Mr2aresiduals)
sink() 
# Implied R Matrix
Mr2armatrix<-mxEval(impliedR,Mrfit2a$mx.fit)
colnames(Mr2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Mr2armatrix.txt")
print(Mr2armatrix)
sink() 
# Summary with Fit Indices
Mr2asumwfit<-summary(Mrfit2a,fitIndices = TRUE)
sink("Mr2asumwfit.txt")
print(Mr2asumwfit)
sink() 
# SRMR
Mr2asrmr<-osmasemSRMR(Mrfit2a)
sink("Mr2asrmr.txt")
print(Mr2asrmr)
sink() 
# Save osmasem List
save(Mrfit2a,file="Mrfit2a.RData")

#### MODELS WITH CORRELATIONS BETWEEN SES COMPONENTS AND PRIOR DEPRESSIVE SYMPTOMS ----
# Advised by Reviewer
### MODEL 1A ----
load('model1adata.RData')
# Display the number of correlations
model1ancorrs<-matrix(pattern.na(model1adata$cordat,show.na=FALSE),ncol=5,nrow=5,
                      dimnames = list(c("INC","EDU","OCCS","DEP","DEPPW"),
                                      c("INC","EDU","OCCS","DEP","DEPPW")))
write.csv(model1ancorrs,file='model1ancorrs.csv')
# Display the accumulative sample size for each correlation
model1ans<-matrix(pattern.n(model1adata$cordat,model1adata$n),ncol=5,nrow=5,
                  dimnames = list(c("INC","EDU","OCCS","DEP","DEPPW"),
                                  c("INC","EDU","OCCS","DEP","DEPPW")))
write.csv(model1ans,file='model1ans.csv')
# Sampling covariance matrix of the correlations
model1adf <- Cor2DataFrame(model1adata$cordat, model1adata$n, acov = "weighted")
save(model1adf, file="model1adf.RData")
# Define Model 1a using lavaan syntax 
Model1a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS + incWdeppw*DEPPW
          EDU ~~ eduWoccs*OCCS + eduWdeppw*DEPPW
          OCCS ~~ occsWdeppw*DEPPW
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model1a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM1a <- lavaan2RAM(Model1a, obs.variables=c("INC","EDU","OCCS","DEP","DEPPW"))
RAM1a

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M1a <- create.vechsR(A0=RAM1a$A, S0=RAM1a$S)

# Heterogeneity Variance-Covariance Matrix
T1a <- create.Tau2(RAM=RAM1a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit1a <- osmasem(model.name="Model_1A", Mmatrix=M1a, Tmatrix=T1a, data=model1adf, intervals.type = 'z',intervals=T)
summary(mx.fit1a)
# Save Model 1A Summary
sink("model1aest.txt")
print(summary(mx.fit1a))
sink()  
# Tau 2 estimates
model1avarcorr<-VarCorr(mx.fit1a)
sink("model1ataus.txt")
print(model1avarcorr)
sink() 
# Residuals
Model1aresiduals<-data.frame(mxEval(Smatrix, mx.fit1a$mx.fit))
colnames(Model1aresiduals)<-c("INC","EDU","OCCS","DEP","DEPPW")
sink("Model1aresiduals.txt")
print(Model1aresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model1avmatrix<-mxEval(V,mx.fit1a$mx.fit)
sink("Model1avmatrix.txt")
print(Model1avmatrix)
sink() 
# Implied R Matrix
Model1armatrix<-mxEval(impliedR,mx.fit1a$mx.fit)
colnames(Model1armatrix)<-c("INC","EDU","OCCS","DEP","DEPPW")
sink("Model1armatrix.txt")
print(Model1armatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model1aexpCovmatrix<-mxEval(expCov,mx.fit1a$mx.fit)
sink("Model1aexpCovmatrix.txt")
print(Model1aexpCovmatrix)
sink() 
## Summary with Fit Indices
#Model1asumwfit<-summary(mx.fit1a,fitIndices = TRUE)
#sink("model1aestwfit.txt")
#print(Model1asumwfit)
#sink() 
## SRMR
#Model1asrmr<-osmasemSRMR(mx.fit1a)
#sink("model1asrmr.txt")
#print(Model1asrmr)
#sink() 
# Save osmasem List
save(mx.fit1a,file="mx_fit1a.RData")

### MODEL 1B ----
load('model1bdata.RData')
# Display the number of correlations
model1bncorrs<-matrix(pattern.na(model1bdata$cordat,show.na=FALSE),ncol=5,nrow=5,
                      dimnames = list(c("INC","EDU","OCCP","DEP","DEPPW"),
                                      c("INC","EDU","OCCP","DEP","DEPPW")))
write.csv(model1bncorrs,file='model1bncorrs.csv')
# Display the accumulative sample size for each correlation
model1bns<-matrix(pattern.n(model1bdata$cordat,model1bdata$n),ncol=5,nrow=5,
                  dimnames = list(c("INC","EDU","OCCP","DEP","DEPPW"),
                                  c("INC","EDU","OCCP","DEP","DEPPW")))
write.csv(model1bns,file='model1bns.csv')
# Sampling covariance matrix of the correlations
model1bdf <- Cor2DataFrame(model1bdata$cordat, model1bdata$n, acov = "weighted")
save(model1bdf, file="model1bdf.RData")
# Define Model 1a using lavaan syntax 
Model1b<-'DEP ~ inctodep*INC + edutodep*EDU + occptodep*OCCP + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccp*OCCP + incWdeppw*DEPPW
          EDU ~~ eduWoccp*OCCP + eduWdeppw*DEPPW
          OCCP ~~ occpWdeppw*DEPPW
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCP ~~ 1*OCCP
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model1b, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM1b <- lavaan2RAM(Model1b, obs.variables=c("INC","EDU","OCCP","DEP","DEPPW"))
RAM1b

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M1b <- create.vechsR(A0=RAM1b$A, S0=RAM1b$S)

# Heterogeneity Variance-Covariance Matrix
T1b <- create.Tau2(RAM=RAM1b, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit1b <- osmasem(model.name="Model_1B", Mmatrix=M1b, Tmatrix=T1b, data=model1bdf)
summary(mx.fit1b)
# Save Model 1A Summary
sink("model1best.txt")
print(summary(mx.fit1b))
sink()  
# Tau 2 estimates
model1bvarcorr<-VarCorr(mx.fit1b)
sink("model1btaus.txt")
print(model1bvarcorr)
sink() 
#Residuals
Model1bresiduals<-data.frame(mxEval(Smatrix, mx.fit1b$mx.fit))
colnames(Model1bresiduals)<-c("INC","EDU","OCCP","DEP","DEPPW")
sink("Model1bresiduals.txt")
print(Model1bresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model1bvmatrix<-mxEval(V,mx.fit1b$mx.fit)
sink("Model1bvmatrix.txt")
print(Model1bvmatrix)
sink() 
# Implied R Matrix
Model1brmatrix<-mxEval(impliedR,mx.fit1b$mx.fit)
colnames(Model1brmatrix)<-c("INC","EDU","OCCP","DEP","DEPPW")
sink("Model1brmatrix.txt")
print(Model1brmatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model1bexpCovmatrix<-mxEval(expCov,mx.fit1b$mx.fit)
sink("Model1bexpCovmatrix.txt")
print(Model1bexpCovmatrix)
sink() 
## Summary with Fit Indices
#Model1bsumwfit<-summary(mx.fit1b,fitIndices = TRUE)
#sink("model1bestwfit.txt")
#print(Model1bsumwfit)
#sink() 
## SRMR
#Model1bsrmr<-osmasemSRMR(mx.fit1b)
#sink("model1bsrmr.txt")
#print(Model1bsrmr)
#sink() 
# Save osmasem List
save(mx.fit1b,file="mx_fit1b.RData")

### MODEL 2A (45-90 minutes) ----
rm(list=ls())
load('model2adata.RData')
# Display the number of correlations
model2ancorrs<-matrix(pattern.na(model2adata$cordat,show.na=FALSE),ncol=8,nrow=8,
                      dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                                      c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")))
write.csv(model2ancorrs,file='model2ancorrs.csv')
# Display the accumulative sample size for each correlation
model2ans<-matrix(pattern.n(model2adata$cordat,model2adata$n),ncol=8,nrow=8,
                  dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                                  c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")))
write.csv(model2ans,file='model2ans.csv')
# Sampling covariance matrix of the correlations
model2adf <- Cor2DataFrame(model2adata$cordat, model2adata$n, acov = "weighted")
save(model2adf, file="model2adf.RData")
# Define Model 1a using lavaan syntax 
Model2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
          INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
          EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
          EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
          OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
          OCCS2 ~~ occs2Wdeppw*DEPPW
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCS2 ~~ 1*OCCS2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2a <- lavaan2RAM(Model2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2a

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2a <- create.vechsR(A0=RAM2a$A, S0=RAM2a$S)

# Heterogeneity Variance-Covariance Matrix
T2a <- create.Tau2(RAM=RAM2a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2a <- osmasem(model.name="Model_2A", Mmatrix=M2a, Tmatrix=T2a, data=model2adf)
summary(mx.fit2a)
# Save Model 2A Summary
sink("Model2aest.txt")
print(summary(mx.fit2a))
sink()  
# Tau 2 estimates
options(max.print = 10000)
Model2avarcorr<-VarCorr(mx.fit2a)
sink("Model2ataus.txt")
print(Model2avarcorr)
sink() 
# Residuals
options(max.print = 10000)
Model2aresiduals<-data.frame(mxEval(Smatrix, mx.fit2a$mx.fit))
colnames(Model2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Model2aresiduals.txt")
print(Model2aresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model2avmatrix<-mxEval(V,mx.fit2a$mx.fit)
sink("Model2avmatrix.txt")
print(Model2avmatrix)
sink() 
# Implied R Matrix
Model2armatrix<-mxEval(impliedR,mx.fit2a$mx.fit)
colnames(Model2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Model2armatrix.txt")
print(Model2armatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model2aexpCovmatrix<-mxEval(expCov,mx.fit2a$mx.fit)
sink("Model2aexpCovmatrix.txt")
print(Model2aexpCovmatrix)
sink() 
## Summary with Fit Indices
#Model2asumwfit<-summary(mx.fit2a,fitIndices = TRUE)
#sink("Model2asumwfit.txt")
#print(Model2asumwfit)
#sink() 
## SRMR
#Model2asrmr<-osmasemSRMR(mx.fit2a)
#sink("Model2asrmr.txt")
#print(Model2asrmr)
#sink() 
# Save osmasem List
save(mx.fit2a,file="mx_fit2a.RData")

### MODEL 2B (45-90 minutes) ----
rm(list=ls())
load('model2bdata.RData')
# Sampling covariance matrix of the correlations
model2bdf <- Cor2DataFrame(model2bdata$cordat, model2bdata$n, acov = "weighted")
save(model2bdf, file="model2bdf.RData")
# Define Model 1a using lavaan syntax 
Model2b<-'DEP ~ inutodep*INC + edutodep*EDU + occptodep*OCCP + inc2todep*INC2 + edu2todep*EDU2 + occp2todep*OCCP2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccp*OCCP + incWinc2*INC2 + incWedu2*EDU2 + incWoccp2*OCCP2 + incWdeppw*DEPPW
          INC2 ~~ icn2Wedu*EDU + inc2Woccp*OCCP + inc2Wedu2*EDU2 + inc2Woccp2*OCCP2 + inc2Wdeppw*DEPPW
          EDU ~~ eduWoccp*OCCP + eduWedu2*EDU2 + eduWoccp2*OCCP2 + eduWdeppw*DEPPW
          EDU2 ~~ edu2Woccp*OCCP + edu2Woccp2*OCCP2 + edu2Wdeppw*DEPPW
          OCCP ~~ occpWoccp2*OCCP2 + occpWdeppw*DEPPW
          OCCP2 ~~ occp2Wdeppw*DEPPW
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCP ~~ 1*OCCP
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCP2 ~~ 1*OCCP2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model2b, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2b <- lavaan2RAM(Model2b, obs.variables=c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW"))
RAM2b

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2b <- create.vechsR(A0=RAM2b$A, S0=RAM2b$S)

# Heterogeneity Variance-Covariance Matrix
T2b <- create.Tau2(RAM=RAM2b, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2b <- osmasem(model.name="Model_2B", Mmatrix=M2b, Tmatrix=T2b, data=model2bdf)
summary(mx.fit2b)
# Save Model 2B Summary
sink("Model2best.txt")
print(summary(mx.fit2b))
sink()  
# Tau 2 estimates
Model2bvarcorr<-VarCorr(mx.fit2b)
sink("Model2btaus.txt")
print(Model2bvarcorr)
sink() 
# Residuals
options(max.print = 10000)
Model2bresiduals<-data.frame(mxEval(Smatrix, mx.fit2b$mx.fit))
colnames(Model2bresiduals)<-c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW")
sink("Model2bresiduals.txt")
print(Model2bresiduals)
sink() 
# V Matrix of Sampling Covariances 
options(max.print = 10000)
Model2bvmatrix<-mxEval(V,mx.fit2b$mx.fit)
sink("Model2bvmatrix.txt")
print(Model2bvmatrix)
sink() 
# Implied R Matrix
Model2brmatrix<-mxEval(impliedR,mx.fit2b$mx.fit)
colnames(Model2brmatrix)<-c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW")
sink("Model2brmatrix.txt")
print(Model2brmatrix)
sink() 
# Expected Covariance Matrix
options(max.print = 10000)
Model2bexpCovmatrix<-mxEval(expCov,mx.fit2b$mx.fit)
sink("Model2bexpCovmatrix.txt")
print(Model2bexpCovmatrix)
sink() 
## Summary with Fit Indices
#Model2bsumwfit<-summary(mx.fit2b,fitIndices = TRUE)
#sink("Model2bsumwfit.txt")
#print(Model2bsumwfit)
#sink() 
## SRMR
#Model2bsrmr<-osmasemSRMR(mx.fit2b)
#sink("Model2bsrmr.txt")
#print(Model2bsrmr)
#sink() 
# Save osmasem List
save(mx.fit2b,file="mx_fit2b.RData")

### MODEL 2A with Moderators ----
rm(list=ls())
## Sampling covariance matrix of the correlations from Model 2A
load('model2adata.RData')
load("model2adf.RData")
## Add moderators (sex, developmental period) to data set
# Dummy code dev. period with adults as reference group
# children(5-11;0), adolescents(12-18;1), young adults(18-25;2), adults(26-64;3), older adults(65+;4)
# No correlations for children 
model2adata$adol<-ifelse(model2adata$devp==1,1,0)
model2adata$yadu<-ifelse(model2adata$devp==2,1,0)
model2adata$oadu<-ifelse(model2adata$devp==4,1,0)
model2adf$data<- data.frame(model2adf$data, 
                            male=model2adata$male, 
                            adol=model2adata$adol,
                            yadu=model2adata$yadu,
                            oadu=model2adata$oadu,
                            check.names=FALSE)

# Define Model 2a using lavaan syntax 
Model2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
          INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
          EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
          EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
          OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
          OCCS2 ~~ occs2Wdeppw*DEPPW
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCS2 ~~ 1*OCCS2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Model2a, col="grey")

## MODERATION BY SEX (~45 minutes) ----
# Hypothesis test
# Convert the lavaan syntax into the RAM specification
RAM2a <- lavaan2RAM(Model2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2a

# Define A matrix (paths to be moderated)
Asex<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.male","0*data.male","0*data.male","0*data.male","0*data.male","0*data.male",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2asex <- create.vechsR(A0=RAM2a$A, S0=RAM2a$S, Ax=Asex)

# Heterogeneity Variance-Covariance Matrix
T2a <- create.Tau2(RAM=RAM2a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2abysex <- osmasem(model.name="Model 2A by Sex", Mmatrix=M2asex, Tmatrix=T2a, data=model2adf)
summary(mx.fit2abysex)
# Save Model 2A Summary
sink("Model2abysex.txt")
print(summary(mx.fit2abysex))
sink()  
# Load Model 2a (without moderators for below)
load("mx_fit2a.RData")
# Tau 2 estimates
options(max.print = 10000)
Model2abysextau<-VarCorr(mx.fit2abysex)
sink("Model2abysextaus.txt")
print(Model2abysextau)
sink() 
## Calculate the R2
model2abysexr2<-osmasemR2(mx.fit2abysex, mx.fit2a)
sink("model2abysexr2.txt")
print(model2abysexr2)
sink()
## Compare the models with and without the moderator
model2abysexanova<-anova(mx.fit2abysex, mx.fit2a)
sink("model2abysexanova.txt")
print(model2abysexanova)
sink()
# Residuals
options(max.print = 10000)
model2abysexresiduals<-data.frame(mxEval(Smatrix, mx.fit2abysex$mx.fit))
colnames(model2abysexresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("model2abysexresiduals.txt")
print(model2abysexresiduals)
sink() 
# Save osmasem List
save(mx.fit2abysex,file="mx.fit2abysex.RData")

## MODERATION BY DEVLOPMENTAL PERIOD (8 hours) ----
# Hypothesis test 
# Define A matrix (paths to be moderated)
Aadol<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.adol","0*data.adol","0*data.adol","0*data.adol","0*data.adol","0*data.adol",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Ayadu<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.yadu","0*data.yadu","0*data.yadu","0*data.yadu","0*data.yadu","0*data.yadu",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Aoadu<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.oadu","0*data.oadu","0*data.oadu","0*data.oadu","0*data.oadu","0*data.oadu",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Axdev=list(Aadol,Ayadu,Aoadu)
# Model Implied Correlation Structure
M2adev<-create.vechsR(A0=RAM2a$A,S0=RAM2a$S,Ax=Axdev)

# Heterogeneity Variance-Covariance Matrix
T2a <- create.Tau2(RAM=RAM2a, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

mx.fit2abydp <- osmasem(model.name="Model 2A by Dev Perd", Mmatrix=M2adev, Tmatrix=T2a, data=model2adf)
summary(mx.fit2abydp)
# Save Model 2A Summary
sink("Model2abydp.txt")
print(summary(mx.fit2abydp))
sink()  
# Tau 2 estimates
options(max.print = 10000)
Model2abydptau<-VarCorr(mx.fit2abydp)
sink("Model2abydptau.txt")
print(Model2abydptau)
sink() 
## Calculate the R2
model2abydpr2<-osmasemR2(mx.fit2abydp, mx.fit2a)
sink("model2abydpr2.txt")
print(model2abydpr2)
sink()
## Compare the models with and without the moderator
model2abydpanova<-anova(mx.fit2abydp, mx.fit2a)
sink("model2abydpanova.txt")
print(model2abydpanova)
sink()
# Residuals
options(max.print = 10000)
model2abydevpresiduals<-data.frame(mxEval(Smatrix, mx.fit2abydp$mx.fit))
colnames(model2abydevpresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("model2abydevpresiduals.txt")
print(model2abydevpresiduals)
sink() 
# Save osmasem List
save(mx.fit2abydp,file="mx.fit2abydp.RData")

## MODERATION BY RACE/ETHNICITY ----
# Hypothesis test 
# Dummy code race/ethnicity with White samples as the reference group
#(WHT=0,LAT=1,BLK=2,AA=3,AI=4,MR=5)
model2adata$lat<-ifelse(model2adata$reth==1,1,0)
model2adata$blk<-ifelse(model2adata$reth==2,1,0)
model2adata$aa<-ifelse(model2adata$reth==3,1,0)
model2adata$ai<-ifelse(model2adata$reth==4,1,0)
model2adata$mr<-ifelse(model2adata$reth==5,1,0)
# Add moderating variables to dataset
model2adf$data<- data.frame(model2adf$data, 
                            lat=model2adata$lat,
                            blk=model2adata$blk,
                            aa=model2adata$aa,
                            ai=model2adata$ai,
                            mr=model2adata$mr,check.names=FALSE)
# Define A matrix (paths to be moderated)
Alat<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat","0*data.lat",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Ablk<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk","0*data.blk",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Aaa<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa","0*data.aa",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Aai<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai","0*data.ai",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Amr<-matrix(c(0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             0,0,0,0,0,0,0,0,
             "0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr",0,0,
             0,0,0,0,0,0,0,0,
             "0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr","0*data.mr",0,0,
             0,0,0,0,0,0,0,0),
             nrow=8, ncol=8, byrow=TRUE)
Ax=list(Alat,Ablk,Aaa,Aai,Amr)
# Model Implied Correlation Structure
M2are<-create.vechsR(A0=RAM2a$A,S0=RAM2a$S,Ax=Ax)
mx.fit2abyre <- osmasem(model.name="Model 2A by RE", Mmatrix=M2are, Tmatrix=T2a, data=model2adf)
summary(mx.fit2abyre)
# Save Model 2A Summary
sink("Model2abyre.txt")
print(summary(mx.fit2abyre))
sink()  
# Tau 2 estimates
options(max.print = 10000)
Model2abyretau<-VarCorr(mx.fit2abyre)
sink("Model2abyretau.txt")
print(Model2abyretau)
sink() 
## Calculate the R2
model2abyrer2<-osmasemR2(mx.fit2abyre, mx.fit2a)
sink("model2abyrer2.txt")
print(model2abyrer2)
sink()
## Compare the models with and without the moderator
model2abyreanova<-anova(mx.fit2abyre, mx.fit2a)
sink("model2abyreanova.txt")
print(model2abyreanova)
sink()
# Save osmasem List
save(mx.fit2abyre,file="mx.fit2abyre.RData")

### MODEL 2A among Adolescent Participants (~ 15 minutes) ----
rm(list=ls())
load('adol2adata.RData')
# Sampling covariance matrix of the correlations
adol2adf <- Cor2DataFrame(adol2adata$cordat, adol2adata$n, acov = "weighted")
save(adol2adf, file="adol2adf.RData")
# Define Model 1a using lavaan syntax 
adol2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
         INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
         INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
         EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
         EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
         OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
         OCCS2 ~~ occs2Wdeppw*DEPPW
         INC ~~ 1*INC
         EDU ~~ 1*EDU
         OCCS ~~ 1*OCCS
         INC2 ~~ 1*INC2
         EDU2 ~~ 1*EDU2
         OCCS2 ~~ 1*OCCS2
         DEPPW ~~ 1*DEPPW
         DEP ~~ Errdep*DEP'
plot(adol2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aadol <- lavaan2RAM(adol2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aadol

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aadol <- create.vechsR(A0=RAM2aadol$A, S0=RAM2aadol$S)

# Heterogeneity Variance-Covariance Matrix
T2aadol <- create.Tau2(RAM=RAM2aadol, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

adolfit2a <- osmasem(model.name="adol_2A", Mmatrix=M2aadol, Tmatrix=T2aadol, data=adol2adf)
summary(adolfit2a)
# Save Model 2A Summary
sink("adol2aest.txt")
print(summary(adolfit2a))
sink()  
# Residuals
options(max.print = 10000)
adol2aresiduals<-data.frame(mxEval(Smatrix, adolfit2a$mx.fit))
colnames(adol2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adol2aresiduals.txt")
print(adol2aresiduals)
sink() 
# Implied R Matrix
adol2armatrix<-mxEval(impliedR,adolfit2a$mx.fit)
colnames(adol2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adol2armatrix.txt")
print(adol2armatrix)
sink() 
## Summary with Fit Indices
#adol2asumwfit<-summary(adolfit2a,fitIndices = TRUE)
#sink("adol2asumwfit.txt")
#print(adol2asumwfit)
#sink() 
## SRMR
#adol2asrmr<-osmasemSRMR(adolfit2a)
#sink("adol2asrmr.txt")
#print(adol2asrmr)
#sink() 
# Save osmasem List
save(adolfit2a,file="adolfit2a.RData")

### MODEL 2A among Young Adult Participants (~ 15 minutes) ----
rm(list=ls())
load('yadu2adata.RData')
# Sampling covariance matrix of the correlations
yadu2adf <- Cor2DataFrame(yadu2adata$cordat, yadu2adata$n, acov = "weighted")
save(yadu2adf, file="yadu2adf.RData")
# Define Model 1a using lavaan syntax 
yadu2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
         INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
         INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
         EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
         EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
         OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
         OCCS2 ~~ occs2Wdeppw*DEPPW
         INC ~~ 1*INC
         EDU ~~ 1*EDU
         OCCS ~~ 1*OCCS
         INC2 ~~ 1*INC2
         EDU2 ~~ 1*EDU2
         OCCS2 ~~ 1*OCCS2
         DEPPW ~~ 1*DEPPW
         DEP ~~ Errdep*DEP'
plot(yadu2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2ayadu <- lavaan2RAM(yadu2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2ayadu

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2ayadu <- create.vechsR(A0=RAM2ayadu$A, S0=RAM2ayadu$S)

# Heterogeneity Variance-Covariance Matrix
T2ayadu <- create.Tau2(RAM=RAM2ayadu, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

yadufit2a <- osmasem(model.name="yadu_2A", Mmatrix=M2ayadu, Tmatrix=T2ayadu, data=yadu2adf)
summary(yadufit2a)
# Save Model 2A Summary
sink("yadu2aest.txt")
print(summary(yadufit2a))
sink()  
# Residuals
options(max.print = 10000)
yadu2aresiduals<-data.frame(mxEval(Smatrix, yadufit2a$mx.fit))
colnames(yadu2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("yadu2aresiduals.txt")
print(yadu2aresiduals)
sink() 
# Implied R Matrix
yadu2armatrix<-mxEval(impliedR,yadufit2a$mx.fit)
colnames(yadu2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("yadu2armatrix.txt")
print(yadu2armatrix)
sink() 
## Summary with Fit Indices
#yadu2asumwfit<-summary(yadufit2a,fitIndices = TRUE)
#sink("yadu2asumwfit.txt")
#print(yadu2asumwfit)
#sink() 
## SRMR
#yadu2asrmr<-osmasemSRMR(yadufit2a)
#sink("yadu2asrmr.txt")
#print(yadu2asrmr)
#sink() 
# Save osmasem List
save(yadufit2a,file="yadufit2a.RData")

### MODEL 2A among Middle-Aged Adult Participants (~ 20 minutes) ----
rm(list=ls())
load('adu2adata.RData')
# Sampling covariance matrix of the correlations
adu2adf <- Cor2DataFrame(adu2adata$cordat, adu2adata$n, acov = "weighted")
save(adu2adf, file="adu2adf.RData")
# Define Model 1a using lavaan syntax 
adu2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
        INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
        INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
        EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
        EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
        OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
        OCCS2 ~~ occs2Wdeppw*DEPPW
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(adu2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aadu <- lavaan2RAM(adu2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aadu

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aadu <- create.vechsR(A0=RAM2aadu$A, S0=RAM2aadu$S)

# Heterogeneity Variance-Covariance Matrix
T2aadu <- create.Tau2(RAM=RAM2aadu, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

adufit2a <- osmasem(model.name="adu_2A", Mmatrix=M2aadu, Tmatrix=T2aadu, data=adu2adf)
summary(adufit2a)
# Save Model 2A Summary
sink("adu2aest.txt")
print(summary(adufit2a))
sink()  
# Residuals
options(max.print = 10000)
adu2aresiduals<-data.frame(mxEval(Smatrix, adufit2a$mx.fit))
colnames(adu2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adu2aresiduals.txt")
print(adu2aresiduals)
sink() 
# Implied R Matrix
adu2armatrix<-mxEval(impliedR,adufit2a$mx.fit)
colnames(adu2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("adu2armatrix.txt")
print(adu2armatrix)
sink() 
## Summary with Fit Indices
#adu2asumwfit<-summary(adufit2a,fitIndices = TRUE)
#sink("adu2asumwfit.txt")
#print(adu2asumwfit)
#sink() 
## SRMR
#adu2asrmr<-osmasemSRMR(adufit2a)
#sink("adu2asrmr.txt")
#print(adu2asrmr)
#sink() 
# Save osmasem List
save(adufit2a,file="adufit2a.RData")

### MODEL 2A among Older Adult Participants (~ 15 minutes) ----
rm(list=ls())
load('oadu2adata.RData')
# Sampling covariance matrix of the correlations
oadu2adf <- Cor2DataFrame(oadu2adata$cordat, oadu2adata$n, acov = "weighted")
save(oadu2adf, file="oadu2adf.RData")
# Define Model 1a using lavaan syntax 
oadu2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
         INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
         INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
         EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
         EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
         OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
         OCCS2 ~~ occs2Wdeppw*DEPPW
         INC ~~ 1*INC
         EDU ~~ 1*EDU
         OCCS ~~ 1*OCCS
         INC2 ~~ 1*INC2
         EDU2 ~~ 1*EDU2
         OCCS2 ~~ 1*OCCS2
         DEPPW ~~ 1*DEPPW
         DEP ~~ Errdep*DEP'
plot(oadu2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aoadu <- lavaan2RAM(oadu2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aoadu

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aoadu <- create.vechsR(A0=RAM2aoadu$A, S0=RAM2aoadu$S)

# Heterogeneity Variance-Covariance Matrix
T2aoadu <- create.Tau2(RAM=RAM2aoadu, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

oadufit2a <- osmasem(model.name="oadu_2A", Mmatrix=M2aoadu, Tmatrix=T2aoadu, data=oadu2adf)
summary(oadufit2a)
# Save Model 2A Summary
sink("oadu2aest.txt")
print(summary(oadufit2a))
sink()  
# Residuals
options(max.print = 10000)
oadu2aresiduals<-data.frame(mxEval(Smatrix, oadufit2a$mx.fit))
colnames(oadu2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("oadu2aresiduals.txt")
print(oadu2aresiduals)
sink() 
# Implied R Matrix
oadu2armatrix<-mxEval(impliedR,oadufit2a$mx.fit)
colnames(oadu2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("oadu2armatrix.txt")
print(oadu2armatrix)
sink() 
## Summary with Fit Indices
#oadu2asumwfit<-summary(oadufit2a,fitIndices = TRUE)
#sink("oadu2asumwfit.txt")
#print(oadu2asumwfit)
#sink() 
## SRMR
#oadu2asrmr<-osmasemSRMR(oadufit2a)
#sink("oadu2asrmr.txt")
#print(oadu2asrmr)
#sink() 
# Save osmasem List
save(oadufit2a,file="oadufit2a.RData")

### MODEL 2A among White Participants (~ 30 minutes) ----
rm(list=ls())
load('Wht2adata.RData')
# Sampling covariance matrix of the correlations
Wht2adf <- Cor2DataFrame(Wht2adata$cordat, Wht2adata$n, acov = "weighted")
save(Wht2adf, file="Wht2adf.RData")
# Define Model 1a using lavaan syntax 
Wht2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
        INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
        INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
        EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
        EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
        OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
        OCCS2 ~~ occs2Wdeppw*DEPPW
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(Wht2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2awht <- lavaan2RAM(Wht2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2awht

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2awht <- create.vechsR(A0=RAM2awht$A, S0=RAM2awht$S)

# Heterogeneity Variance-Covariance Matrix
T2awht <- create.Tau2(RAM=RAM2awht, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Whtfit2a <- osmasem(model.name="Wht_2A", Mmatrix=M2awht, Tmatrix=T2awht, data=Wht2adf)
summary(Whtfit2a)
# Save Model 2A Summary
sink("Wht2aest.txt")
print(summary(Whtfit2a))
sink()  
# Residuals
options(max.print = 10000)
Wht2aresiduals<-data.frame(mxEval(Smatrix, Whtfit2a$mx.fit))
colnames(Wht2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Wht2aresiduals.txt")
print(Wht2aresiduals)
sink() 
# Implied R Matrix
Wht2armatrix<-mxEval(impliedR,Whtfit2a$mx.fit)
colnames(Wht2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Wht2armatrix.txt")
print(Wht2armatrix)
sink() 
## Summary with Fit Indices
#Wht2asumwfit<-summary(Whtfit2a,fitIndices = TRUE)
#sink("Wht2asumwfit.txt")
#print(Wht2asumwfit)
#sink() 
## SRMR
#Wht2asrmr<-osmasemSRMR(Whtfit2a)
#sink("Wht2asrmr.txt")
#print(Wht2asrmr)
#sink() 
# Save osmasem List
save(Whtfit2a,file="Whtfit2a.RData")


### MODEL 2A among Latinx Participants (< 30 minutes) ----
rm(list=ls())
load('Lat2adata.RData')
# Sampling covariance matrix of the correlations
Lat2adf <- Cor2DataFrame(Lat2adata$cordat, Lat2adata$n, acov = "weighted")
save(Lat2adf, file="Lat2adf.RData")
# Define Model 1a using lavaan syntax 
Lat2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
        INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
        INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
        EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
        EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
        OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
        OCCS2 ~~ occs2Wdeppw*DEPPW
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(Lat2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2alat <- lavaan2RAM(Lat2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2alat

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2alat <- create.vechsR(A0=RAM2alat$A, S0=RAM2alat$S)

# Heterogeneity Variance-Covariance Matrix
T2alat <- create.Tau2(RAM=RAM2alat, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Latfit2a <- osmasem(model.name="Lat_2A", Mmatrix=M2alat, Tmatrix=T2alat, data=Lat2adf)
summary(Latfit2a)
# Save Model 2A Summary
sink("Lat2aest.txt")
print(summary(Latfit2a))
sink()  
# Residuals
options(max.print = 10000)
Lat2aresiduals<-data.frame(mxEval(Smatrix, Latfit2a$mx.fit))
colnames(Lat2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Lat2aresiduals.txt")
print(Lat2aresiduals)
sink() 
# Implied R Matrix
Lat2armatrix<-mxEval(impliedR,Latfit2a$mx.fit)
colnames(Lat2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Lat2armatrix.txt")
print(Lat2armatrix)
sink() 
## Summary with Fit Indices
#Lat2asumwfit<-summary(Latfit2a,fitIndices = TRUE)
#sink("Lat2asumwfit.txt")
#print(Lat2asumwfit)
#sink() 
## SRMR
#Lat2asrmr<-osmasemSRMR(Latfit2a)
#sink("Lat2asrmr.txt")
#print(Lat2asrmr)
#sink() 
# Save osmasem List
save(Latfit2a,file="Latfit2a.RData")

### MODEL 2A among Black Participants (< 30 minutes) ----
rm(list=ls())
load('Blk2adata.RData')
# Sampling covariance matrix of the correlations
Blk2adf <- Cor2DataFrame(Blk2adata$cordat, Blk2adata$n, acov = "weighted")
save(Blk2adf, file="Blk2adf.RData")
# Define Model 1a using lavaan syntax 
Blk2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
        INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
        INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
        EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
        EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
        OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
        OCCS2 ~~ occs2Wdeppw*DEPPW
        INC ~~ 1*INC
        EDU ~~ 1*EDU
        OCCS ~~ 1*OCCS
        INC2 ~~ 1*INC2
        EDU2 ~~ 1*EDU2
        OCCS2 ~~ 1*OCCS2
        DEPPW ~~ 1*DEPPW
        DEP ~~ Errdep*DEP'
plot(Blk2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2ablk <- lavaan2RAM(Blk2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2ablk

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2ablk <- create.vechsR(A0=RAM2ablk$A, S0=RAM2ablk$S)

# Heterogeneity Variance-Covariance Matrix
T2ablk <- create.Tau2(RAM=RAM2ablk, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Blkfit2a <- osmasem(model.name="Blk_2A", Mmatrix=M2ablk, Tmatrix=T2ablk, data=Blk2adf)
summary(Blkfit2a)
# Save Model 2A Summary
sink("Blk2aest.txt")
print(summary(Blkfit2a))
sink()  
# Residuals
options(max.print = 10000)
Blk2aresiduals<-data.frame(mxEval(Smatrix, Blkfit2a$mx.fit))
colnames(Blk2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Blk2aresiduals.txt")
print(Blk2aresiduals)
sink() 
# Implied R Matrix
Blk2armatrix<-mxEval(impliedR,Blkfit2a$mx.fit)
colnames(Blk2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Blk2armatrix.txt")
print(Blk2armatrix)
sink() 
## Summary with Fit Indices
#Blk2asumwfit<-summary(Blkfit2a,fitIndices = TRUE)
#sink("Blk2asumwfit.txt")
#print(Blk2asumwfit)
#sink() 
## SRMR
#Blk2asrmr<-osmasemSRMR(Blkfit2a)
#sink("Blk2asrmr.txt")
#print(Blk2asrmr)
#sink() 
# Save osmasem List
save(Blkfit2a,file="Blkfit2a.RData")

### MODEL 2A among Asian American Participants (< 15 minutes) ----
rm(list=ls())
load('Aa2adata.RData')
# Sampling covariance matrix of the correlations
Aadep2adf <- Cor2DataFrame(Aadep2adata$cordat, Aadep2adata$n, acov = "weighted")
save(Aadep2adf, file="Aadep2adf.RData")
# Define Model 1a using lavaan syntax 
Aadep2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
          INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
          INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
          EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
          EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
          OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
          OCCS2 ~~ occs2Wdeppw*DEPPW
          INC ~~ 1*INC
          EDU ~~ 1*EDU
          OCCS ~~ 1*OCCS
          INC2 ~~ 1*INC2
          EDU2 ~~ 1*EDU2
          OCCS2 ~~ 1*OCCS2
          DEPPW ~~ 1*DEPPW
          DEP ~~ Errdep*DEP'
plot(Aadep2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aaa <- lavaan2RAM(Aadep2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aaa

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aaa <- create.vechsR(A0=RAM2aaa$A, S0=RAM2aaa$S)

# Heterogeneity Variance-Covariance Matrix
T2aaa <- create.Tau2(RAM=RAM2aaa, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Aadepfit2a <- osmasem(model.name="AA_2A", Mmatrix=M2aaa, Tmatrix=T2aaa, data=Aadep2adf)
summary(Aadepfit2a)
# Save Model 2A Summary
sink("Aadep2aest.txt")
print(summary(Aadepfit2a))
sink()  
# Residuals
options(max.print = 10000)
Aadep2aresiduals<-data.frame(mxEval(Smatrix, Aadepfit2a$mx.fit))
colnames(Aadep2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Aadep2aresiduals.txt")
print(Aadep2aresiduals)
sink() 
# Implied R Matrix
Aadep2armatrix<-mxEval(impliedR,Aadepfit2a$mx.fit)
colnames(Aadep2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Aadep2armatrix.txt")
print(Aadep2armatrix)
sink() 
## Summary with Fit Indices
#Aadep2asumwfit<-summary(Aadepfit2a,fitIndices = TRUE)
#sink("Aadep2asumwfit.txt")
#print(Aadep2asumwfit)
#sink() 
## SRMR
#Aadep2asrmr<-osmasemSRMR(Aadepfit2a)
#sink("Aadep2asrmr.txt")
#print(Aadep2asrmr)
#sink() 
# Save osmasem List
save(Aadepfit2a,file="Aadepfit2a.RData")

### MODEL 2A among Native American Participants (< 15 minutes) ----
rm(list=ls())
load('Ai2adata.RData')
# Sampling covariance matrix of the correlations
Ai2adf <- Cor2DataFrame(Ai2adata$cordat, Ai2adata$n, acov = "weighted")
save(Ai2adf, file="Ai2adf.RData")
# Define Model 1a using lavaan syntax 
Ai2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
       INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
       INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
       EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
       EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
       OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
       OCCS2 ~~ occs2Wdeppw*DEPPW
       INC ~~ 1*INC
       EDU ~~ 1*EDU
       OCCS ~~ 1*OCCS
       INC2 ~~ 1*INC2
       EDU2 ~~ 1*EDU2
       OCCS2 ~~ 1*OCCS2
       DEPPW ~~ 1*DEPPW
       DEP ~~ Errdep*DEP'
plot(Ai2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2aai <- lavaan2RAM(Ai2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2aai

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2aai <- create.vechsR(A0=RAM2aai$A, S0=RAM2aai$S)

# Heterogeneity Variance-Covariance Matrix
T2aai <- create.Tau2(RAM=RAM2aai, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Aifit2a <- osmasem(model.name="AI_2A", Mmatrix=M2aai, Tmatrix=T2aai, data=Ai2adf)
summary(Aifit2a)
# Save Model 2A Summary
sink("Ai2aest.txt")
print(summary(Aifit2a))
sink()  
# Residuals
options(max.print = 10000)
Ai2aresiduals<-data.frame(mxEval(Smatrix, Aifit2a$mx.fit))
colnames(Ai2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Ai2aresiduals.txt")
print(Ai2aresiduals)
sink() 
# Implied R Matrix
Ai2armatrix<-mxEval(impliedR,Aifit2a$mx.fit)
colnames(Ai2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Ai2armatrix.txt")
print(Ai2armatrix)
sink() 
## Summary with Fit Indices
#Ai2asumwfit<-summary(Aifit2a,fitIndices = TRUE)
#sink("Ai2asumwfit.txt")
#print(Ai2asumwfit)
#sink() 
## SRMR
#Ai2asrmr<-osmasemSRMR(Aifit2a)
#sink("Ai2asrmr.txt")
#print(Ai2asrmr)
#sink() 
# Save osmasem List
save(Aifit2a,file="Aifit2a.RData")

### MODEL 2A among Multiracial Participants (20 mins) ----
rm(list=ls())
load('Mr2adata.RData')
# Sampling covariance matrix of the correlations
Mr2adf <- Cor2DataFrame(Mr2adata$cordat, Mr2adata$n, acov = "weighted")
save(Mr2adf, file="Mr2adf.RData")
# Define Model 1a using lavaan syntax 
Mr2a<-'DEP ~ inctodep*INC + edutodep*EDU + occstodep*OCCS + inc2todep*INC2 + edu2todep*EDU2 + occs2todep*OCCS2 + deppwtodep*DEPPW
       INC ~~ incWedu*EDU + incWoccs*OCCS + incWinc2*INC2 + incWedu2*EDU2 + incWoccs2*OCCS2 + incWdeppw*DEPPW
       INC2 ~~ inc2Wedu*EDU + inc2Woccs*OCCS + inc2Wedu2*EDU2 + inc2Woccs2*OCCS2 + inc2Wdeppw*DEPPW
       EDU ~~ eduWoccs*OCCS + eduWedu2*EDU2 + eduWoccs2*OCCS2 + eduWdeppw*DEPPW
       EDU2 ~~ edu2Woccs*OCCS + edu2Woccs2*OCCS2 + edu2Wdeppw*DEPPW
       OCCS ~~ occsWoccs2*OCCS2 + occsWdeppw*DEPPW
       OCCS2 ~~ occs2Wdeppw*DEPPW
       INC ~~ 1*INC
       EDU ~~ 1*EDU
       OCCS ~~ 1*OCCS
       INC2 ~~ 1*INC2
       EDU2 ~~ 1*EDU2
       OCCS2 ~~ 1*OCCS2
       DEPPW ~~ 1*DEPPW
       DEP ~~ Errdep*DEP'
plot(Mr2a, col="grey")

# Convert the lavaan syntax into the RAM specification
RAM2amr <- lavaan2RAM(Mr2a, obs.variables=c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))
RAM2amr

# Model Implied Correlation Structure W/Implicit Diagonal Constraints
M2amr <- create.vechsR(A0=RAM2amr$A, S0=RAM2amr$S)

# Heterogeneity Variance-Covariance Matrix
T2amr <- create.Tau2(RAM=RAM2amr, RE.type="Diag", Transform="sqSD", RE.startvalues=0.01)

Mrfit2a <- osmasem(model.name="MR_2A", Mmatrix=M2amr, Tmatrix=T2amr, data=Mr2adf)
summary(Mrfit2a)
# Save Model 2A Summary
sink("Mr2aest.txt")
print(summary(Mrfit2a))
sink()  
# Residuals
options(max.print = 10000)
Mr2aresiduals<-data.frame(mxEval(Smatrix, Mrfit2a$mx.fit))
colnames(Mr2aresiduals)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Mr2aresiduals.txt")
print(Mr2aresiduals)
sink() 
# Implied R Matrix
Mr2armatrix<-mxEval(impliedR,Mrfit2a$mx.fit)
colnames(Mr2armatrix)<-c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW")
sink("Mr2armatrix.txt")
print(Mr2armatrix)
sink() 
## Summary with Fit Indices
#Mr2asumwfit<-summary(Mrfit2a,fitIndices = TRUE)
#sink("Mr2asumwfit.txt")
#print(Mr2asumwfit)
#sink() 
## SRMR
#Mr2asrmr<-osmasemSRMR(Mrfit2a)
#sink("Mr2asrmr.txt")
#print(Mr2asrmr)
#sink() 
# Save osmasem List
save(Mrfit2a,file="Mrfit2a.RData")
