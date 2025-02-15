### SYNTAX FOR REFROMATTING DATA FOR OMASEM ANALYSES ####
# Following data format in Jak & Cheung (2020) example
load('SESIPD_CESD.rda')

# Load metaSEM for vec2symMat function
library('metaSEM')

### CREATE LIST FOR OMASEM ANALYSIS ----
## All Correlations for Model 1A ----
# Reformat columns of the correlations into a symmetric correlation matrix
model1adata<-dplyr::select(SESIPD_CESD,!contains(c("INC2","EDU2","OCCS2","OCCP","OCCP2")))
m1matslist <- list()
for(i in 1:dim(model1adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(model1adata[i,4:13],diag=FALSE)),nrow=5,ncol=5,
              dimnames = list(c("INC","EDU","OCCS","DEP","DEPPW"),
                              c("INC","EDU","OCCS","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(model1adata[i,4:13],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  m1matslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
model1adata$msamsize<-round(rowMeans(model1adata[14:23],na.rm = TRUE))

# Add additional variabes
model1adata<-list(cordat=m1matslist,
             n=model1adata$msamsize,
             devp=model1adata$DEVP,
             male=model1adata$MALE,
             reth=model1adata$RACETH,
             year=model1adata$DATAYEAR,
             dfid=model1adata$DFID,
             dataid=model1adata$DATAID,
             dsid=model1adata$DSID)
# SAVE DATASET
save(model1adata, file="model1adata.RData")

## All Correlations for Model 1B ----
# Reformat columns of the correlations into a symmetric correlation matrix
model1bdata<-dplyr::select(SESIPD_CESD,!contains(c("INC2","EDU2","OCCS","OCCS2","OCCP2")))
m1bmatslist <- list()
for(i in 1:dim(model1bdata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(model1bdata[i,4:13],diag=FALSE)),nrow=5,ncol=5,
              dimnames = list(c("INC","EDU","OCCP","DEP","DEPPW"),
                              c("INC","EDU","OCCP","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(model1bdata[i,4:13],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  m1bmatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
model1bdata$msamsize<-round(rowMeans(model1bdata[14:23],na.rm = TRUE))

# Add additional variabes
model1bdata<-list(cordat=m1bmatslist,
             n=model1bdata$msamsize,
             devp=model1bdata$DEVP,
             male=model1bdata$MALE,
             reth=model1bdata$RACETH,
             year=model1bdata$DATAYEAR,
             dfid=model1bdata$DFID,
             dataid=model1bdata$DATAID,
             dsid=model1bdata$DSID)
# SAVE DATASET
save(model1bdata, file="model1bdata.RData")

## All Correlations for Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
model2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
m2amatslist <- list()
for(i in 1:dim(model2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(model2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(model2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  m2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
model2adata$msamsize<-round(rowMeans(model2adata[32:59],na.rm = TRUE))

# Add additional variabes
model2adata<-list(cordat=m2amatslist,
             n=model2adata$msamsize,
             devp=model2adata$DEVP,
             male=model2adata$MALE,
             reth=model2adata$RACETH,
             year=model2adata$DATAYEAR,
             dfid=model2adata$DFID,
             dataid=model2adata$DATAID,
             dsid=model2adata$DSID)
# SAVE DATASET
save(model2adata, file="model2adata.RData")

## All Correlations for Model 2B ----
# Reformat columns of the correlations into a symmetric correlation matrix
model2bdata<-dplyr::select(SESIPD_CESD,!contains(c("OCCS","OCCS2")))
m2bmatslist <- list()
for(i in 1:dim(model2bdata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(model2bdata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCP","OCCP2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(model2bdata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  m2bmatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
model2bdata$msamsize<-round(rowMeans(model2bdata[32:59],na.rm = TRUE))

# Add additional variabes
model2bdata<-list(cordat=m2bmatslist,
             n=model2bdata$msamsize,
             devp=model2bdata$DEVP,
             male=model2bdata$MALE,
             reth=model2bdata$RACETH,
             year=model2bdata$DATAYEAR,
             dfid=model2bdata$DFID,
             dataid=model2bdata$DATAID,
             dsid=model2bdata$DSID)
# SAVE DATASET
save(model2bdata, file="model2bdata.RData")

## All Correlations for White participants Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
Wht2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
Wht2adata<-dplyr::filter(Wht2adata,RACETH==0)
Wht2amatslist <- list()
for(i in 1:dim(Wht2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(Wht2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(Wht2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  Wht2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
Wht2adata$msamsize<-round(rowMeans(Wht2adata[32:59],na.rm = TRUE))

# Add additional variabes
Wht2adata<-list(cordat=Wht2amatslist,
             n=Wht2adata$msamsize,
             devp=Wht2adata$DEVP,
             male=Wht2adata$MALE,
             reth=Wht2adata$RACETH,
             year=Wht2adata$DATAYEAR,
             dfid=Wht2adata$DFID,
             dataid=Wht2adata$DATAID,
             dsid=Wht2adata$DSID)
# SAVE DATASET
save(Wht2adata, file="Wht2adata.RData")

## All Correlations for Latinx participants Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
Lat2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
Lat2adata<-dplyr::filter(Lat2adata,RACETH==1)
Lat2amatslist <- list()
for(i in 1:dim(Lat2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(Lat2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(Lat2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  Lat2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
Lat2adata$msamsize<-round(rowMeans(Lat2adata[32:59],na.rm = TRUE))

# Add additional variabes
Lat2adata<-list(cordat=Lat2amatslist,
             n=Lat2adata$msamsize,
             devp=Lat2adata$DEVP,
             male=Lat2adata$MALE,
             reth=Lat2adata$RACETH,
             year=Lat2adata$DATAYEAR,
             dfid=Lat2adata$DFID,
             dataid=Lat2adata$DATAID,
             dsid=Lat2adata$DSID)
# SAVE DATASET
save(Lat2adata, file="Lat2adata.RData")

## All Correlations for Black participants Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
Blk2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
Blk2adata<-dplyr::filter(Blk2adata,RACETH==2)
Blk2amatslist <- list()
for(i in 1:dim(Blk2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(Blk2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(Blk2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  Blk2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
Blk2adata$msamsize<-round(rowMeans(Blk2adata[32:59],na.rm = TRUE))

# Add additional variabes
Blk2adata<-list(cordat=Blk2amatslist,
             n=Blk2adata$msamsize,
             devp=Blk2adata$DEVP,
             male=Blk2adata$MALE,
             reth=Blk2adata$RACETH,
             year=Blk2adata$DATAYEAR,
             dfid=Blk2adata$DFID,
             dataid=Blk2adata$DATAID,
             dsid=Blk2adata$DSID)
# SAVE DATASET
save(Blk2adata, file="Blk2adata.RData")

## All Correlations for Asian American participants Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
Aadep2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
Aadep2adata<-dplyr::filter(Aadep2adata,RACETH==3)
Aadep2amatslist <- list()
for(i in 1:dim(Aadep2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(Aadep2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(Aadep2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  Aadep2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
Aadep2adata$msamsize<-round(rowMeans(Aadep2adata[32:59],na.rm = TRUE))

# Add additional variabes
Aadep2adata<-list(cordat=Aadep2amatslist,
             n=Aadep2adata$msamsize,
             devp=Aadep2adata$DEVP,
             male=Aadep2adata$MALE,
             reth=Aadep2adata$RACETH,
             year=Aadep2adata$DATAYEAR,
             dfid=Aadep2adata$DFID,
             dataid=Aadep2adata$DATAID,
             dsid=Aadep2adata$DSID)
# SAVE DATASET
save(Aadep2adata, file="Aa2adata.RData")

## All Correlations for Native American participants Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
Ai2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
Ai2adata<-dplyr::filter(Ai2adata,RACETH==4)
Ai2amatslist <- list()
for(i in 1:dim(Ai2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(Ai2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(Ai2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  Ai2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
Ai2adata$msamsize<-round(rowMeans(Ai2adata[32:59],na.rm = TRUE))

# Add additional variabes
Ai2adata<-list(cordat=Ai2amatslist,
             n=Ai2adata$msamsize,
             devp=Ai2adata$DEVP,
             male=Ai2adata$MALE,
             reth=Ai2adata$RACETH,
             year=Ai2adata$DATAYEAR,
             dfid=Ai2adata$DFID,
             dataid=Ai2adata$DATAID,
             dsid=Ai2adata$DSID)
# SAVE DATASET
save(Ai2adata, file="Ai2adata.RData")

## All Correlations for Multiracial participants Model 2A ----
# Reformat columns of the correlations into a symmetric correlation matrix
Mr2adata<-dplyr::select(SESIPD_CESD,!contains(c("OCCP","OCCP2")))
Mr2adata<-dplyr::filter(Mr2adata,RACETH==5)
Mr2amatslist <- list()
for(i in 1:dim(Mr2adata)[1]){
  a<-t(matrix(as.numeric(vec2symMat(Mr2adata[i,4:31],diag=FALSE)),nrow=8,ncol=8,
              dimnames = list(c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"),
                              c("INC","INC2","EDU","EDU2","OCCS","OCCS2","DEP","DEPPW"))))
  b<-a[lower.tri(a)]<-matrix(as.numeric(Mr2adata[i,4:31],byrow=FALSE))
  name <- paste('mat',i,sep='.')
  tmp <- list(b)
  Mr2amatslist[[name]]<-a}

# Compute one sample size per correlation matrix, average sample size
Mr2adata$msamsize<-round(rowMeans(Mr2adata[32:59],na.rm = TRUE))

# Add additional variabes
Mr2adata<-list(cordat=Mr2amatslist,
             n=Mr2adata$msamsize,
             devp=Mr2adata$DEVP,
             male=Mr2adata$MALE,
             reth=Mr2adata$RACETH,
             year=Mr2adata$DATAYEAR,
             dfid=Mr2adata$DFID,
             dataid=Mr2adata$DATAID,
             dsid=Mr2adata$DSID)
# SAVE DATASET
save(Mr2adata, file="Mr2adata.RData")
