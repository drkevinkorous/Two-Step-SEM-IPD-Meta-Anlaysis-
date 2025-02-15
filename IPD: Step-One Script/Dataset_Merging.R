### SYNTAX FOR MERGING AGGREGATED DATASETS (CORRELATIONS) ####
## READ IN .CSV FILES ----
ID171DS1_R<-read.csv('ID171DS1_R_aggregated.csv')
ID171DS1_S<-read.csv('ID171DS1_S_aggregated.csv')
ID182DS0_1992<-read.csv('ID182DS0_1992_aggregated.csv')
ID182DS0_1994<-read.csv('ID182DS0_1994_aggregated.csv')
ID182DS0_2006<-read.csv('ID182DS0_2006_aggregated.csv')
ID182DS0_2016<-read.csv('ID182DS0_2016_aggregated.csv')
ID182DS0_CH1986<-read.csv('ID182DS0_CH1986_aggregated.csv')
ID182DS0_CH1988<-read.csv('ID182DS0_CH1988_aggregated.csv')
ID182DS0_CH1990<-read.csv('ID182DS0_CH1990_aggregated.csv')
ID182DS0_CH1992<-read.csv('ID182DS0_CH1992_aggregated.csv')
ID182DS0_CH1994<-read.csv('ID182DS0_CH1994_aggregated.csv')
ID182DS0_CH1996<-read.csv('ID182DS0_CH1996_aggregated.csv')
ID182DS0_CH1998<-read.csv('ID182DS0_CH1998_aggregated.csv')
ID182DS0_CH2000<-read.csv('ID182DS0_CH2000_aggregated.csv')
ID182DS0_CH2002<-read.csv('ID182DS0_CH2002_aggregated.csv')
ID182DS0_CH2004<-read.csv('ID182DS0_CH2004_aggregated.csv')
ID182DS0_CH2006<-read.csv('ID182DS0_CH2006_aggregated.csv')
ID182DS0_CH2008<-read.csv('ID182DS0_CH2008_aggregated.csv')
ID182DS0_YA1996<-read.csv('ID182DS0_YA1996_aggregated.csv')
ID182DS0_YA1998<-read.csv('ID182DS0_YA1998_aggregated.csv')
ID182DS0_YA2000<-read.csv('ID182DS0_YA2000_aggregated.csv')
ID182DS0_YA2002<-read.csv('ID182DS0_YA2002_aggregated.csv')
ID182DS0_YA2004<-read.csv('ID182DS0_YA2004_aggregated.csv')
ID182DS0_YA2006<-read.csv('ID182DS0_YA2006_aggregated.csv')
ID182DS0_YA2008<-read.csv('ID182DS0_YA2008_aggregated.csv')
ID182DS0_YA2010<-read.csv('ID182DS0_YA2010_aggregated.csv')
ID182DS0_YA2012<-read.csv('ID182DS0_YA2012_aggregated.csv')
ID182DS0_YA2014<-read.csv('ID182DS0_YA2014_aggregated.csv')
ID182NLSY97_2000<-read.csv('ID182NLSY97_2000_aggregated.csv')
ID182NLSY97_2002<-read.csv('ID182NLSY97_2002_aggregated.csv')
ID182NLSY97_2004<-read.csv('ID182NLSY97_2004_aggregated.csv')
ID182NLSY97_2006<-read.csv('ID182NLSY97_2006_aggregated.csv')
ID182NLSY97_2008<-read.csv('ID182NLSY97_2008_aggregated.csv')
ID182NLSY97_2010<-read.csv('ID182NLSY97_2010_aggregated.csv')
ID182NLSY97_2015<-read.csv('ID182NLSY97_2015_aggregated.csv')
ID182NLSY97_2017<-read.csv('ID182NLSY97_2017_aggregated.csv')
ID2566DS0<-read.csv('ID2566DS0_aggregated.csv')
ID2954DS7<-read.csv('ID2954DS7_aggregated.csv')
ID3107DS4<-read.csv('ID3107DS4_aggregated.csv')
ID3255DS1<-read.csv('ID3255DS1_aggregated.csv')
ID3255DS2<-read.csv('ID3255DS2_aggregated.csv')
ID3334DS1_1995<-read.csv('ID3334DS1_1995_aggregated.csv')
ID3334DS1_1998<-read.csv('ID3334DS1_1998_aggregated.csv')
ID3334DS2<-read.csv('ID3334DS2_aggregated.csv')
ID3381DS4<-read.csv('ID3381DS4_aggregated.csv')
ID3397DS4<-read.csv('ID3397DS4_aggregated.csv')
ID3605DS4<-read.csv('ID3605DS4_aggregated.csv')
ID3903DS1_ACHV<-read.csv('ID3903DS1_ACHV_aggregated.csv')
ID3903DS1<-read.csv('ID3903DS1_aggregated.csv')
ID3927DS1<-read.csv('ID3927DS1_aggregated.csv')
ID3927DS4<-read.csv('ID3927DS4_aggregated.csv')
ID4176DS4<-read.csv('ID4176DS4_aggregated.csv')
ID4222DS4<-read.csv('ID4222DS4_aggregated.csv')
ID4373DS1_ACHV<-read.csv('ID4373DS1_ACHV_aggregated.csv')
ID4373DS1<-read.csv('ID4373DS1_aggregated.csv')
ID4581DS1<-read.csv('ID4581DS1_aggregated.csv')
ID4581DS4<-read.csv('ID4581DS4_aggregated.csv')
ID4582DS1<-read.csv('ID4582DS1_aggregated.csv')
ID4582DS4<-read.csv('ID4582DS4_aggregated.csv')
ID4596DS1_ACHV<-read.csv('ID4596DS1_ACHV_aggregated.csv')
ID4596DS1<-read.csv('ID4596DS1_aggregated.csv')
ID4652DS1<-read.csv('ID4652DS1_aggregated.csv')
ID4690DS1_1986<-read.csv('ID4690DS1_1986_aggregated.csv')
ID4690DS1_1989<-read.csv('ID4690DS1_1989_aggregated.csv')
ID4690DS1_1994<-read.csv('ID4690DS1_1994_aggregated.csv')
ID4690DS1_2002<-read.csv('ID4690DS1_2002_aggregated.csv')
ID4690DS1_2012<-read.csv('ID4690DS1_2012_aggregated.csv')
ID6041DS1<-read.csv('ID6041DS1_aggregated.csv')
ID6370DS1<-read.csv('ID6370DS1_aggregated.csv')
ID6844DS1<-read.csv('ID6844DS1_aggregated.csv')
ID6852DS1<-read.csv('ID6852DS1_aggregated.csv')
ID6854DS0_1992<-read.csv('ID6854DS0_1992_aggregated.csv')
ID6854DS0_1993<-read.csv('ID6854DS0_1993_aggregated.csv')
ID6854DS0_1995<-read.csv('ID6854DS0_1995_aggregated.csv')
ID6854DS0_1996<-read.csv('ID6854DS0_1996_aggregated.csv')
ID6854DS0_1998<-read.csv('ID6854DS0_1998_aggregated.csv')
ID6854DS0_2000<-read.csv('ID6854DS0_2000_aggregated.csv')
ID6854DS0_2002<-read.csv('ID6854DS0_2002_aggregated.csv')
ID6854DS0_2004<-read.csv('ID6854DS0_2004_aggregated.csv')
ID6854DS0_2006<-read.csv('ID6854DS0_2006_aggregated.csv')
ID6854DS0_2008<-read.csv('ID6854DS0_2008_aggregated.csv')
ID6854DS0_2010<-read.csv('ID6854DS0_2010_aggregated.csv')
ID6854DS0_2012<-read.csv('ID6854DS0_2012_aggregated.csv')
ID6854DS0_2014<-read.csv('ID6854DS0_2014_aggregated.csv')
ID6854DS0_2016<-read.csv('ID6854DS0_2016_aggregated.csv')
ID6854DS0_2018<-read.csv('ID6854DS0_2018_aggregated.csv')
ID6906DS4<-read.csv('ID6906DS4_aggregated.csv')
ID6906DS5<-read.csv('ID6906DS5_aggregated.csv')
ID6949DS1<-read.csv('ID6949DS1_aggregated.csv')
ID9211DS1<-read.csv('ID9211DS1_aggregated.csv')
ID9786DS1<-read.csv('ID9786DS1_aggregated.csv')
ID20240DS1<-read.csv('ID20240DS1_aggregated.csv')
ID20541DS1<-read.csv('ID20541DS1_aggregated.csv')
ID21240DS1_ACHV<-read.csv('ID21240DS1_ACHV_aggregated.csv')
ID21240DS1<-read.csv('ID21240DS1_aggregated.csv')
ID21600DS1<-read.csv('ID21600DS1_aggregated.csv')
ID21600DS5<-read.csv('ID21600DS5_aggregated.csv')
ID21600DS8<-read.csv('ID21600DS8_aggregated.csv')
ID21600DS22<-read.csv('ID21600DS22_aggregated.csv')
ID23263DS1<-read.csv('ID23263DS1_aggregated.csv')
ID23782DS1_ACHV<-read.csv('ID23782DS1_ACHV_aggregated.csv')
ID23782DS1<-read.csv('ID23782DS1_aggregated.csv')
ID25504DS209<-read.csv('ID25504DS209_aggregated.csv')
ID25505DS208<-read.csv('ID25505DS208_aggregated.csv')
ID26701DS1_ACHV<-read.csv('ID26701DS1_ACHV_aggregated.csv')
ID26701DS1<-read.csv('ID26701DS1_aggregated.csv')
ID27201DS4<-read.csv('ID27201DS4_aggregated.csv')
ID27341DS4<-read.csv('ID27341DS4_aggregated.csv')
ID28023DS1_1999<-read.csv('ID28023DS1_1999_aggregated.csv')
ID28023DS1_2002<-read.csv('ID28023DS1_2002_aggregated.csv')
ID28023DS1_2007<-read.csv('ID28023DS1_2007_aggregated.csv')
ID28721DS4<-read.csv('ID28721DS4_aggregated.csv')
ID29621DS1_ACHV<-read.csv('ID29621DS1_ACHV_aggregated.csv')
ID29621DS1<-read.csv('ID29621DS1_aggregated.csv')
ID32722DS1_ACHV<-read.csv('ID32722DS1_ACHV_aggregated.csv')
ID32722DS1<-read.csv('ID32722DS1_aggregated.csv')
ID34392DS1<-read.csv('ID34392DS1_aggregated.csv')
ID34481DS1_ACHV<-read.csv('ID34481DS1_ACHV_aggregated.csv')
ID34481DS1<-read.csv('ID34481DS1_aggregated.csv')
ID34921DS1<-read.csv('ID34921DS1_aggregated.csv')
ID34933DS1_ACHV<-read.csv('ID34933DS1_ACHV_aggregated.csv')
ID34933DS1<-read.csv('ID34933DS1_aggregated.csv')
ID35067DS1<-read.csv('ID35067DS1_aggregated.csv')
ID35509DS1_ACHV<-read.csv('ID35509DS1_ACHV_aggregated.csv')
ID35509DS1<-read.csv('ID35509DS1_aggregated.csv')
ID36144DS6<-read.csv('ID36144DS6_aggregated.csv')
ID36145DS4<-read.csv('ID36145DS4_aggregated.csv')
ID36146DS4<-read.csv('ID36146DS4_aggregated.csv')
ID36147DS4<-read.csv('ID36147DS4_aggregated.csv')
ID36346DS1<-read.csv('ID36346DS1_aggregated.csv')
ID36361DS1_ACHV<-read.csv('ID36361DS1_ACHV_aggregated.csv')
ID36361DS1<-read.csv('ID36361DS1_aggregated.csv')
ID36532DS1<-read.csv('ID36532DS1_aggregated.csv')
ID36797DS1<-read.csv('ID36797DS1_aggregated.csv')
ID36873DS1<-read.csv('ID36873DS1_aggregated.csv')

#### BIND INDIVIDUAL AGGREGATE DATA FILES ----
SESIPD_DATA<-dplyr::bind_rows(
ID171DS1_R,
ID171DS1_S,
ID182DS0_1992,
ID182DS0_1994,
ID182DS0_2006,
ID182DS0_2016,
ID182DS0_CH1986,
ID182DS0_CH1988,
ID182DS0_CH1990,
ID182DS0_CH1992,
ID182DS0_CH1994,
ID182DS0_CH1996,
ID182DS0_CH1998,
ID182DS0_CH2000,
ID182DS0_CH2002,
ID182DS0_CH2004,
ID182DS0_CH2006,
ID182DS0_CH2008,
ID182DS0_YA1996,
ID182DS0_YA1998,
ID182DS0_YA2000,
ID182DS0_YA2002,
ID182DS0_YA2004,
ID182DS0_YA2006,
ID182DS0_YA2008,
ID182DS0_YA2010,
ID182DS0_YA2012,
ID182DS0_YA2014,
ID182NLSY97_2000,
ID182NLSY97_2002,
ID182NLSY97_2004,
ID182NLSY97_2006,
ID182NLSY97_2008,
ID182NLSY97_2010,
ID182NLSY97_2015,
ID182NLSY97_2017,
ID2566DS0,
ID2954DS7,
ID3107DS4,
ID3255DS1,
ID3255DS2,
ID3334DS1_1995,
ID3334DS1_1998,
ID3334DS2,
ID3381DS4,
ID3397DS4,
ID3605DS4,
ID3903DS1_ACHV,
ID3903DS1,
ID3927DS1,
ID3927DS4,
ID4176DS4,
ID4222DS4,
ID4373DS1_ACHV,
ID4373DS1,
ID4581DS1,
ID4581DS4,
ID4582DS1,
ID4582DS4,
ID4596DS1_ACHV,
ID4596DS1,
ID4652DS1,
ID4690DS1_1986,
ID4690DS1_1989,
ID4690DS1_1994,
ID4690DS1_2002,
ID4690DS1_2012,
ID6041DS1,
ID6370DS1,
ID6844DS1,
ID6852DS1,
ID6854DS0_1992,
ID6854DS0_1993,
ID6854DS0_1995,
ID6854DS0_1996,
ID6854DS0_1998,
ID6854DS0_2000,
ID6854DS0_2002,
ID6854DS0_2004,
ID6854DS0_2006,
ID6854DS0_2008,
ID6854DS0_2010,
ID6854DS0_2012,
ID6854DS0_2014,
ID6854DS0_2016,
ID6854DS0_2018,
ID6906DS4,
ID6906DS5,
ID6949DS1,
ID9211DS1,
ID9786DS1,
ID20240DS1,
ID20541DS1,
ID21240DS1_ACHV,
ID21240DS1,
ID21600DS1,
ID21600DS5,
ID21600DS8,
ID21600DS22,
ID23263DS1,
ID23782DS1_ACHV,
ID23782DS1,
ID25504DS209,
ID25505DS208,
ID26701DS1_ACHV,
ID26701DS1,
ID27201DS4,
ID27341DS4,
ID28023DS1_1999,
ID28023DS1_2002,
ID28023DS1_2007,
ID28721DS4,
ID29621DS1_ACHV,
ID29621DS1,
ID32722DS1_ACHV,
ID32722DS1,
ID34392DS1,
ID34481DS1_ACHV,
ID34481DS1,
ID34921DS1,
ID34933DS1_ACHV,
ID34933DS1,
ID35067DS1,
ID35509DS1_ACHV,
ID35509DS1,
ID36144DS6,
ID36145DS4,
ID36146DS4,
ID36147DS4,
ID36346DS1,
ID36361DS1_ACHV,
ID36361DS1,
ID36532DS1,
ID36797DS1,
ID36873DS1,.id = "DFID")

#REORDER DATA
SESIPD_DATA<-dplyr::relocate(SESIPD_DATA,
                             DFID,DATAID,DSID,INCMCINC2,INCMCEDUMC,INCMCEDU2,INCMCOCCSMC,
                             INCMCOCCS2,INCMCOCCPMC,INCMCOCCP2,INCMCDEP,INCMCDEPPW,INCMCACHV,
                             INCMCACHVPW,INC2EDUMC,INC2EDU2,INC2OCCSMC,INC2OCCS2,INC2OCCPMC,
                             INC2OCCP2,INC2DEP,INC2DEPPW,INC2ACHV,INC2ACHVPW,EDUMCEDU2,
                             EDUMCOCCSMC,EDUMCOCCS2,EDUMCOCCPMC,EDUMCOCCP2,EDUMCDEP,EDUMCDEPPW,
                             EDUMCACHV,EDUMCACHVPW,EDU2OCCSMC,EDU2OCCS2,EDU2OCCPMC,EDU2OCCP2,
                             EDU2DEP,EDU2DEPPW,EDU2ACHV,EDU2ACHVPW,OCCSMCOCCS2,OCCSMCOCCPMC,
                             OCCSMCOCCP2,OCCSMCDEP,OCCSMCDEPPW,OCCSMCACHV,OCCSMCACHVPW,
                             OCCS2OCCPMC,OCCS2OCCP2,OCCS2DEP,OCCS2DEPPW,OCCS2ACHV,OCCS2ACHVPW,
                             OCCPMCOCCP2,OCCPMCDEP,OCCPMCDEPPW,OCCPMCACHV,OCCPMCACHVPW,OCCP2DEP,
                             OCCP2DEPPW,OCCP2ACHV,OCCP2ACHVPW,DEPDEPPW,DEPACHV,DEPACHVPW,
                             DEPPWACHV,DEPPWACHVPW,ACHVACHVPW,NINCMCINC2,NINCMCEDUMC,
                             NINCMCEDU2,NINCMCOCCSMC,NINCMCOCCS2,NINCMCOCCPMC,NINCMCOCCP2,
                             NINCMCDEP,NINCMCDEPPW,NINCMCACHV,NINCMCACHVPW,NINC2EDUMC,
                             NINC2EDU2,NINC2OCCSMC,NINC2OCCS2,NINC2OCCPMC,NINC2OCCP2,NINC2DEP,
                             NINC2DEPPW,NINC2ACHV,NINC2ACHVPW,NEDUMCEDU2,NEDUMCOCCSMC,
                             NEDUMCOCCS2,NEDUMCOCCPMC,NEDUMCOCCP2,NEDUMCDEP,NEDUMCDEPPW,
                             NEDUMCACHV,NEDUMCACHVPW,NEDU2OCCSMC,NEDU2OCCS2,NEDU2OCCPMC,
                             NEDU2OCCP2,NEDU2DEP,NEDU2DEPPW,NEDU2ACHV,NEDU2ACHVPW,NOCCSMCOCCS2,
                             NOCCSMCOCCPMC,NOCCSMCOCCP2,NOCCSMCDEP,NOCCSMCDEPPW,NOCCSMCACHV,
                             NOCCSMCACHVPW,NOCCS2OCCPMC,NOCCS2OCCP2,NOCCS2DEP,NOCCS2DEPPW,
                             NOCCS2ACHV,NOCCS2ACHVPW,NOCCPMCOCCP2,NOCCPMCDEP,NOCCPMCDEPPW,
                             NOCCPMCACHV,NOCCPMCACHVPW,NOCCP2DEP,NOCCP2DEPPW,NOCCP2ACHV,
                             NOCCP2ACHVPW,NDEPDEPPW,NDEPACHV,NDEPACHVPW,NDEPPWACHV,NDEPPWACHVPW,
                             NACHVACHVPW,AAMEAS,DEVP,MALE,RACETH,DEPMEAS,DATAYEAR,ALPHA,OMEGA)

###ATTACH IDS TO DENOTE ICPSR IDS THAT SHARE DATASETS (IPDID) AND ICPSR STUDY NAMES ----
#CREATING DATAFRAME OF ALL DATAIDS (ICPSR IDS) AND STUDY NAME WITH IPDID
idandnames<-data.frame(IPDID=c(1,3,4,6,10,11,15,15,15,17,18,18,19,19,19,19,19,19,19,19,
                               19,19,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20,20,20,
                               20,20,21,22,24,24,24,27,27,27,32,32,32,33,34,36,38,44,45,47),
                       DATAID=c(3334,4690,20240,28023,36797,6854,4652,36346,36532,35067,
                                25504,25505,2954,3107,3381,3397,3605,4176,4222,27201,27341,
                                28721,36144,36145,36146,36147,6949,3903,4373,4596,6844,6852,
                                21240,23782,26701,29621,32722,34481,34933,35509,36361,21600,
                                182,20541,34921,36873,3927,4581,4582,6906,171,6041,6370,34392,
                                9211,3255,9786,23263,2566),
                       ICPSRNAMES=as.factor(c("Aging, Status, and Sense of Control (ASOC), 1995, 1998, 2001 [United States]",
                                              "Americans' Changing Lives: Waves I, II, III, IV, and V, 1986, 1989, 1994, 2002, and 2011",
                                              "Collaborative Psychiatric Epidemiology Surveys (CPES), 2001-2003 [United States]",
                                              "Early Childhood Longitudinal Study [United States]: Kindergarten Class of 1998-1999, Kindergarten-Eighth Grade Full Sample",
                                              "General Social Survey, 1972-2016 [Cumulative File]",
                                              "Health and Retirement Study (HRS)",
                                              "Midlife in the United States (MIDUS 2), 2004-2006",
                                              "Midlife in the United States (MIDUS 3), 2013-2014",
                                              "Midlife in the United States (MIDUS Refresher), 2011-2014",
                                              "National Comorbidity Survey: Reinterview (NCS-2), 2001-2002",
                                              "National Health and Nutrition Examination Survey (NHANES), 2005-2006",
                                              "National Health and Nutrition Examination Survey (NHANES), 2007-2008",
                                              "National Health Interview Survey, 1997",
                                              "National Health Interview Survey, 1998",
                                              "National Health Interview Survey, 2000 ",
                                              "National Health Interview Survey, 1999",
                                              "National Health Interview Survey, 2001",
                                              "National Health Interview Survey, 2002",
                                              "National Health Interview Survey, 2003",
                                              "National Health Interview Survey, 2007",
                                              "National Health Interview Survey, 2008",
                                              "National Health Interview Survey, 2009",
                                              "National Health Interview Survey, 2010",
                                              "National Health Interview Survey, 2011",
                                              "National Health Interview Survey, 2012",
                                              "National Health Interview Survey, 2013",
                                              "National Household Survey on Drug Abuse, 1994",
                                              "National Survey on Drug Use and Health, 2002",
                                              "National Survey on Drug Use and Health, 2004",
                                              "National Survey on Drug Use and Health, 2005",
                                              "National Household Survey on Drug Abuse, 1985",
                                              "National Household Survey on Drug Abuse, 1993",
                                              "National Survey on Drug Use and Health, 2006",
                                              "National Survey on Drug Use and Health, 2007",
                                              "National Survey on Drug Use and Health, 2008",
                                              "National Survey on Drug Use and Health, 2009",
                                              "National Survey on Drug Use and Health, 2010",
                                              "National Survey on Drug Use and Health, 2011",
                                              "National Survey on Drug Use and Health, 2012",
                                              "National Survey on Drug Use and Health, 2013",
                                              "National Survey on Drug Use and Health, 2014",
                                              "National Longitudinal Study of Adolescent to Adult Health (Add Health), 1994-2008 [Public Use]",
                                              "National Longitudinal Survey of Youth (NLSY): Child Supplement",
                                              "National Social Life, Health, and Aging Project (NSHAP): Wave 1, [United States], July 2005-March 2006",
                                              "National Social Life, Health, and Aging Project (NSHAP): Wave 2 and Partner Data Collection, [United States], 2010-2011",
                                              "National Social Life, Health and Aging Project (NSHAP): Wave 3",
                                              "National Survey of America's Families (NSAF), 1999",
                                              "National Survey of America's Families (NSAF), 1997",
                                              "National Survey of America's Families (NSAF), 2002",
                                              "National Survey of Families and Households, Wave 2: 1992-1994, [United States]",
                                              "National Survey of Families and Households, Wave 3: 2001-2003, [United States]",
                                              "National Survey of Families and Households, Wave 1: 1987-1988, [United States]",
                                              "National Survey of Functional Health Status, 1990",
                                              "New Family Structures Study",
                                              "Physical Violence in American Families, 1985",
                                              "Religion, Aging, and Health Survey, 2001, 2004 [United States]",
                                              "Teenage Attitudes and Practices Survey, 1989: [United States]",
                                              "United States National Health Measurement Study, 2005-2006",
                                              "Violence and Threats of Violence Against Women and Men in the United States, 1994-1996"))) 
#USING MATCH FUNCTION TO ASSIGN IPDIDs TO CORRESPOND WITH DATAID IN AGGREGATE DATA FILE
ipdidvalues<-factor(c(idandnames$IPDID))  
SESIPD_DATA$IPDID<-as.numeric(as.character(ipdidvalues[match(SESIPD_DATA$DATAID, idandnames$DATAID)]))
SESIPD_DATA$ICPSRNAMES<-as.factor(idandnames$ICPSRNAMES[match(SESIPD_DATA$DATAID, idandnames$DATAID)])

###SAVE MASTER AGGREGATE DATA AS .CSV AND .RDA ----
write.csv(SESIPD_DATA, file = 'SESIPD_MASTER.csv',row.names = FALSE)
save(SESIPD_DATA, file = 'SESIPD_MASTER.rda')
