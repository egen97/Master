
rm(list=ls())
library(Amelia)
library(tcltk)
install.packages("Zelig")
library(Zelig)
install.packages("visreg")
install.packages("parallel")
library(visreg)
library(parallel)



#####################################################
######### IMPUTING DATA ##########
#####################################################
library(foreign)
setwd("~/SDimpute")
smallestsample <- read.dta("Amelia_smallestsample.dta")
names(data)
head(data)
library(parallel)
cores <- detectCores()


bounds <- matrix(NA,118,3) #logical and empirical bounds for the imputations (matrise for minimum og maximum)

bounds[1,1] <- 2 
bounds[1,2] <- min(smallestsample[,2],na.rm=T) 
bounds[1,3] <- min(smallestsample[,2],na.rm=T)
bounds[2,1] <- 3 
bounds[2,2] <- 0 
bounds[2,3] <- 1
bounds[3,1] <- 4
bounds[3,2] <- 0
bounds[3,3] <- 2
bounds[4,1] <- 5
bounds[4,2] <- 0
bounds[4,3] <- 2
bounds[5,1] <- 6
bounds[5,2] <- 0
bounds[5,3] <- 8
bounds[6,1] <- 7
bounds[6,2] <- 0
bounds[6,3] <- 2
bounds[7,1] <- 8
bounds[7,2] <- 0
bounds[7,3] <- 2
bounds[8,1] <- 9
bounds[8,2] <- min(smallestsample[,9],na.rm=T)
bounds[8,3] <- max(smallestsample[,9],na.rm=T)
bounds[9,1] <- 10
bounds[9,2] <- min(smallestsample[,10],na.rm=T)
bounds[9,3] <- max(smallestsample[,10],na.rm=T)
bounds[10,1] <- 11
bounds[10,2] <- min(smallestsample[,11],na.rm=T)
bounds[10,3] <- max(smallestsample[,11],na.rm=T)
bounds[11,1] <- 13
bounds[11,2] <- min(smallestsample[,13],na.rm=T)
bounds[11,3] <- max(smallestsample[,13],na.rm=T)
bounds[12,1] <- 14
bounds[12,2] <- min(smallestsample[,14],na.rm=T)
bounds[12,3] <- max(smallestsample[,14],na.rm=T)
bounds[13,1] <- 15
bounds[13,2] <- min(smallestsample[,15],na.rm=T)
bounds[13,3] <- max(smallestsample[,15],na.rm=T)
bounds[14,1] <- 16
bounds[14,2] <- min(smallestsample[,16],na.rm=T)
bounds[14,3] <- max(smallestsample[,16],na.rm=T)
bounds[15,1] <- 17
bounds[15,2] <- min(smallestsample[,17],na.rm=T)
bounds[15,3] <- max(smallestsample[,17],na.rm=T)
bounds[16,1] <- 18
bounds[16,2] <- min(smallestsample[,18],na.rm=T)
bounds[16,3] <- max(smallestsample[,18],na.rm=T)
bounds[17,1] <- 19
bounds[17,2] <- min(smallestsample[,19],na.rm=T)
bounds[17,3] <- max(smallestsample[,19],na.rm=T)
bounds[18,1] <- 20
bounds[18,2] <- min(smallestsample[,20],na.rm=T)
bounds[18,3] <- max(smallestsample[,20],na.rm=T)
bounds[19,1] <- 21
bounds[19,2] <- min(smallestsample[,21],na.rm=T)
bounds[19,3] <- max(smallestsample[,21],na.rm=T)
bounds[20,1] <- 22
bounds[20,2] <- min(smallestsample[,22],na.rm=T)
bounds[20,3] <- max(smallestsample[,22],na.rm=T)
bounds[21,1] <- 23
bounds[21,2] <- min(smallestsample[,23],na.rm=T)
bounds[21,3] <- max(smallestsample[,23],na.rm=T)
bounds[22,1] <- 24
bounds[22,2] <- min(smallestsample[,24],na.rm=T)
bounds[22,3] <- max(smallestsample[,24],na.rm=T)
bounds[23,1] <- 25
bounds[23,2] <- min(smallestsample[,25],na.rm=T)
bounds[23,3] <- max(smallestsample[,25],na.rm=T)
bounds[24,1] <- 26
bounds[24,2] <- min(smallestsample[,26],na.rm=T)
bounds[24,3] <- max(smallestsample[,26],na.rm=T)
bounds[25,1] <- 27
bounds[25,2] <- min(smallestsample[,27],na.rm=T)
bounds[25,3] <- max(smallestsample[,27],na.rm=T)
bounds[26,1] <- 31
bounds[26,2] <- max(smallestsample[,31],na.rm=T)
bounds[26,3] <- max(smallestsample[,31],na.rm=T)
bounds[27,1] <- 32
bounds[27,2] <- min(smallestsample[,32],na.rm=T)
bounds[27,3] <- max(smallestsample[,32],na.rm=T)
bounds[28,1] <- 33
bounds[28,2] <- min(smallestsample[,33],na.rm=T)
bounds[28,3] <- max(smallestsample[,33],na.rm=T)
bounds[29,1] <- 34
bounds[29,2] <- max(smallestsample[,34],na.rm=T)
bounds[29,3] <- max(smallestsample[,34],na.rm=T)
bounds[30,1] <- 35
bounds[30,2] <- max(smallestsample[,35],na.rm=T)
bounds[30,3] <- max(smallestsample[,35],na.rm=T)
bounds[31,1] <- 36
bounds[31,2] <- min(smallestsample[,36],na.rm=T)
bounds[31,3] <- max(smallestsample[,36],na.rm=T)
bounds[32,1] <- 37
bounds[32,2] <- min(smallestsample[,37],na.rm=T)
bounds[32,3] <- max(smallestsample[,37],na.rm=T)
bounds[33,1] <- 38
bounds[33,2] <- min(smallestsample[,38],na.rm=T)
bounds[33,3] <- max(smallestsample[,38],na.rm=T)
bounds[34,1] <- 39
bounds[34,2] <- min(smallestsample[,39],na.rm=T)
bounds[34,3] <- max(smallestsample[,39],na.rm=T)
bounds[35,1] <- 40
bounds[35,2] <- min(smallestsample[,40],na.rm=T)#na.rm=T fjerner missing
bounds[35,3] <- max(smallestsample[,40],na.rm=T)
bounds[36,1] <- 41
bounds[36,2] <- min(smallestsample[,41],na.rm=T)
bounds[36,3] <- max(smallestsample[,41],na.rm=T)
bounds[37,1] <- 42
bounds[37,2] <- max(smallestsample[,42],na.rm=T)
bounds[37,3] <- max(smallestsample[,42],na.rm=T)
bounds[38,1] <- 43
bounds[38,2] <- max(smallestsample[,43],na.rm=T)
bounds[38,3] <- max(smallestsample[,43],na.rm=T)
bounds[39,1] <- 44
bounds[39,2] <- min(smallestsample[,44],na.rm=T)
bounds[39,3] <- max(smallestsample[,44],na.rm=T)
bounds[40,1] <- 45
bounds[40,2] <- min(smallestsample[,45],na.rm=T)
bounds[40,3] <- max(smallestsample[,45],na.rm=T)
bounds[41,1] <- 51
bounds[41,2] <- min(smallestsample[,51],na.rm=T)
bounds[41,3] <- max(smallestsample[,51],na.rm=T)
bounds[42,1] <- 52
bounds[42,2] <- min(smallestsample[,52],na.rm=T)
bounds[42,3] <- max(smallestsample[,52],na.rm=T)
bounds[43,1] <- 53
bounds[43,2] <- min(smallestsample[,53],na.rm=T)
bounds[43,3] <- max(smallestsample[,53],na.rm=T)
bounds[44,1] <- 54
bounds[44,2] <- min(smallestsample[,54],na.rm=T)
bounds[44,3] <- max(smallestsample[,54],na.rm=T)
bounds[45,1] <- 55
bounds[45,2] <- min(smallestsample[,55],na.rm=T)
bounds[45,3] <- max(smallestsample[,55],na.rm=T)
bounds[46,1] <- 56
bounds[46,2] <- min(smallestsample[,56],na.rm=T)
bounds[46,3] <- max(smallestsample[,56],na.rm=T)
bounds[47,1] <- 57
bounds[47,2] <- min(smallestsample[,57],na.rm=T)
bounds[47,3] <- max(smallestsample[,57],na.rm=T)
bounds[48,1] <- 58
bounds[48,2] <- min(smallestsample[,58],na.rm=T)
bounds[48,3] <- max(smallestsample[,58],na.rm=T)
bounds[49,1] <- 59
bounds[49,2] <- min(smallestsample[,59],na.rm=T)
bounds[49,3] <- max(smallestsample[,59],na.rm=T)
bounds[50,1] <- 60
bounds[50,2] <- min(smallestsample[,60],na.rm=T)
bounds[50,3] <- max(smallestsample[,60],na.rm=T)
bounds[51,1] <- 61
bounds[51,2] <- min(smallestsample[,61],na.rm=T)
bounds[51,3] <- max(smallestsample[,61],na.rm=T)
bounds[52,1] <- 62
bounds[52,2] <- min(smallestsample[,62],na.rm=T)#na.rm=T fjerner missing
bounds[52,3] <- max(smallestsample[,62],na.rm=T)
bounds[53,1] <- 63
bounds[53,2] <- min(smallestsample[,63],na.rm=T)
bounds[53,3] <- max(smallestsample[,63],na.rm=T)
bounds[54,1] <- 64
bounds[54,2] <- min(smallestsample[,64],na.rm=T)
bounds[54,3] <- max(smallestsample[,64],na.rm=T)
bounds[55,1] <- 65
bounds[55,2] <- min(smallestsample[,65],na.rm=T)
bounds[55,3] <- max(smallestsample[,65],na.rm=T)
bounds[56,1] <- 66
bounds[56,2] <- min(smallestsample[,66],na.rm=T)
bounds[56,3] <- max(smallestsample[,66],na.rm=T)
bounds[57,1] <- 67
bounds[57,2] <- min(smallestsample[,67],na.rm=T)
bounds[57,3] <- max(smallestsample[,67],na.rm=T)
bounds[58,1] <- 68
bounds[58,2] <- min(smallestsample[,68],na.rm=T)
bounds[58,3] <- max(smallestsample[,68],na.rm=T)
bounds[59,1] <- 69
bounds[59,2] <- min(smallestsample[,69],na.rm=T)
bounds[59,3] <- max(smallestsample[,69],na.rm=T)
bounds[60,1] <- 70
bounds[60,2] <- min(smallestsample[,70],na.rm=T)
bounds[60,3] <- max(smallestsample[,70],na.rm=T)
bounds[61,1] <- 71
bounds[61,2] <- min(smallestsample[,71],na.rm=T)
bounds[61,3] <- max(smallestsample[,71],na.rm=T)
bounds[62,1] <- 72
bounds[62,2] <- min(smallestsample[,72],na.rm=T)
bounds[62,3] <- max(smallestsample[,72],na.rm=T)
bounds[63,1] <- 73
bounds[63,2] <- min(smallestsample[,73],na.rm=T)
bounds[63,3] <- max(smallestsample[,73],na.rm=T)
bounds[64,1] <- 74
bounds[64,2] <- min(smallestsample[,74],na.rm=T)
bounds[64,3] <- max(smallestsample[,74],na.rm=T)
bounds[65,1] <- 75
bounds[65,2] <- min(smallestsample[,75],na.rm=T)
bounds[65,3] <- max(smallestsample[,75],na.rm=T)
bounds[66,1] <- 76
bounds[66,2] <- min(smallestsample[,76],na.rm=T)
bounds[66,3] <- max(smallestsample[,76],na.rm=T)
bounds[67,1] <- 77
bounds[67,2] <- min(smallestsample[,77],na.rm=T)
bounds[67,3] <- max(smallestsample[,77],na.rm=T)
bounds[68,1] <- 78
bounds[68,2] <- min(smallestsample[,78],na.rm=T)
bounds[68,3] <- max(smallestsample[,78],na.rm=T)
bounds[69,1] <- 79
bounds[69,2] <- min(smallestsample[,79],na.rm=T)#na.rm=T fjerner missing
bounds[69,3] <- max(smallestsample[,79],na.rm=T)
bounds[70,1] <- 80
bounds[70,2] <- min(smallestsample[,80],na.rm=T)
bounds[70,3] <- max(smallestsample[,80],na.rm=T)
bounds[71,1] <- 81
bounds[71,2] <- min(smallestsample[,81],na.rm=T)
bounds[71,3] <- max(smallestsample[,81],na.rm=T)
bounds[72,1] <- 82
bounds[72,2] <- min(smallestsample[,82],na.rm=T)
bounds[72,3] <- max(smallestsample[,82],na.rm=T)
bounds[73,1] <- 83
bounds[73,2] <- min(smallestsample[,83],na.rm=T)
bounds[73,3] <- max(smallestsample[,83],na.rm=T)
bounds[74,1] <- 84
bounds[74,2] <- min(smallestsample[,84],na.rm=T)
bounds[74,3] <- max(smallestsample[,84],na.rm=T)
bounds[75,1] <- 85
bounds[75,2] <- min(smallestsample[,85],na.rm=T)
bounds[75,3] <- max(smallestsample[,85],na.rm=T)
bounds[76,1] <- 86
bounds[76,2] <- min(smallestsample[,86],na.rm=T)
bounds[76,3] <- max(smallestsample[,86],na.rm=T)
bounds[77,1] <- 87
bounds[77,2] <- min(smallestsample[,87],na.rm=T)
bounds[77,3] <- max(smallestsample[,87],na.rm=T)
bounds[78,1] <- 88
bounds[78,2] <- min(smallestsample[,88],na.rm=T)
bounds[78,3] <- max(smallestsample[,88],na.rm=T)
bounds[79,1] <- 89
bounds[79,2] <- min(smallestsample[,89],na.rm=T)
bounds[79,3] <- max(smallestsample[,89],na.rm=T)
bounds[80,1] <- 90
bounds[80,2] <- min(smallestsample[,90],na.rm=T)
bounds[80,3] <- max(smallestsample[,90],na.rm=T)
bounds[81,1] <- 91
bounds[81,2] <- min(smallestsample[,91],na.rm=T)
bounds[81,3] <- max(smallestsample[,91],na.rm=T)
bounds[82,1] <- 92
bounds[82,2] <- min(smallestsample[,92],na.rm=T)
bounds[82,3] <- max(smallestsample[,92],na.rm=T)
bounds[83,1] <- 93
bounds[83,2] <- min(smallestsample[,93],na.rm=T)
bounds[83,3] <- max(smallestsample[,93],na.rm=T)
bounds[84,1] <- 94
bounds[84,2] <- min(smallestsample[,94],na.rm=T)
bounds[84,3] <- max(smallestsample[,94],na.rm=T)
bounds[85,1] <- 95
bounds[85,2] <- min(smallestsample[,95],na.rm=T)
bounds[85,3] <- max(smallestsample[,95],na.rm=T)
bounds[86,1] <- 96
bounds[86,2] <- min(smallestsample[,96],na.rm=T)#na.rm=T fjerner missing
bounds[86,3] <- max(smallestsample[,96],na.rm=T)
bounds[87,1] <- 97
bounds[87,2] <- min(smallestsample[,97],na.rm=T)
bounds[87,3] <- max(smallestsample[,97],na.rm=T)
bounds[88,1] <- 98
bounds[88,2] <- min(smallestsample[,98],na.rm=T)
bounds[88,3] <- max(smallestsample[,98],na.rm=T)
bounds[89,1] <- 99
bounds[89,2] <- min(smallestsample[,99],na.rm=T)
bounds[89,3] <- max(smallestsample[,99],na.rm=T)
bounds[90,1] <- 100
bounds[90,2] <- min(smallestsample[,100],na.rm=T)
bounds[90,3] <- max(smallestsample[,100],na.rm=T)
bounds[91,1] <- 101
bounds[91,2] <- min(smallestsample[,101],na.rm=T)
bounds[91,3] <- max(smallestsample[,101],na.rm=T)
bounds[92,1] <- 102
bounds[92,2] <- min(smallestsample[,102],na.rm=T)
bounds[92,3] <- max(smallestsample[,102],na.rm=T)
bounds[93,1] <- 103
bounds[93,2] <- min(smallestsample[,103],na.rm=T)
bounds[93,3] <- max(smallestsample[,103],na.rm=T)
bounds[94,1] <- 104
bounds[94,2] <- min(smallestsample[,104],na.rm=T)
bounds[94,3] <- max(smallestsample[,104],na.rm=T)
bounds[95,1] <- 105
bounds[95,2] <- min(smallestsample[,105],na.rm=T)
bounds[95,3] <- max(smallestsample[,105],na.rm=T)
bounds[96,1] <- 106
bounds[96,2] <- min(smallestsample[,106],na.rm=T)
bounds[96,3] <- max(smallestsample[,106],na.rm=T)
bounds[97,1] <- 107
bounds[97,2] <- min(smallestsample[,107],na.rm=T)
bounds[97,3] <- max(smallestsample[,107],na.rm=T)
bounds[98,1] <- 108
bounds[98,2] <- min(smallestsample[,108],na.rm=T)
bounds[98,3] <- max(smallestsample[,108],na.rm=T)
bounds[99,1] <- 109
bounds[99,2] <- min(smallestsample[,109],na.rm=T)
bounds[99,3] <- max(smallestsample[,109],na.rm=T)
bounds[100,1] <- 110
bounds[100,2] <- min(smallestsample[,110],na.rm=T)
bounds[100,3] <- max(smallestsample[,110],na.rm=T)
bounds[101,1] <- 111
bounds[101,2] <- min(smallestsample[,111],na.rm=T)
bounds[101,3] <- max(smallestsample[,111],na.rm=T)
bounds[102,1] <- 112
bounds[102,2] <- min(smallestsample[,112],na.rm=T)
bounds[102,3] <- max(smallestsample[,112],na.rm=T)
bounds[103,1] <- 113
bounds[103,2] <- min(smallestsample[,113],na.rm=T)
bounds[103,3] <- max(smallestsample[,113],na.rm=T)
bounds[104,1] <- 114
bounds[104,2] <- min(smallestsample[,114],na.rm=T)
bounds[104,3] <- max(smallestsample[,114],na.rm=T)
bounds[105,1] <- 115
bounds[105,2] <- min(smallestsample[,115],na.rm=T)
bounds[105,3] <- max(smallestsample[,115],na.rm=T)
bounds[106,1] <- 116
bounds[106,2] <- min(smallestsample[,116],na.rm=T)
bounds[106,3] <- max(smallestsample[,116],na.rm=T)
bounds[107,1] <- 117
bounds[107,2] <- min(smallestsample[,117],na.rm=T)
bounds[107,3] <- max(smallestsample[,117],na.rm=T)
bounds[108,1] <- 118
bounds[108,2] <- min(smallestsample[,118],na.rm=T)
bounds[108,3] <- max(smallestsample[,118],na.rm=T)
bounds[109,1] <- 119
bounds[109,2] <- min(smallestsample[,119],na.rm=T)
bounds[109,3] <- max(smallestsample[,119],na.rm=T)
bounds[110,1] <- 120
bounds[110,2] <- min(smallestsample[,120],na.rm=T)
bounds[110,3] <- max(smallestsample[,120],na.rm=T)
bounds[111,1] <- 121
bounds[111,2] <- min(smallestsample[,121],na.rm=T)
bounds[111,3] <- max(smallestsample[,121],na.rm=T)
bounds[112,1] <- 122
bounds[112,2] <- min(smallestsample[,122],na.rm=T)
bounds[112,3] <- max(smallestsample[,122],na.rm=T)
bounds[113,1] <- 123
bounds[113,2] <- min(smallestsample[,123],na.rm=T)#na.rm=T fjerner missing
bounds[113,3] <- max(smallestsample[,123],na.rm=T)
bounds[114,1] <- 124
bounds[114,2] <- min(smallestsample[,124],na.rm=T)
bounds[114,3] <- min(smallestsample[,124],na.rm=T)
bounds[115,1] <- 126
bounds[115,2] <- min(smallestsample[,126],na.rm=T)
bounds[115,3] <- min(smallestsample[,126],na.rm=T)
bounds[116,1] <- 129
bounds[116,2] <- min(smallestsample[,129],na.rm=T)
bounds[116,3] <- min(smallestsample[,129],na.rm=T)
bounds[117,1] <- 131
bounds[117,2] <- min(smallestsample[,131],na.rm=T)
bounds[117,3] <- min(smallestsample[,131],na.rm=T)
bounds[118,1] <- 132
bounds[118,2] <- min(smallestsample[,132],na.rm=T)
bounds[118,3] <- min(smallestsample[,132],na.rm=T)



set.seed(12345)
smallestsample <- amelia(x=smallestsample,m=5,ts="year",cs="imfcode",
                         max.resample=100,ords=c("ciri_disap", "ciri_kill","ciri_physint","ciri_polpris","ciri_tort","fh_status", "h_l1", "ht_regtype"),polytime=2,intercs=T,empri=0.05*nrow(smallestsample),
                         lags=c("al_ethnic","fh_cl","fh_pr","fh_rol","lp_muslim80","lp_protmg80","p_polity2","wdi_fe","wdi_gini","van_mean","wdi_gdp","yr_sch","Banksmediascale","ms_mil_xpnd_zs","se_xpd_totl_gb_zs","sh_xpd_publ_gx_zs","trust2","post_materialist2","happiness2","petitions2","satisfaction2","liberty_asp2","selfexpressionA1","selfexpressionB1","FreedomHouse"),
                         leads=c("al_ethnic","fh_cl","fh_pr","fh_rol","lp_muslim80","lp_protmg80","p_polity2","wdi_fe","wdi_gini","van_mean", "wdi_gdp","yr_sch","Banksmediascale","ms_mil_xpnd_zs","se_xpd_totl_gb_zs","sh_xpd_publ_gx_zs","trust2","post_materialist2","happiness2","petitions2","satisfaction2","liberty_asp2","selfexpressionA1","selfexpressionB1","FreedomHouse"),
                         idvars=c("ccode","ID","id","s001","s002","s002evs","s003","s025","inWVS_EVS","inRegional","inWVS_EVS2"),
                         startvals=0,tolerance=0.0001,sqrts=NULL,lgstc=NULL,incheck=TRUE,collect=FALSE,arglist=NULL,autopri=0.10,emburn=c(0,0),bounds=bounds,overimp=NULL)

write.amelia(smallestsample, file.stem ="imputed26mai", format = "dta") 

write.amelia(smallestsample, file.stem="imputed28mai", format="csv")
write.amelia(smallestsample, file.stem="imputed28mai", format="dta")
write.amelia(smallestsample, separate=FALSE, file.stem="imputed28maiMERGED", format="dta")
save.image("imputedsmallestsample28mai.Rdata")
load("imputedsmallestsample28mai.Rdata")
