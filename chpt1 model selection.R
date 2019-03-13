# chpt1 model selection code

### MODEL SELECTION CODE 

library(MuMIn)
options(na.action = "na.fail") # THIS IS REQUIRED FOR THE DREDGE FUNCTION
rm(list=ls(all=TRUE))
getwd()
setwd("/Users/samjordan/Dropbox/Biodiversity 2016 Data/misc")
setwd("C:/Users/sej36/Dropbox/Biodiversity 2016 Data/misc")

all.groups <- read.csv(file = "summary_stats_by_FG.csv", header = TRUE)
dat <- read.csv(file = "Predictors_final.csv", header = TRUE)
#merge functional type richness at 1000m2
dat$grass1000 <- all.groups$grass.mean1000
dat$forb1000 <- all.groups$forb.mean1000
dat$shrub1000 <- all.groups$shrubs.mean1000


# Ecohydrological models ####
# dataframe for correlation matrix
# new eco.all to omit ec.total 
eco.all <- cbind.data.frame(dat$slope , dat$aspect , dat$elevation,
                            dat$sand.total , dat$silt.total , dat$clay.total , 
                            dat$SWP_bottomLayers_DailyMax_MPa_mean , dat$SWP_topLayers_DailyMin_MPa_mean,
                            dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean ,
                            dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean , dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_Duration_Total_months_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AllLayersWet_Duration_Total_days_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_AllLayersDry_Duration_Total_days_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_topLayers_Duration_days_quantile0_5 ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_bottomLayers_Duration_days_quantile0_5 ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_topLayers_count_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_bottomLayers_count_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m10.3_mm_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m4.9_mm_mean)


# Make global models with all possible predictors
emd <- lm(dat$mean1000 ~ dat$elevation + dat$slope + dat$aspect +
            dat$sand.total + dat$silt.total + dat$clay.total + #dat$ec.total + # EC NOT USED IN FINAL ANLAYSIS
            dat$SWP_bottomLayers_DailyMax_MPa_mean +
            dat$SWP_topLayers_DailyMin_MPa_mean +
            dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean +
            dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean + dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_Duration_Total_months_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AllLayersWet_Duration_Total_days_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_AllLayersDry_Duration_Total_days_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_topLayers_Duration_days_quantile0_5 +
            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_bottomLayers_Duration_days_quantile0_5 +
            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_topLayers_count_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_bottomLayers_count_mean +
            dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean +
            dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean +
            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m10.3_mm_mean +
            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m4.9_mm_mean)

eg <- lm(dat$grass1000 ~  dat$slope + dat$aspect +
            dat$sand.total + dat$silt.total + dat$clay.total + dat$elevation+# dat$ec.total   OMITTED EC 
            dat$SWP_bottomLayers_DailyMax_MPa_mean +
            dat$SWP_topLayers_DailyMin_MPa_mean +
            dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean +
            dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean + dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_Duration_Total_months_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AllLayersWet_Duration_Total_days_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean +
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_AllLayersDry_Duration_Total_days_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_topLayers_Duration_days_quantile0_5 +
            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_bottomLayers_Duration_days_quantile0_5 +
            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_topLayers_count_mean +
            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_bottomLayers_count_mean +
            dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean +
            dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean +
            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m10.3_mm_mean +
            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m4.9_mm_mean)

ef <- lm(dat$forb1000 ~ dat$elevation + dat$slope + dat$aspect +
           dat$sand.total + dat$silt.total + dat$clay.total + #dat$ec.total +
           dat$SWP_bottomLayers_DailyMin_MPa_mean + 
           dat$SWP_topLayers_DailyMin_MPa_mean +
           dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean +
           dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean + dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_Duration_Total_months_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AllLayersWet_Duration_Total_days_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_AllLayersDry_Duration_Total_days_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_topLayers_Duration_days_quantile0_5 +
           dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_bottomLayers_Duration_days_quantile0_5 +
           dat$DrySoilPeriods_SWPcrit3900kPa_Annual_topLayers_count_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_Annual_bottomLayers_count_mean +
           dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean +
           dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean +
           dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m10.3_mm_mean +
           dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m4.9_mm_mean)


es <- lm(dat$shrub1000 ~ dat$elevation + dat$slope + dat$aspect +
           dat$sand.total + dat$silt.total + dat$clay.total +# dat$ec.total +
           dat$SWP_bottomLayers_DailyMin_MPa_mean + 
           dat$SWP_topLayers_DailyMin_MPa_mean +
           dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean +
           dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean + dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_Duration_Total_months_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AllLayersWet_Duration_Total_days_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean +
           dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_AllLayersDry_Duration_Total_days_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_topLayers_Duration_days_quantile0_5 +
           dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_bottomLayers_Duration_days_quantile0_5 +
           dat$DrySoilPeriods_SWPcrit3900kPa_Annual_topLayers_count_mean +
           dat$DrySoilPeriods_SWPcrit3900kPa_Annual_bottomLayers_count_mean +
           dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean +
           dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean +
           dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m10.3_mm_mean +
           dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m4.9_mm_mean)
### Make a correlation matrix that excludes predictors correlated over .6 from the same model
smat <- abs(cor(eco.all)) <= .6
smat[!lower.tri(smat)] <- NA
# FOUR TERM DREDGES
# m.lim limits the number of terms in the model

dredge4 <- dredge(emd, subset = smat, m.lim = c(0,4)) #worked
grass.dredge4 <- dredge(eg, subset = smat, m.lim = c(0,4)) #worked
forb.dredge4 <- dredge(ef, subset = smat, m.lim = c(0,4)) #worked
shrub.dredge4 <- dredge(es, subset = smat, m.lim = c(0,4)) #worked
# THREE TERM DREDGES
dredge3 <- dredge(emd, subset = smat, m.lim = c(0,3)) #worked
grass.dredge3 <- dredge(eg, subset = smat, m.lim = c(0,3)) #worked
forb.dredge3 <- dredge(ef, subset = smat, m.lim = c(0,4)) #worked
shrub.dredge3 <- dredge(es, subset = smat, m.lim = c(0,4)) #worked
# TWO TERM DREDGES
forb.dredge2 <- dredge(ef, subset = smat, m.lim = c(0,2)) #worked
shrub.dredge2 <- dredge(es, subset = smat, m.lim = c(0,2))


#write.csv(grass.dredge3, file = "/Users/samjordan/Dropbox/Biodiversity/Chapter 1/Analysis/Grass_ecohydro_3terms.csv")


#Macroclimate models ####

mf <- lm(dat$forb1000 ~ dat$MAP + dat$MAT + dat$AET_mm_mean +dat$Rain_mm_mean+
     dat$Snowfall_mm_mean + dat$UNAridityIndex_Annual_none_mean +
     dat$Seasonality_monthlyTandPPT_PearsonCor_mean)

ms <- lm(dat$shrub1000 ~ dat$MAP + dat$MAT + dat$AET_mm_mean +dat$Rain_mm_mean+
           dat$Snowfall_mm_mean + dat$UNAridityIndex_Annual_none_mean +
           dat$Seasonality_monthlyTandPPT_PearsonCor_mean)
#dataframe for correlation matrix
mc.all <- cbind.data.frame(dat$MAP , dat$MAT , dat$AET_mm_mean ,dat$Rain_mm_mean,
                             dat$Snowfall_mm_mean , dat$UNAridityIndex_Annual_none_mean ,
                             dat$Seasonality_monthlyTandPPT_PearsonCor_mean)

#correlation matrix, see above
mc.cor <- abs(cor(mc.all)) <= .6
mc.cor[!lower.tri(mc.cor)] <- NA
#i <- as.vector(mc.cor == FALSE & !is.na(mc.cor)) #unecessary

forb.macroclimate.dredge4 <- dredge(mf, subset = mc.cor, m.lim = c(0,4)) #worked
shrub.macroclimate.dredge4 <- dredge(ms, subset = mc.cor, m.lim = c(0,4)) #worked

# Biotic Models ####
#grass

bg <- lm(dat$grass1000 ~ dat$maxage + dat$ARTR.mean.volume + dat$ARTR.median.volume +
            dat$shrub.mean.volume + dat$shrub.median.volume + dat$ARTR.density +
            dat$shrub.density + dat$ARTR_cover)
biotic.all <- cbind.data.frame(dat$maxage , dat$ARTR.mean.volume , dat$ARTR.median.volume ,
  dat$shrub.mean.volume , dat$shrub.median.volume , dat$ARTR.density ,
  dat$shrub.density , dat$ARTR_cover)


bg.cor <- abs(cor(biotic.all)) <= .6
bg.cor[!lower.tri(bg.cor)] <- NA
i <- as.vector(bg.cor == FALSE & !is.na(bg.cor))

grass.biotic.dredge4 <- dredge(bg, subset = bg.cor, m.lim = c(0,4)) #worked

# Combined Models ####
#total
# THESE TERMS HAVE BEEN HAND-PICKED FROM THE FINAL MODELS CODE
# ONLY THE TERMS IN THOSE FINAL MODELS APPEAR HERE
# MODELS WERE SELECTED FROM THE DREDGES WITH ALL SIGNIFICANT TERMS
ct <- lm(dat$mean1000 ~ dat$MAT+ dat$MAP+ dat$UNAridityIndex_Annual_none_mean+
                                   dat$Snowfall_mm_mean + dat$Rain_mm_mean + 
                                     dat$clay.total + dat$elevation +
                                     dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean +
                                     dat$SWP_bottomLayers_DailyMax_MPa_mean +
                                     dat$SWP_topLayers_DailyMin_MPa_mean +
                                     dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean)


combined.total <- cbind.data.frame(dat$MAT, dat$MAP, dat$UNAridityIndex_Annual_none_mean,
                                   dat$Snowfall_mm_mean , dat$Rain_mm_mean , 
                                   dat$clay.total , dat$elevation ,
                                   dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean ,
                                   dat$SWP_bottomLayers_DailyMax_MPa_mean ,
                                   dat$SWP_topLayers_DailyMin_MPa_mean,
                                   dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean)

ct.cor <- abs(cor(combined.total)) <= .6
ct.cor[!lower.tri(ct.cor)] <- NA
#i <- as.vector(ct.cor == FALSE & !is.na(ct.cor))
dredge4.combined.total <- dredge(ct, subset = ct.cor, m.lim = c(0,4)) #worked
dredge3.combined.total <- dredge(ct, subset = ct.cor, m.lim = c(0,3)) #worked

#grass

cg <- lm(dat$grass1000 ~ dat$MAP+ dat$UNAridityIndex_Annual_none_mean+
         dat$Rain_mm_mean+ dat$AET_mm_mean+
         dat$elevation + dat$slope+
         dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean +
         dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean+
         dat$SWP_topLayers_DailyMin_MPa_mean+
         dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean+
         dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean+
           dat$ARTR.density +
           dat$maxage)


combined.grass <- cbind.data.frame(dat$MAP, dat$UNAridityIndex_Annual_none_mean,
                                     dat$Rain_mm_mean, dat$AET_mm_mean,
                                     dat$elevation , dat$slope,
                                   dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean ,
                                     dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean,
                                   dat$SWP_topLayers_DailyMin_MPa_mean,
                                    dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean,
                                     dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean,
                                     dat$ARTR.density ,
                                     dat$maxage)

cg.cor <- abs(cor(combined.grass)) <= .6
cg.cor[!lower.tri(cg.cor)] <- NA
#i <- as.vector(cg.cor == FALSE & !is.na(cg.cor))
dredge4.combined.grass <- dredge(cg, subset = cg.cor, m.lim = c(0,4)) #worked
#testdredge.combined.grass <- dredge(ct, m.lim = c(0,4)) #worked

#forb

cf <- lm(dat$forb1000 ~ dat$MAT+ dat$UNAridityIndex_Annual_none_mean+
                         dat$Snowfall_mm_mean + dat$Rain_mm_mean + 
                         dat$AET_mm_mean+
                         dat$clay.total + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean+
                         dat$sand.total+ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean+
                         dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean+
                         dat$SWP_topLayers_DailyMin_MPa_mean)


combined.forb <- cbind.data.frame(dat$MAT, dat$UNAridityIndex_Annual_none_mean,
                                   dat$Snowfall_mm_mean , dat$Rain_mm_mean , 
                                   dat$AET_mm_mean,
                                   dat$clay.total , dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean,
                                   dat$sand.total, dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean,
                                   dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean ,
                                   dat$SWP_topLayers_DailyMin_MPa_mean)

cf.cor <- abs(cor(combined.forb)) <= .6
cf.cor[!lower.tri(cf.cor)] <- NA
i <- as.vector(cf.cor == FALSE & !is.na(cf.cor))
dredge4.combined.forb <- dredge(cf, subset = cf.cor, m.lim = c(0,4))

#shrub

cs <- lm(dat$shrub1000 ~  dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean + 
           dat$slope + dat$elevation +  dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean )
combined.shrub <-  cbind.data.frame(dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean , 
  dat$slope , dat$elevation ,  dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean)
cs.cor <- abs(cor(combined.shrub)) <= .6
cs.cor[!lower.tri(cs.cor)] <- NA
i <- as.vector(cs.cor == FALSE & !is.na(cs.cor))
dredge4.combined.shrub <- dredge(cs, subset = cs.cor, m.lim = c(0,4))



#OLD CODE 

# CORRELATION MATRICES INCLUDED EC
#old eco.all including ec

eco.all <- cbind.data.frame(dat$elevation , dat$slope , dat$aspect ,
                            dat$sand.total , dat$silt.total , dat$clay.total , dat$ec.total ,
                            dat$SWP_bottomLayers_DailyMax_MPa_mean , dat$SWP_topLayers_DailyMin_MPa_mean,
                            dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean ,
                            dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean , dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_Duration_Total_months_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AllLayersWet_Duration_Total_days_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean ,
                            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_topLayers_AllLayersDry_Duration_Total_days_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_topLayers_Duration_days_quantile0_5 ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_MonthJJA_bottomLayers_Duration_days_quantile0_5 ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_topLayers_count_mean ,
                            dat$DrySoilPeriods_SWPcrit3900kPa_Annual_bottomLayers_count_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m10.3_mm_mean ,
                            dat$SWAbulk_SWPcrit3900kPa_bottomLayers_m4.9_mm_mean)
