# this script was written by Sam Jordan and has large redundant sections

#Chpt 1 appendix 7

#BIVARIATE SCATTERS OF TERMS IN TOP MODELS WITH TOTAL AND FT RICHNESS

#setwd("/Users/samjordan/Dropbox/Biodiversity 2016 Data/misc")
setwd("C:/Users/sej36/Dropbox/Biodiversity 2016 Data/misc")

all.groups <- read.csv(file = "summary_stats_by_FG.csv", header = TRUE)
dat <- read.csv(file = "Predictors_final.csv", header = TRUE)
dat$grass1000 <- all.groups$grass.mean1000
dat$forb1000 <- all.groups$forb.mean1000
dat$shrub1000 <- all.groups$shrubs.mean1000

library(corrplot)


#### CORRELATION DOTPLOT

#spp richness and all vars used in appendix 
corrplot(cor(dat[,c(2,43,28,29,18,3,31, 21, 20, 23, 25, 26)]), tl.cex = .1,
         type = "upper",# method = "color",
         order = "hclust",
         col=c("black", "white"),
         bg="lightblue",
         tl.col="black", tl.srt=45,
         diag = FALSE)


###### ALL RICHNESS TYPES AND ALL VARS

corrplot(cor(dat[,c(2, 50, 49, 48, 
                    43,28,29,18,3,31, 21, 20, 23, 25, 26, #from total richness
                   16, 18, 24, 35, #forbs
                   14, 44, 8, 13, #grasses
                   38,33 #shrubs
                   )]), tl.cex = .01,
         type = "upper",# method = "color",
        order = "hclust",
         col=c("black", "white"),
         bg="lightblue",
         tl.col="black", tl.srt=45,
         diag = FALSE)

# version two

dat2 <- cor(dat[,c(2, 50, 49, 48, 
                   43,28,29,18,3,31, 21, 20, 23, 25, 26, #from total richness
                   16, 18, 24, 35, #forbs
                   14, 44, 8, 13, #grasses
                   38,33 #shrubs
)])

res1 <- cor.mtest(dat2, conf.level = .95)
res2 <- cor.mtest(dat2, conf.level = .95)

corrplot(dat2, tl.cex = .01,
         type = "upper", method = "color", outline = TRUE,
         #order = "AOE",
         #col=c("black", "white"),
         bg= "white", #bg="lightblue",
         tl.col="white", tl.srt=45,
         diag = FALSE,
         cl.pos = "b")


corrplot(dat2, tl.cex = .01,
                    type = "upper", method = "square", outline = TRUE,
                    order = "AOE",
                    #col=c("black", "white"),
                    bg= "white", #bg="lightblue",
                    tl.col="black", tl.srt=45,
                    diag = FALSE)


# cool but probably overkill
corrplot(dat2, low = res1$lowCI, upp = res1$uppCI, order = "hclust",
         rect.col = "navy", plotC = "rect", cl.pos = "n", tl.cex = .3,
         type = "upper")

#probably too busy 
corrplot(dat2, p.mat = res1$p, insig = "label_sig",
         sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white",
         tl.cex = .01, type = "upper", diag = FALSE)

#Kinda says it all
corrplot(dat2, p.mat = res1$p, order = "hclust", insig = "pch", addrect = 3, tl.cex = .01)

#what about just for the best models? for just richness? How to make it crunchy to fit in manuscript?
par(mfrow = c(1,1))
corrplot(cor(dat[,2:20]))
corrplot(cor(dat[,21:30]), tl.cex = .1)
corrplot(cor(dat[,31:50]), tl.cex = .3)

#just richness and top vars- ecohydro only
corrplot(cor(dat[,c(2,43,28,29)]), tl.cex = .3)

#TOP MIXED MODEL
corrplot(cor(dat[,c(2,43,28,29,18,21)]), tl.cex = .3,
         type = "upper", method = "color",
         order = "hclust")
#corrplot(cor(dat[,c(2,43,28,29,18,21)]), type = "upper")



#FT richness and top layers 
#grass
corrplot(cor(dat[,c(48,8,3,44,28,25)]), tl.cex = .3)
#forbs
corrplot(cor(dat[,c(49,16,18,21,45,28)]), tl.cex = .3)
#shrubs
corrplot(cor(dat[,c(50,38,14,3,35)]), tl.cex = .2)



##### THIRD VERSION (just edited over the top of V2, can be found in V2 script) #####
#MAP
#par(cex.lab = 1.2,  mar = c(4.5, 0, 2, 1), xpd = FALSE)
#par(fig=c(.3,1,0,01))
par(mar = c(5.1, 4.1, 4.1, 2.1)) # DEFAULT MARGIN SETTINGS
par(mfrow = c(5,4), cex.lab = .9, xpd = F, mgp = c(2,1,0))
par(cex.lab = .9,  mar = c(4.5, 2.2, 2, 1), xpd = FALSE)

#par(mfrow = c(3,2), cex.lab = .9, xpd = F, mgp = c(2,1,0))


#TOTAL
plot(dat$mean1000 ~ dat$clay.total, tck = -.03,  mgp=c(1.5,.5,0),
     xlab = "clay.total",
     pch=16, ylab = "Total Richness")
 #abline(lm(dat$mean1000 ~  dat$clay.total))
plot(dat$mean1000 ~ dat$MAT, tck = -.03,mgp=c(1.5,.5,0),
     xlab = "MAT",
     pch=16, ylab = "Total Richness")
#abline(lm(dat$mean1000 ~  dat$MAT))
plot(dat$mean1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWAbulk_SWPcrit3900kPa_","topLayers_m10.3_mm_mean"),
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean)) #SIG
plot(dat$mean1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWP_topLayers_","DailyMin_MPa_mean"),
     pch=16, ylab = "Total Richness")
#abline(lm(dat$mean1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean))
plot(dat$mean1000 ~ dat$SWP_bottomLayers_DailyMax_MPa_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWP_bottomLayers_","DailyMax_MPa_mean"),
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~ dat$SWP_bottomLayers_DailyMax_MPa_mean)) #SIG
plot(dat$mean1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetDegreeDays_SWPcrit3900kPa_","topLayers_Cdays_mean"),
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean))#sIG
  
plot(dat$mean1000 ~ dat$elevation, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "elevation",
     pch=16, ylab = "Total Richness")
#abline(lm(dat$mean1000 ~  dat$elevation))
plot(dat$mean1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetDegreeDays_SWPcrit3900kPa","_topLayers_Cdays_mean"),
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean)) #SIG
plot(dat$mean1000 ~ dat$MAP, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "MAP",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$MAP)) #SIG
plot(dat$mean1000 ~ dat$UNAridityIndex_Annual_none_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "UNAridityIndex_Annual_mean",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$UNAridityIndex_Annual_none_mean))#SIG
plot(dat$mean1000 ~ dat$Snowfall_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "Snowfall_mm_mean",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$Snowfall_mm_mean)) #SIG
plot(dat$mean1000 ~ dat$Rain_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "Rain_mm_mean",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$Rain_mm_mean)) #SIG
#FORBS 
#combined
plot(dat$forb1000 ~ dat$clay.total, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "clay.total",
     pch=16, ylab = "Forb Richness", col = "blue")
#abline(lm(dat$forb1000 ~  dat$clay.total))
plot(dat$forb1000 ~ dat$MAT, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "MAT",
     pch=16, ylab = "Forb Richness", col = "blue")
#abline(lm(dat$forb1000 ~  dat$MAT))
plot(dat$forb1000 ~ dat$sand.total, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "sand.total",
     pch=16, ylab = "Forb Richness", col = "blue")
#abline(lm(dat$forb1000 ~ dat$sand.total))
plot(dat$forb1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWAbulk_SWPcrit3900kPa","_topLayers_m10.3_mm_mean"),
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean)) #SIG
plot(dat$forb1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWP_topLayers_","DailyMin_MPa_mean"),
     pch=16, ylab = "Forb Richness", col = "blue")
#abline(lm(dat$forb1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean))
#ecohydro
plot(dat$forb1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetDegreeDays_SWPcrit3900kPa_","topLayers_Cdays_mean"),
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~  dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean)) #SIG
plot(dat$forb1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetSoilPeriods_SWPcrit1500kPa_","NSadj_topLayers_AllLayersWet_","Duration_Total_days_mean"),
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~  dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AllLayersWet_Duration_Total_days_mean)) #SIG

#macroclimate
plot(dat$forb1000 ~ dat$AET_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "AET_mm_mean",
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~  dat$AET_mm_mean)) #SIG
plot(dat$forb1000 ~ dat$UNAridityIndex_Annual_none_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "UNAridityIndex_","Annual_none_mean",
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~  dat$UNAridityIndex_Annual_none_mean)) #SIG
plot(dat$forb1000 ~ dat$Snowfall_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "Snowfall_mm_mean",
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~  dat$Snowfall_mm_mean)) #SIG
plot(dat$forb1000 ~ dat$Rain_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "Rain_mm_mean",
     pch=16, ylab = "Forb Richness", col = "blue")
abline(lm(dat$forb1000 ~  dat$Rain_mm_mean)) #SIG

#GRASSES 
#ecohydro
plot(dat$grass1000 ~ dat$elevation, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "elevation",
     pch=16, ylab = "Grass Richness", col = "green")
#abline(lm(dat$grass1000 ~  dat$elevation))
plot(dat$grass1000 ~ dat$slope, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "slope",
     pch=16, ylab = "Grass Richness", col = "green")
#abline(lm(dat$grass1000 ~  dat$slope))
plot(dat$grass1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWP_topLayers_","DailyMin_MPa_mean"),
     pch=16, ylab = "Grass Richness", col = "green")
#abline(lm(dat$grass1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean))
plot(dat$grass1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetDegreeDays_","SWPcrit3900kPa_","bottomLayers_Cdays_mean"),
     pch=16, ylab = "Grass Richness", col = "green")
#abline(lm(dat$grass1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean))
plot(dat$grass1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_anyLayer_Cdays_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetDegreeDays_","SWPcrit3900kPa_","anyLayer_Cdays_mean"),
     pch=16, ylab = "Grass Richness", col = "green")
#abline(lm(dat$grass1000 ~ dat$WetDegreeDays_SWPcrit3900kPa_bottomLayers_Cdays_mean))

plot(dat$grass1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWAbulk_SWPcrit3900kPa_","topLayers_m10.3_mm_mean"),
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean)) #SIG
plot(dat$grass1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("SWAbulk_SWPcrit3900kPa","_topLayers_m4.9_mm_mean"),
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean)) #SIG

#macroclimate
plot(dat$grass1000 ~ dat$Rain_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "Rain_mm_mean",
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$Rain_mm_mean)) #SIG
plot(dat$grass1000 ~ dat$AET_mm_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "AET_mm_mean",
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$AET_mm_mean)) #SIG
plot(dat$grass1000 ~ dat$MAP, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "MAP",
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$MAP)) #SIG
plot(dat$grass1000 ~ dat$UNAridityIndex_Annual_none_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "UNAridityIndex","_Annual_none_mean",
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$UNAridityIndex_Annual_none_mean)) #SIG

#biotic
plot(dat$grass1000 ~ dat$ARTR.density, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "ARTR.density",
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$ARTR.density)) #SIG
plot(dat$grass1000 ~ dat$maxage, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "maxage",
     pch=16, ylab = "Grass Richness", col = "green")
abline(lm(dat$grass1000 ~  dat$maxage)) #SIG

## SHRUB ECOHYDRO


plot(dat$shrub1000 ~ dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean	, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("DrySoilPeriods_SWPcrit3900kPa_","NSadj_bottomLayers_AllLayersDry_","Duration_Total_days_mean"),
     pch=16, ylab = "Shrub Richness", col = "darkgrey")
abline(lm(dat$shrub1000 ~ dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean)) #SIG

plot(dat$shrub1000 ~ dat$slope, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "slope",
     pch=16, ylab = "Shrub Richness", col = "darkgrey")
abline(lm(dat$shrub1000 ~  dat$slope)) #SIG

plot(dat$shrub1000 ~ dat$elevation, tck = -.03, mgp=c(1.5,.5,0),
     xlab = "elevation",
     pch=16, ylab = "Shrub Richness", col = "darkgrey")
abline(lm(dat$shrub1000 ~  dat$elevation))#SIG

plot(dat$shrub1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean, tck = -.03, mgp=c(1.5,.5,0),
     xlab = c("WetSoilPeriods_SWPcrit1500kPa","topLayers_AnyLayerWet_","Duration_Total_days_mean"),
     pch=16, ylab = "Shrub Richness", col = "darkgrey")
abline(lm(dat$shrub1000 ~  dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean))#SIG



##### FIRST VERSION #####
#MAP
#par(cex.lab = 1.2,  mar = c(4.5, 0, 2, 1), xpd = FALSE)
#par(fig=c(.3,1,0,01))
par(mfrow = c(5,4), cex.lab = 1.2, xpd = F)

plot(dat$mean1000 ~ dat$MAP,
     xlab = "MAP",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$MAP))
plot(dat$forb1000 ~ dat$MAP,
     xlab = "MAP",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$MAP))
plot(dat$grass1000 ~ dat$MAP,
     xlab = "MAP",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$MAP))
plot(dat$shrub1000 ~ dat$MAP,
     xlab = "MAP",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$MAP))
# LINES

plot(dat$mean1000 ~ dat$MAT,
     xlab = "MAT",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$MAT))
plot(dat$forb1000 ~ dat$MAT,
     xlab = "MAT",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$MAT))
plot(dat$grass1000 ~ dat$MAT,
     xlab = "MAT",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$MAT))
plot(dat$shrub1000 ~ dat$MAT,
     xlab = "MAT",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$MAT))
# LINES

plot(dat$mean1000 ~ dat$UNAridityIndex_Annual_none_mean,
     xlab = "UN Aridity Index",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$UNAridityIndex_Annual_none_mean))
plot(dat$forb1000 ~ dat$UNAridityIndex_Annual_none_mean,
     xlab = "UN Aridity Index",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$UNAridityIndex_Annual_none_mean))
plot(dat$grass1000 ~ dat$UNAridityIndex_Annual_none_mean,
     xlab = "UN Aridity Index",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$UNAridityIndex_Annual_none_mean))
plot(dat$shrub1000 ~ dat$UNAridityIndex_Annual_none_mean,
     xlab = "UN Aridity Index",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$UNAridityIndex_Annual_none_mean))
# LINES

plot(dat$mean1000 ~ dat$AET_mm_mean,
     xlab = "AET",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$AET_mm_mean))
plot(dat$forb1000 ~ dat$AET_mm_mean,
     xlab = "AET",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$AET_mm_mean))
plot(dat$grass1000 ~ dat$AET_mm_mean,
     xlab = "AET",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$AET_mm_mean))
plot(dat$shrub1000 ~ dat$AET_mm_mean,
     xlab = "AET",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$AET_mm_mean))
# LINES
plot(dat$mean1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean,
     xlab = "Days Any Wet Top Soil Layer",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean))
plot(dat$forb1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean,
     xlab = "Days Any Wet Top Soil Layer",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean))
plot(dat$grass1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean,
     xlab = "Days Any Wet Top Soil Layer",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean))
plot(dat$shrub1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean,
     xlab = "Days Any Wet Top Soil Layer",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean))

# LINES

plot(dat$mean1000 ~ dat$SWP_topLayers_DailyMin,
     xlab = "Min SWP Top Layers",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$SWP_topLayers_DailyMin_MPa_mean))
plot(dat$forb1000 ~ dat$SWP_topLayers_DailyMin,
     xlab = "Min SWP Top Layers",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$SWP_topLayers_DailyMin_MPa_mean))
plot(dat$grass1000 ~ dat$SWP_topLayers_DailyMin,
     xlab = "Min SWP Top Layers",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean))
plot(dat$shrub1000 ~ dat$SWP_topLayers_DailyMin,
     xlab = "Min SWP Top Layers",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$SWP_topLayers_DailyMin_MPa_mean))
# LINES

plot(dat$mean1000 ~ dat$SWP_bottomLayers_DailyMin,
     xlab = "Min SWP Bottom Layers",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$SWP_bottomLayers_DailyMin_MPa_mean))
plot(dat$forb1000 ~ dat$SWP_bottomLayers_DailyMin,
     xlab = "Min SWP Bottom Layers",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$SWP_bottomLayers_DailyMin_MPa_mean))
plot(dat$grass1000 ~ dat$SWP_bottomLayers_DailyMin,
     xlab = "Min SWP Bottom Layers",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$SWP_bottomLayers_DailyMin_MPa_mean))
plot(dat$shrub1000 ~ dat$SWP_bottomLayers_DailyMin,
     xlab = "Min SWP Bottom Layers",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$SWP_bottomLayers_DailyMin_MPa_mean))
# LINES

plot(dat$mean1000 ~ dat$clay.total,
     xlab = "Clay Content",
     pch=16, ylab = "Total Richness")
abline(lm(dat$mean1000 ~  dat$clay.total))
plot(dat$forb1000 ~ dat$clay.total,
     xlab = "Clay Content",
     pch=16, ylab = "Forb Richness")
abline(lm(dat$forb1000 ~  dat$clay.total))
plot(dat$grass1000 ~ dat$clay.total,
     xlab = "Clay Content",
     pch=16, ylab = "Grass Richness")
abline(lm(dat$grass1000 ~ dat$clay.total))
plot(dat$shrub1000 ~ dat$clay.total,
     xlab = "Clay Content",
     pch=16, ylab = "Shrub Richness")
abline(lm(dat$shrub1000 ~ dat$clay.total))
# LINES
