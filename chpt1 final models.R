### CHAPTER 1 FINAL MODELS ####
dat <- read.csv(file = "data_for_varpart.csv", header = TRUE)
library(vegan)


#FINAL TOP MODELS FROM DREDGES FOR TOTAL AND FUNCTIONAL TYPE RICHNESS


# EXAMPLE OF HOW I UTILIZED VARPART IN VEGAN
#top model for total richness

new.ct1 <- varpart(dat$mean1000 , dat$clay.total , dat$MAT , dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean , 
                        dat$SWP_topLayers_DailyMin_MPa_mean )

plot(new.ct1.vp)
new.ct1.vp # I extracted values from the individual fractions table

#accessing individual fractions directly
new.ct1.vp$part$indfract$Adj.R.square

# MODELS #####

#MODELS ARE LISTED IN DESCENDING RANK BY AIC
#TOTAL 
new.ct1 <- lm(formula = dat$mean1000 ~ dat$clay.total + dat$MAT + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
                dat$SWP_topLayers_DailyMin_MPa_mean + 1)
new.ct3 <- lm(formula = dat$mean1000 ~ dat$MAT + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
                dat$SWP_topLayers_DailyMin_MPa_mean + 1) 
new.ct4 <- lm(formula = dat$mean1000 ~ dat$clay.total + dat$MAT + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
                1)
new.ct7 <- lm(formula = dat$mean1000 ~ dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
                dat$SWP_topLayers_DailyMin_MPa_mean + dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean + 
                1)
### VARPARTING
new.ct3 <- varpart(dat$mean1000 , dat$MAT , dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean , 
                dat$SWP_topLayers_DailyMin_MPa_mean) 
new.ct4 <- varpart(dat$mean1000 , dat$clay.total, dat$MAT , dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean)
new.ct7 <- varpart(dat$mean1000 , dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean , 
                dat$SWP_topLayers_DailyMin_MPa_mean , dat$WetDegreeDays_SWPcrit3900kPa_topLayers_Cdays_mean)

plot(new.ct3)
plot(new.ct4)
plot(new.ct7)



#FORBS
cf2a <- lm(formula = dat$forb1000 ~ dat$clay.total + dat$MAT + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
             1)
cf3a <- lm(formula = dat$forb1000 ~ dat$MAT + dat$sand.total + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
             dat$SWP_topLayers_DailyMin_MPa_mean + 1)
cf6a <- lm(formula = dat$forb1000 ~ dat$MAT + dat$sand.total + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
             1)
cf7a <- lm(formula = dat$forb1000 ~ dat$clay.total + dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean + 
             1)
#GRASS
new.cg1 <- lm(formula = dat$grass1000 ~ dat$ARTR.density + dat$elevation + 
                dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean + dat$SWP_topLayers_DailyMin_MPa_mean + 
                1)
new.cg2 <- lm(formula = dat$grass1000 ~ dat$elevation + dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean + 
                dat$SWP_topLayers_DailyMin_MPa_mean + 1)
new.cg3 <- lm(formula = dat$grass1000 ~ dat$ARTR.density + dat$elevation + 
                dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean + 1)
new.cg9 <- lm(formula = dat$grass1000 ~ dat$ARTR.density + dat$Rain_mm_mean + 
                1)

#VARPARTING
new.cg1 <- varpart(dat$grass1000 , dat$ARTR.density , dat$elevation , 
             dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean , dat$SWP_topLayers_DailyMin_MPa_mean)
new.cg2 <- varpart(dat$grass1000 ,dat$elevation , dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean , 
             dat$SWP_topLayers_DailyMin_MPa_mean )
new.cg3 <- varpart(dat$grass1000 , dat$ARTR.density , dat$elevation , 
              dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean)
new.cg9 <- varpart(dat$grass1000, dat$ARTR.density , dat$Rain_mm_mean) 
           
plot(new.cg1)
plot(new.cg2)
plot(new.cg3)
plot(new.cg9)
#SHRUB
#ecohydrological only as no combined models were significant
es1 <- lm(formula = dat$shrub1000 ~ dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean + 
            dat$slope + 1)
es2b <-lm(formula = dat$shrub1000 ~ dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean + 
            dat$elevation + 1)
es3b <-lm(formula = dat$shrub1000 ~ dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean + 
            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_topLayers_AnyLayerWet_Duration_Total_days_mean + 
            1)
es9b <-lm(formula = dat$shrub1000 ~ dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean + 
            1)


#https://www.researchgate.net/post/Why_in_regression_analysis_does_the_inclusion_of_a_new_variable_makes_other_variables_that_previously_were_not_statistically_significant
#possible solutions
#PCA
#VARIABLE INFLATION FACTORS
#VARCLUS
