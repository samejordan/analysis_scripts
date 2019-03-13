# CVs of FT richness ####

# GENERATE SUMMARY STATISTICS BY FUNCTIONAL TYPE FOR ANALYSIS


#alternatively
dat.forbs <- read.csv(file ="forbs_only.csv", header = TRUE)
dat.shrubs <- read.csv(file ="shrubs_only.csv", header = TRUE)
dat.grass <- read.csv(file ="grasses_only.csv", header = TRUE)
#dat.succulents <- read.csv(file ="succulents_only.csv", header = TRUE)


dat3 <- read.csv(file = "Predictors_final.csv", header = TRUE)
dat <- read.csv(file = "expandedrichness.csv", header = TRUE)
#excluding cheatgrass plots :/
dat <- dat[dat$plot!= 18 & 
             dat$plot!= 22 &
             dat$plot!= 26, ]
dat.ex.forbs <- dat.ex.forbs[dat.ex.forbs$plot!= 18 & 
             dat.ex.forbs$plot!= 22 &
             dat.ex.forbs$plot!= 26, ]
dat.ex.grass <- dat.ex.grass[dat.ex.grass$plot!= 18 & 
                               dat.ex.grass$plot!= 22 &
                               dat.ex.grass$plot!= 26, ]
dat.ex.shrubs <- dat.ex.shrubs[dat.ex.shrubs$plot!= 18 & 
                               dat.ex.shrubs$plot!= 22 &
                               dat.ex.shrubs$plot!= 26, ]

# Summary Stats by Spatial Scale ####
# generating mean values from expanded richnesses
forb.scale1 <- dat.ex.forbs[dat.ex.forbs$scale == 1,]
forb.mean1 <- tapply(forb.scale1$value, forb.scale1$plot, mean)
forb.sd1 <- tapply(forb.scale1$value, forb.scale1$plot, sd)
forb.se1 <- forb.sd1/sqrt(length(dat$value[dat$scale ==1]))
forb.cv1 <- forb.sd1/forb.mean1 * 100
forb.se1.plus <- forb.mean1 + forb.se1
forb.se1.minus <- forb.mean1 - forb.se1
forb.cv1.across <- sd(forb.scale1$value)/mean(forb.scale1$value) * 100

forb.scale2 <- dat.ex.forbs[dat.ex.forbs$scale == 2,]
forb.mean2 <- tapply(forb.scale2$value, forb.scale2$plot, mean)
forb.sd2 <- tapply(forb.scale2$value, forb.scale2$plot, sd)
forb.se2 <- forb.sd2/sqrt(length(dat$value[dat$scale ==2]))
forb.cv2 <- forb.sd2/forb.mean2 *100
forb.se2.plus <- forb.mean2 + forb.se2
forb.se2.minus <- forb.mean2 - forb.se2
forb.cv2.across <- sd(forb.scale2$value)/mean(forb.scale2$value) * 100


forb.scale3 <- dat.ex.forbs[dat.ex.forbs$scale == 3,]
forb.mean3 <- tapply(forb.scale3$value, forb.scale3$plot, mean)
forb.sd3 <- tapply(forb.scale3$value, forb.scale3$plot, sd)
forb.se3 <- forb.sd3/sqrt(length(dat$value[dat$scale ==3]))
forb.cv3 <- forb.sd3/forb.mean3 *100
forb.se3.plus <- forb.mean3 + forb.se3
forb.se3.minus <- forb.mean3 - forb.se3
forb.cv3.across <- sd(forb.scale3$value)/mean(forb.scale3$value) * 100


forb.scale4 <- dat.ex.forbs[dat.ex.forbs$scale == 4,]
forb.mean4 <- tapply(forb.scale4$value, forb.scale4$plot, mean)
forb.sd4 <- tapply(forb.scale4$value, forb.scale4$plot, sd)
forb.se4 <- forb.sd4/sqrt(length(dat$value[dat$scale ==4]))
forb.cv4 <- forb.sd4/forb.mean4 *100
forb.se4.plus <- forb.mean4 + forb.se4
forb.se4.minus <- forb.mean4 - forb.se4
forb.cv4.across <- sd(forb.scale4$value)/mean(forb.scale4$value) * 100


forb.scale5 <- dat.ex.forbs[dat.ex.forbs$scale == 5,]
forb.mean5 <- tapply(forb.scale5$value, forb.scale5$plot, mean)
forb.sd5 <- tapply(forb.scale5$value, forb.scale5$plot, sd)
forb.se5 <- forb.sd5/sqrt(length(dat$value[dat$scale ==5]))
forb.cv5 <- forb.sd5/forb.mean5 *100
forb.se5.plus <- forb.mean5 + forb.se5
forb.se5.minus <- forb.mean5 - forb.se5
forb.cv5.across <- sd(forb.scale5$value)/mean(forb.scale5$value) * 100


forb.scale400 <- dat.ex.forbs[dat.ex.forbs$scale == 400,]
forb.mean400 <- tapply(forb.scale400$value, forb.scale400$plot, mean)
#forb.sd400 <- tapply(forb.scale400$value, forb.scale400$plot, sd)
#forb.se400 <- forb.sd400/sqrt(length(dat$value[dat$scale ==400]))
#forb.cv400 <- forb.sd400/forb.mean400 *100
forb.cv400.across <- sd(forb.scale400$value)/mean(forb.scale400$value) * 100


forb.scale1000 <- dat.ex.forbs[dat.ex.forbs$scale == 1000,]
forb.mean1000 <- tapply(forb.scale1000$value, forb.scale1000$plot, mean)
#forb.sd1000 <- tapply(forb.scale1000$value, forb.scale1000$plot, sd)
#forb.se1000 <- forb.sd1000/sqrt(length(dat$value[dat$scale ==1000]))
#forb.cv1000 <- forb.sd1000/forb.mean1000 *100
forb.cv1000.across <- sd(forb.scale1000$value)/mean(forb.scale1000$value) * 100


all.scales.forbs <- cbind(forb.mean5, forb.mean4, forb.mean3, forb.mean2, forb.mean1, forb.mean400, forb.mean1000,
                          forb.sd5, forb.sd4, forb.sd3, forb.sd2, forb.sd1,
                          forb.se5, forb.se4, forb.se3, forb.se2, forb.se1, 
                          forb.cv5, forb.cv4, forb.cv3, forb.cv2, forb.cv1,
                          forb.se5.plus, forb.se5.minus, forb.se4.plus, forb.se4.minus, forb.se3.plus, forb.se3.minus, forb.se2.plus, forb.se2.minus, forb.se1.plus, forb.se1.minus)
all.scales.forbs

scale5.forb <- as.data.frame(cbind(forb.mean5, 
                                    forb.sd5,  
                                    forb.se5,  
                                    forb.cv5, 
                                    forb.se5.plus, forb.se5.minus))
names(scale5.forb) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale5.forb$scale <- 5
scale4.forb <- as.data.frame(cbind(forb.mean4, 
                                    forb.sd4,  
                                    forb.se4,  
                                    forb.cv4, 
                                    forb.se4.plus, forb.se4.minus))
names(scale4.forb) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale4.forb$scale <- 4
scale3.forb <- as.data.frame(cbind(forb.mean3, 
                                    forb.sd3,  
                                    forb.se3,  
                                    forb.cv3, 
                                    forb.se3.plus, forb.se3.minus))
names(scale3.forb) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale3.forb$scale <- 3
scale2.forb <- as.data.frame(cbind(forb.mean2, 
                                    forb.sd2,  
                                    forb.se2,  
                                    forb.cv2, 
                                    forb.se2.plus, forb.se2.minus))
names(scale2.forb) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale2.forb$scale <- 2
scale1.forb <- as.data.frame(cbind(forb.mean1, 
                                    forb.sd1,  
                                    forb.se1,  
                                    forb.cv1, 
                                    forb.se1.plus, forb.se1.minus))
names(scale1.forb) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale1.forb$scale <- 1

all.scales.forb.alt <- rbind(scale5.forb, scale4.forb, scale3.forb, scale2.forb, scale1.forb)

#grasses

grass.scale1 <- dat.ex.grass[dat.ex.grass$scale == 1,]
grass.mean1 <- tapply(grass.scale1$value, grass.scale1$plot, mean)
grass.sd1 <- tapply(grass.scale1$value, grass.scale1$plot, sd)
grass.se1 <- grass.sd1/sqrt(length(dat$value[dat$scale ==1]))
grass.cv1 <- grass.sd1/grass.mean1 *100
grass.se1.plus <- grass.mean1 + grass.se1
grass.se1.minus <- grass.mean1 - grass.se1
grass.cv1.across <- sd(grass.scale1$value)/mean(grass.scale1$value) * 100


grass.scale2 <- dat.ex.grass[dat.ex.grass$scale == 2,]
grass.mean2 <- tapply(grass.scale2$value, grass.scale2$plot, mean)
grass.sd2 <- tapply(grass.scale2$value, grass.scale2$plot, sd)
grass.se2 <- grass.sd2/sqrt(length(dat$value[dat$scale ==2]))
grass.cv2 <- grass.sd2/grass.mean2 *100
grass.se2.plus <- grass.mean2 + grass.se2
grass.se2.minus <- grass.mean2 - grass.se2
grass.cv2.across <- sd(grass.scale2$value)/mean(grass.scale2$value) * 100


grass.scale3 <- dat.ex.grass[dat.ex.grass$scale == 3,]
grass.mean3 <- tapply(grass.scale3$value, grass.scale3$plot, mean)
grass.sd3 <- tapply(grass.scale3$value, grass.scale3$plot, sd)
grass.se3 <- grass.sd3/sqrt(length(dat$value[dat$scale ==3]))
grass.cv3 <- grass.sd3/grass.mean3 *100
grass.se3.plus <- grass.mean3 + grass.se3
grass.se3.minus <- grass.mean3 - grass.se3
grass.cv3.across <- sd(grass.scale3$value)/mean(grass.scale3$value) * 100


grass.scale4 <- dat.ex.grass[dat.ex.grass$scale == 4,]
grass.mean4 <- tapply(grass.scale4$value, grass.scale4$plot, mean)
grass.sd4 <- tapply(grass.scale4$value, grass.scale4$plot, sd)
grass.se4 <- grass.sd4/sqrt(length(dat$value[dat$scale ==4]))
grass.cv4 <- grass.sd4/grass.mean4 *100
grass.se4.plus <- grass.mean4 + grass.se4
grass.se4.minus <- grass.mean4 - grass.se4
grass.cv4.across <- sd(grass.scale4$value)/mean(grass.scale4$value) * 100


grass.scale5 <- dat.ex.grass[dat.ex.grass$scale == 5,]
grass.mean5 <- tapply(grass.scale5$value, grass.scale5$plot, mean)
grass.sd5 <- tapply(grass.scale5$value, grass.scale5$plot, sd)
grass.se5 <- grass.sd5/sqrt(length(dat$value[dat$scale ==5]))
grass.cv5 <- grass.sd5/grass.mean5 *100
grass.se5.plus <- grass.mean5 + grass.se5
grass.se5.minus <- grass.mean5 - grass.se5
grass.cv5.across <- sd(grass.scale5$value)/mean(grass.scale5$value) * 100


grass.scale400 <- dat.ex.grass[dat.ex.grass$scale == 400,]
grass.mean400 <- tapply(grass.scale400$value, grass.scale400$plot, mean)
#grass.sd400 <- tapply(grass.scale400$value, grass.scale400$plot, sd)
#grass.se400 <- grass.sd400/sqrt(length(dat$value[dat$scale ==400]))
#grass.cv400 <- grass.sd400/grass.mean400 *100
grass.cv400.across <- sd(grass.scale400$value)/mean(grass.scale400$value) * 100


grass.scale1000 <- dat.ex.grass[dat.ex.grass$scale == 1000,]
grass.mean1000 <- tapply(grass.scale1000$value, grass.scale1000$plot, mean)
#grass.sd1000 <- tapply(grass.scale1000$value, grass.scale1000$plot, sd)
#grass.se1000 <- grass.sd1000/sqrt(length(dat$value[dat$scale ==1000]))
#grass.cv1000 <- grass.sd1000/grass.mean1000 *100
grass.cv1000.across <- sd(grass.scale1000$value)/mean(grass.scale1000$value) * 100


all.scales.grass <- cbind(grass.mean5, grass.mean4, grass.mean3, grass.mean2, grass.mean1, grass.mean400, grass.mean1000,
                          grass.sd5, grass.sd4, grass.sd3, grass.sd2, grass.sd1, 
                          grass.se5, grass.se4, grass.se3, grass.se2, grass.se1, 
                          grass.cv5, grass.cv4, grass.cv3, grass.cv2, grass.cv1,
                          grass.se5.plus, grass.se5.minus, grass.se4.plus, grass.se4.minus, grass.se3.plus, grass.se3.minus, grass.se2.plus, grass.se2.minus, grass.se1.plus, grass.se1.minus)

scale5.grass <- as.data.frame(cbind(grass.mean5, 
                          grass.sd5,  
                          grass.se5,  
                          grass.cv5, 
                          grass.se5.plus, grass.se5.minus))
names(scale5.grass) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale5.grass$scale <- 5
scale4.grass <- as.data.frame(cbind(grass.mean4, 
                                    grass.sd4,  
                                    grass.se4,  
                                    grass.cv4, 
                                    grass.se4.plus, grass.se4.minus))
names(scale4.grass) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale4.grass$scale <- 4
scale3.grass <- as.data.frame(cbind(grass.mean3, 
                                    grass.sd3,  
                                    grass.se3,  
                                    grass.cv3, 
                                    grass.se3.plus, grass.se3.minus))
names(scale3.grass) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale3.grass$scale <- 3
scale2.grass <- as.data.frame(cbind(grass.mean2, 
                                    grass.sd2,  
                                    grass.se2,  
                                    grass.cv2, 
                                    grass.se2.plus, grass.se2.minus))
names(scale2.grass) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale2.grass$scale <- 2
scale1.grass <- as.data.frame(cbind(grass.mean1, 
                                    grass.sd1,  
                                    grass.se1,  
                                    grass.cv1, 
                                    grass.se1.plus, grass.se1.minus))
names(scale1.grass) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale1.grass$scale <- 1

all.scales.grass.alt <- rbind(scale5.grass, scale4.grass, scale3.grass, scale2.grass, scale1.grass)

#shrubs

shrubs.scale1 <- dat.ex.shrubs[dat.ex.shrubs$scale == 1,]
shrubs.mean1 <- tapply(shrubs.scale1$value, shrubs.scale1$plot, mean)
shrubs.sd1 <- tapply(shrubs.scale1$value, shrubs.scale1$plot, sd)
shrubs.se1 <- shrubs.sd1/sqrt(length(dat$value[dat$scale ==1]))
shrubs.cv1 <- shrubs.sd1/shrubs.mean1 *100
shrubs.se1.plus <- shrubs.mean1 + shrubs.se1
shrubs.se1.minus <- shrubs.mean1 - shrubs.se1
shrubs.cv1.across <- sd(shrubs.scale1$value)/mean(shrubs.scale1$value) * 100


shrubs.scale2 <- dat.ex.shrubs[dat.ex.shrubs$scale == 2,]
shrubs.mean2 <- tapply(shrubs.scale2$value, shrubs.scale2$plot, mean)
shrubs.sd2 <- tapply(shrubs.scale2$value, shrubs.scale2$plot, sd)
shrubs.se2 <- shrubs.sd2/sqrt(length(dat$value[dat$scale ==2]))
shrubs.cv2 <- shrubs.sd2/shrubs.mean2 *100
shrubs.se2.plus <- shrubs.mean2 + shrubs.se2
shrubs.se2.minus <- shrubs.mean2 - shrubs.se2
shrubs.cv2.across <- sd(shrubs.scale2$value)/mean(shrubs.scale2$value) * 100


shrubs.scale3 <- dat.ex.shrubs[dat.ex.shrubs$scale == 3,]
shrubs.mean3 <- tapply(shrubs.scale3$value, shrubs.scale3$plot, mean)
shrubs.sd3 <- tapply(shrubs.scale3$value, shrubs.scale3$plot, sd)
shrubs.se3 <- shrubs.sd3/sqrt(length(dat$value[dat$scale ==3]))
shrubs.cv3 <- shrubs.sd3/shrubs.mean3 *100
shrubs.se3.plus <- shrubs.mean3 + shrubs.se3
shrubs.se3.minus <- shrubs.mean3 - shrubs.se3
shrubs.cv3.across <- sd(shrubs.scale3$value)/mean(shrubs.scale3$value) * 100


shrubs.scale4 <- dat.ex.shrubs[dat.ex.shrubs$scale == 4,]
shrubs.mean4 <- tapply(shrubs.scale4$value, shrubs.scale4$plot, mean)
shrubs.sd4 <- tapply(shrubs.scale4$value, shrubs.scale4$plot, sd)
shrubs.se4 <- shrubs.sd4/sqrt(length(dat$value[dat$scale ==4]))
shrubs.cv4 <- shrubs.sd4/shrubs.mean4 *100
shrubs.se4.plus <- shrubs.mean4 + shrubs.se4
shrubs.se4.minus <- shrubs.mean4 - shrubs.se4
shrubs.cv4.across <- sd(shrubs.scale4$value)/mean(shrubs.scale4$value) * 100


shrubs.scale5 <- dat.ex.shrubs[dat.ex.shrubs$scale == 5,]
shrubs.mean5 <- tapply(shrubs.scale5$value, shrubs.scale5$plot, mean)
shrubs.sd5 <- tapply(shrubs.scale5$value, shrubs.scale5$plot, sd)
shrubs.se5 <- shrubs.sd5/sqrt(length(dat$value[dat$scale ==5]))
shrubs.cv5 <- shrubs.sd5/shrubs.mean5 *100
shrubs.se5.plus <- shrubs.mean5 + shrubs.se5
shrubs.se5.minus <- shrubs.mean5 - shrubs.se5
shrubs.cv5.across <- sd(shrubs.scale5$value)/mean(shrubs.scale5$value) * 100


shrubs.scale400 <- dat.ex.shrubs[dat.ex.shrubs$scale == 400,]
shrubs.mean400 <- tapply(shrubs.scale400$value, shrubs.scale400$plot, mean)
#shrubs.sd400 <- tapply(shrubs.scale400$value, shrubs.scale400$plot, sd)
#shrubs.se400 <- shrubs.sd400/sqrt(length(dat$value[dat$scale ==400]))
#shrubs.cv400 <- shrubs.sd400/shrubs.mean400 *100
shrubs.cv400.across <- sd(shrubs.scale400$value)/mean(shrubs.scale400$value) * 100



shrubs.scale1000 <- dat.ex.shrubs[dat.ex.shrubs$scale == 1000,]
shrubs.mean1000 <- tapply(shrubs.scale1000$value, shrubs.scale1000$plot, mean)
#shrubs.sd1000 <- tapply(shrubs.scale1000$value, shrubs.scale1000$plot, sd)
#shrubs.se1000 <- shrubs.sd1000/sqrt(length(dat$value[dat$scale ==1000]))
#shrubs.cv1000 <- shrubs.sd1000/shrubs.mean1000 *100
shrubs.cv1000.across <- sd(shrubs.scale1000$value)/mean(shrubs.scale1000$value) * 100



all.scales.shrubs <- cbind(shrubs.mean5, shrubs.mean4, shrubs.mean3, shrubs.mean2, shrubs.mean1, shrubs.mean400, shrubs.mean1000,
                          shrubs.sd5, shrubs.sd4, shrubs.sd3, shrubs.sd2, shrubs.sd1, 
                          shrubs.se5, shrubs.se4, shrubs.se3, shrubs.se2, shrubs.se1, 
                          shrubs.cv5, shrubs.cv4, shrubs.cv3, shrubs.cv2, shrubs.cv1,
                          shrubs.se5.plus, shrubs.se5.minus, shrubs.se4.plus, shrubs.se4.minus, shrubs.se3.plus, shrubs.se3.minus, shrubs.se2.plus, shrubs.se2.minus, shrubs.se1.plus, shrubs.se1.minus)


all.scales.all.fgs <- as.data.frame(cbind(all.scales.grass, all.scales.forbs, all.scales.shrubs))
all.scales.all.fgs$plot <- dat3$plot

scale5.shrubs <- as.data.frame(cbind(shrubs.mean5, 
                                    shrubs.sd5,  
                                    shrubs.se5,  
                                    shrubs.cv5, 
                                    shrubs.se5.plus, shrubs.se5.minus))
names(scale5.shrubs) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale5.shrubs$scale <- 5
scale4.shrubs <- as.data.frame(cbind(shrubs.mean4, 
                                    shrubs.sd4,  
                                    shrubs.se4,  
                                    shrubs.cv4, 
                                    shrubs.se4.plus, shrubs.se4.minus))
names(scale4.shrubs) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale4.shrubs$scale <- 4
scale3.shrubs <- as.data.frame(cbind(shrubs.mean3, 
                                    shrubs.sd3,  
                                    shrubs.se3,  
                                    shrubs.cv3, 
                                    shrubs.se3.plus, shrubs.se3.minus))
names(scale3.shrubs) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale3.shrubs$scale <- 3
scale2.shrubs <- as.data.frame(cbind(shrubs.mean2, 
                                    shrubs.sd2,  
                                    shrubs.se2,  
                                    shrubs.cv2, 
                                    shrubs.se2.plus, shrubs.se2.minus))
names(scale2.shrubs) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale2.shrubs$scale <- 2
scale1.shrubs <- as.data.frame(cbind(shrubs.mean1, 
                                    shrubs.sd1,  
                                    shrubs.se1,  
                                    shrubs.cv1, 
                                    shrubs.se1.plus, shrubs.se1.minus))
names(scale1.shrubs) <- c('mean', 'sd', 'se', 'cv','se_plus','se_minus')
scale1.shrubs$scale <- 1

#all.scales.shrubs.alt <- rbind(scale5.shrubs, scale4.shrubs, scale3.shrubs, scale2.shrubs, scale1.shrubs)
#write.csv(all.scales.all.fgs, file = "summary_stats_by_FG.csv")
#write.csv(all.scales.forb.alt, file = "all_scales_forbs_alt.csv")
#write.csv(all.scales.grass.alt, file = "all_scales_grass_alt.csv")
#write.csv(all.scales.shrubs.alt, file = "all_scales_shrubs_alt.csv")
