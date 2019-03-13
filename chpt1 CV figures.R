# Figures For CVs of FGs

# CODE FOR FIGURE THREE IN MANUSCRIPT

all.groups <- read.csv(file = "summary_stats_by_FG.csv", header = TRUE)
dat <- read.csv("predictors.csv", header = TRUE)

# newest figure for manuscript

par(cex.lab = 1.2,  mar = c(4.5, 0, 2, 1), xpd = FALSE)
par(fig=c(.3,1,0,01))
boxplot(
  all.groups$grass.cv5,
  all.groups$forb.cv5,
  all.groups$shrubs.cv5,
  all.groups$grass.cv4,
  all.groups$forb.cv4,
  all.groups$shrubs.cv4,
  all.groups$grass.cv3,
  all.groups$forb.cv3,
  all.groups$shrubs.cv3,
  all.groups$grass.cv2,
  all.groups$forb.cv2,
  all.groups$shrubs.cv2,
  all.groups$grass.cv1,
  all.groups$forb.cv1,
  all.groups$shrubs.cv1,
  xlab= "",
  xaxt ="n",
  yaxt = "n",
  ylim = c(0,275),
  col = c("green", "blue","grey","green", "blue","grey","green", "blue","grey"))
axis(1, at = c(2,5,8,11,14), tick = FALSE,
     labels =  c("0.01", "0.1",  "1",  "10", "100" ))
mtext(expression(paste("Area (m"^"2",")")), 1, at = 8,3, cex = 1.2)
abline(v=c(3.5, 6.5, 9.5, 12.5), lty=3)
legend(12.6,250, c( "Grasses", "Forbs", "Shrubs"),
       col=c( "green", "blue","grey"), pt.cex =2.6, cex=1.2, pch = 15,
       bty = "n")
par(xpd = TRUE)
text(.75,300, "(b)", font = 2)
par(cex.lab = 1.2,  mar = c(4.5, 4, 2, 0), xpd = FALSE)
par(cex.lab = 1.2, par(fig=c(0,.3,0,1)), new = TRUE)
boxplot(
  dat$cv5*100,
  dat$cv4*100,
  dat$cv3*100,
  dat$cv2*100,
  dat$cv1*100,
  xlab= "",
  ylim = c(0,275),
  yaxt = "n",
  ylab= "Coefficient of Variation",
  col = c("black"), border = "grey40")
axis(2, at = c(0,25,50,100,150,200,250), labels = 
       c("0","25", "50", "100", "150", "200", "250"))
axis(1, at = c(1:5), labels = 
       c("0.01","0.1", "1", "10", "100"))
mtext(expression(paste("Area (m"^"2",")")), 1, at = 3,3, cex = 1.2)

par(xpd = TRUE)
text(1,300, "(a)", font = 2)


# VERSION ONE
par(cex.lab = 1.2,  mar = c(4.5, 2, 2, 1))
par(fig=c(.3,1,0,01))
boxplot(
  all.groups$grass.cv5,
  all.groups$forb.cv5,
  all.groups$shrubs.cv5,
  all.groups$grass.cv4,
  all.groups$forb.cv4,
  all.groups$shrubs.cv4,
  all.groups$grass.cv3,
  all.groups$forb.cv3,
  all.groups$shrubs.cv3,
  all.groups$grass.cv2,
  all.groups$forb.cv2,
  all.groups$shrubs.cv2,
  all.groups$grass.cv1,
  all.groups$forb.cv1,
  all.groups$shrubs.cv1,
xlab= "",
xaxt ="n",
ylab= "Coefficient of Variation",
col = c("green", "blue","grey","green", "blue","grey","green", "blue","grey"))
axis(1, at = c(2,5,8,11,14), tick = FALSE,
     labels =  c("0.01", "0.1",  "1",  "10", "100" ))
mtext(expression(paste("Area (m"^"2",")")), 1, at = 8,3, cex = 1.2)
abline(v=c(3.5, 6.5, 9.5, 12.5), lty=3)
legend(12.6,250, c( "Grasses", "Forbs", "Shrubs"),
       col=c( "green", "blue","grey"), pt.cex =2.6, cex=1.2, pch = 15,
        bty = "n")

par(cex.lab = 1.2,  mar = c(4.5, 4, 2, 1))
par(cex.lab = 1.2, par(fig=c(0,.3,0,1)), new = TRUE)
boxplot(
        dat$cv5*100,
        dat$cv4*100,
        dat$cv3*100,
        dat$cv2*100,
        dat$cv1*100,
  xlab= "",
  ylim = c(0,275),
  ylab= "Coefficient of Variation",
  col = c("black"), border = "grey40")
axis(1, at = c(1:5), labels = 
       c("0.01","0.1", "1", "10", "100"))
mtext(expression(paste("Area (m"^"2",")")), 1, at = 3,3, cex = 1.2)




# All three FTs one Boxplot ####
par(mfrow = c(1,1))
boxplot(
    grass.cv5,
    grass.cv4,
  grass.cv3,
  grass.cv2,
  grass.cv1,
  forb.cv5,
  forb.cv4,
  forb.cv3,
  forb.cv2,
  forb.cv1,
  shrubs.cv5,
  shrubs.cv4,
  shrubs.cv3,
  shrubs.cv2,
  shrubs.cv1,
  xlab= "",
  ylab= "Coefficient of Variation",
  main = "Coefficient of Variation for Functional Types in Repeated Spatial Scales")
axis(1, at = c(1:15), labels = 
       c("0.01",  "0.1", "1", "10", "100","0.01",  "0.1", "1", "10", "100","0.01",  "0.1", "1", "10", "100"))
mtext("Area m^2", 1, at = 8,3)
legend(1,250, c( "Grasses", "Forbs", "Shrubs"),
       lty=c(1,1,1), col=c( "green", "pink","brown"),
       lwd=c(12,12,12))

########### OLD FIGURES ###########
# Boxplot of Within and Btwn for forbs

boxplot(
        forb.cv5.across,
        forb.cv5,
        forb.cv4.across,
        forb.cv4,
        forb.cv3.across,
        forb.cv3,
        forb.cv2.across,
        forb.cv2,
        forb.cv1.across,
        forb.cv1,
        forb.cv400.across,
        forb.cv1000.across,
        xlab= "",
        ylab= "Coefficient of Variation",
        col = "pink",
        main = "Forb Richness CVs Within Plots and Across Plots")
axis(1, at = c(1:12), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)
boxplot(
  grass.cv5.across,
  grass.cv5,
  grass.cv4.across,
  grass.cv4,
  grass.cv3.across,
  grass.cv3,
  grass.cv2.across,
  grass.cv2,
  grass.cv1.across,
  grass.cv1,
  grass.cv400.across,
  grass.cv1000.across,
  xlab= "",
  ylab= "Coefficient of Variation",
  col = "green",
  main = "Grass Richness CVs Within Plots and Across Plots")
axis(1, at = c(1:12), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)

boxplot(
  shrubs.cv5.across,
  shrubs.cv5,
  shrubs.cv4.across,
  shrubs.cv4,
  shrubs.cv3.across,
  shrubs.cv3,
  shrubs.cv2.across,
  shrubs.cv2,
  shrubs.cv1.across,
  shrubs.cv1,
  shrubs.cv400.across,
  shrubs.cv1000.across,
  xlab= "",
  ylab= "Coefficient of Variation",
  col = "lightblue",
  main = "Shrub Richness CVs Within Plots and Across Plots")
axis(1, at = c(1:12), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)

############### Same Panel ####
par(mfrow = c(1,3))

boxplot(
  forb.cv5.across,
  forb.cv5,
  forb.cv4.across,
  forb.cv4,
  forb.cv3.across,
  forb.cv3,
  forb.cv2.across,
  forb.cv2,
  forb.cv1.across,
  forb.cv1,
  forb.cv400.across,
  forb.cv1000.across,
  xlab= "",
  ylab= "Coefficient of Variation",
  col = "pink",
  main = "Forb Richness CVs Within Plots and Across Plots")
axis(1, at = c(1:12), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)
boxplot(
  grass.cv5.across,
  grass.cv5,
  grass.cv4.across,
  grass.cv4,
  grass.cv3.across,
  grass.cv3,
  grass.cv2.across,
  grass.cv2,
  grass.cv1.across,
  grass.cv1,
  grass.cv400.across,
  grass.cv1000.across,
  xlab= "",
  ylab= "Coefficient of Variation",
  col = "green",
  main = "Grass Richness CVs Within Plots and Across Plots")
axis(1, at = c(1:12), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)

boxplot(
  shrubs.cv5.across,
  shrubs.cv5,
  shrubs.cv4.across,
  shrubs.cv4,
  shrubs.cv3.across,
  shrubs.cv3,
  shrubs.cv2.across,
  shrubs.cv2,
  shrubs.cv1.across,
  shrubs.cv1,
  shrubs.cv400.across,
  shrubs.cv1000.across,
  xlab= "",
  ylab= "Coefficient of Variation",
  col = "brown",
  main = "Shrub Richness CVs Within Plots and Across Plots")
axis(1, at = c(1:12), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)
 
# All three FTs one Boxplot ####
par(mfrow = c(1,1))
boxplot(
  grass.cv5.across,
  grass.cv5,
  grass.cv4.across,
  grass.cv4,
  grass.cv3.across,
  grass.cv3,
  grass.cv2.across,
  grass.cv2,
  grass.cv1.across,
  grass.cv1,
  grass.cv400.across,
  grass.cv1000.across,
  forb.cv5.across,
  forb.cv5,
  forb.cv4.across,
  forb.cv4,
  forb.cv3.across,
  forb.cv3,
  forb.cv2.across,
  forb.cv2,
  forb.cv1.across,
  forb.cv1,
  forb.cv400.across,
  forb.cv1000.across,
  shrubs.cv5.across,
  shrubs.cv5,
  shrubs.cv4.across,
  shrubs.cv4,
  shrubs.cv3.across,
  shrubs.cv3,
  shrubs.cv2.across,
  shrubs.cv2,
  shrubs.cv1.across,
  shrubs.cv1,
  shrubs.cv400.across,
  shrubs.cv1000.across,
  xlab= "",
  ylab= "Coefficient of Variation",
  col = c("green","green","green","green","green","green","green","green","green","green","green","green",
          "pink","pink","pink","pink","pink","pink","pink","pink","pink","pink","pink","pink",
          "brown","brown","brown","brown","brown","brown","brown","brown","brown","brown","brown","brown"),
  main = "Richness CVs Within Plots and Across Plots For FTs")
axis(1, at = c(1:36), labels = 
       c("Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000",
         "Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000",
         "Across-Plot .01", "Within Plots .01", "Across-Plot .1", "Within Plots .1", "Across-Plot 1", "Within Plots 1",
         "Across-Plot 10", "Within Plots 10", "Across-Plot 100", "Within Plots 100", "Across Plot 400", "Across Plot 1000"), las=2)
legend(1,400, c( "Grasses", "Forbs", "Shrubs"),
       lty=c(1,1,1), col=c( "green", "pink","brown"),
       lwd=c(12,12,12))

# more scratch

axis(1, at = c(1:15), labels = 
       c("0.01",  "0.01", "0.01", "0.1", "0.1","0.1",  "1", "1", "1", "10","10","10", "100", "100", "100"))
mtext(expression(paste("Area (Meters"^"2)")), 1, at = 8,3)
legend(10,250, c( "Grasses", "Forbs", "Shrubs"),
       col=c( "green", "blue","grey"), pt.cex =3, cex=2, pch = 15,
       bty = "n")
