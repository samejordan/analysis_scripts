# chpt1 figures 2

# CODE FOR SPECIES AREA CURVE FIGURE IN MANUSCRIPT

dat <- read.csv(file = "/Users/samjordan/Dropbox/Biodiversity 2016 Data/misc/expandedrichness.csv", header = TRUE)
dat <- read.csv(file = "expandedrichness.csv", header = TRUE)

#excluding cheatgrass plots :/
dat <- dat[dat$plot!= 18 & 
             dat$plot!= 22 &
             dat$plot!= 26, ]

dat.ex.forbs <- read.csv(file ="expandedrichness_forbs_only.csv", header = TRUE)
dat.ex.shrubs <- read.csv(file ="expandedrichness_shrubs_only.csv", header = TRUE)
dat.ex.grass <- read.csv(file ="expandedrichness_grasses_only.csv", header = TRUE)

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

# new figure with scaled x axis



#objects for ranges
ay<-c(max(dat$value[dat$scale==5]),
      max(dat$value[dat$scale==4]),
      max(dat$value[dat$scale==3]),
      max(dat$value[dat$scale==2]),
      max(dat$value[dat$scale==1]),
      max(dat$value[dat$scale==400]),
      max(dat$value[dat$scale==1000]),
      min(dat$value[dat$scale==1000]),
      min(dat$value[dat$scale==400]),
      min(dat$value[dat$scale==1]),
      min(dat$value[dat$scale==2]),
      min(dat$value[dat$scale==3]),
      min(dat$value[dat$scale==4]),
      min(dat$value[dat$scale==5]))

gy <- (c(max(dat.ex.grass$value[dat.ex.grass$scale==5]),
         max(dat.ex.grass$value[dat.ex.grass$scale==4]),
         max(dat.ex.grass$value[dat.ex.grass$scale==3]),
         max(dat.ex.grass$value[dat.ex.grass$scale==2]),
         max(dat.ex.grass$value[dat.ex.grass$scale==1]),
         max(dat.ex.grass$value[dat.ex.grass$scale==400]),
         max(dat.ex.grass$value[dat.ex.grass$scale==1000]),
         min(dat.ex.grass$value[dat.ex.grass$scale==1000]),
         min(dat.ex.grass$value[dat.ex.grass$scale==400]),
         min(dat.ex.grass$value[dat.ex.grass$scale==1]),
         min(dat.ex.grass$value[dat.ex.grass$scale==2]),
         min(dat.ex.grass$value[dat.ex.grass$scale==3]),
         min(dat.ex.grass$value[dat.ex.grass$scale==4]),
         min(dat.ex.grass$value[dat.ex.grass$scale==5])))

fy <- (c(max(dat.ex.forbs$value[dat.ex.forbs$scale==5]),
         max(dat.ex.forbs$value[dat.ex.forbs$scale==4]),
         max(dat.ex.forbs$value[dat.ex.forbs$scale==3]),
         max(dat.ex.forbs$value[dat.ex.forbs$scale==2]),
         max(dat.ex.forbs$value[dat.ex.forbs$scale==1]),
         max(dat.ex.forbs$value[dat.ex.forbs$scale==400]),
         max(dat.ex.forbs$value[dat.ex.forbs$scale==1000]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==1000]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==400]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==1]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==2]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==3]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==4]),
         min(dat.ex.forbs$value[dat.ex.forbs$scale==5])))

sy <- (c(max(dat.ex.shrubs$value[dat.ex.shrubs$scale==5]),
         max(dat.ex.shrubs$value[dat.ex.shrubs$scale==4]),
         max(dat.ex.shrubs$value[dat.ex.shrubs$scale==3]),
         max(dat.ex.shrubs$value[dat.ex.shrubs$scale==2]),
         max(dat.ex.shrubs$value[dat.ex.shrubs$scale==1]),
         max(dat.ex.shrubs$value[dat.ex.shrubs$scale==400]),
         max(dat.ex.shrubs$value[dat.ex.shrubs$scale==1000]),
         min(dat.ex.shrubs$value[dat.ex.shrubs$scale==1000]),
         min(dat.ex.shrubs$value[dat.ex.shrubs$scale==400]),
         min(dat.ex.shrubs$value[dat.ex.shrubs$scale==1]),
         min(dat.ex.grass$value[dat.ex.grass$scale==2]),
         min(dat.ex.grass$value[dat.ex.grass$scale==3]),
         min(dat.ex.grass$value[dat.ex.grass$scale==4]),
         min(dat.ex.grass$value[dat.ex.grass$scale==5])))





# figure with ranges
par(cex.lab = 1.2,  mar = c(4.5, 4, 2, 1), mfrow = c(1,1))

boxplot(dat$value[dat$scale==5],
        dat$value[dat$scale==4],
        dat$value[dat$scale==3],
        dat$value[dat$scale==2],
        dat$value[dat$scale==2],
        dat$value[dat$scale==2],
        dat$value[dat$scale==2],col = "white",
        border = "white",
        xlab = "",
        ylim = c(0,55),
        xlim = c(.95,6.05),
        ylab = "Species Richness",
        at = c(1,2,3,4,5,5.35,6))
axis(1, at = c(c(1,2,3,4,5,5.35,6)), labels = 
       c("0.01" , "0.1" , "1", "10", "100", "400" , "1000"))
mtext(expression(paste("Area (m"^"2",")")),1, at = 3.5,3, cex = 1.2)

legend(1,50, c("All Species", "Forbs", "Grasses", "Shrubs"),
       lty=c(1,1,1,1), col=c("black", "blue", "green","navyblue"),
       lwd=c(4,4,4,4), bty = "n")

x <- c(1,2,3,4,5,5.35,6,6,5.35,5,4,3,2,1)
polygon(x,ay, col = "#00000033", border = NA)
polygon(x,sy, col = "#00008066", border = NA)
polygon(x,fy, col = "#87CEFF80", border = NA)
polygon(x,gy, col = "#00FF0033", border = NA)

# Mean lines
x <-  c(1,2,3,4,5,5.35,6)

lines (x,c(mean(dat$value[dat$scale==5]),
        mean(dat$value[dat$scale==4]),
        mean(dat$value[dat$scale==3]),
        mean(dat$value[dat$scale==2]),
        mean(dat$value[dat$scale==1]),
        mean(dat$value[dat$scale==400]),
        mean(dat$value[dat$scale==1000])),
       col = "black", lwd = 3)


lines(x,c(mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==5]),
        mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==4]),
        mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==3]),
        mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==2]),
        mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==1]),
        mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==400]),
        mean(dat.ex.shrubs$value[dat.ex.shrubs$scale==1000])),
      lwd = 3, col = "navyblue")

lines(x,c(mean(dat.ex.forbs$value[dat.ex.forbs$scale==5]),
        mean(dat.ex.forbs$value[dat.ex.forbs$scale==4]),
        mean(dat.ex.forbs$value[dat.ex.forbs$scale==3]),
        mean(dat.ex.forbs$value[dat.ex.forbs$scale==2]),
        mean(dat.ex.forbs$value[dat.ex.forbs$scale==1]),
        mean(dat.ex.forbs$value[dat.ex.forbs$scale==400]),
        mean(dat.ex.forbs$value[dat.ex.forbs$scale==1000])),
      lwd = 3, col = "blue")

lines(x,c(mean(dat.ex.grass$value[dat.ex.grass$scale==5]),
        mean(dat.ex.grass$value[dat.ex.grass$scale==4]),
        mean(dat.ex.grass$value[dat.ex.grass$scale==3]),
        mean(dat.ex.grass$value[dat.ex.grass$scale==2]),
        mean(dat.ex.grass$value[dat.ex.grass$scale==1]),
        mean(dat.ex.grass$value[dat.ex.grass$scale==400]),
        mean(dat.ex.grass$value[dat.ex.grass$scale==1000])),
      lwd = 3, col = "green")




# first draft
adjustcolor( "red", alpha.f = 0.05)
adjustcolor( "green", alpha.f = 0.05)
adjustcolor( "blue", alpha.f = 0.05)
adjustcolor( "brown", alpha.f = 0.1)
#alternate - BETTER
adjustcolor( "red", alpha.f = 0.1)
adjustcolor( "green", alpha.f = 0.2)
adjustcolor( "blue", alpha.f = 0.1)
adjustcolor( "brown", alpha.f = 0.2)
#alternate colors 
adjustcolor( "black", alpha.f = 0.2)
adjustcolor( "green", alpha.f = 0.2)
adjustcolor( "skyblue1", alpha.f = 0.5)
adjustcolor( "navyblue", alpha.f = 0.4)