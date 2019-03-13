# Species frequency distribution


#SPECIES FREQUENCY FIGURE FOR MANUSCRIPT


rm(list=ls(all=TRUE))
getwd()
setwd("C:/Users/sej36/Dropbox/Biodiversity 2016 Data/Misc")
setwd("C:/Users/sej36/Dropbox/Chapter2")
dat4 <- read.csv(file = "data_for_sppfreq_figure.csv", header = TRUE)

library(plyr)

# new code ####

# new figure

par(cex = 1.4)
plot(dat4$freq,col = as.character(dat4$color),
     type='h',bty="l",
     lwd = 2,
     ylim=c(0,50),
     cex.axis = 1,
     ylab = "Number of plots",
     xlab = "Rank-ordered Species")


legend(150,40,c("Grasses", "Forbs", "Shrubs", "Succulents"), 
       col = c("green", "blue", "grey", "yellow"),
       pch = 15,
       bty = "n",
       cex = 1.1,
       pt.cex = 3)






#### EXTRAS ##


dat4$color <- "black"
dat4$color[dat4$functional_group == "g"] <- "green"
dat4$color[dat4$functional_group == "f"] <- "blue"
dat4$color[dat4$functional_group == "s"] <- "grey"
dat4$color[dat4$functional_group == "c"] <- "yellow"


# first frequency vs abundance figures
plot(jitter(log(dat4$cover))~ jitter(dat4$freq) , cex = 1.2, 
     xlab = "number of plots detected",
     col = dat4$color,
     pch = as.numeric(dat4$native_invasive),
     ylab = "log(mean cover values)")


# TESTING FOR A NEW FIGURE

dat4 <- read.csv(file = "avg_cover_frequency_all_spp.csv", header = TRUE)

dat4 <- read.csv(file = "sorted_frequency_for_plotting.csv", header = TRUE)

#might have to combine unknowns
par(mfrow = c(1,1), cex = 1.2)

#dat4 <- dat2[!duplicated(dat2$species),]
#dat4<-  dat4[order(dat4$freq),]

dat4 <- dat4[order(dat4$freq, decreasing = TRUE),]

dat4$color <- "black"
dat4$color[dat4$functional_group == "g"] <- "green"
dat4$color[dat4$functional_group == "f"] <- "blue"
dat4$color[dat4$functional_group == "s"] <- "grey"
dat4$color[dat4$functional_group == "c"] <- "gold"


mycol<- palette(c( "black", "yellow","green", "blue", "grey"))

barplot(sort(dat4$freq, decreasing = TRUE),
        ylab = "Number of plots",
        xlab = "Rank-order",
        col = as.character(dat4$color),
        border = NA,
        cex.axis = 1.2)
axis_values <-barplot(sort(dat4$freq, decreasing = TRUE),
        ylab = "Number of plots",
        xlab = "Rank-order",
        col = as.character(dat4$color),
        border = NA,
        cex.axis = 1.2)

axis(side = 1, at = axis_values)

legend(200,50, c( "Grasses", "Forbs", "Shrubs", "Succulents", "Unknown"),
       col=c( "green", "blue","grey", "yellow", "black"), pt.cex =1.5, cex=1.2, pch = 15,
       bty = "n")
abline(v=10, lty = 2, lwd = 2, col = "black")
abline(v=100, lty = 2, lwd = 2, col = "black")

text(30,45,"10 species" )
text(120,45,"100 species" )

# OLD THREE PANEL FIGURE FROM PREVIOUS VERSIONS
par(mar = c(4, 4, 7, 2) + 0.2, mfrow = c(1,3))
mtext("Frequency of occurences of individual species across plots", side = 3,
      line = -2, outer = TRUE, cex = 2, font = 2)


barplot(sort(dat2$freq, decreasing = TRUE),
        main = "All occurences (n=304)", 
        #ylab = "Number of occurences",
        col = "lightgrey",
        border = NA,
        space = .001,
        cex.axis = 2)

dat3<- dat2[dat2$freq>10,]
barplot(sort(dat3$freq, decreasing = TRUE),
        main = ">10 occurences (n=49)",
        col = "lightgrey",
        border = NA,
        space = .001,
        cex.axis = 2)
dat4<- dat2[dat2$freq<10,]
barplot(sort(dat4$freq, decreasing = TRUE),
        main = " <10 occurences (n=255)",
        col = "lightgrey",
        border = NA,
        space = .001,
        cex.axis = 2)
       # ylim = c(0,50))

### PRESENTATION FIGURE #####
#dat4 <- dat2[!duplicated(dat2$species),]
#dat4$sqrtcover <- dat3$sqrtcover
#dat4$cover <- dat5$cover

#dat2 <- merge(dat2, dat3, by = "species")
#dat2 <- merge(dat2, dat5, by = "species")
# new code ####

# new figure

par(cex = 1.4, font.axis = 2, font.lab = 2)
plot(dat4$freq,col = as.character(dat4$color),
     type='h',bty="l",
     lwd = 2,
     ylim=c(0,50),
     cex.axis = 1,
     font.axis = 2,
     ylab = "Number of plots",
     xlab = "Rank-ordered Species")

par(cex = 1.4, font.axis = 1, font.lab = 1)

legend(150,40,c("Grasses", "Forbs", "Shrubs", "Succulents"), 
       col = c("green", "blue", "grey", "yellow"),
       pch = 15,
       bty = "n",
       cex = 1,
       pt.cex = 3)


