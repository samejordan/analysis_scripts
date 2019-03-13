# Chapter one figure four

#VARIANCE PARTITIONING FIGURE FOR MANUSCRIPT
#CODE FOR VARPART OBJECTS IS BELOW FIGURES
dat <- read.csv(file = "data_for_varpart.csv", header = TRUE)

library(vegan)
library(zetadiv)

# figures ####
par(mfrow = c(2,2), mar = c(.5, .01, .7, .01))
#total
new.ct1_colors <- c("blue","green","green","green",
                 "darkcyan","darkcyan","darkcyan","darkcyan","darkgreen","darkgrey","darkgrey","darkgrey","darkgreen",
                 "darkcyan", "darkcyan","black")
new.ct1_labels <- c("A 5.1%" , "B 8.8%",  "C 54.0%" ,"D 5.8%",  "E " ,"","","F ","","","","","G ","","H",  "U 46.5%")
pie.neg(new.ct1$part$indfract$Adj.R.square, col = new.ct1_colors, labels = new.ct1_labels, cex=1.2,
        main = "Total Richness", cex.main = 1.2, radius = .99)
#grass
new.cg1_colors <- c("lightgrey","green","green","green","darkcyan","darkcyan","darkcyan","darkcyan",
                "pink", "pink", "pink", "pink", "darkgreen", "black","black", "black")
new.cg1_labels <- c( "I 3.7%" ,  "J 15.6%" , "K 44.9%" ,  "D 1.6%" , "M" ,  "" , "N " ,
                                 "O " ,  "" ,"" , "" , "" , "P" ,  "" , "" ,  "U 49.3%" )
pie.neg(new.cg1$part$indfract$Adj.R.square, col = new.cg1_colors, cex = 1.1, 
      labels = new.cg1_labels, 
     main = "Grass Richness", cex.main = 1.2, radius = .99)
#forb
cf1_labels <- c("A 5.6%" , "B 7.9%" , "C 47.2%" ,"Q 2.9%",  "", "" ,"" ,  "U 53.8%")
cf1_colors <- c("blue","green","green","darkgreen","darkgrey","darkgrey","darkcyan","black")
pie.neg(cf1$part$indfract$Adj.R.square, col = cf1_colors, labels = cf1_labels, cex=1.2,
        main = "Forb Richness", cex.main = 1.2, radius = .99)
#shrub
es1_labels <- c("R 13.2%", "", "S 12.4%", "U 79.2%")
es1_colors <- c("green","green","green","black")
pie.neg(es1$part$indfract$Adj.R.square, col = es1_colors, labels = es1_labels, cex=1.2,
        main = "Shrub Richness", cex.main = 1.2, radius = .99)



# Combined models ####
#total


new.ct1 <- varpart(dat$mean1000 ,  dat$MAT ,dat$clay.total , dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean, dat$SWP_topLayers_DailyMin_MPa_mean)

#grass

#cg3 <- varpart( dat$grass1000 , dat$elevation , dat$SWP_bottomLayers_DailyMin_MPa_mean , 
#            dat$WetSoilPeriods_SWPcrit1500kPa_NSadj_bottomLayers_AnyLayerWet_Duration_Total_days_mean) 

new.cg1 <- varpart(dat$grass1000 , dat$ARTR.density , dat$elevation , 
                   dat$SWAbulk_SWPcrit3900kPa_topLayers_m4.9_mm_mean , dat$SWP_topLayers_DailyMin_MPa_mean)
#forb

cf1 <- varpart( dat$forb1000 , dat$MAT, dat$clay.total ,  dat$SWAbulk_SWPcrit3900kPa_topLayers_m10.3_mm_mean)


# Ecohydrological ####

#shrub
es1 <- varpart(dat$shrub1000 , dat$DrySoilPeriods_SWPcrit3900kPa_NSadj_bottomLayers_AllLayersDry_Duration_Total_days_mean , 
            dat$slope)


########### OLD CODE ###############
# figures ####
par(mfrow = c(2,2), mar = c(1.5, .1, 1.2, .1), cex.main = 2)
#total
# Calculate the percentage variance explained for each, rounded to one decimal place
#new.ct1_labels <- round(new.ct1$part$indfract$Adj.R.square* 100, 1)
#new.ct1_labels <- new.ct1_labels[new.ct1_labels >= 0]

# Concatenate a '%' char after each value
#new.ct1_labels <- paste(new.ct1_labels, "%", sep="")
new.ct1_labels <- c("6.2% A" , "5.5% B",  "49.8% C" ,"2.6% D",  "" ,"" ,"" ,  "52.3% U")
new.ct1_colors <- c("green","blue","green","darkgreen","darkgrey","darkgrey","darkcyan","black")

pie.neg(new.ct1$part$indfract$Adj.R.square, col = new.ct1_colors, labels = new.ct1_labels, cex=1.2,
        main = "Total Richness", cex.main = 1.9)

#grass
# Calculate the percentage variance explained for each, rounded to one decimal place
#new.cg1_labels <- round(new.cg1$part$indfract$Adj.R.square* 100, 1)

# Concatenate a '%' char after each value
#new.cg1_labels <- paste(new.cg1_labels, "%", sep="")
new.cg1_labels <- c( "3.4% E" ,  "11.6% F" , "9.7% G" ,  "40.1% H" , "0.7% I" ,  "" , "4.1% J" ,
                 "0.9% K" ,  "" ,"" , "" , "" , "2.1% L" ,  "" , "" ,  "47.8% U" )
new.cg1_colors <- c("lightgrey","green","green","green","darkgreen","darkgreen","darkgreen","darkgreen",
                "pink", "pink", "pink", "pink", "darkcyan", "black","black", "black")

pie.neg(new.cg1$part$indfract$Adj.R.square, col = new.cg1_colors, labels = new.cg1_labels, cex=1.1,
        main = "Grass Richness", cex.main = 2)

#forb
# Calculate the percentage variance explained for each, rounded to one decimal place
#cf1_labels <- round(cf1$part$indfract$Adj.R.square* 100, 1)

# Concatenate a '%' char after each value
#cf1_labels <- paste(cf1_labels, "%", sep="")
cf1_labels <- c("7.9% A" , "5.6% B" , "47.2% C" ,"2.9% D",  "", "" ,"" ,  "53.8% U")
cf1_colors <- c("green","blue","green","darkgreen","darkgrey","darkgrey","darkcyan","black")

pie.neg(cf1$part$indfract$Adj.R.square, col = cf1_colors, labels = cf1_labels, cex=1.2,
        main = "Forb Richness", cex.main = 2)

#shrub
# Calculate the percentage variance explained for each, rounded to one decimal place
#es1_labels <- round(es1$part$indfract$Adj.R.square* 100, 1)

# Concatenate a '%' char after each value
#es1_labels <- paste(es1_labels, "%", sep="")
es1_labels <- c("13.2% N", "", "12.4% O", "79.2% U")
es1_colors <- c("green","green","green","black")

pie.neg(es1$part$indfract$Adj.R.square, col = es1_colors, labels = es1_labels, cex=1.2,
        main = "Shrub Richness", cex.main = 2)

#legend

legend("topright", inset=c(1,-1), legend=c("A","B"), pch=c(1,3), title="Group")

par(xpd=TRUE)
legend(-1.8,1.2,c("Ecohydrological", "Macroclimatic", "Biotic", "Shared", "Shared", "Unexplained"), 
       col = c("green", "blue", "lightgrey", "darkgreen", "darkcyan","black"),
       pch = 15,
       bty = "n",
       cex = 1.8,
       pt.cex = 5)

# from Kyle ####

#variance explained for all variables, including shared and unexplained
var100m <- c(localvar.100,regvar.100,sharevar.100,unexvar.100)

#create colors for 4 categories
colors.100 <- c("red","black","green","blue")

# Calculate the percentage variance explained for each, rounded to one decimal place
var100m_labels <- round(var100m/sum(var100m) * 100, 1)

# Concatenate a '%' char after each value
var100m_labels <- paste(var100m_labels, "%", sep="")

# Create a pie chart with defined heading and custom colors
pie(var100m, col=colors.100,labels=var100m_labels,cex=1.2)
par(xpd=NA)
legend(-2.5,.4, c("Soils","Climate","Shared","Unexplained"),bty='n', 
       cex=1.2,fill=colors.1)
par(xpd=F)
