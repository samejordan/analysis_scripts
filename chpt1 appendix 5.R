# Chapter one appendices

### CREATES STARGAZER TABLES OF FINAL MODELS
### FINAL MODELS COME FROM THE FINAL MODELS SCRIPT



# summary tables for models
##GRASS####
####################ecohydrological#
setwd("C:/Users/sej36/Dropbox/Biodiversity/Chapter 1/Analysis/Manuscript Figures/Appendix_tables")
library(stargazer)
eg <- stargazer(new.eg4,new.eg16,new.eg2a, new.eg6a, title="", align=TRUE, type = "html",
                out="new_grass_ecohydro.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(new.eg4),1), round(AIC(new.eg16),1),round(AIC(new.eg2a),1),round(AIC(new.eg6a),1))),
                dep.var.labels="Grass richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
#######################BIOTIC
bg <- stargazer(bg2,bg11,bg12, title="", align=TRUE, type = "html",
                out="new_grass_biotic.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(bg2),1), round(AIC(bg11),1),round(AIC(bg12),1))),
                dep.var.labels="Grass richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
#######################MACROCLIMATIC
mg <- stargazer(gm1,gm5,gm6,gm7, title="", align=TRUE, type = "html",
                out="new_grass_macroclimate.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(gm1),1), round(AIC(gm5),1),round(AIC(gm6),1),round(AIC(gm7),1))),
                dep.var.labels="Grass richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
#####################COMBINED
cg <- stargazer(new.cg1,new.cg2,new.cg3,new.cg9, title="", align=TRUE, type = "html",
                out="new_grass_combined.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(new.cg1),1), round(AIC(new.cg2),1),round(AIC(new.cg3),1),round(AIC(new.cg9),1))),
                dep.var.labels="Grass richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
##FORBS####
####################ecohydrological
ef <- stargazer(ef3,ef11b,ef2b,ef3b, title="", align=TRUE, type = "html",
                out="new_forb_ecohydro.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(ef3),1), round(AIC(ef11b),1),round(AIC(ef2b),1),round(AIC(ef3b),1))),
                dep.var.labels="Forb richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
#######################MACROCLIMATIC
mf <- stargazer(mf1,mf2,mf4,mf5, title="", align=TRUE, type = "html",
                out="new_forb_macroclimate.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(mf1),1), round(AIC(mf2),1),round(AIC(mf4),1),round(AIC(mf5),1))),
                dep.var.labels="Forb richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
####################COMBINED
ef <- stargazer(cf2a,cf3a,cf6a,cf7a, title="", align=TRUE, type = "html",
                out="new_forb_combined.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(cf2a),1), round(AIC(cf3a),1),round(AIC(cf6a),1),round(AIC(cf7a),1))),
                dep.var.labels="Forb richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)

##SHRUBS####
####################ecohydrological
ef <- stargazer(es1,es2b,es3b,es9b, title="", align=TRUE, type = "html",
                out="new_shrub_ecohydro.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(ef3),1), round(AIC(ef11b),1),round(AIC(ef2b),1),round(AIC(ef3b),1))),
                dep.var.labels="Shrub richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)


##TOTAL####
####################ecohydrological

#need farm out the last model from the new dredge
ef <- stargazer(new.et1, new.et5, etb27, new.et8, title="", align=TRUE,type = "html",
                out="new_total_ecohydro.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(new.et1),1), round(AIC(new.et5),1),round(AIC(etb27),1),round(AIC(new.et8),1))),
                dep.var.labels="Total richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)


#######################MACROCLIMATIC
mf <- stargazer(mc3,mc6,mc12,mc15, title="", align=TRUE, type = "html",
                out="new_total_macroclimate.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(mc3),1), round(AIC(mc6),1),round(AIC(mc12),1),round(AIC(mc15),1))),
                dep.var.labels="Total richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)
####################COMBINED
#ct1b.p <- c(NA,5,9,54,6)
#ct2b.p <- c(NA,16,7,49)
#ct6b.p <- c(NA,12,47,7)
#ct10b.p<- c(NA,6,6,50)
stargazer(new.ct1, new.ct3, new.ct4, new.ct7, title="", align=TRUE, type = "html",
                out="new_total_combined.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                no.space = TRUE, 
                add.lines=list(c("AIC", round(AIC(new.ct1),1), round(AIC(new.ct3),1),round(AIC(new.ct4),1),round(AIC(new.ct7),1))),
                dep.var.labels="Total richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)




#scratch
egparts <- c(NA, 1, 2, 3)
stargazer(eg3a,eg6,eg18,eg4b, title="ECOHYDROLOGICAL MODELS OF GRASS RICHNESS", align=TRUE, type = "html",
                out="test.htm", se = list(NA,NA,NA,NA), digits = 2, omit.stat = "n",
                add.lines=list(c("AIC", round(AIC(eg3a),1), round(AIC(eg6),1),round(AIC(eg18),1),round(AIC(eg4b),1))),
                dep.var.labels="Grass richness at 1000 square meters",
                notes = "",
                notes.label = "",
                notes.append = FALSE)

#OTHER POSSIBLE CHAPTER ONE SUPPLEMENTAL MATERIAL
setwd("C:/Users/sej36/Dropbox/Biodiversity 2016 Data")
dat <- read.csv("expandedrichness.csv", header = TRUE)
head(dat)

temp <- aggregate(dat$value, by = list(dat$scale, dat$plot), FUN = mean)
colnames(temp) <- c("scale", "plot", "value")
write.csv(temp, file = "chapter1_appendix_1.csv")
temp <- aggregate(dat$value, by = list(dat$plot, dat$scale), FUN = mean)
write.csv(temp, file = "chapter1_appendix_1_alternate_format.csv")

