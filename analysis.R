## -- Analysis of Plethodon clutch and developmental data

# Authors: Zachary R. Lewis, Brooks G. Mathewson, James Hanken

####################
# Load dependencies
####################

library(tidyverse)
library(lubridate) # to work with dates, for instance the day of year = yday(Collection.Date)
library(ggplot2)
library(grid)
library(gridExtra)
library(broom)

###################
# load clutch data
###################

clutchData <-read_csv("clutchData.csv", 
                      col_types=cols(Collection.Date= col_date(format="%m/%d/%y")))
head(clutchData)
data.precip <-read_csv("clutchDataClimate.csv", 
                       col_types=cols(Collection.Date= col_date(format="%m/%d/%y")))
stageData <-read_csv("staging_at_15C.csv")
head(stageData)

# some renaming

Clutch.Number=clutchData[,1]
Collection.Date=clutchData[,2]
Calendar.Date=clutchData[,3]
Collection.Site=clutchData[,4]
Stage=clutchData[,5]
Clutch.Size=clutchData[,6]
Year=clutchData[,7]
Simple.Site=clutchData[,8] #HF is a bin of Harvard Pond, Slab City, North of Slab City and Rutland Brook
#defining levels for categorical variables
yearLevels<-c("2012","2013","2014","2015")
cat.Year<-parse_factor(clutchData$Year,yearLevels)
siteLevels <- c("HF", "WM", "WB")
cat.Simple.Site <-parse_factor(clutchData$Simple.Site,siteLevels)

#note that HF is a bin of Harvard Pond, Slab City, North of Slab City and Rutland Brook. WB is Willard Brook. WM is Wachusett Meadows.

###############################
# Plot developmental timecourse
###############################

# note: to output the inset ("blowup") of early stages, need to un comment the viewport code and run in console directly


mainplot <- ggplot(stageData, aes(x=DPF,y=Stage)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Days Post-Oviposition (dpo)", y = "Stage") +
  theme_classic()

blowup <- ggplot(stageData, aes(x=DPF,y=Stage)) + 
  geom_point() + 
  geom_line() + 
  labs(x = "dpo", y = "Stage") +
  theme_classic(base_size = 8) + #reducing font size here
  ylim(2, 7) +
  xlim(1,6) +
  #theme_set(theme_classic(base_size = 8))
  
ggsave( "figure1.pdf", mainplot, device=pdf, width=5, height=4 )

# uncomment the following lines to save the blowup 

# pdf("figure1.pdf", width = 6, height = 4)
# subvp <- viewport(width = 0.4, height = 0.4, x = 0.795, y = 0.32)
# blowup
# print(mainplot)
# 
# print(blowup, vp = subvp)
# dev.off()

#############################################
# plot stage by collection date scatter plots
#############################################

stage_by_site <- ggplot(clutchData, aes(x=yday(Collection.Date),y=Stage,col=Simple.Site)) + 
  geom_point() + 
  geom_smooth(method =lm) + #can be left blank for no linear model
  scale_colour_hue(l=50) + #make the colors a little darker
  labs(x = "Collection Day") +
  theme_classic() + #removes grey panel background 
  scale_colour_discrete(name  = "Field\nSite") +
  theme(legend.position = "bottom")

#display clutch size as color, showing decline in clutch size over time
stage_by_clutch <- ggplot(data = subset(clutchData, !is.na(Clutch.Size)),  #excluding those with no clutch size
                          aes(x=yday(Collection.Date),y=Stage,col=Clutch.Size)) + 
  geom_point(size=3) +
  labs(x = "Collection Day", y = "Stage") +
  scale_colour_gradientn(colours = heat.colors(4, alpha = 0.5), name="Clutch\nSize") +
  theme_classic() + #removes grey panel background
  theme(legend.position = "bottom")

#adding lettering
stage_by_site <- arrangeGrob(stage_by_site, top = textGrob("A", x = unit(0, "npc")
                                                           , y   = unit(1, "npc"), just=c("left","top"),
                                                           gp=gpar(col="black", fontsize=14, fontfamily="")))

stage_by_clutch <- arrangeGrob(stage_by_clutch, top = textGrob("B", x = unit(0, "npc")
                                                               , y = unit(1, "npc"), just=c("left","top"),
                                                               gp=gpar(col="black", fontsize=14, fontfamily="")))

figure2 <- grid.arrange(stage_by_site, stage_by_clutch, ncol = 2)

#export figure

ggsave( "figure2.pdf", figure2, device=pdf, width=6, height=4 )

################################
# fitting linear model to stage
################################

#subset to just Harvard Forest Data, if desired
#clutchData_HF <-dplyr::filter(clutchData, Simple.Site %in% c("HF"))

#plot collection date by stage
collectionDate_by_stage <- ggplot(clutchData, aes(x=Stage,y=yday(Collection.Date))) + 
  geom_point(size=3) + 
  # geom_smooth(method =lm) + #can be left blank for no linear model
  labs(y = "Collection Day") +
  theme_classic() + #removes grey panel background 
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 18), ylim=c(155,190)) + #change x and y limits
  scale_x_continuous(expand = c(0, 0), limits = c(0, 15)) # make x start at 0

model<-lm(yday(Collection.Date)~ Stage, data=clutchData)
#summary(model)



#' Parses parses a linear model and returns slope intercept, etc.
#' 
#' @param lm linear model to extract
#' @return values of slope intercept, etc
#' @export
#' Adapted from Peter Solymos

modelExtract <-function(lm)
{
  out <- c(
    lm$coefficients[1],
    lm$coefficients[2],
    length(lm$model$y),
    summary(lm)$coefficients[2,2],
    pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
       summary(lm)$fstatistic[3], lower.tail = FALSE),
    summary(lm)$r.squared)
  names(out) <- c("intercept","slope","n","slope.SE","p.value","r.squared")
  return(out)}

modelFeatures<-as.data.frame(modelExtract(model))



#' Parses parses a linear model and returns slope intercept for 
#' printing as annotation on figure
#' @param m linear model to extract
#' @return slope intercept formula and r-squared
#' @export
#' Adapted from Fernando (fgtaboada) https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

figure6<- collectionDate_by_stage + 
  geom_abline(intercept = modelFeatures[1,], slope = modelFeatures[2,], 
              color="red", linetype="dashed", size=1.5) +
  annotate("text", x = 5, y = 190,
           label = lm_eqn(lm(yday(Collection.Date) ~ Stage, data=clutchData)), 
           colour="black", size = 4, parse=TRUE)

ggsave( "figure6.pdf", figure6, device=pdf, width=6, height=4 )

####################
#clutch size by year
####################

clutch_by_year <- ggplot(data=clutchData, mapping = aes(y=Clutch.Size,x=cat.Year)) +
  geom_boxplot() + 
  geom_jitter(width=0.1, aes(colour=yday(Collection.Date))) + #added data points here
  scale_colour_gradientn(colours = heat.colors(4, alpha = 0.6), name="Collection \nDay") + #color scale for jitter
  theme_classic() +
  labs(x = "Year", y = "Clutch Size")


collection_date <- ggplot(data=clutchData, mapping = aes(yday(Collection.Date), col=cat.Year)) +
  geom_freqpoly(binwidth = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) +
  theme_classic() +
  guides(colour = guide_legend(title="Year")) +
  labs(x = "Collection Day", y = "Clutches") +
  theme(legend.position = "top")

#adding lettering
clutch_by_year <- arrangeGrob(clutch_by_year, top = textGrob("A", x = unit(0, "npc")
                                                             , y   = unit(1, "npc"), just=c("left","top"),
                                                             gp=gpar(col="black", fontsize=14, fontfamily="")))

collection_date <- arrangeGrob(collection_date, top = textGrob("B", x = unit(0, "npc")
                                                               , y = unit(1, "npc"), just=c("left","top"),
                                                               gp=gpar(col="black", fontsize=14, fontfamily="")))

figure4 <- grid.arrange(clutch_by_year, collection_date, ncol = 2)

ggsave( "figure4.pdf", figure4, device=pdf, width=6.5, height=4 )


#####################
# clutch size by date
#####################

# filtering out Willard Brook because only a single day of observation with clutch sizes:
clutchData_noWB <-dplyr::filter(clutchData, Simple.Site %in% c("HF", "WM"))
clutchData_noWB

size_by_date <- ggplot(clutchData_noWB, aes(x=yday(Collection.Date),y=Clutch.Size,col=Simple.Site)) + 
  geom_point() + 
  geom_smooth(method =lm) + #can be left blank for no linear model
  scale_colour_hue(l=50) + #make the colors a little darker
  labs(x = "Date", y = "Clutch Size") +
  scale_colour_discrete(name  = "Field Site") +
  guides(colour = guide_legend(title="Field Site")) +
  #guides(colour=FALSE) + #remove legend
  scale_colour_brewer(palette="Set1") +
  theme_classic() +
  theme(legend.position = "bottom")

size_by_stage <- ggplot(clutchData_noWB, aes(x=Stage,y=Clutch.Size,col=Simple.Site)) + 
  geom_point() + 
  geom_smooth(method =lm) + #can be left blank for no linear model
  scale_colour_hue(l=50) + #make the colors a little darker
  guides(colour = guide_legend(title="Field Site")) +
  labs(x = "Stage", y = "Clutch Size") +
  scale_colour_discrete(name  = "Field Site") +
  scale_colour_brewer(palette="Set1") +
  theme_classic() +
  theme(legend.position = "bottom")

#adding lettering
size_by_date <- arrangeGrob(size_by_date, top = textGrob("A", x = unit(0, "npc")
                                                         , y   = unit(1, "npc"), just=c("left","top"),
                                                         gp=gpar(col="black", fontsize=14, fontfamily="")))

size_by_stage <- arrangeGrob(size_by_stage, top = textGrob("B", x = unit(0, "npc")
                                                           , y = unit(1, "npc"), just=c("left","top"),
                                                           gp=gpar(col="black", fontsize=14, fontfamily="")))

figure3 <- grid.arrange(size_by_date, size_by_stage, ncol = 2)

ggsave( "figure3.pdf", figure3, device=pdf, width=6, height=4 )


#######################
# statistical analyses
#######################

# There is no significant difference in clutch sizes between sites:

res <- TukeyHSD(aov(lm(Clutch.Size~Simple.Site, data=clutchData)))
write.csv(tidy(res), file="TableS2.output.csv")

# There are significant differences in clutch size between certain years:

TukeyHSD(aov(lm(Clutch.Size~cat.Year, data=clutchData)))

# There is no effect of site of stage:

TukeyHSD(aov(lm(Stage~Simple.Site, data=clutchData)))

# Clutch size declines over the years: 

summary(lm(Clutch.Size~Year, data=clutchData))

# Year and collection date explain the variation in stage.
stage.lm <-lm(Stage~ Year + Calendar.Date + Simple.Site, data=clutchData)
summary(stage.lm)
#plot(residuals(stage.lm), fitted(stage.lm))

#write output to table
write.csv(tidy(stage.lm), file="TableS1.output.csv")


# Clutch size declines as a function of date:

clutchDecline<-lm(formula = Clutch.Size ~ Stage * Calendar.Date, data = clutchData)
summary(clutchDecline)

#write output to table
write.csv(tidy(clutchDecline), file="Table2.output.csv")


