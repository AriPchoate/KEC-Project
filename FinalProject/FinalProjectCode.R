setwd("~/Desktop/RCoding/FinalProject")
largeData <- read.csv("newdatatable.csv", header=TRUE)
#View(largeData)
 
#-------------------------------------------------------------------------------
# Table for Looking at difference of one variable across two others

#****This can be easily changed to do a graph of any other thing just by changing the columns I put in java****


# install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/Desktop/RCoding/FinalProject")
largeData <- read.csv("newdatatable.csv", header=TRUE)

ja = filter(largeData, Infected== 0)
#View(ja)

java = subset(ja, select = c(Mycorrhizae, Phosphorus, AboveBiomass))
#View(java)


PhosphorusCat <- factor(java$Phosphorus, levels = c("Low", "Med", "High"))
MycorrCat = factor(java$Mycorrhizae, labels = c("No Mycorrhizae", "Mycorrhizae"))
summary(MycorrCat)

p <- ggplot(java, aes(x = PhosphorusCat, y = AboveBiomass, fill = MycorrCat)) + 
  geom_boxplot() +
  labs(x="Phosphorus Levels", y="Above-Ground Biomass (g)", fill="Myco") +
  scale_fill_manual(values = c("#A6D96A", "#4D9221")) +
  theme_light() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )

print(p)

summary(fm1<-aov(java$AboveBiomass~PhosphorusCat))
fm1Tukey = TukeyHSD(fm1, "PhosphorusCat");fm1Tukey

summary(fm1<-aov(java$AboveBiomass~MycorrCat))
fm1Tukey = TukeyHSD(fm1, "MycorrCat");fm1Tukey

#-------------------------------------------------------------------------------

#Disease calculation functions

diseaseIncidence <- function(numInf, total) {
  return (numInf/total) * 100
}

PDI <- function(scores, total) {. #Percent disease index - NEED TO FIX THIS
  pass. #[SUM((ratingScore - numberRate)/(total number of plants * 4)] * 100
}

diseasecontrol <- function() {  #- NEED TO FIX THIS
  #[(control PDI - treatment PDI) / control PDI] × 100
}

#install(ggpubr)
library(ggpubr)
ggboxplot(largeData, x = "Phosphorus", y = "AboveBiomass",  color = "Mycorrhizae", palette = c("#A6D96A", "#4D9221"), facet.by = "Infected")











#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

######Doing anova and tukey

#***************************** Three way anova - can easily change independent variable
results <- aov(AboveBiomass ~ Infected * Mycorrhizae * Phosphorus, data = largeData)
summary(results)




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#*********I do not know if I will actually need these tukey tests for my analysis, but I coded them anyway

#Not infected group
hikaru = filter(largeData, Infected == 0)

PhosphorusCat <- factor(hikaru$Phosphorus, levels = c("Low", "Med", "High"))
MycorrCat <- factor(hikaru$Mycorrhizae, labels = c("No Mycorrhizae", "Mycorrhizae"))

diseaseAssessment = {} #DO IT

summary(fm1<-aov(hikaru$AboveBiomass~PhosphorusCat))
fm1Tukey = TukeyHSD(fm1, "PhosphorusCat");fm1Tukey

summary(fm2<-aov(hikaru$AboveBiomass~MycorrCat))
fm2Tukey = TukeyHSD(fm2, "MycorrCat");fm2Tukey

#

summary(fm3<-aov(hikaru$BelowBiomass~PhosphorusCat))
fm3Tukey = TukeyHSD(fm3, "PhosphorusCat");fm3Tukey

summary(fm4<-aov(hikaru$BelowBiomass~MycorrCat))
fm4Tukey = TukeyHSD(fm4, "MycorrCat");fm3Tukey

#

summary(fm5<-aov(hikaru$AMF.Colonization~PhosphorusCat))
fm5Tukey = TukeyHSD(fm5, "PhosphorusCat");fm5Tukey

summary(fm6<-aov(hikaru$AMF.Colonization~MycorrCat))
fm6Tukey = TukeyHSD(fm6, "MycorrCat");fm6Tukey

#

summary(fm7<-aov(diseaseAssessment~PhosphorusCat))
fm7Tukey = TukeyHSD(fm7, "PhosphorusCat");fm7Tukey

summary(fm8<-aov(diseaseAssessment~MycorrCat))
fm8Tukey = TukeyHSD(fm8, "MycorrCat");fm8Tukey


#

#--------------------------------------------------------------------

magnus = filter(largeData, Infected == 1)

PhosphorusCat <- factor(magnus$Phosphorus, levels = c("Low", "Med", "High"))
MycorrCat <- factor(magnus$Mycorrhizae, labels = c("No Mycorrhizae", "Mycorrhizae"))

diseaseAssessment = {} #DO IT


summary(fm9<-aov(magnus$AboveBiomass~PhosphorusCat))
fm9Tukey = TukeyHSD(fm9, "PhosphorusCat");fm9Tukey

summary(fm10<-aov(magnus$AboveBiomass~MycorrCat))
fm10Tukey = TukeyHSD(fm10, "MycorrCat");fm10Tukey

#

summary(fm11<-aov(magnus$BelowBiomass~PhosphorusCat))
fm11Tukey = TukeyHSD(fm11, "PhosphorusCat");fm11Tukey

summary(fm12<-aov(magnus$BelowBiomass~MycorrCat))
fm12Tukey = TukeyHSD(fm12, "MycorrCat");fm12Tukey

#

summary(fm13<-aov(magnus$AMF.Colonization~PhosphorusCat))
fm13Tukey = TukeyHSD(fm13, "PhosphorusCat");fm13Tukey

summary(fm14<-aov(magnus$AMF.Colonization~MycorrCat))
fm14Tukey = TukeyHSD(fm14, "MycorrCat");fm14Tukey

#

summary(fm15<-aov(diseaseAssessment~PhosphorusCat))
fm15Tukey = TukeyHSD(fm15, "PhosphorusCat");fm15Tukey

summary(fm16<-aov(diseaseAssessment~MycorrCat))
fm16Tukey = TukeyHSD(fm16, "MycorrCat");fm16Tukey

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# install.packages("gt")
# install.packages("gtsummary")
library(gt)
library(gtsummary)
library(dplyr)

#-----------------------

partialDI <- largeData[seq(1, nrow(largeData), by = 4), ]

partialDI_DF <- partialDI[, c("Infected", "Mycorrhizae", "Phosphorus", "Partial.Disease.Index")]

partialDI_Table <- gt(partialDI_DF) %>%
  tab_header(
    title = "Disease Index Across Manipulations"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "all",    # Add borders on all sides
      color = "black",  # Border color
      weight = px(2)    # Border thickness
    ),
    locations = cells_body()
  ) %>%  # Properly chain tab_footnote
  tab_footnote(
    footnote = "Footnote space"
  ) %>%
  text_transform(
  locations = cells_body(columns = vars(Value)),
  fn = function(x) {
    # Add '**' if the value is less than 0.05 (for example, to indicate significance)
    ifelse(x < 0.05, paste0(x, " **"), as.character(x))
  }
  )


partialDI_Table




#-----------------------
#***** Averages the 4 replications of each manipulation together and puts it into a new dataframe (every4total)


# Create a grouping variable for every 4 rows
group <- rep(1:ceiling(nrow(largeData) / 4), each = 4)[1:nrow(largeData)]

averaged_largeData <- aggregate(largeData[, c("AboveBiomass", "BelowBiomass", "AMF.Colonization")], 
                                by = list(group), 
                                FUN = mean)

names(averaged_largeData)[1] <- "group"
averaged_largeData$group <- NULL

every4 <- largeData[seq(1, nrow(largeData), by = 4), ]

every4_DF <- every4[, c("Infected", "Mycorrhizae", "Phosphorus")]

every4total <- cbind(every4_DF, averaged_largeData)












