#####################################################################
## Non-Metric Multidimensional Scaling Community Analysis of Seasonal
## Mean Biomass 
## 
#####################################################################
rm(list = ls())

install.packages("vegan")
install.packages("factoextra")
install.packages("ecodist")
install.packages("extrafont")
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(rlang)
library(bestNormalize)
library(factoextra)
library(ecodist)
library(extrafont)

#####################################
## Load biomass file
#####################################

seasonal_biomass <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/seasonal_mean_biomass_benth_all_reaches.csv")
seasonal_biomass_site_variables <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/seasonal_biomass_site_variables.csv")

###############################################
## Transform & Standardize biomass data
##############################################

seasonal_biomass <- column_to_rownames(seasonal_biomass, var = "X")

# log transform using decostand
seasonal_biomass_log <- decostand(seasonal_biomass, "log", na.rm = T)

# Standardize by proportion of row totals:
# Divide each value in each row by the row total (total biomass)
# so that each taxa biomass value becomes a proportion of total B
seasonal_biomass_logged_transformed <-
  decostand(seasonal_biomass_log, "total")

################################################
### NMDS analysis
###
################################################

biomass_NMDS <-
  metaMDS(
    seasonal_biomass_logged_transformed,
    distance = "bray",
    k = 2,
    trymax = 500,
    wascores = T,
    
  )

## Create stressplot of results to see how well represented the original
## variables are by the new, reduced variables
stressplot(biomass_NMDS)



###############################################################
## Graph NMDS scores using ggplot2
##
##
###############################################################

##################################################
## find species vectors (intrinsic variables) and p-values
intrinsic_species <-
  envfit(biomass_NMDS,
         seasonal_biomass_logged_transformed,
         permutations = 999)

####################################################
## Create dataframes of site scores to use with ggplot2
## save NMDS results into dataframe
site_scores <-
  as.data.frame(scores(biomass_NMDS, display = "sites"))

# add site names
site_scores <- cbind(site_scores, Site = rownames(site_scores))

#######################################################
## add Site grouping variables for Season and Treatment
site_scores <-
  cbind(site_scores, seasonal_biomass_site_variables)

head(site_scores)

##write.csv(site_scores,"~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/site scores.csv",
##          row.names = F
##)
site_scores <- read.csv("~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/site scores.csv")

##########################################################
## Create dataframes of species scores to use with ggplot2
## Save NMDS results into dataframe

## save species intrinsic values into dataframe
species_scores <- as.data.frame(scores(intrinsic_species, display = "vectors"))

species_scores <- cbind(species_scores, Species = rownames(species_scores)) #add species names to dataframe
species_scores <- cbind(species_scores, pval = intrinsic_species$vectors$pvals) #add pvalues to dataframe so you can select species which are significant

##  subset the variables that most influence the spread
significant_species_scores <- subset(species_scores, pval<=0.001) #subset data to show species significant at 0.05

head(species_scores)

write.csv(significant_species_scores,"~/S.Fk.McKenzie_FoodWeb_Study/DataDerived/intrinsic_species_scores_NMDS.csv",
          row.names = T
)

#######################################################
## Create plot with ggplot2
##

#Get new fonts
install.packages("extrafont")
library(extrafont)

font_import()
y

loadfonts(device="win")  #Register fonts for Windows bitmap output

fonts()  #See list of available fonts


#set up the plot
NMDS_plot_seasonal <- ggplot(site_scores, aes(x=NMDS1, y=NMDS2))+
  labs(x = "Axis 1")+
  labs(y = "Axis 2")+
  geom_point(aes(NMDS1, NMDS2, colour = factor(site_scores$Treatment), shape = factor(site_scores$Season)), size = 5)+ #adds site points to plot, shape determined by Treatment, colour determined by Season
  scale_color_manual(values=c("#F5793A", "#85C0F9"))+
  scale_shape_manual(values = c(15,16,17,18))+
  geom_label(x = -0.35, y = 0.4, label = "Stress = 0.131", label.size = 0.4)+ # add stress value
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Treatment", shape = "Season")+ # add legend labels for Treatment and Season
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 12))+ # add legend at right of plot
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank())+
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())+
  theme(text=element_text(family="Times New Roman", face="bold", size=12))+
  ggrepel::geom_text_repel(
    data = site_scores,
    aes(x = NMDS1, y = NMDS2, label = Site),
    cex = 4,
    direction = "both",
    segment.size = 0.25
  )
NMDS_plot_seasonal
## Write the plot to file
ggsave("~/S.Fk.McKenzie_FoodWeb_Study/Figures/bp35.jpg", NMDS_plot_seasonal, width=35, height=20, 
       device = "jpeg", units = "cm")



############################################################
##   Conduct Analysis of Similarity (ANOSIM) on the monthly 
##   biomass data using ANOSIM in the Vegan package
##
##
############################################################

# create a data frame with biomass data and factor columns "Treatment" and "Season"
biomass_factors <- cbind(seasonal_biomass_logged_transformed,seasonal_biomass_site_variables)


anosim1 <- anosim(seasonal_biomass_logged_transformed, biomass_factors$Treatment, distance = "bray", permutations = 9999)

anosim1

anosim2 <- anosim(seasonal_biomass_logged_transformed, biomass_factors$Season, distance = "bray", permutations = 9999)

anosim2


##################################
# Scrap Code
#######################

## add species to the plot
ordilabel(
  biomass_NMDS,
  display = "species",
  font = 2,
  priority = priSpp,
  scaling = scl
)

## side-by-side biplots of sites and species separately

layout(matrix(1:2, ncol = 2))
plot(biomass_NMDS, type = "n", scaling = scl)
ordilabel(
  biomass_NMDS,
  display = "sites",
  font = 3,
  fill = "hotpink",
  col = "blue",
  priority = priSite,
  scaling = scl
)
plot(biomass_NMDS, type = "n", scaling = scl)
ordilabel(
  biomass_NMDS,
  display = "species",
  font = 2,
  priority = priSpp,
  scaling = scl
)
layout(1)

orditorp(biomass_NMDS,
         display = "species",
         col = "red",
         air = 0.01)
orditorp(biomass_NMDS,
         display = "sites",
         cex = 1.25,
         air = 0.5)



## biplot results
plot(biomass_NMDS, type = "t")
