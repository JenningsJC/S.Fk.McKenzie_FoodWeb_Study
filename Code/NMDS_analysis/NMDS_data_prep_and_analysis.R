#####################################################################
## Non-Metric Multidimensional Scaling Community Analysis of Seasonal
## Mean Biomass 
## 
#####################################################################
rm(list = ls())

install.packages("vegan")
install.packages("factoextra")
install.packages("ecodist")
library(vegan)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(rlang)
library(bestNormalize)
library(factoextra)
library(ecodist)

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

###################################
## Graph biplot of reaches
###################################

## create a blank plot

ordiplot(
  biomass_NMDS,
  type = "n",
  xlim = c(-0.6, 0.6),
  ylim = c(-0.6, 0.6),
  main = "Reaches"
)


## add sites/reaches to the plot
orditorp(biomass_NMDS,
         display = "sites",
         cex = 1,
         air = 0.1)


##########################################################
## Graph biplot of intrinsic species using vegan package
## intrinsic = drives the pattern of
## distibution of the reaches
##########################################################


## find vectors of Intrinsic Variables and p-values
intrinsics <-
  envfit(biomass_NMDS,
         seasonal_biomass_logged_transformed,
         permutations = 999)

## create a blank plot

ordiplot(biomass_NMDS, type = "n", main = "Intrinsic Taxa p<0.001")

## plot the sites as points
#orditorp(biomass_NMDS, display = "sites", labels = F, cex = 2)
orditorp(biomass_NMDS,
         display = "sites",
         cex = 1,
         air = 0.07)

## then the intrinsic variables by selecting desired p-value


plot(intrinsics,
     p.max = 0.001,
     col = "black",
     cex = 0.6) # change the significance level of species shown with p.max



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

##########################################################
## Create dataframes of species scores to use with ggplot2
## Save NMDS results into dataframe

## save species intrinsic values into dataframe
species_scores <- as.data.frame(scores(intrinsic_species, display = "vectors"))

species_scores <- cbind(species_scores, Species = rownames(species_scores)) #add species names to dataframe
species_scores <- cbind(species_scores, pval = intrinsic_species$vectors$pvals) #add pvalues to dataframe so you can select species which are significant

##  subset the variables that most influence the spread
significant_species_scores <- subset(species_scores, pval<=0.01) #subset data to show species significant at 0.05

head(species_scores)

#######################################################
## Create ggplot plot
##

#set up the plot
NMDS_plot_seasonal <- ggplot(site_scores, aes(x=NMDS1, y=NMDS2))+ 
  geom_point(aes(NMDS1, NMDS2, colour = factor(site_scores$Season), shape = factor(site_scores$Treatment)), size = 2)+ #adds site points to plot, shape determined by Treatment, colour determined by Season
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Season", shape = "Treatment")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

NMDS_plot_seasonal +
  labs(subtitle = "stress = 0.131") +
  ggrepel::geom_text_repel(
    data = site_scores,
    aes(x = NMDS1, y = NMDS2, label = Site),
    cex = 3,
    direction = "both",
    segment.size = 0.25
  )+
  geom_segment(
    data = significant_species_scores,
    aes(
      x = 0,
      xend = NMDS1,
      y = 0,
      yend = NMDS2
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    colour = "grey10",
    lwd = 0.3
  ) + #add vector arrows of significant species
  ggrepel::geom_text_repel(
    data = significant_species_scores,
    aes(x = NMDS1, y = NMDS2, label = Species),
    cex = 3,
    direction = "both",
    segment.size = 0.25
  )





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
