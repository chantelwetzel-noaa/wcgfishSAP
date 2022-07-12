---
title: "Calculate Ecoystem Importance Scores"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


library(tidyr)
library(dplyr)
```
This file documents how the ecoystemp importance scores are calculated from the species list, fishing mortalities, and the balanced Koehn ecopath model. 
A github repository with data and code is available: https://github.com/kristinmarshall-NOAA/eco_import_scores

## Reading in the files

There are two files to read in. species_mort has the species and functional group key, matching up the species list with functional group names from ecopath.  It also has the fishing mortality calculated from the current year. ecopath has the ecopath-derived fields we need: biomass, QB, and the proportion of diet that is managed or protected species, calculated externally for now.

```{r input }
ecopath<-read.csv("../data/Koehn_ecopath_balanced.csv", header=T)

species_mort<-read.csv("../data/2022_species_mort_group.csv", header=T)

```
## Calculate scores for ecopath functional groups

The first step is to calculate the scores for each ecopath functional group.  Top-down scores are derived from the consumption rate (Q/B* B) weighted by the percent of the functional group's diet that is made up of managed or protected species.  These are then converted into proportions of the total consumption of managed and protected species in the model, and re-scaled relative to the maximum value.

Bottom-up scores are calculated as the proportion of total consumer biomass (not including primary producers or detritus).  These are also re-scaled relative to the maximum value.

```{r ecopath}
ecopath_out<-ecopath %>%
  filter(group!='phytoplankton' & group!='Detritus')  %>%
  
  #top-down
  mutate(QB_B_diet = biomass * QB * diet_manpro)  %>%
  mutate(prop_consumption = QB_B_diet / sum(QB_B_diet)) %>%
  mutate(scaled_consumption = prop_consumption / max(prop_consumption) * 10) %>%
  
  #bottom-up
  mutate(prop_consumer_bio = biomass / sum(biomass)) %>%
  mutate(scaled_consumer_bio = prop_consumer_bio / max(prop_consumer_bio) * 10)

```

Some species in the list don't have associated functional groups in the ecopath model. Here I calculate median values from all the groundfish groups in the model that are used down below to fill in NAs.
```{r filler }
  gf_groups<-c("dogfish", "Sablefish", "Arrowtooth flounder","Lingcod","shelf rockfish", "Skates", 
               "Yellowtail rockfish", "Shortspine thornyhead", "Black rockfish",  "Petrale sole", 
               "Widow rockfish" , "Grenadiers", "nearshore rockfish", "Yelloweye rockfish", "flatfish", 
               "Longspine thornyhead",  "Greenstriped rockfish",  "Canary rockfish", "Pacific Ocean Perch",
               "Shortbelly", "Splitnose rockfish", "slope rockfish")           
  gf_meds<-ecopath_out %>%
    filter(group %in% gf_groups) %>%
    select(prop_consumption, scaled_consumption, prop_consumer_bio, scaled_consumer_bio)%>%
    summarise_all(median)

```

## Calculate relative proportion of species within functional groups

The next step is to calculate the proportion that each species in the prioritization list makes up of each ecopath functional group, adult and juvenile (if there is a juvenile group).  These are based on the relative fishing mortality by species within a functional group, and assume fishing mortality is proportional to biomass.

Then, the functional group scores from ecopath are joined to the species and weighted by the proportions just calculated (ad_prop or juv_prop).

```{r scores, include=T}
species_scores_ad<- species_mort %>%
  group_by(group) %>%
  mutate(ad_prop = fishing_mortality / sum(fishing_mortality)) %>%
  ungroup() %>%
  left_join(ecopath_out, by="group") %>%
  rename(ad_biomass = biomass, QB_ad = QB, diet_ad = diet_manpro)  %>%
  mutate(prop_cons_ad = prop_consumption * ad_prop) %>%
  mutate(cons_scaled_ad = scaled_consumption * ad_prop) %>%
  mutate(prop_cons_bio_ad = prop_consumer_bio * ad_prop) %>%
  mutate(scaled_consumer_bio_ad = scaled_consumer_bio * ad_prop) 

species_scores_juv<- species_mort %>%
  select(species, fishing_mortality, juv_group) %>%
  rename(group = juv_group)  %>%
  group_by(group) %>%
  mutate(juv_prop = fishing_mortality / sum(fishing_mortality)) %>%
  ungroup()  %>%
  left_join(ecopath_out, by="group") %>%
  rename(juv_biomass = biomass, QB_juv = QB, diet_juv = diet_manpro)  %>%
  mutate(prop_cons_juv = prop_consumption * juv_prop) %>%
  mutate(cons_scaled_juv = scaled_consumption * juv_prop) %>%
  mutate(prop_cons_bio_juv = prop_consumer_bio * juv_prop) %>%
  mutate(scaled_consumer_bio_juv = scaled_consumer_bio * juv_prop)
```

###Replace missing values

For species that don't correspond well to an ecopath functional group, there is a NA in the functional group field.  These are replaced with median values for the raw scores of all the adult groundfish functional groups in the ecopath model.

```{r missing, include=T}
species_scores_ad$prop_cons_ad[is.na(species_scores_ad$group)] = gf_meds$prop_consumption
species_scores_ad$cons_scaled_ad[is.na(species_scores_ad$group)] = gf_meds$scaled_consumption
species_scores_ad$prop_cons_bio_ad[is.na(species_scores_ad$group)] = gf_meds$prop_consumer_bio
species_scores_ad$scaled_consumer_bio_ad[is.na(species_scores_ad$group)] = gf_meds$scaled_consumer_bio
```
For species that don't have corresponding juvenile functional groups in the ecopath model, the score values are set to zero. All biomass, consumption, and diet for the functional grouped is assumed to be adult. 
```{r missing2, include=T}
species_scores_juv$prop_cons_juv[is.na(species_scores_juv$group)]=0
species_scores_juv$cons_scaled_juv[is.na(species_scores_juv$group)]=0
species_scores_juv$prop_cons_bio_juv[is.na(species_scores_juv$group)]=0
species_scores_juv$scaled_consumer_bio_juv[is.na(species_scores_juv$group)]=0
```

###Combine adult and juvenile components

The final step is to join the adult and juvenile score dataframes and sum them.

```{r combine, include=T}
species_scores_all<- species_scores_juv  %>%  
  select(one_of("species","prop_cons_juv","cons_scaled_juv","prop_cons_bio_juv","scaled_consumer_bio_juv")) %>%
  left_join(species_scores_ad, by="species") %>%
  mutate(prop_consumption_raw = prop_cons_juv + prop_cons_ad)  %>%
  mutate(prop_consumption_scaled = cons_scaled_juv + cons_scaled_ad) %>%
  mutate(prop_consumer_bio_raw = prop_cons_bio_ad + prop_cons_bio_juv) %>%
  mutate(prop_consumer_bio_scaled = scaled_consumer_bio_ad + scaled_consumer_bio_juv)   %>%
  select(species, prop_consumption_raw, prop_consumption_scaled, prop_consumer_bio_raw, 
         prop_consumer_bio_scaled) 

write.csv(species_scores_all,"../output/ecosystem_importance_scores.csv", row.names=F)

```
