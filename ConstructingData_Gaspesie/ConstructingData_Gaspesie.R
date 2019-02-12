### Getting Gaspesie data in shape for analysis ###

# Notes -------------------------------------------------------------------

# 3 study territories: Matane, ZEC Casault, Chic-choc
# on these, we have multiple study sites:
# Matane: 6 (plus 4 with treatment)
# ZEC Casault: 5
# Chic-choc: 4 (plus one outlier that is excluded)
# altogether, this is 15 usable study sites
# each study site = 1 exclosure + 1 open plot

# at the level of all data from the exclosures,
# there were some NA years on some variables
# but heights were measured each year, so should have no NAs there,
# and thus zero regenerations are real

# Laurent's original data lacked Picea observations from 2011,
# and it turned out that we only know the height class for Picea of that year;
# probably exact height were measured but lost in data puching & processing phase...

# as the data format was different, missing Piceas were added manually to
# the datasheet before importing to R, these individuals can me recognized
# based on new column "missingpicnote"

# browsing data is missing at T5 for sites built in 2010;
# this is correct

# Preparations ------------------------------------------------------------

library(reshape2, lib="C:/RpackagesQuebec")
library(data.table, lib="C:/RpackagesQuebec")
library(crayon, lib="C:/RpackagesQuebec")
library(utf8, lib="C:/RpackagesQuebec")
library(cli, lib="C:/RpackagesQuebec")
library(dplyr, lib="C:/RpackagesQuebec")


rm(list=ls())
setwd("H:/Trondheim/BOREAL SUCCESSION/ConstructingData_Gaspesie")
getwd()

source(file = "HighstatLibV10.R") 

# Read data
Gasp_raw<-read.csv(file = "Gaspesie_raw.csv",
               header = TRUE,
               dec = ".")
str(Gasp_raw)

Gasp_raw$site_Treatment <- do.call(paste, c(Gasp_raw[c("site", "EX")], sep = "_"))
Gasp_raw$year_site <- do.call(paste, c(Gasp_raw[c("sample_year", "site")], sep = "_"))

# add precise coordinate information
coordprecise<-read.csv(file = "coordprecise.csv",
                       header = TRUE,
                       dec = ".")
str(coordprecise)
Gasp_raw<-merge(Gasp_raw, coordprecise, by="site_Treatment")

# bring in climate and coordinate data

Gasp_clim<-read.csv(file = "climate.csv",
                   header = TRUE,
                   dec = ".")
str(Gasp_clim)
Gasp_clim$mean.temp<-((Gasp_clim$Temp.rature.maximale+Gasp_clim$Temp.rature.minimale)/2)


sitecoord<-read.csv(file = "sitecoord.csv",
                    header = TRUE,
                    dec = ".")
str(sitecoord)

# combine these two by LON_LAT.short
Gasp_clim<-merge(Gasp_clim, sitecoord, by="LON_LAT.short")
str(Gasp_clim)

# Calculate yearly climate variables:

# We want to know 
# 1) growth period temprature after measuring i.e. June-September for a year before measurement,
# 2) growth period rain for the same period,
# 3) winter temperature for October-March before measuring, and
# 4) winter rain for the same period
# 5) yearly temperature
# 6) yearly rain

# => be careful which years to use!

# Growth period average first
Gasp_climSUMMER<-Gasp_clim[Gasp_clim$month==6|Gasp_clim$month==7|Gasp_clim$month==8|Gasp_clim$month==9,]
summ.temp<-tapply(Gasp_climSUMMER$mean.temp,list(Gasp_climSUMMER$year, Gasp_climSUMMER$site),mean)
summ.temp<-data.frame(summ.temp)
# Then winter months: use only correct months and change spring months
# to be under the last years value:
Gasp_climWINTER<-Gasp_clim[Gasp_clim$month==10|Gasp_clim$month==11|Gasp_clim$month==12|Gasp_clim$month==1|Gasp_clim$month==2|Gasp_clim$month==3,]
Gasp_climWINTERsping<-Gasp_climWINTER[Gasp_climWINTER$month==1|Gasp_climWINTER$month==2|Gasp_climWINTER$month==3,]
Gasp_climWINTERautumn<-Gasp_climWINTER[Gasp_climWINTER$month==10|Gasp_climWINTER$month==11|Gasp_climWINTER$month==12,]
Gasp_climWINTERsping$year<-Gasp_climWINTERsping$year-1
Gasp_climWINTER<-rbind(Gasp_climWINTERautumn, Gasp_climWINTERsping)
wint.temp<-tapply(Gasp_climWINTER$mean.temp,list(Gasp_climWINTER$year, Gasp_climWINTER$site),mean)
wint.temp<-data.frame(wint.temp)
# Yearly averages
Gasp_climMONTHSUNTILNEXT<-Gasp_clim[Gasp_clim$month==6|Gasp_clim$month==7|Gasp_clim$month==8|Gasp_clim$month==9|Gasp_clim$month==10|Gasp_clim$month==11|Gasp_clim$month==12,]
Gasp_climMONTHSAFTERNEXT<-Gasp_clim[Gasp_clim$month==1|Gasp_clim$month==2|Gasp_clim$month==3|Gasp_clim$month==4|Gasp_clim$month==5,]
Gasp_climMONTHSAFTERNEXT$year<-Gasp_climMONTHSAFTERNEXT$year-1
Gasp_climYEARS<-rbind(Gasp_climMONTHSAFTERNEXT, Gasp_climMONTHSUNTILNEXT)
mean.temp<-tapply(Gasp_climYEARS$mean.temp,list(Gasp_climYEARS$year, Gasp_climYEARS$site),mean)
mean.temp<-data.frame(mean.temp)
# and rain yearly
rain.year<-tapply(Gasp_climYEARS$Pr.cipitations.totales,list(Gasp_climYEARS$year,Gasp_climYEARS$site),mean)
rain.year<-data.frame(rain.year)
# and rain for growth period
rain.GP<-tapply(Gasp_climSUMMER$Pr.cipitations.totales,list(Gasp_climSUMMER$year, Gasp_climSUMMER$site),mean)
rain.GP<-data.frame(rain.GP)
# and rain for winter
rain.wint<-tapply(Gasp_climWINTER$Pr.cipitations.totales,list(Gasp_climWINTER$year, Gasp_climWINTER$site),mean)
rain.wint<-data.frame(rain.wint)

# year 2008 does not make sense for these:
wint.temp = wint.temp[-1,]
rain.wint = rain.wint[-1,]
rain.year = rain.year[-1,]
mean.temp = mean.temp[-1,]

# melt to combine

sample_year <- rownames(summ.temp)
rownames(summ.temp) <- NULL
summ.temp <- cbind(sample_year,summ.temp)
summ.temp<-melt(summ.temp, id.vars = "sample_year")
colnames(summ.temp)[colnames(summ.temp)=="variable"] <- "site"
colnames(summ.temp)[colnames(summ.temp)=="value"] <- "summ.temp"

sample_year <- rownames(wint.temp)
rownames(wint.temp) <- NULL
wint.temp <- cbind(sample_year,wint.temp)
wint.temp<-melt(wint.temp, id.vars = "sample_year")
colnames(wint.temp)[colnames(wint.temp)=="variable"] <- "site"
colnames(wint.temp)[colnames(wint.temp)=="value"] <- "wint.temp"

sample_year <- rownames(mean.temp)
rownames(mean.temp) <- NULL
mean.temp <- cbind(sample_year,mean.temp)
mean.temp<-melt(mean.temp, id.vars = "sample_year")
colnames(mean.temp)[colnames(mean.temp)=="variable"] <- "site"
colnames(mean.temp)[colnames(mean.temp)=="value"] <- "mean.temp"

sample_year <- rownames(rain.GP)
rownames(rain.GP) <- NULL
rain.GP <- cbind(sample_year,rain.GP)
rain.GP<-melt(rain.GP, id.vars = "sample_year")
colnames(rain.GP)[colnames(rain.GP)=="variable"] <- "site"
colnames(rain.GP)[colnames(rain.GP)=="value"] <- "rain.GP"

sample_year <- rownames(rain.year)
rownames(rain.year) <- NULL
rain.year <- cbind(sample_year,rain.year)
rain.year<-melt(rain.year, id.vars = "sample_year")
colnames(rain.year)[colnames(rain.year)=="variable"] <- "site"
colnames(rain.year)[colnames(rain.year)=="value"] <- "rain.year"

sample_year <- rownames(rain.wint)
rownames(rain.wint) <- NULL
rain.wint <- cbind(sample_year,rain.wint)
rain.wint<-melt(rain.wint, id.vars = "sample_year")
colnames(rain.wint)[colnames(rain.wint)=="variable"] <- "site"
colnames(rain.wint)[colnames(rain.wint)=="value"] <- "rain.wint"

mean.temp$year_site <- do.call(paste, c(mean.temp[c("sample_year", "site")], sep = "_"))
summ.temp$year_site <- do.call(paste, c(summ.temp[c("sample_year", "site")], sep = "_"))
wint.temp$year_site <- do.call(paste, c(wint.temp[c("sample_year", "site")], sep = "_"))
rain.GP$year_site <- do.call(paste, c(rain.GP[c("sample_year", "site")], sep = "_"))
rain.year$year_site <- do.call(paste, c(rain.year[c("sample_year", "site")], sep = "_"))
rain.wint$year_site <- do.call(paste, c(rain.wint[c("sample_year", "site")], sep = "_"))

mean.temp$sample_year<-NULL
mean.temp$site<-NULL
summ.temp$sample_year<-NULL
summ.temp$site<-NULL
wint.temp$sample_year<-NULL
wint.temp$site<-NULL
rain.GP$sample_year<-NULL
rain.GP$site<-NULL
rain.year$sample_year<-NULL
rain.year$site<-NULL
rain.wint$sample_year<-NULL
rain.wint$site<-NULL

# and combine these all
Gasp_climyearlyfinal<-merge(mean.temp, summ.temp, by="year_site")
Gasp_climyearlyfinal<-merge(Gasp_climyearlyfinal, wint.temp, by="year_site")
Gasp_climyearlyfinal<-merge(Gasp_climyearlyfinal, rain.GP, by="year_site")
Gasp_climyearlyfinal<-merge(Gasp_climyearlyfinal, rain.year, by="year_site")
Gasp_climyearlyfinal<-merge(Gasp_climyearlyfinal, rain.wint, by="year_site")

write.table(Gasp_climyearlyfinal, "H:/Trondheim/BOREAL SUCCESSION/ConstructingData_Gaspesie/Gasp_climyearlyfinal.txt", sep="\t") 

str(Gasp_climyearlyfinal)

# combine with tree data:
Gasp_raw<-merge(Gasp_climyearlyfinal, Gasp_raw, by="year_site")
str(Gasp_raw)

Gasp_raw$Browsing_percent<-as.numeric(as.character(Gasp_raw$Browsing_percent))

# And finally, bring in NPP and moose density data

NPP_Moose<-read.csv(file = "FINAL_Gaspesie_Productivity&MooseDensity_forsites.csv",
                   header = TRUE,
                   dec = ".")
str(NPP_Moose)
# View(NPP_Moose)
# View(Gasp_raw)
NPP_Moose<-data.frame(NPP_Moose$Site, NPP_Moose$NPP_July2016, NPP_Moose$NPP_June2016, NPP_Moose$EffectiveMooseDensitykm2)
colnames(NPP_Moose)[colnames(NPP_Moose)=="NPP_Moose.Site"] <- "site"
colnames(NPP_Moose)[colnames(NPP_Moose)=="NPP_Moose.NPP_July2016"] <- "NPP_July2016"
colnames(NPP_Moose)[colnames(NPP_Moose)=="NPP_Moose.NPP_June2016"] <- "NPP_June2016"
colnames(NPP_Moose)[colnames(NPP_Moose)=="NPP_Moose.EffectiveMooseDensitykm2"] <- "EffectiveMooseDensitykm2"

Gasp_raw<-merge(NPP_Moose, Gasp_raw, by="site")

# FAI  --------------------------------------------------------------------

# We want to create "food avaiablity index" for each plot;
# This should depict the number of edible forage.
# So we use the number of individuals weighted by moose preference.

unique(Gasp_raw$species_cor)

# calculate the number of observations per species in plots
indiv_numbers<-count(Gasp_raw, species_cor, plot_year)
# View(indiv_numbers)

# are there numbers realistic? Ask Laurent!

# Anyway, we want a weighted number based on moose preference
indiv_numbers$weightedn<-NA
unique(indiv_numbers$species_cor)

# to get an objective value of moose reference, we could calculate
# the proportion on browsed individuals for each species in the whole data.

Gasp_raw$browsingfactorial[Gasp_raw$Browsing_percent>0]<-"yes"
Gasp_raw$browsingfactorial[Gasp_raw$Browsing_percent==0]<-"no"

speciesbrowsing<-count(Gasp_raw, species_cor, browsingfactorial)
# View(speciesbrowsing)
speciesbrowsing <- dcast(speciesbrowsing,                           # Specifies data
                     value.var = "n",         # Name of the column whose values will be filled to cast
                     species_cor~browsingfactorial,             # LHS ~ RHS formula
                     fun.aggregate = sum)
speciesbrowsing$percentageofbrowsed<-speciesbrowsing$yes/(speciesbrowsing$no+speciesbrowsing$yes)
# this looks good, excet that rare pecies give a bit missleadin values...
# these won't affect the big picture, so we can as well leave them in.

speciesweights<-merge(speciesbrowsing, indiv_numbers, by="species_cor")
# View(speciesweights)

# Now if we simply multiply n with percentageofbrowsed, we get weighted n which
# is the bigger the bigger is the preference
speciesweights$weightedn<-speciesweights$percentageofbrowsed*speciesweights$n

# and finally, we want to calculate the sum of this for each plot for each year:

FAI<-aggregate(speciesweights$weightedn, by=list(speciesweights$plot_year), FUN=mean)
colnames(FAI)[colnames(FAI)=="Group.1"] <- "plot_year"
colnames(FAI)[colnames(FAI)=="x"] <- "FAI"

Gasp_raw<-merge(FAI, Gasp_raw, by="plot_year")


# Data exploration & modification --------------------------------------------------------

# So, what do we have:

# Site, plot: need to nest within these
# Territory: might be interesting, but nested structurew would get too complicated
# EX: exclosure or open, factorial presence-absence of moose
# build year: we might need this when calculating yearly growths
# sample year & time: repetitive
# date, day, month: this is actually very tricky: varies from May to August!!!
table(Gasp_raw$month) 
# ok good, mostly done early before growth starts
sum(is.na(Gasp_raw$month))
# but quite a few NAs as well...
# Lauren't answer: June and August values are from the start years: so first time
# trees were measure in late summer.
# For all the following years, browsing and regeneration data were collected 
# between the last week of May and the second week of June.
# nom_lat: need to decide which species to include
# height: response variable
# diam: this is full of NAs, just measured for couple of years, don't use
# ... and rest is browsing data.

table(Gasp_raw$site, Gasp_raw$sample_year)

#      2010 2011 2012 2013 2014 2015 2016
# CA1001  142  431  342  357  367  348  375 
# CA1002   97  156  345  457  491  958 1326
# CA1003   71  119   96  115  137  281  301
# CA1104    0  161  139  229  380  519  539
# CA1105    0  145  166  297  382  398  472 => Casault sites CA1104 and CA1105, no observations 2010: zer growth or not set up yet?
# CH1001    0   34    0    0    0    0    0
# CH1002  141  362  294  284  448  441  557
# CH1003   65  174   92   79  181  241  309
# CH1104    0  275  324  271  599  828  941
# CH1105    0  340  180  180  297  306  411 => Chic-choc sites: is the first one the outlier that should be excluded?
# MA1001   98  185  184  185  257  268  297
# MA1002   99   45  259  454  757  990 1235
# MA1003  142  305  338  387  546  476  526
# MA1104    0  117  309  523  655  797  646
# MA1105    0  101   62   53   69  315  291
# MA1106    0  111   66  155  276  479  540 => Matane sites MA1104, MA1105 and Ma1106, no observations 2010: zer growth or not set up yet?

# Sites with code 10 were set up 2010, with 11 2011, so zeros in above tables are NAs.
# This type of table should be produced for each species to see when
# there are real zeros and when the gaos are due to later setup of fences.

# remove species with very few observations
# Gasp_raw<-Gasp_raw[!(Gasp_raw$nom_lat=="Acer rubrum" 
#                      | Gasp_raw$nom_lat=="Alnus incana subsp. rugosa"
#                      | Gasp_raw$nom_lat=="Amelanchier canadensis"
#                     | Gasp_raw$nom_lat=="Cornus stolonifera"
#                     | Gasp_raw$nom_lat=="Corylus cornuta"
#                     | Gasp_raw$nom_lat=="Lonicera canadensis"
#                     | Gasp_raw$nom_lat=="Populus balsamifera"
#                     | Gasp_raw$nom_lat=="Populus tremuloides"
#                     | Gasp_raw$nom_lat=="Salix sp."
#                     | Gasp_raw$nom_lat=="Sambucus racemosa subsp. pubens var. pubens"
#                     ),]
# unique(Gasp_raw$nom_lat)

# yes, Lauren't says CH1001 was left out:
Gasp_raw<-Gasp_raw[!(Gasp_raw$site=="CH1001"),]

# ...and because the comparison between the two first measurement year is not
# comparable to later, we need to include some NAs:
Gasp_raw$height[Gasp_raw$build_year==2010 & Gasp_raw$sample_year==2010]<-NA
Gasp_raw$height[Gasp_raw$build_year==2011 & Gasp_raw$sample_year==2011]<-NA

# We are intrested  trees that are still at browsing height.
# Study desing is such that above 300 cm, trees only get value of 300 cm or 500 cm.
# How to deal with this???
# Growth data stops to be accurate enough when trees get above 300, and we are also 
# not  interested in this growth. However, if we just exclude measurements
# above 300 cm it would make this look like average growh would suddenly decease
# (e.g. if there are three trees, two small and one big, and big grew above 300 cm.
# we end up with a year where growth collapses just because biggest tree changes to NA...).

# Only way to deal with this: if in a plot there appears even one height values that is >299,
# change all height values in that plot to NAs.

plot(Gasp_raw$sample_year, Gasp_raw$height) #  well this probably deletes a good amount of data...
# well if I would do this separately for each species not necessarily so big loss!!!

# The four species of interest
Abibal<-Gasp_raw[(Gasp_raw$nom_lat=="Abies balsamea"),]
Bet<-Gasp_raw[(Gasp_raw$nom_lat=="Betula sp."),]
Pic<-Gasp_raw[(Gasp_raw$nom_lat=="Picea sp."),]
Sorame<-Gasp_raw[(Gasp_raw$nom_lat=="Sorbus americana"),]

plot(Abibal$sample_year, Abibal$height)
# ...or maybe it is, cannot help this though...

# change all growth values to NAs in plots in years where there is 
# even one individual that is >299cm

# recognize plots&years with tall trees:
talltreesAbibal <- subset(Abibal, height>299)
talltreesAbibalstring<-talltreesAbibal$plot_year
talltreesAbibalstring<-unique(talltreesAbibalstring)

# change growth values to NAs on these:
Abibal[Abibal$plot_year %in% talltreesAbibalstring,]<-NA
plot(Abibal$sample_year, Abibal$height)

# recognize plots&years with tall trees:
talltreesSorame <- subset(Sorame, height>299)
talltreesSoramestring<-talltreesSorame$plot_year
talltreesSoramestring<-unique(talltreesSoramestring)

# change growth values to NAs on these:
Sorame[Sorame$plot_year %in% talltreesSoramestring,]<-NA
plot(Sorame$sample_year, Sorame$height)

# recognize plots&years with tall trees:
talltreesBet <- subset(Bet, height>299)
talltreesBetstring<-talltreesBet$plot_year
talltreesBetstring<-unique(talltreesBetstring)

# change growth values to NAs on these:
Bet[Bet$plot_year %in% talltreesBetstring,]<-NA
plot(Bet$sample_year, Bet$height)

# recognize plots&years with tall trees:
talltreesPic <- subset(Pic, height>299)
talltreesPicstring<-talltreesPic$plot_year
talltreesPicstring<-unique(talltreesPicstring)

# change growth values to NAs on these:
Pic[Pic$plot_year %in% talltreesPicstring,]<-NA
plot(Pic$sample_year, Pic$height)

# Calculate average height for each plot for each year for each sp --------
# * Pic ------------------------------------------------------------------

PicH<-tapply(Pic$height,list(Pic$plot, Pic$sample_year),mean)

PicH<-as.data.frame(PicH)
colnames(PicH)[colnames(PicH)=="2010"] <- "x2010x"
colnames(PicH)[colnames(PicH)=="2011"] <- "x2011x"
colnames(PicH)[colnames(PicH)=="2012"] <- "x2012x"
colnames(PicH)[colnames(PicH)=="2013"] <- "x2013x"
colnames(PicH)[colnames(PicH)=="2014"] <- "x2014x"
colnames(PicH)[colnames(PicH)=="2015"] <- "x2015x"
colnames(PicH)[colnames(PicH)=="2016"] <- "x2016x"

# Calculate height growth each plot for each year
PicH$x10x<-PicH$x2011x-PicH$x2010x
PicH$x11x<-PicH$x2012x-PicH$x2011x
PicH$x12x<-PicH$x2013x-PicH$x2012x
PicH$x13x<-PicH$x2014x-PicH$x2013x
PicH$x14x<-PicH$x2015x-PicH$x2014x
PicH$x15x<-PicH$x2016x-PicH$x2015x
PicH<-PicH[, 8:13]
colnames(PicH)[colnames(PicH)=="x10x"] <- "2010"
colnames(PicH)[colnames(PicH)=="x11x"] <- "2011"
colnames(PicH)[colnames(PicH)=="x12x"] <- "2012"
colnames(PicH)[colnames(PicH)=="x13x"] <- "2013"
colnames(PicH)[colnames(PicH)=="x14x"] <- "2014"
colnames(PicH)[colnames(PicH)=="x15x"] <- "2015"
colnames(PicH)[colnames(PicH)=="x16x"] <- "2016"
names <- rownames(PicH)
rownames(PicH) <- NULL
PicH <- cbind(names,PicH)
colnames(PicH)[colnames(PicH)=="names"] <- "plot"
PicH<-melt(PicH)
colnames(PicH)[colnames(PicH)=="variable"] <- "Year"
colnames(PicH)[colnames(PicH)=="value"] <- "growth"

# View(PicH)

# Now we need to connect this back to original data,
#  so we need a column with plot_year
PicH$plot_year <- do.call(paste, c(PicH[c("plot", "Year")], sep = "_"))
PicH$plot<-NULL
PicH$Year<-NULL

PicG <- merge(PicH,Pic,by="plot_year")

# we also want to know the average height of each plot:
PicH<-tapply(Pic$height,list(Pic$plot_year),mean)
names <- rownames(PicH)
rownames(PicH) <- NULL
PicH <- cbind(names,PicH)
colnames(PicH)[colnames(PicH)=="names"] <- "plot_year"
colnames(PicH)[colnames(PicH)=="PicH"] <- "height_plot"

PicG <- merge(PicH,PicG,by="plot_year")

# ...and same for browsing variables:
PicB<-tapply(Pic$no_browsing,list(Pic$plot_year),mean)
names <- rownames(PicB)
rownames(PicH) <- NULL
PicB <- cbind(names,PicB)
colnames(PicB)[colnames(PicB)=="names"] <- "plot_year"
colnames(PicB)[colnames(PicB)=="PicB"] <- "no_browsing_plot"
PicG <- merge(PicB,PicG,by="plot_year")

PicB<-tapply(Pic$young_browsing,list(Pic$plot_year),mean)
names <- rownames(PicB)
rownames(PicH) <- NULL
PicB <- cbind(names,PicB)
colnames(PicB)[colnames(PicB)=="names"] <- "plot_year"
colnames(PicB)[colnames(PicB)=="PicB"] <- "young_browsing_plot"
PicG <- merge(PicB,PicG,by="plot_year")

PicB<-tapply(Pic$old_browsing,list(Pic$plot_year),mean)
names <- rownames(PicB)
rownames(PicH) <- NULL
PicB <- cbind(names,PicB)
colnames(PicB)[colnames(PicB)=="names"] <- "plot_year"
colnames(PicB)[colnames(PicB)=="PicB"] <- "old_browsing_plot"
PicG <- merge(PicB,PicG,by="plot_year")

PicB<-tapply(Pic$Browsing_percent,list(Pic$plot_year),mean)
names <- rownames(PicB)
rownames(PicH) <- NULL
PicB <- cbind(names,PicB)
colnames(PicB)[colnames(PicB)=="names"] <- "plot_year"
colnames(PicB)[colnames(PicB)=="PicB"] <- "Browsing_percent_plot"
PicG <- merge(PicB,PicG,by="plot_year")

PicG$Browsing_percent_plot<-as.numeric(as.character(PicG$Browsing_percent_plot))


# View(PicG)

# now we have unnecessary dublicates in the data frame as all idividuals are presented as a
# row; individual level information can now be removed which makes working on this easier:

PicG <- PicG[!duplicated(PicG[c("plot_year")]),]
# NOTE THAT AFTER THIS STEP ALL TREE LEVEL INORMATION MEANS NOTHING;
# so all variables that you want to use later in plot levelanalysis need to be 
# calculated (averaged) before this stage if you want to use them!!!

PicG$height_plot<-as.numeric(as.character(PicG$height_plot))


# Now all plots that have growth NA in year after buiding fences, it's not really an NA but zero;
# Replace these NAs
PicG$growth[is.na(PicG$growth) & PicG$build_year==2010 & PicG$sample_year>2010]<-0
PicG$growth[is.na(PicG$growth) & PicG$build_year==2011 & PicG$sample_year>2011]<-0
# and same for height NAs:
PicG$height_plot[is.na(PicG$height_plot) & PicG$build_year==2010 & PicG$sample_year>2010]<-0
PicG$height_plot[is.na(PicG$height_plot) & PicG$build_year==2011 & PicG$sample_year>2011]<-0

# Check main patterns:
plot(PicG$growth,PicG$EX)
plot(PicG$growth)

# Discussed with Laurent: might be some errors (some individuals in in some year and
# out in other years), but values up to 100 cm aren't impossible, so he recommends to have all
# in.

# * Bet ------------------------------------------------------------------

BetH<-tapply(Bet$height,list(Bet$plot, Bet$sample_year),mean)

BetH<-as.data.frame(BetH)
colnames(BetH)[colnames(BetH)=="2010"] <- "x2010x"
colnames(BetH)[colnames(BetH)=="2011"] <- "x2011x"
colnames(BetH)[colnames(BetH)=="2012"] <- "x2012x"
colnames(BetH)[colnames(BetH)=="2013"] <- "x2013x"
colnames(BetH)[colnames(BetH)=="2014"] <- "x2014x"
colnames(BetH)[colnames(BetH)=="2015"] <- "x2015x"
colnames(BetH)[colnames(BetH)=="2016"] <- "x2016x"

# Calculate height growth each plot for each year
BetH$x10x<-BetH$x2011x-BetH$x2010x
BetH$x11x<-BetH$x2012x-BetH$x2011x
BetH$x12x<-BetH$x2013x-BetH$x2012x
BetH$x13x<-BetH$x2014x-BetH$x2013x
BetH$x14x<-BetH$x2015x-BetH$x2014x
BetH$x15x<-BetH$x2016x-BetH$x2015x
BetH<-BetH[, 8:13]
colnames(BetH)[colnames(BetH)=="x10x"] <- "2010"
colnames(BetH)[colnames(BetH)=="x11x"] <- "2011"
colnames(BetH)[colnames(BetH)=="x12x"] <- "2012"
colnames(BetH)[colnames(BetH)=="x13x"] <- "2013"
colnames(BetH)[colnames(BetH)=="x14x"] <- "2014"
colnames(BetH)[colnames(BetH)=="x15x"] <- "2015"
colnames(BetH)[colnames(BetH)=="x16x"] <- "2016"
names <- rownames(BetH)
rownames(BetH) <- NULL
BetH <- cbind(names,BetH)
colnames(BetH)[colnames(BetH)=="names"] <- "plot"
BetH<-melt(BetH)
colnames(BetH)[colnames(BetH)=="variable"] <- "Year"
colnames(BetH)[colnames(BetH)=="value"] <- "growth"

# View(BetH)

# Now we need to connect this back to original data,
#  so we need a column with plot_year
BetH$plot_year <- do.call(paste, c(BetH[c("plot", "Year")], sep = "_"))
BetH$plot<-NULL
BetH$Year<-NULL

BetG <- merge(BetH,Bet,by="plot_year")

# we also want to know the average height of each plot:
BetH<-tapply(Bet$height,list(Bet$plot_year),mean)
names <- rownames(BetH)
rownames(BetH) <- NULL
BetH <- cbind(names,BetH)
colnames(BetH)[colnames(BetH)=="names"] <- "plot_year"
colnames(BetH)[colnames(BetH)=="BetH"] <- "height_plot"

BetG <- merge(BetH,BetG,by="plot_year")

# ...and same for browsing variables:
BetB<-tapply(Bet$no_browsing,list(Bet$plot_year),mean)
names <- rownames(BetB)
rownames(BetH) <- NULL
BetB <- cbind(names,BetB)
colnames(BetB)[colnames(BetB)=="names"] <- "plot_year"
colnames(BetB)[colnames(BetB)=="BetB"] <- "no_browsing_plot"
BetG <- merge(BetB,BetG,by="plot_year")

BetB<-tapply(Bet$young_browsing,list(Bet$plot_year),mean)
names <- rownames(BetB)
rownames(BetH) <- NULL
BetB <- cbind(names,BetB)
colnames(BetB)[colnames(BetB)=="names"] <- "plot_year"
colnames(BetB)[colnames(BetB)=="BetB"] <- "young_browsing_plot"
BetG <- merge(BetB,BetG,by="plot_year")

BetB<-tapply(Bet$old_browsing,list(Bet$plot_year),mean)
names <- rownames(BetB)
rownames(BetH) <- NULL
BetB <- cbind(names,BetB)
colnames(BetB)[colnames(BetB)=="names"] <- "plot_year"
colnames(BetB)[colnames(BetB)=="BetB"] <- "old_browsing_plot"
BetG <- merge(BetB,BetG,by="plot_year")

BetB<-tapply(Bet$Browsing_percent,list(Bet$plot_year),mean)
names <- rownames(BetB)
rownames(BetH) <- NULL
BetB <- cbind(names,BetB)
colnames(BetB)[colnames(BetB)=="names"] <- "plot_year"
colnames(BetB)[colnames(BetB)=="BetB"] <- "Browsing_percent_plot"
BetG <- merge(BetB,BetG,by="plot_year")

BetG$Browsing_percent_plot<-as.numeric(as.character(BetG$Browsing_percent_plot))

# View(BetG)

# now we have unnecessary dublicates in the data frame as all idividuals are presented as a
# row; individual level information can now be removed which makes working on this easier:

BetG <- BetG[!duplicated(BetG[c("plot_year")]),]
# NOTE THAT AFTER THIS STEP ALL TREE LEVEL INORMATION MEANS NOTHING;
# so all variables that you want to use later in plot levelanalysis need to be 
# calculated (averaged) before this stage if you want to use them!!!

BetG$height_plot<-as.numeric(as.character(BetG$height_plot))


# Now all plots that have growth NA in year after buiding fences, it's not really an NA but zero;
# Replace these NAs
BetG$growth[is.na(BetG$growth) & BetG$build_year==2010 & BetG$sample_year>2010]<-0
BetG$growth[is.na(BetG$growth) & BetG$build_year==2011 & BetG$sample_year>2011]<-0
# and same for height NAs:
BetG$height_plot[is.na(BetG$height_plot) & BetG$build_year==2010 & BetG$sample_year>2010]<-0
BetG$height_plot[is.na(BetG$height_plot) & BetG$build_year==2011 & BetG$sample_year>2011]<-0

# Check main patterns:
plot(BetG$growth,BetG$EX)
plot(BetG$growth)
# some crazy outliers...

# Discussed with Laurent: might be some errors (some individuals in in some year and
# out in other years), but values up to 100 cm aren't impossible, so he recommends to have all
# in.
BetG <- subset(BetG, growth<90)
BetG <- subset(BetG, growth>-90)

# * Sorame ------------------------------------------------------------------

SorameH<-tapply(Sorame$height,list(Sorame$plot, Sorame$sample_year),mean)

SorameH<-as.data.frame(SorameH)
colnames(SorameH)[colnames(SorameH)=="2010"] <- "x2010x"
colnames(SorameH)[colnames(SorameH)=="2011"] <- "x2011x"
colnames(SorameH)[colnames(SorameH)=="2012"] <- "x2012x"
colnames(SorameH)[colnames(SorameH)=="2013"] <- "x2013x"
colnames(SorameH)[colnames(SorameH)=="2014"] <- "x2014x"
colnames(SorameH)[colnames(SorameH)=="2015"] <- "x2015x"
colnames(SorameH)[colnames(SorameH)=="2016"] <- "x2016x"

# Calculate height growth each plot for each year
SorameH$x10x<-SorameH$x2011x-SorameH$x2010x
SorameH$x11x<-SorameH$x2012x-SorameH$x2011x
SorameH$x12x<-SorameH$x2013x-SorameH$x2012x
SorameH$x13x<-SorameH$x2014x-SorameH$x2013x
SorameH$x14x<-SorameH$x2015x-SorameH$x2014x
SorameH$x15x<-SorameH$x2016x-SorameH$x2015x
SorameH<-SorameH[, 8:13]
colnames(SorameH)[colnames(SorameH)=="x10x"] <- "2010"
colnames(SorameH)[colnames(SorameH)=="x11x"] <- "2011"
colnames(SorameH)[colnames(SorameH)=="x12x"] <- "2012"
colnames(SorameH)[colnames(SorameH)=="x13x"] <- "2013"
colnames(SorameH)[colnames(SorameH)=="x14x"] <- "2014"
colnames(SorameH)[colnames(SorameH)=="x15x"] <- "2015"
colnames(SorameH)[colnames(SorameH)=="x16x"] <- "2016"
names <- rownames(SorameH)
rownames(SorameH) <- NULL
SorameH <- cbind(names,SorameH)
colnames(SorameH)[colnames(SorameH)=="names"] <- "plot"
SorameH<-melt(SorameH)
colnames(SorameH)[colnames(SorameH)=="variable"] <- "Year"
colnames(SorameH)[colnames(SorameH)=="value"] <- "growth"

# View(SorameH)

# Now we need to connect this back to original data,
#  so we need a column with plot_year
SorameH$plot_year <- do.call(paste, c(SorameH[c("plot", "Year")], sep = "_"))
SorameH$plot<-NULL
SorameH$Year<-NULL

SorameG <- merge(SorameH,Sorame,by="plot_year")

# we also want to know the average height of each plot:
SorameH<-tapply(Sorame$height,list(Sorame$plot_year),mean)
names <- rownames(SorameH)
rownames(SorameH) <- NULL
SorameH <- cbind(names,SorameH)
colnames(SorameH)[colnames(SorameH)=="names"] <- "plot_year"
colnames(SorameH)[colnames(SorameH)=="SorameH"] <- "height_plot"

SorameG <- merge(SorameH,SorameG,by="plot_year")

# ...and same for browsing variables:
SorameB<-tapply(Sorame$no_browsing,list(Sorame$plot_year),mean)
names <- rownames(SorameB)
rownames(SorameH) <- NULL
SorameB <- cbind(names,SorameB)
colnames(SorameB)[colnames(SorameB)=="names"] <- "plot_year"
colnames(SorameB)[colnames(SorameB)=="SorameB"] <- "no_browsing_plot"
SorameG <- merge(SorameB,SorameG,by="plot_year")

SorameB<-tapply(Sorame$young_browsing,list(Sorame$plot_year),mean)
names <- rownames(SorameB)
rownames(SorameH) <- NULL
SorameB <- cbind(names,SorameB)
colnames(SorameB)[colnames(SorameB)=="names"] <- "plot_year"
colnames(SorameB)[colnames(SorameB)=="SorameB"] <- "young_browsing_plot"
SorameG <- merge(SorameB,SorameG,by="plot_year")

SorameB<-tapply(Sorame$old_browsing,list(Sorame$plot_year),mean)
names <- rownames(SorameB)
rownames(SorameH) <- NULL
SorameB <- cbind(names,SorameB)
colnames(SorameB)[colnames(SorameB)=="names"] <- "plot_year"
colnames(SorameB)[colnames(SorameB)=="SorameB"] <- "old_browsing_plot"
SorameG <- merge(SorameB,SorameG,by="plot_year")

SorameB<-tapply(Sorame$Browsing_percent,list(Sorame$plot_year),mean)
names <- rownames(SorameB)
rownames(SorameH) <- NULL
SorameB <- cbind(names,SorameB)
colnames(SorameB)[colnames(SorameB)=="names"] <- "plot_year"
colnames(SorameB)[colnames(SorameB)=="SorameB"] <- "Browsing_percent_plot"
SorameG <- merge(SorameB,SorameG,by="plot_year")

SorameG$Browsing_percent_plot<-as.numeric(as.character(SorameG$Browsing_percent_plot))


# View(SorameG)

# now we have unnecessary dublicates in the data frame as all idividuals are presented as a
# row; individual level information can now be removed which makes working on this easier:

SorameG <- SorameG[!duplicated(SorameG[c("plot_year")]),]
# NOTE THAT AFTER THIS STEP ALL TREE LEVEL INORMATION MEANS NOTHING;
# so all variables that you want to use later in plot levelanalysis need to be 
# calculated (averaged) before this stage if you want to use them!!!

SorameG$height_plot<-as.numeric(as.character(SorameG$height_plot))


# Now all plots that have growth NA in year after buiding fences, it's not really an NA but zero;
# Replace these NAs
SorameG$growth[is.na(SorameG$growth) & SorameG$build_year==2010 & SorameG$sample_year>2010]<-0
SorameG$growth[is.na(SorameG$growth) & SorameG$build_year==2011 & SorameG$sample_year>2011]<-0
# and same for height NAs:
SorameG$height_plot[is.na(SorameG$height_plot) & SorameG$build_year==2010 & SorameG$sample_year>2010]<-0
SorameG$height_plot[is.na(SorameG$height_plot) & SorameG$build_year==2011 & SorameG$sample_year>2011]<-0

# Check main patterns:
plot(SorameG$growth,SorameG$EX)
plot(SorameG$growth)
# some crazy outliers...

# Discussed with Laurent: might be some errors (some individuals in in some year and
# out in other years), but values up to 100 cm aren't impossible, so he recommends to have all
# in.
SorameG <- subset(SorameG, growth>-100)
SorameG <- subset(SorameG, growth<85)

# * Abibal ------------------------------------------------------------------

AbibalH<-tapply(Abibal$height,list(Abibal$plot, Abibal$sample_year),mean)

AbibalH<-as.data.frame(AbibalH)
colnames(AbibalH)[colnames(AbibalH)=="2010"] <- "x2010x"
colnames(AbibalH)[colnames(AbibalH)=="2011"] <- "x2011x"
colnames(AbibalH)[colnames(AbibalH)=="2012"] <- "x2012x"
colnames(AbibalH)[colnames(AbibalH)=="2013"] <- "x2013x"
colnames(AbibalH)[colnames(AbibalH)=="2014"] <- "x2014x"
colnames(AbibalH)[colnames(AbibalH)=="2015"] <- "x2015x"
colnames(AbibalH)[colnames(AbibalH)=="2016"] <- "x2016x"

# Calculate height growth each plot for each year
AbibalH$x10x<-AbibalH$x2011x-AbibalH$x2010x
AbibalH$x11x<-AbibalH$x2012x-AbibalH$x2011x
AbibalH$x12x<-AbibalH$x2013x-AbibalH$x2012x
AbibalH$x13x<-AbibalH$x2014x-AbibalH$x2013x
AbibalH$x14x<-AbibalH$x2015x-AbibalH$x2014x
AbibalH$x15x<-AbibalH$x2016x-AbibalH$x2015x
AbibalH<-AbibalH[, 8:13]
colnames(AbibalH)[colnames(AbibalH)=="x10x"] <- "2010"
colnames(AbibalH)[colnames(AbibalH)=="x11x"] <- "2011"
colnames(AbibalH)[colnames(AbibalH)=="x12x"] <- "2012"
colnames(AbibalH)[colnames(AbibalH)=="x13x"] <- "2013"
colnames(AbibalH)[colnames(AbibalH)=="x14x"] <- "2014"
colnames(AbibalH)[colnames(AbibalH)=="x15x"] <- "2015"
colnames(AbibalH)[colnames(AbibalH)=="x16x"] <- "2016"
names <- rownames(AbibalH)
rownames(AbibalH) <- NULL
AbibalH <- cbind(names,AbibalH)
colnames(AbibalH)[colnames(AbibalH)=="names"] <- "plot"
AbibalH<-melt(AbibalH)
colnames(AbibalH)[colnames(AbibalH)=="variable"] <- "Year"
colnames(AbibalH)[colnames(AbibalH)=="value"] <- "growth"

# View(AbibalH)

# Now we need to connect this back to original data,
#  so we need a column with plot_year
AbibalH$plot_year <- do.call(paste, c(AbibalH[c("plot", "Year")], sep = "_"))
AbibalH$plot<-NULL
AbibalH$Year<-NULL

AbibalG <- merge(AbibalH,Abibal,by="plot_year")

# we also want to know the average height of each plot:
AbibalH<-tapply(Abibal$height,list(Abibal$plot_year),mean)
names <- rownames(AbibalH)
rownames(AbibalH) <- NULL
AbibalH <- cbind(names,AbibalH)
colnames(AbibalH)[colnames(AbibalH)=="names"] <- "plot_year"
colnames(AbibalH)[colnames(AbibalH)=="AbibalH"] <- "height_plot"

AbibalG <- merge(AbibalH,AbibalG,by="plot_year")

# ...and same for browsing variables:
AbibalB<-tapply(Abibal$no_browsing,list(Abibal$plot_year),mean)
names <- rownames(AbibalB)
rownames(AbibalH) <- NULL
AbibalB <- cbind(names,AbibalB)
colnames(AbibalB)[colnames(AbibalB)=="names"] <- "plot_year"
colnames(AbibalB)[colnames(AbibalB)=="AbibalB"] <- "no_browsing_plot"
AbibalG <- merge(AbibalB,AbibalG,by="plot_year")

AbibalB<-tapply(Abibal$young_browsing,list(Abibal$plot_year),mean)
names <- rownames(AbibalB)
rownames(AbibalH) <- NULL
AbibalB <- cbind(names,AbibalB)
colnames(AbibalB)[colnames(AbibalB)=="names"] <- "plot_year"
colnames(AbibalB)[colnames(AbibalB)=="AbibalB"] <- "young_browsing_plot"
AbibalG <- merge(AbibalB,AbibalG,by="plot_year")

AbibalB<-tapply(Abibal$old_browsing,list(Abibal$plot_year),mean)
names <- rownames(AbibalB)
rownames(AbibalH) <- NULL
AbibalB <- cbind(names,AbibalB)
colnames(AbibalB)[colnames(AbibalB)=="names"] <- "plot_year"
colnames(AbibalB)[colnames(AbibalB)=="AbibalB"] <- "old_browsing_plot"
AbibalG <- merge(AbibalB,AbibalG,by="plot_year")

AbibalB<-tapply(Abibal$Browsing_percent,list(Abibal$plot_year),mean)
names <- rownames(AbibalB)
rownames(AbibalH) <- NULL
AbibalB <- cbind(names,AbibalB)
colnames(AbibalB)[colnames(AbibalB)=="names"] <- "plot_year"
colnames(AbibalB)[colnames(AbibalB)=="AbibalB"] <- "Browsing_percent_plot"
AbibalG <- merge(AbibalB,AbibalG,by="plot_year")

AbibalG$Browsing_percent_plot<-as.numeric(as.character(AbibalG$Browsing_percent_plot))


# View(AbibalG)

# now we have unnecessary dublicates in the data frame as all idividuals are presented as a
# row; individual level information can now be removed which makes working on this easier:

AbibalG <- AbibalG[!duplicated(AbibalG[c("plot_year")]),]
# NOTE THAT AFTER THIS STEP ALL TREE LEVEL INORMATION MEANS NOTHING;
# so all variables that you want to use later in plot levelanalysis need to be 
# calculated (averaged) before this stage if you want to use them!!!

AbibalG$height_plot<-as.numeric(as.character(AbibalG$height_plot))


# Now all plots that have growth NA in year after buiding fences, it's not really an NA but zero;
# Replace these NAs
AbibalG$growth[is.na(AbibalG$growth) & AbibalG$build_year==2010 & AbibalG$sample_year>2010]<-0
AbibalG$growth[is.na(AbibalG$growth) & AbibalG$build_year==2011 & AbibalG$sample_year>2011]<-0
# and same for height NAs:
AbibalG$height_plot[is.na(AbibalG$height_plot) & AbibalG$build_year==2010 & AbibalG$sample_year>2010]<-0
AbibalG$height_plot[is.na(AbibalG$height_plot) & AbibalG$build_year==2011 & AbibalG$sample_year>2011]<-0

# Check main patterns:
plot(AbibalG$growth,AbibalG$EX)
plot(AbibalG$growth)
# some crazy outliers...

# plot 52 got -180:
plot(data=AbibalG[AbibalG$plot=="52",],height_plot~sample_year)
new<-AbibalG[AbibalG$plot==52,]
myvars <- c("sample_year", "height_plot")
check <- new[myvars]
check
# plot level height crashes 2015

# plot 142 got -120:
plot(data=AbibalG[AbibalG$plot=="142",],height_plot~sample_year)
new<-AbibalG[AbibalG$plot==142,]
myvars <- c("sample_year", "height_plot")
check <- new[myvars]
check
# crazy peak in 2012

# plot 381 got -104:
plot(data=AbibalG[AbibalG$plot=="381",],height_plot~sample_year)
new<-AbibalG[AbibalG$plot==381,]
myvars <- c("sample_year", "height_plot")
check <- new[myvars]
check
# plot level height crashes 2015
# 2013 NA? should not be! but cannot be zero either...

# plot 242 got -80:
plot(data=AbibalG[AbibalG$plot=="242",],height_plot~sample_year)
new<-AbibalG[AbibalG$plot==242,]
myvars <- c("sample_year", "height_plot")
check <- new[myvars]
check
# crash  in 2013 

# and some crazy positive growth values: 

# plot 89 got 120:
plot(data=AbibalG[AbibalG$plot=="89",],height_plot~sample_year)
new<-AbibalG[AbibalG$plot==89,]
myvars <- c("sample_year", "height_plot")
check <- new[myvars]
check
# crash  in 2012  

# plot 194 got 95:
plot(data=AbibalG[AbibalG$plot=="194",],height_plot~sample_year)
new<-AbibalG[AbibalG$plot==194,]
myvars <- c("sample_year", "height_plot")
check <- new[myvars]
check
# crazy peak on 2015

# Discussed with Laurent: might be some errors (some individuals in in some year and
# out in other years), but values up to 100 cm aren't impossible, so he recommends to have all
# in.
AbibalG <- subset(AbibalG, growth>-100)
AbibalG <- subset(AbibalG, growth<85)


# per area tranformation --------------------------------------------------

# And finally, we want to change grwoth for all species to be
# per m2, not per plot. Radius is 1.13m, so:

AbibalG$growth<-AbibalG$growth/4.011499659
BetG$growth<-BetG$growth/4.011499659
PicG$growth<-PicG$growth/4.011499659
SorameG$growth<-SorameG$growth/4.011499659

# How does the height-growth relatioship look? ---------------------------------------------

# * Pic ----------------------------------------------------------------

str(PicG)
plot(PicG$height_plot,PicG$growth)
fit <- glm(PicG$height_plot~PicG$growth)
abline(fit, lwd=2)

checkM <- lme(growth ~  height_plot,
              na.action=na.exclude, 
              random =~ 1 | site/plot,
              data = PicG,
              method = "ML") 
summary(checkM)

plot(PicG$height_plot,PicG$growth)
GAM <- gam(growth ~ s(height_plot, bs="cr"), random =~ 1 | site, data=PicG)
summary(GAM)
plot(GAM)




# * Bet ----------------------------------------------------------------

str(BetG)
plot(BetG$height_plot,BetG$growth)
fit <- glm(BetG$height_plot~BetG$growth)
abline(fit, lwd=2)

checkM <- lme(growth ~  height_plot,
              na.action=na.exclude, 
              random =~ 1 | site/plot,
              data = BetG,
              method = "ML") 
summary(checkM)

# just to test how this would look wihtout minus values:
BetGPLUGRWTH<-BetG[BetG$growth>0,]
plot(BetGPLUGRWTH$height_plot,BetGPLUGRWTH$growth)
fit <- glm(BetGPLUGRWTH$height_plot~BetGPLUGRWTH$growth)
abline(fit, lwd=2)


# * Sorame ----------------------------------------------------------------

str(SorameG)
plot(SorameG$height_plot,SorameG$growth)
fit <- glm(SorameG$height_plot~SorameG$growth)
abline(fit, lwd=2)

checkM <- lme(growth ~  height_plot,
              na.action=na.exclude, 
              random =~ 1 | site/plot,
              data = SorameG,
              method = "ML") 
summary(checkM)

plot(SorameG$height_plot,SorameG$growth)
GAM <- gam(growth ~ s(height_plot, bs="cr"), random =~ 1 | site, data=SorameG)
summary(GAM)
plot(GAM)

# * Abibal ----------------------------------------------------------------

str(AbibalG)
plot(AbibalG$height_plot,AbibalG$growth)
fit <- glm(AbibalG$height_plot~AbibalG$growth)
abline(fit, lwd=2)
# hmmm, this is queer... is this caused by negative growth values?
# In Sustherb, if one tree died it was replaced with a new one, so there arent
# very many negative values; in gaspesie, al trees were in all the time, so negative values
# occur more often (more realistic picture of population level growth).
# Sustherb is biased towards trees alive!!! (kind of similar as in Scotland dendro versus 
# height monitoring...)

# just to test how this would look wihtout minus values:
AbibalGPLUGRWTH<-AbibalG[AbibalG$growth>0,]
plot(AbibalGPLUGRWTH$height_plot,AbibalGPLUGRWTH$growth)
fit <- glm(AbibalGPLUGRWTH$height_plot~AbibalGPLUGRWTH$growth)
abline(fit, lwd=2)
# yep, then height effect is clear

checkM <- lme(growth ~  height_plot,
              na.action=na.exclude, 
              random =~ 1 | site/plot,
              data = AbibalG,
              method = "ML") 
summary(checkM)

plot(AbibalG$height_plot,AbibalG$growth)
GAM <- gam(growth ~ s(height_plot, bs="cr"), random =~ 1 | site, data=AbibalG)
summary(GAM)
plot(GAM)


# Write final table -------------------------

dfGasp<-rbind(AbibalG, SorameG, BetG, PicG)

write.table(dfGasp, "H:/Trondheim/BOREAL SUCCESSION/ConstructingData_Gaspesie/dfGasp.txt", sep="\t") 

