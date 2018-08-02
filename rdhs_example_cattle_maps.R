# nice version for OJ

# example of using rdhs package 
# by Hannah Slater 2nd July 2018 (hannah.slater@imperial.ac.uk)

# AIM: plot the proportion of homes that own any animal, any cattle, and any pigs in 3 African countries

# (for a project looking at the impact of targeting cattle or pigs with ivermectin to kill zoophilic 
# mosquitoes, and reduce malaria transmission)

devtools::install_github("OJWatson/rdhs")
install.packages("digest")

library(dplyr)
library(rgdal)
library(rdhs)
library(RColorBrewer)

set_rdhs_config("hannah.slater@imperial.ac.uk","Historical patterns of vector control usage",config_path = "~/.rdhs.json",password_prompt = FALSE, timeout=120)
#set_rdhs_config("your email address","project name registered on DHS website",config_path = "~/.rdhs.json",password_prompt = FALSE)


####  target countries - Benin, Mozambique and Tanzania

# find country codes for target countries
all = dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))
country_codes = all$DHS_CountryCode[all$CountryName %in% c("Tanzania", "Mozambique", "Benin")]

# select the relevant surveys
dat <- dhs_data(countryIds = country_codes,breakdown = "subnational")

# if the above command is hanging, can do the same with the below
# surv <- dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel", "SurveyType","CountryName"))
# dat <- surv[unlist(lapply(country_codes, FUN=function(x)grep(x, surv$SurveyId))),]

# API request of datasets
dats <- dhs_datasets(surveyIds = unique(dat$SurveyId), fileType = "HR", fileFormat = "FL")

# define search terms (experiment with terms a bit to ensure you have an exhaustive list)
qu <- search_variable_labels(dats$FileName,c("cattle", "cow", "livestock", "pig"))
# extract surveys that are from target countries and contain 1+ of these search terms
ex <- extract_dhs(qu, add_geo = TRUE)

names(ex)

d1 = ex[[i]]

vars = qu[qu$dataset_filename == names(ex[i]),]
vars

# check variables
table(d1$hv246)
table(d1$hv246a)
table(d1$hv246b)
table(d1$hv246g)

# if any variables missing, replace with NA's
if(is.null(d1$hv246a)) d1$hv246a = rep(NA, nrow(d1))
if(is.null(d1$hv246b)) d1$hv246b = rep(NA, nrow(d1))
if(is.null(d1$hv246g)) d1$hv246g = rep(NA, nrow(d1))


# 98's and 99's mean unknown or not answered, so set to NA
d1$hv246b[d1$hv246a %in% c(98,99)] = NA
d1$hv246a[d1$hv246b %in% c(98,99)] = NA
d1$hv246g[d1$hv246g %in% c(98,99)] = NA

# combine 'Owns cattle' and 'Owns cows/ bulls'
d1$hv246ab = apply(cbind(d1$hv246a,d1$hv246b), MARGIN = 1, FUN=sum, na.rm=T)

# make a new variable: 0 if d1$hv246ab = 0, 1 otherwise 
d1$own_cattle = d1$hv246ab
d1$own_cattle[d1$hv246ab >= 1] = 1

# 
d1$own_pig = d1$hv246g
d1$own_pig[d1$hv246g >= 1] = 1


# summarise the proportion of household that own cattle by cluster
d1_clus = dplyr::summarise(group_by(d1, CLUSTER),
                           own_cattle_clus = mean(own_cattle, na.rm=TRUE),
                           any_animals = mean(hv246, na.rm=TRUE),
                           own_pig_clus = mean(own_pig, na.rm=TRUE),
                           yy = LATNUM[1],
                           xx = LONGNUM[1])
d1_clus

# remove any locations with (0,0) co-ordinates
if(any(round(d1_clus$xx,1)==0)) d1_clus = d1_clus[-which(round(d1_clus$xx,1) == 0),]

# cut the variables up to make colour codes
d1_clus$col = cut(d1_clus$own_cattle_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
d1_clus$col2 = cut(d1_clus$own_pig_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
d1_clus$col3 = cut(d1_clus$any_animals, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)

cols = rev(brewer.pal(6, "Spectral"))

mz2 <- readOGR(dsn="C:/Users/hs1308/Dropbox (SPH Imperial College)/all zambia/zam_borders/shapefiles/moz_polbnda_adm2_districts_wfp_ine_pop2012_15_ocha.shp", layer="moz_polbnda_adm2_districts_wfp_ine_pop2012_15_ocha")

tiff("C:/Users/hs1308/Dropbox (SPH Imperial College)/Ivermectin/BOHEMIA MODELLING/JULY18_ALL/PLOTS/moz_animals_2015.tiff",width=280,height=130,units="mm",res=300, compression="lzw")
par(mar=rep(0,4), mfrow=c(1,3))

plot(mz2, border="white", col="grey50")
points(d3_clus$xx, d3_clus$yy, col=cols3[d3_clus$col3], pch=16)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols3, title = "Proportion of homes in\ncluster that own any animal", bty="n")

plot(mz2, border="white", col="grey50")
points(d3_clus$xx, d3_clus$yy, col=cols[d3_clus$col], pch=16)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own cattle", bty="n")

plot(mz2, border="white", col="grey50")
points(d3_clus$xx, d3_clus$yy, col=cols2[d3_clus$col2], pch=16)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols2, title = "Proportion of homes in\ncluster that own pigs", bty="n")

dev.off()
