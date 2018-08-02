devtools::install_github("OJWatson/rdhs")
library(dplyr)
library(rgdal)
install.packages("digest")

library(rdhs)
dat <- dhs_data(indicatorIds = "HC_AGON_H_ANM",breakdown = "subnational")
dats <- dhs_datasets(surveyIds = unique(dat$SurveyId), fileType = "HR", fileFormat = "FL")
set_rdhs_config("hannah.slater@imperial.ac.uk","Historical patterns of vector control usage",config_path = "~/.rdhs.json",password_prompt = TRUE)
s <- get_datasets(dats)

qu <- search_variable_labels(dats$FileName,"farm animals")
ex <- extract_dhs(qu, add_geo = TRUE)


# Mozambique
dat <- dhs_data(countryIds = "MZ",breakdown = "subnational")
dats <- dhs_datasets(surveyIds = unique(dat$SurveyId), fileType = "HR", fileFormat = "FL")
qu <- search_variable_labels(dats$FileName,"farm animals")

qu <- search_variable_labels(dats$FileName,c("cattle", "cow", "livestock", "pig"))
ex <- extract_dhs(qu, add_geo = TRUE)

table(ex$MZHR51FL$hv246)
table(ex$MZHR62FL$hv246)
table(ex$MZHR71FL$hv246)

table(ex$MZHR51FL$hv246a)
table(ex$MZHR62FL$hv246b)
summary(ex$MZHR51FL)
summary(ex$MZHR62FL)

d2 = ex$MZHR62FL
d2$hv246b[d2$hv246b %in% c(98,99)] = NA
d2$own_cattle = d2$hv246b
d2$own_cattle[d2$hv246b >= 1] = 1

table(d2$hv246g)
d2$hv246g[d2$hv246g %in% c(98,99)] = NA
d2$own_pig = d2$hv246g
d2$own_pig[d2$hv246g >= 1] = 1

d2_clus = dplyr::summarise(group_by(d2, CLUSTER),
          own_cattle_clus = mean(own_cattle, na.rm=TRUE),
          own_pig_clus = mean(own_pig, na.rm=TRUE),
          any_animals = mean(hv246, na.rm=TRUE),
          yy = LATNUM[1],
          xx = LONGNUM[1])


summary(d2_clus)

d2_clus = d2_clus[-which(round(d2_clus$xx,1) == 0),]

d2_clus$col = cut(d2_clus$own_cattle_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
d2_clus$col2 = cut(d2_clus$own_pig_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
d2_clus$col3 = cut(d2_clus$any_animals, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)

# cols = brewer.pal(6, "YlOrRd")
# cols2 = brewer.pal(6, "YlGnBu")
cols3 = cols2 = cols = rev(brewer.pal(6, "Spectral"))


mz2 <- readOGR(dsn="C:/Users/hs1308/Dropbox (SPH Imperial College)/all zambia/zam_borders/shapefiles/moz_polbnda_adm2_districts_wfp_ine_pop2012_15_ocha.shp", layer="moz_polbnda_adm2_districts_wfp_ine_pop2012_15_ocha")

tiff("C:/Users/hs1308/Dropbox (SPH Imperial College)/Ivermectin/BOHEMIA MODELLING/JULY18_ALL/PLOTS/moz_animals_2011.tiff",width=280,height=130,units="mm",res=300, compression="lzw")
par(mar=rep(0,4), mfrow=c(1,3))

plot(mz2, border="white", col="grey50")
points(d2_clus$xx, d2_clus$yy, col=cols3[d2_clus$col3], pch=16)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols3, title = "Proportion of homes in\ncluster that own any animal", bty="n")

plot(mz2, border="white", col="grey50")
points(d2_clus$xx, d2_clus$yy, col=cols[d2_clus$col], pch=16)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own cattle", bty="n")

plot(mz2, border="white", col="grey50")
points(d2_clus$xx, d2_clus$yy, col=cols2[d2_clus$col2], pch=16)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols2, title = "Proportion of homes in\ncluster that own pigs", bty="n")

dev.off()

######################################################################################################################


d3 = ex$MZHR71FL
d3$hv246b[d3$hv246b %in% c(98,99)] = NA
d3$own_cattle = d3$hv246b
d3$own_cattle[d3$hv246b >= 1] = 1

d3$hv246[d3$hv246 == 9] = NA

table(d3$hv246g)
d3$hv246g[d3$hv246g %in% c(98,99)] = NA
d3$own_pig = d3$hv246g
d3$own_pig[d3$hv246g >= 1] = 1

d3_clus = dplyr::summarise(group_by(d3, CLUSTER),
                           own_cattle_clus = mean(own_cattle, na.rm=TRUE),
                           own_pig_clus = mean(own_pig, na.rm=TRUE),
                           any_animals = mean(hv246, na.rm=TRUE),
                           yy = LATNUM[1],
                           xx = LONGNUM[1])


summary(d3_clus)

d3_clus$col = cut(d3_clus$own_cattle_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
d3_clus$col2 = cut(d3_clus$own_pig_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
d3_clus$col3 = cut(d3_clus$any_animals, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)

# cols = brewer.pal(6, "YlOrRd")
# cols2 = brewer.pal(6, "YlGnBu")
cols3 = cols2 = cols = rev(brewer.pal(6, "Spectral"))


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

#########################################################################################################################



# Benin
dat <- dhs_data(countryIds = "BJ",breakdown = "subnational")
dats <- dhs_datasets(surveyIds = unique(dat$SurveyId), fileType = "HR", fileFormat = "FL")


qu <- search_variable_labels(dats$FileName,c("cattle", "cow", "livestock", "pig"))
ex <- extract_dhs(qu, add_geo = TRUE)

table(ex$MZHR51FL$hv246)
table(ex$MZHR62FL$hv246)
table(ex$MZHR71FL$hv246)

table(ex$MZHR51FL$hv246a)
table(ex$MZHR62FL$hv246b)
summary(ex$MZHR51FL)
summary(ex$MZHR62FL)

b2 = ex$BJHR61FL

b2$hv246b[b2$hv246b %in% c(98,99)] = NA
b2$hv246a[b2$hv246a %in% c(98,99)] = NA

b2$hv246ab = b2$hv246a + b2$hv246b

b2$own_cattle = b2$hv246ab
b2$own_cattle[b2$hv246ab >= 1] = 1



b2_clus = dplyr::summarise(group_by(b2, CLUSTER),
                           own_cattle_clus = mean(own_cattle, na.rm=TRUE),
                           any_animals = mean(hv246, na.rm=TRUE),
                           yy = LATNUM[1],
                           xx = LONGNUM[1])


summary(b2_clus)

b2_clus = b2_clus[-which(round(b2_clus$xx,1) == 0),]

b2_clus$col = cut(b2_clus$own_cattle_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
b2_clus$col3 = cut(b2_clus$any_animals, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)

# cols = brewer.pal(6, "YlOrRd")
# cols2 = brewer.pal(6, "YlGnBu")
cols3 = cols2 = cols = rev(brewer.pal(6, "Spectral"))


bn2 <- readOGR(dsn="C:/Users/hs1308/Dropbox (SPH Imperial College)/SHAPEFILES/benin/ben_admbnda_adm2_1m_salb.shp", layer="ben_admbnda_adm2_1m_salb")

tiff("C:/Users/hs1308/Dropbox (SPH Imperial College)/Ivermectin/BOHEMIA MODELLING/JULY18_ALL/PLOTS/benin_animals_2011.tiff",width=300,height=180,units="mm",res=300, compression="lzw")
par(mar=rep(0,4), mfrow=c(1,2))

plot(bn2, border="white", col="grey50")
points(b2_clus$xx, b2_clus$yy, col=cols3[b2_clus$col3], pch=16, cex=0.7)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols3, title = "Proportion of homes in\ncluster that own any animal", bty="n")

plot(bn2, border="white", col="grey50")
points(b2_clus$xx, b2_clus$yy, col=cols[b2_clus$col], pch=16, cex=0.7)
legend("bottomright", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own cattle", bty="n")

dev.off()



###########################################################################################################################

# Tanzania 2012
dat <- dhs_data(countryIds = "TZ",breakdown = "subnational")

f <- dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel", "SurveyType","CountryName"))
datname = f$SurveyId[grep("TZ", f$SurveyId)]

dats <- dhs_datasets(surveyIds = datname, fileType = "HR", fileFormat = "FL")



qu <- search_variable_labels(dats$FileName,c("cattle", "cow", "livestock", "pig"))
ex <- extract_dhs(qu, add_geo = TRUE)

table(ex$TZHR6AFL$hv246)

t2 = ex$TZHR6AFL
t2$hv246[t2$hv246 == 9] = NA

t2$hv246b[t2$hv246b %in% c(98,99)] = NA
t2$hv246a[t2$hv246a %in% c(98,99)] = NA
t2$hv246ab = t2$hv246b + t2$hv246a
t2$own_cattle = t2$hv246ab
t2$own_cattle[t2$hv246ab >= 1] = 1



table(t2$hv246g)
t2$hv246g[t2$hv246g %in% c(98,99)] = NA
t2$own_pig = t2$hv246g
t2$own_pig[t2$hv246g >= 1] = 1

t2_clus = dplyr::summarise(group_by(t2, CLUSTER),
                           own_cattle_clus = mean(own_cattle, na.rm=TRUE),
                           own_pig_clus = mean(own_pig, na.rm=TRUE),
                           any_animals = mean(hv246, na.rm=TRUE),
                           yy = LATNUM[1],
                           xx = LONGNUM[1])


summary(t2_clus)

t2_clus$col = cut(t2_clus$own_cattle_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
t2_clus$col2 = cut(t2_clus$own_pig_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
t2_clus$col3 = cut(t2_clus$any_animals, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)

# cols = brewer.pal(6, "YlOrRd")
# cols2 = brewer.pal(6, "YlGnBu")
cols3 = cols2 = cols = rev(brewer.pal(6, "Spectral"))


tz2 <- readOGR(dsn="C:/Users/hs1308/Dropbox (SPH Imperial College)/SHAPEFILES/tanzania/districts.shp", layer="districts")

tiff("C:/Users/hs1308/Dropbox (SPH Imperial College)/Ivermectin/BOHEMIA MODELLING/JULY18_ALL/PLOTS/tanz_animals_2012.tiff",width=330,height=120,units="mm",res=300, compression="lzw")
par(mar=rep(0,4), mfrow=c(1,3))

plot(tz2, border="white", col="grey50")
points(t2_clus$xx, t2_clus$yy, col=cols3[t2_clus$col3], pch=16)
legend("bottomleft", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols3, title = "Proportion of homes in\ncluster that own any animal", bty="n")

plot(tz2, border="white", col="grey50")
points(t2_clus$xx, t2_clus$yy, col=cols[t2_clus$col], pch=16)
legend("bottomleft", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own cattle", bty="n")

plot(tz2, border="white", col="grey50")
points(t2_clus$xx, t2_clus$yy, col=cols2[t2_clus$col2], pch=16)
legend("bottomleft", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols2, title = "Proportion of homes in\ncluster that own pigs", bty="n")

dev.off()

###########################################################################################################################

# Tanzania 2012
dat <- dhs_data(countryIds = "TZ",breakdown = "subnational")

f <- dhs_surveys(returnFields=c("SurveyId","SurveyYearLabel", "SurveyType","CountryName"))
datname = f$SurveyId[grep("TZ", f$SurveyId)]

dats <- dhs_datasets(surveyIds = datname, fileType = "HR", fileFormat = "FL")


qu <- search_variable_labels(dats$FileName,c("cattle", "cow", "livestock", "pig"))
ex <- extract_dhs(qu, add_geo = TRUE)

table(ex$TZHR6AFL$hv246)

t3 = ex$TZHR7HFL

t3$hv246b[t3$hv246b %in% c(98,99)] = NA
t3$hv246a[t3$hv246a %in% c(98,99)] = NA
t3$hv246ab = t3$hv246b + t3$hv246a
t3$own_cattle = t3$hv246ab
t3$own_cattle[t3$hv246ab >= 1] = 1



t3_clus = dplyr::summarise(group_by(t3, CLUSTER),
                           own_cattle_clus = mean(own_cattle, na.rm=TRUE),
                           any_animals = mean(hv246, na.rm=TRUE),
                           yy = LATNUM[1],
                           xx = LONGNUM[1])


summary(t3_clus)

t3_clus$col = cut(t3_clus$own_cattle_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
#t3_clus$col2 = cut(t3_clus$own_pig_clus, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)
t3_clus$col3 = cut(t3_clus$any_animals, c(0, 0.05, 0.1, 0.2, 0.5, 0.75, 1),include.lowest=TRUE, labels = 1:6)

# cols = brewer.pal(6, "YlOrRd")
# cols2 = brewer.pal(6, "YlGnBu")
cols3 = cols2 = cols = rev(brewer.pal(6, "Spectral"))


tz2 <- readOGR(dsn="C:/Users/hs1308/Dropbox (SPH Imperial College)/SHAPEFILES/tanzania/districts.shp", layer="districts")

tiff("C:/Users/hs1308/Dropbox (SPH Imperial College)/Ivermectin/BOHEMIA MODELLING/JULY18_ALL/PLOTS/tanz_animals_2016.tiff",width=290,height=135,units="mm",res=300, compression="lzw")
par(mar=rep(0,4), mfrow=c(1,2))

plot(tz2, border="white", col="grey50")
points(t3_clus$xx, t3_clus$yy, col=cols3[t3_clus$col3], pch=16)
legend("bottomleft", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols3, title = "Proportion of\nhomes in cluster\nthat own any\nanimal", bty="n", cex=0.9)

plot(tz2, border="white", col="grey50")
points(t3_clus$xx, t3_clus$yy, col=cols[t3_clus$col], pch=16)
legend("bottomleft", c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of\nhomes in cluster\nthat own cattle", bty="n", cex=0.9)

dev.off()


