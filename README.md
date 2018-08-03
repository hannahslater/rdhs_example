## Example using rdhs 
Example using rdhs package developed by Oliver Watson and Jeff Eaton


by Hannah Slater 2nd July 2018 (hannah.slater@imperial.ac.uk)

AIM: plot the proportion of homes that own any animal, any cattle, and any pigs in 3 African countries

(For a project looking at the impact of targeting cattle or pigs with ivermectin to kill zoophilic 
mosquitoes, and reduce malaria transmission)
```R
devtools::install_github("OJWatson/rdhs")
install.packages("digest")

library(dplyr)
library(rgdal)
library(rdhs)
library(RColorBrewer)+
library(digest)

set_rdhs_config("your email address","project name registered on DHS website",config_path = "~/.rdhs.json",password_prompt = FALSE)
```
Target countries - Benin, Mozambique and Tanzania

Find country codes for target countries
```R
all = dhs_countries(returnFields=c("CountryName","DHS_CountryCode"))
country_codes = all$DHS_CountryCode[all$CountryName %in% c("Tanzania", "Mozambique", "Benin")]
dats <- dhs_datasets(countryIds = country_codes, fileType = "HR", fileFormat = "FL")
```
Define search terms (experiment with terms a bit to ensure you have an exhaustive list)
```
qu <- search_variable_labels(dats$FileName,c("cattle", "cow", "livestock", "pig"))
```
Extract surveys that are from target countries and contain 1+ of these search terms
```
ex <- extract_dhs(qu, add_geo = TRUE)
```

Make an list object to store summaries from each survey, and vector to hold survey names
 ```
d1_clus <- list()
names <- rep(NA, length(names(ex)))
```
Loop through each survey, extract and clean relevant variables, then aggregate by cluster
```
for(i in 1:length(names(ex))){
  d1 = ex[[i]]
  
  names[i] = qu$survey_id[qu$dataset_filename == names(ex)[i]][1]
  
  vars = qu[qu$dataset_filename == names(ex[i]),]
  vars
  
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
  d1_clus[[i]] = dplyr::summarise(group_by(d1, CLUSTER),
                             own_cattle_clus = mean(own_cattle, na.rm=TRUE),
                             any_animals = mean(hv246, na.rm=TRUE),
                             own_pig_clus = mean(own_pig, na.rm=TRUE),
                             yy = LATNUM[1],
                             xx = LONGNUM[1])
  d1_clus
  
  # remove any locations with (0,0) co-ordinates
  if(any(round(d1_clus[[i]]$xx,1)==0)) d1_clus[[i]] = d1_clus[[i]][-which(round(d1_clus[[i]]$xx,1) == 0),]
  
  # cut the variables up to make colour codes
  d1_clus[[i]]$col = cut(d1_clus[[i]]$own_cattle_clus, c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),include.lowest=TRUE, labels = 1:6)
  d1_clus[[i]]$col2 = cut(d1_clus[[i]]$own_pig_clus, c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),include.lowest=TRUE, labels = 1:6)
  d1_clus[[i]]$col3 = cut(d1_clus[[i]]$any_animals, c(0, 0.1, 0.2, 0.4, 0.6, 0.8, 1),include.lowest=TRUE, labels = 1:6)

}
```

## Plotting
```
cols = rev(brewer.pal(6, "Spectral"))

# download shapefiles from my github 
get_shps <- function(urlname){
  wd = getwd()
  file <- basename(urlname)
  download.file(urlname, file)
  unzip(file, exdir = wd)
}

get_shps("https://raw.githubusercontent.com/hannahslater/rdhs_example/master/moz_adm2.zip")
get_shps("https://raw.githubusercontent.com/hannahslater/rdhs_example/master/benin_adm2.zip")
get_shps("https://raw.githubusercontent.com/hannahslater/rdhs_example/master/tan_adm2.zip")

list.files(".", pattern = "shp")
wd = getwd()
mz2 <- readOGR(dsn=wd, layer="moz_polbnda_adm2_districts_wfp_ine_pop2012_15_ocha")
ben2 <- readOGR(dsn=wd, layer="ben_admbnda_adm2_1m_salb")
tz2 <- readOGR(dsn=wd, layer="Districts")

plot_func <- function(dat, shp, nmaps, legplace = "bottomright"){
  par(mar=rep(0,4), mfrow=c(1,nmaps))
  plot(shp, border="white", col="grey50")
  points(dat$xx, dat$yy, col=cols[dat$col3], pch=16)
  legend(legplace, c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own any animal", bty="n")
  
  if(nmaps>1){
    plot(shp, border="white", col="grey50")
    points(dat$xx, dat$yy, col=cols[dat$col], pch=16)
    legend(legplace, c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own cattle", bty="n")
  }
  if(nmaps > 2){
    plot(shp, border="white", col="grey50")
    points(dat$xx, dat$yy, col=cols[dat$col2], pch=16)
    legend(legplace, c("0-10%", "10-20%", "20-40%", "40-60%", "60-80%", "80-100%"), pch=16, col=cols, title = "Proportion of homes in\ncluster that own pigs", bty="n")
  }
}

names

tiff(paste0(names[1],"_map.tiff"),width=300,height=180,units="mm",res=300, compression="lzw")
plot_func(d1_clus[[1]], shp = ben2, nmaps=2)
dev.off()

tiff(paste0(names[3],"_map.tiff"),width=280,height=130,units="mm",res=300, compression="lzw")
plot_func(d1_clus[[3]], shp = mz2, nmaps=3)
dev.off()

tiff(paste0(names[4],"_map.tiff"),width=280,height=130,units="mm",res=300, compression="lzw")
plot_func(d1_clus[[4]], shp = mz2, nmaps=3)
dev.off()

tiff(paste0(names[5],"_map.tiff"),width=280,height=130,units="mm",res=300, compression="lzw")
plot_func(d1_clus[[5]], shp = tz2, nmaps=3, legplace = "bottomleft")
dev.off()

tiff(paste0(names[6],"_map.tiff"),width=300,height=180,units="mm",res=300, compression="lzw")
plot_func(d1_clus[[6]], shp = tz2, nmaps=2, legplace = "bottomleft")
dev.off()


jpeg(paste0(names[5],"_map.jpg"),width=280,height=130,units="mm",res=300)
plot_func(d1_clus[[5]], shp = tz2, nmaps=3, legplace = "bottomleft")
dev.off()
```
