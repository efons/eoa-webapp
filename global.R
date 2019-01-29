# EOA's website redesign 
# Data visualization app 
# Emilie Fons 
# Michaela Palmer


# Script 1 - Global Environment 


# set wd
# setwd("F:/SCVURPPP (SC)/SC94 (2018-19)/Website Re-design/R code for webapp")

#publish app 
#rsconnect::setAccountInfo(name='scvurppp',token= 'B910374A8BB4335B26ECEEBB507FBD59',secret = 'GnPUQAibzBmyN2bS0qbHqxHmp/bt1CCtHOZaSfpw')



# Required libraries 
library(xlsx)
library(readxl)
library(plyr)
library(tidyverse)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(magrittr)
library(rgdal)
library(lubridate)
library(stringr)
library(scales)
library(latex2exp)
library(beanplot)
library(rsconnect)




#  - Import spatial files to be used for mapping
################################################################################################################################################
################################################################################################################################################
# watersheds delineation 
sheds <- readOGR("./shp/SBC_Sheds_SCC_Only_w_Alameda.shp", GDAL1_integer64_policy = F) %>%
  spTransform(CRS("+init=epsg:4326"))

# creeks 
#creeks <- readOGR("M:/Santa Clara/Hydrography/Surface Waters/Working/scvurppp_creeks.shp") %>%
# spTransform(CRS("+init=epsg:4326"))
# is there a file with a ws data field as well? 


# sites info 
sites <- read_excel("sites master file.xlsx", na=c("", "NA", "#N/A"),sheet="copy_01.28") %>%
  arrange(rmc_id)


# make sure shp watershed names are consistent with sites file
ws <- c("Lower Penitencia", "San Francisquito", "Coyote", "Matadero/Barron", "Adobe", "Sunnyvale West", "Guadalupe", "Permanente", "Calabazas", "San Tomas Aquino", "Sunnyvale East","Stevens")
sites$watershed <- ifelse(!is.na(sites$watershed) & sites$watershed == "San Thomas Aquino", "San Tomas Aquino", sites$watershed)
sites$watershed <- ifelse(!is.na(sites$watershed) & sites$watershed == "Matadero Creek", "Matadero/Barron Creeks", sites$watershed)
sites$watershed <- mapvalues(sites$watershed, from=unique(sites$watershed), to=c(NA,"Alameda","Coyote","Guadalupe", "San Tomas Aquino", "Calabazas", "Stevens", "Lower Penitencia","Matadero/Barron","Adobe","Permanente","San Francisquito"))

ws_sheds <- unique(sheds$SYSTEM)

# Make sure creek names are consistent with sites file 
# later 
#vars_creek <- unique(creeks$NAME)
#vars_creek # 196 creeks






# A - Import and reshape bioassessment data 
##############################################################################################################################
##############################################################################################################################

# upload data

# data 
df_bio <- read_excel("data_master_bio_2012_18.xlsx", sheet="All",na=c("","NR", "Not Recorded", "NA"))


# select data fields to be plotted and rename
old_param_names <- c("Elevation (m)", "BMI Taxa", "Diatom Taxa", "CSCI","D18", "ASCI Hybrid", "ASCI diatoms", "Shannon Diversity (H) of Natural Substrate Types",'Percent Substrate Smaller than Sand (<2 mm)',"Percent Impervious", "Road Density Watershed", "Total N", "Chlorophyll a (mg/m2)", '% Macroalgae Cover', "Mean Filamentous Algae Cover", "IPI Score", "Epifaunal Substrate", "Sediment Deposition", "Channel Alteration", "Total PHAB")

select_param <- c("rmc_id","wb_id","year","sample_date", 
                  "bmi_taxa", "diatom_taxa", "csci", "d18", "h20", "s2","asci_hyb","asci_diatom",
                  "do_mg_l","ph","alkalinity_mg_l", "sp_cond_us_cm", "temp_c", # water quality
                  "tn_mg_l", "tp_mg_l", "uia_ug_l", # Nutrients 
                  "chloro_a_mg_m2", 'pct_macroalg_cvr', "filam_alg_cvr", "afdm_g_m2",# biomass indicators 
                  "ipi", "tot_phab", # Physical Habitat
                  "epifaun_substr", "sed_deposition", # PHAB components
                  "shannon_nst", "pct_smaller_sand", "pct_boulder_ls", "pct_fast_water",# physical habitat metric scores
                  "crhdi_swamp") 

full_names <- c("RMC Station ID","Water Board ID", "Water Year", "Sample Date",
                "Total BMI Taxa", "Total Diatom Taxa", "CSCI Score",'D18 IBI Score',"S2 IBI Score","H20 IBI Score","ASCI Hybrid Score", "ASCI Diatom Score",
                "Dissolved Oxygen (mg/L)", "pH", "Alkalinity (as CaCO3, mg/L)", "Conductivity (uS/cm)", "Temperature (C)",
                "Total Nitrogen (mg/L)", "Total Phosphorus (mg/L)", "Unionized Ammonia (ug/L)",
                "Chlorophyll a (mg/m2)", '% Macroalgae Cover', "Mean Filamentous Algae Cover (%)", "AFDM (g/m2)",
                "IPI Score", "Total PHAB",
                "Epifaunal Substrate", "Sediment Deposition",
                'Shannon Diversity (H) of Natural Substrate Types','% Substrate Smaller than Sand (<2 mm)', 'Percent Boulders - large & small', 'Percent Fast Water of Reach',
                "Human Disturbance Index")

param_names <- data.frame(fullname=c("Watershed","Creek",full_names,"Percent Impervious - Watershed", "Road Density - Watershed"), dataname=c("ws", "creek", select_param, "pct_imperv_ws", "road_dsty_ws")) # add all other parameters from site file


# subset bioassessment data
df_bio <- df_bio %>% filter(year %in% seq(2012,2018,1)) %>% dplyr::select(select_param) %>%
  arrange(rmc_id) %>%
  mutate(chloro_a_mg_m2 = as.numeric(chloro_a_mg_m2),
         epifaun_substr=as.numeric(epifaun_substr),
         sed_deposition = as.numeric(sed_deposition)) %>% 
  mutate(lat = sites$lat[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         long = sites$long[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         elevation_m = sites$elevation_m[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         drain_area_km2 = sites$drain_area_km2[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         ws = sites$watershed[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         creek= sites$creek[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         pct_imperv_ws=sites$pct_imperv_ws[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)],
         road_dsty_ws=sites$road_dsty_ws[which(!(is.na(sites$rmc_id)) & sites$rmc_id %in% rmc_id)]) %>%
  mutate(pct_imperv_ws =  pct_imperv_ws * 100)

# Variables that will be used in ui and server
bio_vars_yr <- unique(df_bio$year)
bio_vars_filter <- data.frame(param=c("csci", "asci_hyb", "asci_diatom", 's2',"d18", 'h20',"tot_phab"),
                          name= c("CSCI Score", "ASCI Hybrid Score", "ASCI Diatom Score","S2 Benthic Algae Score", "D18 Benthic Algae Score","H20 Benthic Algae Score", "Total PHAB"),
                          threshold1=c(0.63, -1, -1,29, 49,54, 15),
                          threshold2=c(0.795, -1, -1, 47, 62, 63, 30),
                          threshold3=c(0.92,-1, -1,60,72,70,46)) # NB: set threshold to -1 when no threshold 
bio_vars_ws <- sort(factor(unique(sheds$SYSTEM), levels=sort(unique(as.character(sheds$SYSTEM)))))

colors_bio <- c("green", "orange", "red","purple")





# B - Import and reshape POC data 
############################################################################################################################
############################################################################################################################

colors_PCB <- c("green", "orange", "red", "purple")
colors_Hg <- c('',"green", "orange", "red")

df_POC <- read_excel("POC_Sediment_Samples.xlsx", sheet="SC")
df_POC <- df_POC %>% 
  dplyr::select(1:6,8:9,11,14) %>% 
  mutate(pcb_conc_cat = factor(pcb_conc_cat, levels=c("<0.2 mg/kg","0.2 - 0.5 mg/kg","0.5 - 1.0 mg/kg",">1.0 mg/kg")),
         hg_conc_cat = factor(hg_conc_cat, levels=c("None","<0.3 mg/kg","0.3 - 1.0 mg/kg",">1.0 mg/kg"))) %>%
  mutate(pcb_col = colors_PCB[as.numeric(pcb_conc_cat)], hg_col = colors_Hg[as.numeric(hg_conc_cat)]) %>%
  mutate(hg_mg_kg = ifelse(hg_mg_kg > 0, hg_mg_kg,0)) %>% 
  filter(!is.na(samp_date)) %>% 
  mutate(year = year(samp_date))

poc_vars_yr <- as.numeric(unique(year(df_POC$samp_date)))
poc_vars_yr <- sort(poc_vars_yr)
unique(df_POC$site_id) # /!\ duplicate sites <- fix for point visualization 





# C - Import and reshape continuous water quality data 
###########################################################################################################################################
###########################################################################################################################################
sites_char <- read_excel("Cont Temp and WQ stations 2012_2018.xlsx", sheet="ALL")

df_wq <- read_excel("data_master_wq_2012_18.xlsx", sheet= "SC WQ ALL") %>%
  mutate(year = year(date)) %>%
  mutate(season = factor(ifelse((month(date) == 5 | month(date) == 6 | month(date) == 7), "S", "F"), levels=c("S", "F"))) %>% 
  mutate(ws = sites_char[match(site_id,sites_char$`Station Number`),"ws"]$ws) %>% 
  arrange(year)


df_temp <-  read_excel("data_master_wq_2012_18.xlsx", sheet= "SC ConTemp", col_types=c("text","text", "date", "text")) %>% 
  mutate(year =year(date)) %>% # make factor or ordered num 
  arrange(year) %>% 
  mutate(ws = sites_char[match(site_id,sites_char$`Station Number`),"ws"]$ws,
         ctemp_c =as.numeric(ctemp_c))


# data frame with unique site Id's for mapping 
all_sites_wq <- unique(c(unique(df_wq$site_id),unique(df_temp$site_id)))

sites_cWQ <- data.frame(site_id = all_sites_wq,
                        lat= sites_char[match(all_sites_wq,sites_char$`Station Number`),"lat"],
                        long= sites_char[match(all_sites_wq,sites_char$`Station Number`),"long"],
                        ws = sites_char[match(all_sites_wq,sites_char$`Station Number`),"ws"],
                        wq_TF = all_sites_wq %in% unique(df_wq$site_id),
                        ctemp_TF = all_sites_wq %in% unique(df_temp$site_id),
                        both_TF = all_sites_wq %in% unique(df_wq$site_id) & all_sites_wq %in% unique(df_temp$site_id)) %>% 
  mutate(marker_group = ifelse(wq_TF & ctemp_TF, 3, ifelse(ctemp_TF,2,1))) %>% 
  mutate(sampl_dates_wq = ifelse(marker_group == 1 | marker_group == 3, as.character(paste0(unique(df_wq[which(df_wq$site_id %in% site_id),"year"]),collapse=", ")),
                                 NA),
         sampl_dates_temp = ifelse(marker_group == 2 | marker_group == 3, paste0(unique(df_temp[which(df_temp$site_id %in% site_id),"year"]),collapse=", "),
                                   NA))

# parameters to be used for inputs 
wq_vars_yr <- as.numeric(unique(year(df_temp$date)))  
wq_vars_yr_2 <- as.numeric(unique(year(df_wq$date))) 
wq_vars_yr <- seq(min(c(wq_vars_yr, wq_vars_yr_2)), max(c(wq_vars_yr, wq_vars_yr_2)),1)

wq_vars_ws <-unique(sites_cWQ$ws)
wq_vars_sub_ws <- unique(sites_cWQ$creek)

# sub-ws groupings 
wq_GUA_1 <- factor(c('205GUA202', '205GUA205', '205GUA210', '205GUA213', '205GUA218', '205GUA229'))
wq_GUA_2 <- factor(c('205GUA190', '205GUA250','205GUA255','205GUA262', '205GUA225', '205GUA340', '205GUA270', '205GUA279'))
wq_STA <- factor(c('205SAR050', '205SAR060', '205SAR070', '205SAR075', '205SAR085'))
wq_STE <- factor(c('205STE064','205STE065', '205STE070', '205STE071', '205STE095', '205STE105'))
wq_COY_1 <- sites_cWQ[sites_cWQ$ws=="Coyote Creek","site_id"]
wq_COY_2 <- factor(wq_COY_1[c(3,2,8,4,1)])
wq_COY_1 <- factor(wq_COY_1[c(9,6,5,7,10,14,11,12,15,13)])

wq_sites <- unlist(lapply(X=list(wq_GUA_1, wq_GUA_2, wq_STA, wq_STE, wq_COY_1, wq_COY_2),as.character))
wq_sites <- factor(wq_sites)
df_temp <- df_temp %>% 
  mutate(site_id = factor(site_id, levels=wq_sites)) 
df_temp$plot_cat <- df_temp$site_id %in% wq_GUA_2 +1
df_wq <- df_wq %>% 
  mutate(site_id = factor(site_id, levels=wq_sites))
df_wq$plot_cat <- df_wq$site_id %in% wq_COY_2 +1

# colors for beanplots 
wq_colors <- rainbow(12)

# Important data 
MRP_threshold <- data.frame(param = c("temp_c", "ph","sp_cond_us_cm" ,"do_mg_l"),
                            label = c("Temperature (\u00B0C)", "pH", "Specific Conductivity ($\\mu$S/cm)", "Dissolved Oxygen (mg/L)"), 
                            value_inf = c(NA,6.5,NA,5), value_sup = c(24,8.5,2000,7), 
                            units = c("oC", "","uS/cm","mg/L"))
temp_thresholds <- data.frame(param=c("conTemp",'avDayTemp', "maxDayTemp", "avWeek", "maxWeek"), 
                              thresh=c(24,17,20,17,21)) # CHECK THRESHOLD VALUES

colors_temp <- colorRampPalette(c("blue","seagreen", "orange", "red"))(11)



# temperature calculations 
df_temp_MWAT <- df_temp %>%  
dplyr::mutate(day1week= as.Date(cut(date, "week"))) %>%
  dplyr::group_by(year,ws, plot_cat,site_id, day1week) %>% dplyr::summarize(avWeek= mean(ctemp_c))
df_temp_7DAVG <- df_temp %>% mutate(date =as.Date(date)) %>%
  dplyr::group_by( year,ws, plot_cat, site_id,date) %>% dplyr::summarize(avDayTemp= mean(ctemp_c)) %>% 
  mutate(grp=paste(year, site_id,sep=''))
  
  



# E - Customized functions that will be used in the app 
#############################################################################################################################################
#############################################################################################################################################

# Custom legend for leaflet maps function 
addLegendCustom <- function(map, position, colors, labels, sizes, shapes, borders, opacity = 0.5, title=NULL){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    n <- length(shapes)
    out <- vector(mode="numeric", length=n)
    for (i in 1:n){
      if (!shapes[i]=="triangle"){
        shapes[i] <- gsub("circle", "50%", shapes[i])
        shapes[i] <- gsub("square", "0%", shapes[i])
        out[i] <- paste0(colors[i], "; width:", sizes[i], "px; height:", sizes[i], "px; border:1px solid ", borders[i], "; border-radius:", shapes[i])
      } 
      
      else {out[i] <- paste0("display: block;",
                             "width: 0;
                             height: 0;
                             border-style: solid;
                             border-width: 0px ", 0.5*sizes[i],"px ", sizes[i],"px " ,0.5*sizes[i], "px;
                             border-color: transparent transparent ",colors[i]," transparent") }
      }
    return(out) 
      }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, position=position ,colors = legend_colors, labels = legend_labels, opacity = opacity, title=title))
    }

# Custom scatterplots
ggplotRegression <- function (fit) {
  
  lb1 <- paste(sep="", "R-squared =", signif(summary(fit)$r.squared, 4))
  lb2 <- paste(sep="", "p-value =",signif(summary(fit)$coef[2,4], 3))
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(col = "blue") + 
    stat_smooth(method = "lm", col = "grey40", se=F, linetype=2) +
    ggtitle(paste(lb1,", ", lb2, sep=""))
  
}

# custom shapes of markers for leaflet
addCustomMarkers <- function(map, data, lng, lat, shapes=c(21), bg=c("blue"), size=20, icon_group=1, ...){
  
  n = length(shapes)
  files = character(n)
  
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')
    png(f, width = 30, height = 30, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = shapes[i], bg=bg[i], cex = 30/8)
    dev.off()
    files[i] = f
  }
  iconFiles = files
  return(addMarkers(map=map, data=data, lng=lng, lat=lat, 
                    icon=~ icons(iconUrl = iconFiles[icon_group], iconWidth = size),...))
}


# add total count on boxplots 
give_tot <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}














##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################



