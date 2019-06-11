# EOA's website redesign 
# Data visualization app 
# Emilie Fons 
# Michaela Palmer


# Script 1 - Global Environment 


# set wd
# setwd("F:/SCVURPPP (SC)/SC94 (2018-19)/Website Re-design/R code for webapp")
# setwd("C:/Users/efons/eoa-webapp")


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
library(scales)
library(beanplot)
library(DT)
library(RColorBrewer)
library(htmltools)
library(reshape2)



#  - Import spatial files 
################################################################################################################################################

# watersheds delineation 
sheds <- readOGR("./shp/SBC_Sheds_SCC_Only_w_Alameda.shp", GDAL1_integer64_policy = F) %>%
  spTransform(CRS("+init=epsg:4326"))

# creeks 
# creeks <- readOGR("M:/Santa Clara/Hydrography/Surface Waters/Working/scvurppp_creeks.shp") %>%
# spTransform(CRS("+init=epsg:4326"))
# is there a file with a ws data field as well? 


# sites info 
sites <- read_excel("sites master file.xlsx", na=c("", "NA", "#N/A"),sheet="copy_05.2019") %>%
  arrange(rmc_id) %>% 
  dplyr::mutate(id=ifelse(historic=="Historical SCVURPPP", station_code, rmc_id))
# useful for biodata, chlorine 









# A - Import and reshape bioassessment data 
##############################################################################################################################

# upload bioassessment data
df_bio <- read_excel("data_master_bio_2003_18.xlsx", sheet="All",na=c("","NR", "Not Recorded", "NA", "-"))


# select data fields to be plotted and rename
select_param <- c("rmc_id","wb_id","assmnt_type","year","sample_date", 
                  "bmi_taxa", "diatom_taxa", "csci", "d18", "h20", "s2","asci_hyb","asci_diatom", "asci_soft_alg", "cram",
                  "do_mg_l","ph","alkalinity_mg_l", "sp_cond_us_cm", "temp_c", # water quality
                  "chloride_mg_l",  "tn_mg_l", "tp_mg_l", "uia_ug_l", # Water chemistry
                  "chloro_a_mg_m2", 'pct_macroalg_cvr', "filam_alg_cvr", "afdm_g_m2",# algal biomass indicators 
                  "tot_phab", # Physical Habitat
                  "epifaun_substr", "sed_deposition", # PHAB components
                  "shannon_nst", "pct_smaller_sand", "pct_boulder_ls", "pct_fast_water", "pct_slow_water",# physical habitat metric scores
                  "crhdi_swamp" ) # habitat   

full_names <- c("RMC Station ID","Water Board ID", "Assessment Type","Water Year", "Sample Date",
                "Total BMI Taxa", "Total Diatom Taxa", "CSCI Score",'D18 IBI Score',"S2 IBI Score","H20 IBI Score","ASCI Hybrid Score", "ASCI Diatom Score", "ASCI Soft Algae", "CRAM Score", 
                "Dissolved Oxygen (mg/L)", "pH", "Alkalinity (as CaCO3, mg/L)", "Conductivity (uS/cm)", "Temperature (C)", "Chloride (mg/L)", 
                "Total Nitrogen (mg/L)", "Total Phosphorus (mg/L)", "Unionized Ammonia (ug/L)",
                "Chlorophyll a (mg/m2)", '% Macroalgae Cover', "Mean Filamentous Algae Cover (%)", "AFDM (g/m2)",
                "Total PHAB",
                "Epifaunal Substrate", "Sediment Deposition",
                'Shannon Diversity (H) of Natural Substrate Types','% Substrate Smaller than Sand (<2 mm)', 'Percent Boulders - large & small', 'Percent Fast Water of Reach','Percent Slow Water of Reach',
                "Human Disturbance Index") # Habitat 

param_names <- data.frame(fullname=c("Watershed","Sub-Watershed","Creek", "Latitude", "Longitude", full_names,"Elevation (m)","Drainage Area (km2)","Percent Impervious - 5K", "Percent Urban - 5K", "Road Density - 5K"), dataname=c("ws","subws","creek","lat", "long", select_param, "elevation_m","drain_area_km2","pct_imperv_5k", "pct_urban_5k","road_dsty_5k")) # add all other parameters from site file


# subset bioassessment data
df_bio <- df_bio %>% dplyr::select(select_param) %>%
  arrange(rmc_id) %>%
  mutate(id = ifelse(assmnt_type=="Historical SCVURPPP", wb_id, rmc_id), 
        epifaun_substr=as.numeric(epifaun_substr),
         sed_deposition = as.numeric(sed_deposition)) %>% 
  left_join(dplyr::select(sites,
                          "id","lat", "long", "elevation_m", "drain_area_km2", 
                          "ws", "subws", "creek", "pct_imperv_5k", "road_dsty_5k", 
                          "pct_urban_5k"), by="id") %>%
  mutate(pct_imperv_5k =  pct_imperv_5k * 100,
         pct_urban_5k =  pct_urban_5k * 100) %>% 
  mutate(ws = factor(ws, levels=sort(unique(ws))),
         subws = factor(subws, levels=sort(unique(subws))))



# Variables that will be used in ui and server
bio_vars_yr <- c(min(df_bio$year),max(df_bio$year))
bio_vars_filter <- data.frame(param=c("csci", "asci_hyb", "asci_diatom", "asci_soft_alg", 's2',"d18", 'h20',"tot_phab", "cram"),
                          name= c("CSCI Score", "ASCI Hybrid Score", "ASCI Diatom Score", "ASCI Soft Algae","S2 Benthic Algae Score", "D18 Benthic Algae Score","H20 Benthic Algae Score", "Total PHAB", "CRAM Score"),
                          threshold0=rep(0,9),
                          threshold1=c(0.63, 0.7, 0.63, 0.68, 29, 49,54, 15, 63),
                          threshold2=c(0.79, 0.83, 0.8, 0.82, 47, 62, 63, 30,72),
                          threshold3=c(0.92, 0.93, 0.92, 0.93, 60,72, 70, 46,79)) # NB: set threshold to -1 when no threshold 
bio_score_list <- list("California Stream Condition Index (CSCI)"=c('Indicator: Benthic Macroinvertebrate'="csci"),
     "Algae Stream Condition Indices (ASCIs)"=c("Indicator: Soft Algae"="asci_soft_alg",
                                                "Indicator: Diatoms" = "asci_diatom",
                                                "Indicator: Diatoms-Soft Algae (Hybrid)" = "asci_hyb"),
     "Riparian Habitat Condition" = c("CRAM Score" = "cram"))
bio_vars_ws <- sort(factor(unique(sheds$SYSTEM), levels=sort(unique(as.character(sheds$SYSTEM)))))

colors_bio <- c(rgb(163,224,129,maxColorValue = 255),rgb(254,255,117,maxColorValue = 255),
                rgb(255,127,125,maxColorValue = 255),rgb(224,114,254,maxColorValue = 255))
names(colors_bio) <- c("Likely Intact", "Possibly Intact", "Likely Altered", "Very Likely Altered")

# subwatershed groups list
subws <- lapply(bio_vars_ws, function(x) (unique(sites[sites$ws == x & !is.na(sites$ws), "subws"]))$subws)
names(subws) <- bio_vars_ws


##### 



# B - Import and reshape POC data 
############################################################################################################################

df_POC <- read_excel("POC_Sediment_Samples.xlsx", sheet="SC") %>% 
  dplyr::select(1:9) %>% 
  mutate(pcb_conc_cat = cut(pcbs_mg_kg, breaks=c(0,0.2,0.5,1,Inf), right=F, labels=c("<0.2 mg/kg","0.2 - 0.5 mg/kg","0.5 - 1.0 mg/kg",">1.0 mg/kg")),
         hg_conc_cat = cut(hg_mg_kg, breaks=c(-Inf,0, 0.3,1,Inf), right=F, labels=c("None","<0.3 mg/kg","0.3 - 1.0 mg/kg",">1.0 mg/kg"))) %>%
  #mutate(hg_mg_kg = ifelse(hg_mg_kg > 0, hg_mg_kg,0)) %>% 
  filter(!is.na(samp_date)) %>% 
  mutate(year = year(samp_date))

poc_vars_yr <- sort(as.numeric(unique(year(df_POC$samp_date))))
length(unique(df_POC$site_id)) # /!\ duplicate sites <- fix for point visualization 


colors_PCB <- colors_bio
names(colors_PCB) <- levels(df_POC$pcb_conc_cat)
colors_Hg <- c('grey',colors_bio[1:3])
names(colors_Hg) <- levels(df_POC$hg_conc_cat)





#####



# C - Import and reshape continuous water quality data 
###########################################################################################################################################

# import seasonal water quality data
df_wq <- read_excel("data_master_wq_2012_18.xlsx", sheet= "SC WQ ALL") %>%
  mutate(year = year(date)) %>%
  mutate(season = factor(ifelse((month(date) == 5 | month(date) == 6 | month(date) == 7), "S", "F"), levels=c("S", "F"))) %>% 
  left_join(dplyr::select(sites, "station_code", "ws", "creek"), by=c("site_id"="station_code")) %>% 
  mutate(temp_c = ifelse(str_detect(comment, "malfunction"), NA, temp_c),
         ph = ifelse(str_detect(comment, "malfunction") | str_detect(comment, "pH"), NA, ph),
         sp_cond_us_cm = ifelse(str_detect(comment, "malfunction") | str_detect(comment, "SpCond"), NA, sp_cond_us_cm),
         do_mg_l = ifelse(str_detect(comment, "malfunction") | str_detect(comment, "DO"), NA, do_mg_l),
         do_pct = ifelse(str_detect(comment, "malfunction") | str_detect(comment, "DO"), NA, do_pct)) %>% 
  arrange(year)

# import continuous temperature data
df_temp <-  read_excel("data_master_wq_2012_18.xlsx", sheet= "SC ConTemp", col_types=c("text","text", "date", "text")) %>% 
  mutate(year =year(date)) %>% # make factor or ordered num 
  arrange(year) %>% 
  left_join(dplyr::select(sites, "station_code", "ws", "creek"), by=c("site_id"="station_code")) %>% 
  dplyr::mutate(ctemp_c =as.numeric(ctemp_c))


# data frame with unique site Id's for mapping 
all_sites_wq <- unique(c(unique(df_wq$site_id),unique(df_temp$site_id)))

sites_cWQ <- data.frame(site_id = all_sites_wq) %>% 
  left_join(dplyr::distinct(dplyr::select(sites, "station_code","lat", "long", "ws", "creek"), station_code, .keep_all=T), by=c("site_id"="station_code")) %>% 
  dplyr::mutate(wq_TF = all_sites_wq %in% unique(df_wq$site_id),
                ctemp_TF = all_sites_wq %in% unique(df_temp$site_id), 
                both_TF = all_sites_wq %in% unique(df_wq$site_id) & all_sites_wq %in% unique(df_temp$site_id)) %>% 
  dplyr::rename("sub_ws" = "creek")

# parameters to be used for inputs 
wq_vars_yr <- as.numeric(unique(year(df_temp$date)))  
wq_vars_date <- 
  c(as.Date(paste(
    min(wq_vars_yr), "-01-01", sep = ""
  )), as.Date(paste(
    max(wq_vars_yr), "-12-31", sep = ""
  )))
wq_vars_yr_2 <- as.numeric(unique(year(df_wq$date))) 
wq_vars_yr <- seq(min(c(wq_vars_yr, wq_vars_yr_2)), max(c(wq_vars_yr, wq_vars_yr_2)),1)

wq_vars_ws <-unique(sites_cWQ$ws)
wq_vars_sub_ws <- unique(sites_cWQ$creek)

# sub-ws groupings 
wq_GUA_1 <- factor(c('205GUA202', '205GUA205', '205GUA210', '205GUA213', '205GUA218', '205GUA229'))
wq_GUA_2 <- factor(c('205GUA190', '205GUA250','205GUA255','205GUA262', '205GUA225', '205GUA340', '205GUA270', '205GUA279'))
wq_STA <- factor(c('205SAR050', '205SAR060', '205SAR070', '205SAR075', '205SAR085'))
wq_STE <- factor(c('205STE064','205STE065', '205STE070', '205STE071', '205STE095', '205STE105'))
wq_COY_1 <- sites_cWQ[sites_cWQ$ws=="Coyote","site_id"]
wq_COY_2 <- factor(wq_COY_1[c(3,2,8,4,1)])
wq_COY_1 <- factor(wq_COY_1[c(9,6,5,7,10,14,11,12,15,13)]) # TO DO: write site ids instead

wq_sites <- unlist(lapply(X=list(wq_GUA_1, wq_GUA_2, wq_STA, wq_STE, wq_COY_1, wq_COY_2),as.character))
wq_sites <- factor(wq_sites)
df_temp <- df_temp %>% 
  mutate(site_id = factor(site_id, levels=wq_sites)) 
df_temp$plot_cat <- df_temp$site_id %in% wq_GUA_2 +1
df_wq <- df_wq %>% 
  mutate(site_id = factor(site_id, levels=wq_sites))
df_wq$plot_cat <- df_wq$site_id %in% wq_COY_2 +1



# Important data 
MRP_threshold <- data.frame(param = c("temp_c", "ph","sp_cond_us_cm" ,"do_mg_l"),
                            label = c("Temperature (\u00B0C)", "pH", "Specific Conductivity ($\\mu$S/cm)", "Dissolved Oxygen (mg/L)"), 
                            value_inf = c(-1,6.5,-1,7), value_sup = c(24,8.5,2000,1000), 
                            units = c("oC", "","uS/cm","mg/L"),
                            lim_sup = c(30,10,2500,12.5))
temp_thresholds <- data.frame(param=c("conTemp",'avDayTemp', "maxDayTemp", "avWeek", "maxWeek"), 
                              thresh=c(24,17,20,17,21)) # CHECK THRESHOLD VALUES

colors_temp <- colorRampPalette(c("yellow", "orange", "red", "purple"))(11)
colors_wq <- colorRampPalette(c("blue","green", "orange", "red"))(11)



# temperature calculations 
df_temp_MWAT <- df_temp %>%  
dplyr::mutate(day1week= as.Date(cut(date, "week"))) %>%
  dplyr::group_by(year,ws, plot_cat,creek, site_id, day1week) %>% dplyr::summarize(avWeek= mean(ctemp_c))
df_temp_7DAVG <- df_temp %>% mutate(date =as.Date(date)) %>%
  dplyr::group_by( year,ws, plot_cat,creek, site_id,date) %>% dplyr::summarize(avDayTemp= mean(ctemp_c)) %>% 
  mutate(grp=paste(year, site_id,sep=''))
  
  


#####




# D - Creek trash data 
#############################################################################################################################################
#############################################################################################################################################

# Upload data

sites_trash <- read_excel("creek_trash.xlsx", sheet= "Sites", skip = 1) %>%
  dplyr::filter(agency=="SCVURPPP", 
                siteType == "Targeted") 

df_trash <- read_excel(path="creek_trash.xlsx", sheet= "Target_Assess", skip=1) %>% 
  dplyr::filter(agency=="SCVURPPP") %>%
  mutate(sum_pct=litter_wind+illegal_camp+dumping+other,
         sum_bank_cvr=grasses+bushes+trees+open+armored,
         sum_chann_cvr=wood+aqVeg+open__1+dry,
         trashCat= factor(trashCat, levels=c("Low", "Moderate", "High", "Very High")),
         juris=sites_trash[match(siteID,sites_trash$siteID),"juris"]$juris) %>%
  dplyr::filter(sum_pct==100,
                sum_bank_cvr==100, 
                sum_chann_cvr == 100) %>% 
  as.data.frame()

# Variables that will be used in ui 
trash_vars_pathways <- c("Litter Wind", "Illegal Camp", "Dumping", "Other")

trash_vars_bank_cvr <- c("Grasses", "Bushes", "Trees", "Open", "Armored")
trash_vars_chann_cvr <- c("Wood", "AqVeg", "Open", "Dry")
trash_vars_city <- unique(sites_trash$juris) 
col_pathways <- brewer.pal(4,"Paired")
col_trashCat <-  c(rgb(166,219,160,maxColorValue = 255), 
                   rgb(254,235,160,maxColorValue = 255), 
                   rgb(255,109,70,maxColorValue = 255),
                   rgb(118,42,131,maxColorValue = 255))





# E - Chlorine data 
#############################################################################################################################################

# Upload data

# monitoring data
df_chlo <- read_excel("All_chlorine_results_2012-2018.xlsx", sheet="SC") %>% 
  dplyr::mutate(Date=as.Date(Date, format="%m/%d/%Y"), 
                AnalyteName=factor(AnalyteName),
                year=year(Date),
                fy = ifelse(month(Date)<=7, paste(year(Date)-1,"-",year(Date)),paste(year(Date),"-",year(Date+1))))
df_chlo_aggreg <- df_chlo %>%
  dplyr::filter(FieldReplicate == 1,
                QACode == "None", 
                !is.na(Result)) %>%  # all duplicate results in a year are deleted for now
  # for later: keep duplicate when triggered by exceedance and show conc as well in popup
  dplyr::arrange(Date) %>%
  dplyr::group_by(StationName, AnalyteName) %>% 
  dplyr::summarize(n=n(),
                  date_1 = first(Date),
                  date_2 = nth(x=Date, n=2, default=NA), 
                  date_3 = nth(x=Date, n=3, default=NA),
                  result_1 = first(Result),
                  result_2 = nth(x=Result, n=2, default=NA),
                  result_3 = nth(x=Result, n=3, default=NA),
                  qa_1 = first(ResQualCode),
                  qa_2 = nth(x=ResQualCode, n=2, default=NA), 
                  qa_3 = nth(x=ResQualCode, n=3, default=NA))


# site data
sites_chlo <- sites %>% 
  dplyr::filter(station_code %in% df_chlo_aggreg$StationName) %>%
  dplyr::distinct(station_code, .keep_all=T)
sites_chlo <- merge(sites_chlo[,c("station_code","long","lat","ws", "subws", "creek")], df_chlo_aggreg[df_chlo_aggreg$AnalyteName == "Chlorine, Free",c(1,4:12)], by.x="station_code", by.y="StationName") %>% 
  dplyr::rename(free_chlo_1 = result_1, free_chlo_2 = result_2, free_chlo_3 = result_3,
                free_chlo_qa_1 = qa_1,free_chlo_qa_2 = qa_2,free_chlo_qa_3 = qa_3)
sites_chlo <- merge(sites_chlo, df_chlo_aggreg[df_chlo_aggreg$AnalyteName == "Chlorine, Total Residual",c(1,7:12)], by.x="station_code", by.y="StationName") %>%
  dplyr::rename(tot_chlo_1 = result_1, tot_chlo_2 = result_2,tot_chlo_3 = result_3,  
                tot_chlo_qa_1 = qa_1,tot_chlo_qa_2 = qa_2,tot_chlo_qa_3 = qa_3)
sites_chlo <- sites_chlo %>% 
  dplyr::mutate(year=year(date_1))

df_chlo <- df_chlo %>% 
  dplyr::left_join(select(sites_chlo, "station_code", "long", "lat", "ws", "subws", "creek"), 
                   by=c("StationName"= "station_code"))

# variables used in ui 
chlo_vars_yr <- seq(min(sites_chlo$year), max(sites_chlo$year))





# F - Pesticide data
#############################################################################################################################################
#############################################################################################################################################


# sites data
sites_tox <-  read_excel("data_pesticides_2012_18.xlsx" , sheet="Site Info") %>% 
  dplyr::filter(!is.na(TargetLongitude)) %>% 
  dplyr::mutate(TargetLongitude = as.numeric(TargetLongitude),
                TargetLatitude = as.numeric(TargetLatitude))

# Toxicity Data
df_tox <- read_excel("data_pesticides_2012_18.xlsx" , sheet="ToxSummary") %>% 
  dplyr::mutate(season = ifelse(month(SampleDate)<5, "W", "D"),
                year= year(SampleDate),
                organism_u = paste0(OrganismName, " (", AnalyteName,")"),
                PercentEffect = signif(PercentEffect, 2))

events_each_year <- df_tox %>% 
  dplyr::arrange(SampleDate) %>% 
  dplyr::group_by(year, season, StationCode) %>% 
  dplyr::summarise(date_event=dplyr::first(SampleDate),
                   n=n())

df_tox$main_events <- sapply(seq(1:nrow(df_tox)), 
                             function(x) as.Date(events_each_year$date_event[which(events_each_year$year == df_tox$year[x] & 
                                                                                     events_each_year$season == df_tox$season[x] & 
                                                                                     events_each_year$StationCode == df_tox$StationCode[x])]))
df_tox <- df_tox %>% 
  dplyr::mutate(
    main_events=as.Date(main_events, origin="1970-01-01"),
    sampleType = ifelse(month(SampleDate) == month(main_events), "First", "Follow-up")) %>% 
  as.data.frame()



# Sediment pesticides data (DRY season) 
df_sedPest <- read_excel("data_pesticides_2012_18.xlsx" , sheet="SedPesticides") %>% 
  dplyr::mutate(year=year(OrigSampleDate)) %>% 
  as.data.frame()
ref_sedPest <- read_excel("data_pesticides_2012_18.xlsx" , sheet="ToxRefInfo")

df_sedPest_metals <- df_sedPest %>% 
  dplyr::filter(AnalyteName %in% ref_sedPest$AnalyteName) %>% 
  dplyr::mutate(AnalyteCat = ref_sedPest$AnalyteCat[match(AnalyteName, ref_sedPest$AnalyteName)],
                RefValue = ref_sedPest$RefValue[match(AnalyteName, ref_sedPest$AnalyteName)]) %>% 
  dplyr::filter(AnalyteCat %in% c("Metal")) %>%
  # for now
  dplyr::mutate(conc_quotient = Result / RefValue,
                trigger = (Result/RefValue > 1))

df_sedPest_pyre <- df_sedPest %>% 
  dplyr::mutate(AnalyteName = ifelse(AnalyteName == "Cyhalothrin, lambda, total", "Cyhalothrin, Total lambda-", AnalyteName),
                Result = ifelse(Result<0, abs(Result)/2, Result)) %>% 
  dplyr::filter(AnalyteName %in% c(ref_sedPest$AnalyteName[ref_sedPest$AnalyteCat == "Pyrethroid"],"Fipronil")) %>% 
  dplyr::mutate(AnalyteCat = ref_sedPest$AnalyteCat[match(AnalyteName, ref_sedPest$AnalyteName)],
                RefValue = ref_sedPest$RefValue[match(AnalyteName, ref_sedPest$AnalyteName)])

df_sedPest_pyre$conc_quotient <- sapply(1:nrow(df_sedPest_pyre), function(x) 
  df_sedPest_pyre$Result[x]*100/(1000*df_sedPest_pyre$RefValue[x]*mean(df_sedPest$Result[df_sedPest$OrigStationCode == df_sedPest_pyre$OrigStationCode[x] & df_sedPest$year == df_sedPest_pyre$year[x] & df_sedPest$AnalyteName == "Total Organic Carbon"])))
df_sedPest_pyre <- df_sedPest_pyre %>% 
  dplyr::group_by(OrigStationCode,year,AnalyteCat) %>% 
  dplyr::summarise(conc_quotient = sum(conc_quotient)) %>% 
  dplyr::rename(AnalyteName = AnalyteCat) %>% 
  dplyr::mutate(trigger = conc_quotient > 1) %>% 
  as.data.frame()


pahs <- c("Acenaphthene","Acenaphthylene","Anthracene","Benz(a)anthracene","Benzo(a)pyrene","Benzo(b)fluoranthene",
          "Benzo(e)pyrene","Benzo(g,h,i)perylene","Benzo(k)fluoranthene" ,"Biphenyl",
          "Chlordane, cis-" , "Chlordane, trans-" , "Chloroxuron(Surrogate)" , 
          "Chrysene", "Dibenz(a,h)anthracene", 
          "Decachlorobiphenyl(Surrogate)", "Dibenzothiophene", "Dimethylnaphthalene, 2,6-","Fluoranthene", "Fluorene" ,"Fluorobiphenyl, 2-(Surrogate)",
          "Indeno(1,2,3-c,d)pyrene","Methylnaphthalene, 1-" , "Methylnaphthalene, 2-",  "Methylphenanthrene, 1-" ,
          "Naphthalene" , "Nitrobenzene-d5(Surrogate)", "Perylene" ,  "Phenanthrene", "Pyrene", "Terphenyl-d14(Surrogate)" ,
          "Tetrachloro-m-xylene(Surrogate)")

df_sedPest_pah <- df_sedPest %>% 
  dplyr::filter(AnalyteName %in% pahs) %>% 
  dplyr::mutate(AnalyteCat = "Total PAHs",
                RefValue = ref_sedPest$RefValue[match("Total PAHs", ref_sedPest$AnalyteName)],
                Result = ifelse(Result<0, abs(Result)/2*DilFactor, Result)) %>% 
  dplyr::group_by(OrigStationCode, year, AnalyteCat) %>%
  dplyr::summarise(conc = sum(Result),
                   conc_quotient = sum(Result)/first(RefValue))





df_sedPest <- rbind(df_sedPest_metals %>% 
                      dplyr::select(c(3,17,7,20,21)), df_sedPest_pyre)


# for app
tox_vars_yr <- seq(min(df_tox$year), max(df_tox$year))
colors_tox <- c("blue", "orange", "red", "purple")
tox_vars_stressors_dry <- sort(unique(df_sedPest$AnalyteName))
colors_chem <- c("blue", "red")




# G - Upload pathogen data 
#############################################################################################################################################
#############################################################################################################################################

patho_threshold <- data.frame(Analyte=c("Coliform, Fecal", "Enterococcus", "E. coli"), 
                              threshold=c(400,130,410))
df_patho <- read_excel("pathogens_2012_18.xlsx") %>%  
  dplyr::filter(!Analyte == "Coliform, Total")%>%
  dplyr::mutate(Date=as.Date(Date), 
                year=year(Date), 
                Analyte=factor(Analyte), 
                Latitude = ifelse(is.na(Latitude), sites$lat[match(Station_Code, sites$station_code)],Latitude),
                Longitude = ifelse(is.na(Longitude), sites$long[match(Station_Code, sites$station_code)],Longitude),
                exceedance = Result > (patho_threshold$threshold[match(Analyte, patho_threshold$Analyte)])
  )

patho_vars_analyte <- sort(unique(df_patho$Analyte))
patho_col <- c("green","red")






# H - Customized functions that will be used in the app 
#############################################################################################################################################
#############################################################################################################################################

# Custom legend for leaflet maps function 
addLegendCustom <- function(map, position, colors=c("blue"), labels, sizes=c(10), shapes=c("circle"), opacity = 0.5, title=NULL){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    n <- length(shapes)
    out <- vector(mode="numeric", length=n)
    for (i in 1:n){
      if (!shapes[i]=="triangle"){
        shapes[i] <- gsub("circle", "50%", shapes[i])
        shapes[i] <- gsub("square", "0%", shapes[i])
        out[i] <- paste0(colors[i], "; width:", sizes[i], "px; height:", sizes[i], "px; border:1px solid ", "; border-radius:", shapes[i])
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
ggplotRegression <- function (x,y) {
  
  fit <- lm(y~x)
  lb1 <- paste(sep="", "R-squared = ", signif(summary(fit)$r.squared, 4))
  p_val_2 <- signif(summary(fit)$coef[2,4], 3)
  lb2 <- ifelse(p_val_2 <0.001,
                paste(sep="", "p-value < 0.001"), paste(sep="", "p-value =", p_val_2))
  lb3 <- paste(sep="", "Rho = ",signif(cor.test(x=x,y=y,method="spearman",use="complete.obs", exact = F)$estimate,4))
  p_val_4 <- signif(cor.test(x=x,y=y,method="spearman",use="complete.obs", exact=F)$p.value,3)
  lb4 <- ifelse(p_val_4 <0.001,
    paste(sep="", "p-value < 0.001"), paste(sep="", "p-value =", p_val_4))
  
  
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(col = "blue") + 
    stat_smooth(method = "lm", col = "grey40", se=F, linetype=2) +
    ggtitle(paste(paste("Pearson: ",lb1,", ", lb2, sep=""),
                  paste("Spearman: ",lb3,', ',lb4, sep=''), sep="\n"))
  
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


# Detect when user presses escape 
js <- '
$(document).on("keyup", function(e) {
if(e.keyCode == 27){
Shiny.onInputChange("keyPressed", Math.random());
}
});
'


# Shading under curve
shade_curve <- function(data=sites_chlo, zlimit, sign=">", fill = "red", alpha = .5){
  if (sign==">"){geom_area(data = subset(data, tot_chlo_1 >= zlimit),
            aes(y=y), fill = fill, color = NA, alpha = alpha)}
    else geom_area(data = subset(data, tot_chlo_1 < zlimit),
                  aes(y=y), fill = fill, color = NA, alpha = alpha)
}








##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################



