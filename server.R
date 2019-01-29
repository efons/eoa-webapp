
# EOA's website redesign 
# Data visualization app 
# Emilie Fons 
# Michaela Palmer


# Script 3 - Server function 






# Server function
server <- (function(input, output, session) {
  
  ## Bioassessment data
  ###################################################################################################################
  
  # map parameter : size filter by stressor variable 
  observe({updateSelectizeInput(session, 'size_by', 
                                choices = list(
                                                "Habitat"=c("Total PHAB" = "tot_phab",
                                                                                               "Epifaunal Substrate"="epifaun_substr",
                                                                                               "Sediment Deposition" = "sed_deposition",
                                                                                               "Shannon Diversity (Natural Substrates)" = "shannon_nst",
                                                                                               "% Substrate Smaller than Sand (<2 mm)" = "pct_smaller_sand",
                                                                                               "Percent Boulders - large & small" = "pct_boulder_ls",
                                                                                               "Percent Fast Water of Reach"="pct_fast_water",
                                                                                               "IPI Score" = "ipi",
                                                                                              "% Impervious Area - Watershed"="pct_imperv_ws",
                                                                                               "Road density - Watershed" = "road_dsty_ws"),
                                                "Biomass" = c("Chlorophyll a (mg/m2)" = "chloro_a_mg_m2",
                                                                                                 "AFDM (g/m2)" = "afdm_g_m2",
                                                                                                 "% Macroalgae Cover" = "pct_macroalg_cvr"),
                                                 "Nutrients"= c("Total Nitrogen (mg/L)" = "tn_mg_l",
                                                                                                  "Total Phosphorus(mg/L)" = "tp_mg_l",
                                                                                                  "Unionized Ammonia (ug/L)" = "uia_ug_l"),
                                                 "Water Quality" = c("Temperature (C)"="temp_c",
                                                                                                       "Dissolved Oxygen"="do_mg_l",
                                                                                                       "Conductivity (uS/cm)" = "sp_cond_us_cm"),
                                                 "Other" = c("Human Disturbance Index (HDI)" = "crhdi_swamp"))
                                , selected = 'tot_phab')
  })
  
  # Spatial Filter: county vs. watersheds 
  output$scnd_sub_ws <- renderUI({ 
    if (input$spatial_filter == "sub_ws"){
      
      pickerInput(inputId="ws", label="Choose Watershed", 
                  choices=as.character(bio_vars_ws), selected=as.character(bio_vars_ws), 
                  options = list(`actions-box` = TRUE, size = 20), multiple=T)
    }
    else NULL
  })
  
  
  # reactive function that returns the subsetted data table 
  data_sub <- reactive({
    
    req(!is.null(input$size_by))
    
    filter_by <- input$filter_by
    threshold1 <- bio_vars_filter$threshold1[which(bio_vars_filter$param == filter_by)]
    threshold2 <- bio_vars_filter$threshold2[which(bio_vars_filter$param == filter_by)]
    threshold3 <- bio_vars_filter$threshold3[which(bio_vars_filter$param == filter_by)]
    filter_name <- as.character(bio_vars_filter$name[which(bio_vars_filter$param == filter_by)])
    size_by <- input$size_by
    size_name <- as.character(param_names$fullname[which(param_names$dataname == size_by)])
    
    data_sub <- df_bio %>% filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      filter(if (input$spatial_filter == "sub_ws") {ws %in% input$ws} else {ws %in% bio_vars_ws} )%>%
      mutate(year=factor(year, levels=seq(min(bio_vars_yr),max(bio_vars_yr),1))) %>%
      dplyr::select(c("rmc_id","ws", "creek","year", filter_by, size_by))  %>%
      arrange(desc(year),ws) %T>%
      {names(.)<- c("RMC ID","Watershed","Creek", "Year", filter_name, size_name) } 
    
    
    # color cells for likely altered site - add new column (values stored as characters)
    
    if (nrow(data_sub) > 0) {
      
      data_sub <- data_sub %>%
        mutate(filter_by_chr = NA ) %>% # Makes unknown column'' error appear 
        select(c(1,2,3,4,5,7,6)) %>%
        rename_at(6, ~paste(filter_name," ", sep=''))
      
      
      
      for (i in 1:nrow(data_sub)) {
        if(!is.na(data_sub[i,5])){
          if (data_sub[i,5] <= threshold1)  {
            data_sub[i, 6] <- paste('<div style="width: 100%; height: 100%; z-index: 0; color:', colors_bio[4],'; position:absolute; top: 0; left: 0; padding:5px;"><span>',signif(data_sub[i,5],2),'</span></div>')
          }
          else {if (data_sub[i,5]< threshold2){
            data_sub[i, 6] <- paste('<div style="width: 100%; height: 100%; z-index: 0; color: ', colors_bio[3],'; position:absolute; top: 0; left: 0; padding:5px;"><span>',signif(data_sub[i,5],2),'</span></div>')
          }
            else {if (data_sub[i,5] < threshold3) {
              data_sub[i, 6] <- paste('<div style="width: 100%; height: 100%; z-index: 0; color: ', colors_bio[2],'; position:absolute; top: 0; left: 0; padding:5px;"><span>',signif(data_sub[i,5],2),'</span></div>')
            }
              else {if (threshold3>=0) {
                data_sub[i, 6] <- paste('<div style="width: 100%; height: 100%; z-index: 0; color: ', colors_bio[1],'; position:absolute; top: 0; left: 0; padding:5px;"><span>',signif(data_sub[i,5],2),'</span></div>')
              }
                else  {data_sub[i, 6] <- paste('<div style="width: 100%; height: 100%; z-index: 0; color: black; position:absolute; top: 0; left: 0; padding:5px;"><span>',signif(data_sub[i,5],2),'</span></div>')}
                
              }}}}}
    }
    
    return(as.data.frame(data_sub))
    
    
  })
  
  
  # List of selected watersheds
  ws_list <- reactive({
    ifelse(input$spatial_filter == "sub_ws",
           paste("Selected watersheds: ", paste(input$ws, collapse=", "), sep=""),
           "All watersheds")
  })
  
  output$ws_list_1 <- renderText({ws_list()})
  output$ws_list_2 <- renderText({ws_list()})
  
  
  
  # Data Table 
  # data table title 
  output$score_tables <- renderText({
    ifelse(!input$size_by == "none",
           paste(bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)]," and ",param_names[which(param_names$dataname == input$size_by),"fullname"], " for ", input$wy[1], " - ", input$wy[2], sep=""),
           paste(bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)], " for ", input$wy[1], " - ", input$wy[2], sep="")
    )
    
  })
  
  
  # Error message when data set is empty 
  output$cond_table <- renderUI({
    if(nrow(data_sub()) == 0)
    {h5("No data to show (no samples for this watershed/time period OR no parameter selected in map parameters)")}
    else {tableOutput("score_table")}
  })
  
  
  # create Table when data set not empty 
  output$score_table <- renderTable({
    t<-data_sub() 
    return(t[,c(1,2,3,4,6,7)])
   
    
  }, bordered=T, align='c', hover=T, sanitize.text.function = function(x) x)
  # error: unknomw column ""
  
  
  # Scatter Plots 
  
  output$scatterplots <- renderText({
    ifelse(input$spatial_filter == "sub_ws",
           paste("Scatterplots for ", input$wy[1]," - ", input$wy[2], sep=""),
           paste("Scatterplots for ", input$wy[1]," - ", input$wy[2], sep="")
    )
  })
  
  
  output$cond_scatter <- renderUI({
    if (nrow(data_sub())<=1 || ncol(data_sub())<7 || sum(!is.na(data_sub()[,ncol(data_sub())]))<=1){
      h5("No data to show: Make sure you selected a stressor variable and there are at least two assessment events for the selected time period/watersheds")
    }
    else {plotOutput("scatter1")}
  })
  
  output$scatter1 <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
    x_var <- data_sub_plots[,7]
    y_var <- data_sub_plots[,5]
    p <- ggplotRegression(lm(y_var ~ x_var)) +
      xlab(colnames(data_sub_plots)[7]) + ylab(colnames(data_sub_plots)[5])
    return(p)
  }, height=300, width=400)
  
  
  output$boxplots <- renderText({
    ifelse(input$spatial_filter == "sub_ws",
           paste("Timeseries for ", input$wy[1]," - ", input$wy[2], sep=""),
           paste("Timeseries for ", input$wy[1]," - ", input$wy[2], sep="")
    )
  })
  
  output$cond_boxplot <- renderUI({
    if(nrow(data_sub()) == 0)
    {h5("No data to show (no samples for this watershed/time period OR no parameter selected in map parameters)")}
    else {plotOutput("boxplot")}
  })
  
  bio_boxplot <- function(data_sub_plots, var_nb=7, threshold=-1){
    if (nrow(data_sub_plots)>0 ){
      x_var <- data_sub_plots[,4]
      y_var <- data_sub_plots[,var_nb]
      df_tempo <- data.frame(x_var=x_var,y_var=y_var) 
      p <- ggplot(df_tempo,aes(x=x_var,y=y_var)) + geom_boxplot(col=rgb(0,0,1,0.6)) + 
        xlab(colnames(data_sub_plots[4])) + ylab(colnames(data_sub_plots[var_nb])) +
        stat_summary(fun.data = give_tot, geom="text", fun.y=median, position=position_dodge(width=0.75))
      
      if (threshold >= 0){
        p <- p+ geom_hline(yintercept = threshold, linetype = 2, col="seagreen")
      }
      return(p)
    }
    else return(NULL)
  }
  
  output$boxplot <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
    threshold <- bio_vars_filter[match(colnames(data_sub_plots[5]),bio_vars_filter$name), "threshold1"]
    return(bio_boxplot(data_sub_plots=data_sub_plots, var_nb=5, threshold=threshold))
  }, height=300, width=400)
  
  
  
  output$boxplot2 <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
    if (ncol(data_sub_plots)==7){
      return(bio_boxplot(data_sub_plots=data_sub_plots, var_nb=7))
    }
    else return(NULL)
  }, height=300, width=400)
  
  
  
  # Map title 
  output$map_title <- renderText({
    filter_name <- bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)]
    paste("Map of ", filter_name, "for ", input$wy[1], " - ", input$wy[2], "(Change in map parameters)")
  })
  
  # Create baseline map  
  output$map_sites <- renderLeaflet({
    
    
    leaflet(options = leafletOptions(zoomControl = F,
                                     attributionControl=F)) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      setView(lng = -121.8, lat = 37.3, zoom = 9) %>%
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "markers", zIndex = 420)
  })
  
  
  
  # Update map based on user input 
  # reset view button 
  observe({
    input$reset_button
    leafletProxy("map_sites") %>% setView(lat = 37.3, lng = -121.8, zoom = 9)
  })
  
  observe({
    req(input$menu_items == "bio_data") 
    
    # other inputs
    filter_by <- input$filter_by
    threshold1 <- bio_vars_filter$threshold1[which(bio_vars_filter$param == filter_by)]
    threshold2 <- bio_vars_filter$threshold2[which(bio_vars_filter$param == filter_by)]
    threshold3 <- bio_vars_filter$threshold3[which(bio_vars_filter$param == filter_by)]
    filter_name <- bio_vars_filter$name[which(bio_vars_filter$param == filter_by)]
    size_by <- input$size_by
    
    # subset data based on user input 
    data_sub <- df_bio %>% filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      filter(if (input$spatial_filter == "sub_ws") {ws %in% input$ws} else {ws %in% bio_vars_ws})
    data_sub <- cbind(data_sub, data_sub[,which(colnames(data_sub)==filter_by)])
    colnames(data_sub)[ncol(data_sub)] <- "filter"
    if (!(size_by == "none")){
      data_sub <- cbind(data_sub, data_sub[,which(colnames(data_sub)==size_by)])
      colnames(data_sub)[ncol(data_sub)] <- "size"
    }
    
    # subset watersheds shapefile based on user input 
    if (input$spatial_filter == "sub_ws"){
      sheds_sub <- sheds[sheds$SYSTEM %in% input$ws,]
    }
    else {sheds_sub <- sheds}
    
    
    
    # pop-up window for individual sites 
    content <- paste(
      sep = "<br/>",
      "<b>Watershed:</b>",
      data_sub$ws,
      "<b>Creek:</b>",
      data_sub$creek,
      "<b>RMC Site ID:</b>",
      data_sub$rmc,
      "<b>Water Year:</b>",
      data_sub$year)
    
    # Customize color and size of circle markers 
    getRadius <- function(){
      if(input$show_radius == T){
        if ((!size_by == "none") & (!sum(!is.na(data_sub$size))==0) ){
          rad <- 3 + 7 * (data_sub$size/max(data_sub$size, na.rm = T))
        } else {rad <- 3}
      }      else {rad <- 3}
      
      
      return(ifelse(is.na(rad),3,rad))
    }
    
    getColor <- function() {
      if (! (filter_by == "none")){
        ifelse(data_sub$filter<threshold1,colors_bio[4], 
               ifelse(data_sub$filter<threshold2,colors_bio[3],
                      ifelse(data_sub$filter<threshold3,colors_bio[2],
                             ifelse(threshold3<0,"white",colors_bio[1]))))
      }
      else "white"
      
    }
    
    # update map 
    leafletProxy("map_sites") %>% clearMarkers() %>% clearControls() %>% clearShapes()
    
    if (nrow(sheds_sub)>0) {
      leafletProxy("map_sites") %>% 
        addPolygons(data=sheds_sub, layerId=sheds_sub$SYSTEM,
                    weight=1, smoothFactor = 0.5, 
                    opacity=0.6, fill = T, fillOpacity = 0.1, label=sheds_sub$SYSTEM, 
                    highlightOptions = highlightOptions(color = "white", weight = 3,
                                                        bringToFront = TRUE),
                    options=leafletOptions(pane="polygons")) %>% 
        addCircleMarkers(group="circles",
                         layerId = data_sub$rmc_id,
                         lng=data_sub$long, lat=data_sub$lat, 
                         radius=getRadius(), weight=1, opacity=0.6,
                         popup = content,
                         fill=T, fillColor=getColor(), fillOpacity = 0.6,
                         options=leafletOptions(pane="markers")) %>%
        addLegendCustom(position="topright",opacity = 1, borders=c("white",'white', 'white', "white"),
                        colors=colors_bio, 
                        labels=c("Likely Intact", "Possibly Intact", "Likely Altered", "Very Likely Altered"),
                        sizes=c(rep(12,4)), shapes=rep("circle",4), 
                        title= as.character(filter_name)) 
      
      #if(input$show_creeks == "yes")
      #{
        #leafletProxy("map_sites") %>% 
        # addPolylines(data=creeks, weight=1, color="black")
      #}
    }
    
    
    
  })
  
  
  # Watershed-specific info that appears when the user clicks on a polygon
  observe({
    click<-input$map_sites_shape_click
    if(is.null(click))
      return()
    else 
      output$ws_info <- renderText({
        paste("Watershed-specific info for", click$id)
      })
    
  })
  
  # Site-specific info that appears when the user clicks on a marker
  observe({
    click<-input$map_sites_marker_click
    if(is.null(click))
      return()
    
    
    else 
      output$site_info <- renderText({
        paste("Site-specific info for", click$id, "(",df_bio$ws[which(df_bio$rmc_id == click$id)],")")
      })
    
    output$table_site_onClick <- renderTable({
      slct_col <-  colnames(df_bio[,c(1,3,29,30,7,11,12,13,14,15,16,17,18,19,23,31,32)])
      d <- df_bio %>% dplyr::select(slct_col) %>%
        filter(rmc_id == click$id) %T>% 
        {names(.)<- as.character(param_names[match(slct_col,param_names$dataname),1])} %>%
        t()
      
      return(d)
    },rownames=T, colnames=F, bordered=T, align='c')
  })
  
  
  
  
  
  ## POC data
  ############################################################################################################################
  
  # Plot for POC
  
  data_sub_poc <- reactive({
    poc_contaminant <- input$poc_contaminant
    sites_POC <- df_POC %>%  
      dplyr::filter(year >= input$poc_yr[1] & year <= input$poc_yr[2])
    
    if(poc_contaminant == "hg")
    {sites_POC <- sites_POC %>% dplyr::mutate(selected_cont = hg_mg_kg) %>%
      dplyr::mutate(conc_cat = as.factor(hg_conc_cat))} 
    else {sites_POC <- sites_POC %>% dplyr::mutate(selected_cont= pcbs_mg_kg) %>%
      dplyr::mutate(conc_cat = as.factor(pcb_conc_cat))}
    
    return(sites_POC)
  })
  
  output$plot_poc_1 <- renderPlot({
    data_sub_poc <- data_sub_poc()
    n <- length(unique(conc_cat))
    colors <- if(n==3) {colors_Hg[2:4]} else {colors_PCB}
    
    ggplot(data=data_sub_poc, aes(x=factor(city),fill=conc_cat)) + geom_bar() +
      scale_fill_manual(values=colors) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) + 
      xlab("City") 
  })
  
  
  # MAP for POC 
  output$map_poc <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      setView(lng = -122, lat = 37.4, zoom = 10)
  })
  
  # Update map with user inputs 
  observe({
    
    # wait for POC menu to be selected 
    req(input$menu_items == "poc") 
    
    poc_contaminant <- input$poc_contaminant
    
    sites_poc <- df_POC %>%  dplyr::filter(year >= input$poc_yr[1] & year <= input$poc_yr[2])
    
    content_poc <- paste(
      sep = "<br/>",
      "<b>City:</b>",
      sites_poc$city,
      "<b>Concentration:</b>",
      if (poc_contaminant == "hg") {paste(sites_poc$hg_mg_kg, "mg/kg")} else paste(sites_poc$pcbs_mg_kg, "mg/kg"),
      "<b>Date:</b>",
      sites_poc$samp_date
    )
    
    getRadius_poc <- function(df){
      if (poc_contaminant == "hg"){
        rad <- 500*(df$hg_mg_kg+1)*(12-as.numeric(df$hg_conc_cat))/max(df$hg_mg_kg)
      }
      else rad <- 30000*(df$pcbs_mg_kg+1)*(4.2-as.numeric(df$pcb_conc_cat))/max(df$pcbs_mg_kg)
      rad
    }
    
    getColor_poc <- function(df){
      if (poc_contaminant == "hg") {col <- df$hg_col}
      else col <- df$pcb_col
      col
    }
    
    lab_poc <- if(poc_contaminant == "hg") {levels(sites_poc$hg_conc_cat)[2:4]} else levels(sites_poc$pcb_conc_cat)
    colors_poc <- if(poc_contaminant == "hg") {colors_Hg[2:4]} else colors_PCB
    
    leafletProxy("map_poc") %>% clearMarkers() %>% clearShapes() %>% clearControls() %>%
      addCircleMarkers(lng=sites_poc$long, lat=sites_poc$lat, radius=5, color=getColor_poc(sites_poc), weight=1, popup = content_poc, opacity = 0.7, fillOpacity = 0.5) %>%
      addLegend("topright", colors=colors_poc, labels=lab_poc, title="Color Key", layerId="colorLegend")
    
  })
  
  
  
  
  
  
  ## Continuous water quality 
  #############################################################################################################################
  
  # update inputs - sub-watersheds - sites 
  
  
  output$map_wq <-  renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      setView(lng=-122,lat=37.3, zoom=9) %>%
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "markers", zIndex = 420)
  })
  
  # update based on user input 
  observe({
    req(input$menu_items == "con_wq")
    
    popup <- paste(
      sep = "<br/>",
      "<b>Site:</b>",
      sites_cWQ$site_id,
      "<b>Watershed:</b>",
      sites_cWQ$ws,
      "<b>Continuous Water Quality Sampling in:</b>",
      sites_cWQ$sampl_dates_wq,
      "<b>Continuous Temperature Sampling in:</b>",
      sites_cWQ$sampl_dates_temp
    )
    
    get_color_wq <- function(){
      if (input$wq_plots == "Continuous Temperature"){
        if (input$temp_param =="avDayTemp"){
          
          threshold <- temp_thresholds[temp_thresholds$param == "avDayTemp","thresh"]   
          df_sub <- df_temp_7DAVG %>% 
            filter(year >= input$wq_yr[1] & year <= input$wq_yr[2])
          
          exc_ratio<- sapply(sites_cWQ$site_id,
                             function(x) sum(df_sub$avDayTemp[which(df_sub$site_id ==x)] > threshold)
                             /length(df_sub$avDayTemp[which(df_sub$site_id == x)]))
          
          return(colors_temp[signif(exc_ratio,1)*10+1])
        }
        if(input$temp_param == "avWeek"){
          
          threshold <- temp_thresholds[temp_thresholds$param == "avWeek","thresh"]   
          
          df_sub <- df_temp_MWAT %>% 
            filter(year >= input$wq_yr[1] & year <= input$wq_yr[2])
          
          exc_ratio <- sapply(sites_cWQ$site_id,
                              function(x) sum(df_sub$avWeek[which(df_sub$site_id ==x)] > threshold)
                              /length(df_sub$avWeek[which(df_sub$site_id == x)]))
          
          return(colors_temp[signif(exc_ratio,1)*10+1])
        }
      }
      
      
      
      else return("black")
    }
    
    
    get_weight <- function(){
      return(sapply(sheds$SYSTEM, function(x) {if (x== input$wq_ws){5} else{1}} ))
    }
    
    #shapes: 15 = square, 16= circle, 17 = triangle
    leafletProxy("map_wq")  %>% clearMarkers() %>% clearShapes() %>% clearControls() %>%
      addPolygons(data=sheds, layerId=sheds$SYSTEM,
                  weight=get_weight(), smoothFactor = 0.5, 
                  opacity=0.6, fill = T, fillOpacity = 0.1, label=sheds$SYSTEM, 
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  options = leafletOptions(pane="polygons")) %>% 
      #addCustomMarkers(data= sites_cWQ, lng=sites_cWQ$long, lat=sites_cWQ$lat,
      #                size=20, bg = c("blue", "orange","purple"),
      #               shapes=c(21,22,24), icon_group = sites_cWQ$marker_group,
      #              popup = popup) %>% 
      addCircleMarkers(data= sites_cWQ, lng=sites_cWQ$long, lat=sites_cWQ$lat,
                       radius=5, opacity=1,fillOpacity = 0.8, color=get_color_wq(),
                       popup = popup,
                       options = leafletOptions(pane="markers")) %>%
      #addLegendCustom(position="topright", colors=c("blue","orange","purple"),
      #               shapes = c("square", "circle", "triangle"), 
      #              labels=c("Continuous WQ", "Continuous Temperature", "Both"),
      #             sizes=c(10,10,10), borders=rep(2,3))
      addLegend("topleft", title= "exceedance/record ratio",colors = colors_temp, labels=c("0","0-0.1", "0.1-0.2", "0.2-0.3",
                                                                                           "0.3-0.4", "0.4-0.5", "0.5-0.6",
                                                                                           "0.6-0.7", "0.7-0.8", "0.8-0.9",
                                                                                           "0.9-1.0"))
  })
  
  
  time_plot_function <- function(data_sub_temp, param){
    if(nrow(data_sub_temp)>0){
      
      x_lims <- c(as.Date(paste(min(data_sub_temp$year),"-01-01",sep="")), as.Date(paste(max(data_sub_temp$year),"-12-31",sep="")))
      
      #if (param== "ConTemp"){
      # p <- ggplot(data=data_sub_temp, aes(x=date, y=ctemp_c, col=site_id)) + geom_line(size=0.3) + ylim(c(0,30))  + ylab("Temperature (\u00B0C)") + xlab("Date")  + scale_x_datetime(breaks=date_breaks("1 year"), labels=date_format("%b-%y")) +
      #geom_hline(yintercept = 24, linetype=2, col = "red") + 
      #theme_bw() 
      #}
      
      if (param == "avDayTemp"){
        threshold <- temp_thresholds[temp_thresholds$param == "avDayTemp","thresh"]   
        
        p <- ggplot(data=data_sub_temp, aes(x=date, y=avDayTemp)) + geom_line(aes(col=site_id,group=grp))  +
          ylim(c(0,30))  + ylab("Average Daily Temperature (\u00B0C)") + 
          xlab("Date")  +   scale_x_date(date_breaks = "3 months",  labels = date_format("%b-%Y"), date_minor_breaks="1 month",limits=x_lims, expand=c(0,0)) +
          theme_bw() + 
          geom_hline(yintercept = threshold, linetype=2, col = "red")
        
      }
      if (param == "avWeek"){
        
        threshold <- temp_thresholds[temp_thresholds$param == "avWeek","thresh"]   
        
        p <- ggplot(data=data_sub_temp, aes(x=day1week, y=avWeek, col=site_id)) + geom_point(aes(shape=site_id), size=2) + 
          ylim(c(0,30))  + ylab("MWAT (\u00B0C)") + xlab("Date") +   
          scale_x_date(date_breaks = "3 months",  labels = date_format("%b-%Y"), date_minor_breaks="1 month",limits=x_lims, expand=c(0,0)) + 
          geom_hline(yintercept = threshold, linetype=2, col = "red") +
          scale_shape_manual(values=seq(1,15,1))+
          theme_bw()
      }
      
      return(p)}
    else {NULL}}
  
  temp_timeseries_1 <- reactive({
    if (input$temp_param == "avDayTemp"){
      data_sub_temp <- df_temp_7DAVG %>% 
        dplyr::filter(year >= input$wq_yr[1] & year <= input$wq_yr[2],
                      ws == input$wq_ws,
                      plot_cat == 1)
      return(time_plot_function(data_sub_temp=data_sub_temp, param=input$temp_param))
    }
    if (input$temp_param == "avWeek"){
      data_sub_temp <- df_temp_MWAT %>% 
        dplyr::filter(year >= input$wq_yr[1] & year <= input$wq_yr[2],
                      ws == input$wq_ws,
                      plot_cat == 1)
      return(time_plot_function(data_sub_temp=data_sub_temp, param=input$temp_param))
    }
  })
  
  temp_timeseries_2 <- reactive({
    if (input$temp_param == "avDayTemp"){
      data_sub_temp <- df_temp_7DAVG %>% 
        dplyr::filter(year >= input$wq_yr[1] & year <= input$wq_yr[2],
                      ws == input$wq_ws,
                      plot_cat == 2)
      p <- time_plot_function(data_sub_temp=data_sub_temp, param=input$temp_param)
    }
    if (input$temp_param == "avWeek"){
      data_sub_temp <- df_temp_MWAT %>% 
        dplyr::filter(year >= input$wq_yr[1] & year <= input$wq_yr[2],
                      ws == input$wq_ws,
                      plot_cat == 2)
      p <- time_plot_function(data_sub_temp=data_sub_temp, param=input$temp_param)
    }
    
    return(p)
  })
  
  
  
  output$temp_timeseries_1 <-renderPlot({
    p<- temp_timeseries_1()
    return(p)  
  })
  output$temp_timeseries_2 <-renderPlot({
    p<- temp_timeseries_2()
    return(p)  
  })
  
  
  
  
  # Boxplots
  
  boxplot_function <- function(data_sub_wq, param, x_param="year", plot_cat =1){
    if (nrow(data_sub_wq)>0){
      data_sub_wq <- data_sub_wq[data_sub_wq$plot_cat==plot_cat,]
      if(nrow(data_sub_wq)>0){
        threshold <- MRP_threshold[match(colnames(data_sub_wq)[ncol(data_sub_wq)],MRP_threshold$label),'value_sup']
        
        df_tempo <- data.frame(x_var=as.factor(data_sub_wq[,which(colnames(data_sub_wq)==x_param)]), 
                               y_var=data_sub_wq[,which(colnames(data_sub_wq)==param)])
        
        p <- ggplot(data=df_tempo, aes(x=x_var,y=y_var)) + geom_boxplot(col=rgb(0,0,1,0.6)) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          xlab(x_param) + ylab(colnames(data_sub_wq)[ncol(data_sub_wq)]) +
          geom_hline(yintercept = threshold, lty=2, col="red", lwd=1) +
          stat_summary(fun.data = give_tot, geom="text", fun.y=median, position=position_dodge(width=0.75))
        
        return(p)
      }
      
    } else return(NULL)
  }
  
  wq_data_sub <- reactive({
    param <- input$wq_param
    season <- input$wq_season
    ws <- input$wq_ws
    season <- input$wq_season
    
    data_sub_wq <- df_wq %>%  
      dplyr::filter(year >= input$wq_yr[1] & year <= input$wq_yr[2],
                    ws %in% input$wq_ws, 
                    season == input$wq_season)
    
    data_sub_wq <- cbind(data_sub_wq, data_sub_wq[,which(colnames(data_sub_wq)==param)])
    
    parameter_name <- as.character(MRP_threshold[match(param,MRP_threshold$param),'label'])
    colnames(data_sub_wq)[ncol(data_sub_wq)] <- parameter_name
    
    
    return(data_sub_wq)
  })
  
  output$wq_boxplot_1 <- renderPlot({
    data_sub_wq <- wq_data_sub()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(boxplot_function(data_sub_wq = data_sub_wq, param=param, plot_cat = 1, x_param="site_id"))
  })
  
  
  output$wq_boxplot_2 <- renderPlot({
    data_sub_wq <- wq_data_sub()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(boxplot_function(data_sub_wq = data_sub_wq, param=param, plot_cat = 2, x_param="site_id"))
  })
  
  
  # Beanplots < boxplots now
  
  output$wq_beanplot_1 <- renderPlot({
    data_sub_wq <- wq_data_sub()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(boxplot_function(data_sub_wq = data_sub_wq, param=param, plot_cat = 1, x_param="year"))
  })
  
  output$wq_beanplot_2 <- renderPlot({
    data_sub_wq <- wq_data_sub()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(boxplot_function(data_sub_wq = data_sub_wq, param=param, plot_cat = 2, x_param="year"))
  })
  
  
  
  
})





