server <- function(input,output){
  
  
## Bioassessment #################################################################################################################################        
#################################################################################################################################################
  
  # Explanatory pop-up windows: bioassessments, scores ##########################
  
  # Help bio tab : description of the tab in popup window
  observeEvent(input$bio_tab_title,{
    showModal(modalDialog(
      title = "Biological Condition Assessment",
      HTML('Biological Data collected within this program corresponds to aggregated
           benthic algae and benthic macroinvertebrate data. 
           <br/>
           <br/>
           
           The collected bioassessment data is used to compute biological health indicators. 
            Several indicators are currently in use in California: 
           <ul><li> California Stream Condition Index (CSCI) for benthic macroinvertebrates </li>
           <li> Algae Stream Condition Index (ASCI) for Soft Algae </li>
           <li> Algae Stream Condition Index (ASCI) for Diatoms </li>
           <li> Hybrid Algae Stream Condition Index (ASCI) for both Soft Algae and diatoms </li></ul>
           The CRAM Indicator was used in the 2012-2015 period. 
            
           <br/> 
           <br/>
           
           <i> Browse the page below to learn more about these indicators.</i>'),
      easyClose = TRUE, footer=modalButton("Got it!")
      )
    )
  }
  )
  
  
  # description of the selected score in popup window
  score_desc_txt <- reactive({
    score <- input$bio_score_map 
    score_name <- bio_vars_filter[which(bio_vars_filter$param == score),"name"]
    
    if (score == "csci") {
      
      description <-  "The benthic (i.e., bottom-dwelling) macroinvertebrates collected through this monitoring
      program are organisms that live on, under, and around the rocks and sediment in the stream
      bed. Examples include dragonfly and stonefly larvae, snails, worms, and beetles.
      Each BMI species has a unique response to water chemistry and physical habitat condition.
      Some are relatively sensitive to poor habitat and pollution; others are more tolerant. Therefore,
      the abundance and variety of BMIs in a stream indicates the biological condition of the stream.
      <br/>
      <br/>
      
      The California Stream Condition Index (CSCI) is a biological index 
      that was developed by the State Water Resources Control Board (State Board) and is used to score the condition
      of benthic macroinvertabrate (BMI) communities in perennial wadeable rivers and streams. The CSCI translates BMI
      data into an overall measure of stream health. The CSCI was developed using a large reference data set that is 
      intended to represent the full range of natural conditions in California (Rehn et al. 2015). It combines two 
      types of indices: 1) taxonomic completeness, as measured by the ratio of observed-to-expected taxa (O/E); and
      2) ecological structure and function, measured as a predictive multi-metric index (pMMI) that is based on 
      reference conditions. The CSCI score is computed as the average of the sum of O/E and pMMI. 
      The values range from 0 to 1, 0 representing the worst score, and 1 the best score.In the current MRP, 
      the Regional Water Board defined a CSCI score of 0.795 as a threshold for identifying sites with degradeded
      biological condition that may be considered as candidates for a Stressor Source Identification project.
      
      <br/>
      <br/>
      
      See the table below for a more detailed presentation of CSCI score breaks."}
    
    if (score %in% c("asci_hyb", "asci_diatom", "asci_soft_alg")) {
      
      description <-  "Similar to benthic macroinvertebrates, the abundance and type of benthic algae species living
      on a streambed can indicate stream health. When evaluated with the CSCI, biological indices based on benthic
      algae can provide a more complete picture of the streams biological condition because algae
      respond more directly to nutrients and water chemistry. In contrast, BMIs are more responsive
      to physical habitat.
      <br/>
      <br/>
      
      The State Water Board and SCCWRP recently developed the draft Algae Stream Condition
      Index (ASCI) which uses benthic algae data as a measure of biological condition for streams in
      California (Theroux et al. in prep.). The ASCI is a non-predictive scoring tool comprised of three 
      single-assemblage metrics associated with either
      diatoms or soft algae, or combinations of metrics representing both assemblages (i.e, <q>hybrid</q>). Additional study
      is needed however, to determine the best approach to apply the ASCI tools to
      evaluate bioassessment data. For example, it is not clear if the ASCI should be used as a
      second line of evidence to understand CSCI scoring results, or if it would be more effective as
      an independent indicator to evaluate different types of stressors (e.g., nutrients) to which BMIs
      are not very responsive. The ASCI is currently under review by the Biostimulatory-Biointegrity
      Policy Science Advisory Panel and the State Water Board. 
      
      <br/>
      <br/>
      
      See the table below for a more detailed presentation of ASCI score breaks."
    }
    
    if (score == "cram") {
      
      description <- "The California Rapid Assessment Method (CRAM) evaluates four different components of riparian condition on a scale from 25 to 100. 
      The four attributes include: 1) buffer and landscape context; 2) hydrology; 3) physical structure; and 4) biotic structure.
      These four attributes are summed together and divided by four to calculate an overall total CRAM score for each bioassessment 
      site. The use of this score was discontinued in 2016. 
      
      <br/>
      <br/>
      
      See the table below for a more detailed presentation of CRAM score breaks."}
    
    paste0(description)
    })
  
  score_desc_table <- reactive({
    df <- bio_vars_filter %>% dplyr::filter(param == input$bio_score_map) %>% 
      dplyr::select(seq(3,6)) %T>% 
      {names(.) <- c("Very Likely Altered","Likely Altered" ,"Possibly Intact", "Likely Intact")} %>% t()
    
    df[1,] <- paste(df[1,], " - ", df[2,])
    df[2,] <- paste(df[2,], " - ", df[3,])
    df[3,] <- paste(df[3,], " - ", df[4,])
    df[4,] <- paste(" > ", df[4,])
    
    
    col=seq(4,1,-1)
    name <- as.character(bio_vars_filter[bio_vars_filter$param == input$bio_score_map,'name'])
    df <- data.frame(df, colors=col) 
    colnames(df) <-c(name,"colors")
    
    
    df <- df %>% datatable(options = list(autoWidth=F,columnDefs = list(list(targets=2, visible=F)), dom='t', bSort=F)) %>% 
      formatStyle(1,2,backgroundColor=styleEqual(col,colors_bio[col]), color=styleEqual(col,c("white",rep("black",3)))) %>%
      formatStyle( 0, target= 'row',lineHeight='70%')
    
    return(df)
  }
  )
  
  
  observeEvent(input$bio_score_popup,{
    showModal(modalDialog(
      title = "Score Description",
      HTML(score_desc_txt()),
      DT::renderDataTable( score_desc_table()),
      easyClose = TRUE, footer=modalButton("Got it!")
      )
    )
  }
  )
  
  
  # RenderUI ##### 
  output$bio_score_subset <- renderUI({
    
    if (length(input$bio_score_dwlnd)>0){
      choices_score_vis <- input$bio_score_dwlnd
      names(choices_score_vis) <- bio_vars_filter[match(choices_score_vis, bio_vars_filter$param),"name"]

      pickerInput(
        inputId = "bio_score_map",
        label = "Visualize map and plots by:",
        choices = choices_score_vis,
        selected =  input$score_dwlnd[1],
        options = list(`actions-box` = F, size = 20),
        multiple = F)
    }
  })
  outputOptions(output, "bio_score_subset", suspendWhenHidden=F)
  
  
  # Bio Data subsetting ###########################################################
  # Reactive function that returns the subsetted data table
  data_sub <- reactive({
    
    slct_bio_score <- which(colnames(df_bio) %in% input$bio_score_dwlnd)
    
    data_sub <-
      df_bio %>% 
      dplyr::filter(year >= input$bio_wy[1] & year <= input$bio_wy[2],
                    ws %in% input$bio_ws,
                    assmnt_type %in% input$bio_assmnt_type) %>%
      dplyr::mutate(year = factor(year, levels = seq(min(bio_vars_yr), max(bio_vars_yr), 1))) %>%
      dplyr::select(c(1:5, slct_bio_score, 39:40, 43:45))  %>%
      arrange(desc(year), ws) 
    
    
    return(as.data.frame(data_sub))
  })
  
  # Bio Download #####################################################
  
  
  # create shapefile for download 
  bio_shp <- reactive({
    
    xyPoints <- data_sub() %>% 
      dplyr::filter(!is.na(long)) %>%
      dplyr::mutate(long=as.numeric(long), lat=as.numeric(lat)) %>% 
      dplyr::arrange(rmc_id)
    
    
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,c("long","lat")], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    
    return(SHP)
    
  })
  
  
  
  # Download user selected file type of selected dataset 
  output$bio_downloadData <- downloadHandler(
    
    filename = function() { 
      if (!input$bio_file_type == ".shp"){
        paste("SC_bioassmnt", "_table", Sys.Date(), input$bio_file_type, sep = "")
      }
      else {paste0("shpExport.zip")} 
    },
    
    content = function(file) {
      if(input$bio_file_type== ".csv") {
        write.csv(data_sub(), file, row.names = FALSE)
      } else if(input$bio_file_type == ".xlsx") {
        write.xlsx(data_sub(), file)
      }
      if (input$bio_file_type == ".shp"){
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
        writeOGR(bio_shp(), dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
        zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"),zip = Sys.getenv("R_ZIPCMD", "zip"))
        file.copy("shpExport.zip", file)
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
      }
    }
  )
  
  
  
  
  # Bio Score overview: Map ###########################################
  
  # MAP title
  output$map_bio_title <- renderText({
    filter_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$bio_score_map)]
    paste(
      "Map of ",
      filter_name,
      "for ",
      input$bio_wy[1],
      " - ",
      input$bio_wy[2]
    )
  })
  
  # baseline map
  output$map_bio <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = F,
                                     attributionControl = F)) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -121.8,
              lat = 37.3,
              zoom = 9) %>%
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "markers", zIndex = 420)
  })
  
  # reset view button
  observe({
    input$bio_reset_button
    leafletProxy("map_bio") %>% setView(lat = 37.3,
                                          lng = -121.8,
                                          zoom = 9)
  })
  
  # Update based on user inputs
  observe({
    req(input$menu_items == "bio_data")
    
    score_data_name <-  input$bio_score_map
    score_full_name <-  bio_vars_filter[which(bio_vars_filter$param == score_data_name),"name"]
    threshold1 <-
      bio_vars_filter$threshold1[which(bio_vars_filter$param == score_data_name)]
    threshold2 <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == score_data_name)]
    threshold3 <-
      bio_vars_filter$threshold3[which(bio_vars_filter$param == score_data_name)]
    
    
    # data subset 
    data_sub <- data_sub() %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == score_data_name, 
                    !is.na(value))
      
       # colors for map 
    col.bins <- colorBin(palette=unname(colors_bio), bins=c(0, threshold1, threshold2, threshold3, 1000), domain=data_sub$value, reverse=T)
    
    # subset watersheds shapefile based on user input
    sheds_sub <- sheds[sheds$SYSTEM %in% input$bio_ws, ]
    
    
    # pop-up window for individual sites
    content <- paste(
      sep = "<br/>",
      "<b>Watershed:</b>",
      paste0(data_sub$ws, " (",data_sub$creek, ")"),
      "<b>RMC Site ID:</b>",
      data_sub$rmc,
      "<b>Water Year:</b>",
      data_sub$year, 
      paste0("<b>",score_full_name,"</b>"), 
      signif(data_sub$value,2)
    )
    
    # update map
    leafletProxy("map_bio") %>% clearMarkers() %>% clearControls() %>% clearShapes()
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_bio") %>%
        addPolygons(
          data = sheds_sub,
          layerId = sheds_sub$SYSTEM,
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0.6,
          fill = T,
          fillOpacity = 0.1,
          label = sheds_sub$SYSTEM,
          highlightOptions = highlightOptions(
            color = "white",
            weight = 3,
            bringToFront = TRUE
          ),
          options = leafletOptions(pane = "polygons")
        ) %>%
        addCircleMarkers(data=data_sub, 
          group = "circles",
          layerId = data_sub$rmc_id,
          lng =data_sub$long,
          lat = data_sub$lat,
          radius = 4,
          weight = 1,
          opacity = 0.9,
          popup = content,
          fill = T,
          fillColor = ~col.bins(value),
          fillOpacity = 0.9,
          options = leafletOptions(pane = "markers")
        )
    }
  }
  )
  
  # Bio data : Summary Plots #################################################
  # Summary barplot 
  output$bio_barplot_title <- renderText({
    score_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$bio_score_map)]
    paste(
      "Barplot of ",
      score_name,
      "for ",
      input$bio_wy[1],
      " - ",
      input$bio_wy[2]
    )
  })  
  
  output$bio_barplot <- renderPlot({
    score_name <- input$bio_score_map
    threshold1 <-
      bio_vars_filter$threshold1[which(bio_vars_filter$param == score_name)]
    threshold2 <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == score_name)]
    threshold3 <-
      bio_vars_filter$threshold3[which(bio_vars_filter$param == score_name)]
    
    
    data_sub <- data_sub()  %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == score_name) %>% 
      dplyr::mutate(score_cat = cut(x=value, 
                                    breaks=c(0, threshold1, threshold2, threshold3,1000),
                                    labels=c("Very Likely Altered", "Likely Altered","Possibly Intact", "Likely Intact")))%>% 
      dplyr::filter(!is.na(score_cat))
    
    
    n="ws"
    if (length(unique(data_sub[,n]))==1){n <- "subws"}
    n_name <- ifelse(n=="ws",'Watershed',"Subwatershed")
    
    if (!input$bio_show_bar_pct){
      p <- ggplot(data = data_sub, aes_string(x = n, fill = "score_cat")) + geom_bar()+ ylab("Number of samples")
    }
    
    else {
   
      p <- ggplot(data = data_sub, aes_string(x = n, fill = "score_cat")) + geom_bar(position="fill") + ylab("Percentage of samples") +
        scale_y_continuous(labels = percent_format())
    }
    
    p <- p + 
      scale_fill_manual(values=colors_bio) + 
      xlab(n_name) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1, size=12), 
            legend.position = "top", legend.box="vertical", legend.title = element_blank(), 
            text = element_text(size=12)) 
    
    return(p)
  })
  
  
  
  # summary boxplot 
  output$bio_boxplot_title <- renderText({
    score_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$bio_score_map)]
    paste(
      "Boxplot of ",
      score_name,
      "for ",
      input$wy[1],
      " - ",
      input$wy[2]
    )
  })  
  
  
  output$bio_boxplot <- renderPlot({
    score_name <- input$bio_score_map
    score_full_name <- bio_vars_filter[which(bio_vars_filter$param == score_name), "name"]
    threshold <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == score_name)]
    
    
    data_sub <- data_sub()  %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == score_name) 
    
    n="ws"
    if (length(unique(data_sub[,n]))==1){n <- "subws"}
    n_name <- ifelse(n=="ws",'Watershed',"Subwatershed")
    
    
   
    p <-
      ggplot(data_sub, aes_string(x = n, y = "value")) + geom_boxplot(col = rgb(0, 0, 1, 0.6)) +
      xlab(n_name) + ylab(score_full_name) +
      stat_summary(
        fun.data = give_tot,
        geom = "text",
        fun.y = median,
        position = position_dodge(width = 0.75)
      ) + 
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size=12))
    
    if (threshold >= 0) {
      p <-
        p + geom_hline(yintercept = threshold,
                       linetype = 2,
                       col = "seagreen")
    }
    return(p + theme(text=element_text(size=12)))
    
    
  })
  


  
  
###### 
  
# Continuous Temperature ##################################################################################################
#################################################################################################################################
  

  
  # Explanatory Popup #############
  observeEvent(input$temp_desc,{
    showModal(modalDialog(
      title = "Temperature Monitoring",
      HTML('San Francisco Bay Area Stormwater Municipal Regional Permit (MRP), Provision C.8. on Water Quality Monitoring, 
           section (iii): "The Permittees shall monitor temperature of their streams using a digital temperature logger or equivalent [...]
           at 60-minute intervals from April through September"
           <br/>
           <br/>
           Exceedances of temperature parameters are based upon the following thresholds: 
           "The temperature trigger is defined as when two or more weekly average temperatures exceed the Maximum Weekly Average
           Temperature of 17.0 C for a Steelhead stream, or when 20% of the results at one sampling station exceed the instantaneous maximum of 24.0 C."')
      ,
      
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  observeEvent(input$map_temp_desc,{
    showModal(modalDialog(
      title = "Temperature Map",
      HTML("For the purpose of this map, the temperature color scale corresponds to the average of all the temperature data points over the selected time period. This color scale is only a <i>relative</i> scale, 
           it does not indicate water quality objective exceedances. Rather, it's a simplified way to compare sites with one another.
        "),
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  # Subset ##### 
  temp_subset <- reactive({ 
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    
    if (input$temp_data_type == "temp_grab") {
      data_sub <- df_bio %>% 
        dplyr::select(c("rmc_id", "ws", "subws", "creek", "long", "lat", "sample_date", "temp_c")) %>% 
        dplyr::filter(as.Date(sample_date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(sample_date) - as.Date(filter_dates[2])<= 0, 
                      ws %in% input$temp_ws_dwld) %>% 
        dplyr::rename(site_id=rmc_id)
    }
    
    
    else {if(input$temp_data_type == "temp_cont") { 
      
        data_sub <- df_temp %>% 
          dplyr::filter(as.Date(date) - as.Date(filter_dates[1])>= 0 & 
                          as.Date(date) - as.Date(filter_dates[2])<= 0, 
                        ws %in% input$temp_ws_dwld)
    }  
    else {if (input$temp_data_type == "temp_seas"){
      
      data_sub <- df_wq %>%
        as.data.frame() %>%
        dplyr:: select(c("site_id", "lat", "long", "ws", "creek", "date", "season", "temp_c")) %>%  
        dplyr::filter(
          as.Date(date) - as.Date(filter_dates[1])>= 0 & 
            as.Date(date) - as.Date(filter_dates[2])<= 0,
          ws %in% input$temp_ws_dwld) 
    }}
    } 
    
    return(data_sub)
  })
  
  output$test <- renderTable({
    head(temp_subset())
  })
  
  
  # Download #### 
  output$downloadData_temp <- downloadHandler(
    
    filename = function() {
      paste(input$temp_data_type, "_table", Sys.Date(), input$file_type_temp, sep = "")
    },
    
    content = function(file) {
      
      
      data_to_dwn <- temp_subset()
      
      if(input$file_type_temp== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$file_type_temp == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  
  # Map ###########
  output$map_temp <-  renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -122,
              lat = 37.3,
              zoom = 9) %>%
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "markers", zIndex = 420) 
    })
  
  # update based on user input
  observe({
    req(input$menu_items == "temp")
    
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    data_sub <- temp_subset()
  
    leafletProxy("map_temp")  %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    
    if (nrow(data_sub)>0) {
        
    if (input$temp_data_type == "temp_cont"){
     data_sub <- data_sub %>% 
       dplyr::group_by(site_id, ws, creek, long, lat) %>% 
       dplyr::summarize(temp_c = round( mean(ctemp_c, na.rm=T),3))
       
     popup_temp <- paste(
       sep = "<br/>",
       "<b>Site:</b>",
       data_sub$site_id,
       "<b>Watershed:</b>",
       data_sub$ws, 
       "<b>Mean Temperature Over Period:</b>",
       paste0(data_sub$temp_c)
     )
     
    
    }
    
    if (input$temp_data_type == "temp_seas"){
      data_sub <- data_sub %>% 
        dplyr::group_by(site_id, ws, creek, long, lat) %>% 
        dplyr::summarize(temp_c = round( mean(temp_c, na.rm=T),3))
      
      popup_temp <- paste(
        sep = "<br/>",
        "<b>Site:</b>",
        data_sub$site_id,
        "<b>Watershed:</b>",
        data_sub$ws, 
        "<b>Mean Temperature Over Period:</b>",
        paste0(data_sub$temp_c)
      )
      
      
    }
    
    
    if (input$temp_data_type == "temp_grab") {
      popup_temp <- paste(
        sep = "<br/>",
        "<b>Site:</b>",
        data_sub$site_id,
        "<b>Watershed:</b>",
        data_sub$ws, 
        "<b>Temperature:</b>",
        paste0(data_sub$temp_c, " (", data_sub$sample_date, ")")
      )
      

    } 
    
    temp.colBins <- colorBin(palette="Spectral", pretty=F, bins= 6, domain=data_sub$temp_c, reverse=T)  

    
    #shapes: 15 = square, 16= circle, 17 = triangle
    leafletProxy("map_temp")  %>% 
      addPolygons(
        data = sheds,
        layerId = sheds$SYSTEM,
        smoothFactor = 0.5,
        opacity = 0.6,
        weight=1, 
        fill = T,
        fillOpacity = 0.1,
        label = sheds$SYSTEM,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 3,
          bringToFront = TRUE
        ),
        options = leafletOptions(pane = "polygons")
      ) %>%
      addCircleMarkers(
        data = data_sub,
        lng = data_sub$long,
        lat = data_sub$lat,
        radius = 5,
        weight = 0.5,
        opacity = 0.9,
        fill = T,
        fillOpacity = 0.8,
        fillColor = ~temp.colBins(temp_c),       
        popup = popup_temp,
        options = leafletOptions(pane = "markers")
      ) %>% 
      addLegend(title= "Temperature Scale",pal= temp.colBins, values=c()) 
    } 
  })

  
  # Summary plot #####
  output$temp_plot <- renderPlot({
    if (nrow(temp_subset())>0){
    
      
    if (input$temp_data_type == "temp_cont"){hide("temp_plot")}
    else {show("temp_plot")}
    
    if(input$temp_data_type == "temp_grab"){ 
      p <- ggplot(data=temp_subset(), aes(x=temp_c)) + geom_histogram()
    }
    
    if(input$temp_data_type == "temp_seas"){ 
      p <- (ggplot(data=temp_subset(), aes(x=creek, y= temp_c, fill=season)) +
        geom_violin() + 
        theme(legend.position = "top", legend.title = element_blank()) + 
        xlab("") + ylab("Temperature (\u00B0C)")) + 
        scale_fill_manual(values= c("lightskyblue", "rosybrown"), labels=c("Spring", "Fall"))

      }
      
    
    }
    
    return(p)
  })
  
  # Timeseries plot when cont temp selected
  ws_click <- reactiveValues(clickedShape=NULL)
  
  observeEvent(input$map_temp_shape_click, { 
   ws_click$clickedShape <- input$map_temp_shape_click
    })
  
  
  output$temp_timeseries_message <- renderUI({
    if (input$temp_data_type == "temp_cont"){
      if(is.null(ws_click$clickedShape)){
              HTML("<i> Click on a watershed on the map above to visualize temperature timeseries for this watershed.</i>")
      }
      else{paste0("Timeseries of temperature for ", ws_click$clickedShape$id, " Watershed")}
    }
    else {NULL}
  })
  
  output$temp_timeseries <- renderPlotly({
    p <- NULL 
    height_plot <- NULL
    
    if (input$temp_data_type == "temp_cont"){
      if (!is.null(ws_click$clickedShape)){
        
          
        data_sub <- temp_subset() %>% 
        dplyr::filter(ws == ws_click$clickedShape$id) %>% 
      mutate(date =as.Date(date)) %>%
      dplyr::group_by( year,ws, plot_cat,creek, site_id,lat,long,date) %>% 
      dplyr::summarize(temp_c= mean(ctemp_c)) %>% 
      mutate(grp=paste(year, site_id,sep=''))
  
    if (nrow(data_sub)>0){
      height_plot <-length(unique(data_sub$creek))*200 + 200
      filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))

    n <- length(unique(data_sub$site_id))
    cols = rainbow(n, s=.6, v=.9)[sample(1:n,n)]
    
    p <-
      ggplot(data = data_sub, aes(x = date, y = temp_c)) + geom_line(size=0.2,aes(col =site_id, group = grp))  +
      scale_fill_manual(values=cols) + 
      facet_grid(rows=vars(creek)) + 
      scale_x_date(
        date_breaks = "4 months",
        labels = date_format("%b-%Y"),
        date_minor_breaks = "2 months",
        limits = filter_dates,
        expand = c(0, 0)) + 
      ylab("Average Daily Temperature (\u00B0C)") +
      xlab("Date") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position="none", axis.title.x = element_blank()) + 
      coord_cartesian(ylim=c(10,25)) 
      }
      
    }   
          
  }
  
    
    return(ggplotly(p, height= height_plot))
  })
  
  

###### 
  
  }


