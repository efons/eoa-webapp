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
    
    slct_bio_score <- colnames(df_bio)[which(colnames(df_bio) %in% input$bio_score_dwlnd)]
    
    data_sub <-
      df_bio %>% 
      dplyr::filter(year >= input$bio_wy[1] & year <= input$bio_wy[2],
                    ws %in% input$bio_ws,
                    assmnt_type %in% input$bio_assmnt_type) %>%
      dplyr::mutate(year = factor(year, levels = seq(min(bio_vars_yr), max(bio_vars_yr), 1))) %>%
      dplyr::select(c("rmc_id","wb_id","assmnt_type","year","sample_date", slct_bio_score, "long","lat", "ws", "subws","creek"))  %>%
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
    paste0(
      "Map of ",
      filter_name, "s ",
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
      addTiles(group="OpenStreetMap") %>% 
      addProviderTiles(providers$Esri.WorldTopoMa, group="Esri World Topo") %>%
      setView(lng = -121.8,
              lat = 37.3,
              zoom = 9) %>%
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "markers", zIndex = 420) %>% 
      addLegend(colors=colors_bio, labels=names(colors_bio), position="topleft", group="Legend") %>%
      addLayersControl(
        baseGroups = c("Esri World Topo","OpenStreetMap"),
        overlayGroups = c("Watersheds", "Legend")) 
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
    leafletProxy("map_bio") %>% clearMarkers() %>% clearShapes()
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_bio") %>%
        addPolygons(
          data = sheds_sub,
          group= "Watersheds",
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
    paste0(
      "Barplot of ",
      score_name,
      "s for ",
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
      input$bio_wy[1],
      " - ",
      input$bio_wy[2]
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
  
  
  # Nutrients ##################################################################################################
  #################################################################################################################################
  
  # RenderUI ##### 
  output$nutr_subset <- renderUI({
    
    if (length(input$nutr_type_dwlnd)>0){
      choices_nutr_vis <- input$nutr_type_dwlnd
      names(choices_nutr_vis) <- names(nutrient_list[match(choices_nutr_vis,nutrient_list)])
      
      pickerInput(
        inputId = "nutr_map",
        label = "Visualize map and plots by:",
        choices = choices_nutr_vis,
        selected=choices_nutr_vis[1],
        options = list(`actions-box` = F, size = 20),
        multiple = F)
    }
  })
  outputOptions(output, "nutr_subset", suspendWhenHidden=F)
  
  # nutrient data sub #####
  nutr_data_sub <- reactive({
    
    slct_nutr <- colnames(df_bio)[which(colnames(df_bio) %in% input$nutr_type_dwlnd)]
    
    data_sub <-
      df_bio %>% 
      dplyr::filter(year >= input$nutr_wy[1] & year <= input$nutr_wy[2],
                    ws %in% input$nutr_ws) %>%
      dplyr::mutate(year = factor(year, levels = seq(min(bio_vars_yr), max(bio_vars_yr), 1))) %>%
      dplyr::select(c("rmc_id","wb_id","assmnt_type","year","sample_date", slct_nutr, "long", "lat", "ws", "subws", "creek"))  %>%
      arrange(desc(year), ws) 
    
    
    return(as.data.frame(data_sub))
  })
  
  
  
  # nutrient Download #########
  
  
  
  # create shapefile for download 
  nutr_shp <- reactive({
    
    xyPoints <- nutr_data_sub() %>% 
      dplyr::filter(!is.na(long)) %>%
      dplyr::mutate(long=as.numeric(long), lat=as.numeric(lat)) %>% 
      dplyr::arrange(rmc_id)
    
    
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,c("long","lat")], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    
    return(SHP)
    
  })
  
  
  
  # Download user selected file type of selected dataset 
  output$nutr_downloadData <- downloadHandler(
    
    filename = function() { 
      if (!input$nutr_file_type == ".shp"){
        paste("SC_bioassmnt", "_table", Sys.Date(), input$nutr_file_type, sep = "")
      }
      else {paste0("shpExport.zip")} 
    },
    
    content = function(file) {
      if(input$nutr_file_type== ".csv") {
        write.csv(nutr_data_sub(), file, row.names = FALSE)
      } else if(input$nutr_file_type == ".xlsx") {
        write.xlsx(nutr_data_sub(), file)
      }
      if (input$nutr_file_type == ".shp"){
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
        writeOGR(nutr_shp(), dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
        zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"),zip = Sys.getenv("R_ZIPCMD", "zip"))
        file.copy("shpExport.zip", file)
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
      }
    }
  )
  
  # Map Nutrients #####
  # MAP title
  output$map_nutr_title <- renderText({
    filter_name <-
      param_names[which(param_names$dataname == input$nutr_map), "fullname"]
    paste(
      "Map of ",
      filter_name,
      "for ",
      input$nutr_wy[1],
      " - ",
      input$nutr_wy[2]
    )
  })
  
  # baseline map
  output$map_nutr <- renderLeaflet({
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
    input$nutr_reset_button
    leafletProxy("map_nutr") %>% setView(lat = 37.3,
                                         lng = -121.8,
                                         zoom = 9)
  })
  
  # Update based on user inputs
  observe({
    req(input$menu_items == "nutrients")
    
    nutr_name <-  input$nutr_map
    nutr_full_name <- param_names[which(param_names$dataname == input$nutr_map), "fullname"]
    
    # data subset 
    data_sub <- nutr_data_sub() %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == nutr_name, 
                    !is.na(value))
    
    # colors for map 
    col.bins <- colorBin(palette="Blues", bins=5, pretty=T, domain=data_sub$value, reverse=F)
    
    # subset watersheds shapefile based on user input
    sheds_sub <- sheds[sheds$SYSTEM %in% input$nutr_ws, ]
    
    
    # pop-up window for individual sites
    content <- paste(
      sep = "<br/>",
      "<b>Watershed:</b>",
      paste0(data_sub$ws, " (",data_sub$creek, ")"),
      "<b>RMC Site ID:</b>",
      data_sub$rmc,
      "<b>Water Year:</b>",
      data_sub$year, 
      paste0("<b>",nutr_full_name,"</b>"), 
      signif(data_sub$value,2)
    )
    
    # update map
    leafletProxy("map_nutr") %>% clearMarkers() %>% clearControls() %>% clearShapes()
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_nutr") %>%
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
        ) %>% 
        addLegend( title= nutr_full_name,position="topright", pal=col.bins, values=c())
    }
  }
  )
  
  
  # summary boxplot  #####
  output$nutr_boxplot_title <- renderText({
    nutr_full_name <-
      param_names[which(param_names$dataname == input$nutr_map), "fullname"]
    
    paste(
      "Boxplot of ",
      nutr_full_name,
      "for ",
      input$nutr_wy[1],
      " - ",
      input$nutr_wy[2]
    )
  })  
  
  
  output$nutr_boxplot <- renderPlot({
    nutr_name <- input$nutr_map
    nutr_full_name <- param_names[which(param_names$dataname == input$nutr_map), "fullname"]
    
    data_sub <- nutr_data_sub()  %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == nutr_name) 
    
    n="ws"
    if (length(unique(data_sub[,n]))==1){n <- "subws"}
    n_name <- ifelse(n=="ws",'Watershed',"Subwatershed")
    
    
    
    p <-
      ggplot(data_sub, aes_string(x = n, y = "value")) + geom_boxplot(col = rgb(0, 0, 1, 0.6)) +
      xlab(n_name) + ylab(nutr_full_name) +
      stat_summary(
        fun.data = give_tot,
        geom = "text",
        fun.y = median,
        position = position_dodge(width=0.75)
      ) + 
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size=12))
    
    return(p + theme(text=element_text(size=12)))
    
    
  })
  
  
  
  
  
  
  
  ##### 
  
  
  # Habitat ##################################################################################################
  #################################################################################################################################
  
  # RenderUI ##### 
  output$hab_subset <- renderUI({
    
    if (length(input$hab_type_dwlnd)>0){
      choices_hab_vis <- input$hab_type_dwlnd
      names(choices_hab_vis) <- param_names[match(choices_hab_vis, param_names$dataname),"fullname"]
      
      pickerInput(
        inputId = "hab_map",
        label = "Visualize map and plots by:",
        choices = choices_hab_vis,
        selected=choices_hab_vis[1],
        options = list(`actions-box` = F, size = 20),
        multiple = F)
    }
  })
  outputOptions(output, "hab_subset", suspendWhenHidden=F)
  
  # habitat data sub #####
  hab_data_sub <- reactive({
    
    slct_hab <- colnames(df_bio)[which(colnames(df_bio) %in% input$hab_type_dwlnd)]
    
    data_sub <-
      df_bio %>% 
      dplyr::filter(year >= input$hab_wy[1] & year <= input$hab_wy[2],
                    ws %in% input$hab_ws) %>%
      dplyr::mutate(year = factor(year, levels = seq(min(bio_vars_yr), max(bio_vars_yr), 1))) %>%
      dplyr::select(c("rmc_id","wb_id","assmnt_type","year","sample_date", slct_hab, "long", "lat", "ws", "subws", "creek"))  %>%
      arrange(desc(year), ws) 
    
    
    return(as.data.frame(data_sub))
  })
  
  
  # habitat Download #########
  
  
  
  # create shapefile for download 
  hab_shp <- reactive({
    
    xyPoints <- hab_data_sub() %>% 
      dplyr::filter(!is.na(long)) %>%
      dplyr::mutate(long=as.numeric(long), lat=as.numeric(lat)) %>% 
      dplyr::arrange(rmc_id)
    
    
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,c("long","lat")], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    
    return(SHP)
    
  })
  
  
  
  # Download user selected file type of selected dataset 
  output$hab_downloadData <- downloadHandler(
    
    filename = function() { 
      if (!input$hab_file_type == ".shp"){
        paste("SC_bioassmnt", "_table", Sys.Date(), input$hab_file_type, sep = "")
      }
      else {paste0("shpExport.zip")} 
    },
    
    content = function(file) {
      if(input$hab_file_type== ".csv") {
        write.csv(hab_data_sub(), file, row.names = FALSE)
      } else if(input$hab_file_type == ".xlsx") {
        write.xlsx(hab_data_sub(), file)
      }
      if (input$hab_file_type == ".shp"){
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
        writeOGR(hab_shp(), dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
        zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"),zip = Sys.getenv("R_ZIPCMD", "zip"))
        file.copy("shpExport.zip", file)
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
      }
    }
  )
  
  # Map Habitat #####
  # MAP title
  output$map_hab_title <- renderText({
    filter_name <-
      param_names[which(param_names$dataname == input$hab_map), "fullname"]
    paste(
      "Map of ",
      filter_name,
      "for ",
      input$hab_wy[1],
      " - ",
      input$hab_wy[2]
    )
  })
  
  # baseline map
  output$map_hab <- renderLeaflet({
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
    input$hab_reset_button
    leafletProxy("map_hab") %>% setView(lat = 37.3,
                                        lng = -121.8,
                                        zoom = 9)
  })
  
  # Update based on user inputs
  observe({
    req(input$menu_items == "habitat")
    
    hab_name <-  input$hab_map
    hab_full_name <- param_names[which(param_names$dataname == input$hab_map), "fullname"]
    
    threshold1 <-
      bio_vars_filter$threshold1[which(bio_vars_filter$param == hab_name)]
    threshold2 <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == hab_name)]
    threshold3 <-
      bio_vars_filter$threshold3[which(bio_vars_filter$param == hab_name)]
    
    # data subset 
    data_sub <- hab_data_sub() %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == hab_name, 
                    !is.na(value))
    
    # colors for map
    
    if (input$hab_map == "tot_phab"){
      col.bins <- colorBin(palette=unname(colors_bio), bins=c(0, threshold1, threshold2, threshold3, 60), domain=data_sub$value, reverse=T)
    }
    else{if (input$hab_map == "crhdi_swamp") {
      col.bins <- colorBin(palette="Blues", bins=5, pretty=T, domain=data_sub$value, reverse=F)
    }
    else {
      col.bins <- colorBin(palette="Blues", bins=5, pretty=T, domain=data_sub$value, reverse=T)
    }} 
    
    # subset watersheds shapefile based on user input
    sheds_sub <- sheds[sheds$SYSTEM %in% input$hab_ws, ]
    
    
    # pop-up window for individual sites
    content <- paste(
      sep = "<br/>",
      "<b>Watershed:</b>",
      paste0(data_sub$ws, " (",data_sub$creek, ")"),
      "<b>RMC Site ID:</b>",
      data_sub$rmc,
      "<b>Water Year:</b>",
      data_sub$year, 
      paste0("<b>",hab_full_name,"</b>"), 
      data_sub$value
    )
    
    # update map
    leafletProxy("map_hab") %>% clearMarkers() %>% clearControls() %>% clearShapes()
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_hab") %>%
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
        ) %>% 
        addLegend( title= hab_full_name,position="topright", pal=col.bins, values=c())
    }
  }
  )
  
  
  # summary boxplot  #####
  output$hab_boxplot_title <- renderText({
    hab_full_name <-
      param_names[which(param_names$dataname == input$hab_map), "fullname"]
    
    paste(
      "Boxplot of ",
      hab_full_name,
      "for ",
      input$hab_wy[1],
      " - ",
      input$hab_wy[2]
    )
  })  
  
  
  output$hab_boxplot <- renderPlot({
    hab_name <- input$hab_map
    hab_full_name <- param_names[which(param_names$dataname == input$hab_map), "fullname"]
    
    data_sub <- hab_data_sub()  %>% 
      reshape2::melt(id=c("rmc_id","wb_id", "assmnt_type", "year", "sample_date", "lat", "long", "ws", "subws", "creek" )) %>% 
      dplyr::filter(variable == hab_name) 
    
    n="ws"
    if (length(unique(data_sub[,n]))==1){n <- "subws"}
    n_name <- ifelse(n=="ws",'Watershed',"Subwatershed")
    
    
    
    p <-
      ggplot(data_sub, aes_string(x = n, y = "value")) + geom_boxplot(col = rgb(0, 0, 1, 0.6)) +
      xlab(n_name) + ylab(hab_full_name) +
      stat_summary(
        fun.data = give_tot,
        geom = "text",
        fun.y = median,
        position = position_dodge(width = 0.75)
      ) + 
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size=12))
    
    return(p + theme(text=element_text(size=12)))
    
    
  })
  
  
  
  
  
  
  
  ##### 
  
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
  
  # pH ##################################################################################################
  #################################################################################################################################
  
  
  # Explanatory Popup #############
  observeEvent(input$ph_desc,{
    showModal(modalDialog(
      title = "pH Monitoring",
      HTML('Description of datasets and methods"')
      ,
      
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  observeEvent(input$map_ph_desc,{
    showModal(modalDialog(
      title = "pH Map",
      HTML("For the purpose of this map, the color scale corresponds to the average of all the pH data points over the selected time period. This color scale is only a <i>relative</i> scale, 
           it does not indicate water quality objective exceedances. Rather, it's a simplified way to compare sites with one another.
           "),
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  # Subset ##### 
  ph_subset <- reactive({ 
    filter_dates <- as.Date(cut(as.POSIXct(input$ph_dates,tz=''),"month"))
    
    if (input$ph_data_type == "ph_grab") {
      data_sub <- df_bio %>% 
        dplyr::select(c("rmc_id", "ws", "subws", "creek", "long", "lat", "sample_date", "ph")) %>% 
        dplyr::filter(as.Date(sample_date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(sample_date) - as.Date(filter_dates[2])<= 0, 
                      ws %in% input$ph_ws_dwld) %>% 
        dplyr::rename(site_id=rmc_id)
    }
    
    
    if(input$ph_data_type == "ph_seas") { 
      
      data_sub <- df_wq %>%
        as.data.frame() %>%
        dplyr:: select(c("site_id", "lat", "long", "ws", "creek", "date", "season", "ph")) %>%  
        dplyr::filter(
          as.Date(date) - as.Date(filter_dates[1])>= 0 & 
            as.Date(date) - as.Date(filter_dates[2])<= 0,
          ws %in% input$ph_ws_dwld) 
    }  
    
    
    return(data_sub)
  })
  

  
  # Download #### 
  output$downloadData_ph <- downloadHandler(
    
    filename = function() {
      paste(input$ph_data_type, "_table", Sys.Date(), input$file_type_ph, sep = "")
    },
    
    content = function(file) {
      
      
      data_to_dwn <-ph_subset()
      
      if(input$file_type_ph== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$file_type_ph == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  
  # Map ###########
  output$map_ph <-  renderLeaflet({
    
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
    req(input$menu_items == "ph")
    
    filter_dates <- as.Date(cut(as.POSIXct(input$ph_dates,tz=''),"month"))
    data_sub <- ph_subset()
    
    leafletProxy("map_ph")  %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    
    if (nrow(data_sub)>0) {
  
      
      if (input$ph_data_type == "ph_seas"){
        data_sub <- data_sub %>% 
          dplyr::group_by(site_id, ws, creek, long, lat) %>% 
          dplyr::summarize(ph = round( mean(ph, na.rm=T),2))
        
        popup_ph <- paste(
          sep = "<br/>",
          "<b>Site:</b>",
          data_sub$site_id,
          "<b>Watershed:</b>",
          data_sub$ws, 
          "<b>Mean pH Over Period:</b>",
          paste0(data_sub$ph)
        )
        
        
      }
      
      
      if (input$ph_data_type == "ph_grab") {
        popup_ph <- paste(
          sep = "<br/>",
          "<b>Site:</b>",
          data_sub$site_id,
          "<b>Watershed:</b>",
          data_sub$ws, 
          "<b>pH:</b>",
          paste0(data_sub$ph, " (", data_sub$sample_date, ")")
        )
        
        
      } 
      
      ph.colBins <- colorBin(palette="Blues", pretty=F, bins= 6, domain=data_sub$ph, reverse=F)  
      
      
      #shapes: 15 = square, 16= circle, 17 = triangle
      leafletProxy("map_ph")  %>% 
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
          fillColor = ~ph.colBins(ph),       
          popup = popup_ph,
          options = leafletOptions(pane = "markers")
        ) %>% 
        addLegend(title= "pH values",pal= ph.colBins, values=c()) 
    } 
  })
  
  
  # Summary plot #####
  output$ph_plot <- renderPlot({
    if (nrow(ph_subset())>0){
      
      
      if(input$ph_data_type == "ph_grab"){ 
        p <- ggplot(data=ph_subset(), aes(x=ph)) + geom_histogram()
      }
      
      if(input$ph_data_type == "ph_seas"){ 
        p <- (ggplot(data=ph_subset(), aes(x=creek, y= ph, fill=season)) +
                geom_violin() + 
                theme(legend.position = "top", legend.title = element_blank()) + 
                xlab("") + ylab("pH")) + 
          scale_fill_manual(values= c("lightskyblue", "rosybrown"), labels=c("Spring", "Fall"))
        
      }
      
      
    }
    
    return(p)
  })

  
  
  
  ###### 
  
  
  
  # DO ##################################################################################################
  #################################################################################################################################
  
  
  # Explanatory Popup #############
  observeEvent(input$do_desc,{
    showModal(modalDialog(
      title = "Dissolved Oxygen Monitoring",
      HTML('Description of datasets and methods"')
      ,
      
      easyClose = TRUE, footer=modalButton("Got it!")
    ))
  })
  
  observeEvent(input$map_do_desc,{
    showModal(modalDialog(
      title = "Dissolved Oxygen (DO) Map",
      HTML("For the purpose of this map, the color scale corresponds to the average of all the DO data points over the selected time period. This color scale is only a <i>relative</i> scale, 
           it does not indicate water quality objective exceedances. Rather, it's a simplified way to compare sites with one another.
           "),
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  # Subset ##### 
  do_subset <- reactive({ 
    filter_dates <- as.Date(cut(as.POSIXct(input$do_dates,tz=''),"month"))
    
    if (input$do_data_type == "do_grab") {
      data_sub <- df_bio %>% 
        dplyr::select(c("rmc_id", "ws", "subws", "creek", "long", "lat", "sample_date", "do_mg_l")) %>% 
        dplyr::filter(as.Date(sample_date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(sample_date) - as.Date(filter_dates[2])<= 0, 
                      ws %in% input$do_ws_dwld) %>% 
        dplyr::rename(site_id=rmc_id)
    }
    
    
    if(input$do_data_type == "do_seas") { 
      
      data_sub <- df_wq %>%
        as.data.frame() %>%
        dplyr:: select(c("site_id", "lat", "long", "ws", "creek", "date", "season", "do_mg_l")) %>%  
        dplyr::filter(
          as.Date(date) - as.Date(filter_dates[1])>= 0 & 
            as.Date(date) - as.Date(filter_dates[2])<= 0,
          ws %in% input$do_ws_dwld) 
    }  
    
    
    return(data_sub)
  })
  
  
  
  # Download #### 
  output$downloadData_do <- downloadHandler(
    
    filename = function() {
      paste(input$do_data_type, "_table", Sys.Date(), input$file_type_do, sep = "")
    },
    
    content = function(file) {
      
      
      data_to_dwn <-do_subset()
      
      if(input$file_type_do== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$file_type_do == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  
  # Map ###########
  output$map_do <-  renderLeaflet({
    
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
    req(input$menu_items == "do")
    
    filter_dates <- as.Date(cut(as.POSIXct(input$do_dates,tz=''),"month"))
    data_sub <- do_subset()
    
    leafletProxy("map_do")  %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    
    if (nrow(data_sub)>0) {
      
      
      if (input$do_data_type == "do_seas"){
        data_sub <- data_sub %>% 
          dplyr::group_by(site_id, ws, creek, long, lat) %>% 
          dplyr::summarize(do_mg_l = round( mean(do_mg_l, na.rm=T),2))
        
        popup_do <- paste(
          sep = "<br/>",
          "<b>Site:</b>",
          data_sub$site_id,
          "<b>Watershed:</b>",
          data_sub$ws, 
          "<b>Mean pH Over Period:</b>",
          paste0(data_sub$do_mg_l)
        )
        
        
      }
      
      
      if (input$do_data_type == "do_grab") {
        popup_do <- paste(
          sep = "<br/>",
          "<b>Site:</b>",
          data_sub$site_id,
          "<b>Watershed:</b>",
          data_sub$ws, 
          "<b>pH:</b>",
          paste0(data_sub$do_mg_l, " (", data_sub$sample_date, ")")
        )
        
        
      } 
      
      do.colBins <- colorBin(palette="Blues", pretty=F, bins= 6, domain=data_sub$do_mg_l, reverse=F)  
      
      
      #shapes: 15 = square, 16= circle, 17 = triangle
      leafletProxy("map_do")  %>% 
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
          fillColor = ~do.colBins(do_mg_l),       
          popup = popup_do,
          options = leafletOptions(pane = "markers")
        ) %>% 
        addLegend(title= "DO values",pal= do.colBins, values=c()) 
    } 
  })
  
  
  # Summary plot #####
  output$do_plot <- renderPlot({
    if (nrow(do_subset())>0){
      
      
      if(input$do_data_type == "do_grab"){ 
        p <- ggplot(data=do_subset(), aes(x=do_mg_l)) + geom_histogram()
      }
      
      if(input$do_data_type == "do_seas"){ 
        p <- (ggplot(data=do_subset(), aes(x=creek, y= do_mg_l, fill=season)) +
                geom_violin() + 
                theme(legend.position = "top", legend.title = element_blank()) + 
                xlab("") + ylab("DO (mg/L)")) + 
          scale_fill_manual(values= c("lightskyblue", "rosybrown"), labels=c("Spring", "Fall"))
        
      }
      
      
    }
    
    return(p)
  })
  
  
  
  
  ###### 
  
  # Spe Cond ##################################################################################################
  #################################################################################################################################
  
  
  # Explanatory Popup #############
  observeEvent(input$sp_cond_desc,{
    showModal(modalDialog(
      title = "Specific Conductivity Monitoring",
      HTML('Description of datasets and methods')
      ,
      
      easyClose = TRUE, footer=modalButton("Got it!")
    ))
  })
  
  observeEvent(input$map_sp_cond_desc,{
    showModal(modalDialog(
      title = "Specific Conductivity Map",
      HTML("For the purpose of this map, the color scale corresponds to the average of all the specifici conductivity data points over the selected time period. This color scale is only a <i>relative</i> scale, 
           it does not indicate water quality objective exceedances. Rather, it's a simplified way to compare sites with one another.
           "),
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  # Subset ##### 
  sp_cond_subset <- reactive({ 
    filter_dates <- as.Date(cut(as.POSIXct(input$sp_cond_dates,tz=''),"month"))
    
    if (input$sp_cond_data_type == "sp_cond_grab") {
      data_sub <- df_bio %>% 
        dplyr::select(c("rmc_id", "ws", "subws", "creek", "long", "lat", "sample_date", "sp_cond_us_cm")) %>% 
        dplyr::filter(as.Date(sample_date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(sample_date) - as.Date(filter_dates[2])<= 0, 
                      ws %in% input$sp_cond_ws_dwld) %>% 
        dplyr::rename(site_id=rmc_id)
    }
    
    
    if(input$sp_cond_data_type == "sp_cond_seas") { 
      
      data_sub <- df_wq %>%
        as.data.frame() %>%
        dplyr:: select(c("site_id", "lat", "long", "ws", "creek", "date", "season", "sp_cond_us_cm")) %>%  
        dplyr::filter(
          as.Date(date) - as.Date(filter_dates[1])>= 0 & 
            as.Date(date) - as.Date(filter_dates[2])<= 0,
          ws %in% input$sp_cond_ws_dwld) 
    }  
    
    
    return(data_sub)
  })
  
  
  
  # Download #### 
  output$downloadData_sp_cond <- downloadHandler(
    
    filename = function() {
      paste(input$sp_cond_data_type, "_table", Sys.Date(), input$file_type_sp_cond, sep = "")
    },
    
    content = function(file) {
      
      
      data_to_dwn <-sp_cond_subset()
      
      if(input$file_type_sp_cond== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$file_type_sp_cond == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  
  # Map ###########
  output$map_sp_cond <-  renderLeaflet({
    
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
    req(input$menu_items == "sp_cond")
    
    filter_dates <- as.Date(cut(as.POSIXct(input$sp_cond_dates,tz=''),"month"))
    data_sub <- sp_cond_subset()
    
    leafletProxy("map_sp_cond")  %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    
    if (nrow(data_sub)>0) {
      
      
      if (input$sp_cond_data_type == "sp_cond_seas"){
        data_sub <- data_sub %>% 
          dplyr::group_by(site_id, ws, creek, long, lat) %>% 
          dplyr::summarize(sp_cond_us_cm = round( mean(sp_cond_us_cm, na.rm=T),2))
        
        popup_sp_cond <- paste(
          sep = "<br/>",
          "<b>Site:</b>",
          data_sub$site_id,
          "<b>Watershed:</b>",
          data_sub$ws, 
          "<b>Mean Sp. Conductivity (uS/cm) Over Period:</b>",
          paste0(data_sub$sp_cond_us_cm)
        )
        
        
      }
      
      
      if (input$sp_cond_data_type == "sp_cond_grab") {
        popup_sp_cond <- paste(
          sep = "<br/>",
          "<b>Site:</b>",
          data_sub$site_id,
          "<b>Watershed:</b>",
          data_sub$ws, 
          "<b>Sp. Conductivity (uS/cm):</b>",
          paste0(data_sub$sp_cond_us_cm, " (", data_sub$sp_cond_us_cm, ")")
        )
        
        
      } 
      
      sp_cond.colBins <- colorBin(palette="Blues", pretty=F, bins= 6, domain=data_sub$sp_cond_us_cm, reverse=F)  
      
      
      #shapes: 15 = square, 16= circle, 17 = triangle
      leafletProxy("map_sp_cond")  %>% 
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
          fillColor = ~sp_cond.colBins(sp_cond_us_cm),       
          popup = popup_sp_cond,
          options = leafletOptions(pane = "markers")
        ) %>% 
        addLegend(title= "Specific Conductivity (uS/cm)",pal= sp_cond.colBins, values=c()) 
    } 
  })
  
  
  # Summary plot #####
  output$sp_cond_plot <- renderPlot({
    if (nrow(sp_cond_subset())>0){
      
      
      if(input$sp_cond_data_type == "sp_cond_grab"){ 
        p <- ggplot(data=sp_cond_subset(), aes(x=sp_cond_us_cm)) + geom_histogram()
      }
      
      if(input$sp_cond_data_type == "sp_cond_seas"){ 
        p <- (ggplot(data=sp_cond_subset(), aes(x=creek, y= sp_cond_us_cm, fill=season)) +
                geom_violin() + 
                theme(legend.position = "top", legend.title = element_blank()) + 
                xlab("") + ylab("Specific Conductivity (uS/cm)")) + 
          scale_fill_manual(values= c("lightskyblue", "rosybrown"), labels=c("Spring", "Fall"))
        
      }
      
      
    }
    
    return(p)
  })
  
  
  
  
  ######
  
  # Chlorine ##################################################################################################
  #################################################################################################################################
  
  # # Subset ##### 
  chlor_subset <- reactive({ 
    filter_yr <- input$chlor_vars_yr
    
      data_sub <- df_chlo %>% 
        dplyr::filter(year >= filter_yr[1] & 
                        year <= filter_yr[2], 
                      ws %in% input$chlor_ws, 
                      AnalyteName %in% input$chlor_type_dwlnd)
    return(data_sub)
  })
  
  
  
  # Download #### 
  output$chlor_downloadData <- downloadHandler(
    
    filename = function() {
      paste0("chlorine_table_", Sys.Date(), input$chlor_file_type)
    },
    
    content = function(file) {
      
      data_to_dwn <-chlor_subset()
      
      if(input$chlor_file_type== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$chlor_file_type == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  # Map ###########
  output$map_chlor <-  renderLeaflet({
    
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
    req(input$menu_items == "chlorine")
    
    filter_yr <- input$chlor_vars_yr
    filter_analyte <- input$chlor_type_dwlnd
    
    sites_chlo_sub <- sites_chlo %>% 
      dplyr::filter(year >= filter_yr[1] & 
                      year <= filter_yr[2], 
                    ws %in% input$chlor_ws) 
    
    sheds_sub <- sheds[sheds$SYSTEM %in% input$chlor_ws, ]
    
    popup_chlor <- paste(
      sep="</br>", 
      sites_chlo_sub$station_code, 
      sites_chlo_sub$collps_all_txt)
    
    get_color_chlor <- function(var) {
      
      var <- lapply(var, remove_num_0)
      
      field_rep_1 <- unlist(lapply(var, `[[`, 1))
      return(ifelse(field_rep_1 < 0.1, "green",
                    "red"))
    }
    
    # with sites df
    leafletProxy("map_chlor") %>% clearMarkers() %>% clearShapes() %>% clearControls() 
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_chlor") %>%
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
          ), options= leafletOptions(pane="polygons")) %>% 
        addCircleMarkers(data=sites_chlo_sub, lat=sites_chlo_sub$lat, lng=sites_chlo_sub$long, 
                         radius=7, 
                         weight=1, color="blue", fillColor=get_color_chlor(sites_chlo_sub$collps_tot_date), 
                         fillOpacity = 0.9) %>%
        addCircleMarkers(data=sites_chlo_sub, lat=sites_chlo_sub$lat, lng=sites_chlo_sub$long, 
                         radius=4, 
                         weight=1, color="blue", fillColor=get_color_chlor(sites_chlo_sub$collps_free_date), 
                         fillOpacity = 0.9,
                         popup=popup_chlor, options= leafletOptions(pane="markers")) %>% 
        addLegendCustom(position="topleft", shapes=rep("circle",4), size=c(14,rep(10,3)), 
                        colors = c(rep("white",2), "green", "red"),
                        labels=c("Total Chlorine", "Free Chlorine", "< 0.1 mg/L", "> 0.1 mg/L"))
      
    } 
  })
  
  
  # Summary plot #####
  output$chlor_plot <- renderPlot({
    if (nrow(chlor_subset())>0){
      
      p <- ggplot(data=chlor_subset(), aes(x=Result)) + geom_histogram() + 
        facet_wrap(~AnalyteName, nrow=2)
    
      
       }
      
      
    
    return(p)
  })
  
  
  
  
  
  
  
  
  # Trash ########################################################################################################################
  #################################################################################################################################
  
  # Data subset #####
  data_sub_trash <- reactive({
    filter_dates <- as.Date(cut(input$trash_wy,"month"))
    
    data_sub <- df_trash %>% 
      dplyr::filter(juris %in% input$trash_city,
                    as.Date(date) - as.Date(filter_dates[1])>= 0 & 
                      as.Date(date) - as.Date(filter_dates[2])<= 0,
                    assmnt_type %in% input$trash_assmnt_type

                    )
    
    return(data_sub)
  })
  
  
  # Data Download #####
  # create shapefile for download 
  trash_shp <- reactive({
    
    xyPoints <- data_sub_trash()
    
    
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,c("dsLong","dsLat")], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    
    return(SHP)
    
  })
  
  
  
  # Download user selected file type of selected dataset 
  output$trash_downloadData <- downloadHandler(
    
    filename = function() { 
      if (!input$trash_file_type == ".shp"){
        paste("SC_trash", "_table", Sys.Date(), input$trash_file_type, sep = "")
      }
      else {paste0("shpExport.zip")} 
    },
    
    content = function(file) {
      if(input$trash_file_type== ".csv") {
        write.csv(data_sub_trash(), file, row.names = FALSE)
      } else if(input$trash_file_type == ".xlsx") {
        write.xlsx(data_sub_trash(), file)
      }
      if (input$trash_file_type == ".shp"){
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
        writeOGR(trash_shp(), dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
        zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"),zip = Sys.getenv("R_ZIPCMD", "zip"))
        file.copy("shpExport.zip", file)
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
      }
    }
  )
  
  
  # Map Trash ######
  
  output$trash_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -121.8,
              lat = 37.3,
              zoom = 9) %>% 
      addLegend(position="topright", title= "Trash Categories",colors=c("seagreen",col_trashCat[2:4]), labels=c("Low", "Moderate","High", "Very High"))
  })
  
  
  
  
  observe({
    
    req(input$menu_items == "trash")
    n= 3
    
    data_group <- data_sub_trash() %>%
      arrange(date) %>%
      group_by(siteID, dsLat, dsLong) %>% 
      summarise(n_col = sum(as.numeric(trashCat))*n/n()*10, 
                popup = paste( paste0(trashCat, " (", Event, ")"), collapse=", ")) %>% 
      as.data.frame() %>% 
      mutate(usLong=as.numeric(dsLong), 
             usLat = as.numeric(dsLat)) %>% 
      filter(!is.na(dsLat))
    
    palette <- colorRampPalette(c("white", "seagreen",col_trashCat[2:4]))
    color_bins <- palette(4*n*10)
    
    popup <- paste(sep="<br>", 
                   data_group$siteID, 
                   data_group$popup)
    
    leafletProxy("trash_map") %>% clearMarkers() %>%
     addCircleMarkers(data=data_group,
                       lng=data_group$dsLong,
                       lat=data_group$dsLat, 
                       radius = 4,
                       weight = 1,
                       popup= popup,
                       fillCol=color_bins[data_group$n_col],
                       opacity = 0.9,
                       fill = T,
                       fillOpacity = 0.9)
    
    
 
  })
  
  #####
  
  
  }


