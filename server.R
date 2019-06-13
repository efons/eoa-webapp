# EOA's website redesign
# Data visualization app
# Emilie Fons
# Michaela Palmer


# Script 3 - Server function


# Server function
server <- (function(input, output, session) {

  
  
## Bioassessment #################################################################################################################################        
#################################################################################################################################################
  
# Explanatory pop-up windows: bioassessments, scores ##########################
  
  # Help bio tab : description of the tab in popup window
  observeEvent(input$bio_tab_title,{
    showModal(modalDialog(
      title = "Biological Condition Assessment",
      HTML('San Francisco Bay Area Stormwater Municipal Regional Permit (MRP), Provision C.8. on Water Quality Monitoring, 
      section C.8.d.(i) on Biological Assessments: "The Permittees shall conduct biological
           assessments (also referred to herein as bioassessments) in accordance with
           SWAMP Standard Operating Procedures and shall include collection
           and reporting of in-stream biological and physical habitat data according to
           the SWAMP Standard Operating Procedures for Bioassessment, including
           benthic algae, benthic macroinvertebrates, water chemistry, and full
           characterization of physical habitat."
      <br/>
      <br/>

      The collected bioassessment data is aggregated to compute biological health indicators. Several indicators are currently in use in California: 
      <ul><li> California Stream Condition Index (CSCI) for benthic macroinvertebrates </li>
          <li> Algae Stream Condition Index (ASCI) for Soft Algae </li>
          <li> Algae Stream Condition Index (ASCI) for Diatoms </li>
          <li> Hybrid Algae Stream Condition Index (ASCI) for both Soft Algae and diatoms </li></ul>

      <br/> 
      <br/>

      <i> Instructions: Start by selecting an indicator of creek health. Each indicator is computed based on the diversity and number of individuals for a specific aquatic species: benthic macrovertebrates, algae, etc. 
              Environmental stress (chemistry, nutrient availability, physical habitat, etc) might affect the survival of these species. Therefore, these biological measures
              can be used as indicators of creek health.This dashboard allows you to visualize the biological health of creeks
              throughout the Santa Clara Valley, and to investigate the role played by environmental stress on biological condition.</i>')
      ,
      easyClose = TRUE, footer=modalButton("Got it!")
    ))
  })
  
  # description of the selected score in popup window
  score_desc_txt <- reactive({
    score <- input$filter_by 
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
    df <- bio_vars_filter %>% dplyr::filter(param == input$filter_by) %>% 
      dplyr::select(seq(3,6)) %T>% 
      {names(.) <- c("Very Likely Altered","Likely Altered" ,"Possibly Intact", "Likely Intact")} %>% t()
    
    df[1,] <- paste(df[1,], " - ", df[2,])
    df[2,] <- paste(df[2,], " - ", df[3,])
    df[3,] <- paste(df[3,], " - ", df[4,])
    df[4,] <- paste(" > ", df[4,])

    
    col=seq(4,1,-1)
    name <- as.character(bio_vars_filter[bio_vars_filter$param == input$filter_by,'name'])
    df <- data.frame(df, colors=col) 
    colnames(df) <-c(name,"colors")
    
    
    df <- df %>% datatable(options = list(autoWidth=F,columnDefs = list(list(targets=2, visible=F)), dom='t', bSort=F)) %>% 
      formatStyle(1,2,backgroundColor=styleEqual(col,colors_bio[col]), color=styleEqual(col,c("white",rep("black",3)))) %>%
      formatStyle( 0, target= 'row',lineHeight='70%')
    
    return(df)
  })
  
  observeEvent(input$score_popup,{
    showModal(modalDialog(
      title = "Score Description",
      HTML(score_desc_txt()),
      DT::renderDataTable( score_desc_table()),
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  
# RenderUI ##### 
  output$score_subset <- renderUI({
    
    if (length(input$score_dwlnd)>0){
      choices_score_vis <- input$score_dwlnd
      names(choices_score_vis) <- bio_vars_filter[match(choices_score_vis, bio_vars_filter$param),"name"]
      
      radioButtons(
      inputId = "filter_by",
      label = NULL,
      choices = choices_score_vis,
      selected =  input$score_dwlnd[1])
    }
  })
  outputOptions(output, "score_subset", suspendWhenHidden=F)
  
  
  # Data subsetting ###########################################################
  # Reactive function that returns the subsetted data table
  data_sub <- reactive({

    filter_by <- input$filter_by
    threshold1 <-
      bio_vars_filter$threshold1[which(bio_vars_filter$param == filter_by)]
    threshold2 <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == filter_by)]
    threshold3 <-
      bio_vars_filter$threshold3[which(bio_vars_filter$param == filter_by)]
    filter_name <-
      as.character(bio_vars_filter$name[which(bio_vars_filter$param == filter_by)])
    column_names <- c("RMC ID",
                      "Watershed",
                      "Sub-watershed",
                      "Creek",
                      "Year",
                      filter_name)
    
    data_sub <-
      df_bio %>% dplyr::filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      dplyr::filter(ws %in% input$ws) %>%
      dplyr::mutate(year = factor(year, levels = seq(
        min(bio_vars_yr), max(bio_vars_yr), 1
      ))) %>%
      dplyr::select(c("rmc_id", "ws", "subws","creek", "year", filter_by))  %>%
      arrange(desc(year), ws) %T>%
      { 
        names(.) <- column_names
      }
    
    
    return(as.data.frame(data_sub))
  })
  


  
  # Data download tool #######################################################  
  # Create data table for download
  bio_table_dwld <- reactive({
    score_slct <- which(colnames(df_bio) %in% input$score_dwlnd)
    col_slct <- as.character(param_names$dataname)
    col_names <- as.character(param_names$fullname)
    
    data_sub <-
      df_bio %>% dplyr::filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      dplyr::filter(ws %in% input$ws
      ) %>%
      dplyr::mutate(year = factor(year, levels = seq(
        min(bio_vars_yr), max(bio_vars_yr), 1
      ))) %>%
      dplyr::select("rmc_id","wb_id",col_slct[c(1:5, 8:47)]) %>% 
      dplyr::arrange(desc(sample_date)) %T>% 
      {names(.) <- c("RMC Station ID","Water Board ID",col_names[c(1:5, 8:47)])} 
    
    return(data_sub)
  })
  
    # create shapefile for download 
  bio_shp <- reactive({
    
    col_slct <- as.character(param_names$dataname)
    xyPoints <- bio_table_dwld()  %T>% 
    {names(.) <- c("rmc_id","wb_id",col_slct[c(1:5, 8:47)])} %>% 
      dplyr::filter(!is.na(long)) %>%
      dplyr::mutate(long=as.numeric(long), lat=as.numeric(lat)) %>% 
      dplyr::arrange(rmc_id)
 
    
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,c(7,6)], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    
    return(SHP)

  })
  

  
  # Download user selected file type of selected dataset 
  output$downloadData <- downloadHandler(
    filename = function() { 
      if (!input$file_type == ".shp"){
              paste("SC_bioassmnt", "_table", Sys.Date(), input$file_type, sep = "")
      }
      else {paste0("shpExport.zip")} 
    },
  
    content = function(file) {
      
       if(input$file_type== ".csv") {
        write.csv(bio_table_dwld(), file, row.names = FALSE)
      } else if(input$file_type == ".xlsx") {
        write.xlsx(bio_table_dwld, file)
      }
      if (input$file_type == ".shp"){
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
  
  
  

  # Score overview: Map + barplot + boxplot ###########################################
  
  # MAP
  output$map_title <- renderText({
    filter_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)]
    paste(
      "Map of ",
      filter_name,
      "for ",
      input$wy[1],
      " - ",
      input$wy[2]
    )
  })  # Map title
  
  # Create baseline map
  output$map_sites <- renderLeaflet({
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
    input$reset_button
    leafletProxy("map_sites") %>% setView(lat = 37.3,
                                          lng = -121.8,
                                          zoom = 9)
  })
  
  # Update based on user inputs
  observe({
    req(input$menu_items == "bio_data")
    
    # other inputs
    filter_by <- input$filter_by
    threshold1 <-
      bio_vars_filter$threshold1[which(bio_vars_filter$param == filter_by)]
    threshold2 <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == filter_by)]
    threshold3 <-
      bio_vars_filter$threshold3[which(bio_vars_filter$param == filter_by)]
    filter_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == filter_by)]

    # subset data based on user input
    data_sub <-
      df_bio %>% filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      filter(ws %in% input$ws)
    data_sub <-
      cbind(data_sub, data_sub[, which(colnames(data_sub) == filter_by)])
    colnames(data_sub)[ncol(data_sub)] <- "filter"
      
    
    
    # subset watersheds shapefile based on user input
    sheds_sub <- sheds[sheds$SYSTEM %in% input$ws, ]
    
    
    
    # pop-up window for individual sites
    content <- paste(
      sep = "<br/>",
      "<b>Watershed:</b>",
      paste0(data_sub$ws, " (",data_sub$creek, ")"),
      "<b>RMC Site ID:</b>",
      data_sub$rmc,
      "<b>Water Year:</b>",
      data_sub$year, 
      paste0("<b>",filter_name,"</b>"), 
      signif(data_sub$filter,2)
    )
    
    # Customize color and size of circle markers
    getColor <- function() {
      ifelse(
        is.na(data_sub$filter),"white",
        ifelse(
          data_sub$filter < threshold1,
          colors_bio[4],
          ifelse(
            data_sub$filter < threshold2,
            colors_bio[3],
            ifelse(
              data_sub$filter < threshold3,
              colors_bio[2],
              ifelse(threshold3 < 0, "white", colors_bio[1])
            )
          )
        )
      )
    }
    
    # update map
    leafletProxy("map_sites") %>% clearMarkers() %>% clearControls() %>% clearShapes()
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_sites") %>%
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
        addCircleMarkers(
          group = "circles",
          layerId = data_sub$rmc_id,
          lng = data_sub$long,
          lat = data_sub$lat,
          radius = 4,
          weight = 1,
          opacity = 0.9,
          popup = content,
          fill = T,
          fillColor = getColor(),
          fillOpacity = 0.6,
          options = leafletOptions(pane = "markers")
        ) %>%
        addLegendCustom(
          position = "topright",
          opacity = 1,
          colors = c(colors_bio, "white"),
          labels = c(
            "Likely Intact",
            "Possibly Intact",
            "Likely Altered",
            "Very Likely Altered",
            "No Data"
          ),
          sizes = c(rep(12, 5)),
          shapes = rep("circle", 5),
          title = as.character(filter_name)
        )
      
      #if(input$show_creeks == "yes")
      #{
      #leafletProxy("map_sites") %>%
      # addPolylines(data=creeks, weight=1, color="black")
      #}
    }
  })
  

  # Summary barplot 
  output$barplot_title <- renderText({
    filter_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)]
    paste(
      "Barplot of ",
      filter_name,
      "for ",
      input$wy[1],
      " - ",
      input$wy[2]
    )
  })  
  
  output$barplot <- renderPlot({
    data_sub <- data_sub() 
    
    name_score <- colnames(data_sub)[6]
    data_sub$score_cat <- cut(x=data_sub[,6], breaks=c(bio_vars_filter[bio_vars_filter$name == name_score,3:6],1000),
                              labels=c("Very Likely Altered", "Likely Altered","Possibly Intact", "Likely Intact"))
    
    data_sub <- data_sub %>% 
      dplyr::filter(!is.na(score_cat))
    n=2
    
    if (length(unique(data_sub[,2]))==1){n <- 3}
    n_name <- ifelse(n==2,'Watershed',"Subwatershed")
    
    if (!input$show_bar_pct){
          p <- ggplot(data = data_sub, aes(x = data_sub[,n], fill = score_cat)) + geom_bar()+ ylab("Number of samples")
    }
    
    else {
      data_sub <- data_sub  %>% 
        dplyr::rename("x_axis"=!!names(.[n]),
              "score" = !!names(.[6])) %>% 
        dplyr::group_by(x_axis, score_cat, score) %>% 
        dplyr::summarise(n = n()) 

      p <- ggplot(data = data_sub, aes(x = x_axis, y=n, fill = score_cat)) + geom_bar(position="fill",stat='identity') + ylab("Percentage of samples") +
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
  output$boxplot_title <- renderText({
    filter_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)]
    paste(
      "Boxplot of ",
      filter_name,
      "for ",
      input$wy[1],
      " - ",
      input$wy[2]
    )
  })  
  
  bio_boxplot <- function(data_sub_plots,
                          threshold = -1) {
    data_sub <- data_sub()

      x_var <- data_sub[,2]
      x_var_name <- "Watersheds"
      if (length(unique(x_var))==1) {x_var <- data_sub[,3]
      x_var_name="Subwatersheds"}
      
      y_var <- data_sub[, 6]
      df_tempo <- data.frame(x_var = x_var, y_var = y_var)
      p <-
        ggplot(df_tempo, aes(x = x_var, y = y_var)) + geom_boxplot(col = rgb(0, 0, 1, 0.6)) +
        xlab(x_var_name) + ylab(colnames(data_sub[6])) +
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
    

  }
  
  output$bio_boxplot <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
    threshold <-
      bio_vars_filter[match(colnames(data_sub_plots[6]), bio_vars_filter$name), "threshold1"]
    return(bio_boxplot(
      data_sub_plots = data_sub_plots,
      threshold = threshold
    ))
  })
  

  
  
  

  
## POC data ############################################################################################################################
############################################################################################################################
  
  # Data Subsetting POC #####
  data_sub_poc <- reactive({
    poc_contaminant <- input$poc_contaminant
    sites_POC <- df_POC %>%  dplyr::filter(year >= input$poc_yr[1] & year <= input$poc_yr[2])
    
    if(poc_contaminant == "hg")
    {sites_POC <- sites_POC %>% mutate( selected_cont = hg_mg_kg )} 
    else {sites_POC <- sites_POC %>% mutate(selected_cont= pcbs_mg_kg)}
  })
  
  # Summary Plot POC #####
  barplot_poc <- reactive({
    data_sub_poc <- data_sub_poc()
    
    
    if (!input$show_bar_pct_POC){
      if (input$poc_contaminant == "hg"){  
      p <- ggplot(data = data_sub_poc, aes(x = city, fill = hg_conc_cat)) + geom_bar(position="stack",stat='count') + ylab("Number of samples") + 
        scale_fill_manual("Concentration Category",values=colors_Hg) 
      
    }
    else {  
      p <- ggplot(data = data_sub_poc, aes(x = city, fill = pcb_conc_cat)) + geom_bar(position="stack",stat='count') + ylab("Number of samples") +
        scale_fill_manual("Concentration Category",values=colors_PCB) 
      
    }
    }
    else {
      if (input$poc_contaminant == "hg"){  
        data_sub_poc <- data_sub_poc %>% 
          dplyr::group_by(city, hg_conc_cat) %>% 
          dplyr::summarise(n = n()) 
        
        p <- ggplot(data = data_sub_poc, aes(x = city, y=n, fill = hg_conc_cat)) + geom_bar(position="fill",stat='identity') + ylab("Percentage of samples") +
          scale_y_continuous(labels = percent_format()) + 
          scale_fill_manual("Concentration Category",values=colors_Hg) 
      }
      
      else{data_sub_poc <- data_sub_poc %>% 
        dplyr::group_by(city, pcb_conc_cat) %>% 
        dplyr::summarise(n = n()) 
      
      p <- ggplot(data = data_sub_poc, aes(x = city, y=n, fill = pcb_conc_cat)) + geom_bar(position="fill",stat='identity') + ylab("Percentage of samples") +
        scale_y_continuous(labels = percent_format()) + 
        scale_fill_manual("Concentration Category",values=colors_PCB) }
    }
    
    
    p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      xlab("City")
  })
  
  
  output$plot_poc_1 <- renderPlot({
    p <- barplot_poc()
    return(p)  
    })
  
  # MAP for POC #####
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
    
    # fix that 
    getColor_poc <- function(df){
      if (poc_contaminant == "hg") {col <- unname(colors_Hg)[df$hg_conc_cat]}
      else col <- unname(colors_PCB)[df$pcb_conc_cat]
      col
    }
    
    lab_poc <- if(poc_contaminant == "hg") {levels(sites_poc$hg_conc_cat)[2:4]} else levels(sites_poc$pcb_conc_cat)
    colors_poc <- if(poc_contaminant == "hg") {colors_Hg[2:4]} else colors_PCB
    
    leafletProxy("map_poc") %>% clearMarkers() %>% clearShapes() %>% clearControls() %>%
      addCircleMarkers(lng=sites_poc$long, lat=sites_poc$lat, radius=5, color=getColor_poc(sites_poc), weight=1, popup = content_poc, opacity = 0.8, fillOpacity = 0.8) %>%
      addLegend("topright", colors=colors_poc, labels=lab_poc, title="Color Key", layerId="colorLegend")
    
  })
  

  
  # Download data ####
  output$downloadData_poc <- downloadHandler(
    
    filename = function() {
      paste(input$poc_contaminant, "_table", Sys.Date(), input$file_type_poc, sep = "")
    },
    #   content = function(file) {
    #      write.xlsx(data_sub(), file)
    #   }
    
    content = function(file) {
      
      
      data_to_dwn <- data_sub_poc() %>% 
        select(c(3,2,1,5,6,4,7,8,9,10)) %T>% 
        {names(.) <- c("County", "City", "Site ID", "Latitude", "Longitude", "Sampling Date", "PCB Concentration (mg/kg)", "Hg Concentration (mg/kg)", "Concentration Category (PCB)", "Concentration Category (Hg")}
      
      
      if(input$file_type_poc== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$file_type == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  
  

## Chlorine  ##################################################################################################
##############################################################################################
  
  # data subset ####
  data_sub_chlo <- reactive({
    return(sites_chlo %>%  
      dplyr::filter(ws %in% input$chlo_ws,
                    year >= input$chlo_yr[1] & year <= input$chlo_yr[2]))
    
  })
  
  
  # data download chlorine #### 
  data_chlo_dwld <- reactive({
    return(df_chlo %>% 
      dplyr::filter(ws %in% input$chlo_ws, 
                    year >= input$chlo_yr[1] & year <= input$chlo_yr[2]) %>% 
     dplyr::select(c(1,13:15,11:12,2:7))) 
  })
  
  chlo_shp <- function() {
    xyPoints <- data_chlo_dwld() 
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,c(5,6)], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    return(SHP)
  }
  

  output$downloadData_chlo <- downloadHandler(
    
    filename = function() {
      if (!input$file_type_chlo == ".shp")
        {paste("chlorine", "_table", Sys.Date(), input$file_type_chlo, sep = "")}
      else {paste0("shpExport.zip")} 
      
    },

    content = function(file) {
      data_to_dwn <- data_chlo_dwld() %T>% 
      {names(.) <- c("Station Code", "Watershed", "Sub-watershed", "Creek", "Longitude", "Latitude", 
                     "Date", "Analyte Name", "Field Replicate Number", "Result (mg/L)", "Result Qualitative Code", 
                     "QA Code")} 

      if(input$file_type_chlo== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      }
      if(input$file_type_chlo == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
       if (input$file_type_chlo == ".shp"){
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
        writeOGR(chlo_shp(), dsn="shpExport.shp", layer="shpExport", driver="ESRI Shapefile")
        zip(zipfile='shpExport.zip', files=Sys.glob("shpExport.*"),zip = Sys.getenv("R_ZIPCMD", "zip"))
        file.copy("shpExport.zip", file)
        if (length(Sys.glob("shpExport.*"))>0){
          file.remove(Sys.glob("shpExport.*"))
        }
      }
    }
    
  )
  # summary graphs #### 
  output$map_chlo <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      setView(lng = -122, lat = 37.4, zoom = 10) %>% 
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "markers", zIndex = 420)
  })
  
  # Update map with user inputs 
  observe({
    
    # wait for POC menu to be selected 
    req(input$menu_items == "chlorine") 
    
    sites_chlo_sub <- data_sub_chlo()
    sheds_sub <- sheds[sheds$SYSTEM %in% input$chlo_ws, ]
    
    popup_chlo <- paste(
      sep="</br>", 
      paste0("<b>",sites_chlo_sub$station_code,"</b>", " (",sites_chlo_sub$date_1, ")"),
      "<b>Chlorine, Total Residual (mg/L):</b>",
      paste(sites_chlo_sub$tot_chlo_qa_1, sites_chlo_sub$tot_chlo_1), 
      "<b>Chlorine, Free (mg/L):</b>",
      paste(sites_chlo_sub$free_chlo_qa_1, sites_chlo_sub$free_chlo_1),
      ifelse(!is.na(sites_chlo_sub$date_2),paste("</br><b>Resampled in:</b>", sites_chlo_sub$date_2,
                                                   "</br>", "Free:", sites_chlo_sub$free_chlo_2, "-", "Total:", sites_chlo_sub$tot_chlo_2),""),
      
      ifelse(!is.na(sites_chlo_sub$date_3),paste("<b>Resampled in:</b>", sites_chlo_sub$date_3,
                                                 "</br>", "Free:", sites_chlo_sub$free_chlo_3, "-", "Total:", sites_chlo_sub$tot_chlo_3),""))
    
    get_color_chlo <- function(var) {

      ifelse(var < 0.08, "green",
             ifelse(var <0.1, "orange",
                    "red"))
    }
    
    # with sites df
    leafletProxy("map_chlo") %>% clearMarkers() %>% clearShapes() %>% clearControls() 
    
    if (nrow(sheds_sub) > 0) {
      leafletProxy("map_chlo") %>%
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
                       weight=1, color="blue", fillColor=get_color_chlo(sites_chlo_sub$tot_chlo_1), 
                       fillOpacity = 0.9) %>%
      addCircleMarkers(data=sites_chlo_sub, lat=sites_chlo_sub$lat, lng=sites_chlo_sub$long, 
                       radius=4, 
                       weight=1, color="blue", fillColor=get_color_chlo(sites_chlo_sub$free_chlo_1), 
                       fillOpacity = 0.9,
                       popup=popup_chlo, options= leafletOptions(pane="markers")) %>% 
      addLegendCustom(position="topright", shapes=rep("circle",5), size=c(14,rep(10,4)), colors = c(rep("white",2), "green", "orange", "red"),
                      labels=c("Total Chlorine", "Free Chlorine", "< 0.08 mg/L", "0.08 - 0.1 mg/L", "> 0.1 mg/L"))

    }
  })
  
  
  output$plot_chlo <- renderPlotly({
    if (nrow(data_sub_chlo())>0){ 
    data_sub <- data_sub_chlo() %>% 
      dplyr::mutate(col=ifelse(tot_chlo_1>0.1, "red", "green"))
    
    p <- ggplot(data=data_sub, aes(x=tot_chlo_1, fill=col)) + geom_bar(width=0.007)+ 
      xlab("Total Residual Chlorine (mg/L)") + ylab("Number of samples") + 
      coord_cartesian(expand=F) + 
      theme(legend.position = "none") + 
      scale_fill_manual(values=c("red"="red", "green"="seagreen"))
     
    return(ggplotly(p))
    }
  })
  
  
  

 
# Continuous Temperature ##################################################################################################
#################################################################################################################################
  
  # Explanatory Popup #############
  observeEvent(input$temp_desc,{
    showModal(modalDialog(
      title = "Continuous Monitoring of Dissolved Oxygen, Temperature, Conductance and pH",
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
           it does not indicate water quality objective exceedances. Rather, it's a simplified way to compare sites with one another."),
      easyClose = TRUE, footer=modalButton("Got it!")
    ))
  })
  
  # Subset ##### 
  temp_subset <- reactive({ 
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    
    if (input$temp_param == "avDayTemp") {
      data_sub_temp <- df_temp_7DAVG %>% 
        as.data.frame() %>% 
        dplyr::filter(as.Date(date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(date) - as.Date(filter_dates[2])<= 0, 
                      ws %in% input$temp_ws_dwld) %>% 
        dplyr::mutate(subws_u = paste0(ws, " Watershed\n(",creek,")")) %>% 
        dplyr::mutate(subws_u = factor(subws_u))
    }
    if (input$temp_param == "avWeek") {
      data_sub_temp <- df_temp_MWAT %>% 
        as.data.frame() %>% 
        dplyr::arrange(ws) %>%
        dplyr::filter(as.Date(day1week) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(day1week) - as.Date(filter_dates[2])<= 0)%>% 
        dplyr::mutate(subws_u = paste0(ws, " Watershed\n(",creek,")")) %>% 
        dplyr::mutate(subws_u = factor(subws_u))
    }  
    return(data_sub_temp)
  })
  
  # Download data ####
  output$downloadData_temp <- downloadHandler(
    
    filename = function() {
      paste(input$param_type_temp, "_table", Sys.Date(), input$file_type_temp, sep = "")
    },
    
    content = function(file) {
      
      
      data_to_dwn <- temp_subset() %>% 
        dplyr::select(c(5,6,7,2,4,1,8,9)) %T>% 
        {names(.) <- c("Site ID", "Latitude","Longitude","Watershed", "Creek", "Year", "Date", "Mean Daily Temperature (Celsius)")}
      
      
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
      addMapPane(name = "markers", zIndex = 420) %>% 
      addLegend("topright", title= NULL, colors=colors_temp[c(1,3,5,7,9,11)], labels=c("Colder",rep("",4),"Warmer"))
  })
  
  # update based on user input
  observe({
    req(input$menu_items == "con_temp")
    
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    
    sites_cWQ_maptemp <- sites_cWQ %>%
      dplyr::filter(ctemp_TF == T) 
    
    popup_temp <- paste(
      sep = "<br/>",
      "<b>Site:</b>",
      sites_cWQ_maptemp$site_id,
      "<b>Watershed:</b>",
      sites_cWQ_maptemp$ws
    )
    
    get_color_temp <- function(site_id) {
        if (input$temp_param == "avDayTemp") {
     
          df_sub <- df_temp_7DAVG %>%
            dplyr::filter(as.Date(date) - as.Date(filter_dates[1])>= 0 & 
                            as.Date(date) - as.Date(filter_dates[2])<= 0)
          
          temp_cat <- sapply(site_id,
                              function(x)
                                (mean(df_sub$avDayTemp[which(df_sub$site_id == x)])-13)/(21-13))

          return(colors_temp[signif(temp_cat, 1) * 10 + 1])
        }
        if (input$temp_param == "avWeek") {
   
          df_sub <- df_temp_MWAT %>%
            filter(as.Date(day1week) - as.Date(filter_dates[1])>= 0 & 
                     as.Date(day1week) - as.Date(filter_dates[2])<= 0)
          
          temp_cat <- sapply(sites_cWQ$site_id,
                              function(x)
                                (mean(df_sub$avWeek[which(df_sub$site_id == x)])-13)/(21-13))

          
          return(colors_temp[signif(temp_cat, 1) * 10 + 1])
        
      }
      
      else return("black")
      
     
    }
    
    
    
    #shapes: 15 = square, 16= circle, 17 = triangle
    leafletProxy("map_temp")  %>% clearMarkers() %>% clearShapes() %>%
      addPolygons(
        data = sheds,
        layerId = sheds$SYSTEM,
        smoothFactor = 0.5,
        opacity = 0.6,
        weight=2, 
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
      #addCustomMarkers(data= sites_cWQ, lng=sites_cWQ$long, lat=sites_cWQ$lat,
      #                size=20, bg = c("blue", "orange","purple"),
      #               shapes=c(21,22,24), icon_group = sites_cWQ$marker_group,
      #              popup = popup) %>%
      addCircleMarkers(
        data = sites_cWQ_maptemp,
        lng = sites_cWQ_maptemp$long,
        lat = sites_cWQ_maptemp$lat,
        radius = 5,
        weight = 1,
        opacity = 0.9,
        fill = T,
        fillOpacity = 0.8,
        fillColor = get_color_temp(sites_cWQ_maptemp$site_id),       
        popup = popup_temp,
        options = leafletOptions(pane = "markers")
      )      #addLegendCustom(position="topright", colors=c("blue","orange","purple"),
      #               shapes = c("square", "circle", "triangle"),
      #              labels=c("Continuous WQ", "Continuous Temperature", "Both"),
      #             sizes=c(10,10,10), borders=rep(2,3))
      
      
  })
  
  
  


  # Timeseries ###############
  

  time_plot_function <- function(data_sub_temp, param) {
    if (nrow(data_sub_temp) > 0) {
      
      if (param == "avDayTemp") {
        threshold <-
          temp_thresholds[temp_thresholds$param == "avDayTemp", "thresh"]
        
        p <-
          ggplot(data = data_sub_temp, aes(x = date, y = avDayTemp)) + geom_line(size=0.2,aes(col =
                                                                                       site_id, group = grp))  +
          facet_grid(rows=vars(subws_u)) + 
          ylab("Average Daily Temperature (\u00B0C)") +
          xlab("Date") +
          theme_bw() +
          geom_hline(yintercept = threshold,
                     linetype = 2,
                     col = "red") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          coord_cartesian(ylim=c(10,25))

      }
      if (param == "avWeek") {
        threshold <-
          temp_thresholds[temp_thresholds$param == "avWeek", "thresh"]
        
        p <-
          ggplot(data = data_sub_temp, aes(x = day1week, y = avWeek, col=site_id)) + geom_point() + geom_line(size=0.2, aes(col=site_id)) + 
          scale_shape_manual(values = rep(1:15, 3)) +
          facet_grid(rows=vars(subws_u)) + 
          ylab("MWAT (\u00B0C)") + xlab("Date") +
          geom_hline(yintercept = threshold,
                     linetype = 2,
                     col = "red") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          coord_cartesian(ylim=c(10,25))
        
      }
      
      return(p + theme(panel.spacing.y = unit(0.2, "lines"),
                       legend.position = "none",
                       plot.margin=unit(c(0,1.5,0,0),"cm"),
                       axis.title.x = element_blank()))
     
    }
    else {
      NULL
    }
  }
  
  output$temp_timeseries_1 <- renderPlotly({
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    
    data_sub_temp <- temp_subset() 
    
    height_plot <-length(unique(data_sub_temp$subws_u))*200
    
    if(nrow(data_sub_temp)>0){
          ggplotly(p=time_plot_function(data_sub_temp = data_sub_temp, param = input$temp_param) + 
             scale_x_date(
               date_breaks = "3 months",
               labels = date_format("%b-%Y"),
               date_minor_breaks = "1 month",
               limits = filter_dates,
               expand = c(0, 0) 
             ), height=height_plot)
    } else {NULL}

    
  })
  

  
  ##### 
  
## Continuous water quality #### 
#############################################################################################################################
  
  # explanatory popup ##############
  observeEvent(input$wq_desc,{
    showModal(modalDialog(
      title = "Continuous Monitoring of Dissolved Oxygen, Temperature, Conductance and pH",
      HTML('San Francisco Bay Area Stormwater Municipal Regional Permit (MRP), Provision C.8. on Water Quality Monitoring, 
           section C.8.d.(iv): "The Permittees shall monitor general water quality parameters of streams using a water quality sonde or equivalent.
           Parameters shall include dissolved oxygen (mg/L and % saturation), pH, specific conductance (uS), and temperature (C)."
           <br/>
           <br/>
           Exceedances of these water quality parameters are based upon the following thresholds: 
           <ul> <li>"Maximum Weekly Average Temperature exceeds 17.0 C for a 
           Steelhead stream, or 20 percent of the instantaneous results exceed 24C...;</li>
           <li> 20 percent of instantaneous pH results are < 6.5 or > 8.5...; </li>
           <li> 20 percent of the instantaneous specific conductance results are > 2000 uS, or there is a spike in readings with no obvious natural explanation...;</li>
           <li> or 20 percent of instantaneous dissolved oxygen results are < 7 mg/L in a cold water fishery stream."</li></ul>'
      )
      ,
      
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })
  
  # Ui update #### 
  output$wq_param_subset <- renderUI({
    
    if (length(input$param_type_wq)>0){
      choices_score_vis <- input$param_type_wq
      names(choices_score_vis) <- MRP_threshold[match(choices_score_vis, MRP_threshold$param),"label"]
      
      radioButtons(
        inputId = "wq_param",
        label = "Water Quality Parameter:",
        choices = choices_score_vis,
        selected = choices_score_vis[1]
        )
    }
  })
  outputOptions(output, "wq_param_subset", suspendWhenHidden=F)
  

  
  # Data subsetting #### 
  wq_data_sub <- reactive({
    
    param_slct <- input$param_type_wq
    
    filter_dates = as.Date(cut(as.POSIXct(input$wq_dates,tz=''),"month")) 
    data_sub_wq <- df_wq %>%
      as.data.frame() %>% 
      dplyr::filter(
        as.Date(date) - as.Date(filter_dates[1])>= 0 & 
          as.Date(date) - as.Date(filter_dates[2])<= 0,
        ws %in% input$wq_ws, 
        season %in% input$wq_season) %>% 
    dplyr:: select(c("site_id", "lat", "long", "ws", "creek", "date", param_slct, "comment")) 

    return(data_sub_wq)
  }) 
  
  
  # Map ##### 
  
  
  output$map_wq <-  renderLeaflet({
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
    req(input$menu_items == "con_wq")
    
    sites_cWQ_mapWQ <- sites_cWQ %>% 
      dplyr::filter(wq_TF == T)
    
    # % exceedances
    data_sub_wq <- wq_data_sub() %>% 
      as.data.frame()
    threshold <-
      MRP_threshold[match(input$wq_param, MRP_threshold$label), 'value_sup']
    threshold_inf <-  MRP_threshold[match(input$wq_param, MRP_threshold$label), 'value_inf']

    num_col <- which(colnames(data_sub_wq)==input$wq_param)  
    color_cat <- sapply(sites_cWQ_mapWQ$site_id,
                        function(x) 
                          (sum(data_sub_wq[which(data_sub_wq$site_id == x),num_col]<threshold_inf |
                                 data_sub_wq[which(data_sub_wq$site_id == x),num_col]>threshold )/
                             nrow(data_sub_wq[which(data_sub_wq$site_id == x),]))
    )
    
    popup <- paste(
      sep = "<br/>",
      "<b>Site:</b>",
      sites_cWQ_mapWQ$site_id,
      "<b>Watershed:</b>",
      sites_cWQ_mapWQ$ws, 
      "<b>% Exceedance:</b>", 
      paste(signif(color_cat,2)*100,"%")
    )
    
    get_color_wq <- function() {
      
      return(colors_wq[signif(color_cat,1)*10+1])
      
    }
    
    
    get_weight <- function() { 
      return(sapply(sheds$SYSTEM, function(x) {
        if (x %in% input$wq_ws) {
          3
        } else{
          1
        }
      }))
    }
    
    #shapes: 15 = square, 16= circle, 17 = triangle
    leafletProxy("map_wq")  %>% clearMarkers() %>% clearShapes()  %>%
      addPolygons(
        data = sheds,
        layerId = sheds$SYSTEM,
        weight = get_weight(),
        smoothFactor = 0.5,
        opacity = 0.6,
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
      #addCustomMarkers(data= sites_cWQ, lng=sites_cWQ$long, lat=sites_cWQ$lat,
      #                size=20, bg = c("blue", "orange","purple"),
      #               shapes=c(21,22,24), icon_group = sites_cWQ$marker_group,
      #              popup = popup) %>%
      addCircleMarkers(
        data = sites_cWQ_mapWQ,
        lng = sites_cWQ_mapWQ$long,
        lat = sites_cWQ_mapWQ$lat,
        radius = 5,
        weight = 1,
        opacity = 0.9,
        fill = T,
        fillOpacity = 0.8,
        fillColor = get_color_wq(),
        popup = popup,
        options = leafletOptions(pane = "markers")
      ) 
    #addLegendCustom(position="topright", colors=c("blue","orange","purple"),
    #               shapes = c("square", "circle", "triangle"),
    #              labels=c("Continuous WQ", "Continuous Temperature", "Both"),
    #             sizes=c(10,10,10), borders=rep(2,3))
    
    
  })
  
  
  
  
  # Box/violin plot ##### 

  output$violinplot_wq <- renderPlot({
    data_sub_wq <- wq_data_sub() %>% 
      dplyr::mutate(creek=factor(creek)) %>% 
      dplyr::arrange(creek) %>% 
      dplyr::mutate(site_id=factor(site_id))
    param_name <- MRP_threshold[match(input$wq_param, MRP_threshold$param), "label"] 
    
    p <- ggplot(data=data_sub_wq, aes_string(x="site_id", y= input$wq_param, fill="creek")) +
      geom_violin(width=1.2) + 
      geom_boxplot(width=0.15, outlier.shape = NA, col="grey", fill="white") +  
      theme(legend.position = "top", legend.title = element_blank(),
            axis.text.x = element_text(angle=45, hjust=1)) + 
      xlab("Site ID") + ylab(param_name) 
      
    return(p)
  })
  
  
  
  # data dowload #### 
  output$downloadData_wq <- downloadHandler(
    
    filename = function() {
      paste("wq_table", Sys.Date(), input$file_type_wq, sep = "")
    },
    
    content = function(file) {
      
      wq_slct_names <- as.character(MRP_threshold[match(input$param_type_wq, MRP_threshold$param),"label"])
      data_to_dwn <- wq_data_sub() %T>% 
      {names(.) <- c("Site ID", "Latitude", "Longitude", "Watershed", "Creek", "Date", wq_slct_names, "Comment")}

      
      if(input$file_type_wq== ".csv") {
        write.csv(data_to_dwn, file, row.names = FALSE)
      } else if(input$file_type_wq == ".xlsx") {
        write.xlsx(data_to_dwn, file)
      }
    }
    
  )
  
  # Creek Trash  #### 
  ################################################################################################################################

  data_sub_trash <- reactive({
    df_trash %>% 
      dplyr::filter(juris %in% input$trash_city)
  })
  
  output$cond_trash_pathway <- renderUI({
    if (nrow(data_sub_trash()) == 0) {
      h5(
        "No data to show: Select at least one city"
      )
    }
    else {
      plotOutput("bar_trashcat_pathway")
    }
  })
  
  output$bar_trashcat_pathway <- renderPlot({
    
    if (nrow(data_sub_trash())>0){ 
 
    df <- data_sub_trash() %>%
      dplyr::group_by(trashCat) %>%
      dplyr::summarise(mean_litter=mean(litter_wind, na.rm=T),
                       mean_camp = mean(illegal_camp, na.rm=T),
                       mean_dumping = mean(dumping,na.rm=T),
                       mean_other = mean(other, na.rm=T),
                       n=n())
      
    df_avg_pct <- data.frame(trashCat= df$trashCat, pathway="Litter Wind", mean_pct=df$mean_litter, n=df$n)
    df_avg_pct <- rbind(df_avg_pct,
                        data.frame(trashCat= df$trashCat, pathway="Dumping", mean_pct=df$mean_dumping,n=df$n),
                        data.frame(trashCat= df$trashCat, pathway="Illegal Camp", mean_pct=df$mean_camp,n=df$n),
                        data.frame(trashCat= df$trashCat, pathway="Other", mean_pct=df$mean_other,n=df$n))
    
    labels <- sapply(1:4, function(x) paste0(as.character(df[x,1]$trashCat)," (n=",df[x,6], ")"))
    ggplot(data=df_avg_pct, aes(x=trashCat, y=mean_pct/100, fill=pathway)) + geom_bar(stat="identity") + 
      xlab("Trash Category") + 
      guides(fill=guide_legend(title="Pathway:")) + ylab("Percentage Averaged Pathway Contribution") +
      scale_fill_manual(values=col_pathways) +
      scale_y_continuous(expand = c(0, 0),labels = percent_format()) +
      scale_x_discrete(name="Trash Category", labels=labels) +
      geom_text(aes(label= ifelse(round(mean_pct)>1,round(mean_pct), '')), position=position_stack(vjust=0.5), size=3, col="black") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1)) 
    } else NULL
    
  })
  
  output$hist_items <- renderPlot({
    
      df <- data_sub_trash()
      
      if (nrow(df)>0){ 
    df_items <- data.frame(trashCat = df$trashCat,  rank=1, mainItem= df$prevItem1)
    df_items <- rbind(df_items, 
                      data.frame(trashCat = df$trashCat,  rank=2, mainItem= df$prevItem2),
                      data.frame(trashCat = df$trashCat,   rank=3, mainItem= df$prevItem3),
                      data.frame(trashCat = df$trashCat, rank=4, mainItem= df$prevItem4),
                      data.frame(trashCat = df$trashCat,  rank=5, mainItem= df$prevItem5))
    sort(as.character(unique(df_items$mainItem)))
    
    to_replace <- c("Fabric and cloth", "Metal Material", "Other plastic", "Other: balls ", "Other: Balls (soccer, basket, tennis) ", "Paper and cardboard", "Shopping carts", "Single Use Plastic Carryout Bags",  "Single Use Plastic Grovery Bags", "Spray paint cans" ,"Wood material" )
    replace_by <- c("Fabric and Cloth", "Metal Materials", "Other Plastic", "Other: balls", "Other: balls", "Paper and Cardboard", "Shopping Carts", "Single Use Plastic Grocery Bags", "Single Use Plastic Grocery Bags" , "Spray Paint Cans", "Wood material/debris"  )
    df_items$mainItem <- sapply(df_items$mainItem, function(x)  if (x %in% to_replace) {replace_by[which(to_replace==x)]} else {as.character(x)})
    
    
    # All trash items
    ggplot(data=df_items %>% 
             dplyr::filter(!is.na(mainItem)) %>%
             dplyr::group_by(mainItem) %>%
             dplyr::summarise(n=n()) %>%
             dplyr::arrange(n),
           aes(x=reorder(mainItem, -n), y=n)) + geom_bar(stat="identity") + 
      xlab("Trash Category") + ylab("Number of sites with trash category") +  guides(fill= guide_legend(title="Main Trash Items"))+ 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      
      else {NULL} 
  })
  
  
  # Map of trash sites 
  
  

  
  
  output$trash_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -121.8,
              lat = 37.3,
              zoom = 9) %>% 
      addLegend(position="topright", title= "Trash Categories",colors=col_trashCat, labels=c("Low", "Moderate","High", "Very High"))
    })
    
    
    
  observe({
    
  req(input$menu_items == "trash")
    
  sites_sub <- sites_trash %>% 
    dplyr::filter(juris %in% input$trash_city)
    
  get_radius_trash <- function(siteID) {
    rad <- 6
    rad <- rad+10*df_trash[match(siteID,df_trash$siteID),"tot_gal_sqft"]/max(df_trash$tot_gal_sqft)}
  
  get_color_trash <- function(siteID) {
    col_trashCat[as.numeric(df_trash[match(siteID,df_trash$siteID),"trashCat"])]}
  
  popup_trash <- paste(
    sep="<br/>",
    sites_sub$siteID,
    sites_sub$juris,
    "<b> Trash Volume (gal/ft2):</b>",
    signif(df_trash[match(sites_sub$siteID,df_trash$siteID),"tot_gal_sqft"],2)
    
  )
  
  leafletProxy("trash_map") %>% clearMarkers() %>% 
      addCircleMarkers(lng=sites_sub$dsLong, lat=sites_sub$dsLat, radius=get_radius_trash(sites_sub$siteID),
                       fillColor=get_color_trash(sites_sub$siteID), fillOpacity = 0.9, weight=1, color="blue",
                       popup=popup_trash)
 
  })  

  
  
  
  
  # Pesticides 
  ################################################################################################################################
  
  
  
  # stressors : sediment chemistry or water pesticides 
  output$stressors <- renderUI({ 
    if(input$tox_season == "D"){
      pickerInput(inputId="dry_stressors", label="Stressors:", 
                  choices=list("Metals" = c("Arsenic", "Cadmium", "Chromium", "Copper", "Lead", "Nickel"),
                               "Pesticides" = c("Pyrethroid", "Other Pesticides")), selected=tox_vars_stressors_dry[1],
                  options = pickerOptions(liveSearch = T)
      )
      
    }
    else {NULL}
  })
  
  # data subsetting
  
  tox_data_sub <- reactive({
    req(!is.null(input$dry_stressors))
    data_sub <- df_tox %>% 
      dplyr::filter(year == input$tox_yr, 
                    season == input$tox_season)
    data_sub_chem <- df_sedPest %>% 
      dplyr::filter(year == input$tox_yr,
                    AnalyteName == input$dry_stressors) %>% 
      dplyr::select(c(1,3,4,5)) %T>% 
      {names(.) <- c("StationCode", "StressorName", "StressorQuotient","StressorTrigger")}
    
    sites_sub <- sites_tox %>% 
      filter(StationCode %in% data_sub$StationCode,
             year(SampleDate)==input$tox_yr) %>% 
      dplyr::distinct(StationCode, .keep_all=T)
    
    if(nrow(data_sub )>0){
      # filter for First vs. follow up
      sites_sub <- merge(sites_sub, (data_sub %>% 
                                       dplyr::filter(sampleType == "First",
                                                     SigEffect %in% c("Fail","SL")) %>% 
                                       dplyr::group_by(StationCode) %>%
                                       dplyr::summarize(n_fail=n(),
                                                        species_fail = paste(organism_u, collapse="and"),
                                                        species_pct = paste(organism_u, " with ",PercentEffect, " % Effect", sep="",collapse=" and ")) %>%
                                       as.data.frame()),all=T) %>% 
        dplyr::mutate(n_fail = ifelse(is.na(n_fail), 0,n_fail))
      
      sites_sub$followups <- sapply(seq(1:nrow(sites_sub)), function(x) 
        
        ifelse(length(df_tox[df_tox$StationCode == sites_sub$StationCode[x] & df_tox$sampleType == "Follow-up"
                             & df_tox$year == input$tox_yr & df_tox$season == input$tox_season & 
                               df_tox$organism_u %in% strsplit(sites_sub$species_fail, split="and"),"SigEffect"]) > 0, 
               
               
               paste(
                 df_tox[df_tox$StationCode == sites_sub$StationCode[x] & df_tox$sampleType == "Follow-up" &
                          df_tox$year == input$tox_yr & df_tox$season == input$tox_season & 
                          df_tox$organism_u %in% strsplit(sites_sub$species_fail,split="and"),"organism_u"],": ",
                 df_tox[df_tox$StationCode == sites_sub$StationCode[x] & df_tox$sampleType == "Follow-up" &
                          df_tox$year == input$tox_yr & df_tox$season == input$tox_season & 
                          df_tox$organism_u %in% strsplit(sites_sub$species_fail,split="and"),"SigEffect"], " with ",
                 df_tox[df_tox$StationCode == sites_sub$StationCode[x] & df_tox$sampleType == "Follow-up" &
                          df_tox$year == input$tox_yr & df_tox$season == input$tox_season & 
                          df_tox$organism_u %in% strsplit(sites_sub$species_fail,split="and"),"PercentEffect"], "% Effect",
                 sep="", collapse="and"), 
               "None"
        ))
      
      sites_sub <- merge(sites_sub,data_sub_chem, all=T)
      
      return(sites_sub)
      
      
    }
    else return(data.frame())
    
    
    
  })
  
  tox_data_sub_plot <- reactive({ 
    df_tox %>% 
      filter(year==input$tox_yr, 
             season == input$tox_season, 
             sampleType=="First")})
  
  output$plot_tox <- renderPlot({
    ggplot(data=tox_data_sub_plot(), aes(x=organism_u, y=PercentEffect)) + 
      geom_bar(aes(fill=organism_u),stat="identity", position="dodge") + 
      facet_grid(MatrixName~ StationCode) + 
      theme(axis.text.x = element_text(angle=60, hjust=1), 
            legend.position  = "none", 
            axis.title.x = element_blank()) + 
      ylab("Percent Effect")
    
  })
  
  
  
  output$map_tox <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -122,
              lat = 37.3,
              zoom = 9) %>% 
      addLegendCustom("topright", title="Significant toxicity to...",colors=colors_tox, labels=c("0 species","1 species","2 species","3 species"), shapes=rep("circle",4), sizes=rep(10,4)) %>% 
      
      addLegend("bottomright", title="Stressor Concentration",colors=c(colors_chem, "grey"), labels=c("Below Threshold", "Above Threshold", "No data for this stressor"))
  })
  
  
  observe({
    
    req(input$menu_items == "pesticide")
    leafletProxy("map_tox") %>% clearMarkers() %>% clearShapes()
    
    sites_sub <- tox_data_sub()
    
    get_color_chem <- function(trigger){ 
      if (input$tox_season == "D") 
      {colors <- colors_chem[trigger+1]
      colors[is.na(colors)] <- "grey"
      return(colors)}
      else "grey"} 
    
    
    if (nrow(sites_sub) >0){
      
      popup_tox <- paste(
        sep="</br>", 
        sites_sub$StationCode, 
        ifelse(sites_sub$n_fail >= 1, 
               paste("<b> Significant Toxicity for: </b></br>", 
                     sites_sub$species_pct,
                     "</br><b>Follow-ups?</b>","</br>", 
                     sites_sub$followups
                     
               ), 
               "<b>No significant toxicity</b>")
        
        
        
      )
      
      leafletProxy("map_tox") %>% clearMarkers() %>% clearShapes() %>% 
        addRectangles(data=sites_sub %>% na.omit(), 
                      lat1=sites_sub$TargetLatitude-0.02, lng1=sites_sub$TargetLongitude-0.02,
                      lat2=sites_sub$TargetLatitude+0.02, lng2=sites_sub$TargetLongitude+0.02,
                      fillColor=get_color_chem(sites_sub$StressorTrigger), fillOpacity=0.5, opacity=0,
                      label=paste("Stressor Quotient:", signif(sites_sub$StressorQuotient,2)))    %>%
        addCircleMarkers(data=sites_sub, lat=sites_sub$TargetLatitude, 
                         lng=sites_sub$TargetLongitude,
                         fillColor=colors_tox[sites_sub$n_fail+1], label=paste(sites_sub$n_fail),
                         fillOpacity=0.8, weight=1, radius=6, 
                         popup=popup_tox) 
    }
  })
  
  
  # Pathogens
  ##########################################################################################################
  
  data_sub_patho <- reactive({
    df_patho %>% 
      dplyr::filter(year >= input$patho_yr[1] & year <=input$patho_yr[2],
                    Analyte== input$patho_analyte) %>% 
      dplyr::arrange(desc(year))
  })
  
  
  output$map_patho <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -121.8,
              lat = 37.3,
              zoom = 9)
  })
  
  
  observe({
    req(input$menu_items == "pathogens")
    
    data_sub <- data_sub_patho()
    popup_patho <- paste(sep="</br>",
                         "<b>Station Code:</b>", 
                         data_sub$Station_Code, 
                         paste("<b>",input$patho_analyte, "concentration (MPN/100mL):</b>",
                               data_sub$Result))
    
    leafletProxy("map_patho") %>% clearMarkers() %>% clearMarkerClusters() %>%
      addCircleMarkers(lng=data_sub$Longitude, lat= data_sub$Latitude, color= patho_col[data_sub$exceedance+1], 
                       popup = popup_patho, fillOpacity = 0.8, radius=10,label= paste(data_sub$year),
                       clusterOptions = markerClusterOptions(showCoverageOnHover = F, freezeAtZoom = 15
                       )
      )
  })  
  
  
})

