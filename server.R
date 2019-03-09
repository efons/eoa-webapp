# EOA's website redesign
# Data visualization app
# Emilie Fons
# Michaela Palmer


# Script 3 - Server function


# Server function
server <- (function(input, output, session) {
  ## Bioassessment data
  ###################################################################################################################
  
  # Help bio tab 
  
  observeEvent(input$bio_tab_title,{
    showModal(modalDialog(
      title = "Biological Condition Assessment",
      HTML('San Francisco Bay Area Stormwater Municipal Regional Permit (MRP), Provision C.8. on Water Quality Monitoring, 
      section C.8.d.(i) on Biological Assessments: "The Permittees shall conduct biological
           assessments (also referred to herein as bioassessments) in accordance with
           SWAMP Standard Operating Procedures22,23,24 and shall include collection
           and reporting of in-stream biological and physical habitat data according to
           the SWAMP Standard Operating Procedures for Bioassessment,3 including
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
  
  score_desc_txt <- reactive({
    score <- input$filter_by 
    score_name <- bio_vars_filter[which(bio_vars_filter$param == score),"name"]
    
    description <- if (score == "csci") {"CSCI stands for California Stream Condition Index. This
      index was developed in ... by ... to .... Values of this index are computed based on ... and thus
      represent .... The values range from 0 to 1, 0 representing the worst score, and 1 the best score.
      See the table to the left for a more detailed presentation of CSCI score breaks"}
    
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
      formatStyle(1,2,backgroundColor=styleEqual(col,colors_bio[col]), color=styleEqual(col,c(rep("black",3),"white"))) %>%
      formatStyle( 0, target= 'row',lineHeight='70%')
    
    return(df)
  })
  
  observeEvent(input$score_popup,{
    showModal(modalDialog(
      title = "Score Description",
      score_desc_txt(),
      DT::renderDataTable( score_desc_table()),
      easyClose = TRUE, footer=modalButton("Got it!")
      ))
  })

    
    

  # Summary or detailed version? 
    # option to show/hide the detailed info ? 
    # using shinyjs or other

    
  
  # map parameter : size filter by stressor variable
 
  
  
  # Spatial Filter: county vs. watersheds
  output$scnd_sub_ws <- renderUI({
   
      pickerInput(
        inputId = "ws",
        label = "Choose Watershed",
        choices = if (input$spatial_filter == "sub_ws") {as.character(bio_vars_ws)} else {NULL},
        selected = if (input$spatial_filter == "sub_ws") {as.character(bio_vars_ws)} else {NULL},
        options = list(`actions-box` = ifelse((input$spatial_filter == "sub_ws"),TRUE,FALSE), size = 20),
        multiple = T
      )
    
 
      
  })
  
  
  
  
  # Reactive function that returns the subsetted data table
  data_sub <- reactive({
    req(!is.null(input$size_by))
    
    filter_by <- input$filter_by
    threshold1 <-
      bio_vars_filter$threshold1[which(bio_vars_filter$param == filter_by)]
    threshold2 <-
      bio_vars_filter$threshold2[which(bio_vars_filter$param == filter_by)]
    threshold3 <-
      bio_vars_filter$threshold3[which(bio_vars_filter$param == filter_by)]
    filter_name <-
      as.character(bio_vars_filter$name[which(bio_vars_filter$param == filter_by)])
    size_by <- input$size_by
    size_name <- as.character(param_names$fullname[which(param_names$dataname == size_by)])
    
    data_sub <-
      df_bio %>% dplyr::filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      dplyr::filter(if (input$spatial_filter == "sub_ws") {
        ws %in% input$ws
      } else {
        ws %in% bio_vars_ws
      }) %>%
      dplyr::mutate(year = factor(year, levels = seq(
        min(bio_vars_yr), max(bio_vars_yr), 1
      ))) %>%
      dplyr::select(c("rmc_id", "ws", "subws","creek", "year", filter_by, size_by))  %>%
      arrange(desc(year), ws) %T>%
      {
        names(.) <-
          c("RMC ID",
            "Watershed",
            "Sub-watershed",
            "Creek",
            "Year",
            filter_name,
            size_name )
      }
    
    
    # Color cells for likely altered site - add new column (values stored as characters)
    
    if (nrow(data_sub) > 0) {
      data_sub <- data_sub %>%
        dplyr::mutate(filter_by_chr = NA) %>% # Makes unknown column'' error appear
        dplyr::select(c(1, 2, 3, 4, 5, 6, 8, 7)) %>%
        dplyr::rename_at(7, ~ paste(filter_name, " ", sep = ''))
      
      
      
      for (i in 1:nrow(data_sub)) {
        if (!is.na(data_sub[i, 6])) {
          if (data_sub[i, 6] <= threshold1)  {
            data_sub[i, 7] <- colors_bio[4]
          }
          else {
            if (data_sub[i, 6] < threshold2) {
              data_sub[i, 7] <-colors_bio[3]
            }
            else {
              if (data_sub[i, 6] < threshold3) {
                data_sub[i, 7] <-colors_bio[2]
              }
              else {
                if (threshold3 >= 0) {
                  data_sub[i, 7] <-colors_bio[1]
                }
                else  {
                  data_sub[i, 7] <- "black"
                }
                
              }
            }
          }
        }
      }
    }
    
    return(as.data.frame(data_sub))
    
    
  })
  
  
  # Value boxes   
  output$vbox_vla <- renderValueBox({
    data <- data_sub()
    pct_vla <- 100*signif(sum(data[,7]==colors_bio[4])/nrow(data),2)
    
    valueBox(
      subtitle=tags$p("of sites are Very Likely Altered", style="font-size:95%"),
      color="purple",
      value= paste(pct_vla, "%"),
      icon = icon("bolt")
    )
  })
  output$vbox_la <- renderValueBox({
    data <- data_sub()
    pct_la <- 100*signif(sum(data[,7]==colors_bio[3])/nrow(data),2)
    
    valueBox(
      subtitle=tags$p("of sites are Likely Altered",style="font-size:95%"),
      color="red",
      value= paste(pct_la, "%"),
      icon = icon("bolt")
    )
  })
  output$vbox_pi <- renderValueBox({
    data <- data_sub()
    pct_pi <- 100*signif(sum(data[,7]==colors_bio[2])/nrow(data),2)
    
    valueBox(
      subtitle=tags$p("of sites are Possibly Intact",style="font-size:95%"),
      color="yellow",
      value= paste(pct_pi, "%"),
      icon = icon("tint")
    )
  })
  
  output$vbox_li <- renderValueBox({
    data <- data_sub()
    pct_li<- 100*signif(sum(data[,7]==colors_bio[1])/nrow(data),2)
    
    valueBox(
      subtitle=tags$p("of sites are Likely Intact",style="font-size:95%"),
      color="green",
      value= paste(pct_li, "%"),
      icon = icon("tint")
    )
  })
  
  # List of selected watersheds
  ws_list <- reactive({
    ifelse(
      input$spatial_filter == "sub_ws",
      paste(
        "Selected watersheds: ",
        paste(input$ws, collapse = ", "),
        sep = ""
      ),
      "All watersheds"
    )
  })
  
  output$ws_list_1 <- renderText({
    ws_list()
  })
  output$ws_list_2 <- renderText({
    ws_list()
  })
  
  
  
  # Data Table
  # Data Table title
  output$score_tables <- renderText({
    ifelse(
      !input$size_by == "none",
      paste(
        bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)],
        " and ",
        param_names[which(param_names$dataname == input$size_by), "fullname"],
        " for ",
        input$wy[1],
        " - ",
        input$wy[2],
        sep = ""
      ),
      paste(
        bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)],
        " for ",
        input$wy[1],
        " - ",
        input$wy[2],
        sep = ""
      )
    )
    
  })
  
  
  # Error message when data set is empty
  output$cond_table <- renderUI({
    if (nrow(data_sub()) == 0)
    {
      h5(
        "No data to show (no samples for this watershed/time period OR no parameter selected in map parameters)"
      )
    }
    else {
      tableOutput("score_table")
    }
  })
  
  
  # Create Table when data set not empty
  output$score_table <- renderTable({
    t <- data_sub()
    
    t[,7] <- sapply(rep(1:nrow(t),1), function(x) paste(
      '<div style="width: 100%; height: 100%; z-index: 0; color:',
      t[x,7],
      '; position:absolute; top: 0; left: 0; padding:5px;"><span>',
      signif(t[x, 6], 2),
      '</span></div>'
    ))
    
    return(t[, c(1, 2, 3, 4,5, 7,8)])
    
    
  }, bordered = T, align = 'c', hover = T, sanitize.text.function = function(x)
    x)
  # error: unknown column ""
  
  
  
  # create shapefile for download 
  bio_shp <- reactive({
    data <- data_sub() %>% 
      dplyr::select(1:6,8) %T>% 
    {names(.) <- c("rmc_id", "ws", "subws", "creek", "year", input$filter_by, input$size_by)} %>% 
      dplyr::arrange(rmc_id)
  
    sites_slct <- data$rmc_id
    sites_sub <- sites %>% 
      dplyr::select(2:7) %>% 
      dplyr::filter(rmc_id %in% sites_slct) %>% 
      dplyr::arrange(rmc_id) 
    
    xyPoints <- merge(sites_sub,data)
    SHP <- SpatialPointsDataFrame(coords= xyPoints[,5:6], data =  xyPoints)
    proj4string(SHP) <- CRS("+init=epsg:4326")
    
    return(SHP)
    
    
  })
  
  
  # Downloadable user selected file type of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() { 
      
      if (!input$file_type == ".shp"){
              paste(input$spatial_filter, "_table", Sys.Date(), input$file_type, sep = "")
      }
      else {paste0("shpExport.zip")} 
    },
 #   content = function(file) {
#      write.xlsx(data_sub(), file)
 #   }
    
    content = function(file) {
      
       if(input$file_type== ".csv") {
        write.csv(data_sub(), file, row.names = FALSE)
      } else if(input$file_type == ".xlsx") {
        write.xlsx(data_sub(), file)
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
  
  
  
  # Summary barplot 
  output$barplot <- renderPlot({
    data_sub <- data_sub()
    data_sub[,7] <- factor(data_sub[,7], levels=colors_bio)
    
    n=2
    if (length(unique(data_sub[,2]))==1){n <- 3}
    n_name <- ifelse(n==2,'Watershed',"Subwatershed")
    
    if (!input$show_bar_pct){
          p <- ggplot(data = data_sub, aes(x = data_sub[,n], fill = data_sub[,7])) + geom_bar()+ ylab("Number of samples")
          }
    else {
      data_sub <- data_sub %>% 
        dplyr::rename("x_axis"=!!names(.[n]),
              "score" = !!names(.[6]),
               "score_cat" = !!names(.[7])) %>% 
        dplyr::group_by(x_axis, score_cat, score) %>% 
        dplyr::summarise(n = n()) 

      p <- ggplot(data = data_sub, aes(x = x_axis, y=n, fill = score_cat)) + geom_bar(position="fill",stat='identity') + ylab("Percentage of samples") +
        scale_y_continuous(labels = percent_format())
    }
    
    
    p <- p + 
      scale_fill_manual(values=c( "#A6DBA0"=colors_bio[1], "#FEEBA0"=colors_bio[2], "#FF6D46"=colors_bio[3], "#762A83"=colors_bio[4]),
                                breaks=colors_bio,
                                labels=c("Likely Intact", "Possibly Intact", "Likely Altered", "Very Likely Altered")) +
      xlab(n_name) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "top", legend.box="vertical", legend.title = element_blank()) 
    
    
    return(p)

  
  })
  
  
  # Scatter Plots
  
  output$scatterplots <- renderText({
    ifelse(
      input$spatial_filter == "sub_ws",
      paste("Scatterplots for ", input$wy[1], " - ", input$wy[2], sep =
              ""),
      paste("Scatterplots for ", input$wy[1], " - ", input$wy[2], sep =
              "")
    )
  })
  
  
  output$cond_scatter <- renderUI({
    if (nrow(data_sub()) <= 1 ||
        sum(!is.na(data_sub()[, ncol(data_sub())])) <= 1) {
      h5(
        "No data to show: Make sure you selected a stressor variable and there are at least two assessment events for the selected time period/watersheds"
      )
    }
    else {
      plotOutput("scatter1")
    }
  })
  
  output$scatter1 <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
    x_var <- data_sub_plots[, 8]
    y_var <- data_sub_plots[, 6]
    p <- ggplotRegression(y=y_var,x= x_var) +
      xlab(colnames(data_sub_plots)[8]) + ylab(colnames(data_sub_plots)[6])
    return(p)
  }, height = 300, width = 400)
  
  
  # Scatterplots help window 
  observeEvent(input$interpret_scatter,{
    showModal(modalDialog(
      title = "Scatterplot coefficients",
      "Pearson's correlation test assesses the strength of the linear relationship
              between two variables (assumed to be normally distributed). 
              Both R-squared and p-value can take values between 0 and 1. The closer R-squared is to 1, the stronger the relationship. 
              The relationship is generally considered to be significant if p-value is below the significance level (e.g. 0.05). 
              Spearman's correlation test assesses the strength of the monotonic relationship (i.e. either decreasing or increasing, 
              but not necessarily linear) between the two variables, regardless of their distribution (non-parametric test). 
              Spearman's Rho can take values between +1 and -1. Values of +1 or -1 indicate a strong monotonic relationship, whether the variables are
              negatively (-) or positively (+) correlated. p-values below 0.05 are generally considered significant.",
      easyClose = TRUE, footer=modalButton("Got it!")
    ))
  })
  
  # Boxplots 
  output$boxplots <- renderText({
    ifelse(
      input$spatial_filter == "sub_ws",
      paste("Boxplots for ", input$wy[1], " - ", input$wy[2], sep =
              ""),
      paste(" Boxplots for ", input$wy[1], " - ", input$wy[2], sep =
              "")
    )
  })
  
  output$cond_boxplot <- renderUI({
    if (nrow(data_sub()) == 0)
    {
      h5(
        "No data to show (no samples for this watershed/time period OR no parameter selected in map parameters)"
      )
    }
    else {
      plotOutput("boxplot")
    }
  })
  
  bio_boxplot <- function(data_sub_plots,
                          var_nb = 8,
                          threshold = -1) {
    if (nrow(data_sub_plots) > 0) {
      x_var <- data_sub_plots[,2]
      x_var_name <- "Watersheds"
      if (length(unique(x_var))==1) {x_var <- data_sub_plots[,3]
      x_var_name="Subwatersheds"}
      
      y_var <- data_sub_plots[, var_nb]
      df_tempo <- data.frame(x_var = x_var, y_var = y_var)
      p <-
        ggplot(df_tempo, aes(x = x_var, y = y_var)) + geom_boxplot(col = rgb(0, 0, 1, 0.6)) +
        xlab(x_var_name) + ylab(colnames(data_sub_plots[var_nb])) +
        stat_summary(
          fun.data = give_tot,
          geom = "text",
          fun.y = median,
          position = position_dodge(width = 0.75)
        ) + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      if (threshold >= 0) {
        p <-
          p + geom_hline(yintercept = threshold,
                         linetype = 2,
                         col = "seagreen")
      }
      return(p)
    }
    else
      return(NULL)
  }
  
  output$boxplot <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
    threshold <-
      bio_vars_filter[match(colnames(data_sub_plots[6]), bio_vars_filter$name), "threshold1"]
    return(bio_boxplot(
      data_sub_plots = data_sub_plots,
      var_nb = 6,
      threshold = threshold
    ))
  }, height = 300, width = 400)
  
  
  
  output$boxplot2 <- renderPlot({
    data_sub_plots <- as.data.frame(data_sub())
      return(bio_boxplot(data_sub_plots = data_sub_plots, var_nb = 8))
    
  }, height = 300, width = 400)
  
  
  
  # Map title
  output$map_title <- renderText({
    filter_name <-
      bio_vars_filter$name[which(bio_vars_filter$param == input$filter_by)]
    paste(
      "Map of ",
      filter_name,
      "for ",
      input$wy[1],
      " - ",
      input$wy[2],
      "(Change in map parameters)"
    )
  })
  
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
  
  
  
  # Update map based on user input
  # reset view button
  observe({
    input$reset_button
    leafletProxy("map_sites") %>% setView(lat = 37.3,
                                          lng = -121.8,
                                          zoom = 9)
  })
  
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
    size_by <- input$size_by
    
    # subset data based on user input
    data_sub <-
      df_bio %>% filter(year >= input$wy[1] & year <= input$wy[2]) %>%
      filter(if (input$spatial_filter == "sub_ws") {
        ws %in% input$ws
      } else {
        ws %in% bio_vars_ws
      })
    data_sub <-
      cbind(data_sub, data_sub[, which(colnames(data_sub) == filter_by)])
    colnames(data_sub)[ncol(data_sub)] <- "filter"
    if (!(size_by == "none")) {
      data_sub <-
        cbind(data_sub, data_sub[, which(colnames(data_sub) == size_by)])
      colnames(data_sub)[ncol(data_sub)] <- "size"
    }
    
    # subset watersheds shapefile based on user input
    if (input$spatial_filter == "sub_ws") {
      sheds_sub <- sheds[sheds$SYSTEM %in% input$ws, ]
    }
    else {
      sheds_sub <- sheds
    }
    
    
    
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
    getRadius <- function() {
      if (input$show_radius == T) {
        if ((!size_by == "none") & (!sum(!is.na(data_sub$size)) == 0)) {
          rad <- 3 + 7 * (data_sub$size / max(data_sub$size, na.rm = T))
        } else {
          rad <- 4
        }
      }      else {
        rad <- 4
      }
      
      
      return(ifelse(is.na(rad), 3, rad))
    }
    
    getColor <- function() {
      if (!(filter_by == "none")) {
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
      }
      else
        "white"
      
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
          radius = getRadius(),
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
          colors = colors_bio,
          labels = c(
            "Likely Intact",
            "Possibly Intact",
            "Likely Altered",
            "Very Likely Altered"
          ),
          sizes = c(rep(12, 4)),
          shapes = rep("circle", 4),
          title = as.character(filter_name)
        )
      
      #if(input$show_creeks == "yes")
      #{
      #leafletProxy("map_sites") %>%
      # addPolylines(data=creeks, weight=1, color="black")
      #}
    }
    
    
    
  })
  
  
  # Watershed-specific info that appears when the user clicks on a polygon
  observe({
    click <- input$map_sites_shape_click
    if (is.null(click))
      return()
    else
      output$ws_info <- renderText({
        paste("Watershed-specific info for", click$id)
      })
    
  })
  
  # Site-specific info that appears when the user clicks on a marker
  observe({
    click <- input$map_sites_marker_click
    if (is.null(click))
      return()
    
    
    else
      output$site_info <- renderText({
        paste("Site-specific info for", click$id, "(", df_bio$ws[which(df_bio$rmc_id == click$id)], ")")
      })
    
    output$table_site_onClick <- renderTable({
      slct_col <-
        colnames(df_bio[, c(1, 3, 29, 30, 7, 11, 12, 13, 14, 15, 16, 17, 18, 19, 23, 31, 32)])
      d <- df_bio %>% dplyr::select(slct_col) %>%
        filter(rmc_id == click$id) %T>%
        {
          names(.) <-
            as.character(param_names[match(slct_col, param_names$dataname), 1])
        } %>%
        t()
      
      return(d)
    }, rownames = T, colnames = F, bordered = T, align = 'c')
  })
  
  
  
  
  
  ## POC data
  ############################################################################################################################

  # Plot for POC
  
  data_sub_poc <- reactive({
    poc_contaminant <- input$poc_contaminant
    sites_POC <- df_POC %>%  dplyr::filter(year >= input$poc_yr[1] & year <= input$poc_yr[2])
    
    if(poc_contaminant == "hg")
    {sites_POC <- sites_POC %>% mutate( selected_cont = hg_mg_kg )} 
    else {sites_POC <- sites_POC %>% mutate(selected_cont= pcbs_mg_kg)}
  })
  
  
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
  
  
  
  # Download data 
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
  
  
  
  # Chlorine 
  ##############################################################################################
  
  # data subset 
  data_sub_chlo <- reactive({
   
    return(sites_chlo %>%  
      dplyr::filter(watershed %in% input$chlo_ws,
                    year >= input$chlo_yr[1] & year <= input$chlo_yr[2]))
    
  })
  
  
  # MAP 
  output$map_chlo <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      setView(lng = -122, lat = 37.4, zoom = 10)
  })
  
  # Update map with user inputs 
  observe({
    
    # wait for POC menu to be selected 
    req(input$menu_items == "chlorine") 
    

    sites_chlo_sub <- data_sub_chlo()
    
    
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
                                                 "</br>", "Free:", sites_chlo_sub$free_chlo_3, "-", "Total:", sites_chlo_sub$tot_chlo_3),"")
      
    )
    
    
    
    
    
    get_color_chlo <- function(var) {
      ifelse(var < 0.08, "green",
             ifelse(var <0.1, "orange",
                    "red"))
    }
    
    # with sites df
    leafletProxy("map_chlo") %>% clearMarkers() %>% clearShapes() %>% clearControls() %>%
      addCircleMarkers(data=sites_chlo_sub, lat=sites_chlo_sub$lat, lng=sites_chlo_sub$long, 
                       radius=7, 
                       weight=1, color="blue", fillColor=get_color_chlo(sites_chlo_sub$tot_chlo_1), 
                       fillOpacity = 0.9) %>%
      addCircleMarkers(data=sites_chlo_sub, lat=sites_chlo_sub$lat, lng=sites_chlo_sub$long, 
                       radius=4, 
                       weight=1, color="blue", fillColor=get_color_chlo(sites_chlo_sub$free_chlo_1), 
                       fillOpacity = 0.9,
                       popup=popup_chlo)

    
  })
  
  
  output$plot_chlo <- renderPlot({
    if (nrow(data_sub_chlo())>0){ 
    data_sub <- data_sub_chlo()
    
    p <- ggplot(data=data_sub, aes(x=tot_chlo_1)) + geom_density(alpha=0.5)+ 
      xlab("Total Residual (mg/L)") + ylab("Density") + 
      coord_cartesian(xlim=c(0,0.4))
    
    d <- ggplot_build(p)$data[[1]]
      
    if (nrow(subset(d,x>0.1))>0){
           p <- p + geom_area(data = subset(d, x > 0.1), aes(x=x, y=y), fill="red")
    }
     
    return(p)
    }
  })
  
  
  
  
  ## Continuous water quality
  #############################################################################################################################
  
  
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
  
  wq_data_sub <- function (filter_dates, param_slct, season_slct, ws_slct) {
    
    data_sub_wq <- df_wq %>%
      dplyr::filter(
        as.Date(date) - as.Date(filter_dates[1])>= 0 & 
          as.Date(date) - as.Date(filter_dates[2])<= 0,
        if (!ws_slct == "all"){ws %in% ws_slct} else ({ws %in% wq_vars_ws}) ) %>% 
      dplyr::filter(if(season_slct == "S_F") {season %in% c("S","F")} else season %in% season_slct )
 
    data_sub_wq <-
      cbind(data_sub_wq, data_sub_wq[, which(colnames(data_sub_wq) == param_slct)])
    
    parameter_name <-
      as.character(MRP_threshold[match(param_slct, MRP_threshold$param), 'label'])
    colnames(data_sub_wq)[ncol(data_sub_wq)] <- parameter_name
    
    
    return(data_sub_wq)
  }
  
  
  wq_data_sub_map <- reactive({
 
    wq_data_sub(filter_dates = as.Date(cut(as.POSIXct(input$wq_dates,tz=''),"month")),
                param_slct = input$wq_param,
                season_slct= input$wq_season,
                ws_slct="all")
  })
  
  
  wq_data_sub_plots <- reactive({

    
    wq_data_sub(filter_dates=as.Date(cut(as.POSIXct(input$wq_dates,tz=''),"month")),
                param_slct=input$wq_param,
                season_slct=input$wq_season, 
                ws_slct=input$wq_ws)
  })
  
  
  # update inputs - sub-watersheds - sites
  
  
  output$map_wq <-  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      setView(lng = -122,
              lat = 37.3,
              zoom = 9) %>%
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "markers", zIndex = 420) %>% 
      addLegend("topright", title="% WQS exceedance", colors=colors_wq, labels=c("0","0-10", "10-20", "20-30",
                                                       "30-40", "40-50", "50-60",
                                                       "60-70", "70-80", "80-90",
                                                       "90-100"))
  })
  
  # update based on user input
  observe({
    req(input$menu_items == "con_wq")
    
    sites_cWQ_mapWQ <- sites_cWQ %>% 
      dplyr::filter(wq_TF == T)
    
  # % exceedances
    data_sub_wq <- wq_data_sub_map()
    threshold <-
      MRP_threshold[match(colnames(data_sub_wq)[ncol(data_sub_wq)], MRP_threshold$label), 'value_sup']
    threshold_inf <-  MRP_threshold[match(colnames(data_sub_wq)[ncol(data_sub_wq)], MRP_threshold$label), 'value_inf']
    
    color_cat <- sapply(sites_cWQ_mapWQ$site_id,
                        function(x) 
                          (sum(data_sub_wq[which(data_sub_wq$site_id == x),ncol(data_sub_wq)]<threshold_inf |
                                 data_sub_wq[which(data_sub_wq$site_id == x),ncol(data_sub_wq)]>threshold )/
                             length(data_sub_wq[which(data_sub_wq$site_id == x),ncol(data_sub_wq)]))
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
        if (x == input$wq_ws) {
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
  
  
  
  
  # Boxplots
  
  boxplot_function <-
    function(data_sub_wq,
             param,
             x_param = "year",
             plot_cat = 1) {
      if (nrow(data_sub_wq) > 0) {
        data_sub_wq <- data_sub_wq[data_sub_wq$plot_cat == plot_cat, ]
        if (nrow(data_sub_wq) > 0) {
          threshold <-
            MRP_threshold[match(colnames(data_sub_wq)[ncol(data_sub_wq)], MRP_threshold$label), 'value_sup']
          threshold_inf <-  MRP_threshold[match(colnames(data_sub_wq)[ncol(data_sub_wq)], MRP_threshold$label), 'value_inf']
          lim_sup <- MRP_threshold[match(colnames(data_sub_wq)[ncol(data_sub_wq)], MRP_threshold$label), 'lim_sup']
          df_tempo <-
            data.frame(x_var = as.factor(data_sub_wq[, which(colnames(data_sub_wq) ==
                                                               x_param)]),
                       y_var = data_sub_wq[, which(colnames(data_sub_wq) ==
                                                     param)]) %>% 
            dplyr::filter(!is.na(y_var))
          
          p <-
            ggplot(data = df_tempo, aes(x = x_var, y = y_var)) + geom_boxplot(col =
                                                                                rgb(0, 0, 1, 0.6)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            xlab(x_param) + ylab(colnames(data_sub_wq)[ncol(data_sub_wq)]) +
            coord_cartesian(ylim=c(0,lim_sup))+ 
            geom_hline(
              yintercept = threshold,
              lty = 2,
              col = "red",
              lwd = 1
            ) +
        
            geom_hline(
              yintercept = threshold_inf,
              lty = 2,
              col = "red",
              lwd = 1
            ) + 
            stat_summary(
              fun.data = give_tot,
              geom = "text",
              fun.y = median,
              position = position_dodge(width = 1)
            ) + 
            ggtitle(paste("Creeks:", paste(unique(data_sub_wq$creek),collapse=",")))
          
          return(p)
        }
        
      } else
        return(NULL)
    }

  
  
  output$wq_boxplot_1 <- renderPlot({
    data_sub_wq <- wq_data_sub_plots()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(
      boxplot_function(
        data_sub_wq = data_sub_wq,
        param = param,
        plot_cat = 1,
        x_param = "site_id"
      )
    )
  })
  
  
  output$wq_boxplot_2 <- renderPlot({
    data_sub_wq <- wq_data_sub_plots()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(
      boxplot_function(
        data_sub_wq = data_sub_wq,
        param = param,
        plot_cat = 2,
        x_param = "site_id"
      )
    )
  })
  
  
  # Beanplots < boxplots now
  
  output$wq_beanplot_1 <- renderPlot({
    data_sub_wq <- wq_data_sub_plots()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(
      boxplot_function(
        data_sub_wq = data_sub_wq,
        param = param,
        plot_cat = 1,
        x_param = "year"
      )
    )
  })
  
  output$wq_beanplot_2 <- renderPlot({
    data_sub_wq <- wq_data_sub_plots()
    param <- colnames(data_sub_wq)[ncol(data_sub_wq)]
    return(
      boxplot_function(
        data_sub_wq = data_sub_wq,
        param = param,
        plot_cat = 2,
        x_param = "year"
      )
    )
  })
  
  
 
# Continuous Temperature 
#################################################################################################################################
  
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
  
  # update inputs - sub-watersheds - sites
  
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
    
    # get dates
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
    
    
    get_weight_temp <- function() {
      return(sapply(sheds$SYSTEM, function(x) {
        if (x == input$temp_ws) {
          3
        } else{
          1
        }
      }))
    }
    
    #shapes: 15 = square, 16= circle, 17 = triangle
    leafletProxy("map_temp")  %>% clearMarkers() %>% clearShapes() %>%
      addPolygons(
        data = sheds,
        layerId = sheds$SYSTEM,
        weight = get_weight_temp(),
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
  
  ranges <- reactiveValues(x=NULL, y=c(0,30))
  
  time_plot_function <- function(data_sub_temp, param) {
    if (nrow(data_sub_temp) > 0) {
      
      #if (param== "ConTemp"){
      # p <- ggplot(data=data_sub_temp, aes(x=date, y=ctemp_c, col=site_id)) + geom_line(size=0.3) + ylim(c(0,30))  + ylab("Temperature (\u00B0C)") + xlab("Date")  + scale_x_datetime(breaks=date_breaks("1 year"), labels=date_format("%b-%y")) +
      #geom_hline(yintercept = 24, linetype=2, col = "red") +
      #theme_bw()
      #}
      
      if (param == "avDayTemp") {
        threshold <-
          temp_thresholds[temp_thresholds$param == "avDayTemp", "thresh"]
        
        p <-
          ggplot(data = data_sub_temp, aes(x = date, y = avDayTemp)) + geom_line(aes(col =
                                                                                       site_id, group = grp))  +
          ylab("Average Daily Temperature (\u00B0C)") +
          xlab("Date") +
          theme_bw() +
          geom_hline(yintercept = threshold,
                     linetype = 2,
                     col = "red") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          coord_cartesian(xlim=ranges$x, ylim=ranges$y, expand=F)
        
      }
      if (param == "avWeek") {
        threshold <-
          temp_thresholds[temp_thresholds$param == "avWeek", "thresh"]
        
        p <-
          ggplot(data = data_sub_temp, aes(x = day1week, y = avWeek, col = site_id)) + geom_point(aes(shape =
                                                                                                        site_id), size = 2) +
         ylab("MWAT (\u00B0C)") + xlab("Date") +
          geom_hline(yintercept = threshold,
                     linetype = 2,
                     col = "red") +
          scale_shape_manual(values = seq(1, 15, 1)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          coord_cartesian(xlim=ranges$x, ylim=ranges$y, expand=F) 
      }
      
      return(p + 
               ggtitle(paste("Creeks:", paste(unique(data_sub_temp$creek)))))
    }
    else {
      NULL
    }
  }
  
  
  
  temp_timeseries_1 <- reactive({
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    
    
    if (input$temp_param == "avDayTemp") {
      data_sub_temp <- df_temp_7DAVG %>%
        dplyr::filter(as.Date(date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(date) - as.Date(filter_dates[2])<= 0,
                      ws == input$temp_ws,
                      plot_cat == 1)
    }
    if (input$temp_param == "avWeek") {
      data_sub_temp <- df_temp_MWAT %>%
        dplyr::filter(as.Date(day1week) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(day1week) - as.Date(filter_dates[2])<= 0,
                      ws == input$temp_ws,
                      plot_cat == 1)
    }  
    return(time_plot_function(data_sub_temp = data_sub_temp, param = input$temp_param) + 
             scale_x_date(
               date_breaks = "3 months",
               labels = date_format("%b-%Y"),
               date_minor_breaks = "1 month",
               limits = filter_dates,
               expand = c(0, 0)
             ))
    
  })
  

  temp_timeseries_2 <- reactive({
    
    filter_dates <- as.Date(cut(as.POSIXct(input$temp_dates,tz=''),"month"))
    
    if (input$temp_param == "avDayTemp") {
      data_sub_temp <- df_temp_7DAVG %>%
        dplyr::filter(as.Date(date) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(date) - as.Date(filter_dates[2])<= 0,
                      ws == input$temp_ws,
                      plot_cat == 2)
    }
    if (input$temp_param == "avWeek") {
      data_sub_temp <- df_temp_MWAT %>%
        dplyr::filter(as.Date(day1week) - as.Date(filter_dates[1])>= 0 & 
                        as.Date(day1week) - as.Date(filter_dates[2])<= 0,
                      ws == input$temp_ws,
                      plot_cat == 2)
      
    }
    p <-
      time_plot_function(data_sub_temp = data_sub_temp, param = input$temp_param) + 
      scale_x_date(
        date_breaks = "3 months",
        labels = date_format("%b-%Y"),
        date_minor_breaks = "1 month",
        limits = filter_dates,
        expand = c(0, 0)
      )
    
    return(p )
  })
  
  
  
  output$temp_timeseries_1 <- renderPlot({
    p <- temp_timeseries_1()
    return(p)
  })
  
  
  observeEvent(input$temp_timeseries_1_brush,{
    brush <- input$temp_timeseries_1_brush
      ranges$x <- c(as.Date(brush$xmin, origin="1970-01-01"), as.Date(brush$xmax, origin="1970-01-01"))
      ranges$y <- c(brush$ymin, brush$ymax)
  })
  
  observeEvent(input$keyPressed, {
    ranges$x <- NULL
    ranges$y <- c(0,30)
      })
  
  
  
  output$temp_timeseries_2 <- renderPlot({
    p <- temp_timeseries_2()
    return(p)
  })
  
  
  # Creek Trash 
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

