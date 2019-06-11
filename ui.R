  # EOA's website redesign
  # Data visualization app
  # Emilie Fons
  # Michaela Palmer
  
  
  # Script 2 - User interface
  
  
  # The APP
  
  # User interface
  ui_db <- dashboardPage(
   
    # Header: Page title
    dashboardHeader(title = "Creek Monitoring Data", titleWidth = 250),
  
    # Sidebar menu with different types of monitoring data
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "menu_items",
        selected = "water_quality",
        menuItem("Homepage", tabName = "overview", icon = icon("home")),
        menuItem(
          "Creek Status Monitoring",
          tabName = "water_quality",
          icon = icon("tint"),
          startExpanded = T,
          menuSubItem("(i) Biological Assessments", tabName = "bio_data"),
          menuSubItem("(ii) Chlorine", tabName = 'chlorine'),
          menuSubItem("(iii) Continuous Temperature", tabName = "con_temp"),
          menuSubItem("(iv) Seasonal Water Quality", tabName= "con_wq"),
          menuSubItem("(v) Pathogen Indicators", tabName = "pathogens")),
        menuItem(
          "Pollutants of Concern",
          tabName = "poc",
          icon = icon("exclamation")),
        menuItem("Pesticides", tabName = "pesticide", icon=icon("spray-can")),
        menuItem("Trash load", tabName = "trash", icon = icon("trash")))),
    
    
    # Body of the webpage
    dashboardBody(
      tags$head(tags$style(
        HTML('.content-wrapper {
             overflow-y:scroll
        }
            .box.box-solid.box-primary>.box-header {
                                color:#FFFFFF;
                                background-color:#007bff;}

                                .box.box-solid.box-primary{
                                border-bottom-color:#007bff;
                                border-left-color:#007bff;
                                border-right-color:#007bff;
                                border-top-color:#007bff;
                                }
             .small-box {height: 100px}
             
            .bg-aqua {background-color: #007bff!important }
             ')))
      ,
    
      tags$script(js),
      
# Overview #################################################################################################################################        
      
      tabItems(
        tabItem(
          tabName = "overview",
          img(src="scvurppp_logo.png", width='600px'),
          br(), 
          br(), 
          box(width=12,status="primary",
          h4("Welcome to the SCVURPPP data visualization dashboard."),
          br(), 
  
          h5("Data is made available thanks to biological, physical and chemical monitoring efforts across Santa Clara Valley over several years.
              Explore data, maps, graphs and interactive features as they become available."),
          HTML("Browse the sidebar menu items on the left to explore the data! Each tab corresponds to a provision of the Municipal Regional Permit (MRP)
                that regulates the different aspects of stormwater pollution for most municipalities of the San Francisco Bay Area. More specifically, the data collected by SCVURPPP and presented in this application 
                corresponds to the monitoring requirements under the following provisions of the MRP:
                <br/>
                <br/>
                <b>C.8: Water Quality Monitoring:</b>
                <ul> <li><b>C.8.d: Creek Status Monitoring</b></li>
                <ul><li> (i) Biological Assessments, including nutrients and general Water Quality Parameters </li>
                <li> (ii) Chlorine </li>
                <li> (iii) Temperature </li>
                <li> (iv) Continuous Monitoring of Dissolved Oxygen, Temperature, pH </li>
                <li> (v) Pathogen Indicators </li></ul>
                <li><b>C.8.f: Pollutants of Concern Monitoring</b></li>
                <li><b>C.8.g: Pesticides and Toxicity Monitoring</b></li></ul>
                <b>C.10: Trash Load Reduction (section iii.v: Receiving Water Monitoring)</b>
                </br>"),
          br(),
          br(),
      
          tagList("For more information about the program, and to access reports and analyses of these monitoring results, go to the", a("SCVURPPP Website", href="http://scvurppp.org"),"."),
          h5("For any questions or issues regarding this web application, please contact: email address TBD"))),
        
        
# Bioassessment #################################################################################################################################        
        tabItem(
          tabName = "bio_data", 
          tags$div(title="Click here for instructions",
                   actionLink(inputId="bio_tab_title", label="Biological Condition Assessment", style="font-size:160%")),
          br(),


        # Box for inputs 
        fluidRow(
          column(4,
               box(width=12, height = 650,
                   # Filters : years and watersheds 
                   h4("Data Download:"),
                       sliderInput(
                         inputId = "wy",
                         label = "Choose Years:",
                         ticks = T,
                         min = min(bio_vars_yr),
                         max = max(bio_vars_yr),
                         value = c(min(bio_vars_yr), max(bio_vars_yr)),
                         sep = ""),
                    
                   pickerInput(
                     inputId = "ws",
                     label = "Choose Watersheds:",
                     choices = as.character(bio_vars_ws),
                     selected = as.character(bio_vars_ws),
                     options = list(`actions-box` = TRUE, size = 20),
                     multiple = T),
                   checkboxGroupInput("score_dwlnd", label="Choose Biological Scores:",
                                      inline=F, choices=c('CSCI'="csci",
                                                          "ASCI: Soft Algae"="asci_soft_alg",
                                                          "ASCI: Diatoms" = "asci_diatom",
                                                          "ASCI: Hybrid" = "asci_hyb",
                                                          "CRAM" = "cram"), selected=c("csci", "asci_soft_alg", "asci_diatom", "asci_hyb", "cram")),
                   br(),
                   

                   
                   downloadButton("downloadData", label = "Data Download", style="background-color: #3c8dbc; border-color: #367fa9; color:white"),                   # Download options
                   radioButtons("file_type", label=NULL, inline = T,
                                    choices = c(".csv", ".xlsx", ".shp")), 
                   br(),
                   div(style="font-style:italic",textOutput("bio_dwld_info"))))
                   ,


        
        # Box with all outputs: map, graphs 
        column(8,
        box(width = 12,
            h4("Overview of data:"),
            
            column(2, offset=10,
            dropdownButton(
              tags$h3("Score shown on Map"),
              # Box for bio score selection
                         uiOutput("score_subset"), 

              circle = TRUE, status = "primary", icon = icon("gear"), width = "300px",
              tooltip = tooltipOptions(title = "Click to change score shown on map"), right=T
            ), tags$head(tags$style(".leaflet-top {z-index:999!important;}"))),
            
            
            #  MAP
                  fluidRow(column(10, offset=1, 
                                  div(style="font-weight:bold",textOutput("map_title")),
                                  br(),
                                 leafletOutput("map_sites"))),
                  br(),
                  fluidRow(column(3,offset=1,actionButton("reset_button", "Reset view"))), 
                  br(),
         
        # Summary plot 
                  fluidRow(column(10,offset=1,
                                  div(style="font-weight:bold",textOutput("barplot_title")),
                                  br(),
                                  plotOutput("barplot"))),      
                  column(3,offset=1,prettyCheckbox(inputId="show_bar_pct", label= "Show as %?")),
            fluidRow(column(10, offset=1, 
                        div(style="font-weight:bold",textOutput("boxplot_title")),
                        br(), 
                        plotOutput("bio_boxplot"))))))),

      
  
             
      
        
        
  
  
# Continuous Water Quality #################################################################################################################################        

  
  tabItem(
    tabName = "con_wq",
  
    actionLink(inputId="wq_desc",  label="Seasonal Water Quality Monitoring", style="font-size:160%"),
    br(), 
    
    # Box for inputs
    fluidRow(
      box(title="Data Filters",
        width = 4,
        sliderInput(
          inputId = "wq_dates",
          label = NULL,
          min = wq_vars_date[1],
          max = wq_vars_date[2],
          value = wq_vars_date,
          timeFormat= "%b %Y"
        ),
        
     
          selectInput(
            inputId = "wq_param",
            label = "Water Quality Parameter:",
            choices = c(
              "Temperature" = "temp_c",
              "pH" = "ph" ,
              "Specific Conductivity" = "sp_cond_us_cm",
              "Dissolved Oxygen" = "do_mg_l"
            ),
            selected = 'ph'
          ),
          selectInput(
            inputId = "wq_season",
            label = "Season:",
            choices = c("Spring" = "S", "Fall" =
                          "F", 
                        "Spring and Fall" = "S_F"),
            selected = "S_F"
          )
      ),
      #box for map
      box(width = 8, leafletOutput("map_wq"))
    ),
    
    # Box for plots
    fluidRow(
      box(
        width = 12,
        
        selectInput(
          inputId = "wq_ws",
          label = "Watershed:",
          choices = as.character(wq_vars_ws),
          selected = as.character(wq_vars_ws[1])
        ),
          fluidRow(
            column(6, plotOutput("wq_boxplot_1"),
                   plotOutput("wq_boxplot_2")),
            
            column(6, plotOutput("wq_beanplot_1"),
                   plotOutput("wq_beanplot_2"))
          )
        )
      )
    )
  ,


# Continuous Temperature ####################################################################################

  tabItem(tabName="con_temp",
          actionLink(inputId="temp_desc",  label="Continuous Temperature Monitoring", style="font-size:160%"),
          br(), 
          
          # Box for inputs
          fluidRow(
            box(h4("Data Download:"),
                width = 4,
                sliderInput(
                  inputId = "temp_dates",
                  label = "Choose years:",
                  min = wq_vars_date[1],
                  max = wq_vars_date[2],
                  value = wq_vars_date,
                  timeFormat= "%b %Y"
                ),
                pickerInput(
                  inputId = "temp_ws_dwld",
                  label = "Choose Watersheds:",
                  choices = as.character(wq_vars_ws),
                  selected = as.character(wq_vars_ws),
                  options = list(`actions-box` = TRUE, size = 20),
                  multiple = T
                ),
                br(), 
                downloadButton("downloadData_temp", label = "Data Download", style="background-color: #3c8dbc; border-color: #367fa9; color:white"),
                # Input: Choose file type -
                radioButtons("file_type_temp", NULL, inline = T,
                             choices = c(".csv", ".xlsx", ".shp"), 
                             selected=".csv") 
  
            ),
            
            #box for summary graphs
            box(width = 8,
                selectInput(
                  inputId = "temp_param",
                  label = "Parameter:",
                  choices = c(
                    "Daily Average" = "avDayTemp",
                    "Weekly Average" = "avWeek"
                  ),
                  selected = "avDayTemp"
                ),
                actionLink(inputId="map_temp_desc", label="Map Explanation", icon=NULL),
                leafletOutput("map_temp"),
          
          
          selectInput(
            inputId = "temp_ws",
            label = "Watershed:",
            choices = as.character(wq_vars_ws),
            selected = as.character(wq_vars_ws[1])
          ),
          br(),
          HTML("<i>Hover over the plot and brush to zoom in. Press 'escape' to zoom out.</i>"),
          plotOutput("temp_timeseries_1", dblclick = "temp_timeseries_1_dbl_click", brush=brushOpts(id="temp_timeseries_1_brush", resetOnNew = T)),
          plotOutput("temp_timeseries_2")))),
  
  
# Pathogens ####################################################################################################
  
  
  tabItem(tabName = "pathogens",
          actionLink(inputId="patho_desc",label="Bacterial Indicators", style="font-size:160%"),
          br(),
          h4("Under construction"),
          
          # Box for inputs 
          column(4,box(width=12, 
                       sliderInput(inputId="patho_yr", label="Choose year:", min =min(df_patho$year), max=max(df_patho$year), value=c(min(df_patho$year),max(df_patho$year)), sep="",step=1),
                       br(),
                       selectInput(
                         inputId = "patho_analyte",
                         label = "Choose Bacterial Indicator:",
                         choices = patho_vars_analyte,
                         selected = "E. coli"
                       ),
                       downloadButton("downloadData_patho", label = "Data"),
                       # Input: Choose file type --
                       radioButtons("file_type_patho", NULL, inline = T,
                                    choices = c(".csv", ".xlsx")))),
          # Box for outputs 
          column(8,tabBox(width=12,
                          tabPanel(title="Map",
                                   leafletOutput("map_patho")),
                          tabPanel(title="Plot",plotOutput("plot_patho", height="400px"))))
          
          
          
          ),
          
  
  
# Chlorine #############################################################################################
  tabItem(
    tabName = "chlorine",
    actionLink(inputId="chlor_desc",label="Chlorine", style="font-size:160%"),
    br(),
    
    h4("Under construction"), 
    
    # Box for inputs 
    column(4,box(width=12, 
                 h4("Data Download:"),
                 sliderInput(inputId="chlo_yr", label="Choose years:", min =min(chlo_vars_yr), max=max(chlo_vars_yr), value=c(min(chlo_vars_yr),max(chlo_vars_yr)), sep="",step=1),
                 pickerInput(
                   inputId = "chlo_ws",
                   label = "Choose Watersheds:",
                   choices = as.character(bio_vars_ws),
                   selected = as.character(bio_vars_ws),
                   options = list(`actions-box` = TRUE, size = 20),
                   multiple = T
                 ),
                 br(), 
                 downloadButton("downloadData_chlo", label = "Data Download", style="background-color: #3c8dbc; border-color: #367fa9; color:white"),
                 # Input: Choose file type -
                 radioButtons("file_type_chlo", NULL, inline = T,
                              choices = c(".csv", ".xlsx", ".shp")))),
    # Box for outputs 
    column(8,box(width=12,
                    h4("Overview of data:"),
                    br(), 
                    
                    column(10, offset=1,leafletOutput("map_chlo")),
                    br(), 
                    br(), 
                 column(10, offset=1,plotOutput("plot_chlo", height="400px"))))
    
    
    
  ),

# Pesticide ##############################################################################################
  tabItem(tabName = "pesticide",
          actionLink(inputId="pesti_desc",label="Pesticides", style="font-size:160%"),
          br(),
          h4("Under Construction"), 
          
          # Box for inputs 
          column(4,box(width=12, 
                       sliderInput(inputId="tox_yr", label="Year",min=min(tox_vars_yr),max=max(tox_vars_yr), value=2018, step=1, sep=""),                     br(),
                       selectInput(inputId="tox_season", label="Season", choices=c("Wet"="W","Dry"= "D"), selected="D"),
                       uiOutput("stressors"),
                       downloadButton("downloadData_tox", label = "Data"),
                       # Input: Choose file type -
                       radioButtons("file_type_tox", NULL, inline = T,
                                    choices = c(".csv", ".xlsx")))),
          # Box for outputs 
          column(8,tabBox(width=12,
                          tabPanel(title="Map",
                                   leafletOutput("map_tox")),
                          tabPanel(title="Plot",plotOutput("plot_tox", height="400px"))))),
  
  
  
  
# POC ##########################################################################################################
  tabItem(
    tabName = "poc",
    actionLink(inputId="poc_desc",label="Pollutants of concern", style="font-size:160%"),
    
    h4("Concentration of Mercury and PCBs in sediments across the Santa Clara Valley"), 
    br(),
    h4("Under construction"), 
    
    # Box for inputs 
    column(4,box(width=12, selectInput(inputId="poc_contaminant",label="Contaminant:", choices=c("Mercury"="hg", "PCB"="pcb"), selected = "hg"),
                  sliderInput(inputId="poc_yr", label="Time period", min =min(poc_vars_yr), max=max(poc_vars_yr), value=c(min(poc_vars_yr),max(poc_vars_yr)), sep=""),
                 br(),
                 downloadButton("downloadData_poc", label = "Data"),
                 # Input: Choose file type -
                 radioButtons("file_type_poc", NULL, inline = T,
                              choices = c(".csv", ".xlsx")))),
     # Box for outputs 
            column(8,tabBox(width=12,
               tabPanel(title="Map",
                       leafletOutput("map_poc")),
               tabPanel(title="Plot",plotOutput("plot_poc_1", height="400px"),
                        prettyCheckbox(inputId="show_bar_pct_POC", label= "Show as %?"))))
    
  
    
  ),
  
# Trash: Receiving Water ####################################################################################
  tabItem(tabName = "trash",
          actionLink(inputId="trash_desc",label="Trash in Receiving Waters", style="font-size:160%"),
          br(),
          h4("Receiving Water Monitoring... Preliminary Results... Targeted Sites only"),
          
          column(3,
                 box(width=12,title="Data Filters",
                  pickerInput(
                   inputId = "trash_city",
                   label = "Choose city:",
                   choices = trash_vars_city,
                   selected = trash_vars_city,
                   options = pickerOptions(actionsBox=T,liveSearch = T),
                   multiple = T
                 ))),
          
          column(9,
          box(width=12,
              leafletOutput("trash_map"))),       
          
          tabBox(width=12,
                 tabPanel(title="Main Pathways",
                          uiOutput("cond_trash_pathway")),
                 tabPanel("Main Trash Items", 
                          plotOutput("hist_items")))
          
          
          )
      )
    )
  )
  
  
  
  
  
  ###############################################################################################################################################################
  ###############################################################################################################################################################
  ###############################################################################################################################################################
