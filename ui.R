# EOA's website redesign
# Data visualization app
# Emilie Fons
# Michaela Palmer


# Script 2 - User interface


# The APP

# User interface
ui_db <- dashboardPage(
  
  # implement js package
  useShinyjs(),
  
  
  # Header: Page title
  dashboardHeader(title = "Monitoring Data", titleWidth = 200),
  
  
  # Sidebar with different types of monitoring data
  dashboardSidebar(
    
    
    width = 200,
    sidebarMenu(
      id = "menu_items",
      selected = "water_quality",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem(
        "Water Quality",
        tabName = "water_quality",
        icon = icon("tint"),
        startExpanded = T,
        menuSubItem("Biomonitoring Data", tabName = "bio_data"),
        menuSubItem("Continuous Water Quality", tabName = "con_wq"),
        menuSubItem("Pathogens", tabName = "pathogens"),
        menuSubItem("Chlorine", tabName = 'chlorine'),
        menuSubItem("Pesticides", tabName = "pesticide")
      ),
      menuItem(
        "Pollutants of Concern",
        tabName = "poc",
        icon = icon("exclamation")
      ),
      menuItem("Trash", tabName = "trash", icon = icon("trash"))
    )
  ),
  
  
  # Body of the webpage
  dashboardBody(
    tags$head(tags$style(
      HTML('.content-wrapper {
           overflow-y:scroll
           } ')
            #.box {margin:10px}
            #.#tabBox { height:450px;margin:2px; padding=0px}
      # to reduce margins between boxes? 
          
)),





tabItems(
  tabItem(
    tabName = "overview",
    h2("Overview"),
    h5("Welcome to EOA's data visualization dashboard!"),
    h5("Presentation of the data: blah blah blah"),
    h5("Browse the sidebar menu items on the left to explore the data!")
  ),
  
  tabItem(
    tabName = "bio_data",
    h2("Biological Condition Assessment"),
    h4("Start by selecting an indicator of creek health. Each indicator is computed based on the diversity and number of individuals for a specific aquatic species: benthic macrovertebrates, algae, etc. Environmental stress might affect the survival of these species. Therefore, these biological measures are a good indicator of creek health."),

      # Box for score choice
      box(
        width = 12,
        status = "warning",
        div(style = "font-weight:bold; color:orange; text-align:center",
            fluidRow(
              h4(" Explore Creek Health Scores in the Santa Clara Basin")
            )),
  
        
        fluidRow(
        column(6,
          pickerInput(
            inputId = "filter_by",
            label = NULL,
            choices = 
              list("California Stream Condition Index (CSCI)"=c('Indicator: Benthic Macroinvertebrate'="csci"),
                   "Algae Stream Condition Indices (ASCIs)"=c("Indicator: Soft Algae"="asci_soft_alg",
                                                      "Indicator: Diatoms" = "asci_diatom",
                                                      "Indicator: Diatoms-Soft Algae (Hybrid)" = "asci_hyb"
                                                      )
                   ),
            selected = "csci",
            multiple=F
          ),
          offset=3
        )
    
      )),
    
    
      # Filter inputs
      fluidRow(
        column(3,
             box(width=12, height = 500,
                 
                 
                       h4("Data Filters:")
                     ,
                 
                     sliderInput(
                       inputId = "wy",
                       label = "Years:",
                       ticks = T,
                       min = min(bio_vars_yr),
                       max = max(bio_vars_yr),
                       value = c(min(bio_vars_yr), max(bio_vars_yr)),
                       sep = ""
                   ),
                 
                  
                     selectInput(
                       inputId = "spatial_filter",
                       label = "Spatial Scale:",
                       choices = c(
                         "All of Santa Clara Basin" = "whole_county",
                         "Watershed-level" = "sub_ws"
                       ),
                       selected = "whole_county"
                   ),
                  
                   uiOutput("scnd_sub_ws"),
                          tags$head(tags$style(".leaflet-top {z-index:999!important;}")),
                 
              
                
                 br(),
                 
                 
                       h4("Data Download:"),
                     
 
                     
                     # Download Table button
                 
                 downloadButton("downloadData", label = "Data"),
                     # Input: Choose file type ----
                radioButtons("file_type", NULL, inline = T,
                                  choices = c(".csv", ".xlsx"))
                  
                     
                )
             ),
      
      # MAP
      
      column(9,
             
      materialSwitch(inputId="show_details", label = "Show more details?", status="warning"), 
      
      
      tabBox(id="summary_bio",
        width = 12,
       
      
       tabPanel(title="Map",
        fluidRow(column(12, leafletOutput("map_sites"))),
        fluidRow(column(
          3, actionButton("reset_button", "Reset view")
       ), column(9,
                 prettyCheckbox(
                   inputId = "show_radius",
                   label = "Stressors as marker size?",
                   value = F,
                   shape = "round",
                   animation = "pulse",
                   fill = F
                 )))), 
       tabPanel(title='Plot',
                plotOutput("barplot"),      
                prettyCheckbox(inputId="show_bar_pct", label= "Show as %?")
       ),
       tabPanel(title="Score categories",
                dataTableOutput("score_desc"))
        
        ),tags$head(tags$style(
          HTML('
               .#tabBox {height:450px;margin:2px; padding=0px}
               '))))),
    
    br(),
    br(), 
    
    fluidRow(
    column(8,
           
      

      box(width = 12,
          
        
              pickerInput(inputId = "size_by",
                          label = "Explore the relationship with potential Stressors:",
                          choices = list(
                            "Habitat" = c(
                              "Total PHAB" = "tot_phab",
                              "Epifaunal Substrate" =
                                "epifaun_substr",
                              "Sediment Deposition" = "sed_deposition",
                              "Shannon Diversity (Natural Substrates)" = "shannon_nst",
                              "% Substrate Smaller than Sand (<2 mm)" = "pct_smaller_sand",
                              "Percent Boulders - large & small" = "pct_boulder_ls",
                              "Percent Fast Water of Reach" =
                                "pct_fast_water",
                              "IPI Score" = "ipi",
                              "% Impervious Area - Watershed" =
                                "pct_imperv_ws",
                              "Road density - Watershed" = "road_dsty_ws"
                            ),
                            "Biomass" = c(
                              "Chlorophyll a (mg/m2)" = "chloro_a_mg_m2",
                              "AFDM (g/m2)" = "afdm_g_m2",
                              "% Macroalgae Cover" = "pct_macroalg_cvr"
                            ),
                            "Nutrients" = c(
                              "Total Nitrogen (mg/L)" = "tn_mg_l",
                              "Total Phosphorus(mg/L)" = "tp_mg_l",
                              "Unionized Ammonia (ug/L)" = "uia_ug_l"
                            ),
                            "Water Quality" = c(
                              "Temperature (C)" = "temp_c",
                              "Dissolved Oxygen" =
                                "do_mg_l",
                              "Conductivity (uS/cm)" = "sp_cond_us_cm"
                            ),
                            "Other" = c("Human Disturbance Index (HDI)" = "crhdi_swamp")
                          )
                          ,
                          selected = 'tot_phab',
                          options = pickerOptions(actionsBox = F, liveSearch = T),
                          multiple = F)
            
          ),
      
      
      # TabBox for outputs
      tabBox(
        id = "output_tabs",
        width = 12,

        tabPanel(
          title = "Plots",
          id = "overview_plots",
          div(
            style = "overflow-y: scroll; height: 700px",
            div(style = "font-weight:bold", textOutput("ws_list_2")),
            br(),           


            div(style = "font-weight:bold", textOutput("scatterplots")),
            
            uiOutput("cond_scatter"),
            
            div(style = "font-weight:bold", textOutput("boxplots")),
            br(),
            uiOutput("cond_boxplot"),
            plotOutput("boxplot2")
          )
        ),
        tabPanel(
          title = "Table",
          id = "overview_table",
          div(
            style = "overflow-y: scroll; height: 700px",
            div(style = "font-weight:bold", textOutput("ws_list_1")),
            br(),
            div(textOutput("score_tables"), style = "font-weight:bold"),
            br(),
            div(uiOutput("cond_table"), style = "font-size:90%"),
            tags$head(tags$style("#cond_table td{
                                 position:relative;
                                 };"))
                              )
            ),
        
        tabPanel(
          title = "Watershed-specific",
          id = "ws_info",
          h5("Click on a watershed to get detailed information"),
          textOutput("ws_info")
        ),
        
        tabPanel(
          title = "Site-specific",
          id = "site_info",
          h5("Click on a site on the map to visualize site-specific information"),
          textOutput("site_info"),
          tableOutput("table_site_onClick")
        ),
        
        tabPanel(
          title = "Data Download",
          id = "dwnload",
          h5("Filter data and download various file types below.")
          
        )
        
        
          )
      , offset=2)))
,




tabItem(
  tabName = "con_wq",
  h2("Continuous Water Quality & Continuous Temperature"),
  
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
        inputId = "wq_ws",
        label = "Watershed:",
        choices = as.character(wq_vars_ws),
        selected = as.character(wq_vars_ws[1])
      )
    ),
    #box for map
    box(width = 8, leafletOutput("map_wq"))
  ),
  
  # Box for plots
  fluidRow(
    tabBox(
      width = 12,
      height = 800,
      id = "wq_plots",
      tabPanel(
        title = "Continuous Temperature",
        id = "con_temp_plot",
        selectInput(
          inputId = "temp_param",
          label = "Parameter:",
          choices = c(
            "Continuous Temperature" = "ConTemp",
            "Daily Average" = "avDayTemp",
            "Daily Max" = "maxDayTemp",
            "Weekly Average" = "avWeek",
            "Weekly Maximum" = "maxWeek"
          ),
          selected = "avDayTemp"
        ),
        plotOutput("temp_timeseries_1"),
        plotOutput("temp_timeseries_2")
      ),
      tabPanel(
        title = "Seasonal Water Quality Parameters",
        id = "wq_plot",
        fluidRow(column(
          3,
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
          )
        ),
        column(
          3,
          selectInput(
            inputId = "wq_season",
            label = "Season:",
            choices = c("Spring" = "S", "Fall" =
                          "F"),
            selected = "S"
          )
        )),
        fluidRow(
          column(6, plotOutput("wq_boxplot_1"),
                 plotOutput("wq_boxplot_2")),
          
          column(6, plotOutput("wq_beanplot_1"),
                 plotOutput("wq_beanplot_2"))
        )
      )
    )
  )
),



tabItem(tabName = "pathogens",
        h2("Pathogens")),

tabItem(tabName = "chlorine",
        h2("Chlorine")),

tabItem(tabName = "pesticide",
        h2("Pesticides")),




# POC Menu Item
tabItem(
  tabName = "poc",
  h2("Pollutants of Concern"),
  
  # Box for inputs
  fluidRow(box(
    selectInput(
      inputId = "poc_contaminant",
      label = "Contaminant:",
      choices = c("Mercury" = "hg", "PCB" = "pcb"),
      selected = "hg"
    ),
    sliderInput(
      inputId = "poc_yr",
      label = "Time period",
      min = min(poc_vars_yr),
      max = max(poc_vars_yr),
      value = c(min(poc_vars_yr), max(poc_vars_yr)),
      sep = ""
    )
  ),
  # Box for plots
  box(
    plotOutput("plot_poc_1", height = "200px")
  )),
  
  # Box for map
  fluidRow(box(width = 12, leafletOutput("map_poc")))
  
  
  
),


# Trash Menu Item
tabItem(tabName = "trash",
        h2("Trash Data"))
    )
  )
)





###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################
