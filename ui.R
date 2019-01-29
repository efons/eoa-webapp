# EOA's website redesign
# Data visualization app
# Emilie Fons
# Michaela Palmer


# Script 2 - User interface


# The APP

# User interface
ui_db <- dashboardPage(
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
           }')
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
    
    column(
      6,
      # Box for inputs
      fluidRow(box(
        width = 12,
        status = "warning",
        div(style = "font-weight:blod; color:orange; text-align:center",
            fluidRow(
              h4("Explore Creek Health Scores in the Santa Clara Basin")
            )),
        column(
          6,
          selectInput(
            inputId = "filter_by",
            label = NULL,
            choices = c(
              "Creek Health Score (CSCI)" = "csci",
              "Algae Health Score (ASCI Hybrid)" = "asci_hyb",
              "Algae Health Score (ASCI Diatom)" = "asci_diatom"
            ),
            selected = "csci"
          ),
          offset = 3
        )
      )),
      
      # Box for Map
      box(
        width = NULL,
        status = 'primary',
        fluidRow(
          column(
            4,
            sliderInput(
              inputId = "wy",
              label = NULL,
              ticks = T,
              min = min(bio_vars_yr),
              max = max(bio_vars_yr),
              value = c(min(bio_vars_yr), max(bio_vars_yr)),
              sep = ""
            )
          ),
          column(
            4,
            selectInput(
              inputId = "spatial_filter",
              label = "Spatial Scale:",
              choices = c(
                "All of Santa Clara Basin" = "whole_county",
                "Watershed-level" = "sub_ws"
              ),
              selected = "whole_county"
            )
          ),
          column(4, uiOutput("scnd_sub_ws"),
                 tags$head(tags$style(".leaflet-top {z-index:999!important;}")))
        ),
        fluidRow(column(12, leafletOutput("map_sites"))),
        fluidRow(column(
          3, actionButton("reset_button", "Reset view")
        ),
        column(
          6,
          prettyCheckbox(
            inputId = "show_radius",
            label = "Show Stressors as marker size?",
            value = F,
            shape = "round",
            animation = "pulse",
            fill = F
          )
        ))
      )
    ),
    
    
    column(
      6,
      
      box(width = 12,
          column(
            6,
            selectizeInput(
              tags$style(type = 'text/css', ".selectize-dropdown-content {max-height: 200px; }"),
              inputId = "size_by",
              label = "Potential Stressors:",
              choices = NULL
            ),
            offset = 3
          )),
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
            # Download Table button
            downloadButton("downloadData", label = "Download Table"),
            # Input: Choose file type ----
            radioButtons("file_type", NULL, inline = T,
                         choices = c(".csv", ".xlsx")),
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
        )
        
          )
      )
),




tabItem(
  tabName = "con_wq",
  h2("Continuous Water Quality & Continuous Temperature"),
  
  # Box for inputs
  fluidRow(
    box(
      width = 3,
      sliderInput(
        inputId = "wq_yr",
        label = "Year:",
        min = min(wq_vars_yr),
        max = max(wq_vars_yr),
        value = c(min(wq_vars_yr), max(wq_vars_yr)),
        sep = ''
      ),
      selectInput(
        inputId = "wq_ws",
        label = "Watershed:",
        choices = c(wq_vars_ws),
        selected = wq_vars_ws[1]
      )
    ),
    #box for map
    box(width = 9, leafletOutput("map_wq"))
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
