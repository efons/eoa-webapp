# EOA's website redesign
# Data visualization app
# Emilie Fons
# Michaela Palmer


# Script 2 - User interface


# The APP

# User interface
ui_db <- dashboardPage(
  
  # implement js package
  
  
  
  # Header: Page title
  dashboardHeader(title = "Creek Monitoring Data", titleWidth = 250),
  
  
  # Sidebar with different types of monitoring data
  dashboardSidebar(
    
    
    width = 250,
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
        menuSubItem("Continuous Temperature", tabName = "con_temp"),
        menuSubItem("Seasonal Water Quality", tabName= "con_wq"),
        menuSubItem("Bacterial Indicators", tabName = "pathogens"),
        menuSubItem("Chlorine", tabName = 'chlorine'),
        menuSubItem("Pesticides", tabName = "pesticide")
      ),
      menuItem(
        "Pollutants of Concern",
        tabName = "poc",
        icon = icon("exclamation")
      ),
      menuItem("Trash in creeks", tabName = "trash", icon = icon("trash"))
    )
  ),
  
  
  # Body of the webpage
  dashboardBody(
    tags$head(tags$style(
      HTML('.content-wrapper {
           overflow-y:scroll
           } 
           .small-box {height: 100px}')

)),

 #.box {margin:10px}
            #.#tabBox { height:450px;margin:2px; padding=0px}
      # to reduce margins between boxes? 


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
    h5("Browse the sidebar menu items on the left to explore the data!"),
    br(),
    
    tagList("For more information about the program, and to access reports and analyses of these monitoring results, go to the", a("SCVURPPP Website", href="http://scvurppp.org/scvurppp_2018/")),
    h5("For any questions or issues regarding this web application, please contact: email address TBD")
    
  
    )),
  
  tabItem(
    tabName = "bio_data",    
    tags$div(title="Click here for instructions",actionLink(inputId="bio_tab_title", label=" ", icon=icon("question-circle"))),

    h2("Biological Condition Assessment"),
 
      # Box for score choice
     box(width=12,
        div(style = "font-weight:bold; color:orange; text-align:center",
            fluidRow(
              h4(" Explore Creek Health Scores in the Santa Clara Basin")
            )),
  
        column(4,
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
          ), offset=4
        ),
        tags$div(title="Click here for description",actionLink(inputId="score_popup", label="What is this?", icon=NULL))
        

      ),
      
    
    
      # Filter inputs
      fluidRow(
        column(3,
             box(width=12, height = 600,
                 
                 
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
                                  choices = c(".csv", ".xlsx", ".shp"))
                  
                     
                )
             ),
        
        
        
        
        
        
      
      # MAP
      
      column(9,
             
             fluidRow(valueBoxOutput("vbox_vla", width=3),
                      valueBoxOutput("vbox_la", width=3),
                      valueBoxOutput("vbox_pi", width=3),
                      valueBoxOutput("vbox_li",width=3)),
      
      box(width = 12, id="stressor_var", title="Stressor Variables:",
          collapsible = T, collapsed=T,
          
          
          pickerInput(inputId = "size_by",
                      label = tags$i("Explore the relationship between the selected score and potential Stressors by selecting a variable, and clicking the 'Detailed Plots' or 'Detailed Table' tabs below:"),
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
      
      
      
      tabBox(id="all_outputs",
        width = 12,
       
      
       tabPanel(title="Map", id="map",
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
       tabPanel(title='Plot', id="summary_plot",
                plotOutput("barplot"),      
                prettyCheckbox(inputId="show_bar_pct", label= "Show as %?")
       ),
       
       
       tabPanel(
         title = "Detailed Plots",
         div(id = "detailed_plots",
           style = "overflow-y: scroll; height: 700px",
           
           tags$i(h5("Select a potential stressor variable in the dropdown menu above to explore its relationship to creek health scores.")),
           br(),
           div(style = "font-weight:bold", textOutput("ws_list_2")),
           br(),           
           div(style = "font-weight:bold", textOutput("scatterplots")),
           
           uiOutput("cond_scatter"),
           
           actionLink(inputId="interpret_scatter", label="Help on interpreting scatterplot coefficients", icon = icon("question-circle")),
           br(),
           br(),
           div(style = "font-weight:bold", textOutput("boxplots")),
           br(),
           uiOutput("cond_boxplot"),
           plotOutput("boxplot2")
         )
       ),
       tabPanel(id="detailed_table_tab",
         title = "Detailed Table",
         
         div(id = "detailed_table",
           style = "overflow-y: scroll; height: 700px",
           
           tags$i(h5("Select a potential stressor variable in the dropdown menu above to explore its relationship to creek health scores.")),
           tags$i(h5("You can download this table, as well as all the other monitoring results for the selected watersheds and time period, using the 'Download Data' tool on the left of the page.")),
           br(),
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
         title = "Site-specific",
         div(
         id = "site_info",
         tags$i(h5("Click on a site on the map to visualize site-specific information")),
         textOutput("site_info"),
         tableOutput("table_site_onClick"))
       )
       
       
       
  
        
        )))),
    

    

           
    
      
      






tabItem(
  tabName = "con_wq",
  h2("Seasonal Water Quality Monitoring"),
  
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

tabItem(tabName="con_temp",
        h2("Continuous Temperature Monitoring"),
        
        # Box for inputs
        fluidRow(
          box(title="Data Filters",
              width = 4,
              sliderInput(
                inputId = "temp_dates",
                label = NULL,
                min = wq_vars_date[1],
                max = wq_vars_date[2],
                value = wq_vars_date,
                timeFormat= "%b %Y"
              ),
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
              )
              
              
              
          ),
          #box for map
          box(width = 8, leafletOutput("map_temp"))
        ),
        
        # Plot Outputs
        box(width=12,
        id = "con_temp_plot",
        selectInput(
          inputId = "temp_ws",
          label = "Watershed:",
          choices = as.character(wq_vars_ws),
          selected = as.character(wq_vars_ws[1])
        ),
        plotOutput("temp_timeseries_1"),
        plotOutput("temp_timeseries_2"))),



tabItem(tabName = "pathogens",
        h2("Bacterial Indicators"),
        h4("Under construction")),
        

tabItem(tabName = "chlorine",
        h2("Chlorine"),
        h4("Under construction")),

tabItem(tabName = "pesticide",
        h2("Pesticides"),
        h4("Under Construction")),




# POC Menu Item
tabItem(
  tabName = "poc",
  h2("Pollutants of Concern"),
  h4("Concentration of Mercury and PCBs in sediments across the Santa Clara Valley"), 
  h4("Under construction"), 
  
  # Box for inputs 
  column(4,box(width=12, selectInput(inputId="poc_contaminant",label="Contaminant:", choices=c("Mercury"="hg", "PCB"="pcb"), selected = "hg"),
                sliderInput(inputId="poc_yr", label="Time period", min =min(poc_vars_yr), max=max(poc_vars_yr), value=c(min(poc_vars_yr),max(poc_vars_yr)), sep=""),
               br(),
               downloadButton("downloadData_poc", label = "Data"),
               # Input: Choose file type ----
               radioButtons("file_type_poc", NULL, inline = T,
                            choices = c(".csv", ".xlsx")))),
   # Box for outputs 
          column(8,tabBox(width=12,
             tabPanel(title="Map",
                     leafletOutput("map_poc")),
             tabPanel(title="Plot",plotOutput("plot_poc_1", height="400px"),
                      prettyCheckbox(inputId="show_bar_pct_POC", label= "Show as %?"))))
  

  
),


# Trash Menu Item
tabItem(tabName = "trash",
        h2("Trash Pollution"),
        h4("Receiving Water Monitoring... Under construction"))
    )
  )
)





###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################
