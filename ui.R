
# The APP

# User interface
ui <- dashboardPage(
  
  # Header: Page title
  dashboardHeader(title = "Creek Monitoring Data", titleWidth = 250),
  
  # Sidebar menu with different types of monitoring data
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "menu_items",
      selected = "bio_data",
      menuItem("Homepage", tabName = "overview", icon = icon("home")),
      menuItem(
        "Biological Data",
        tabName = "bio_data",
        icon = icon("bug")),
      menuItem(
        "Nutrients",
        tabName = "nutrients",
        icon = icon("circle")), 
      menuItem(
        "Habitat",
        tabName = "habitat",
        icon = icon("home")), 
      menuItem(
        "Conventional Water Quality",
        tabName = "conv_wq",
        icon = icon("tint"),
        startExpanded = F,
        menuSubItem("Temperature", tabName = "temp"),
        menuSubItem("pH", tabName = 'ph'),
        menuSubItem("Dissolved Oxygen", tabName = "do"),
        menuSubItem("Specific Conductivity", tabName= "sp_cond")),
      menuItem("Pathogens", tabName = "pathogens", icon=icon("microscope")),
      menuItem(
        "Pollutants of Concern",
        tabName = "poc",
        icon = icon("exclamation")),
      menuItem("Pesticides", tabName = "pesticide", icon=icon("spray-can")),
      menuItem("Trash load", tabName = "trash", icon = icon("trash"))
      )
    ),
  
  
  # Body of the webpage
  dashboardBody(
    useShinyjs(),
    
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
           ')
      )
      ),
    
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
            HTML("Browse the sidebar menu items on the left to explore the data!"),
            br(),
            br(),
            
            tagList("For more information about the program, and to access reports and analyses of these monitoring results, go to the", a("SCVURPPP Website", href="http://scvurppp.org"),"."),
            h5("For any questions or issues regarding this web application, please contact: email address TBD")
            )
        ),
      
      
    # Bioassessment ##############################################################################################
      tabItem(
        tabName = "bio_data", 
        tags$div(title="Click here for instructions",
                 actionLink(inputId="bio_tab_title", label="Biological Data", style="font-size:160%")),
        br(),
  
        
        # Box for inputs 
        column(4,
               box(width=12, height = 650,
                   h4("Data Download:"),
                   
                   # Filters : years 
                   sliderInput(
                     inputId = "bio_wy",
                     label = "Choose Years:",
                     ticks = T,
                     min = min(bio_vars_yr),
                     max = max(bio_vars_yr),
                     value = c(min(bio_vars_yr), max(bio_vars_yr)),
                     sep = ""),
                   checkboxGroupInput("bio_assmnt_type", label=NULL,
                                      choices=c("Historical Data"="Historical SCVURPPP", 
                                                "MRP Data"= "SCVURPPP"), 
                                      selected=c("Historical SCVURPPP","SCVURPPP")), 
                   
                   # Filters : watersheds 
                   pickerInput(
                     inputId = "bio_ws",
                     label = "Choose Watersheds:",
                     choices = as.character(bio_vars_ws),
                     selected = as.character(bio_vars_ws),
                     options = list(`actions-box` = TRUE, size = 20),
                     multiple = T),
                   
                   # Filters : Scores 
                   checkboxGroupInput("bio_score_dwlnd", label="Choose Biological Scores:",
                                      inline=F, choices=c('CSCI'="csci",
                                                          "ASCI: Soft Algae"="asci_soft_alg",
                                                          "ASCI: Diatoms" = "asci_diatom",
                                                          "ASCI: Hybrid" = "asci_hyb",
                                                          "CRAM" = "cram"), selected=c("csci", "asci_soft_alg", "asci_diatom", "asci_hyb", "cram")),
                   br(), 
                   
                   
                  
                   # Downloading tool
                   downloadButton("bio_downloadData", label = "Data Download", style="background-color: #3c8dbc; border-color: #367fa9; color:white"),                   # Download options
                   radioButtons("bio_file_type", label=NULL, inline = T,
                                choices = c(".csv", ".xlsx", ".shp")), 
                   br()
                   )
               ),
        
        
        # Box with all outputs: map, graphs 
        column(8,
               box(width = 12,
                   h4("Overview of data:"), 
                   column(4, offset=8,
                          
                          # Map Options 
                          uiOutput("bio_score_subset")
                          ),
                          
                          #  MAP
                   column(10, offset=1, 
                          div(style="font-weight:bold",textOutput("map_bio_title")),
                          br(),
                          column(4, offset=8,                           
                                 actionLink(inputId= "bio_score_popup", label="Definition and Legend")
                                 ),
                          leafletOutput("map_bio")
                          ),
                   br(),
                   br(), 
                   column(3,offset=1,actionButton("bio_reset_button", "Reset view")),
                   br(),
                          
                          
                          # Summary plot 
                          fluidRow(column(10,offset=1,
                                   div(style="font-weight:bold",textOutput("bio_barplot_title")),
                                   br(),
                                   plotOutput("bio_barplot"))), 
                          column(3,offset=1,prettyCheckbox(inputId="bio_show_bar_pct", label= "Show as %?")),
                          fluidRow(column(10, offset=1, 
                                   div(style="font-weight:bold",textOutput("bio_boxplot_title")),
                                   br(), 
                                   plotOutput("bio_boxplot"))
                                   )
                          )
                   )
               
        ),
    
    

      
    
    
    # Nutrients ##########################################################################################################
    tabItem(
      tabName = "nutrients",
      actionLink(inputId="nutrients_desc",label="Nutrients", style="font-size:160%"),
      br(),
      
      # Box for inputs 
      column(4,box(width=12, 
                   h4("Data Download:"),
                   # Filters : years 
                   sliderInput(
                     inputId = "nut_wy",
                     label = "Choose Years:",
                     ticks = T,
                     min = min(bio_vars_yr),
                     max = max(bio_vars_yr),
                     value = c(min(bio_vars_yr), max(bio_vars_yr)),
                     sep = ""), 
                   
                   # Filters : watersheds 
                   pickerInput(
                     inputId = "nut_ws",
                     label = "Choose Watersheds:",
                     choices = as.character(bio_vars_ws),
                     selected = as.character(bio_vars_ws),
                     options = list(`actions-box` = TRUE, size = 20),
                     multiple = T),
                   
                   # Filters : Scores 
                   pickerInput("nut_type_dwlnd", label="Choose Nutrients:",
                               choices=c('Nitrate'="nitrate_mg_l",
                                                          "Nitrite"="nitrite_mg_l",
                                                          "TKN" = "tkn_mg_l",
                                                          "Total Nitrogen" = "tn_mg_l",
                                                          "Total Phosphorus" = "tp_mg_l", 
                                         "Orthophosphate"="orthophosph_mg_l",
                                         "Silica"= "silica_mg_l", 
                                         "Ammonia" = "ammonia_mg_l", 
                                         "Unionized Ammonia" = "uia_ug_l"), 
                               selected=c("nitrate_mg_l", "nitrite_mg_l", "tkn_mg_l", "tn_mg_l", "tp_mg_l", "orthophosph_mg_l", "silica_mg_l", "ammonia_mg_l", "uia_ug_l"), 
                               multiple=T),
                   br(), 
             
             
             
             # Downloading tool
             downloadButton("nut_downloadData", label = "Data Download", style="background-color: #3c8dbc; border-color: #367fa9; color:white"),                   # Download options
             radioButtons("nut_file_type", label=NULL, inline = T,
                          choices = c(".csv", ".xlsx", ".shp")), 
             br()
      )
      ),
      
      # Box for outputs 
      column(8,box(width=12,
                   h4("Data Overview:")
      )
      )
    ),
    
    
    # Habitat ##########################################################################################################
    tabItem(
      tabName = "habitat",
      actionLink(inputId="habitat_desc",label="Habitat", style="font-size:160%"),
      br(),
      
      # Box for inputs 
      column(4,box(width=12, 
                   h4("Data Download:")
      )
      ),
      
      # Box for outputs 
      column(8,box(width=12,
                   h4("Data Overview:")
      )
      )
    ),
    
    # Temperature ####################################################################################
    
    tabItem(tabName="temp",
            actionLink(inputId="temp_desc",  label="Temperature Data", style="font-size:160%"),
            shiny_busy(), 
            br(), 

            
            # Box for inputs
            column(4,
                   box(width=12,
                       h4("Data Download:"), 
                       # Filter: years 
                       sliderInput(
                         inputId = "temp_dates",
                         label = "Choose years:",
                         min = wq_vars_date[1],
                         max = wq_vars_date[2],
                         value = wq_vars_date,
                         timeFormat= "%b %Y"
                       ),
                       # Filter: watersheds 
                       pickerInput(
                         inputId = "temp_ws_dwld",
                         label = "Choose Watersheds:",
                         choices = as.character(bio_vars_ws),
                         selected = as.character(bio_vars_ws),
                         options = list(`actions-box` = TRUE, size = 20),
                         multiple = T
                       ), 
                       radioButtons(inputId="temp_data_type", label="Choose Temperature Dataset:", 
                                    choices=c("Continuous Monitoring (April through Oct)"="temp_cont", 
                                              "Seasonal Monitoring (2 weeks, Spring vs. Fall)" = "temp_seas", 
                                              "Grab samples" = "temp_grab"), 
                                    selected="temp_grab"),
                       br(), 
                       downloadButton("downloadData_temp", label = "Data Download", style="background-color: #3c8dbc; border-color: #367fa9; color:white"), 
                       # Input: Choose file type -
                       radioButtons("file_type_temp", NULL, inline = T,
                                    choices = c(".csv", ".xlsx"), 
                                    selected=".csv")
                       )
                   ),
            
            #box for outputs
            column(8,
                   box(width=12, height="2000px",
                       h4("Overview of data:"),
                       
                       column(4, offset=8,
                         tags$head(tags$style(".leaflet-top {z-index:999!important;}")),
                         actionLink(inputId="map_temp_desc", label="Map Explanation", icon=NULL)
                       ), 
                       
                       
                       column(12,
                              column(2, offset=5),
                              leafletOutput("map_temp"),
                              br(), 
                              div(style="font-weight:bold; font-size:14px", "Plot of temperatures:"), 
                              br()
                              ), 
                       column(12,
                       plotOutput("temp_plot"), 
                       htmlOutput("temp_timeseries_message"), 
                       plotlyOutput("temp_timeseries"))
                      
                      
                   )
            )
                          
                   
            ),
    
    
    # pH #################################################################################################################################        
    
    
    tabItem(
      tabName = "ph",
      actionLink(inputId="ph_desc",  label="pH", style="font-size:160%"),
      br(),

        
        # Box for inputs
        column(4,  
               box(width=12,
                   h4("Data Download:")
                   )
               ),
        
        #box for map and plots 
        column(8,
               box(width=12, 
                   h4("Overview of data:")
                   )
               )
      ),
    
    
    # DO #################################################################################################################################        
    
    
    tabItem(
      tabName = "do",
      actionLink(inputId="do_desc",  label="Dissolved Oxygen", style="font-size:160%"),
      br(),

        
        # Box for inputs
        column(4,  
               box(width=12,
                   h4("Data Download:")
               )
        ),
        
        #box for map and plots 
        column(8,
               box(width=12, 
                   h4("Overview of data:")
               )
        )
      
    ),
    
    
    # Spe Cond #################################################################################################################################        
    
    
    tabItem(
      tabName = "sp_cond",
      actionLink(inputId="sp_cond_desc",  label="Specific Conductivity", style="font-size:160%"),
      br(),
      
        # Box for inputs
        column(4,  
               box(width=12,
                   h4("Data Download:")
               )
        ),
        
        #box for map and plots 
        column(8,
               box(width=12, 
                   h4("Overview of data:")
               )
        )
    ),
    
    
    
    # Chlorine #############################################################################################
    tabItem(
      tabName = "chlorine",
      actionLink(inputId="chlor_desc",label="Chlorine Data", style="font-size:160%"),
      br(),
      
      # Box for inputs 
        column(4,box(width=12, 
                     h4("Data Download:")
        )
        ),
        
        # Box for outputs 
        column(8,box(width=12,
                     h4("Overview of data:")
        )
        )
    ),
    
    # Pathogens ####################################################################################################
    
    
    tabItem(tabName = "pathogens",
            actionLink(inputId="patho_desc",label="Bacterial Indicators", style="font-size:160%"),
            br(),
        
                       
            # Box for inputs 
            column(4,
                   box(width=12, 
                       h4("Data Download:")
                       )
                   ),
            
            # Box for outputs 
            column(8,
                   box(width=12,
                         h4("Data Overview:")
                         )
                   )
            ),
    
    
    
    # Pesticide ##############################################################################################
    tabItem(tabName = "pesticide",
            actionLink(inputId="pesti_desc",label="Pesticides", style="font-size:160%"),
            br(),

            
            # Box for inputs 
            column(4,
                   box(width=12, 
                         h4("Data Download:")
                         )
                   ),

            # Box for outputs 
            column(8,
                   box(width=12,
                         h4("Data Overview:")
                         )
                   )
            ),
    
    
    
    
    # POC ##########################################################################################################
    tabItem(
      tabName = "poc",
      actionLink(inputId="poc_desc",label="Pollutants of concern", style="font-size:160%"),
      br(),
      
      # Box for inputs 
      column(4,box(width=12, 
                   h4("Data Download:")
                   )
             ),
      
      # Box for outputs 
      column(8,box(width=12,
                   h4("Data Overview:")
                   )
             )
      ),
    
    
    # Trash: Receiving Water ####################################################################################
    tabItem(tabName = "trash",
            actionLink(inputId="trash_desc",label="Trash in Receiving Waters", style="font-size:160%"),
            br(),
            h4("Receiving Water Monitoring... Preliminary Results... Targeted Sites only"),
            
            column(4,
                   box(width=12,
                       h4("Data Download:")
                       )
                   ),
            
            column(8,
                   box(width=12,
                       h4("Data Overview:")
                       )
                   )
            )
    )
  )
)
  
    






###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################

      