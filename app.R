#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

##### LIBRARIES #####
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(bslib)
library(tidyverse)
library(survey)
library(haven)
library(hrbrthemes)
library(gt)
library(gtforester)
library(gridExtra)
library(grid)
library(forcats)
library(broom)
library(broom.helpers)
library(ggpubr)
library(labelled)
library(targets)
library(arrow)
library(rlang)
library(scales)
library(plotly)
library(broom)
#####################

### HELPER FILES ###
source("R/utils.R")
####################

# Define UI
ui <- fluidPage(

    # Shiny Theme
    theme = shinytheme("flatly"),
    tags$head(
      tags$title("Hawaiʻi PRAMS Data Explorer"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    # Application title
    div(
      style = "text-align: center;",
      titlePanel(
        div(
          tags$img(src = "dqhs.png", height = "80px", style = "display:inline-block;vertical-align:middle;"),
          tags$span("Hawaiʻi PRAMS Data Explorer", style = "display:inline-block;vertical-align:middle; margin-left:10px; font-family: Georgia, serif; font-size: 40px;")
        )
      )
    ),
    
    # Navigation Bar
    navbarPage(
      "",
      id = "active_tab",
      
      ####### TAB 1: The About Tab #######
      
      tabPanel("About",
               fluidPage(
                 br(),
                 
                 # Welcome section
                 div(style = "max-width: 1100px; margin: 0 auto;",
                     wellPanel(
                       tagList(
                         div(style = "text-align: center;",
                             # Header with Hawaiian islands image
                             div(style = "display: inline-block; vertical-align: middle;",
                                 tags$img(src = "hawaiian-islands.png", height = "45px", style = "margin-right: 10px;")
                             ),
                             div(style = "display: inline-block; vertical-align: middle;",
                                 h2(class = "text-primary", "Welcome to the Hawaiʻi PRAMS Data Explorer")
                             )
                         ),
                         div(style = "text-align: left; margin-top: 15px;",
                             p("This interactive Shiny app allows you to explore trends in maternal health using data from the Hawaiʻi Pregnancy Risk Assessment Monitoring System (PRAMS)."),
                             p("Designed for researchers, clinicians, public health professionals, and others, this tool makes it easy to visualize and analyze key variables across time and demographic subgroups.")
                         )
                       )
                     )
                 ),
                 
                 # App Features section
                 div(style = "max-width: 1100px; margin: 0 auto;",
                     wellPanel(
                       tagList(
                         div(style = "text-align: center;",
                             h2(class = "text-primary", "What You Can Do:"),
                             p("Each section of this app offers a unique lens into maternal health trends. Below is a quick overview of what you can explore.")
                         ),
                         div(style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap; margin-top: 20px;",
                             
                             # Trend Analysis card
                             div(class = "card-style",
                                 style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px; width: 480px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); text-align: left;",
                                 
                                 div(style = "text-align: center;",
                                     h4(tags$b("📈 Trend Analysis"))
                                 ),
                                 
                                 p("Analyze how maternal health conditions (such as gestational diabetes, chronic hypertension, depression, etc.) change in frequency across years."),
                                 p("Stratify the analysis by different demographic groups, such as race, education level, and socioeconomic status.")
                            ),
                             
                             # Logistic Regression card
                            div(class = "card-style",
                                style = "background-color: #f8f9fa; border-radius: 10px; padding: 20px; width: 480px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); text-align: left;",
                                
                                div(style = "text-align: center;",
                                    h4(tags$b("📊 Logistic Regression Analysis"))
                                ),
                                
                                p("Evaluate the relationships between maternal health conditions and key predictors."),
                                p("Interpret odds ratios and confidence intervals using summary tables and interactive plots.")
                            ),
                         )
                       )
                     )
                 ),
                 
                 # Citation section
                 div(style = "max-width: 1100px; margin: 0 auto;",
                     wellPanel(
                       tagList(
                         div(style = "text-align: center;",
                             h2(class = "text-primary", "Cite This App"),
                             p("If you use this app in your research, please cite:")
                         ),
                         div(style = "text-align: left; margin-top: 10px;",
                             tags$blockquote(
                               "Brenner N., et al. (2025). Visualizing Maternal Health Trends in Hawai‘i Using PRAMS: An Interactive Shiny Application. Hawaiʻi Journal of Health & Social Welfare (in submission)."
                             )
                         )
                       )
                     )
                 )
               )
      ),

      ####### TAB 2: The Trend Analysis Tab #######
      tabPanel(
        "Trend Analysis",
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            wellPanel(
              
              # Analysis Type User Selection
              div(
                tags$label("Select Analysis Type:",
                           `for` = "trend_analysis_type"),
                icon("question-circle", id = "trend_analysis_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                radioButtons("trend_analysis_type",
                             label = NULL,
                             choices = c("One Condition", "Co-occurence"),
                             selected = "One Condition",
                             inline = TRUE
                ),
                bsPopover("trend_analysis_type_info",
                          title = "",
                          content = "Choose whether you would like to analyze the proportions of one variable over time (One Condition), or the occurrence of two or three variables simultaneously over time (Co-occurence)",
                          placement = "right", trigger = "click",
                          options = list(container = "body"))
                
              ),
              
              # Phase Range User Selection
              div(
                tags$label("Select range of Phases:",
                           `for` = "phase_type"),
                icon("question-circle", id = "phase_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("phase_type",
                            label = NULL,
                            choices = c("Phase 8", "Phases 6, 7, and 8", "All Phases"),
                            selected = "Phase 8"
                ),
                bsPopover("phase_type_info",
                          title = "",
                          content = "Choose the range of PRAMS phases you would like to explore. Phase 8 contains years 2016-2023, Phases 6-8 contain years 2009-2023, and All Phases contains years 2000-2023.",
                          placement = "right", trigger = "click",
                          options = list(container = "body"))
                
              ),
              
              # Demographic Grouping Variable User Selection
              div(
                tags$label("Select grouping demographic variable:",
                           `for` = "demographic_var"),
                icon("question-circle", id = "demographic_var_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("demographic_var",
                            label = NULL,
                            choices = c("Race", "Education", "SES"),
                            selected = "Race"
                ),
                bsPopover("demographic_var_info",
                          title = "",
                          content = "Choose the demographic variable you would like to stratify the analysis by.",
                          placement = "right", trigger = "click",
                          options = list(container = "body"))
              ),
              
              ## Variable User Selection ##
              # “One Condition” mode
              conditionalPanel(
                condition = "input.trend_analysis_type == 'One Condition'",
                fluidRow(
                  column(
                    width = 4,
                    
                    # Phase 8 Variables
                    conditionalPanel(
                      condition = "input.phase_type == 'Phase 8'",
                      div(
                        tags$label("Select variable:", `for` = "var1_single"),
                        icon("question-circle", id = "var1_info", 
                             style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                        pickerInput(
                          inputId = "var1_single",
                          label   = NULL,
                          choices = phase8_variable_choices,
                          selected = "Pregestational Diabetes",
                          options = list(
                            `live-search` = TRUE,
                            `title`       = "Search or scroll..."
                          )
                        ),
                        bsPopover("var1_info", title = "",
                                  content = "For Phase 8, choose the health condition or behavior you want to track over time.",
                                  placement = "right", trigger = "click",
                                  options = list(container = "body"))
                      )
                    ),
                    
                    # Phases 6–8 Variables
                    conditionalPanel(
                      condition = "input.phase_type == 'Phases 6, 7, and 8'",
                      div(
                        tags$label("Select variable:", `for` = "var1_single"),
                        icon("question-circle", id = "var1_678_info", 
                             style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                        pickerInput(
                          inputId = "var1_single",
                          label   = NULL,
                          choices = phase6_7_8_variable_choices,
                          selected= "Pregestational Diabetes",
                          options = list(
                            `live-search` = TRUE,
                            `title`       = "Search or scroll..."
                          )
                        ),
                        bsPopover("var1_678_info", title = "",
                                  content = "For Phases 6–8 (2009–2022), choose the health condition or behavior you want to track over time.",
                                  placement = "right", trigger = "click",
                                  options = list(container = "body"))
                      )
                    ),
                    
                    # All Phases Variables
                    conditionalPanel(
                      condition = "input.phase_type == 'All Phases'",
                      div(
                        tags$label("Select variable:", `for` = "var1_single"),
                        icon("question-circle", id = "var1_all_info", 
                             style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                        pickerInput(
                          inputId = "var1_single",
                          label   = NULL,
                          choices = all_phases_variable_choices,
                          selected= "Pregestational Diabetes",
                          options = list(
                            `live-search` = TRUE,
                            `title`       = "Search or scroll..."
                          )
                        ),
                        bsPopover("var1_all_info", title = "",
                                  content = "For all Phases (2000–2022), choose the health condition or behavior you want to track over time.",
                                  placement = "right", trigger = "click",
                                  options = list(container = "body"))
                      )
                    )
                    
                  )
                )
              ),
              
              # “Co-occurence” mode
              conditionalPanel(
                condition = "input.trend_analysis_type == 'Co-occurence'",
                fluidRow(
                  column(
                    width = 4,
                    
                    # First variable
                    conditionalPanel(
                      condition = "input.phase_type == 'Phase 8'",
                      div(
                        tags$label("Select first variable:", `for` = "var1"),
                        icon("question-circle", id = "var1_sub_info", 
                             style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                        pickerInput(
                          inputId = "var1",
                          label   = NULL,
                          choices = phase8_variable_choices,
                          selected= "Pregestational Diabetes",
                          options = list(`live-search` = TRUE, `title` = "Search or scroll...")
                        ),
                        bsPopover("var1_sub_info", title = "",
                                  content = "For Phase 8, choose the first health condition or behavior to track.",
                                  placement = "right", trigger = "click",
                                  options = list(container = "body"))
                      )
                    ),
                    conditionalPanel(
                      condition = "input.phase_type == 'Phases 6, 7, and 8'",
                      div(
                        tags$label("Select first variable:", `for` = "var1"),
                        icon("question-circle", id = "var1_678_sub_info", 
                             style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                        pickerInput(
                          inputId = "var1",
                          label   = NULL,
                          choices = phase6_7_8_variable_choices,
                          selected= "Pregestational Diabetes",
                          options = list(`live-search` = TRUE, `title` = "Search or scroll...")
                        ),
                        bsPopover("var1_678_sub_info", title = "",
                                  content = "For Phases 6–8, choose the first health condition or behavior to track.",
                                  placement = "right", trigger = "click",
                                  options = list(container = "body"))
                      )
                    ),
                    conditionalPanel(
                      condition = "input.phase_type == 'All Phases'",
                      div(
                        tags$label("Select first variable:", `for` = "var1"),
                        icon("question-circle", id = "var1_all_sub_info", 
                             style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                        pickerInput(
                          inputId = "var1",
                          label   = NULL,
                          choices = all_phases_variable_choices,
                          selected= "Pregestational Diabetes",
                          options = list(`live-search` = TRUE, `title` = "Search or scroll...")
                        ),
                        bsPopover("var1_all_sub_info", title = "",
                                  content = "For all Phases, choose the first health condition or behavior to track.",
                                  placement = "right", trigger = "click",
                                  options = list(container = "body"))
                      )
                    )
                  ),
                  
                  # Second variable
                  column(
                    width = 4,
                    conditionalPanel(
                      condition = "input.phase_type == 'Phase 8'",
                      pickerInput("var2", label = "Select second variable:",
                                  choices = phase8_variable_choices,
                                  selected = "Chronic Hypertension",
                                  options = list(`live-search`=TRUE, `title`="Search or scroll...")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.phase_type == 'Phases 6, 7, and 8'",
                      pickerInput("var2", label = "Select second variable:",
                                  choices = phase6_7_8_variable_choices,
                                  selected = "Chronic Hypertension",
                                  options = list(`live-search`=TRUE, `title`="Search or scroll...")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.phase_type == 'All Phases'",
                      pickerInput("var2", label = "Select second variable:",
                                  choices = all_phases_variable_choices,
                                  selected = "Chronic Hypertension",
                                  options = list(`live-search`=TRUE, `title`="Search or scroll...")
                      )
                    )
                  ),
                  
                  # Third variable
                  column(
                    width = 4,
                    conditionalPanel(
                      condition = "input.phase_type == 'Phase 8'",
                      pickerInput("var3", label = "Select third variable:",
                                  choices = c("None"="None", phase8_variable_choices),
                                  selected = "None",
                                  options = list(`live-search`=TRUE, `title`="Search or scroll...")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.phase_type == 'Phases 6, 7, and 8'",
                      pickerInput("var3", label = "Select third variable:",
                                  choices = c("None"="None", phase6_7_8_variable_choices),
                                  selected = "None",
                                  options = list(`live-search`=TRUE, `title`="Search or scroll...")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.phase_type == 'All Phases'",
                      pickerInput("var3", label = "Select third variable:",
                                  choices = c("None"="None", all_phases_variable_choices),
                                  selected = "None",
                                  options = list(`live-search`=TRUE, `title`="Search or scroll...")
                      )
                    )
                  )
                )
              ),
              
              # Action Button
              actionButton(
                "plot_button",
                "Generate Plot",
                icon = icon("play"),
                class = "btn-primary"
              ),
            ),
          )
        ),
        
        # Download Feature
        fluidRow(
          column(
            width = 8,
            offset = 2,
            wellPanel(
              
              div(
                style = "display: flex; align-items: center; justify-content: flex-start;",
                
                radioButtons("file_type",
                             "Choose File Type:",
                             choices = c("HTML" = "html", "PNG" = "png", "PDF" = "pdf"),
                             selected = "html"),
                
                downloadButton("downloadPlot",
                               "Download Plot",
                               icon = icon("download"),
                               class = "btn-primary",
                               style = "margin-left: 20px;")
              )
            )
          )
        ),
        
        # Plot Output
        fluidRow(
          column(
            width = 10,
            offset = 1,
            plotlyOutput("linePlot", width = "100%", height = "600px") |> withSpinner(type = 1, color = "#1d0ed5", hide.ui = FALSE)
          )
        ),
      ),
      
      ####### TAB 3: The Logistic Regression Tab #######
      tabPanel(
        "Logistic Regression Analysis",
        
        fluidRow(
          column(
            width = 8,
            offset = 2,
            wellPanel(
              
              # Plot Type User Selection
              div(
                tags$label("Select plot type:",
                           `for` = "plot_type"),
                icon("question-circle", id = "plot_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                  selectInput("plot_type",
                              label = NULL,
                              choices = c("Table", "Forest"),
                              selected = "Table"
                ),
                bsPopover("plot_type_info", title = "",
                          content = "Choose the type of plot you want to generate.",
                          placement = "right", trigger = "click",
                          options = list(container = "body"))
              ),
              
              # Phase Range User Selection
              div(
                tags$label("Select range of Phases:",
                           `for` = "forest_phase_type"),
                icon("question-circle", id = "forest_phase_type_info", style = "margin-left: 5px; color: #007bff; cursor: pointer;"),
                selectInput("forest_phase_type",
                            label = NULL,
                            choices = c("Phase 8", "Phases 6, 7, and 8", "All Phases"),
                            selected = "Phase 8"
                ),
                bsPopover("forest_phase_type_info",
                          title = "",
                          content = "Choose the range of PRAMS phases you would like to explore. Phase 8 contains years 2016-2023, Phases 6-8 contain years 2009-2023, and All Phases contains years 2000-2023.",
                          placement = "right", trigger = "click",
                          options = list(container = "body"))
                
              ),
              
              ## Variable User Selection ##
              # Phase 8 Variables
              conditionalPanel(
                condition = "input.forest_phase_type == 'Phase 8'",
                div(
                  tags$label("Select variable:", `for` = "forest_var"),
                  icon("question-circle", id = "forest_var_info",
                       style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                  pickerInput(
                    inputId = "forest_var",
                    label   = NULL,
                    choices = phase8_variable_choices,
                    selected= "Pregestational Diabetes",
                    options = list(
                      `live-search` = TRUE,
                      `title`       = "Search or scroll..."
                    )
                  ),
                  bsPopover("forest_var_info",
                            title = "",
                            content = "For Phase 8, choose the health condition or behavior you want to analyze.",
                            placement = "right", trigger = "click",
                            options = list(container = "body"))
                )
              ),
              
              # Phases 6–8 Variables
              conditionalPanel(
                condition = "input.forest_phase_type == 'Phases 6, 7, and 8'",
                div(
                  tags$label("Select variable:", `for` = "forest_var"),
                  icon("question-circle", id = "forest_var_678_info",
                       style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                  pickerInput(
                    inputId = "forest_var",
                    label   = NULL,
                    choices = phase6_7_8_variable_choices,
                    selected= "Pregestational Diabetes",
                    options = list(
                      `live-search` = TRUE,
                      `title`       = "Search or scroll..."
                    )
                  ),
                  bsPopover("forest_var_678_info",
                            title = "",
                            content = "For Phases 6–8, choose the health condition or behavior you want to analyze.",
                            placement = "right", trigger = "click",
                            options = list(container = "body"))
                )
              ),
              
              # All Phases Variabels
              conditionalPanel(
                condition = "input.forest_phase_type == 'All Phases'",
                div(
                  tags$label("Select variable:", `for` = "forest_var"),
                  icon("question-circle", id = "forest_var_all_info",
                       style = "margin-left:5px;color:#007bff;cursor:pointer;"),
                  pickerInput(
                    inputId = "forest_var",
                    label   = NULL,
                    choices = all_phases_variable_choices,
                    selected= "Pregestational Diabetes",
                    options = list(
                      `live-search` = TRUE,
                      `title`       = "Search or scroll..."
                    )
                  ),
                  bsPopover("forest_var_all_info",
                            title = "",
                            content = "For all Phases, choose the health condition or behavior you want to analyze.",
                            placement = "right", trigger = "click",
                            options = list(container = "body"))
                )
              ),

              # Action Button
              actionButton(
                "forest_plot_button",
                "Generate Plot",
                icon = icon("play"),
                class = "btn-primary"
              )
            )
          )
        ),
        
        # Download Feature
        fluidRow(
          column(
            width = 8,
            offset = 2,
            wellPanel(
              
              div(
                style = "display: flex; align-items: center; justify-content: flex-start;",
                
                radioButtons("forest_file_type",
                             "Choose File Type:",
                             choices = c("HTML" = "html", "PNG" = "png", "PDF" = "pdf"),
                             selected = "html"),
                
                downloadButton("downloadForest",
                               "Download Plot",
                               icon = icon("download"),
                               class = "btn-primary",
                               style = "margin-left: 20px;")
              )
            )
          )
        ),
        
        # Plot Output
        fluidRow(
          column(
            width = 10,
            offset = 1,
            uiOutput("forestPlotUI") |> 
              withSpinner(type = 1, color = "#1d0ed5", hide.ui = FALSE)
          )
        )
        
      )
    ),
    
    tags$script(HTML("
  $(document).ready(function() {
    $(document).on('click', function (e) {
      const target = $(e.target);
      const isHelpIcon = target.closest('i[id$=\"_info\"]').length > 0;
      const isInsidePopover = target.closest('.popover').length > 0;

      if (!isHelpIcon && !isInsidePopover) {
        $('i[id$=\"_info\"]').popover('hide');
      }
    });
  });
"))
    
  )


    

# Server
server <- function(input, output, session) {
  
  # Parquet Data
  phase8_parquet <- read_parquet("processed_data/phase8.parquet")
  phases6_7_8_parquet <- read_parquet("processed_data/phases6_7_8.parquet")
  all_phases_parquet <- read_parquet("processed_data/all_phases.parquet")
  
  #################### TAB 2 ####################
  
  ### ACTION BUTTON ###
  reactive_plot_data <- eventReactive(input$plot_button, {
    
    selected_data <- switch(input$phase_type,
                            "Phases 6, 7, and 8" = phases6_7_8_parquet,
                            "Phase 8" = phase8_parquet,
                            "All Phases" = all_phases_parquet)
    
    list(
      trend_analysis_type = input$trend_analysis_type,
      phase_type = input$phase_type,
      var1_single = input$var1_single,
      var1 = input$var1,
      var2 = input$var2,
      var3 = input$var3,
      demographic_var = input$demographic_var,
      selected_data = selected_data
    )
    
  })
  #####################
  
  
  ### Observer for Detailed Race Option ###
  observeEvent(input$phase_type, {
    if (input$phase_type == "Phase 8") {
      updateSelectInput(session, "demographic_var",
                        choices  = c("Race", "Detailed Race" = "Detailed_Race", "Education", "SES"),
                        selected = "Detailed_Race")
    } else {
      updateSelectInput(session, "demographic_var",
                        choices  = c("Race", "Education", "SES"),
                        selected = "Race")
    }
  })
  
  # In server.R
  observeEvent(input$phase_type, {
    # Reset ALL variable inputs to defaults when phase changes
    updatePickerInput(session, "var1_single", selected = "Pregestational Diabetes")
    updatePickerInput(session, "var1", selected = "Pregestational Diabetes")
    updatePickerInput(session, "var2", selected = "Chronic Hypertension")
    updatePickerInput(session, "var3", selected = "None")
  })
  
  #####################
  
  ### Tab 1 Plot ###
    linePlot <- reactive({
      
      # Default Plot
      if (is.null(input$plot_button) || input$plot_button == 0) {
        data_source <- phase8_parquet  # Load data for default plot
        default_data <- data_source |> filter(!is.na(Race))  # Filter out NA values for Race
        return(default_plot_univariate_trend_line(default_data))
      }
      
      # Load Reactive Data
      plot_data <- reactive_plot_data()
      
      trend_analysis_type <- plot_data$trend_analysis_type
      phase_type <- plot_data$phase_type
      var1_single <- plot_data$var1_single
      var1 <- plot_data$var1
      var2 <- plot_data$var2
      var3 <- plot_data$var3
      demographic_var <- plot_data$demographic_var
      selected_data <- plot_data$selected_data
      
      multi_level_vars <- c("Education", "First_PNC_Visit_Wks_Mnths", "Postpartum_MH_Depressed_Since_Birth")
        
      # Reactive Plot
      if (trend_analysis_type == "One Condition") {
        if (var1_single %in% multi_level_vars) {
          univariate_trend_line_multiple_levels(selected_data, var1_single)
        } else {
          univariate_trend_line(selected_data, var1_single, demographic_var)
        }
      } else {
        if (var3 != "None") {
          multivariate_trend_line_3_vars(selected_data, var1, var2, var3)
        } else if (var1 %in% multi_level_vars || var2 %in% multi_level_vars) {
          multivariate_trend_line_multiple_level_vars(selected_data, var1, var2)
        } else {
          multivariate_trend_line(selected_data, var1, var2)
        }
      }
      
    })
  
    output$linePlot <- renderPlotly({
      plotly::ggplotly(linePlot(), tooltip = "text") |> 
        layout(
          margin = list(t = 120, r = 20, b = 120, l = 20),
          title = list(x = 0.5,
                       y = 0.925),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = 1.125
          )
        )
      })
    
    # Download Plot Handler
    output$downloadPlot <- downloadHandler(
      filename = function() {
        if (input$file_type == "html") {
          "plot_UnivariateAnalysis.html"
        } else {
          paste0("plot_UnivariateAnalysis.", input$file_type)
        }
      },
      content = function(file) {
        if (input$file_type == "html") {
          htmlwidgets::saveWidget(as_widget(
            ggplotly(linePlot(), tooltip = "text") |> 
              layout(
                margin = list(t = 160, r = 20, b = 120, l = 20),
                title = list(x = 0.5,
                             y = 0.95),
                legend = list(
                  orientation = "h",
                  x = 0.5,
                  xanchor = "center",
                  y = 1.125
                )
              )), 
            file, selfcontained = TRUE)
        } else if (input$file_type %in% c("png", "pdf")) {
          ggsave(file, plot = linePlot(), device = input$file_type, width = 12, height = 6, dpi = 300)
        } else {
          stop("Error: Unsupported file type for download.")
        }
      }
    )
  
  #################### TAB 3 ####################
  
  ### ACTION BUTTON ###
  reactive_forest_data <- eventReactive(input$forest_plot_button, {
    
    selected_data <- switch(input$forest_phase_type,
                            "Phases 6, 7, and 8" = phases6_7_8_parquet,
                            "Phase 8" = phase8_parquet,
                            "All Phases" = all_phases_parquet)
    
    list(
      forest_phase_type = input$forest_phase_type,
      forest_var = input$forest_var,
      plot_type = input$plot_type,
      selected_data = selected_data
    )
    
  }, ignoreNULL = FALSE)
  #####################
  
  
  #################### Tab 3 Plot ####################
    output$forestPlotUI <- renderUI({
      if (is.null(input$forest_plot_button) || input$forest_plot_button == 0) {
        # Show the default interactive plot
        gt_output("forestTable")
      } else {
        # Once the button has been clicked, use reactive_forest_data
        forest_data <- reactive_forest_data()
        
        req(forest_data)
        req(forest_data$plot_type)
        
        if (forest_data$plot_type == "Table") {
          gt_output("forestTable")
        } else {
          plotlyOutput("forestPlotly", width = "100%", height = "600px")
        }
      }
    })
    
    # Table
    forestTableObj <- reactive({
      
      if (is.null(input$forest_plot_button) || input$forest_plot_button == 0) {
        data_source <- phase8_parquet  # Load data for default plot
        default_data <- data_source |> filter(!is.na(Race))
        return(default_forest_table(default_data))
      }
      
      forest_data <- reactive_forest_data()
      
      forest_phase_type <- forest_data$forest_phase_type
      forest_var     <- forest_data$forest_var
      selected_data  <- forest_data$selected_data
      
      if (forest_phase_type == "Phase 8") {
        forest_table_detailed(selected_data, forest_var)
      } else {
        forest_table(selected_data, forest_var)
      }
      
    })
    
    output$forestTable <- render_gt({
      
      forestTableObj()
      
    })

    # Forest Plot
    forestPlotly <- reactive({
      
      if (is.null(input$forest_plot_button) || input$forest_plot_button == 0) {
        data_source <- phase8_parquet  # Load data for default plot
        default_data <- data_source |> filter(!is.na(Race))
        return(default_forest_table(default_data))
      }
      
      # Load Reactive Data
      forest_data <- reactive_forest_data()
      
      forest_phase_type <- forest_data$forest_phase_type
      forest_var     <- forest_data$forest_var
      selected_data  <- forest_data$selected_data
      
      if (forest_phase_type == "Phase 8") {
        forest_plot_detailed(selected_data, forest_var)
      } else {
        forest_plot(selected_data, forest_var)
      }

    })
    
    output$forestPlotly <- renderPlotly({
      
      plotly::ggplotly(forestPlotly(), tooltip = "text") |> 
        layout(
          margin = list(b = 120)
        )
      
    })
    
    # Download Plot Handler
    output$downloadForest <- downloadHandler(
      filename = function() {
        ext <- input$forest_file_type
        paste0("forest_analysis.", ext)
      },
      content = function(file) {
        
        ext <- input$forest_file_type
        
        if (ext == "html") {
          if (input$plot_type == "Forest") {
            # interactive forest → HTML
            htmlwidgets::saveWidget(
              as_widget(ggplotly(forestPlotly(), tooltip = "text")),
              file,
              selfcontained = TRUE
            )
          } else {
            # gt table → HTML
            gtsave(forestTableObj(), file)
          }
          
        } else if (ext %in% c("png","pdf")) {
          if (input$plot_type == "Forest") {
            # static forest plot → PNG/PDF
            ggsave(file, plot = forestPlotly(), device = ext,
                   width = 12, height = 6, dpi = 300)
          } else {
            # static table → PNG/PDF
            gtsave(forestTableObj(), file)
          }
        } else {
          stop("Unsupported file type")
        }
      }
    )
  
}

# Run the application!
shinyApp(ui = ui, server = server)
