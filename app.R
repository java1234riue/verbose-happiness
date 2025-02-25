#############################
#         Packages         #
#############################
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)  # for theming (optional)

#############################
#       Data Import        #
#############################
base_url <- "https://raw.githubusercontent.com/java1234riue/verbose-happiness/main/"
years <- 2015:2023
file_names <- paste0("WHR_", years, ".csv")
urls <- paste0(base_url, file_names)

# Force consistent col types
read_whr_file <- function(url, year) {
  read_csv(
    file = url,
    col_types = cols(
      country = col_character(),
      region = col_character(),
      happiness_score = col_double(),
      gdp_per_capita = col_double(),
      social_support = col_double(),
      healthy_life_expectancy = col_double(),
      freedom_to_make_life_choices = col_double(),
      generosity = col_double(),
      perceptions_of_corruption = col_double()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(year = year)
}

all_data <- purrr::map2_dfr(urls, years, read_whr_file)

# Drop rows with missing critical columns
all_data <- all_data %>%
  drop_na(country, region, happiness_score)

# Numeric variables for X/Y axes
numeric_vars <- c(
  "happiness_score",
  "gdp_per_capita",
  "social_support",
  "healthy_life_expectancy",
  "freedom_to_make_life_choices",
  "generosity",
  "perceptions_of_corruption"
)

#############################
#         UI Layout        #
#############################
ui <- navbarPage(
  title = "World Happiness Analysis (2015–2023)",
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  id = "main_navbar",
  
  #---------------------------------------------------------------------------
  # 1) Data Exploration Tab
  #---------------------------------------------------------------------------
  tabPanel(
    title = "Data Exploration",
    sidebarLayout(
      sidebarPanel(
        h4("Filters and Options"),
        
        #--- Years Selection ---
        strong("Add Years"),
        selectInput(
          inputId = "year_input",
          label = NULL,
          choices = sort(unique(all_data$year)),
          selected = NULL,
          multiple = TRUE,
          selectize = TRUE
        ),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "year_remove",
              label = "Remove a Year",
              choices = NULL,     # will be updated server-side to match year_input
              selected = NULL,
              multiple = FALSE,
              selectize = TRUE
            )
          ),
          column(
            width = 4,
            br(),
            actionButton("remove_year", "Remove")
          )
        ),
        hr(),
        
        #--- Regions Selection ---
        strong("Add Regions"),
        selectInput(
          inputId = "region_input",
          label = NULL,
          choices = sort(unique(all_data$region)),
          selected = NULL,
          multiple = TRUE,
          selectize = TRUE
        ),
        fluidRow(
          column(
            width = 8,
            selectInput(
              inputId = "region_remove",
              label = "Remove a Region",
              choices = NULL,  # updated server-side
              selected = NULL,
              multiple = FALSE,
              selectize = TRUE
            )
          ),
          column(
            width = 4,
            br(),
            actionButton("remove_region", "Remove")
          )
        ),
        hr(),
        
        #--- X & Y Variables ---
        strong("X-axis Variable"),
        selectInput(
          inputId = "x_var",
          label = NULL,
          choices = numeric_vars,
          selected = "gdp_per_capita"
        ),
        
        strong("Y-axis Variable"),
        selectInput(
          inputId = "y_var",
          label = NULL,
          choices = numeric_vars,
          selected = "happiness_score"
        ),
        
        br(),
        p("Instructions:"),
        tags$ul(
          tags$li("Use 'Add Years' or 'Add Regions' to choose items."),
          tags$li("Select the item you want to remove in 'Remove a Year/Region' and click 'Remove'."),
          tags$li("Pick numeric variables for the X and Y axes below.")
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Scatter Plot",
            br(),
            plotlyOutput("scatter_plot"),
            br(),
            p("Hover over points to see details. Zoom or pan with the plotly toolbar.")
          ),
          tabPanel(
            title = "Filtered Table",
            br(),
            DTOutput("filtered_table")
          )
        )
      )
    )
  ),
  
  #---------------------------------------------------------------------------
  # 2) Discussion Tab
  #---------------------------------------------------------------------------
  tabPanel(
    title = "Discussion",
    fluidRow(
      column(
        width = 10, offset = 1,
        br(),
        h2("Discussion & Key Insights"),
        p("Below is a brief discussion of insights gleaned from the data, the design of this Shiny application, and the reactive graph structure."),
        h4("Interesting Facts & Unexpected Findings"),
        p("1. Some regions show consistently high happiness scores, correlating strongly with GDP per capita and social support. However, there are notable outliers in certain years where a country’s generosity is high despite lower GDP or higher perceived corruption."),
        p("2. An unexpected finding is the relative stability of happiness scores for some countries over time, suggesting that freedom in life choices can mitigate large economic fluctuations."),
        
        h4("Creation of the Interface"),
        p("The user interface provides separate sections for adding and removing years/regions explicitly, allowing flexible selection. All data is retrieved from the raw CSV files on GitHub, with consistent column types enforced."),
        
        h4("Reactive Graph Structure"),
        p("A single reactive expression filters the dataset based on the selected years and regions. Both the scatter plot and the data table depend on this reactive subset. This avoids duplication and ensures they stay in sync."),
        br()
      )
    )
  )
)

#############################
#       Server Logic       #
#############################
server <- function(input, output, session) {
  
  #--- Observers to update "remove" dropdowns based on current selections ---
  # This way, the "remove" dropdown only shows what's actually selected.
  
  observeEvent(input$year_input, {
    updateSelectInput(
      session,
      "year_remove",
      choices = input$year_input,
      selected = NULL
    )
  })
  
  observeEvent(input$region_input, {
    updateSelectInput(
      session,
      "region_remove",
      choices = input$region_input,
      selected = NULL
    )
  })
  
  #--- Remove selected year from "year_input" ---
  observeEvent(input$remove_year, {
    req(input$year_remove) # must have a year selected to remove
    old_sel <- input$year_input
    remove_this <- input$year_remove
    new_sel <- setdiff(old_sel, remove_this)  # remove from the old selection
    updateSelectInput(session, "year_input", selected = new_sel)
  })
  
  #--- Remove selected region from "region_input" ---
  observeEvent(input$remove_region, {
    req(input$region_remove)
    old_sel <- input$region_input
    remove_this <- input$region_remove
    new_sel <- setdiff(old_sel, remove_this)
    updateSelectInput(session, "region_input", selected = new_sel)
  })
  
  #--- Reactive expression to filter data based on selections ---
  filtered_data <- reactive({
    # If user selects no years or no regions, return empty
    if (length(input$year_input) == 0 || length(input$region_input) == 0) {
      return(tibble())
    }
    all_data %>%
      filter(
        year %in% input$year_input,
        region %in% input$region_input
      )
  })
  
  #--- Scatter Plot ---
  output$scatter_plot <- renderPlotly({
    req(input$x_var, input$y_var)
    df <- filtered_data()
    
    # If no data, show a placeholder
    if (nrow(df) == 0) {
      p <- ggplot() +
        annotate("text", x = 0, y = 0,
                 label = "No data selected. Please choose at least one Year and one Region.",
                 size = 5, color = "red") +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(df, aes_string(
      x = input$x_var,
      y = input$y_var,
      color = "region",
      label = "country"
    )) +
      geom_point(alpha = 0.7, size = 3) +
      labs(
        x = input$x_var,
        y = input$y_var,
        title = "World Happiness Report Scatter Plot",
        color = "Region"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = c("label", input$x_var, input$y_var, "color"))
  })
  
  #--- Filtered Table ---
  output$filtered_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No rows to display. Please select data."),
        options = list(dom = "t")  # only show the table body
      ))
    }
    datatable(
      df %>%
        select(year, country, region, all_of(numeric_vars)),
      options = list(pageLength = 10)
    )
  })
}

#############################
#      Shiny App Run       #
#############################
shinyApp(ui, server)