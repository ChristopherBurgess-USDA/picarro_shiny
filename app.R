## Setup
library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(scales)

source("picarro_parse_script.R")

graph_format = function(g_data, y_var){
  ## The function graphs takes the filtered data and a y variable to plot
  ## Paramters: data (tibble) and y variable (string)
  ## Returns: ggplot
  ggplot(
    data = g_data,
    aes(x = valve, y = .data[[y_var]], color = file_name, shape = quality)
  ) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = g_data$valve, labels = g_data$valve)+
    theme_bw(16) +
    labs(x = "Valve Number") %>%
    return()
}

# User interface
ui <- fluidPage( # Application title
  
  sidebarLayout(
    sidebarPanel(
      h2("Upload Data"),
      helpText("Choose all picarro files you wish to parse. Follow the below directions to ensure app runs smoothly:",
               br(),
               tags$li("All files are DAT files (.dat)"),
               tags$li("Please select all file you wish to parse."),
               tags$b("You can select multiple files when uploading.")
      ),
      fileInput(
        inputId = "upload",
        label = "Upload",
        multiple = T,
        placeholder = "Browse",
        accept = c(".dat", ".txt")
      ),
      h2("Explore Data"),
      helpText("Choose which files you wish to explore.",
               br(),
               tags$b("Note:"),
               " you can click on the scatter plot's points to peek at the data."
      ),
      checkboxGroupInput(
        inputId = "f_keep",
        label = "Which Files:",
        choices = NULL,
        selected = NULL
      ),
      actionButton(
        inputId = "click",
        label = "Filter Data",
        width = "100%"
      ),
      h2("Download Data"),
      helpText("Download all uploaded data as csv (comma seperated) files."),
      downloadButton(
        outputId = "download",
        label = "Download",
        width = "100%"
      )
    ),
    
    
    ### WHat if you made all the click values the  same then filtered that way!
    mainPanel(
      tabsetPanel(
        tabPanel(
          HTML(paste0("Total CO", tags$sub(2))),
          plotOutput(
            outputId = "co_scatter",
            click = "co_click"
          ),
          plotOutput("co_time"),
          verbatimTextOutput(outputId = "co_info")
          
        ),
        tabPanel(
          HTML(paste0("&delta;", tags$sup(13), "C")),
          plotOutput(
            outputId = "delta_scatter",
            click = "delta_click"
          ),
          plotOutput("delta_time"),
          verbatimTextOutput(outputId = "delta_info")
          
        ),
        tabPanel(
          "Outlet Pressure",
          plotOutput(
            outputId = "pressure_scatter",
            click = "pressure_click"
          ),
          plotOutput("pressure_time"),
          verbatimTextOutput(outputId = "pressure_info")
          
        ),
        tabPanel("Table", DTOutput("tab"))
      )
      
    ) ## Main Panel
  ) ## sidebar layout
) ## fuild page

# Server function
server <- function(input, output, session) {
  
  data = reactive({
    ## This code makes sure it was a .dat or a .txt file before sending it to the file parsing script
    req(input$upload)
    ext = tools::file_ext(input$upload$name)
    validate(need(
      ext %in% c("dat", "txt"),
      "Please upload raw Picarro Data (tab-delimateted) as .dat or .txt"
    ))
    
    as_tibble(input$upload) %>%
      select(name, datapath) %>%
      picarro_file_parser() %>%
      return()
  })
  
  observe({
    ## The observe is like ta reactive accept it doesn't return anything
    ## Since we do not know how many or which files the user will be uploading the observe
    ## is constantly updating the multiple choice list with the name of the files.
    f_list = data()$file_name
    
    if(is.null(f_list))
      f_list = character(0)
    else
      f_list = levels(f_list)
    updateCheckboxGroupInput(
      session,
      inputId = "f_keep",
      choices = f_list,
      selected = f_list
    )
  })
  
  f_data = eventReactive(
    ## This reactive function activates the file name filtering and data graphing of each of the plots
    input$click,
    {
      filter(data(), file_name %in% input$f_keep)
    }
  )
  
  ## A little copy and paste to make all plots and info for the plots when points are clicked.
  output$co_scatter = renderPlot({
    graph_format(f_data(), "co2_total") +
      labs(
        y = bquote("Total" ~ CO[2]~ "Concentration (ppm)")
      )
  })
  
  co_data= eventReactive(
    input$co_click,
    {
      nearPoints(f_data(), input$co_click, xvar = "valve", yvar = "co2_total")
    }
  )
  
  delta_data = eventReactive(
    input$delta_click,
    {
      nearPoints(f_data(), input$delta_click, xvar = "valve", yvar = "delta_raw")
    }
  )
  
  
  pressure_data = eventReactive(
    input$pressure_click,
    {
      nearPoints(f_data(), input$pressure_click, xvar = "valve", yvar = "pressure")
    }
  )
  
  output$co_info = renderPrint({
    co_data() %>%
      select(-data) %>%
      print.data.frame()
  })
  
  output$co_time = renderPlot({
    co_data()$data[[1]] %>%
      mutate(co_total = co2_12 + co2_13) %>%
      ggplot(aes(x = date_time, y = co_total)) +
      geom_point() +
      scale_x_time(labels = time_format("%H:%M:%S")) +
      labs(
        y = bquote("Total" ~ CO[2]~ "Concentration (ppm)"),
        x = "Time",
        title = "Raw Valve Readings"
      ) +
      theme_bw(16)
  })
  
  
  output$delta_scatter = renderPlot({
    graph_format(f_data(), "delta_raw") +
      labs(
        y = bquote(delta^{13} * "C")
      )
  })
  
  output$delta_info = renderPrint({
    delta_data() %>%
      select(-data) %>%
      print.data.frame()
  })
  
  output$delta_time = renderPlot({
    delta_data()$data[[1]] %>%
      ggplot(aes(x = date_time, y = delta_raw)) +
      geom_point() +
      scale_x_time(labels = time_format("%H:%M:%S")) +
      labs(
        y = bquote(delta^{13} * "C"),
        x = "Time",
        title = "Raw Valve Readings"
      ) +
      theme_bw(16)
  })
  
  output$pressure_scatter = renderPlot({
    graph_format(f_data(), "pressure") +
      labs(y = "Outlet Pressure")
  })
  output$pressure_info = renderPrint({
    pressure_data() %>%
      select(-data) %>%
      print.data.frame()
  })
  
  output$pressure_time = renderPlot({
    pressure_data()$data[[1]] %>%
      ggplot(aes(x = date_time, y = pressure)) +
      geom_point() +
      scale_x_time(labels = time_format("%H:%M:%S")) +
      labs(
        y = "Outlet Pressure",
        x = "Time",
        title = "Raw Valve Readings"
      ) +
      theme_bw(16)
  })
  
  
  ## Creates table of filtered data
  output$tab = renderDT({
    f_data() %>%
      select(-data) %>%
      mutate_if(is.numeric, ~round(., digits = 3)) %>%
      as_data_frame()
  })
  
  ## Allows the user to download data.
  output$download = downloadHandler(
    filename = "parsed_picarro_data.csv",
    content = function(file){
      select(data(), -data) %>%
        write_csv(file)
    }
  )
  
}
# Create app object
shinyApp(ui = ui, server = server)