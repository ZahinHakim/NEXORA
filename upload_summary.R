library(shiny)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. UI FUNCTION
# ------------------------------------------------------------------------------
upload_summary_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("📁 Data Upload"),
        fileInput(
          ns("file_upload"),
          "Choose CSV or Excel File:",
          accept = c(".csv", ".xlsx")
        ),
        checkboxInput(ns("clean_na"), "🧹 Auto-remove missing values (NA rows)", value = FALSE),
        downloadButton("download_report", "📄 Export Summary Report", class = "btn-success w-100 mt-2"),
        hr(),
        h4("ℹ️ Dataset Overview"),
        verbatimTextOutput(ns("meta_rows_cols")),
        br(),
        verbatimTextOutput(ns("total_missing"))
      ),
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel(
            "Preview Data",
            br(),
            DTOutput(ns("preview_table"))
          ),
          tabPanel(
            "Column Data Types",
            br(),
            DTOutput(ns("meta_types_table"))
          ),
          tabPanel(
            "Missing Values Breakdown",
            br(),
            DTOutput(ns("missing_by_column"))
          ),
          tabPanel(
            "Quantitative Summary Metrics",
            br(),
            DTOutput(ns("numeric_stats_table"))
          ),
          tabPanel(
            "Qualitative Frequency Tables",
            br(),
            uiOutput(ns("cat_var_select_ui")),
            br(),
            DTOutput(ns("freq_table"))
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# 2. SERVER FUNCTION
# ------------------------------------------------------------------------------
upload_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Reactive Data Loader
    uploaded_data <- reactive({
      req_file <- input$file_upload
      req(req_file)
      
      ext <- tolower(tools::file_ext(req_file$name))
      df_raw <- switch(
        ext,
        csv = read_csv(req_file$datapath, show_col_types = FALSE),
        xlsx = read_excel(req_file$datapath),
        stop("Invalid file format. Please upload a CSV or Excel file.")
      )
      
      df <- as.data.frame(df_raw, stringsAsFactors = FALSE)
      
      if (isTRUE(input$clean_na)) {
        df <- na.omit(df)
        df <- as.data.frame(df)
      }
      
      return(df)
    })
    
    # 2. Outputs
    output$preview_table <- renderDT({
      df <- uploaded_data()
      req(df)
      req(is.data.frame(df))
      req(ncol(df) > 0)
      
      datatable(
        df, 
        options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE), 
        rownames = FALSE
      )
    })
    
    observeEvent(input$file_upload, {
      showNotification("✅ Dataset successfully uploaded and parsed!", type = "message", duration = 3)
    })
    
    observeEvent(input$clean_na, {
      if (input$clean_na) {
        showNotification("🧹 Incomplete rows (NA values) automatically removed.", type = "warning", duration = 3)
      }
    })
    
    output$meta_rows_cols <- renderText({
      df <- uploaded_data()
      req(df)
      paste0("Total Rows: ", nrow(df), "\nTotal Columns: ", ncol(df))
    })
    
    output$meta_types_table <- renderDT({
      df <- uploaded_data()
      req(df)
      types_df <- data.frame(
        `Column Name` = colnames(df),
        `Data Type` = sapply(df, class),
        check.names = FALSE
      )
      datatable(types_df, options = list(paging = FALSE, dom = 't'), rownames = FALSE)
    })
    
    output$total_missing <- renderText({
      df <- uploaded_data()
      req(df)
      paste0("Total Missing Values in Dataset: ", sum(is.na(df)))
    })
    
    output$missing_by_column <- renderDT({
      df <- uploaded_data()
      req(df)
      missing_df <- data.frame(
        `Column Name` = colnames(df),
        `Missing Count` = colSums(is.na(df)),
        `Missing Percentage` = paste0(round((colSums(is.na(df)) / nrow(df)) * 100, 2), "%"),
        check.names = FALSE
      )
      datatable(missing_df, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    output$numeric_stats_table <- renderDT({
      df <- uploaded_data()
      req(df)
      numeric_df <- df %>% select(where(is.numeric))
      
      if (ncol(numeric_df) == 0) {
        return(datatable(data.frame(Message = "No numeric variables found.")))
      }
      
      stats <- numeric_df %>%
        summarise(across(
          everything(),
          list(
            Mean = ~ mean(.x, na.rm = TRUE),
            Median = ~ median(.x, na.rm = TRUE),
            Min = ~ min(.x, na.rm = TRUE),
            Max = ~ max(.x, na.rm = TRUE),
            SD = ~ sd(.x, na.rm = TRUE),
            Variance = ~ var(.x, na.rm = TRUE),
            Q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
            Q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
          )
        )) %>%
        tidyr::pivot_longer(
          cols = everything(),
          names_to = c("Variable", "Metric"),
          names_sep = "_(?=[^_]+$)"
        ) %>%
        tidyr::pivot_wider(names_from = "Metric", values_from = "value") %>%
        mutate(across(where(is.numeric), ~ round(.x, 2)))
      
      datatable(stats, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })
    
    output$cat_var_select_ui <- renderUI({
      df <- uploaded_data()
      req(df)
      cat_vars <- df %>% select(where(~ is.character(.x) || is.factor(.x))) %>% colnames()
      
      if (length(cat_vars) == 0) {
        return(div(
          style = "padding: 15px; background-color: #F8FAFC; border: 1px solid #E2E8F0; border-radius: 8px; color: #64748B; font-size: 1rem;",
          "⚠️ No categorical (text/grouping) variables found in this dataset."
        ))
      }
      
      selectInput(session$ns("cat_var_choice"), "Select Categorical Variable:", choices = cat_vars)
    })
    
    output$freq_table <- renderDT({
      df <- uploaded_data()
      req(df)
      
      cat_vars <- df %>% select(where(~ is.character(.x) || is.factor(.x))) %>% colnames()
      
      if (length(cat_vars) == 0 || is.null(input$cat_var_choice) || !(input$cat_var_choice %in% cat_vars)) {
        return(datatable(
          data.frame(),
          options = list(dom = 't'),
          rownames = FALSE,
          colnames = NULL
        ))
      }
      
      var_sym <- rlang::sym(input$cat_var_choice)
      freq_df <- df %>%
        group_by(!!var_sym) %>%
        summarise(Frequency = n()) %>%
        mutate(Percentage = paste0(round((Frequency / sum(Frequency)) * 100, 2), "%")) %>%
        rename(Category = 1)
      
      datatable(freq_df, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
    
    return(uploaded_data)
  })
}