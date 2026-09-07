options(shiny.maxRequestSize = 50 * 1024^2) # Increases upload limit to 50MB

library(shiny)
library(ggplot2)
library(DT)
library(readr)
library(readxl)
library(dplyr)
library(bslib)

source("modules/upload_summary.R")
source("modules/visualization.R")
source("modules/hypothesis.R")

# ------------------------------------------------------------------------------
# Modern Slate-Ice Blue Theme & Vertical Alignment CSS
# ------------------------------------------------------------------------------
nexora_theme <- bs_theme(
  version = 5,
  bg = "#F4F7FC",          # Soft bluish-white background
  fg = "#1E293B",          # Deep slate text
  primary = "#2563EB",     # Royal blue accent
  secondary = "#475569",   # Muted slate blue
  success = "#0D9488",     # Teal green
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)

ui <- navbarPage(
  title = div(
    style = "font-weight: 700; font-size: 1.4rem; color: #FFFFFF; letter-spacing: 0.5px;",
    "⚡ NEXORA"
  ),
  windowTitle = "NEXORA — No-Code Data Analytics",
  id = "nav_bar",
  theme = nexora_theme,
  
  footer = div(
    style = "text-align: center; padding: 10px 0 15px 0; margin-top: 20px; color: #94A3B8; font-size: 0.75rem; background: transparent; border: none;",
    "⚡ NEXORA — Intelligent No-Code Data Analytics | Developed by UPNM Team (Hakim, Syirah, Dalili, Awatif, Naim)"
  ),
  
  header = tags$head(
    tags$style(HTML("
      /* Top Navbar Flexbox & Vertical Alignment Fixes */
      .navbar {
        background: linear-gradient(135deg, #1E3A8A 0%, #2563EB 100%) !important;
        box-shadow: 0 4px 12px rgba(37, 99, 235, 0.15);
        border: none;
        padding: 0.6rem 1.2rem !important;
        display: flex !important;
        align-items: center !important;
      }
      
      .navbar-brand {
        display: flex !important;
        align-items: center !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
        margin-top: 0 !important;
        margin-bottom: 0 !important;
        line-height: 1 !important;
      }
      
      .navbar-nav {
        display: flex !important;
        align-items: center !important;
      }
      
      .navbar-nav .nav-link {
        color: #E2E8F0 !important;
        font-weight: 500;
        margin-right: 6px;
        border-radius: 8px;
        padding: 0.5rem 0.9rem !important;
        transition: all 0.2s ease;
        display: flex !important;
        align-items: center !important;
      }
      
      .navbar-nav .nav-link:hover {
        color: #FFFFFF !important;
        background: rgba(255, 255, 255, 0.15);
      }
      
      .navbar-nav .nav-link.active {
        color: #FFFFFF !important;
        background: rgba(255, 255, 255, 0.25) !important;
        font-weight: 600;
      }
      
      /* Card & Panel Styling */
      .well, .card, .wellPanel {
        background-color: #FFFFFF !important;
        border: 1px solid #E2E8F0 !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 6px -1px rgba(148, 163, 184, 0.1), 0 2px 4px -1px rgba(148, 163, 184, 0.06);
        padding: 20px;
        margin-bottom: 20px;
      }
      
      /* Action Buttons */
      .btn-primary {
        background: linear-gradient(135deg, #2563EB 0%, #1D4ED8 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        padding: 10px 20px;
        box-shadow: 0 4px 6px -1px rgba(37, 99, 235, 0.2);
        transition: all 0.2s ease;
      }
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 10px -1px rgba(37, 99, 235, 0.3);
      }

      /* Headings & Lists */
      h1, h2, h3, h4 {
        color: #0F172A;
        font-weight: 600;
      }
      
      ul.styled-list li {
        margin-bottom: 8px;
        color: #334155;
        font-size: 1.05rem;
      }
    "))
  ),
  
  # ---------------- HOME ----------------
  tabPanel(
    "Home",
    fluidPage(
      div(
        class = "card",
        style = "margin-top: 10px; background: linear-gradient(135deg, #EFF6FF 0%, #DBEAFE 100%) !important; border: 1px solid #BFDBFE !important;",
        h1("Welcome to NEXORA", style = "color: #1E3A8A; font-weight: 700;"),
        h4("Interactive Statistical Analysis & Visualization Dashboard", style = "color: #2563EB;"),
        hr(style = "border-color: #93C5FD;")
      ),
      div(
        class = "card",
        h3("🎯 Project Objective"),
        p(
          "This dashboard allows users to upload custom datasets and perform exploratory data analysis, interactive visualizations, hypothesis testing, and regression modeling without writing code.",
          style = "font-size: 1.05rem; color: #475569;"
        )
      ),
      div(
        class = "row mt-4",
        div(
          class = "col-md-4",
          div(
            class = "card p-3 text-center shadow-sm",
            h4("📁 Step 1: Upload"),
            p("Import CSV or Excel files and clean missing values."),
            actionButton("go_upload", "Start Analysis ➔", class = "btn-primary btn-sm")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = "card p-3 text-center shadow-sm",
            h4("📊 Step 2: Visualize"),
            p("Generate histograms, box plots, and bar charts."),
            actionButton("go_viz", "Explore Visuals ➔", class = "btn-outline-primary btn-sm")
          )
        ),
        div(
          class = "col-md-4",
          div(
            class = "card p-3 text-center shadow-sm",
            h4("📖 Step 3: Guide"),
            p("Find the exact statistical test for your research data."),
            actionButton("go_help", "View Guide ➔", class = "btn-outline-primary btn-sm")
          )
        )
      )
    )
  ),
  
  # ---------------- UPLOAD ----------------
  tabPanel(
    "Upload & Summary",
    upload_summary_ui("upload")
  ),
  
  # ---------------- VISUALIZATION ----------------
  tabPanel(
    "Data Visualization",
    sidebarLayout(
      sidebarPanel(
        h4("Graph Options"),
        selectInput(
          "plot_type",
          "Choose Plot Type",
          choices = c("Histogram", "Box Plot", "Scatter Plot", "Bar Chart", "Pie Chart")
        ),
        uiOutput("plot_controls")
      ),
      mainPanel(
        div(
          class = "card",
          plotOutput("visualizationPlot", height = "600px"),
          downloadButton("download_plot", "🖼️ Download High-Res Plot (PNG)", class = "btn-primary w-100 mt-2")
        )
      )
    )
  ),
  
  # ---------------- HYPOTHESIS ----------------
  tabPanel(
    "Hypothesis Testing",
    hypothesis_ui("hypothesis")
  ),
  
  # ---------------- REGRESSION ----------------
  tabPanel(
    "Regression & Correlation",
    sidebarLayout(
      sidebarPanel(
        h3("Analysis Settings"),
        selectInput("xvar", "Choose X Variable (Predictor)", choices = NULL),
        selectInput("yvar", "Choose Y Variable (Response)", choices = NULL),
        radioButtons("method", "Correlation Method", choices = c("Pearson", "Spearman"), selected = "Pearson"),
        actionButton("run", "Run Analysis", class = "btn-primary w-100")
      ),
      mainPanel(
        h2("📈 Regression & Correlation Analysis"),
        div(
          class = "card",
          h4("Correlation Result"),
          verbatimTextOutput("correlation")
        ),
        div(
          class = "card",
          h4("Regression Summary & Explicit Model"),
          verbatimTextOutput("regression")
        ),
        div(
          class = "card",
          h4("Fitted Regression Scatter Plot"),
          plotOutput("scatterPlot", height = "450px")
        ),
        div(
          class = "card",
          h4("Diagnostic Residual Plots"),
          plotOutput("residualPlot", height = "450px")
        )
      )
    )
  ),
  
  # ---------------- HELP & DECISION GUIDE ----------------
  tabPanel(
    "Help",
    fluidPage(
      div(
        class = "card",
        style = "margin-top: 10px; background: linear-gradient(135deg, #EFF6FF 0%, #DBEAFE 100%) !important; border: 1px solid #BFDBFE !important;",
        h3("📖 NEXORA Universal Research & Operating Guide", style = "color: #1E3A8A; font-weight: 700;"),
        p("No statistical background needed! Find your research question below to see which tool fits your dataset.", style = "color: #2563EB; font-size: 1.05rem;")
      ),
      
      div(
        class = "card",
        h4("🧪 Find the Right Analysis for Your Goal"),
        tags$table(
          class = "table table-hover table-bordered",
          style = "margin-top: 15px; background-color: #FFFFFF;",
          tags$thead(
            style = "background-color: #1E3A8A; color: #FFFFFF;",
            tags$tr(
              tags$th("Your Core Question"),
              tags$th("Required Data Columns"),
              tags$th("Example Use Cases"),
              tags$th("Recommended Tool in NEXORA")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$b("Do two numeric factors move together, or does X predict Y?")),
              tags$td("2 Numeric Columns"),
              tags$td("• Study Hours vs. Test Score\n• Ad Spend vs. Sales Volume\n• Dosage vs. Response Rate"),
              tags$td(tags$span(class = "badge bg-success", "Regression & Correlation"), " ➔ Linear Regression")
            ),
            tags$tr(
              tags$td(tags$b("Is there a significant difference between two groups?")),
              tags$td("1 Text/Group Column + 1 Numeric Column"),
              tags$td("• Control vs. Treatment Group\n• Online vs. In-Store Buyers\n• Daytime vs. Night Shift Output"),
              tags$td(tags$span(class = "badge bg-primary", "Hypothesis Testing"), " ➔ 2-Sample t-Test (or Box Plot)")
            ),
            tags$tr(
              tags$td(tags$b("Does my average match a specific benchmark or standard?")),
              tags$td("1 Numeric Column + 1 Target Value"),
              tags$td("• Product weight vs. 500g label target\n• Patient temp vs. 37.0°C baseline\n• Class average vs. 50% pass mark"),
              tags$td(tags$span(class = "badge bg-primary", "Hypothesis Testing"), " ➔ 1-Sample t-Test")
            ),
            tags$tr(
              tags$td(tags$b("How is my numerical data spread out across values?")),
              tags$td("1 Numeric Column"),
              tags$td("• Income or age distribution\n• Daily call center wait times\n• Laboratory measurement ranges"),
              tags$td(tags$span(class = "badge bg-info", "Data Visualization"), " ➔ Histogram")
            ),
            tags$tr(
              tags$td(tags$b("What are the counts or percentage breakdowns of categories?")),
              tags$td("1 Categorical/Text Column"),
              tags$td("• Survey ratings (Low/Med/High)\n• Department classifications\n• Yes/No poll responses"),
              tags$td(tags$span(class = "badge bg-info", "Data Visualization"), " ➔ Bar Chart or Pie Chart")
            )
          )
        )
      ),
      
      div(
        class = "card",
        h4("⚡ Standard 4-Step Workflow"),
        tags$ol(
          style = "line-height: 1.8; font-size: 1.05rem; color: #334155;",
          tags$li(tags$b("Upload: "), "Go to 'Upload & Summary' and drop your .csv or .xlsx file to review row counts and missing data."),
          tags$li(tags$b("Visualize: "), "Use 'Data Visualization' to inspect patterns or check for unexpected outliers."),
          tags$li(tags$b("Test Significance: "), "Use 'Hypothesis Testing' to verify if group differences are real or random noise (p < 0.05)."),
          tags$li(tags$b("Model Relationships: "), "Use 'Regression & Correlation' to calculate trend formulas (", HTML("<i>Y</i> = &beta;<sub>0</sub> + &beta;<sub>1</sub><i>X</i>"), ") and assess model fit.")
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  # Home Launchpad Navigation Handlers
  observeEvent(input$go_upload, { updateNavbarPage(session, "nav_bar", selected = "Upload & Summary") })
  observeEvent(input$go_viz, { updateNavbarPage(session, "nav_bar", selected = "Data Visualization") })
  observeEvent(input$go_help, { updateNavbarPage(session, "nav_bar", selected = "Help") })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("NEXORA_Plot_", gsub(" ", "_", input$plot_type), "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(dataset())
      
      # Recreate the current active plot
      p <- generate_selected_plot(
        data = dataset(),
        plot_type = input$plot_type,
        x = input$plot_x,
        y = input$plot_y,
        bins = input$hist_bins
      )
      
      # Save high-resolution 300 DPI PNG image
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300, device = "png")
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("NEXORA_Full_Analysis_Report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      df <- dataset()
      req(df)
      
      temp_html <- tempfile(fileext = ".html")
      
      # 1. Compute Column Data Types Table
      types_rows <- sapply(colnames(df), function(col) {
        sprintf("<tr><td><b>%s</b></td><td>%s</td></tr>", col, paste(class(df[[col]]), collapse = ", "))
      })
      types_section <- paste0(
        "<table class='report-table'><thead><tr><th>Column Name</th><th>Data Type</th></tr></thead><tbody>",
        paste(types_rows, collapse = ""),
        "</tbody></table>"
      )
      
      # 2. Compute Quantitative Metrics
      num_cols <- names(df)[sapply(df, is.numeric)]
      if (length(num_cols) > 0) {
        num_rows <- sapply(num_cols, function(col) {
          vals <- df[[col]]
          vals_clean <- vals[!is.na(vals)]
          if (length(vals_clean) == 0) return("")
          sprintf(
            "<tr><td><b>%s</b></td><td>%.2f</td><td>%.2f</td><td>%.2f</td><td>%.2f</td><td>%.2f</td></tr>",
            col, mean(vals_clean), median(vals_clean), sd(vals_clean), min(vals_clean), max(vals_clean)
          )
        })
        num_section <- paste0(
          "<table class='report-table'>",
          "<thead><tr><th>Variable</th><th>Mean</th><th>Median</th><th>Std Dev</th><th>Min</th><th>Max</th></tr></thead>",
          "<tbody>", paste(num_rows, collapse = ""), "</tbody></table>"
        )
      } else {
        num_section <- "<p><i>No quantitative (numeric) variables found in this dataset.</i></p>"
      }
      
      # 3. Compute Qualitative Frequency Tables
      cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      if (length(cat_cols) > 0) {
        cat_sections <- sapply(cat_cols, function(col) {
          tbl_df <- as.data.frame(table(df[[col]], useNA = "no"))
          if (nrow(tbl_df) == 0) return("")
          names(tbl_df) <- c("Category", "Count")
          tbl_df <- tbl_df[order(-tbl_df$Count), ]
          total_n <- sum(tbl_df$Count)
          
          rows <- apply(head(tbl_df, 6), 1, function(r) {
            pct <- round((as.numeric(r[2]) / total_n) * 100, 1)
            sprintf("<tr><td>%s</td><td>%s</td><td>%.1f%%</td></tr>", r[1], r[2], pct)
          })
          
          paste0(
            "<h4 style='margin-bottom: 5px; color: #1E3A8A;'>Column: ", col, "</h4>",
            "<table class='report-table'><thead><tr><th>Category</th><th>Count</th><th>Percentage</th></tr></thead>",
            "<tbody>", paste(rows, collapse = ""), "</tbody></table>"
          )
        })
        cat_section <- paste(cat_sections, collapse = "")
      } else {
        cat_section <- "<p><i>No qualitative (categorical) variables found in this dataset.</i></p>"
      }
      
      # 4. Assemble Full HTML Document
      report_content <- paste0(
        "<!DOCTYPE html>",
        "<html><head>",
        '<meta charset="UTF-8">',
        "<style>",
        "@page { size: A4; margin: 15mm; }",
        "body { font-family: Arial, sans-serif; color: #1E293B; background: #FFFFFF; font-size: 13px; }",
        "h1 { color: #1E3A8A; font-size: 22px; border-bottom: 2px solid #2563EB; padding-bottom: 6px; margin-bottom: 10px; }",
        "h3 { color: #1E3A8A; margin-top: 0; margin-bottom: 10px; border-bottom: 1px solid #E2E8F0; padding-bottom: 4px; }",
        ".card { background: #F8FAFC; border: 1px solid #CBD5E1; padding: 14px; border-radius: 8px; margin-bottom: 16px; page-break-inside: avoid; }",
        ".report-table { width: 100%; border-collapse: collapse; margin-top: 6px; margin-bottom: 10px; background: #FFFFFF; }",
        ".report-table th, .report-table td { border: 1px solid #CBD5E1; padding: 6px 10px; text-align: left; }",
        ".report-table th { background-color: #1E3A8A; color: #FFFFFF; font-weight: 600; }",
        "</style></head><body>",
        
        "<h1>⚡ NEXORA Automated Data Summary Report</h1>",
        "<p style='color: #64748B;'><b>Generated on:</b> ", Sys.time(), "</p>",
        
        "<div class='card'><h3>📊 Dataset Overview</h3>",
        "<p><b>Total Observations (Rows):</b> ", nrow(df), "<br>",
        "<b>Total Variables (Columns):</b> ", ncol(df), "<br>",
        "<b>Total Missing Values:</b> ", sum(is.na(df)), "</p></div>",
        
        "<div class='card'><h3>📋 Column Variables & Data Types</h3>", types_section, "</div>",
        
        "<div class='card'><h3>📈 Quantitative Summary Metrics</h3>", num_section, "</div>",
        
        "<div class='card'><h3>🗂️ Qualitative Frequency Tables</h3>", cat_section, "</div>",
        
        "</body></html>"
      )
      
      con <- file(temp_html, open = "wt", encoding = "UTF-8")
      writeLines(report_content, con, useBytes = TRUE)
      close(con)
      
      pagedown::chrome_print(input = temp_html, output = file)
    }
  )
  
  dataset <- upload_summary_server("upload")
  
  output$plot_controls <- renderUI({
    req(dataset())
    
    nums <- get_numeric_columns(dataset())
    cats <- get_categorical_columns(dataset())
    
    switch(
      input$plot_type,
      "Histogram" = tagList(
        selectInput("plot_x", "Numeric Variable", choices = nums),
        sliderInput("hist_bins", "Number of Bins:", min = 5, max = 100, value = 30)
      ),
      "Bar Chart" = selectInput("plot_x", "Categorical Variable", choices = cats),
      "Pie Chart" = selectInput("plot_x", "Categorical Variable", choices = cats),
      "Scatter Plot" = tagList(
        selectInput("plot_x", "X Variable", choices = nums),
        selectInput("plot_y", "Y Variable", choices = nums, selected = nums[min(2, length(nums))])
      ),
      "Box Plot" = tagList(
        selectInput("plot_x", "Categorical Variable", choices = cats),
        selectInput("plot_y", "Numeric Variable", choices = nums)
      )
    )
  })
  
  observe({
    req(dataset())
    nums <- get_numeric_columns(dataset())
    
    if(length(nums) < 2){
      updateSelectInput(session, "xvar", choices = character(0))
      updateSelectInput(session, "yvar", choices = character(0))
      return()
    }
    
    updateSelectInput(session, "xvar", choices = nums)
    updateSelectInput(session, "yvar", choices = nums, selected = nums[2])
  })
  
  output$visualizationPlot <- renderPlot({
    req(dataset())
    nums <- get_numeric_columns(dataset())
    cats <- get_categorical_columns(dataset())
    
    if(input$plot_type == "Histogram")
      validate(need(length(nums) >= 1, "Dataset has no numeric variables."))
    
    if(input$plot_type == "Scatter Plot")
      validate(need(length(nums) >= 2, "Need at least two numeric variables."))
    
    if(input$plot_type %in% c("Bar Chart","Pie Chart"))
      validate(need(length(cats) >= 1, "Dataset has no categorical variables."))
    
    if(input$plot_type == "Box Plot")
      validate(need(length(nums) >= 1 && length(cats) >= 1, "Need one numeric and one categorical variable."))
    
    generate_selected_plot(
      data = dataset(),
      plot_type = input$plot_type,
      x = input$plot_x,
      y = input$plot_y,
      bins = input$hist_bins
    )
  })
  
  hypothesis_server("hypothesis", dataset)
  
  observeEvent(input$run, {
    req(input$xvar, input$yvar)
    df <- dataset()
    nums <- get_numeric_columns(df)
    
    validate(
      need(length(nums) >= 2, "Dataset must contain at least two numeric variables.")
    )
    
    x <- df[[input$xvar]]
    y <- df[[input$yvar]]
    
    valid <- complete.cases(x, y)
    x <- x[valid]
    y <- y[valid]
    
    validate(
      need(length(x) >= 2, "Not enough complete observations (at least 2 required) for regression."),
      need(sd(x) > 0, paste("Selected X variable (", input$xvar, ") has zero variance.")),
      need(sd(y) > 0, paste("Selected Y variable (", input$yvar, ") has zero variance."))
    )
    
    cor_result <- cor.test(x, y, method = tolower(input$method))
    model <- lm(y ~ x)
    
    output$correlation <- renderPrint({
      cat("Correlation Coefficient (r):", round(cor_result$estimate, 4), "\n\n")
      cat("P-value:", signif(cor_result$p.value, 4), "\n\n")
      
      r <- cor_result$estimate
      cat("Interpretation:\n")
      if(abs(r) >= 0.8){
        cat("There is a very strong relationship between", input$xvar, "and", input$yvar, "\n")
      } else if(abs(r) >= 0.6){
        cat("There is a strong relationship between", input$xvar, "and", input$yvar, "\n")
      } else if(abs(r) >= 0.4){
        cat("There is a moderate relationship between", input$xvar, "and", input$yvar, "\n")
      } else{
        cat("There is a weak relationship between", input$xvar, "and", input$yvar, "\n")
      }
      
      if(!is.null(cor_result$conf.int)){
        cat("\n95% Confidence Interval:\n")
        print(cor_result$conf.int)
      }
    })
    
    output$regression <- renderPrint({
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      sign_str <- if(slope >= 0) "+" else "-"
      
      cat("====================================================\n")
      cat(" EXPLICIT LINEAR REGRESSION EQUATION:\n")
      cat(sprintf(" %s = %.4f %s %.4f * %s\n", input$yvar, intercept, sign_str, abs(slope), input$xvar))
      cat("====================================================\n\n")
      
      summary(model)
    })
    
    output$scatterPlot <- renderPlot({
      plot_df <- data.frame(x = x, y = y)
      
      ggplot(plot_df, aes(x = x, y = y)) +
        geom_point(color = "#2563EB", size = 4, alpha = 0.7) +
        geom_smooth(method = "lm", color = "#DC2626", linewidth = 1.2, se = TRUE) +
        labs(title = paste(input$xvar, "vs", input$yvar), x = input$xvar, y = input$yvar) +
        theme_minimal(base_size = 14)
    })
    
    output$residualPlot <- renderPlot({
      par(mfrow = c(2, 2))
      plot(model)
    })
  })
}

shinyApp(ui = ui, server = server)