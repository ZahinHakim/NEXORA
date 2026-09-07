# ==============================================================================
# Hypothesis.R
# Purpose: Reusable hypothesis testing functions for NEXORA
# ==============================================================================

library(shiny)
library(dplyr)
library(bslib)
library(rlang)

# ------------------------------------------------------------------------------
# 1. UI FUNCTION
# ------------------------------------------------------------------------------
hypothesis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("🧪 Hypothesis Testing"),
      hr(),
      selectInput(ns("test_type"), "1. Select Test Type:", 
                  choices = c("One-Sample t-Test" = "one_sample", "Two-Sample Independent t-Test" = "two_sample")),
      numericInput(ns("alpha"), "2. Significance Level (α):", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
      uiOutput(ns("test_controls_ui"))
    ),
    mainPanel(
      width = 8,
      card(card_header(h4("📋 Test Hypotheses & Specification")), br(), verbatimTextOutput(ns("hypotheses_display"))),
      br(),
      card(card_header(h4("📊 Statistical Test Results")), br(), tableOutput(ns("results_table"))),
      br(),
      card(card_header(h4("💡 Conclusion & Decision")), br(), uiOutput(ns("conclusion_display"))),
      
      uiOutput(ns("insight_card"))
      
    )
  )
}

# ------------------------------------------------------------------------------
# 2. SERVER FUNCTION
# ------------------------------------------------------------------------------
hypothesis_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Dynamic Inputs based on Test Selection
    output$test_controls_ui <- renderUI({
      df <- data_reactive()
      if (is.null(df)) return(p(tags$span(style = "color: red;", "Please upload a dataset first.")))
      
      num_cols <- df %>% select(where(is.numeric)) %>% colnames()
      cat_cols <- df %>% select(where(~ is.character(.x) || is.factor(.x))) %>% colnames()
      validate(need(length(num_cols) > 0, "No numeric variables available."))
      
      if (input$test_type == "one_sample") {
        tagList(
          selectInput(ns("var_one"), "3. Select Quantitative Variable:", choices = num_cols),
          numericInput(ns("mu_0"), "4. Hypothesized Mean (μ₀):", value = 0),
          selectInput(ns("alternative"), "5. Direction:", choices = c("Two-Sided (≠)" = "two.sided", "Greater (>)" = "greater", "Less (<)" = "less"))
        )
      } else {
        validate(need(length(cat_cols) > 0, "No categorical variables available for grouping."))
        tagList(
          selectInput(ns("var_num"), "3. Select Outcome Variable:", choices = num_cols),
          selectInput(ns("var_cat"), "4. Select Grouping Variable (2 groups):", choices = cat_cols),
          selectInput(ns("alternative"), "5. Direction:", choices = c("Two-Sided (≠)" = "two.sided", "Greater (>)" = "greater", "Less (<)" = "less")),
          checkboxInput(ns("var_equal"), "Assume Equal Variances (Pooled)", value = FALSE)
        )
      }
    })
    
    # Core Computation Logic
    test_results <- reactive({
      df <- data_reactive()
      req(df, input$test_type, input$alpha, input$alternative)
      alpha <- input$alpha
      validate(need(!is.na(alpha) && alpha > 0 && alpha < 1, "α must be between 0 and 1."))
      
      sym <- switch(input$alternative, "two.sided" = "≠", "greater" = ">", "less" = "<")
      
      if (input$test_type == "one_sample") {
        req(input$var_one, !is.null(input$mu_0))
        vec <- na.omit(df[[input$var_one]])
        validate(need(length(vec) > 1, "At least 2 observations required."))
        
        t_res <- t.test(vec, mu = input$mu_0, alternative = input$alternative, conf.level = 1 - alpha)
        
        list(
          method = if (length(vec) >= 30) "One-Sample t-Test (CLT)" else "One-Sample t-Test",
          n_info = paste0("n = ", length(vec)),
          h0 = paste0("H₀: Mean(", input$var_one, ") = ", input$mu_0),
          h1 = paste0("H₁: Mean(", input$var_one, ") ", sym, " ", input$mu_0),
          stat = as.numeric(t_res$statistic),
          df = as.numeric(t_res$parameter),
          p_val = t_res$p.value,
          ci = t_res$conf.int,
          alpha = alpha
        )
      } else {
        req(input$var_num, input$var_cat)
        sub_df <- df %>% select(all_of(c(input$var_num, input$var_cat))) %>% na.omit()
        groups <- unique(sub_df[[input$var_cat]])
        validate(need(length(groups) == 2, "Grouping variable must have exactly 2 categories."))
        
        g1 <- sub_df %>% filter(.data[[input$var_cat]] == groups[1]) %>% pull(input$var_num)
        g2 <- sub_df %>% filter(.data[[input$var_cat]] == groups[2]) %>% pull(input$var_num)
        validate(need(length(g1) > 1 && length(g2) > 1, "Each group needs at least 2 observations."))
        
        t_res <- t.test(g1, g2, alternative = input$alternative, var.equal = isTRUE(input$var_equal), conf.level = 1 - alpha)
        
        list(
          method = if (isTRUE(input$var_equal)) "Two-Sample Pooled t-Test" else "Two-Sample Welch's t-Test",
          n_info = paste0(groups[1], " (n₁=", length(g1), "), ", groups[2], " (n₂=", length(g2), ")"),
          h0 = paste0("H₀: Mean(", groups[1], ") - Mean(", groups[2], ") = 0"),
          h1 = paste0("H₁: Mean(", groups[1], ") - Mean(", groups[2], ") ", sym, " 0"),
          df = as.numeric(t_res$parameter),
          stat = as.numeric(t_res$statistic), p_val = t_res$p.value, ci = t_res$conf.int, alpha = alpha
        )
      }
    })
    
    # Outputs
    output$hypotheses_display <- renderText({
      res <- test_results()
      paste0("Method      : ", res$method, "\nSample Size : ", res$n_info, "\nNull        : ", res$h0, "\nAlternative : ", res$h1)
    })
    
    output$results_table <- renderTable({
      res <- test_results()
      data.frame(
        `Statistical Metric` = c(
          "Test Method",
          "Degrees of Freedom",
          "t-Statistic",
          "p-value",
          "Alpha (α)",
          paste0((1 - res$alpha) * 100, "% CI")
        ),
        `Value` = c(
          res$method,
          round(res$df,2),
          round(res$stat,4),
          format.pval(res$p_val, digits = 4, eps = 0.0001),
          res$alpha,
          paste0("[", round(res$ci[1],4), ", ", round(res$ci[2],4), "]")
        ),
        check.names = FALSE
      )
    })
    
    output$conclusion_display <- renderUI({
      res <- test_results()
      p_fmt <- format.pval(res$p_val, digits = 4, eps = 0.0001)
      reject <- res$p_val < res$alpha
      
      div(
        class =
          if(reject)
            "alert alert-danger"
        else
          "alert alert-success",
        tags$strong(
          if (reject)
            "Decision: Reject H₀"
          else
            "Decision: Fail to Reject H₀"
        ),
        
        br(),
        
        if(reject){
          
          p("There is sufficient statistical evidence to support the alternative hypothesis.")
          
        }else{
          
          p("There is insufficient statistical evidence to reject the null hypothesis.")
          
        },
        p(if (reject) paste0("p-value (", p_fmt, ") < α (", res$alpha, "). Sufficient evidence to support H₁.") 
          else paste0("p-value (", p_fmt, ") ≥ α (", res$alpha, "). Insufficient evidence to reject H₀."))
      )
    })
  })
}
