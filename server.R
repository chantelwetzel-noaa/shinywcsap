# PACKAGES:
## shiny
library(shiny)
library(shinyBS)

## data manipulation
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)

## tables + data visualizations
library(ggplot2)
library(gt)
library(gtExtras)
library(plotly)
library(viridis)

## downloads
library(rmarkdown)
library(readr)
library(openxlsx)

# define server logic to display user inputs
shinyServer(function(input, output, session) {
  
  #browser()
  values <- reactiveValues()
  values$year <- 2026

  species_groups <- reactive({
    req(input$yr)
    species_groups <- readr::read_csv(here::here("tables", input$yr, "species_management_groups.csv"), show_col_types = FALSE) 
    species_groups
    })
  
  output$calendar <- renderImage({ 
    req(input$yr)
    image <- paste0("www/figs/", input$yr, "/calendar.png")
    width <- "600px"
    height <- "600px"
    list(
      src = image, 
      alt = "Calendar",
      contentType = "image/png", 
      width = width, 
      height = height,
      style = "display: block; margin-left: auto; margin-right: auto;"
    )
  }, deleteFile = FALSE)

  
  output$management_groups1 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups1 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "fm_species_selector",
        label = "Select a species management group:",
        choices = management_groups1,
        selected = management_groups1,
        multiple = TRUE
      )
    }
  })
  output$management_groups2 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups2 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "com_species_selector",
        label = "Select a species management group:",
        choices = management_groups2,
        selected = management_groups2,
        multiple = TRUE
      )
    }
  })
  output$management_groups3 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups3 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "tribal_species_selector",
        label = "Select a species management group:",
        choices = management_groups3,
        selected = management_groups3,
        multiple = TRUE
      )
    }
  })
  output$management_groups4 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups4 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "rec_species_selector",
        label = "Select a species management group:",
        choices = management_groups4,
        selected = management_groups4,
        multiple = TRUE
      )
    }
  })
  output$management_groups5 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups5 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "cd_species_selector",
        label = "Select a species management group:",
        choices = management_groups5,
        selected = management_groups5,
        multiple = TRUE
      )
    }
  })
  output$management_groups6 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups6 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "ss_species_selector",
        label = "Select a species management group:",
        choices = management_groups6,
        selected = management_groups6,
        multiple = TRUE
      )
    }
  })
  output$management_groups7 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups7 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "reb_species_selector",
        label = "Select a species management group:",
        choices = management_groups7,
        selected = management_groups7,
        multiple = TRUE
      )
    }
  })
  output$management_groups8 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups8 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "eco_species_selector",
        label = "Select a species management group:",
        choices = management_groups8,
        selected = management_groups8,
        multiple = TRUE
      )
    }
  })
  output$management_groups9 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups9 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "af_species_selector",
        label = "Select a species management group:",
        choices = management_groups9,
        selected = management_groups9,
        multiple = TRUE
      )
    }
  })
  output$management_groups10 <- renderUI({
    if(!is.null(species_groups)) {
      management_groups10 <- unique(as.character(species_groups()$`Management Group`))
      selectInput(
        inputId = "ni_species_selector",
        label = "Select a species management group:",
        choices = management_groups10,
        selected = management_groups10,
        multiple = TRUE
      )
    }
  })

  fish_mort_data <- reactive({
    req(input$yr)
    fish_mort_data <- readr::read_csv(here::here("tables", input$yr, "1_fishing_mortality.csv"), show_col_types = FALSE)
    fish_mort_data})
  com_rev_data <- reactive({
    req(input$yr)
    com_rev_data <- readr::read_csv(here::here("tables", input$yr, "2_commercial_revenue.csv"), show_col_types = FALSE) 
    com_rev_data})
  tribal_data <- reactive({
    req(input$yr)
    tribal_data <- readr::read_csv(here::here("tables", input$yr, "3_tribal_revenue.csv"), show_col_types = FALSE) 
    tribal_data})
  rec_data <- reactive({
    req(input$yr)
    rec_data <- readr::read_csv(here::here("tables", input$yr, "4_recreational_importance.csv"), show_col_types = FALSE) 
    rec_data})
  eco_data <- reactive({
    req(input$yr)
    eco_data <- readr::read_csv(here::here("tables", input$yr, "5_ecosystem.csv"), show_col_types = FALSE) 
    eco_data})
  stock_stat_data <- reactive({
    req(input$yr)
    stock_stat_data <- readr::read_csv(here::here("tables", input$yr, "6_stock_status.csv"), show_col_types = FALSE) 
    stock_stat_data})
  assess_freq_data <- reactive({
    req(input$yr)
    assess_freq_data <- readr::read_csv(here::here("tables", input$yr, "7_assessment_frequency.csv"), show_col_types = FALSE) 
    assess_freq_data})
  const_dem_data <- reactive({
    req(input$yr)
    const_dem_data <- readr::read_csv(here::here("tables", input$yr, "8_constituent_demand.csv"), show_col_types = FALSE)
    const_dem_data})
  new_info_data <- reactive({
    req(input$yr)
    new_info_data <- readr::read_csv(here::here("tables", input$yr, "9_new_information.csv"), show_col_types = FALSE) 
    new_info_data})
  rebuilding_data <- reactive({
    req(input$yr)
    rebuilding_data <- readr::read_csv(here::here("tables", input$yr, "10_rebuilding.csv"), show_col_types = FALSE) 
    rebuilding_data})
  year_assessed <- reactive({
    req(assess_freq_data)
    year_assessed <- as.data.frame(assess_freq_data()[,"Last Assessment Year"]) 
    colnames(year_assessed) <- NULL
    year_assessed
  })
  
  # results <- reactive({
  #   results <- data.frame(
  #     species = com_rev_data$Species,
  #     com_rev_fs = com_rev_data$`Factor Score`,
  #     rec_fs = rec_data$`Factor Score`,
  #     tribal_fs = tribal_data$`Factor Score`,
  #     const_dem_fs = const_dem_data$`Factor Score`,
  #     reb_fs = rebuilding_data$`Factor Score`,
  #     ss_fs = stock_stat_data$`Factor Score`,
  #     fish_mort_fs = fish_mort_data$`Factor Score`,
  #     eco_fs = eco_data$`Factor Score`,
  #     new_info_fs = new_info_data$`Factor Score`,
  #     assess_fs = assess_freq_data$`Factor Score`)
  #   results
  # })
  
  # create overall ranking table
  results <- reactive({
    join_cols <- c("Species", "Factor Score")
    results <- dplyr::left_join(
      dplyr::left_join(
        dplyr::left_join(
          dplyr::left_join(
            dplyr::left_join(
              dplyr::left_join(
                dplyr::left_join(
                  dplyr::left_join(
                    dplyr::left_join(com_rev_data()[, join_cols], rec_data()[, join_cols], by = "Species"),
                    tribal_data()[, join_cols], by = "Species"),
                  const_dem_data()[, join_cols], by = "Species"),
                rebuilding_data()[, join_cols], by = "Species"),
              stock_stat_data()[, join_cols], by = "Species"),
            fish_mort_data()[, join_cols], by = "Species"),
          eco_data()[, join_cols], by = "Species"),
        new_info_data()[, join_cols], by = "Species"),
      assess_freq_data()[, join_cols], by = "Species")
    colnames(results) <- c("species", "com_rev_fs", "rec_fs", "tribal_fs", "const_dem_fs", "reb_fs", "ss_fs",
                           "fish_mort_fs", "eco_fs", "new_info_fs", "assess_fs")
    results <- as.data.frame(results)
    results
  })
  
  text_recent_five_years <- reactive({
    req(input$yr)
    if (input$yr == 2024) {
      text_recent_five_years <- "2018 - 2022"
    } 
    if (input$yr == 2026) {
      text_recent_five_years <- "2020 - 2024"
    }
    text_recent_five_years
  })
  
  # render HTML files for methodology page
  output$intro <- renderUI({
    tags$iframe(id = "introduction",
                seamless = "seamless",
                src = "11introduction.html",
                width = "74%", height = 600,
                style = "border:none;")
  })
  
  output$factors <- renderUI({
    tags$iframe(id = "factors",
                seamless = "seamless",
                src = "21factors.html",
                width = "74%", height = 800,
                style = "border:none;")
  })
  
  output$comm_importance <- renderUI({
    tags$iframe(id = "commercial_importance",
                seamless = "seamless",
                src = "22commercial_importance.html",
                width = "74%", height = 300,
                style = "border:none;")
  })
  
  output$tribal_importance <- renderUI({
    tags$iframe(id = "tribal_importance",
                seamless = "seamless",
                src = "23tribal_importance.html",
                width = "74%", height = 800,
                style = "border:none;")
  })
  
  output$rec_importance <- renderUI({
    tags$iframe(id = "recreational_importance",
                seamless = "seamless",
                src = "24recreational_importance.html",
                width = "74%", height = 800,
                style = "border:none;")
  })
  
  output$const_demand <- renderUI({
    tags$iframe(id = "constituent_demand",
                seamless = "seamless",
                src = "25constituent_demand.html",
                width = "74%", height = 700,
                style = "border:none;")
  })
  
  output$abundance <- renderUI({
    tags$iframe(id = "stock_status",
                seamless = "seamless",
                src = "26abundance.html",
                width = "74%", height = 800,
                style = "border:none;")
  })
  
  output$rebuild <- renderUI({
    tags$iframe(id = "rebuilding",
                seamless = "seamless",
                src = "27rebuild.html",
                width = "74%", height = 500,
                style = "border:none;")
  })
  
  output$fishing_mort <- renderUI({
    tags$iframe(id = "fishing_mortality",
                seamless = "seamless",
                src = "28fishing_mort.html",
                width = "74%", height = 700,
                style = "border:none;")
  })
  
  output$ecosystem <- renderUI({
    tags$iframe(id = "ecosystem",
                seamless = "seamless",
                src = "29ecosystem.html",
                width = "74%", height = 800,
                style = "border:none;")
  })
  
  output$new_data <- renderUI({
    tags$iframe(id = "new_information",
                seamless = "seamless",
                src = "30new_data.html",
                width = "74%", height = 500,
                style = "border:none;")
  })
  
  output$assessment_freq <- renderUI({
    tags$iframe(id = "assessment_frequency",
                seamless = "seamless",
                src = "31assessment_freq.html",
                width = "74%", height = 800,
                style = "border:none;")
  })
  
  # add factor weights
  comm_weight <- reactive({
    req(input$comm_weight)
    comm_weight <- input$comm_weight
    comm_weight
  })
  rec_weight <- reactive({
    req(input$rec_weight)
    rec_weight <- input$rec_weight
    rec_weight
  })
  tribal_weight <- reactive({
    req(tribal_weight)
    tribal_weight <- input$tribal_weight
    tribal_weight})
  cd_weight <- reactive({
    req(cd_weight)
    cd_weight <- input$cd_weight
    cd_weight})
  reb_weight <- reactive({
    req(reb_weight)
    reb_weight <- input$reb_weight
    reb_weight})
  ss_weight <- reactive({
    req(ss_weight)
    ss_weight <- input$ss_weight
    ss_weight})
  fm_weight <- reactive({
    req(fm_weight)
    fm_weight <- input$fm_weight
    fm_weight})
  eco_weight <- reactive({
    req(eco_weight)
    eco_weight <- input$eco_weight
    eco_weight})
  ni_weight <- reactive({
    req(ni_weight)
    ni_weight <- input$ni_weight
    ni_weight})
  af_weight <- reactive({
    req(af_weight)
    af_weight <- input$af_weight
    af_weight})
  sum_weights <- reactive({
    sum_weights <- round(comm_weight() + rec_weight() + tribal_weight() +
                                cd_weight() + reb_weight() + ss_weight() + fm_weight() +
                                eco_weight() + ni_weight() + af_weight(), 3)
    sum_weights
  })
  
  # display sum of factor weights
  output$weights_sum <- renderText({
    paste("Sum of weights:", sum_weights())
  })
  
  # print warning if sum is > 1.00
  output$warning <- renderText({
    if(sum_weights() != 1.00) {
      paste("<span style=\"color:red\">WARNING: Ensure all weights add up to 1.</span>")
    }
  })
  
  output$test <- renderPrint({
    overall_data()
  })
  # reset weights if button is pressed
  observeEvent(input$reset, {
    updateNumericInput(session, "comm_weight", value = 0.21)
    updateNumericInput(session, "rec_weight", value = 0.09)
    updateNumericInput(session, "tribal_weight", value = 0.05)
    updateNumericInput(session, "cd_weight", value = 0.11)
    updateNumericInput(session, "reb_weight", value = 0.10)
    updateNumericInput(session, "ss_weight", value = 0.08)
    updateNumericInput(session, "fm_weight", value = 0.08)
    updateNumericInput(session, "eco_weight", value = 0.05)
    updateNumericInput(session, "ni_weight", value = 0.05)
    updateNumericInput(session, "af_weight", value = 0.18)
  }, ignoreInit = TRUE)
  
  
  #' this function rescales all non-zero weights
  #' @param factor_weights numeric vector that holds the 10 factor weights
  #' @returns numeric vector that holds the rescaled 10 factor weights
  rescale_weights <- function(factor_weights) {
    # count non-zero weights
    count <- length(factor_weights[factor_weights > 0])
    
    # avoid zero division
    if(count == 0) {
      rescaled_weights <- rep(1 / length(factor_weights), length(factor_weights))
    } else {
      rescaled_weights <- factor_weights
      rem <- (1.000 - sum_weights()) / count
      rescaled_weights[factor_weights > 0] <- factor_weights[factor_weights > 0] + rem
    }
    return(rescaled_weights)
  }
  
  # add popover to rescale button
  addPopover(session, "rescale", title = "What happens here?",
             placement = "top",
             content = "Rescaling the weights will distribute the remainder
             evenly to all non-zero weights.")
  
  # rescale weights if button is pressed
  observeEvent(input$rescale, {
    # store factor weights
    factor_weights <- reactiveVal(c(comm_weight(), rec_weight(),
                                    tribal_weight(), cd_weight(),
                                    reb_weight(), ss_weight(),
                                    fm_weight(), eco_weight(),
                                    ni_weight(), af_weight()))
    
    # update factor weights
    factor_weights(rescale_weights(factor_weights()))
    updateNumericInput(session, "comm_weight", value = round(factor_weights()[1], 4))
    updateNumericInput(session, "rec_weight", value = round(factor_weights()[2], 4))
    updateNumericInput(session, "tribal_weight", value = round(factor_weights()[3], 4))
    updateNumericInput(session, "cd_weight", value = round(factor_weights()[4], 4))
    updateNumericInput(session, "reb_weight", value = round(factor_weights()[5], 4))
    updateNumericInput(session, "ss_weight", value = round(factor_weights()[6], 4))
    updateNumericInput(session, "fm_weight", value = round(factor_weights()[7], 4))
    updateNumericInput(session, "eco_weight", value = round(factor_weights()[8], 4))
    updateNumericInput(session, "ni_weight", value = round(factor_weights()[9], 4))
    updateNumericInput(session, "af_weight", value = round(factor_weights()[10], 4))
  }, ignoreInit = TRUE)
  
  
  # create reactive dataframe (used for overall table + plot)
  overall_data <- reactive({
    overall_data <- data.frame(
      species = results()$species, #1
      rank = NA, #2
      last_assessed = year_assessed(), #3
      total = NA, #4
      com_rev_fs_weight = results()$com_rev_fs * comm_weight(), #5
      rec_fs_weight = results()$rec_fs* rec_weight(), #6
      tribal_fs_weight = results()$tribal_fs * tribal_weight(), #7
      const_dem_fs_weight = results()$const_dem_fs * cd_weight(), #8
      reb_fs_weight = results()$reb_fs * reb_weight(), #9
      ss_fs_weight = results()$ss_fs * ss_weight(), #10
      fish_mort_fs_weight = results()$fish_mort_fs * fm_weight(), #11
      eco_fs_weight = results()$eco_fs * eco_weight(), #12
      new_info_fs_weight = results()$new_info_fs * ni_weight(), #13
      assess_fs_weight = results()$assess_fs * af_weight() #14
    )
    # create column with weighted sum
    overall_data$total <- rowSums(overall_data[,5:ncol(overall_data)])
    
    overall_data <- overall_data |>
      arrange(desc(total))
    
    # create rank column
    #overall_data$rank <- NA
    order_totals <- order(overall_data$total, overall_data$species, 
                          decreasing = TRUE)
    overall_data$rank[order_totals] <- 1:nrow(overall_data)

    # rename columns in table
    colnames(overall_data) <- c("Species", "Rank", "Last Assessed", "Weighted Total Score",
                           "Commercial Importance", "Recreational Importance",
                           "Tribal Importance", "Constituent Demand", "Rebuilding",
                           "Stock Status", "Fishing Mortality", "Ecosystem",
                           "New Information", "Assessment Frequency")
    overall_data
  })
  
  
  # overall ranking table
  output$overall_gt_table <- renderUI({
    req(overall_data)
    
    # create table if weights sum to 1
    if(sum_weights() == 1.00) {
      overall_gt_table <- overall_data() |>
        gt() |>
        tab_header(
          title = "Overall Factor Summary",
          subtitle = "The weighted score using the specified weights (shown above) for each factor and the sum of all weighted factors (Weighted Total Score) by species to determine overall rank."
        ) |>
        tab_options(
          heading.subtitle.font.size = 14,
          footnotes.font.size = 14
        ) |>
        cols_label(
          `Commercial Importance` = "Comm. Importance Factor Score",
          `Recreational Importance` = "Rec. Importance Factor Score",
          `Tribal Importance` = "Tribal Importance Factor Score",
          `Constituent Demand` = "Const. Demand Factor Score",
          Rebuilding = "Rebuilding Factor Score",
          `Stock Status` = "Stock Status Factor Score",
          `Fishing Mortality` = "Fishing Mortality Factor Score",
          Ecosystem = "Ecosystem Factor Score",
          `New Information` = "New Information Factor Score",
          `Assessment Frequency` = "Assmt. Frequency Factor Score"
        ) |>
        fmt_number(columns = -c("Rank"), decimals = 2) |>
        tab_style(style = list(cell_text(weight = "bold")),
                  locations = cells_body(columns = c("Species", "Rank"))
        ) |>
        tab_style(style = list(cell_text(weight = "bold")),
                  locations = cells_body(columns = `Weighted Total Score`)) |>
        opt_interactive(use_search = TRUE,
                        use_highlight = TRUE,
                        use_page_size_select = TRUE) |>
        data_color(columns = 4:14, method = "numeric",
                   palette = "Greys", reverse = TRUE)
      
      overall_gt_table
    }
  })
  
  # download overall ranking table
  ## as csv
  output$overall_csv <- downloadHandler(
    filename = function() {
      paste("overall_ranking_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(overall_data(), file)
    }
  )
  
  ## as excel spreadsheet
  output$overall_xlsx <- downloadHandler(
    filename = function() {
      paste("overall_ranking_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(overall_data(), file = file)
    }
  )
  
  # as R object
  output$overall_rds <- downloadHandler(
    filename = function() {
      paste("overall_ranking_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(overall_data(), file = file)
    }
  )

  # overall ranking plot
  output$overall_ranking <- renderPlotly({
    req(overall_data)
    
    if(sum_weights() == 1.00) {
      # reshape dataframe
      for_plot <- overall_data() |>
        pivot_longer(
          cols = `Commercial Importance`:`Assessment Frequency`,
          names_to = "factor",
          values_to = "score"
        )
      
      for_plot$rank_species <- paste0(for_plot$Rank, ". ", for_plot$Species)
      
      if(input$num_col == "10" | input$num_col == "20") {
        top_species <- head(for_plot, as.numeric(input$num_col) * 10)
      } else if(input$num_col == "21-40") {
        top_species <- for_plot[201:400, ]
      } else if(input$num_col == "41-60") {
        top_species <- for_plot[401:600, ]
      } else {
        top_species <- for_plot[601:650, ]
      }
      
      overall_plot <- ggplot(top_species, aes(x = reorder(rank_species, score, sum),
                                              y = score,
                                              fill = factor,
                                              text = paste0("Species: ", Species,
                                                            "\nFactor: ", factor,
                                                            "\nWt'd. Factor Score: ",
                                                            paste0(round(score, digits = 2),
                                                                   " (",
                                                                  round((score / `Weighted Total Score`) * 100,
                                                                        digits = 1),
                                                                  "%)"),
                                                            "\nTotal Wt'd. Factor Score: ",
                                                            round(`Weighted Total Score`, digits = 2)))
        ) +
        geom_col(color = "white") +
        coord_flip() +
        labs(
          title = "Overall Fish Species Ranking",
          y = "Overall Weighted Factor Score", x = "Species",
          fill = "Factors"
        ) +
        theme_light() +
        scale_fill_viridis_d()
      
      final_fig <- ggplotly(overall_plot, tooltip = "text")
      final_fig <- final_fig |>
        layout(
          xaxis = list(range = c(0, max(for_plot$`Weighted Total Score`) + 0.5),
                       tickmode = "linear",
                       dtick = 1)
        )
      
      final_fig
    }
  })
  

  # create reactive dataframe (commercial importance)
  reactive_com_df <- reactive({
    req(com_rev_data)
    # filter data down to selected species + columns
    reactive_com_df <- com_rev_data()[com_rev_data()$`Management Group` %in% input$com_species_selector,]
    reactive_com_df <- com_rev_data() |>
      select("Species", input$com_columns)
    
    reactive_com_df
  })
  
  # commercial importance table
  output$com_gt_table <- render_gt({
    req(reactive_com_df)
  
    com_table <- reactive_com_df() |>
      gt() |>
      tab_header(
        title = "Commercial Importance",
        subtitle = paste0("Measured by total inflation adjusted ex-vessel revenue data ($1,000)
        between ", text_recent_five_years(), ".")
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$com_colors) {
      if(i %in% input$com_columns) {
        if(i == "Rank") {
          com_table <- com_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          com_table <- com_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Rank" %in% input$com_columns) {
      com_table <- com_table |>
        fmt_number(columns = -c("Rank"), decimals = 2)
    } else {
      com_table <- com_table |>
        fmt_number(columns = everything(), decimals = 2)
    }
    
    com_table |>
      fmt_currency(columns = contains("Revenue"), decimals = 0) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      tab_footnote(footnote = "Source: PacFIN") |>
      tab_footnote(footnote = "") |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download com ranking table
  ## as csv
  output$com_csv <- downloadHandler(
    filename = function() {
      paste("commercial_importance_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_com_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$com_xlsx <- downloadHandler(
    filename = function() {
      paste("commercial_importance_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_com_df(), file = file)
    }
  )
  
  ## as R object
  output$com_rds <- downloadHandler(
    filename = function() {
      paste("commercial_importance_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write.csv(reactive_com_df(), file = file)
    }
  )
  
  
  #' function to create base species ranking plot
  #' @param df a factor dataframe
  #' @returns lollipop plot without labels
  create_base_plot <- function(df) {
    base_plot <- ggplot(df, aes(x = Species, y = Rank,
                                text = paste0("Species: ", Species,
                                              "\nRank: ", Rank,
                                              "\nFactor Score: ",
                                              round(`Factor Score`, digits = 2),
                                              "\nManagement Group: ", `Management Group`))
    ) +
      geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                   color = "gray") +
      geom_hline(yintercept = 65, color = "gray") +
      geom_point(aes(color = `Management Group`), size = 3) +
      scale_y_reverse() +
      theme_light() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(), panel.border = element_blank()) +
      scale_color_viridis(discrete = TRUE)
    
    return(base_plot)
  }
  
  # commercial importance species ranking plot
  output$com_ranking <- renderPlotly({
    req(com_rev_data)
    
    com_plot <- create_base_plot(com_rev_data()) +
      labs(
        title = "Fish Species Ranking by Commercial Importance",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group")
    
    ggplotly(com_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (recreational importance)
  reactive_rec_df <- reactive({
    # filter data down to selected species + columns
    reactive_rec_df <- rec_data()[rec_data()$`Management Group` %in% input$rec_species_selector,]
    reactive_rec_df<- rec_data() |>
      select("Species", input$rec_columns)
      
    reactive_rec_df
  })
  
  # recreational importance table
  output$rec_gt_table <- render_gt({
    req(rec_data)
    
    rec_table <- reactive_rec_df() |>
      gt() |>
      tab_header(
        title = "Recreational Importance",
        subtitle = paste0("Measured by total recreational catch between ", text_recent_five_years(), ".")
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$rec_colors) {
      if(i %in% input$rec_columns) {
        if(i == "Rank") {
          rec_table <- rec_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          rec_table <- rec_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    rec_table <- rec_table |>
      fmt_number(columns = -c("Rank"), decimals = 2) |>   
      tab_footnote(paste0("Source: GEMM ", text_recent_five_years(), ".")) |>
      tab_footnote(footnote = "Recreational catches are not calculated
                   for petrale sole and widow rockfish in Washington.",
                   locations = cells_column_labels(columns = `Factor Score`)
      ) 

    rec_table |>
      fmt_currency(columns = contains("Revenue"), decimals = 0) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download rec ranking table
  ## as csv
  output$rec_csv <- downloadHandler(
    filename = function() {
      paste("recreational_importance_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_rec_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$rec_xlsx <- downloadHandler(
    filename = function() {
      paste("recreational_importance_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_rec_df(), file = file)
    }
  )
  
  ## as R object
  output$rec_rds <- downloadHandler(
    filename = function() {
      paste("recreational_importance_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_rec_df(), file = file)
    }
  )
  
  # recreational importance species ranking plot
  output$rec_species_ranking <- renderPlotly({
    req(rec_data)
    
    rec_plot <- create_base_plot(rec_data()) +
      labs(
        title = "Fish Species Ranking by Recreational Importance",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group")
    
    ggplotly(rec_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (tribal importance)
  reactive_tribal_df <- reactive({
    # filter data down to selected species + columns
    reactive_tribal_df <- tribal_data()[tribal_data()$`Management Group` %in% input$tribal_species_selector,]
    reactive_tribal_df <- tribal_data() |>
      select("Species", input$tribal_columns)
    
    reactive_tribal_df
  })
  
  # tribal revenue table
  output$tribal_gt_table <- render_gt({
    req(tribal_data)
    
    tribal_table <- reactive_tribal_df() |>
      gt() |>
      tab_header(
        title = "Tribal Importance",
        subtitle = paste0("Measured by total inflation adjusted ex-vessel revenue data for tribal landings
        between ", text_recent_five_years(), ".")
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$tribal_colors) {
      if(i %in% input$tribal_columns) {
        if(i == "Rank") {
          tribal_table <- tribal_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          tribal_table <- tribal_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Rank" %in% input$tribal_columns) {
      tribal_table <- tribal_table |>
        fmt_number(columns = -c("Rank"), decimals = 2)
    } else {
      tribal_table <- tribal_table |>
        fmt_number(columns = everything(), decimals = 2)
    }
    
    tribal_table |>
      fmt_currency(columns = contains("Revenue"), decimals = 0) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      tab_footnote(footnote = "Source: PacFIN") |>
      tab_footnote(footnote = "") |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download tribal ranking table
  ## as csv
  output$tribal_csv <- downloadHandler(
    filename = function() {
      paste("tribal_importance_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_tribal_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$tribal_xlsx <- downloadHandler(
    filename = function() {
      paste("tribal_importance_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_tribal_df(), file = file)
    }
  )
  
  ## as R object
  output$tribal_rds <- downloadHandler(
    filename = function() {
      paste("tribal_importance_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_tribal_df(), file = file)
    }
  )
  
  # tribal importance species ranking plot
  output$tribal_species_ranking <- renderPlotly({
    req(tribal_data)
    
    tribal_plot <- create_base_plot(tribal_data()) +
      labs(
        title = "Fish Species Ranking by Tribal Importance",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group")
    
    ggplotly(tribal_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (constituent demand)
  reactive_cd_df <- reactive({
    # filter data down to selected species + columns
    reactive_cd_df <- const_dem_data()[const_dem_data()$`Management Group` %in% input$cd_species_selector,]
    reactive_cd_df <- const_dem_data() |>
      select("Species", input$cd_columns)
    
    reactive_cd_df
  })
  
  # constituent demand table
  ## negative scores adjusted
  output$cd_gt_table <- render_gt({
    req(const_dem_data)
    
    # create recreational gt table output, display in ascending order by rank
    cd_table <- reactive_cd_df() |>
      gt() |>
      tab_header(
        title = "Constituent Demand",
        subtitle = "Measured by projected future constraints given current average catch
        compared to anticipated future ACLs, for species with greater important in a 
        particular state compared to coastwide, and commercial species that are important 
        to both trawl and fixed gear fisheries."
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$cd_colors) {
      if(i %in% input$cd_columns) {
        if(i == "Rank") {
          cd_table <- cd_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          cd_table <- cd_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Projected ACL Attainment" %in% input$cd_columns) {
      cd_table <- cd_table |>
        tab_style(style = cell_text(color = "red", weight = "bold"),
                  locations = cells_body(
                    columns = `Projected ACL Attainment`,
                    rows = `Projected ACL Attainment` > 1.00
                  )
        ) |>
        tab_footnote(footnote = paste0("Source: GEMM ", text_recent_five_years(), " and PacFIN APEX GMT008"),
                     locations = cells_column_labels(columns = `Projected ACL Attainment`) 
        ) |>
        tab_footnote(footnote = "Cells with red text indicate
                     high projected ACL attainment percentages and cells with
                     italic text indicate ACL contributions.",
                     locations = cells_column_labels(columns = `Projected ACL Attainment`)
        )
    }
    
    if("Projected ACL Attainment" %in% input$cd_columns &
       "Management Group" %in% input$cd_columns) {
      cd_table <- cd_table |>
        tab_style(style = cell_text(style = "italic"),
                  locations = cells_body(
                    columns = `Projected ACL Attainment`,
                    rows = `Management Group` != "species specific"
                  )
        ) 
    }
    
    cd_table |>
      fmt_percent(columns = contains("Attainment"), decimals = 1,
                  scale_values = TRUE) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download cd ranking table
  ## as csv
  output$cd_csv <- downloadHandler(
    filename = function() {
      paste("constituent_demand_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_cd_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$cd_xlsx <- downloadHandler(
    filename = function() {
      paste("constituent_demand_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_cd_df(), file = file)
    }
  )
  
  ## as R object
  output$cd_rds <- downloadHandler(
    filename = function() {
      paste("constituent_demand_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_cd_df(), file = file)
    }
  )
  
  # constituent demand species ranking plot
  output$cd_species_ranking <- renderPlotly({
    req(const_dem_data)
    
    cd_plot <- create_base_plot(const_dem_data()) +
      labs(
        title = "Fish Species Ranking by Constituent Demand",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group")
    
    ggplotly(cd_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (rebuilding)
  reactive_reb_df <- reactive({
    reactive_reb_df <- rebuilding_data()[rebuilding_data()$`Management Group` %in% input$reb_species_selector,]
    reactive_reb_df <- rebuilding_data() |>
      select("Species", input$reb_columns)
    
    reactive_reb_df
  })
  
  # rebuilding table
  output$reb_gt_table <- render_gt({
    req(rebuilding_data)

    reb_table <- reactive_reb_df() |>
      gt() |>
      tab_header(
        title = "Rebuilding",
        subtitle = "Accounts for species that are overfished, recent trends in the 
        abundance, and the anticipated rebuilding time."
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$reb_colors) {
      if(i %in% input$reb_columns) {
        if(i == "Rank") {
          reb_table <- reb_table |>
            data_color(columns = Rank, method = "auto", palette = "viridis",
                       reverse = TRUE)
        } else {
          reb_table <- reb_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    reb_table |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download reb ranking table
  ## as csv
  output$reb_csv <- downloadHandler(
    filename = function() {
      paste("rebuilding_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_reb_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$reb_xlsx <- downloadHandler(
    filename = function() {
      paste("rebuilding_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_reb_df(), file = file)
    }
  )
  
  ## as R object
  output$reb_rds <- downloadHandler(
    filename = function() {
      paste("rebuilding_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_reb_df(), file = file)
    }
  )
  
  # rebuilding species ranking plot - uses rebuilding score
  output$reb_species_ranking <- renderPlotly({
    req(rebuilding_data)
  
    reb_plot <- ggplot(rebuilding_data(), aes(x = Species, y = `Factor Score`,
                                          text = paste0("Species: ", Species,
                                                        "\nFactor Score: ",
                                                        round(`Factor Score`, digits = 2),
                                                        "\nManagement Group: ", `Management Group`))
      ) +
      geom_segment(aes(x = Species, xend = Species, y = 0, yend = `Factor Score`),
                   color = "gray") +
      geom_point(aes(color = `Management Group`), size = 3) +
      ylim(NA, 10) +
      labs(
        title = "Fish Species Ranking by Rebuilding",
        x = "Species (in alphabetical order)", y = "Rebuilding Score", color = "Management Group") +
      theme_light() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(), panel.border = element_blank()) +
      scale_color_viridis(discrete = TRUE)
    
    ggplotly(reb_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (stock status)
  reactive_ss_df <- reactive({
    reactive_ss_df <- stock_stat_data()[stock_stat_data()$`Management Group` %in% input$ss_species_selector,]
    reactive_ss_df <- stock_stat_data() |>
      select("Species", input$ss_columns)
    
    reactive_ss_df  
  })
  
  # stock status table
  output$ss_gt_table <- render_gt({
    req(stock_stat_data)
    
    ss_table <- reactive_ss_df() |>
      gt() |>
      tab_header(
        title = "Stock Status",
        subtitle = "Measured by the estimated fraction unfished at the time of the most
        recent assessment or the PSA score for un-assessed species"
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    # reverse color scale for Fraction_Unfished?
    for(i in input$ss_colors) {
      if(i %in% input$ss_columns) {
        if(i == "Rank") {
          ss_table <- ss_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          ss_table <- ss_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Fraction Unfished" %in% input$ss_columns) {
      ss_table <- ss_table |>
        fmt_percent(columns = `Fraction Unfished`, decimals = 1)
    }
    
    if("Target Fraction Unfised" %in% input$ss_columns) {
      ss_table <- ss_table |>
        fmt_percent(columns = `Target Fraction Unfised`, decimals = 1)
    }
    
    if("MSST" %in% input$ss_columns) {
      ss_table <- ss_table |>
        fmt_percent(columns = MSST, decimals = 1)
    }
    
    ss_table |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download ss ranking table
  ## as csv
  output$ss_csv <- downloadHandler(
    filename = function() {
      paste("stock_status_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_ss_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$ss_xlsx <- downloadHandler(
    filename = function() {
      paste("stock_status_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_ss_df(), file = file)
    }
  )
  
  ## as R object
  output$ss_rds <- downloadHandler(
    filename = function() {
      paste("stock_status_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_ss_df(), file = file)
    }
  )
  
  # stock status species ranking plot
  output$ss_species_ranking <- renderPlotly({
    req(stock_stat_data)
    
    ss_plot <- ggplot(stock_stat_data(), aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(`Factor Score`, digits = 2),
                                                      "\nManagement Group: ", `Management Group`))
      ) +
      geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                   color = "gray") +
      geom_hline(yintercept = 65, color = "gray") +
      geom_point(aes(color = `Management Group`), size = 3) +
      scale_y_reverse() +
      labs(
        title = "Fish Species Ranking by Stock Status",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
      theme_light() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(), panel.border = element_blank()) +
      scale_color_viridis(discrete = TRUE)
    
    ggplotly(ss_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (fishing mortality)
  reactive_fm_df <- reactive({
    # filter data down to selected species + columns
    reactive_fm_df <- fish_mort_data()[fish_mort_data()$`Management Group` %in% input$fm_species_selector,]
    reactive_fm_df <- fish_mort_data() |>
      select("Species", input$fm_columns)
    
    reactive_fm_df
  })
  
  # fishing mortality table
  ## footnotes disappear if less than 2
  output$fm_gt_table <- render_gt({
    req(fish_mort_data)
      
    fm_table <- reactive_fm_df() |>
      gt() |>
      tab_header(
        title = "Fishing Mortality",
        subtitle = paste0("Measured by average OFLs and average catch between ", text_recent_five_years(), ".")
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      ) |>
      tab_footnote(
        footnote = "Source: GEMM"
      )
      
      
    for(i in input$fm_colors) {
      if(i %in% input$fm_columns) {
        if(i == "Rank") {
          fm_table <- fm_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          fm_table <- fm_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
      
    if("Average OFL Attainment" %in% input$fm_columns) {
      fm_table <- fm_table |>
        tab_style(style = cell_text(color = "red", weight = "bold"),
                  locations = cells_body(
                    columns = `Average OFL Attainment`,
                    rows = `Average OFL Attainment` > 1.00
                  )
        ) |>
        tab_footnote(footnote = "Cells with red text indicate
                     high OFL attainment percentages.",
                     locations = cells_column_labels(columns = `Average OFL Attainment`))
    }
      
    if("Average OFL" %in% input$fm_columns &
       "Management Group" %in% input$fm_columns) {
      fm_table <- fm_table |>
        tab_style(style = cell_text(style = "italic"),
                  locations = cells_body(
                    columns = `Average OFL`,
                    rows = `Management Group` != "species specific"
                  )
        ) |>
        tab_footnote(footnote = "Cells with italic text indicate OFL contributions.",
                     locations = cells_column_labels(columns = `Average OFL`)
        )
    }
    
    fm_table |>
      fmt_number(columns = contains("Average"), decimals = 2) |>
      fmt_percent(columns = contains("Attainment"), decimals = 1) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download fm ranking table
  ## as csv
  output$fm_csv <- downloadHandler(
    filename = function() {
      paste("fishing_mortality_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_fm_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$fm_xlsx <- downloadHandler(
    filename = function() {
      paste("fishing_mortality_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_fm_df(), file = file)
    }
  )
  
  ## as R object
  output$fm_rds <- downloadHandler(
    filename = function() {
      paste("fishing_mortality_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_fm_df(), file = file)
    }
  )
  
  # fishing mortality species ranking plot
  output$fm_species_ranking <- renderPlotly({
    req(fish_mort_data)
    
    fm_plot <- create_base_plot(fish_mort_data()) +
      labs(
        title = "Fish Species Ranking by Fishing Mortality",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group")
    
    ggplotly(fm_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (ecosystem)
  reactive_eco_df <- reactive({
    reactive_eco_df <- eco_data()[eco_data()$`Management Group` %in% input$eco_species_selector,]
    reactive_eco_df <- eco_data() |>
      select("Species", input$eco_columns)
    
    reactive_eco_df
  })
  
  # ecosystem table
  output$eco_gt_table <- render_gt({
    req(eco_data)
  
    eco_table <- reactive_eco_df() |>
      gt() |>
      tab_header(
        title = "Ecosystem",
        subtitle = "Measures importance of species to the trophic cynamics of the 
        California Current ecosystem based on an Ecopath model."
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$eco_colors) {
      if(i %in% input$eco_columns) {
        if(i == "Rank") {
          eco_table <- eco_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          eco_table <- eco_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    if("Factor Score" %in% input$eco_columns) {
      eco_table <- eco_table |>
        fmt_number(columns = `Factor Score`, decimals = 2)
    }
    
    eco_table |>
      fmt_number(columns = ends_with("Score"), decimals = 2) |>
      data_color(columns = Rank, method = "numeric", palette = "viridis",
                 reverse = TRUE) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download eco ranking table
  ## as csv
  output$eco_csv <- downloadHandler(
    filename = function() {
      paste("ecosystem_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_eco_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$eco_xlsx <- downloadHandler(
    filename = function() {
      paste("ecosystem_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_eco_df(), file = file)
    }
  )
  
  ## as R object
  output$eco_rds <- downloadHandler(
    filename = function() {
      paste("ecosystem_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_eco_df(), file = file)
    }
  )
  
  # ecosystem species ranking plot
  output$eco_species_ranking <- renderPlotly({
    req(eco_data)
    
    eco_plot <- create_base_plot(eco_data()) +
      labs(
        title = "Fish Species Ranking by Ecosystem",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group")
    
    ggplotly(eco_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (new information)
  reactive_ni_df <- reactive({
    reactive_ni_df <- new_info_data()[new_info_data()$`Management Group` %in% input$ni_species_selector,]
    reactive_ni_df <- new_info_data() |>
      select("Species", input$ni_columns)
    
    reactive_ni_df
  })
  
  # new information table
  output$ni_gt_table <- render_gt({
    req(new_info_data)
  
    ni_table <- reactive_ni_df() |>
      gt() |>
      tab_header(
        title = "New Information", 
        subtitle = "Measured by known available on ongoing research and data available in 
        the Northwest Fisheries Science Center West Coast Groundfish Bottom Trawl and 
        Hook-and-Line Surveys since the most recent assessment by species."
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$ni_colors) {
      if(i %in% input$ni_columns) {
        if(i == "Rank") {
          ni_table <- ni_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else if(i == "Year Last Assessed") {
          ni_table <- ni_table |>
            data_color(columns = `Year Last Assessed`, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          ni_table <- ni_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    ni_table |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download ni ranking table
  ## as csv
  output$ni_csv <- downloadHandler(
    filename = function() {
      paste("new_information_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_ni_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$ni_xlsx <- downloadHandler(
    filename = function() {
      paste("new_information_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_ni_df(), file = file)
    }
  )
  
  ## as R object
  output$ni_rds <- downloadHandler(
    filename = function() {
      paste("new_information_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_ni_df(), file = file)
    }
  )
  
  # new information species ranking plot
  output$ni_species_ranking <- renderPlotly({
    req(new_info_data)
    
    ni_plot <- ggplot(new_info_data(), aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(`Factor Score`, digits = 2),
                                                      "\nManagement Group: ", `Management Group`))
      ) +
      geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                   color = "gray") +
      geom_hline(yintercept = 65, color = "gray") +
      geom_point(aes(color = `Management Group`), size = 3) +
      scale_y_reverse() +
      labs(
        title = "Fish Species Ranking by New Information",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
      theme_light() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(), panel.border = element_blank()) +
      scale_color_viridis(discrete = TRUE)
    
    ggplotly(ni_plot, tooltip = "text")
  })
  
  
  # create reactive dataframe (assessment frequency)
  reactive_af_df <- reactive({
    reactive_af_df <- assess_freq_data()[assess_freq_data()$`Management Group` %in% input$af_species_selector,]
    reactive_af_df <- assess_freq_data() |>
      select("Species", input$af_columns)
      
    reactive_af_df
  })
  
  # assessment frequency table
  output$af_gt_table <- render_gt({
    req(assess_freq_data)
  
    af_table <- reactive_af_df() |>
      gt() |>
      tab_header(
        title = "Assessment Frequency",
        subtitle = "Accounts for the target assessment frequency based on the biology, 
        the time since the last assessment, and 
        whether that time is greater than the target assessment frequency and or greater 
        than ten years."
      ) |>
      tab_options(
        heading.subtitle.font.size = 14,
        footnotes.font.size = 14
      )
    
    for(i in input$af_colors) {
      if(i %in% input$af_columns) {
        if(i == "Rank") {
          af_table <- af_table |>
            data_color(columns = Rank, method = "numeric", palette = "viridis", reverse = TRUE)
        } else if(i == "Last Assessment Year") {
          af_table <- af_table |>
            data_color(columns = `Last Assessment Year`, method = "numeric", palette = "viridis",
                       reverse = TRUE)
        } else {
          af_table <- af_table |>
            data_color(columns = i, method = "auto", palette = "viridis")
        }
      }
    }
    
    af_table |>
      fmt_number(columns = contains("Age"), decimals = 2) |>
      tab_style(style = list(cell_text(weight = "bold")),
                locations = cells_body(columns = Species)) |>
      opt_interactive(use_search = TRUE,
                      use_highlight = TRUE,
                      use_page_size_select = TRUE)
  })
  
  # download af ranking table
  ## as csv
  output$af_csv <- downloadHandler(
    filename = function() {
      paste("assessment_frequency_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactive_af_df(), file)
    }
  )
  
  ## as excel spreadsheet
  output$af_xlsx <- downloadHandler(
    filename = function() {
      paste("assessment_frequency_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(reactive_af_df(), file = file)
    }
  )
  
  ## as R object
  output$af_rds <- downloadHandler(
    filename = function() {
      paste("assessment_frequency_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      write_rds(reactive_af_df(), file = file)
    }
  )
  
  # assessment frequency species ranking plot
  output$af_species_ranking <- renderPlotly({
    req(assess_freq_data)
    
    af_plot <- ggplot(assess_freq_data(), aes(x = Species, y = Rank,
                                        text = paste0("Species: ", Species,
                                                      "\nRank: ", Rank,
                                                      "\nFactor Score: ",
                                                      round(`Factor Score`, digits = 2),
                                                      "\nManagement Group: ", `Management Group`))
      ) +
      geom_segment(aes(x = Species, xend = Species, y = Rank, yend = 65),
                   color = "gray") +
      geom_hline(yintercept = 65, color = "gray") +
      geom_point(aes(color = `Management Group`), size = 3) +
      scale_y_reverse() +
      labs(
        title = "Fish Species Ranking by Fishing Mortality",
        x = "Species (in alphabetical order)", y = "Rank", color = "Management Group") +
      theme_light() +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(), panel.border = element_blank()) +
      scale_color_viridis(discrete = TRUE)
    
    ggplotly(af_plot, tooltip = "text")
  })
  
})