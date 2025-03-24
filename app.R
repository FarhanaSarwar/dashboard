options(repos = c(CRAN = "https://cloud.r-project.org"))

library(shinycssloaders)

library(shiny)
library(dplyr)
library(readxl)
library(writexl)
library(sf)
library(leaflet)
library(VIM)
library(DT)
library(purrr)
library(tidyr)
library(ggplot2)

# Function to rescale values to 0-10
rescale_to_0_10 <- function(x) {
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  if (x_max == x_min) {
    return(rep(5, length(x)))  # If all values are the same, return mid-scale value
  } else {
    return(10 * (x - x_min) / (x_max - x_min))
  }
}

# Data processing functions moved outside reactive expressions
process_data <- function(data, missing_threshold = 0.7, merge_var = NULL, general_desc_vars = NULL) {
  missing_ratio <- colSums(is.na(data)) / nrow(data)
  valid_vars <- names(missing_ratio[missing_ratio <= missing_threshold])
  # Ensure the merge variable is retained
  if (!is.null(merge_var) && !(merge_var %in% valid_vars)) {
    valid_vars <- c(valid_vars, merge_var)
  }
  data <- data %>% select(all_of(valid_vars))
  
  # Variables to impute are those in data excluding general_desc_vars
  vars_to_impute <- setdiff(names(data), general_desc_vars)
  
  # Apply kNN imputation only to vars_to_impute
  data_imputed <- kNN(data, variable = vars_to_impute, imp_var = FALSE)
  
  return(data_imputed)
}

# Function for directional adjustment
directional_adjustment <- function(data, inverse_variables) {
  for (var in inverse_variables) {
    if (var %in% names(data)) {
      data[[var]] <- max(data[[var]], na.rm = TRUE) - data[[var]]
    }
  }
  return(data)
}

# Function for population-based normalization
normalize_population <- function(data, population_col, size_adjusted_vars) {
  for (var in size_adjusted_vars) {
    if (var %in% names(data)) {
      data[[var]] <- data[[var]] / data[[population_col]]
    }
  }
  return(data)
}

# UI for the Shiny app with sliders for weighting
ui <- fluidPage(
  titlePanel("Vulnerability Index Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # New action button for default Colombia data
      actionButton("use_colombia", "Use Colombia Data"),
      br(), br(),
      
      fileInput("data_file", "Upload Data File (Excel)", accept = c(".xlsx")),
      fileInput("shapefile", "Upload Shapefile (.zip)", accept = c(".zip")),
      
      # Merge variables UI: defaults will be automatically selected if "Use Colombia Data" is clicked.
      uiOutput("merge_var_ui"),
      
      # Population variable UI with automatic default for Colombia data.
      uiOutput("population_var_ui"),
      
      selectInput("transformation", "Select Transformation Method:",
                  choices = c("Percentile", "Min-Max", "Z-score", "Quintile", "Geometric", "Equal Interval"),
                  selected = NULL),
      
      selectInput("score_type", "Select Score to Display:",
                  choices = NULL, selected = "Vulnerability Index"),
      
      uiOutput("popup_vars_ui"),
      
      # Dynamically create sliders for dimension weights
      uiOutput("dimension_sliders"),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", withSpinner(leafletOutput("colombia_map", width = "100%", height = "800px"), type = 6, color = "#03405c")),
        
        # Processed Data Tab with Download Button
        tabPanel("Processed Data",
                 downloadButton("download_processed_data", "Download Excel"),
                 br(), br(),
                 DT::dataTableOutput("processed_data")),
        
        # Transformed Data Tab with Download Button
        tabPanel("Transformed Data",
                 downloadButton("download_transformed_data", "Download Excel"),
                 br(), br(),
                 DT::dataTableOutput("transformed_data")
        ),
        
        # Index Scores Tab with Download Button
        tabPanel("Index Scores",
                 downloadButton("download_index_scores_data", "Download Excel"),
                 br(), br(),
                 DT::dataTableOutput("index_scores_data")
        ),
        
        # Shapefile Variables Tab with Download Button
        tabPanel("Shapefile Variables",
                 downloadButton("download_shapefile_vars", "Download Excel"),
                 br(), br(),
                 DT::dataTableOutput("shapefile_vars")
        ),
        tabPanel("Vulnerability Analysis",
                 fluidRow(
                   column(3,
                          uiOutput("child_population_var_ui"),
                          numericInput("threshold_proportion", "Threshold Proportion of Subregions to Select:",
                                       value = 0.1, min = 0.01, max = 1, step = 0.01)
                   ),
                   column(9,
                          withSpinner(plotOutput("pareto_scatter_plot"), type = 6, color = "#03405c"),
                          br(),
                          withSpinner(leafletOutput("vulnerable_regions_map"), type = 6, color = "#03405c")
                   )
                   
                 )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Define default file paths (relative to the app's root directory)
  default_data_file <- "data/colombia_data.xlsx"         # Excel file with sheets "Metadata" and "Data"
  default_shapefile_file <- "data/colombia_shapefile.zip"   # ZIP file containing the shapefile
  
  # Reactive expression to load metadata
  metadata_df <- reactive({
    if (input$use_colombia > 0) {
      if (!file.exists(default_data_file)) {
        stop("Default Colombia data file not found. Please place 'colombia_data.xlsx' in the 'data' folder.")
      }
      data_file_path <- default_data_file
    } else {
      req(input$data_file)
      data_file_path <- input$data_file$datapath
    }
    read_excel(data_file_path, sheet = "Metadata")
  })
  
  # Combined mapping functions
  field_mappings <- reactive({
    metadata <- metadata_df()
    list(
      field_to_description = setNames(metadata$FieldName_Description, metadata$FieldName),
      description_to_field = setNames(metadata$FieldName, metadata$FieldName_Description)
    )
  })
  
  # Reactive expression to load data columns
  data_columns <- reactive({
    if (input$use_colombia > 0) {
      if (!file.exists(default_data_file)) {
        stop("Default Colombia data file not found. Please place 'colombia_data.xlsx' in the 'data' folder.")
      }
      data_file_path <- default_data_file
    } else {
      req(input$data_file)
      data_file_path <- input$data_file$datapath
    }
    colnames(read_excel(data_file_path, sheet = "Data", n_max = 0))
  })
  
  # Reactive expressions to get variables from metadata
  size_adjusted_variables <- reactive({
    req(metadata_df())
    metadata_df() %>%
      filter(Size_Adjusted_Variables == "Yes") %>%
      pull(FieldName)
  })
  
  inverse_variables <- reactive({
    req(metadata_df())
    metadata_df() %>%
      filter(Inverse_Variables == "Yes") %>%
      pull(FieldName)
  })
  
  general_desc_vars <- reactive({
    req(metadata_df())
    metadata_df() %>%
      filter(Dimension == "General description") %>%
      pull(FieldName)
  })
  
  # Reactive expression for dimensions list
  dimensions_list <- reactive({
    req(metadata_df())
    metadata_df() %>%
      filter(Dimension != "General description") %>%
      group_by(Dimension) %>%
      summarise(variables = list(FieldName), .groups = 'drop')
  })
  
  # Reactive expression for dimensions
  dimensions <- reactive({
    setNames(dimensions_list()$variables, dimensions_list()$Dimension)
  })
  
  # UI for selecting merge variables with default selections for Colombia data
  output$merge_var_ui <- renderUI({
    req(data_columns(), map_data())
    
    # Create named vectors for choices
    data_choices <- setNames(data_columns(), field_mappings()$field_to_description[data_columns()])
    shapefile_choices <- setNames(names(map_data()), names(map_data()))
    
    # If "Use Colombia Data" is active, pre-select the defaults.
    selected_data <- if (input$use_colombia > 0) "Municipality_Code" else ""
    selected_shapefile <- if (input$use_colombia > 0) "MPIOS" else ""
    
    tagList(
      selectInput("merge_var_data", "Select Merge Variable in Data:", 
                  choices = c("", data_choices), selected = selected_data),
      selectInput("merge_var_shapefile", "Select Merge Variable in Shapefile:", 
                  choices = c("", shapefile_choices), selected = selected_shapefile)
    )
  })
  
  # UI for selecting total population variable with default for Colombia data
  output$population_var_ui <- renderUI({
    req(data_columns())
    req(size_adjusted_variables())
    
    if (length(size_adjusted_variables()) > 0) {
      data_choices <- setNames(data_columns(), field_mappings()$field_to_description[data_columns()])
      selected_pop <- if (input$use_colombia > 0) "TotalPop" else ""
      selectInput("population_var", "Select Total Population Variable for Normalization:", 
                  choices = c("", data_choices), selected = selected_pop)
    } else {
      return(NULL)
    }
  })
  
  # UI for selecting popup variables
  output$popup_vars_ui <- renderUI({
    req(data_columns(), map_data(), index_scores())
    data_choices <- setNames(data_columns(), field_mappings()$field_to_description[data_columns()])
    map_choices <- setNames(names(map_data()), names(map_data()))
    index_choices <- setNames(names(index_scores()), names(index_scores()))
    
    display_choices <- c(data_choices, map_choices, index_choices)
    
    selectInput("popup_vars", "Select Variables for Map Popup:",
                choices = display_choices, multiple = TRUE, selected = NULL)
  })
  
  # Reactive expressions for selected field names
  merge_var_data_field <- reactive({
    req(input$merge_var_data)
    if (input$merge_var_data %in% names(field_mappings()$description_to_field)) {
      field_mappings()$description_to_field[[input$merge_var_data]]
    } else {
      input$merge_var_data
    }
  })
  
  population_var_field <- reactive({
    req(input$population_var)
    if (input$population_var %in% names(field_mappings()$description_to_field)) {
      field_mappings()$description_to_field[[input$population_var]]
    } else {
      input$population_var
    }
  })
  
  popup_vars_fields <- reactive({
    # Allow for no selection (i.e., NULL or empty)
    if (is.null(input$popup_vars) || length(input$popup_vars) == 0) {
      return(NULL)
    }
    sapply(input$popup_vars, function(var) {
      if (var %in% names(field_mappings()$description_to_field)) {
        field_mappings()$description_to_field[[var]]
      } else {
        var  # Keep the variable name as is
      }
    }, USE.NAMES = FALSE)
  })
  
  # Reactive expression to load shapefile data
  map_data <- reactive({
    if (input$use_colombia > 0) {
      if (!file.exists(default_shapefile_file)) {
        stop("Default Colombia shapefile not found. Please place 'colombia_shapefile.zip' in the 'data' folder.")
      }
      shapefile_path <- default_shapefile_file
    } else {
      req(input$shapefile)
      shapefile_path <- input$shapefile$datapath
    }
    temp_dir <- tempdir()
    unzip(shapefile_path, exdir = temp_dir)
    shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_file) == 0) {
      stop("No .shp file found in the zip file.")
    }
    # Read shapefile
    shp <- st_read(shp_file[1], quiet = TRUE)
    
    # Check if CRS is defined
    if (is.na(st_crs(shp))) {
      stop("The shapefile has no coordinate reference system defined.")
    }
    
    # If CRS is not EPSG:4326, transform it
    if (!st_crs(shp) == st_crs(4326)) {
      shp <- st_transform(shp, crs = 4326)
    }
    return(shp)
  })
  
  # Reactive expression to process data
  data_processed <- reactive({
    if (input$use_colombia > 0) {
      if (!file.exists(default_data_file)) {
        stop("Default Colombia data file not found. Please place 'colombia_data.xlsx' in the 'data' folder.")
      }
      data_file_path <- default_data_file
    } else {
      req(input$data_file, merge_var_data_field())
      data_file_path <- input$data_file$datapath
    }
    data_raw <- read_excel(data_file_path, sheet = "Data")
    
    processed <- data_raw %>%
      process_data(merge_var = merge_var_data_field(), general_desc_vars = general_desc_vars()) %>%
      directional_adjustment(inverse_variables()) %>%
      mutate(across(all_of(merge_var_data_field()), as.character))
    
    # Conditionally perform population normalization
    if (length(size_adjusted_variables()) > 0) {
      req(population_var_field())
      processed <- processed %>%
        normalize_population(population_var_field(), size_adjusted_variables())
    }
    
    # Confirm the merge variable is present
    if (!(merge_var_data_field() %in% names(processed))) {
      stop("Merge variable is missing from processed data.")
    }
    
    return(processed)
  })
  
  # Variables to transform
  vars_to_transform <- reactive({
    metadata <- metadata_df()
    vars <- metadata %>% filter(Dimension != "General description") %>% pull(FieldName)
    vars <- intersect(vars, names(data_processed()))
    vars <- vars[sapply(data_processed()[vars], is.numeric)]
    vars
  })
  
  # Reactive expression for transformations
  transformed_data <- reactive({
    req(data_processed(), vars_to_transform())
    data <- data_processed()
    vars <- vars_to_transform()
    
    if (input$transformation == "Percentile") {
      data <- data %>% mutate(across(all_of(vars), percent_rank))
    } else if (input$transformation == "Min-Max") {
      data <- data %>% mutate(across(all_of(vars),
                                     ~(. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))))
    } else if (input$transformation == "Z-score") {
      data <- data %>% mutate(across(all_of(vars), ~ {
        sd_val <- sd(., na.rm = TRUE)
        if (sd_val == 0) 0 else (. - mean(., na.rm = TRUE)) / sd_val
      }))
    } else if (input$transformation == "Quintile") {
      data <- data %>% mutate(across(all_of(vars),
                                     ~ntile(., 5)))
    } else if (input$transformation == "Geometric") {
      data <- data %>% mutate(across(all_of(vars), ~ ifelse(. <= 0, NA, log(.))))
    } else if (input$transformation == "Equal Interval") {
      data <- data %>% mutate(across(all_of(vars),
                                     ~cut(., breaks = 5, labels = FALSE)))
    } else {
      # If no transformation method is selected, do nothing
      data <- data
    }
    
    return(data)
  })
  
  # Dynamically generate sliders for each dimension based on the metadata_df
  output$dimension_sliders <- renderUI({
    req(dimensions())
    slider_list <- list()
    
    for (dimension in names(dimensions())) {
      slider_list[[dimension]] <- sliderInput(
        paste0("weight_", dimension),
        label = paste("Weight for", dimension),
        min = 1, max = 10, value = 5
      )
    }
    do.call(tagList, slider_list)
  })
  
  # UI for selecting total child population variable
  output$child_population_var_ui <- renderUI({
    req(data_columns())
    data_choices <- setNames(data_columns(), field_mappings()$field_to_description[data_columns()])
    selectInput("child_population_var", "Select Population Variable:", choices = c("", data_choices), selected = NULL)
  })
  
  # Reactive expression for child population variable field
  child_population_var_field <- reactive({
    req(input$child_population_var)
    if (input$child_population_var %in% names(field_mappings()$description_to_field)) {
      field_mappings()$description_to_field[[input$child_population_var]]
    } else {
      input$child_population_var
    }
  })
  
  # Reactive expression for vulnerability analysis data
  vulnerability_analysis_data <- reactive({
    req(index_scores(), data_processed(), child_population_var_field())
    index_data <- index_scores()
    data_proc <- data_processed()
    child_pop_var <- child_population_var_field()
    merge_var <- merge_var_data_field()
    
    # Ensure that the merge variable is of the same type
    index_data[[merge_var]] <- as.character(index_data[[merge_var]])
    data_proc[[merge_var]] <- as.character(data_proc[[merge_var]])
    
    # Merge index_scores with data_processed to get the child population variable
    combined_data <- index_data %>%
      left_join(data_proc %>% select(all_of(c(merge_var, child_pop_var))), by = merge_var)
    
    # Remove rows with missing data
    combined_data <- combined_data %>% drop_na(all_of(c(child_pop_var, "Vulnerability Index")))
    
    # Return the combined data
    combined_data
  })
  
  # Reactive expression to compute Pareto frontier
  pareto_frontier <- reactive({
    req(vulnerability_analysis_data())
    data <- vulnerability_analysis_data()
    n <- nrow(data)
    dominated <- logical(n)
    v_index <- data$`Vulnerability Index`
    child_pop <- data[[child_population_var_field()]]
    
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          if ((v_index[j] >= v_index[i]) &&
              (child_pop[j] >= child_pop[i]) &&
              ((v_index[j] > v_index[i]) ||
               (child_pop[j] > child_pop[i]))) {
            dominated[i] <- TRUE
            break
          }
        }
      }
    }
    
    pareto_points <- data[!dominated, ]
    
    # Now, check if the number of Pareto points is less than the threshold number
    total_subregions <- n
    threshold_number <- ceiling(input$threshold_proportion * total_subregions)
    if (nrow(pareto_points) < threshold_number) {
      # Select additional subregions with higher vulnerability index
      remaining_subregions <- data[dominated, ]
      additional_subregions <- remaining_subregions %>%
        arrange(desc(`Vulnerability Index`)) %>%
        head(threshold_number - nrow(pareto_points))
      pareto_points <- bind_rows(pareto_points, additional_subregions)
    } else if (nrow(pareto_points) > threshold_number) {
      # If more than threshold number, select top ones based on vulnerability index
      pareto_points <- pareto_points %>%
        arrange(desc(`Vulnerability Index`)) %>%
        head(threshold_number)
    }
    
    pareto_points
  })
  
  # Render the Pareto scatter plot
  output$pareto_scatter_plot <- renderPlot({
    req(vulnerability_analysis_data(), pareto_frontier())
    data <- vulnerability_analysis_data()
    pareto_points <- pareto_frontier()
    child_pop_var <- child_population_var_field()
    merge_var <- merge_var_data_field()
    
    ggplot(data, aes(x = log10(.data[[child_pop_var]]), y = `Vulnerability Index`)) +
      geom_point() +
      geom_point(data = pareto_points, aes(x = log10(.data[[child_pop_var]]), y = `Vulnerability Index`), color = "red") +
      labs(x = paste("Log of", field_mappings()$field_to_description[[child_pop_var]]),
           y = "Vulnerability Index") +
      theme_minimal()
  })
  
  # Reactive expression for vulnerable map data
  vulnerable_map_data <- reactive({
    req(map_data_processed(), pareto_frontier(), merge_var_data_field(), input$merge_var_data, input$merge_var_shapefile)
    map_data <- map_data_processed()
    pareto_subregions <- pareto_frontier()
    merge_var_data <- merge_var_data_field()
    merge_var_shapefile <- input$merge_var_shapefile
    
    # Rename shapefile merge variable to match data merge variable
    map_data <- map_data %>%
      rename(!!!setNames(merge_var_shapefile, merge_var_data))
    
    # Ensure merge variables are character
    map_data[[merge_var_data]] <- as.character(map_data[[merge_var_data]])
    
    if (nrow(pareto_subregions) == 0) {
      showNotification("No vulnerable subregions found based on the current threshold.", type = "warning")
      return(NULL)
    }
    
    pareto_subregions[[merge_var_data]] <- as.character(pareto_subregions[[merge_var_data]])
    
    # Merge to get the vulnerable regions
    map_vulnerable <- map_data %>%
      inner_join(pareto_subregions, by = merge_var_data)
    
    map_vulnerable
  })
  
  # Render the map of vulnerable regions
  output$vulnerable_regions_map <- renderLeaflet({
    req(vulnerable_map_data())
    map_vulnerable <- vulnerable_map_data()
    
    if (is.null(map_vulnerable) || nrow(map_vulnerable) == 0) {
      leaflet() %>%
        addTiles() %>%
        addControl("No vulnerable regions to display.", position = "topright")
    } else {
      leaflet(map_vulnerable) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(color = "red", weight = 1, fillOpacity = 0.7,
                    popup = ~paste0(field_mappings()$field_to_description[[merge_var_data_field()]], ": ", get(merge_var_data_field()))) %>%
        addLayersControl(baseGroups = c("CartoDB.Positron"), position = "topright")
    }
  })
  
  # Reactive expression for index scores
  index_scores <- reactive({
    req(transformed_data(), merge_var_data_field())
    data_transformed <- transformed_data()
    
    dimension_scores <- list()
    dimension_weights <- list()
    
    for (dimension in names(dimensions())) {
      valid_vars <- dimensions()[[dimension]][dimensions()[[dimension]] %in% colnames(data_transformed)]
      
      # Calculate dimension scores and rescale to 0-10
      if (length(valid_vars) > 1) {
        raw_score <- rowMeans(data_transformed[, valid_vars], na.rm = TRUE)
      } else if (length(valid_vars) == 1) {
        raw_score <- data_transformed[[valid_vars]]
      } else {
        next  # Skip if no valid variables
      }
      dimension_scores[[dimension]] <- rescale_to_0_10(raw_score)
      
      weight_input <- input[[paste0("weight_", dimension)]]
      dimension_weights[[dimension]] <- ifelse(is.null(weight_input), 5, weight_input)
    }
    
    # Check if any dimension scores were calculated
    if (length(dimension_scores) == 0) {
      showNotification("No dimension scores available. Please check your data and metadata.", type = "error")
      return(NULL)
    }
    
    # Calculate weighted scores
    weighted_scores <- map2_dfc(dimension_scores, dimension_weights, ~ .x * .y)
    index_scores_df <- bind_cols(
      data_transformed %>% select(all_of(c(merge_var_data_field()))),
      dimension_scores
    )
    
    # Calculate weighted average of dimension scores
    total_weight <- sum(unlist(dimension_weights))
    index_scores_df$Vulnerability_Index <- rowSums(weighted_scores, na.rm = TRUE) / total_weight
    
    # Rescale the vulnerability index to 0-10
    index_scores_df$Vulnerability_Index <- rescale_to_0_10(index_scores_df$Vulnerability_Index)
    
    # Ensure merge variable is of character type
    index_scores_df[[merge_var_data_field()]] <- as.character(index_scores_df[[merge_var_data_field()]])
    
    # Rename the vulnerability index column for consistency
    index_scores_df <- index_scores_df %>%
      rename("Vulnerability Index" = "Vulnerability_Index")
    
    return(index_scores_df)
  })
  
  # Update score_type choices based on the dimensions from metadata_df
  observe({
    req(dimensions())
    dimension_names <- names(dimensions())
    
    updateSelectInput(session, "score_type", choices = c("Vulnerability Index", dimension_names), selected = "Vulnerability Index")
  })
  
  # Reactive expression for processed map data
  map_data_processed <- reactive({
    req(map_data(), input$merge_var_shapefile)
    
    # Ensure the merge variable exists in map_data()
    if (!(input$merge_var_shapefile %in% names(map_data()))) {
      showNotification("Merge variable in shapefile not found.", type = "error")
      return(NULL)
    }
    
    data <- map_data()
    data[[input$merge_var_shapefile]] <- as.character(data[[input$merge_var_shapefile]])
    return(data)
  })
  
  # Reactive expression for merged map scores
  map_scores_reactive <- reactive({
    req(map_data_processed(), index_scores(), data_processed(), merge_var_data_field(), input$merge_var_shapefile)
    
    map_scores <- map_data_processed() %>%
      rename(!!!setNames(input$merge_var_shapefile, merge_var_data_field()))
    
    # Merge index scores with spatial data and data_processed()
    map_scores <- map_scores %>%
      left_join(index_scores(), by = merge_var_data_field()) %>%
      left_join(data_processed(), by = merge_var_data_field())
    
    if (nrow(map_scores) == 0) {
      showNotification("Error: Unable to merge data and shapefile. Please check the merge variables.", type = "error")
      return(NULL)
    }
    
    return(map_scores)
  })
  
  # Separate Reactive Function for Pop-up Content
  popup_content_reactive <- reactive({
    req(map_scores_reactive(), input$score_type)
    
    map_scores <- map_scores_reactive()
    score_column <- input$score_type
    
    # Prepare score content
    if (!is.null(score_column) && score_column %in% names(map_scores)) {
      score_values <- map_scores[[score_column]]
      score_content <- paste0(score_column, ": ", signif(score_values, 3))
    } else {
      score_content <- NULL
    }
    
    # Prepare additional pop-up variables
    popup_vars <- popup_vars_fields()
    
    if (!is.null(popup_vars) && length(popup_vars) > 0) {
      popup_data <- map_scores %>% st_drop_geometry() %>% select(all_of(popup_vars))
      var_names <- sapply(popup_vars, function(var) {
        if (var %in% data_columns()) {
          field_mappings()$field_to_description[[var]]
        } else if (var %in% names(index_scores())) {
          var  # Use the variable name as is for index scores
        } else {
          var  # Keep the variable name as is
        }
      }, USE.NAMES = FALSE)
      
      # Build popup content for each feature
      popup_content <- sapply(seq_len(nrow(popup_data)), function(i) {
        row <- popup_data[i, ]
        row_formatted <- sapply(row, function(x) {
          if (is.numeric(x)) {
            signif(x, 3)
          } else {
            x
          }
        })
        popup_items <- paste0(var_names, ": ", as.character(row_formatted))
        popup_text <- paste(popup_items, collapse = "<br>")
        
        # Combine with score_content for the same feature
        if (!is.null(score_content)) {
          popup_text <- paste(popup_text, score_content[i], sep = "<br>")
        }
        return(popup_text)
      })
      
    } else if (!is.null(score_content)) {
      # Only score_content is available
      popup_content <- score_content
    } else {
      # No popup content available
      popup_content <- NULL
    }
    
    return(popup_content)
  })
  
  # Render the map using the processed map data and pop-up content
  output$colombia_map <- renderLeaflet({
    req(map_scores_reactive(), popup_content_reactive(), input$score_type)
    req(merge_var_data_field() != "", input$merge_var_shapefile != "")
    
    map_scores <- map_scores_reactive()
    
    score_column <- input$score_type
    # Handle cases where score_column is NULL or empty
    if (is.null(score_column) || score_column == "" || !(score_column %in% names(map_scores))) {
      # Set a default fill color and disable the legend
      fill_color <- "gray"
      show_legend <- FALSE
      pal <- colorNumeric(palette = "RdYlGn", domain = NULL, reverse = TRUE, na.color = "gray")
    } else if (all(is.na(map_scores[[score_column]]))) {
      fill_color <- "gray"
      show_legend <- FALSE
      showNotification(paste("Warning: The selected score", score_column, "has no valid data."), type = "warning")
      pal <- colorNumeric(palette = "RdYlGn", domain = NULL, reverse = TRUE, na.color = "gray")
    } else {
      # Create color palette based on the selected score
      pal <- colorNumeric(palette = "RdYlGn", domain = map_scores[[score_column]], reverse = TRUE, na.color = "gray")
      fill_color <- ~pal(map_scores[[score_column]])
      show_legend <- TRUE
    }
    
    # Get the pop-up content from the reactive function
    popup_content <- popup_content_reactive()
    
    # Create the leaflet map
    map <- leaflet(map_scores) %>%
      addProviderTiles("CartoDB.Positron", group = "Grayscale") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "Streets")
    
    # Conditionally add polygons with or without popup
    if (is.null(popup_content)) {
      map <- map %>%
        addPolygons(
          fillColor = fill_color,
          color = "white", weight = 1, fillOpacity = 0.7
        )
    } else {
      map <- map %>%
        addPolygons(
          fillColor = fill_color,
          color = "white", weight = 1, fillOpacity = 0.7,
          popup = popup_content
        )
    }
    
    # Add layers control
    map <- map %>%
      addLayersControl(baseGroups = c("Grayscale", "Streets"), position = "topright")
    
    # Conditionally add legend
    if (show_legend) {
      map <- map %>%
        addLegend(pal = pal, values = map_scores[[score_column]], position = "bottomright", title = score_column)
    }
    
    # Add scale bar
    map <- map %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
    
    # Return the map
    map
  })
  
  # Render the shapefile variables table
  output$shapefile_vars <- DT::renderDataTable({
    validate(
      need(map_data(), "Shapefile data is not available.")
    )
    DT::datatable(as.data.frame(st_drop_geometry(map_data())))
  })
  
  # Render the tables
  output$processed_data <- DT::renderDataTable({
    validate(
      need(data_processed(), "Processed data is not available.")
    )
    data_display <- data_processed() %>%
      rename_with(~ field_mappings()$field_to_description[.x], .cols = everything()) %>%
      mutate(across(where(is.numeric), ~ signif(., 3)))
    DT::datatable(data_display)
  })
  
  output$transformed_data <- DT::renderDataTable({
    validate(
      need(transformed_data(), "Transformed data is not available.")
    )
    data_display <- transformed_data() %>%
      rename_with(~ field_mappings()$field_to_description[.x], .cols = everything()) %>%
      mutate(across(where(is.numeric), ~ signif(., 3)))
    DT::datatable(data_display)
  })
  
  output$index_scores_data <- DT::renderDataTable({
    validate(
      need(index_scores(), "Index scores data is not available.")
    )
    data_display <- index_scores() %>%
      mutate(across(where(is.numeric), ~ signif(., 3)))
    DT::datatable(data_display)
  })
  
  # ======= Download Handlers =======
  
  # Download Handler for Processed Data
  output$download_processed_data <- downloadHandler(
    filename = function() {
      paste("processed_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Prepare data for download
      data_to_download <- data_processed() %>%
        rename_with(~ field_mappings()$field_to_description[.x], .cols = everything())
      write_xlsx(data_to_download, path = file)
    }
  )
  
  # Download Handler for Transformed Data
  output$download_transformed_data <- downloadHandler(
    filename = function() {
      paste("transformed_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Prepare data for download
      data_to_download <- transformed_data() %>%
        rename_with(~ field_mappings()$field_to_description[.x], .cols = everything())
      write_xlsx(data_to_download, path = file)
    }
  )
  
  # Download Handler for Index Scores Data
  output$download_index_scores_data <- downloadHandler(
    filename = function() {
      paste("index_scores_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Prepare data for download
      data_to_download <- index_scores()
      write_xlsx(data_to_download, path = file)
    }
  )
  
  # Download Handler for Shapefile Variables
  output$download_shapefile_vars <- downloadHandler(
    filename = function() {
      paste("shapefile_vars-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Prepare data for download
      data_to_download <- as.data.frame(st_drop_geometry(map_data()))
      write_xlsx(data_to_download, path = file)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
