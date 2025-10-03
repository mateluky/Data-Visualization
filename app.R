library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(fmsb)
library(shinyWidgets)

# 1) Loading Dataset
ufc_data <- read_csv("data/ufc-master.csv")  # Load the Kaggle dataset into R



# Load GeoJSON files
world_geo <- st_read("data/countries.geojson")  # GeoJSON file with all countries
us_states_geo <- st_read("data/us_states.geojson")  # GeoJSON for US states


# 2) Data Preprocessing
dataset_clean_question1 <- ufc_data%>%
  # Remove empty/NA finish details
  filter(!is.na(FinishDetails), FinishDetails != "") %>%
  
  # Convert date column 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  
  # Example grouping for FightID
  group_by(Date, RedFighter, BlueFighter) %>%
  mutate(FightID = cur_group_id()) %>%
  ungroup() %>%
  
  # Standardize finishing details
  mutate(
    FinishDetails = case_when(
      FinishDetails %in% c("Punch", "Punches") ~ "Punch(es)",
      FinishDetails %in% c("Kick", "Kicks") ~ "Kick(s)",
      FinishDetails %in% c("Knee", "Knees") ~ "Knee(s)",
      FinishDetails %in% c("Elbow", "Elbows") ~ "Elbow(s)",
      FinishDetails %in% c("Spinning Back Elbow", "Spinning Back Fist") ~ "Spinning Back Strike",
      TRUE ~ FinishDetails
    ),
    category = case_when(
      FinishDetails %in% c(
        "Guillotine Choke", "Rear Naked Choke", "Anaconda Choke", 
        "North-South Choke", "D'Arce Choke", "Triangle Choke", 
        "Arm Triangle", "Peruvian Necktie", "Scarf Hold",
        "Von Flue Choke", "Ezekiel Choke", "Inverted Triangle",
        "Other - Choke"
      ) ~ "Chokes",
      
      FinishDetails %in% c(
        "Armbar", "Straight Armbar", "Triangle Armbar", "Kimura", 
        "Heel Hook", "Kneebar", "Ankle Lock", "Omoplata", 
        "Keylock", "Other - Lock"
      ) ~ "Joint Locks",
      
      FinishDetails %in% c("Flying Knee", "Spinning Back Kick", "Knee(s)", "Kick(s)") ~ "Kicks",
      
      FinishDetails %in% c("Punch(es)", "Elbow(s)", "Spinning Back Strike") ~ "Strikes",
      
      FinishDetails %in% c("Slam", "Takedown", "Other") ~ "Other",
      TRUE ~ "Other"
    ),
    year = as.numeric(format(Date, "%Y"))
  )

fights_per_country <- ufc_data %>% group_by(Country) %>% summarise(fights = n())

# Ensure the 'Date' column is in Date format
if ("Date" %in% names(ufc_data)) {
  ufc_data$Date <- as.Date(ufc_data$Date, format = "%Y-%m-%d")
} else {
  stop("The dataset does not contain a 'Date' column. Please check your data.")
}

# Add a new column "winnerage" to the dataset
ufc_data <- ufc_data %>%
  mutate(winnerage = ifelse(Winner == "Red", RedAge,
                            ifelse(Winner == "Blue", BlueAge, NA)))



# Define the UI
ui <- fluidPage(
  titlePanel("UFC Fight Analytics"),
  navbarPage("Select One View",
             
             ############ View 1 starting###############
             tabPanel("Idiom 1 - Fight Finishes Trend Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Explore UFC fight finishes over time, by showing their relative percentage and total count. Explore specific fight finish catgegories in detail."),
                          selectInput(
                            inputId  = "main_category",
                            label    = "Select Finish Category",
                            choices  = c("All", setdiff(sort(unique(dataset_clean_question1$category)), "Other")),
                            selected = "All"
                          ),
                          checkboxInput("show_smoothing", "Enable Smoothing", FALSE),
                          checkboxInput("show_counts",    "Show Raw Counts", FALSE),
                          
                          # slider for year 
                          sliderInput(
                            "year_range",
                            "Select Year Range:",
                            min   = 2010,  # data starts 2010
                            max   = 2024,  # end in cy 2024
                            value = c(2010, 2024),   
                            step  = 1,
                            sep   = ""  
                          )
                        ),
                        mainPanel(
                          plotlyOutput("interactive_area_chart", height = "600px")
                        )
                      )),
             ############ View 1 ending###############
             
             ############ View 2 starting###############
             tabPanel(
               "Idiom 2 - Exploring Fight Outcomes",
               sidebarLayout(
                 sidebarPanel(
                   helpText("Explore how fight outcomes are distributed across
different woman and men weight classes in the UFC."),
                   width = 4,
                   
                   # Gender selector with icons for Male and Female
                   prettyRadioButtons(
                     "gender", # Input ID for gender selection
                     "Gender", # Label for the radio buttons
                     choices = c(
                       "Male" = "MALE",
                       "Female" = "FEMALE"
                     ),
                     selected = "MALE", # Default selection
                     status = "primary",
                     icon = icon("user") # User icon for the buttons
                   ),
                   
                   # Toggle switch to filter title bouts only
                   materialSwitch("boutTypeSwitch", "Show Title Bouts Only", status = "primary"),
                   
                   # Section for selecting finish types with Select All/Deselect All buttons
                   div(
                     style = "margin-bottom: 10px;",
                     h4("Select Finish Types"),
                     div(
                       style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
                       actionButton("selectAll", "Select All", class = "btn-sm btn-primary"),
                       actionButton("deselectAll", "Deselect All", class = "btn-sm btn-secondary")
                     )
                   ),
                   
                   # Checkbox group for selecting specific finish types
                   prettyCheckboxGroup(
                     "finishTypes", # Input ID for finish type selection
                     label = NULL, # No label required as header is already present
                     choices = c(
                       "KO/TKO" = "KO/TKO",
                       "Submission" = "SUB",
                       "Unanimous Decision" = "U-DEC",
                       "Split Decision" = "S-DEC",
                       "Majority Decision" = "M-DEC" # Default selection includes all options
                     ), 
                     selected = c("KO/TKO", "SUB", "U-DEC", "S-DEC", "M-DEC"),
                     status = "primary",
                     animation = "pulse" # Pulsing animation during clicking
                   )
                 ),
                 mainPanel(
                   width = 8,
                   
                   # Div container to adjust height dynamically and render the radar chart
                   div(
                     style = "height: calc(100vh - 100px);",
                     plotlyOutput("weightClassPlot", 
                                  height = "100%",
                                  width = "100%")
                   )
                 )
               )
             ),
             ############ View 2 ending###############
             
             ############ View 3 starting###############
             tabPanel(
               "Idiom 3 - Heatmap Analysis",
               sidebarLayout(
                 sidebarPanel(
                   helpText("Explore UFC trends with a heatmap by filtering winner age, nationality, weight class, and fight location."),
                   
                   sliderInput("timespan", 
                               label = "Select Year Range:",
                               min = 2010, max = 2023, value = c(2010, 2023), step = 1, sep = ""),
                   
                   selectInput("heatmap_var", 
                               label = "Select Variable to Group By:",
                               choices = c("Country", "Finish"), selected = "Country"),
                   
                   checkboxInput("show_binned", 
                                 label = "Show Age Category", value = FALSE),
                   
                   selectInput("weightclass", 
                               label = "Select Weight Class:",
                               choices = c("All", unique(ufc_data$WeightClass)), selected = "All"),
                   
                   textInput("fight_location", 
                             label = "Enter Fight Location (Country):", value = ""),
                   
                   radioButtons("gender_filter", 
                                label = "Select Gender:",
                                choices = c("All", unique(ufc_data$Gender)), selected = "All")
                 ),
                 mainPanel(
                   plotlyOutput("heatmap_plot")
                 )
               )
             ),
             ############ View 3 ending###############
             ############ View 4 starting###############
             tabPanel(
               "Idiom 4 - Geographic Distribution",
               sidebarLayout(
                 sidebarPanel(
                   tags$div(
                     p("This map visualizes UFC fight distributions, showing the number and percentage of fights and underdog victories across different locations."),
                     tags$ul(
                       tags$li("Gender Filter: View data for all fighters, men, or women."),
                       tags$li("Underdog Toggle: Show underdog win total and percentages in popups."),
                       tags$li("Map Type Toggle: Switch between global and US state-level views."),
                       tags$li("Nevada Toggle (available for map with US states): Include or exclude Nevada from the US states map analysis.")
                     )
                   ),
                   
                   radioButtons(
                     inputId = "gender_view4",
                     label = "Select Gender",
                     choices = c("All", "Men", "Women"),
                     selected = "All"
                   ),
                   checkboxInput(
                     inputId = "show_underdog",
                     label = "Show Underdog Calculation",
                     value = FALSE
                   ),
                   radioButtons(
                     inputId = "map_type",
                     label = "Select Map Type",
                     choices = c("Full World Map", "World Map with US states"),
                     selected = "Full World Map"
                   ),
                   # Conditional panel for "Include Nevada" checkbox
                   conditionalPanel(
                     condition = "input.map_type == 'World Map with US states'",
                     div(
                       style = "margin-left: 20px;",
                       checkboxInput(
                         inputId = "include_nevada",
                         label = "Include Nevada in the Map",
                         value = TRUE # Default is checked
                       )
                     )
                   )
                 ),
                 mainPanel(
                   leafletOutput("fightsMap", height = 600)  # Placeholder for the map
                 )
               )
             )
             ############ View 4 starting###############
             
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive filtered data based on gender and winner
  filtered_data <- reactive({
    data <- ufc_data %>%
      mutate(Country = ifelse(Country == "USA", "United States of America", Country))
    
    if (input$gender_view4 != "All") {
      gender_filter_view4 <- ifelse(input$gender_view4 == "Men", "MALE", "FEMALE")  # Map "Men" -> "MALE" and "Women" -> "FEMALE"
      data <- data %>% filter(Gender == gender_filter_view4)
    }
    
    # Exclude Nevada only when the "World Map with US states" is selected
    if (input$map_type == "World Map with US states" && !input$include_nevada) {
      data <- data %>% filter(!(Country == "United States of America" & str_detect(Location, ", Nevada,")))
    }
    data
  })
  
  output$fightsMap <- renderLeaflet({
    filtered <- filtered_data()
    total_fights <- nrow(filtered)
    
    # Replace "USA" with "United States of America"
    filtered <- filtered %>%
      mutate(Country = ifelse(Country == "USA", "United States of America", Country))
    
    # Country-level fight percentages
    fights_per_country <- filtered %>%
      group_by(Country) %>%
      summarise(fights = n(), .groups = "drop") %>%
      mutate(percentage = (fights / total_fights) * 100)
    
    # Calculate the underdog percentage for each country
    underdog_fights_per_country <- filtered %>%
      mutate(
        red_fighter_underdog = ifelse(RedOdds > BlueOdds & Winner == "Blue", TRUE, FALSE),
        blue_fighter_underdog = ifelse(BlueOdds > RedOdds & Winner == "Red", TRUE, FALSE)
      ) %>%
      mutate(underdog = red_fighter_underdog | blue_fighter_underdog) %>%
      group_by(Country) %>%
      summarise(underdog_fights = sum(underdog, na.rm = TRUE), .groups = "drop")
    
    merged_contry_data <- left_join(fights_per_country, underdog_fights_per_country, by = "Country") %>%
      mutate(
        underdog_percentage = ifelse(fights > 0, (underdog_fights / fights) * 100, 0),
        percentage = ifelse(is.na(percentage), 0, percentage),
        fights = ifelse(is.na(fights), 0, fights),
        underdog_fights = ifelse(is.na(underdog_fights), 0, underdog_fights)
      )
    
    # State-level fight percentages
    fights_per_state <- filtered %>%
      filter(Country == "United States of America") %>%
      separate(Location, into = c("City", "State", "Country"), sep = ",", extra = "merge") %>%
      mutate(State = str_trim(State)) %>%
      group_by(State) %>%
      summarise(fights = n(), .groups = "drop") %>%
      mutate(percentage = (fights / total_fights) * 100)
    
    # Calculate the underdog percentage for each state
    underdog_fights_per_state <- filtered %>%
      filter(Country == "United States of America") %>%
      mutate(
        red_fighter_underdog = ifelse(RedOdds > BlueOdds & Winner == "Blue", TRUE, FALSE),
        blue_fighter_underdog = ifelse(BlueOdds > RedOdds & Winner == "Red", TRUE, FALSE)
      ) %>%
      mutate(underdog = red_fighter_underdog | blue_fighter_underdog) %>%
      separate(Location, into = c("City", "State", "Country"), sep = ",", extra = "merge") %>%
      mutate(State = str_trim(State)) %>%
      group_by(State) %>%
      summarise(underdog_fights = sum(underdog, na.rm = TRUE), .groups = "drop")
    
    merged_state_data <- left_join(fights_per_state, underdog_fights_per_state, by = "State") %>%
      mutate(
        underdog_percentage = ifelse(fights > 0, (underdog_fights / fights) * 100, 0),
        percentage = ifelse(is.na(percentage), 0, percentage),
        fights = ifelse(is.na(fights), 0, fights),
        underdog_fights = ifelse(is.na(underdog_fights), 0, underdog_fights)
      )
    
    # Merge data into GeoJSON files
    world_geo <- world_geo %>%
      left_join(merged_contry_data, by = c("admin" = "Country")) %>%
      mutate(percentage = ifelse(is.na(percentage), 0, percentage),
             underdog_fights = ifelse(is.na(underdog_fights), 0, underdog_fights),
             fights = ifelse(is.na(fights), 0, fights))
    
    us_states_geo <- us_states_geo %>%
      left_join(merged_state_data, by = c("NAME" = "State")) %>%
      mutate(percentage = ifelse(is.na(percentage), 0, percentage),
             underdog_fights = ifelse(is.na(underdog_fights), 0, underdog_fights),
             fights = ifelse(is.na(fights), 0, fights))
    
    # Calculate global max for color scales
    if (input$map_type == "Full World Map") {
      global_max <- max(world_geo$percentage, na.rm = TRUE)
    } else {
      global_max <- max(
        c(
          world_geo %>% filter(admin != "United States of America") %>% pull(percentage),
          us_states_geo$percentage
        ),
        na.rm = TRUE
      )
    }
    
    # Palettes
    world_palette <- colorNumeric(rev(viridis::viridis(256)), domain = c(0, global_max), na.color = "#d3d3d3")
    us_palette <- colorNumeric(rev(viridis::viridis(256)), domain = c(0, global_max), na.color = "#d3d3d3")
    
    # Render maps
    if (input$map_type == "Full World Map") {
      leaflet() %>%
        addPolygons(
          data = world_geo,
          fillColor = ~ifelse(percentage == 0, "white", world_palette(percentage)),
          weight = 1,
          color = "white",
          fillOpacity = 0.8,
          popup = ~paste(
            "<strong>Country:</strong>", admin, "<br>",
            "<strong>Fight Percentage:</strong>", round(percentage, 2), "%",
            "<strong>Total Fights:</strong>", fights,
            if (input$show_underdog) paste("<br><strong>Underdog Percentage:</strong>", round(underdog_percentage, 2), "% <strong>Total Underdog:</strong>", underdog_fights) else ""
          )
        ) %>%
        addLegend(
          pal = world_palette,
          values = c(0, global_max),
          title = "Fight Percentage",
          position = "bottomleft"
        )
    } else {
      leaflet() %>%
        addPolygons(
          data = world_geo %>% filter(admin != "United States of America"),
          fillColor = ~ifelse(percentage == 0, "white", world_palette(percentage)),
          weight = 1,
          color = "white",
          fillOpacity = 0.8,
          popup = ~paste(
            "<strong>Country:</strong>", admin, "<br>",
            "<strong>Fight Percentage:</strong>", round(percentage, 2), "%",
            "<strong>Total Fights:</strong>", fights,
            if (input$show_underdog) paste("<br><strong>Underdog Percentage:</strong>", round(underdog_percentage, 2), "% <strong>Total Underdog:</strong>", underdog_fights) else ""
          )
        ) %>%
        addPolygons(
          data = us_states_geo %>%
            filter(!(NAME == "Nevada" & !input$include_nevada)),
          fillColor = ~ifelse(percentage == 0, "white", us_palette(percentage)),
          weight = 1,
          color = "white",
          fillOpacity = 0.8,
          popup = ~paste(
            "<strong>State:</strong>", NAME, "<br>",
            "<strong>Fight Percentage:</strong>", round(percentage, 2), "%",
            "<strong>Total Fights:</strong>", fights,
            if (input$show_underdog) paste("<br><strong>Underdog Percentage:</strong>", round(underdog_percentage, 2), "% <strong>Total Underdog:</strong>", underdog_fights) else ""
          )
        ) %>%
        addLegend(
          pal = us_palette,
          values = c(0, global_max),
          title = "Fight Percentage",
          position = "bottomright"
        )
    }
  })
  
  ################# View 1 starts here####################
  # -- Summaries --
  
  summarize_all_categories <- function() {
    data_summarized <- dataset_clean_question1 %>%
      group_by(year, category) %>%
      summarize(count = n_distinct(FightID), .groups = "drop")
    
    all_years  <- sort(unique(dataset_clean_question1$year))
    valid_cats <- setdiff(unique(dataset_clean_question1$category), NA)
    
    data_completed <- data_summarized %>%
      complete(year = all_years, category = valid_cats, fill = list(count = 0)) %>%
      group_by(year) %>%
      mutate(year_total = sum(count)) %>%
      ungroup() %>%
      mutate(proportion = ifelse(year_total > 0, (count / year_total)*100, 0)) %>%
      filter(year_total > 0)
    
    data_completed
  }
  
  summarize_subcategories <- function(main_cat) {
    subcat_data <- dataset_clean_question1 %>%
      filter(category == main_cat) %>%
      group_by(FinishDetails) %>%
      summarize(total_count = n_distinct(FightID), .groups = "drop") %>%
      arrange(desc(total_count))
    
    # I'll pick top 5 (4 top plus other)
    top_4 <- head(subcat_data$FinishDetails, 4)
    other_label <- paste("Other -", main_cat)
    
    processed <- dataset_clean_question1 %>%
      filter(category == main_cat) %>%
      mutate(FinishDetails = ifelse(
        FinishDetails %in% top_4, FinishDetails, other_label
      )) %>%
      group_by(year, FinishDetails) %>%
      summarize(count = n_distinct(FightID), .groups = "drop") %>%
      rename(category = FinishDetails)
    
    all_years <- sort(unique(dataset_clean_question1$year))
    subcat_list <- unique(c(top_4, other_label))
    
    data_completed <- processed %>%
      complete(year = all_years, category = subcat_list, fill = list(count = 0)) %>%
      group_by(year) %>%
      mutate(year_total = sum(count)) %>%
      ungroup() %>%
      mutate(proportion = ifelse(year_total > 0, (count / year_total)*100, 0)) %>%
      filter(year_total > 0) %>%
      group_by(category) %>%
      mutate(cat_sum = sum(count)) %>%
      ungroup() %>%
      filter(!(category == other_label & cat_sum == 0)) %>%
      select(-cat_sum)
    
    # Factor order
    subcategory_order <- c(top_4, other_label)
    subcategory_order <- subcategory_order[subcategory_order %in% unique(data_completed$category)]
    data_completed$category <- factor(
      data_completed$category,
      levels = subcategory_order
    )
    
    data_completed
  }
  
  # Reactive data (plus filter by year_range)
  filtered_data_view1 <- reactive({
    # 1) Summaries
    summary_df <-
      if (input$main_category == "All") {
        summarize_all_categories()
      } else {
        summarize_subcategories(input$main_category)
      }
    
    # 2) Filter by year range slider
    summary_df %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })
  
  output$interactive_area_chart <- renderPlotly({
    
    data <- filtered_data_view1()
    
    # Build x_label
    year_info <- data %>%
      group_by(year) %>%
      summarize(year_total = unique(year_total), .groups = "drop") %>%
      mutate(x_label = paste("Year:", year, " Total Finishes", year_total))
    
    data_merged <- data %>%
      left_join(year_info, by = "year")
    
    measure_col  <- if (input$show_counts) "count" else "proportion"
    y_axis_label <- if (input$show_counts) "Total Finishes (Stacked)" else "Proportion (%)"
    
    fig <- plot_ly()
    
    # Basic hover templates
    # Different hover templates for raw counts vs. percentage
    create_hover_template_raw <- function(name_placeholder) {
      paste0(
        "<b>", name_placeholder,"</b>",
        "<br>%{y:.0f} Total",
        "<extra></extra>"
      )
    }
    create_hover_template <- function(name_placeholder) {
      paste0(
        "<b>", name_placeholder,"</b>",
        "<br>%{y:.0f}%",
        "<extra></extra>"
      )
    }
    # Different hover templates for smooth counts vs. normal
    create_hover_template_raw_smooth <- function(name_placeholder) {
      paste0(
        "<b>", name_placeholder,"</b>",
        "<br>%{y:.0f} Total Smoothend",
        "<extra></extra>"
      )
    }
    create_hover_template_smooth <- function(name_placeholder) {
      paste0(
        "<b>", name_placeholder,"</b>",
        "<br>%{y:.0f}% Smoothend",
        "<extra></extra>"
      )
    }
    raw_hover <- "<b>Category</b>: %{name}<br>Count: %{y:.0f}<extra></extra>"
    perc_hover <- "<b>Category</b>: %{name}<br>Percentage: %{y:.2f}%<extra></extra>"
    
    if (!input$show_smoothing) {
      # No smoothing
      for (cat_name in sort(unique(data_merged$category))) {
        
        df_cat <- data_merged %>% 
          filter(category == cat_name) %>%
          arrange(year)
        
        fig <- fig %>%
          add_trace(
            data       = df_cat,
            x          = ~x_label,
            y          = as.formula(paste0("~", measure_col)),
            name       = cat_name,
            type       = "scatter",
            mode       = "none",
            stackgroup = "one",
            hovertemplate = if (input$show_counts) create_hover_template_raw(cat_name) else create_hover_template(cat_name)
          )
      }
      
    } else {
      # LOESS smoothing
      loess_data <- data_merged %>%
        group_by(category) %>%
        do({
          measure_vals <- .[[measure_col]]
          yrs          <- .$year
          fit <- loess(measure_vals ~ yrs, span = 0.75)
          data.frame(
            year = unique(yrs),
            predicted = predict(fit, newdata = data.frame(yrs = unique(yrs))),
            category  = unique(.$category)
          )
        }) %>%
        ungroup()
      
      # Clip negatives or large proportions
      if (input$show_counts) {
        loess_data$predicted <- pmax(0, loess_data$predicted)
      } else {
        loess_data$predicted <- pmax(0, pmin(loess_data$predicted, 100))
      }
      
      loess_joined <- loess_data %>%
        left_join(year_info, by = "year") %>%
        arrange(category, year)
      
      if (!input$show_counts) {
        # re-normalize so sum=100
        loess_stacked <- loess_joined %>%
          group_by(year) %>%
          mutate(
            sum_pred  = sum(predicted),
            norm_pred = ifelse(sum_pred>0, predicted/sum_pred*100, 0)
          ) %>%
          ungroup()
        
        for (cat_name in sort(unique(loess_stacked$category))) {
          df_smooth <- loess_stacked %>%
            filter(category == cat_name) %>%
            arrange(year)
          
          fig <- fig %>%
            add_trace(
              data       = df_smooth,
              x          = ~x_label,
              y          = ~norm_pred,
              name       = cat_name,
              type       = "scatter",
              mode       = "none",
              stackgroup = "one",
              hovertemplate = create_hover_template_smooth(cat_name)
            )
        }
        
      } else {
        # Smoothing raw counts
        loess_stacked <- loess_joined %>%
          group_by(year) %>%
          mutate(sum_pred = sum(predicted)) %>%
          ungroup()
        
        for (cat_name in sort(unique(loess_stacked$category))) {
          df_smooth <- loess_stacked %>%
            filter(category == cat_name) %>%
            arrange(year)
          
          fig <- fig %>%
            add_trace(
              data       = df_smooth,
              x          = ~x_label,
              y          = ~predicted,
              name       = cat_name,
              type       = "scatter",
              mode       = "none",
              stackgroup = "one",
              hovertemplate = create_hover_template_raw(cat_name)
            )
        }
      }
    }
    
    fig <- fig %>%
      layout(
        hovermode = "x unified",
        title     = "UFC Relative Finish Types Over Time",
        xaxis = list(
          title         = "Year",
          type          = "category",
          categoryorder = "array",
          categoryarray = year_info$x_label,
          tickvals      = year_info$x_label,
          ticktext      = as.character(year_info$year),
          fixedrange    = TRUE  # Disable zoom for x-axis
        ),
        yaxis = list(
          title       = y_axis_label,
          fixedrange  = TRUE    # Disable zoom for y-axis
        ),
        dragmode = FALSE        # No zoom/pan
      )
    
    fig
  })
  ################# View 1 ends here######################
  
  ################# View 2 starts here####################
  
  # Observer to handle the Select All button functionality
  observeEvent(input$selectAll, {
    updatePrettyCheckboxGroup(session,
                              "finishTypes",
                              selected = c("KO/TKO", "SUB", "U-DEC", "S-DEC", "M-DEC")) # Select all finish types
  })
  
  # Observer to handle the Deselect All button functionality
  observeEvent(input$deselectAll, {
    updatePrettyCheckboxGroup(session, "finishTypes", selected = character(0)) # Deselect all finish types
  })
  
  # Render radar chart output based on user inputs
  output$weightClassPlot <- renderPlotly({
    
    # Filter data based on user selections (gender, title bouts, finish types)
    filtered_data <- ufc_data %>%
      # Filter title bouts if enabled
      filter(if (input$boutTypeSwitch) TitleBout == TRUE else TRUE,
             # Filter by gender if specified
             if (input$gender != "all") Gender == input$gender else TRUE) %>%
      
      # Group data by weight class
      group_by(WeightClass) %>%
      summarise(
        `KO/TKO` = sum(Finish == "KO/TKO", na.rm = TRUE),
        SUB = sum(Finish == "SUB", na.rm = TRUE),
        `U-DEC` = sum(Finish == "U-DEC", na.rm = TRUE),
        `S-DEC` = sum(Finish == "S-DEC", na.rm = TRUE),
        `M-DEC` = sum(Finish == "M-DEC", na.rm = TRUE)
      ) %>%
      select(WeightClass, all_of(input$finishTypes)) # Include only selected finish types
    
    # Check if valid data is available for rendering the chart
    if (nrow(filtered_data) == 0 || ncol(filtered_data) <= 1) {
      return(NULL)
    }
    
    # Prepare data for radar chart visualization
    radar_data <- as.data.frame(t(filtered_data[, -1]))
    colnames(radar_data) <- filtered_data$WeightClass
    
    # Define colors for each finish type in the radar chart
    finish_colors <- c(
      "KO/TKO" = "#FF0000",
      "SUB" = "#0000FF",
      "U-DEC" = "#00FF00",
      "S-DEC" = "#FFA500",
      "M-DEC" = "#800080"
    )
    
    # Initialize Plotly radar chart object with layout configurations
    plot_ly() %>%
      config(displayModeBar = FALSE) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, ceiling(max(radar_data, na.rm = TRUE))),
            tickfont = list(size = 10),
            automargin = TRUE
          ),
          angularaxis = list(
            tickfont = list(size = 12),
            rotation = 90,
            direction = "clockwise",
            automargin = TRUE
          ),
          gridshape = "circular"
        ),
        showlegend = TRUE,
        legend = list(
          x = 0,
          y = 1.2
        ),
        title = list(
          text = "Exploring Fight Outcome Distributions Across UFC Weight Classes",
          y = 0.95
        ),
        margin = list(
          l = 100,
          r = 150,  
          t = 100,
          b = 100,
          pad = 0
        ),
        width = 800,   
        height = 500
      ) -> p
    
    # Add traces dynamically per finish type:
    for(i in 1:nrow(radar_data)) {
      finish_type <- rownames(radar_data)[i]  # Get the name of the current finish type
      
      # Closing the loops
      r_values <- c(as.numeric(radar_data[i, ]), as.numeric(radar_data[i, 1]))
      theta_values <- c(colnames(radar_data), colnames(radar_data)[1])
      
      # Add a trace (data series) to the radar chart for the current finish type
      p <- add_trace(
        p,
        type = 'scatterpolar',
        r = r_values,
        theta = theta_values,
        name = finish_type,
        mode = 'lines+markers',  # Display both lines and markers on the chart
        fill = 'toself',
        fillcolor = adjustcolor(finish_colors[finish_type], alpha.f = 0.2),
        line = list(color = finish_colors[finish_type]),
        marker = list(color = finish_colors[finish_type]), 
        
        # Tooltip to display details when hovering over a point
        hovertemplate = paste(
          "<b>%{theta}</b><br>", # Weight class
          finish_type, ": %{r}<br>", # Finish type and its count
          "<extra></extra>" # Remove additional tooltip information
        )
      )
    }
    p #Without returning `p`, the chart would not be rendered or displayed in the Shiny app.
  })
  
  ################ View 2 ends here #################
  
  ############### View 3 starts here################
  # Reactive filtered data
  filtered_data_view3 <- reactive({
    data <- ufc_data %>%
      filter(as.numeric(format(as.Date(Date, format = "%Y-%m-%d"), "%Y")) >= input$timespan[1],
             as.numeric(format(as.Date(Date, format = "%Y-%m-%d"), "%Y")) <= input$timespan[2])
    
    if (input$weightclass != "All") {
      data <- data %>% filter(WeightClass == input$weightclass)
    }
    
    if (input$fight_location != "") {
      data <- data %>% filter(grepl(input$fight_location, Country, ignore.case = TRUE))
    }
    
    if (input$gender_filter != "All") {
      data <- data %>% filter(Gender == input$gender_filter)
    }
    
    if (nrow(data) == 0) {
      showModal(modalDialog(
        title = "No Data Available",
        "No data matches the current filter settings. Please adjust your selections.",
        easyClose = TRUE
      ))
    }
    
    data
  })
  
  # Reactive grouped data
  grouped_data <- reactive({
    filtered_data_view3() %>%
      group_by(`Winner's Age` = winnerage, !!sym(input$heatmap_var)) %>%
      summarize(`Number of Average Rounds` = mean(NumberOfRounds, na.rm = TRUE), .groups = "drop")
  })
  
  # Reactive binned data
  binned_data <- reactive({
    grouped_data() %>%
      mutate(Category = cut(`Winner's Age`, 
                            breaks = c(-Inf, 25, 30, 35, 40, Inf), 
                            labels = c("[0,25]", "[26,30]", "[31,35]", "[36,40]", "[41,Inf]"))) %>%
      group_by(Category, !!sym(input$heatmap_var)) %>%
      summarize(`Number of Average Rounds` = mean(`Number of Average Rounds`, na.rm = TRUE), .groups = "drop")
  })
  
  # Heatmap plot
  heatmap_plot <- reactive({
    if (input$show_binned) {
      data <- binned_data()
      gg <- ggplot(data, aes(x = Category, y = !!sym(input$heatmap_var), fill = `Number of Average Rounds`)) +
        geom_tile() +
        scale_fill_gradient(low = "lightskyblue", high = "navy", name = "Number of Average Rounds") +
        labs(
          title = paste("Combat Analytics Heatmap"),
          x = "Age Category",
          y = input$heatmap_var
        ) +
        theme_minimal()
    } else {
      data <- grouped_data()
      gg <- ggplot(data, aes(x = `Winner's Age`, y = !!sym(input$heatmap_var), fill = `Number of Average Rounds`)) +
        geom_tile() +
        scale_fill_gradient(low = "lightskyblue", high = "navy", name = "Number of Average Rounds") +
        labs(
          title = paste("Combat Analytics Heatmap"),
          x = "Winner's Age",
          y = input$heatmap_var
        ) +
        theme_minimal()
    }
    gg
  })
  
  # Render the heatmap as Plotly
  output$heatmap_plot <- renderPlotly({
    ggplotly(heatmap_plot()) %>% layout(height = 600)
  })
  
  ############### View 3 ends here#################
}

# Run the app
shinyApp(ui = ui, server = server)