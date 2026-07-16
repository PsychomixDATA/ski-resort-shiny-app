# ============================================================
# What Makes an Ideal Ski Resort?
# Complete Shiny application
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(bsicons)
library(ggplot2)
library(DT)


# ============================================================
# Load and prepare data
# ============================================================

resorts <- readr::read_csv(
  "resorts_clean_final.csv",
  show_col_types = FALSE
)


# Convert important fields to appropriate data types
resorts <- resorts %>%
  mutate(
    resort = as.character(resort),
    country = as.character(country),
    continent = as.character(continent),
    
    price_clean = readr::parse_number(
      as.character(price_clean)
    ),
    
    total_slopes = readr::parse_number(
      as.character(total_slopes)
    ),
    
    vertical_drop = readr::parse_number(
      as.character(vertical_drop)
    ),
    
    longest_run = readr::parse_number(
      as.character(longest_run)
    ),
    
    lift_capacity_clean = readr::parse_number(
      as.character(lift_capacity_clean)
    ),
    
    value_score = readr::parse_number(
      as.character(value_score)
    ),
    
    beginner_slopes = readr::parse_number(
      as.character(beginner_slopes)
    ),
    
    intermediate_slopes = readr::parse_number(
      as.character(intermediate_slopes)
    ),
    
    difficult_slopes = readr::parse_number(
      as.character(difficult_slopes)
    ),
    
    total_lifts = readr::parse_number(
      as.character(total_lifts)
    ),
    
    nightskiing = trimws(as.character(nightskiing)),
    snowparks = trimws(as.character(snowparks)),
    summer_skiing = trimws(as.character(summer_skiing)),
    child_friendly = trimws(as.character(child_friendly))
  )


# Remove rows that cannot support the main app functions
# and remove resorts with less than 40 km of terrain
resorts <- resorts %>%
  filter(
    !is.na(resort),
    resort != "",
    !is.na(country),
    country != "",
    !is.na(continent),
    continent != "",
    !is.na(total_slopes),
    total_slopes >= 40
  )


# ============================================================
# Summary values
# ============================================================

number_of_resorts <- nrow(resorts)

number_of_countries <- dplyr::n_distinct(
  resorts$country,
  na.rm = TRUE
)

number_of_continents <- dplyr::n_distinct(
  resorts$continent,
  na.rm = TRUE
)


# ============================================================
# Slider limits
# ============================================================

minimum_price <- floor(
  min(resorts$price_clean, na.rm = TRUE)
)

maximum_price <- ceiling(
  max(resorts$price_clean, na.rm = TRUE)
)

maximum_terrain <- ceiling(
  max(resorts$total_slopes, na.rm = TRUE)
)


# ============================================================
# Resort selector choices
# ============================================================

resort_choices <- resorts %>%
  filter(
    !is.na(resort),
    resort != ""
  ) %>%
  arrange(resort) %>%
  distinct(resort) %>%
  pull(resort)


# ============================================================
# User interface
# ============================================================

ui <- bslib::page_navbar(
  
  title = "What Makes an Ideal Ski Resort?",
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  
  # ==========================================================
  # Home tab
  # ==========================================================
  
  bslib::nav_panel(
    title = "Home",
    icon = bsicons::bs_icon("house"),
    
    div(
      class = "p-4",
      
      div(
        class = "text-center mb-5",
        
        h1(
          class = "display-4 fw-bold",
          "What Makes an Ideal Ski Resort?"
        ),
        
        p(
          class = "lead",
          paste(
            "An interactive decision-support tool",
            "for planning a ski trip"
          )
        )
      ),
      
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        
        bslib::value_box(
          title = "Destination Resorts",
          value = format(
            number_of_resorts,
            big.mark = ","
          ),
          showcase = bsicons::bs_icon("snow2")
        ),
        
        bslib::value_box(
          title = "Countries",
          value = number_of_countries,
          showcase = bsicons::bs_icon("globe-americas")
        ),
        
        bslib::value_box(
          title = "Continents",
          value = number_of_continents,
          showcase = bsicons::bs_icon("map")
        )
      ),
      
      bslib::card(
        class = "mt-4",
        
        bslib::card_header(
          h3("Finding the right mountain")
        ),
        
        bslib::card_body(
          p(
            paste(
              "Choosing a ski destination involves more than",
              "identifying the largest or most famous resort.",
              "Skiers may prioritize affordable lift tickets,",
              "extensive terrain, large vertical drops, long",
              "runs, strong lift infrastructure, or amenities",
              "such as night skiing and terrain parks."
            )
          ),
          
          p(
            paste(
              "This application uses information from",
              "destination ski resorts around the world.",
              "Resorts with fewer than 40 kilometers of",
              "skiable terrain have been removed so that the",
              "comparisons focus on locations that are more",
              "relevant to ski-trip planning."
            )
          ),
          
          p(
            strong("Central question: "),
            "What makes an ideal ski resort?"
          )
        )
      ),
      
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        
        bslib::card(
          class = "mt-4",
          
          bslib::card_header(
            bsicons::bs_icon("search"),
            " Explore"
          ),
          
          bslib::card_body(
            paste(
              "Filter destination resorts by continent,",
              "price, terrain, and amenities."
            )
          )
        ),
        
        bslib::card(
          class = "mt-4",
          
          bslib::card_header(
            bsicons::bs_icon("bar-chart"),
            " Compare"
          ),
          
          bslib::card_body(
            paste(
              "Compare terrain, price, vertical drop,",
              "longest runs, and resort value."
            )
          )
        ),
        
        bslib::card(
          class = "mt-4",
          
          bslib::card_header(
            bsicons::bs_icon("sliders"),
            " Personalize"
          ),
          
          bslib::card_body(
            paste(
              "Change the importance of different features",
              "and receive personalized resort matches."
            )
          )
        )
      ),
      
      bslib::card(
        class = "mt-4 mb-4",
        
        bslib::card_header(
          h3("There is no single perfect resort")
        ),
        
        bslib::card_body(
          p(
            paste(
              "A resort with enormous terrain may be",
              "expensive. A mountain with exceptional",
              "vertical drop may offer fewer total slopes.",
              "Another destination may provide better value",
              "despite having less famous terrain."
            )
          ),
          
          p(
            paste(
              "The app is designed to reveal these tradeoffs",
              "and help users identify the resort that best",
              "fits their own priorities."
            )
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # Explore Resorts tab
  # ==========================================================
  
  bslib::nav_panel(
    title = "Explore Resorts",
    icon = bsicons::bs_icon("geo-alt"),
    
    bslib::layout_sidebar(
      
      sidebar = bslib::sidebar(
        title = "Filter Resorts",
        
        selectInput(
          inputId = "continent_filter",
          label = "Continent",
          choices = c(
            "All continents",
            sort(unique(resorts$continent))
          ),
          selected = "All continents"
        ),
        
        sliderInput(
          inputId = "price_filter",
          label = "Maximum daily price (€)",
          min = minimum_price,
          max = maximum_price,
          value = maximum_price,
          step = 5
        ),
        
        sliderInput(
          inputId = "terrain_filter",
          label = "Minimum total slopes (km)",
          min = 40,
          max = maximum_terrain,
          value = 40,
          step = 20
        ),
        
        checkboxInput(
          inputId = "explore_night",
          label = "Require night skiing",
          value = FALSE
        ),
        
        checkboxInput(
          inputId = "explore_snowpark",
          label = "Require a snowpark",
          value = FALSE
        ),
        
        width = 300
      ),
      
      bslib::card(
        full_screen = TRUE,
        
        bslib::card_header(
          h3("Matching Ski Resorts"),
          textOutput("matching_count")
        ),
        
        bslib::card_body(
          DT::DTOutput("resort_table")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # Compare tab
  # ==========================================================
  
  bslib::nav_panel(
    title = "Compare",
    icon = bsicons::bs_icon("graph-up"),
    
    div(
      class = "p-4",
      
      h2("Compare Ski Resorts"),
      
      p(
        class = "lead",
        paste(
          "Examine how resort size relates to price,",
          "vertical drop, and longest run."
        )
      ),
      
      selectInput(
        inputId = "compare_variable",
        label = "Select a comparison:",
        choices = c(
          "Daily price" = "price",
          "Vertical drop" = "vertical",
          "Longest run" = "run"
        ),
        selected = "price",
        width = "350px"
      ),
      
      bslib::card(
        bslib::card_header(
          h3("Resort Comparison")
        ),
        
        bslib::card_body(
          plotOutput(
            outputId = "comparison_plot",
            height = "450px"
          )
        )
      ),
      
      bslib::card(
        class = "mt-4 mb-4",
        
        bslib::card_header(
          h3("Top 10 Resorts by Value")
        ),
        
        bslib::card_body(
          DT::DTOutput("ranking_table")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # Find My Resort tab
  # ==========================================================
  
  bslib::nav_panel(
    title = "Find My Resort",
    icon = bsicons::bs_icon("stars"),
    
    bslib::layout_sidebar(
      
      sidebar = bslib::sidebar(
        title = "Your Priorities",
        
        p(
          paste(
            "Rate the importance of each feature",
            "from 0 to 5."
          )
        ),
        
        sliderInput(
          inputId = "terrain_weight",
          label = "Skiable terrain",
          min = 0,
          max = 5,
          value = 4,
          step = 1
        ),
        
        sliderInput(
          inputId = "vertical_weight",
          label = "Vertical drop",
          min = 0,
          max = 5,
          value = 4,
          step = 1
        ),
        
        sliderInput(
          inputId = "price_weight",
          label = "Affordability",
          min = 0,
          max = 5,
          value = 3,
          step = 1
        ),
        
        sliderInput(
          inputId = "lift_weight",
          label = "Lift capacity",
          min = 0,
          max = 5,
          value = 2,
          step = 1
        ),
        
        sliderInput(
          inputId = "run_weight",
          label = "Longest run",
          min = 0,
          max = 5,
          value = 2,
          step = 1
        ),
        
        checkboxInput(
          inputId = "require_night",
          label = "Require night skiing",
          value = FALSE
        ),
        
        checkboxInput(
          inputId = "require_snowpark",
          label = "Require a snowpark",
          value = FALSE
        ),
        
        width = 320
      ),
      
      bslib::card(
        full_screen = TRUE,
        
        bslib::card_header(
          h3("Your Top Resort Matches")
        ),
        
        bslib::card_body(
          p(
            paste(
              "The match score changes according to the",
              "priorities selected in the sidebar.",
              "A higher score indicates a stronger match."
            )
          ),
          
          DT::DTOutput("ideal_table")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # Resort Profile tab
  # ==========================================================
  
  bslib::nav_panel(
    title = "Resort Profile",
    icon = bsicons::bs_icon("card-list"),
    
    div(
      class = "p-4",
      
      h2("Resort Profile"),
      
      selectInput(
        inputId = "profile_resort",
        label = "Select a resort:",
        choices = resort_choices,
        selected = resort_choices[[1]],
        width = "500px"
      ),
      
      uiOutput("profile_heading"),
      
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        bslib::value_box(
          title = "Daily Price",
          value = textOutput("profile_price"),
          showcase = bsicons::bs_icon("cash")
        ),
        
        bslib::value_box(
          title = "Total Slopes",
          value = textOutput("profile_slopes"),
          showcase = bsicons::bs_icon("signpost-split")
        ),
        
        bslib::value_box(
          title = "Vertical Drop",
          value = textOutput("profile_vertical"),
          showcase = bsicons::bs_icon("arrow-down-up")
        ),
        
        bslib::value_box(
          title = "Value Score",
          value = textOutput("profile_value"),
          showcase = bsicons::bs_icon("star")
        )
      ),
      
      bslib::card(
        class = "mt-4",
        
        bslib::card_header(
          h3("Terrain Breakdown")
        ),
        
        bslib::card_body(
          plotOutput(
            outputId = "terrain_plot",
            height = "350px"
          )
        )
      ),
      
      bslib::card(
        class = "mt-4 mb-4",
        
        bslib::card_header(
          h3("Resort Details")
        ),
        
        bslib::card_body(
          tableOutput("profile_details")
        )
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # Explore Resorts
  # ==========================================================
  
  filtered_resorts <- reactive({
    
    req(
      input$continent_filter,
      input$price_filter,
      input$terrain_filter
    )
    
    result <- resorts %>%
      filter(
        !is.na(price_clean),
        price_clean <= input$price_filter,
        total_slopes >= input$terrain_filter
      )
    
    if (input$continent_filter != "All continents") {
      result <- result %>%
        filter(
          continent == input$continent_filter
        )
    }
    
    if (isTRUE(input$explore_night)) {
      result <- result %>%
        filter(
          tolower(nightskiing) == "yes"
        )
    }
    
    if (isTRUE(input$explore_snowpark)) {
      result <- result %>%
        filter(
          tolower(snowparks) == "yes"
        )
    }
    
    result
  })
  
  
  output$matching_count <- renderText({
    
    paste(
      nrow(filtered_resorts()),
      "resorts match the selected filters"
    )
  })
  
  
  output$resort_table <- DT::renderDT({
    
    table_data <- filtered_resorts() %>%
      arrange(desc(value_score)) %>%
      transmute(
        Resort = resort,
        Country = country,
        Continent = continent,
        `Price (€)` = price_clean,
        `Total slopes (km)` = total_slopes,
        `Vertical drop (m)` = vertical_drop,
        `Longest run (km)` = longest_run,
        `Value score` = value_score,
        `Night skiing` = nightskiing,
        Snowpark = snowparks
      )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatRound(
        columns = c(
          "Price (€)",
          "Total slopes (km)",
          "Vertical drop (m)",
          "Longest run (km)",
          "Value score"
        ),
        digits = 1
      )
  })
  
  
  # ==========================================================
  # Compare Resorts
  # ==========================================================
  
  output$comparison_plot <- renderPlot({
    
    req(input$compare_variable)
    
    if (input$compare_variable == "vertical") {
      
      plot_data <- resorts %>%
        filter(
          !is.na(total_slopes),
          !is.na(vertical_drop)
        )
      
      validate(
        need(
          nrow(plot_data) > 1,
          paste(
            "Not enough data are available",
            "for this comparison."
          )
        )
      )
      
      ggplot(
        plot_data,
        aes(
          x = total_slopes,
          y = vertical_drop,
          color = continent
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7
        ) +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "black"
        ) +
        labs(
          title = "Skiable Terrain and Vertical Drop",
          x = "Total slopes (km)",
          y = "Vertical drop (m)",
          color = "Continent"
        ) +
        theme_minimal(base_size = 13)
      
    } else if (input$compare_variable == "run") {
      
      plot_data <- resorts %>%
        filter(
          !is.na(total_slopes),
          !is.na(longest_run)
        )
      
      validate(
        need(
          nrow(plot_data) > 1,
          paste(
            "Not enough data are available",
            "for this comparison."
          )
        )
      )
      
      ggplot(
        plot_data,
        aes(
          x = total_slopes,
          y = longest_run,
          color = continent
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7
        ) +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "black"
        ) +
        labs(
          title = "Skiable Terrain and Longest Run",
          x = "Total slopes (km)",
          y = "Longest run (km)",
          color = "Continent"
        ) +
        theme_minimal(base_size = 13)
      
    } else {
      
      plot_data <- resorts %>%
        filter(
          !is.na(total_slopes),
          !is.na(price_clean)
        )
      
      validate(
        need(
          nrow(plot_data) > 1,
          paste(
            "Not enough data are available",
            "for this comparison."
          )
        )
      )
      
      ggplot(
        plot_data,
        aes(
          x = total_slopes,
          y = price_clean,
          color = continent
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7
        ) +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "black"
        ) +
        labs(
          title = "Skiable Terrain and Daily Price",
          x = "Total slopes (km)",
          y = "Daily price (€)",
          color = "Continent"
        ) +
        theme_minimal(base_size = 13)
    }
  })
  
  
  output$ranking_table <- DT::renderDT({
    
    ranking_data <- resorts %>%
      filter(
        !is.na(value_score)
      ) %>%
      arrange(
        desc(value_score)
      ) %>%
      slice_head(n = 10) %>%
      transmute(
        Resort = resort,
        Country = country,
        `Value score` = value_score,
        `Price (€)` = price_clean,
        `Total slopes (km)` = total_slopes,
        `Vertical drop (m)` = vertical_drop
      )
    
    validate(
      need(
        nrow(ranking_data) > 0,
        "No resorts have a usable value score."
      )
    )
    
    DT::datatable(
      ranking_data,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10,
        scrollX = TRUE
      )
    ) %>%
      DT::formatRound(
        columns = c(
          "Value score",
          "Price (€)",
          "Total slopes (km)",
          "Vertical drop (m)"
        ),
        digits = 1
      )
  })
  
  
  # ==========================================================
  # Find My Resort
  # ==========================================================
  
  personalized_resorts <- reactive({
    
    req(
      input$terrain_weight,
      input$vertical_weight,
      input$price_weight,
      input$lift_weight,
      input$run_weight
    )
    
    total_weight <-
      input$terrain_weight +
      input$vertical_weight +
      input$price_weight +
      input$lift_weight +
      input$run_weight
    
    validate(
      need(
        total_weight > 0,
        "Set at least one preference slider above zero."
      )
    )
    
    result <- resorts
    
    if (isTRUE(input$require_night)) {
      result <- result %>%
        filter(
          tolower(nightskiing) == "yes"
        )
    }
    
    if (isTRUE(input$require_snowpark)) {
      result <- result %>%
        filter(
          tolower(snowparks) == "yes"
        )
    }
    
    validate(
      need(
        nrow(result) > 0,
        paste(
          "No resorts meet the required",
          "amenity selections."
        )
      )
    )
    
    result %>%
      mutate(
        terrain_component =
          dplyr::percent_rank(total_slopes),
        
        vertical_component =
          dplyr::percent_rank(vertical_drop),
        
        affordability_component =
          1 - dplyr::percent_rank(price_clean),
        
        lift_component =
          dplyr::percent_rank(lift_capacity_clean),
        
        run_component =
          dplyr::percent_rank(longest_run),
        
        terrain_component =
          dplyr::coalesce(
            terrain_component,
            0.5
          ),
        
        vertical_component =
          dplyr::coalesce(
            vertical_component,
            0.5
          ),
        
        affordability_component =
          dplyr::coalesce(
            affordability_component,
            0.5
          ),
        
        lift_component =
          dplyr::coalesce(
            lift_component,
            0.5
          ),
        
        run_component =
          dplyr::coalesce(
            run_component,
            0.5
          ),
        
        match_score = 100 * (
          input$terrain_weight * terrain_component +
            input$vertical_weight * vertical_component +
            input$price_weight * affordability_component +
            input$lift_weight * lift_component +
            input$run_weight * run_component
        ) / total_weight,
        
        match_score = round(
          match_score,
          1
        )
      ) %>%
      arrange(
        desc(match_score)
      )
  })
  
  
  output$ideal_table <- DT::renderDT({
    
    table_data <- personalized_resorts() %>%
      slice_head(n = 10) %>%
      transmute(
        Resort = resort,
        Country = country,
        `Match score` = match_score,
        `Price (€)` = price_clean,
        `Total slopes (km)` = total_slopes,
        `Vertical drop (m)` = vertical_drop,
        `Longest run (km)` = longest_run,
        `Lift capacity` = lift_capacity_clean,
        `Night skiing` = nightskiing,
        Snowpark = snowparks
      )
    
    validate(
      need(
        nrow(table_data) > 0,
        "No resorts match the current preferences."
      )
    )
    
    DT::datatable(
      table_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "t",
        scrollX = TRUE
      )
    ) %>%
      DT::formatRound(
        columns = c(
          "Match score",
          "Price (€)",
          "Total slopes (km)",
          "Vertical drop (m)",
          "Longest run (km)",
          "Lift capacity"
        ),
        digits = 1
      )
  })
  
  
  # ==========================================================
  # Resort Profile
  # ==========================================================
  
  selected_resort <- reactive({
    
    req(input$profile_resort)
    
    resorts %>%
      filter(
        resort == input$profile_resort
      ) %>%
      slice_head(n = 1)
  })
  
  
  output$profile_heading <- renderUI({
    
    profile <- selected_resort()
    
    tagList(
      h3(profile$resort),
      
      p(
        class = "lead",
        paste(
          profile$country,
          "—",
          profile$continent
        )
      )
    )
  })
  
  
  output$profile_price <- renderText({
    
    profile <- selected_resort()
    
    if (
      nrow(profile) == 0 ||
      is.na(profile$price_clean[[1]])
    ) {
      "Not available"
    } else {
      paste0(
        "€",
        round(
          profile$price_clean[[1]],
          0
        )
      )
    }
  })
  
  
  output$profile_slopes <- renderText({
    
    profile <- selected_resort()
    
    if (
      nrow(profile) == 0 ||
      is.na(profile$total_slopes[[1]])
    ) {
      "Not available"
    } else {
      paste0(
        round(
          profile$total_slopes[[1]],
          0
        ),
        " km"
      )
    }
  })
  
  
  output$profile_vertical <- renderText({
    
    profile <- selected_resort()
    
    if (
      nrow(profile) == 0 ||
      is.na(profile$vertical_drop[[1]])
    ) {
      "Not available"
    } else {
      paste0(
        round(
          profile$vertical_drop[[1]],
          0
        ),
        " m"
      )
    }
  })
  
  
  output$profile_value <- renderText({
    
    profile <- selected_resort()
    
    if (
      nrow(profile) == 0 ||
      is.na(profile$value_score[[1]])
    ) {
      "Not available"
    } else {
      round(
        profile$value_score[[1]],
        1
      )
    }
  })
  
  
  output$terrain_plot <- renderPlot({
    
    profile <- selected_resort()
    
    terrain_data <- data.frame(
      Terrain = factor(
        c(
          "Beginner",
          "Intermediate",
          "Difficult"
        ),
        levels = c(
          "Beginner",
          "Intermediate",
          "Difficult"
        )
      ),
      
      Kilometers = c(
        profile$beginner_slopes[[1]],
        profile$intermediate_slopes[[1]],
        profile$difficult_slopes[[1]]
      )
    )
    
    terrain_data <- terrain_data %>%
      mutate(
        Kilometers = dplyr::coalesce(
          Kilometers,
          0
        )
      )
    
    ggplot(
      terrain_data,
      aes(
        x = Terrain,
        y = Kilometers,
        fill = Terrain
      )
    ) +
      geom_col() +
      labs(
        title = paste(
          "Terrain at",
          profile$resort[[1]]
        ),
        x = NULL,
        y = "Slope length (km)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none"
      )
  })
  
  
  output$profile_details <- renderTable({
    
    profile <- selected_resort()
    
    display_value <- function(value, suffix = "") {
      
      if (
        length(value) == 0 ||
        is.na(value)
      ) {
        return("Not available")
      }
      
      paste0(value, suffix)
    }
    
    data.frame(
      Feature = c(
        "Longest run",
        "Total lifts",
        "Lift capacity per hour",
        "Night skiing",
        "Snowpark",
        "Summer skiing",
        "Child friendly"
      ),
      
      Value = c(
        display_value(
          round(
            profile$longest_run[[1]],
            1
          ),
          " km"
        ),
        
        display_value(
          round(
            profile$total_lifts[[1]],
            0
          )
        ),
        
        display_value(
          round(
            profile$lift_capacity_clean[[1]],
            0
          )
        ),
        
        display_value(
          profile$nightskiing[[1]]
        ),
        
        display_value(
          profile$snowparks[[1]]
        ),
        
        display_value(
          profile$summer_skiing[[1]]
        ),
        
        display_value(
          profile$child_friendly[[1]]
        )
      )
    )
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "m")
}


# ============================================================
# Run application
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

