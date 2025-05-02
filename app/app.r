## ------------------------------------------------------------------------
##
## Script name: app.r
## Purpose: Run the Shiny app
## Author: Yanwen Wang
## Date Created: 2025-04-29
## Email: yanwenwang@u.nus.edu
##
## ------------------------------------------------------------------------
##
## Notes:
## Web app predicting attractiveness of individuals as romantic partners
##
## ------------------------------------------------------------------------

# 1 Load packages, models, and translations -------------------------------

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load packages
library(arrow)
library(lme4)
library(shiny)
library(shinyjs)
library(tidyverse)

# Load models
mods_predict <- readRDS("mods_predict.rds")
mod_marriage_men <- mods_predict$mod_marriage_men # Men evaluating women
mod_dating_men <- mods_predict$mod_dating_men
mod_marriage_women <- mods_predict$mod_marriage_women # Women evaluating men
mod_dating_women <- mods_predict$mod_dating_women

# Load percentile data
pred_df <- readRDS("pred_df.rds")

# Source translations
source("translations.r", local = TRUE)

# 2 Define UI -------------------------------------------------------------

ui <- fluidPage(
  # Enable shinyjs
  useShinyjs(),
  # Add head tag to link the CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  # Language switcher
  div(
    style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
    actionButton("lang_en", "English", style = "margin-left: 5px;"),
    actionButton("lang_cn", "中文", style = "margin-left: 5px;")
  ),

  # Title
  uiOutput("title_panel_ui"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Use uiOutput for dynamic labels/choices
      uiOutput("sidebar_content_ui")
    ),

    # Main panel with results
    mainPanel(
      uiOutput("results_panel")
    )
  )
)

# 3 Define server logic ---------------------------------------------------

server <- function(input, output, session) {
  # Reactive value for current language, default to Chinese ('cn')
  current_lang <- reactiveVal("cn")

  # Translator function
  t_ <- function(key) {
    translation <- translations[[key]]
    if (is.null(translation)) {
      warning(paste("Translation key not found:", key))
      return(key) # Return key if translation missing
    }
    lang_val <- current_lang()
    # Fallback: if translation missing, use 'en', if 'en' missing, return key
    selected_translation <- translation[[lang_val]]
    if (is.null(selected_translation)) {
      warning(
        paste(
          "Translation for lang '",
          lang_val,
          "' not found for key: '",
          key, "'. ",
          "Falling back to 'en'.",
          sep = ""
        )
      )
      selected_translation <- translation[["en"]]
      if (is.null(selected_translation)) {
        warning(
          paste(
            "Fallback 'en' translation not found for key: '",
            key, "'. Returning key.",
            sep = ""
          )
        )
        return(key)
      }
    }
    selected_translation
  }

  # Update language when buttons are clicked
  observeEvent(input$lang_en, {
    current_lang("en")
    # Add/remove active class
    shinyjs::removeClass(selector = "#lang_cn", class = "btn-lang-active")
    shinyjs::addClass(selector = "#lang_en", class = "btn-lang-active")
  })
  observeEvent(input$lang_cn, {
    current_lang("cn")
    # Add/remove active class
    shinyjs::removeClass(selector = "#lang_en", class = "btn-lang-active")
    shinyjs::addClass(selector = "#lang_cn", class = "btn-lang-active")
  })

  # Set initial active button class and ensure it updates on language change
  observe({
    if (current_lang() == "cn") {
      shinyjs::addClass(selector = "#lang_cn", class = "btn-lang-active")
      shinyjs::removeClass(selector = "#lang_en", class = "btn-lang-active")
    } else {
      shinyjs::addClass(selector = "#lang_en", class = "btn-lang-active")
      shinyjs::removeClass(selector = "#lang_cn", class = "btn-lang-active")
    }
  }) |> bindEvent(current_lang(), ignoreInit = FALSE)


  # Render Title Panel dynamically
  output$title_panel_ui <- renderUI({
    req(current_lang())
    titlePanel(t_("app_title"))
  })

  # Render Sidebar Content dynamically
  output$sidebar_content_ui <- renderUI({
    req(current_lang())
    lang <- current_lang() # Get current language

    # Define choices dynamically
    age_choices <- setNames(
      c("", "similar", "older", "younger"),
      c(
        t_("choose_one"),
        t_("age_similar"),
        t_("age_older"),
        t_("age_younger")
      )
    )
    educ_choices <- setNames(
      c("", "secondary", "bachelor", "graduate"),
      c(
        t_("choose_one"),
        t_("educ_secondary"),
        t_("educ_bachelor"),
        t_("educ_graduate")
      )
    )
    income_choices <- setNames(
      c("", "below average", "average", "above average"),
      c(
        t_("choose_one"),
        t_("income_below"),
        t_("income_average"),
        t_("income_above")
      )
    )
    appearance_choices <- setNames(
      c("", "below average", "average", "above average"),
      c(
        t_("choose_one"),
        t_("appearance_below"),
        t_("appearance_average"),
        t_("appearance_above")
      )
    )
    housework_choices <- setNames(
      c("", "equal", "women do more", "men do more"),
      c(
        t_("choose_one"),
        t_("housework_equal"),
        t_("housework_women_do_more"),
        t_("housework_men_do_more")
      )
    )
    career_choices <- setNames(
      c("", "low", "moderate", "strong"),
      c(
        t_("choose_one"),
        t_("support_low"),
        t_("support_moderate"),
        t_("support_strong")
      )
    )
    emotion_choices <- setNames(
      c("", "low", "moderate", "strong"),
      c(
        t_("choose_one"),
        t_("support_low"),
        t_("support_moderate"),
        t_("support_strong")
      )
    )
    fertility_choices <- setNames(
      c("", "yes", "no", "uncertain"),
      c(
        t_("choose_one"),
        t_("fertility_yes"),
        t_("fertility_no"),
        t_("fertility_uncertain")
      )
    )

    # Keep track of selected values
    selected_gender <- isolate(input$gender)
    selected_age <- isolate(input$v_age)
    selected_educ <- isolate(input$v_educ)
    selected_income <- isolate(input$v_income)
    selected_nativity <- isolate(input$v_nativity)
    selected_appearance <- isolate(input$v_appearance)
    selected_housework <- isolate(input$v_housework)
    selected_career <- isolate(input$v_career)
    selected_emotion <- isolate(input$v_emotion)
    selected_fertility <- isolate(input$v_fertility)

    tagList(
      h2(t_("about_you")),
      div(class = "required-note", t_("required_note")),
      radioButtons("gender", t_("gender"),
        choices = setNames(c("men", "women"), c(t_("man"), t_("woman"))),
        selected = selected_gender %||% character(0)
      ),
      selectInput(
        "v_age", t_("v_age"),
        choices = age_choices,
        selected = selected_age %||% ""
      ),
      selectInput(
        "v_educ", t_("v_educ"),
        choices = educ_choices,
        selected = selected_educ %||% ""
      ),
      selectInput(
        "v_income", t_("v_income"),
        choices = income_choices,
        selected = selected_income %||% ""
      ),
      radioButtons(
        "v_nativity", t_("v_nativity"),
        choices = setNames(
          c("local", "migrant"),
          c(t_("nativity_local"), t_("nativity_migrant"))
        ),
        selected = selected_nativity %||% character(0)
      ),
      selectInput(
        "v_appearance", t_("v_appearance"),
        choices = appearance_choices,
        selected = selected_appearance %||% ""
      ),
      selectInput(
        "v_housework", t_("v_housework"),
        choices = housework_choices,
        selected = selected_housework %||% ""
      ),
      selectInput(
        "v_career", t_("v_career"),
        choices = career_choices,
        selected = selected_career %||% ""
      ),
      selectInput(
        "v_emotion", t_("v_emotion"),
        choices = emotion_choices,
        selected = selected_emotion %||% ""
      ),
      selectInput(
        "v_fertility", t_("v_fertility"),
        choices = fertility_choices,
        selected = selected_fertility %||% ""
      ),
      actionButton(
        "predict", t_("predict_button"),
        class = "btn-primary btn-block"
      )
    )
  })

  # Store predictions
  results <- reactiveValues(
    marriage_score = NULL,
    dating_score = NULL,
    marriage_percentile = NULL,
    dating_percentile = NULL
  )

  # Check if all fields are filled
  all_inputs_filled <- reactive({
    req(
      input$gender,
      input$v_age,
      input$v_educ,
      input$v_income,
      input$v_nativity,
      input$v_appearance,
      input$v_housework,
      input$v_career,
      input$v_emotion,
      input$v_fertility
    )

    # Check specifically for non-empty string selections in dropdowns
    !is.null(input$gender) && input$gender != "" &&
      !is.null(input$v_age) && input$v_age != "" &&
      !is.null(input$v_educ) && input$v_educ != "" &&
      !is.null(input$v_income) && input$v_income != "" &&
      !is.null(input$v_nativity) && input$v_nativity != "" &&
      !is.null(input$v_appearance) && input$v_appearance != "" &&
      !is.null(input$v_housework) && input$v_housework != "" &&
      !is.null(input$v_career) && input$v_career != "" &&
      !is.null(input$v_emotion) && input$v_emotion != "" &&
      !is.null(input$v_fertility) && input$v_fertility != ""
  })

  # Enable/disable predict button based on input validation
  observe({
    # Need current_lang() to update button text
    current_lang()
    # Update button text explicitly if it exists
    session$sendCustomMessage(type = "jsCode", list(code = paste0(
      "if ($('#predict').length) { ",
      "  $('#predict').text('", t_("predict_button"), "');",
      "}"
    )))

    # Toggle state
    shinyjs::toggleState(
      id = "predict",
      condition = all_inputs_filled()
    )
  })

  # Calculate predictions when button is clicked
  observeEvent(input$predict, {
    # Validate all fields are filled
    req(all_inputs_filled())

    # Create new data frame for prediction
    new_data <- data.frame(
      id = 1,
      v_age = input$v_age,
      v_educ = input$v_educ,
      v_income = input$v_income,
      v_nativity = input$v_nativity,
      v_appearance = input$v_appearance,
      v_housework = input$v_housework,
      v_career = input$v_career,
      v_emotion = input$v_emotion,
      v_fertility = input$v_fertility
    )

    # Predict based on gender
    if (input$gender == "men") {
      # Use women's models to predict men's attractiveness
      results$marriage_score <- predict(
        mod_marriage_women, new_data,
        re.form = NA
      )
      results$dating_score <- predict(
        mod_dating_women, new_data,
        re.form = NA
      )

      # Calculate percentiles
      men_marriage_scores <- pred_df %>%
        filter(gender == "Men" & type == "Marriage") %>%
        pull(pred)
      men_dating_scores <- pred_df %>%
        filter(gender == "Men" & type == "Dating") %>%
        pull(pred)

      results$marriage_percentile <- round(
        mean(results$marriage_score > men_marriage_scores) * 100
      )
      results$dating_percentile <- round(
        mean(results$dating_score > men_dating_scores) * 100
      )
    } else {
      # Use men's models to predict women's attractiveness
      results$marriage_score <- predict(
        mod_marriage_men, new_data,
        re.form = NA
      )
      results$dating_score <- predict(
        mod_dating_men, new_data,
        re.form = NA
      )

      # Calculate percentiles
      women_marriage_scores <- pred_df %>%
        filter(gender == "Women" & type == "Marriage") %>%
        pull(pred)
      women_dating_scores <- pred_df %>%
        filter(gender == "Women" & type == "Dating") %>%
        pull(pred)

      results$marriage_percentile <- round(
        mean(results$marriage_score > women_marriage_scores) * 100
      )
      results$dating_percentile <- round(
        mean(results$dating_score > women_dating_scores) * 100
      )
    }
  })

  # Generate results panel
  output$results_panel <- renderUI({
    req(current_lang())
    lang <- current_lang()

    if (is.null(results$marriage_score)) {
      # Initial state
      return(
        div(
          h2(
            t_("results_panel_initial_title"),
            class = "main-heading-adjust"
          ),
          p(t_("results_panel_initial_p1")),
          p(t_("results_panel_initial_p2"))
        )
      )
    } else {
      # Results state
      req(input$gender)

      # Prepare percentile messages
      marriage_percentile_msg <- t_("percentile_msg") |>
        str_replace(
          fixed("__percentile__"),
          as.character(results$marriage_percentile)
        ) |>
        str_replace(
          fixed("__gender__"),
          t_(input$gender)
        ) |>
        str_replace(
          fixed("__potential_type__"),
          t_("potential_type_marriage")
        )

      dating_percentile_msg <- t_("percentile_msg") |>
        str_replace(
          fixed("__percentile__"),
          as.character(results$dating_percentile)
        ) |>
        str_replace(
          fixed("__gender__"),
          t_(input$gender)
        ) |>
        str_replace(
          fixed("__potential_type__"),
          t_("potential_type_dating")
        )

      perceived_by_msg <- t_("results_panel_perceived_by")


      return(
        div(
          h2(
            t_("results_panel_results_title"),
            class = "main-heading-adjust"
          ),
          p(perceived_by_msg),
          fluidRow(
            column(
              6,
              div(
                class = "score-box",
                h3(t_("marriage_potential"), class = "marriage"),
                div(
                  class = "big-number marriage",
                  round(results$marriage_score, 1)
                ),
                p(t_("out_of_10")),
                hr(),
                p(marriage_percentile_msg)
              )
            ),
            column(
              6,
              div(
                class = "score-box",
                h3(t_("dating_potential"), class = "dating"),
                div(
                  class = "big-number dating", round(results$dating_score, 1)
                ),
                p(t_("out_of_10")),
                hr(),
                p(dating_percentile_msg)
              )
            )
          ),
          div(
            style = "margin-top: 30px;",
            h3(t_("understanding_score_title")),
            p(t_("understanding_score_p1")),
            p(t_("understanding_score_p2")),
            p(
              strong(t_("understanding_score_disclaimer_strong")),
              t_("understanding_score_disclaimer")
            )
          )
        )
      )
    }
  })
}

# 4 Run the app -----------------------------------------------------------

shinyApp(ui = ui, server = server)
