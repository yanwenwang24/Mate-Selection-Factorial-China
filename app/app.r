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
##
## ------------------------------------------------------------------------

# 1 Load packages and models ----------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org/"))

# List of packages to check/install
library(arrow)
library(lme4)
library(shiny)
library(tidyverse)

# Load models
mods_predict <- readRDS("mods_predict.rds")
mod_marriage_men <- mods_predict$mod_marriage_men # Men evaluating women
mod_dating_men <- mods_predict$mod_dating_men
mod_marriage_women <- mods_predict$mod_marriage_women # Women evaluating men
mod_dating_women <- mods_predict$mod_dating_women

# Load percentile data
pred_df <- readRDS("pred_df.rds")

# 2 Define UI -------------------------------------------------------------

ui <- fluidPage(
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }
      .well {
        background-color: #f8f9fa;
        border: none;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .btn-primary {
        background-color: #7eb0d5;
        border-color: #7eb0d5;
      }
      .btn-primary:hover {
        background-color: #6a9bc0;
        border-color: #6a9bc0;
      }
      h2 {
        font-weight: 500;
        color: #333;
        margin-bottom: 20px;
      }
      .main-heading-adjust {
        margin-top: 40px;
      }
      h3 {
        font-weight: 500;
        color: #444;
      }
      .score-box {
        text-align: center;
        padding: 20px;
        margin-bottom: 15px;
        border-radius: 8px;
        background-color: white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .big-number {
        font-size: 3em;
        font-weight: bold;
      }
      .marriage {
        color: #7eb0d5;
      }
      .dating {
        color: #fd7f6f;
      }
    "))
  ),

  # Title
  titlePanel("How Attractive Are You as a Romantic Partner?"),

  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      h2("About You"),

      # Gender
      radioButtons("gender", "Gender:",
        choices = c("Man" = "men", "Woman" = "women"),
        selected = "men"
      ),

      # Age
      selectInput("v_age", "Age vs. potential partner:",
        choices = c(
          "About the same" = "similar",
          "Older" = "older",
          "Younger" = "younger"
        ),
        selected = "similar"
      ),

      # Education
      selectInput("v_educ", "Highest education:",
        choices = c(
          "Secondary or below" = "secondary",
          "Bachelor's degree" = "bachelor",
          "Graduate degree or above" = "graduate"
        ),
        selected = "bachelor"
      ),

      # Income
      selectInput("v_income", "Income level:",
        choices = c(
          "Below average" = "below average",
          "Average" = "average",
          "Above average" = "above average"
        ),
        selected = "average"
      ),

      # Nativity
      radioButtons("v_nativity", "Are you a local or a migrant?",
        choices = c(
          "Local" = "local",
          "Migrant" = "migrant"
        ),
        selected = "local"
      ),

      # Appearance
      selectInput("v_appearance", "Appearance:",
        choices = c(
          "Below average" = "below average",
          "Average" = "average",
          "Above average" = "above average"
        ),
        selected = "average"
      ),

      # Housework
      selectInput("v_housework", "Attitude on housework division:",
        choices = c(
          "Equal sharing" = "equal",
          "Women do more" = "women do more",
          "Men do more" = "men do more"
        ),
        selected = "equal"
      ),

      # Career support
      selectInput("v_career", "Career support for partner:",
        choices = c(
          "Low" = "low",
          "Moderate" = "moderate",
          "Strong" = "strong"
        ),
        selected = "moderate"
      ),

      # Emotional support
      selectInput("v_emotion", "Emotional support for partner:",
        choices = c(
          "Low" = "low",
          "Moderate" = "moderate",
          "Strong" = "strong"
        ),
        selected = "moderate"
      ),

      # Fertility
      selectInput("v_fertility", "Desire for children:",
        choices = c(
          "Yes" = "yes",
          "No" = "no",
          "Uncertain" = "uncertain"
        ),
        selected = "uncertain"
      ),

      # Predict button
      actionButton("predict", "Predict Attractiveness",
        class = "btn-primary btn-block"
      )
    ),

    # Main panel with results
    mainPanel(
      uiOutput("results_panel")
    )
  )
)

# 3 Define server logic ---------------------------------------------------

server <- function(input, output, session) {
  # Reactive values to store predictions
  results <- reactiveValues(
    marriage_score = NULL,
    dating_score = NULL,
    marriage_percentile = NULL,
    dating_percentile = NULL
  )

  # Calculate predictions when button is clicked
  observeEvent(input$predict, {
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
    if (is.null(results$marriage_score)) {
      # Initial state
      return(
        div(
          h2("Your Attractiveness Prediction", class = "main-heading-adjust"),
          p("Enter your characteristics on the left, then click 'Predict Attractiveness' to see how attractive you might be perceived by the opposite sex for marriage and dating."),
          p("This prediction is based on a population-based factorial survey experiment conducted in four major Chinese cities: Beijing, Shanghai, Guangzhou, and Shenzhen.")
        )
      )
    } else {
      # Results state
      opposite_gender <- ifelse(input$gender == "men", "women", "men")

      return(
        div(
          h2("Your Attractiveness Prediction", class = "main-heading-adjust"),
          p(paste0("Here's how you might be perceived by ", opposite_gender, ":")),
          fluidRow(
            column(
              6,
              div(
                class = "score-box",
                h3("Marriage Potential", class = "marriage"),
                div(class = "big-number marriage", round(results$marriage_score, 1)),
                p("out of 10"),
                hr(),
                p(paste0("You score higher than ", results$marriage_percentile, "% of other ", input$gender, " in marriage potential"))
              )
            ),
            column(
              6,
              div(
                class = "score-box",
                h3("Dating Potential", class = "dating"),
                div(class = "big-number dating", round(results$dating_score, 1)),
                p("out of 10"),
                hr(),
                p(paste0("You score higher than ", results$dating_percentile, "% of other ", input$gender, " in dating potential"))
              )
            )
          ),
          div(
            style = "margin-top: 30px;",
            h3("Understanding Your Score"),
            p("These scores predict how members of the opposite sex might rate your attractiveness for marriage or dating based on the characteristics you entered."),
            p("The percentile rank indicates how your predicted attractiveness compares to others of the same gender within the study sample."),
            p("Remember, these predictions reflect general patterns observed in the study sample. Individual preferences can vary significantly.")
          )
        )
      )
    }
  })
}

# 4 Run the app -----------------------------------------------------------

shinyApp(ui = ui, server = server)
