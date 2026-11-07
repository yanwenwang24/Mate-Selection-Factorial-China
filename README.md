# Mate-Selection-Factorial-China

Replication package

Wang, Yanwen, Zheng Mu, Anni Ni, and Chan Zhang. 2026. “Partner Preferences Among Never-Married Women and Men: A Factorial Survey Experiment in Urban China.” *Family Relations*.

## Replication procedures

Install [Julia](https://julialang.org/downloads/), [R](https://cran.r-project.org), and relevant packages. Follow these steps to replicate the study:

1. Clean survey data using the scripts in the [code/01_clean](./code/01_clean/) directory.
2. Run analysis using the scripts in the [code/02_analyze](./code/02_analyze/) directory. Fitted models are saved to `outputs/models/`.
3. Visualize results using the scripts in the [code/03_visualize](./code/03_visualize/) directory. Figures are saved to `outputs/figures/`.
4. Initialize the web app by running `shiny::runApp("app/app.r")` via R interface or visiting [Posit Cloud](https://yanwenwang24-predict-attractiveness.share.connect.posit.cloud/).
