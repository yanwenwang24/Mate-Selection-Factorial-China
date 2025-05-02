# Mate-Selection-Factorial-China

Replication package

Wang, Yanwen, Anni Ni, Zheng Mu, and Chan Zhang. 2025. “What Do Men and Women Value in a Romantic Partner? A Factorial Survey Experiment in Urban China.” (Working Paper)

## Replication procedures

Install [Julia](https://julialang.org/downloads/), [R](https://cran.r-project.org), and relevant packages. Follow these steps to replicate the study:

1. Clean survey data using the scripts in the [scripts_tidy](./scripts_tidy/) directory.
2. Run analysis using the scripts in the [scripts_analysis](./scripts_analysis/) directory.
3. Visualize results using the scripts in the [scripts_visualize](./script_visualize) directory.
4. Initialize the web app by running `shiny::runApp("app/app.r")` via R interface or visiting [Posit Cloud](https://yanwenwang24-predict-attractiveness.share.connect.posit.cloud/).
