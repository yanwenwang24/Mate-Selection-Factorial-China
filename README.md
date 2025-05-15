# Mate-Selection-Factorial-China

Replication package

Wang, Yanwen, Zheng Mu, Anni Ni, and Chan Zhang. 2025. “What Do Men and Women Value in a Romantic Partner? A Factorial Survey Experiment in Urban China.” (Working Paper)

## Abstract

**Objective:** This study investigates factors influencing perceived attractiveness of a romantic partner in urban China.

**Background:** As marriage becomes increasingly deinstitutionalized, ideational factors have become more important in partner selection. Concurrently, tensions between genders surrounding relationship expectations have intensified.

**Method:** A factorial survey was conducted to investigate how sociodemographic and ideational attributes influence perceptions of partner attractiveness in both marital and dating contexts. Data were collected between April and May 2025 from a sample of 1,200 never-married heterosexual respondents across four major cities: Beijing, Shanghai, Guangzhou, and Shenzhen.

**Results:** Men prioritized fertility intentions for marriage and physical appearance for dating, whereas women placed greater emphasis on gender role attitudes, socioeconomic prospects, and emotional support. Gender tensions were most pronounced in perceptions of housework arrangements, with neither party willing to bear a greater share; fertility intentions, where a partner’s unwillingness to have children was the strongest deterrent to marital attractiveness for men but inconsequential for women; and educational attainment, which women valued more than men did. Overall, women were perceived as more attractive than men in both marital and dating contexts.

**Conclusion:** Profound gender disagreements over relationship ideals likely drive the retreat from marriage in urban China. Bridging these gaps is crucial to alleviating such tensions.

## Replication procedures

Install [Julia](https://julialang.org/downloads/), [R](https://cran.r-project.org), and relevant packages. Follow these steps to replicate the study:

1. Clean survey data using the scripts in the [scripts_tidy](./scripts_tidy/) directory.
2. Run analysis using the scripts in the [scripts_analysis](./scripts_analysis/) directory.
3. Visualize results using the scripts in the [scripts_visualize](./script_visualize) directory.
4. Initialize the web app by running `shiny::runApp("app/app.r")` via R interface or visiting [Posit Cloud](https://yanwenwang24-predict-attractiveness.share.connect.posit.cloud/).
