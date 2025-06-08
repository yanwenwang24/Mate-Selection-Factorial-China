# Mate-Selection-Factorial-China

Replication package

Wang, Yanwen, Zheng Mu, Anni Ni, and Chan Zhang. 2025. “What Do Men and Women Value in a Romantic Partner? A Factorial Survey Experiment in Urban China.” (Under Review)

## Abstract

**Objective:** This study investigates the factors influencing perceived attractiveness of a romantic partner in urban China.

**Background:** Amidst the deinstitutionalization of marriage, ideational factors have come to play a more central role in partner selection. Concurrently, gender differences in relationship expectations have intensified.

**Method:** A factorial survey experiment was conducted to investigate how sociodemographic and ideational attributes influence perceptions of partner attractiveness in both marital and dating contexts. Data were collected between April and May 2025 from a sample of 1,225 never-married heterosexual respondents aged 25–34 across four metropolises: Beijing, Shanghai, Guangzhou, and Shenzhen.

**Results:** Men prioritized fertility intentions of marriage partners and physical appearance of dating partners, whereas women placed greater emphasis on gender role attitudes, socioeconomic prospects, and emotional support of marriage partners as well as physical appearance of dating partners. Gender differences were most evident in perceptions of housework arrangements, with neither gender willing to assume a greater share; in fertility intentions, where a partner’s unwillingness to have children was the strongest deterrent to marital attractiveness from men’s perspective but inconsequential to women; and in educational attainment, which women valued more than men did. Overall, women were perceived as more attractive in both marital and dating contexts.

**Conclusion:** Profound gender differences in relationship ideals are likely driving the retreat from marriage in urban China. Bridging these gaps is crucial to alleviating such tensions.

## Replication procedures

Install [Julia](https://julialang.org/downloads/), [R](https://cran.r-project.org), and relevant packages. Follow these steps to replicate the study:

1. Clean survey data using the scripts in the [scripts_tidy](./scripts_tidy/) directory.
2. Run analysis using the scripts in the [scripts_analysis](./scripts_analysis/) directory.
3. Visualize results using the scripts in the [scripts_visualize](./script_visualize) directory.
4. Initialize the web app by running `shiny::runApp("app/app.r")` via R interface or visiting [Posit Cloud](https://yanwenwang24-predict-attractiveness.share.connect.posit.cloud/).
