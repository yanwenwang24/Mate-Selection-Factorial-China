# Mate-Selection-Factorial-China

Replication package

Wang, Yanwen, Zheng Mu, Anni Ni, and Chan Zhang. 2025. “What Do Men and Women Value in a Romantic Partner? A Factorial Survey Experiment in Urban China.” (Under Review)

## Abstract

Amidst the deinstitutionalization of marriage, ideational factors have become more prominent in partner selection. However, the relative importance of ideations compared to sociodemographic characteristics, as well as their differences between genders, remains unclear. This study uses a factorial survey experiment with 1,225 never-married heterosexual respondents across four Chinese metropolises—Beijing, Shanghai, Guangzhou, and Shenzhen—to investigate how sociodemographic and ideational attributes influence perceptions of partner desirability in marital versus dating contexts. Results indicate that when seeking marriage partners, men prioritize fertility intention, whereas women emphasize gender role attitudes, socioeconomic prospects, and emotional support. In dating, however, both genders prioritize physical appearance. Gender differences were most pronounced regarding housework arrangements, with neither gender willing to assume a greater share; fertility intention, where a partner's unwillingness to have children is the strongest deterrent for men but inconsequential to women; and education, which women value more than men did. Overall, women are perceived as more desirable partners. These findings offer valuable insights into gender differences in relationship ideals and can inform interventions designed to bridge these divides.

## Replication procedures

Install [Julia](https://julialang.org/downloads/), [R](https://cran.r-project.org), and relevant packages. Follow these steps to replicate the study:

1. Clean survey data using the scripts in the [code/01_clean](./code/01_clean/) directory.
2. Run analysis using the scripts in the [code/02_analyze](./code/02_analyze/) directory. Fitted models are saved to `outputs/models/`.
3. Visualize results using the scripts in the [code/03_visualize](./code/03_visualize/) directory. Figures are saved to `outputs/figures/`.
4. Initialize the web app by running `shiny::runApp("app/app.r")` via R interface or visiting [Posit Cloud](https://yanwenwang24-predict-attractiveness.share.connect.posit.cloud/).
