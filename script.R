library(tidyverse)
library(readxl)
library(plotly)


data <- read_excel("./WHR2018Chapter2OnlineData.xls", sheet = "Table2.1") %>%
  select(1:9) %>%
  filter(year == "2017") %>%
  na.omit()


happiness <- read_excel("./WHR2018Chapter2OnlineData.xls", sheet = "Figure2.2") %>%
  select(1,2) %>%
  rename(country = Country,
         happiness_score = "Happiness score")


colnames(data) <- gsub(x = names(data), pattern = " ", replacement = "_")
colnames(data) <- tolower(names(data))


data <- left_join(data, happiness, by = "country") %>%
  mutate(health = round(healthy_life_expectancy_at_birth, 2),
         happiness_score = round(happiness_score, 2),
         freedom = round(freedom_to_make_life_choices*10, 2),
         gdp = round(log_gdp_per_capita, 2))

make_plot <- function(){
  plot <- plot_ly(data = data, y = ~happiness_score, x = ~gdp, type = "scatter",
          mode = "markers", color = ~freedom,
          size = ~(health^3),
          name = ~country, hoverinfo = "text",
          text = ~paste(country,
                        "\n Happiness: ", happiness_score,
                        "\n GDP: ", gdp,
                        "\n Freedom: ", freedom,
                        "\n Life Expectancy: ", health)) %>%
    colorbar(title = "Freedom") %>%
    layout(title = "Happiness seen against Economy, Freedom & Health",
           xaxis = list(title = "Economy (log GDP per capita)"),
           yaxis = list(title = "Happiness Score"),
           showlegend = F)
  return(plot)
}

