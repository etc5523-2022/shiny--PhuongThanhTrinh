library(shiny)
library(tidyverse)
library(ggplot2)
library(sf)
library(spData)
library(glue)
library(plotly)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv") %>%
  drop_na(stars)

data(world, package = "spData")

world <- world %>%
  select(c(name_long, continent, geom))

ramen_ratings <- ramen_ratings %>%
  mutate(country = ifelse(country == "Hong Kong", "China", country),
         country = ifelse(country == "Russia", "Russian Federation", country),
         country = ifelse(country == "Holland", "Netherlands", country),
         country = ifelse(country == "Dubai", "United Arab Emirates", country),
         country = ifelse(country == "South Korea", "Republic of Korea", country),
         country = ifelse(country == "Singapore", "Malaysia", country),
         country = ifelse(country == "Sarawak", "Malaysia", country),
         country = ifelse(country == "USA", "United States", country),
         country = ifelse(country == "UK", "United Kingdom", country))

