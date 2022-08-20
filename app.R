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

ramen_distribution <- ramen_ratings %>%
  group_by(country) %>%
  summarise(prop = n()/nrow(ramen_ratings)*100) %>%
  left_join(world, c("country" = "name_long"))


countries <- sort(unique(ramen_ratings$country))

brands <- unique(ramen_ratings$brand)

styles <- unique(ramen_ratings$style)

ui <- fluidPage(
  titlePanel("Ramen Reviews"),
  h3("1. How are ramen manufacturer distributed across countries?"),
  sidebarLayout(
    sidebarPanel(
      h1("Country"),
      selectInput("country", "Select a country",
                  choices = countries)),
    mainPanel(
      plotOutput("map"),
      textOutput("proportion")
    )
  )
)





server <- function(input, output) {

  output$map <- renderPlot({
    ggplot(ramen_distribution) +
      geom_sf(aes(geometry = geom, fill = prop),
              color = "black") +
      scale_fill_distiller(palette = "Accent") +
      theme(legend.title = element_blank(), legend.position = "bottom")
  })

  output$proportion <- renderText({

    names <- ramen_distribution %>%
      filter(country==input$country)

    prop <- names %>%
      pull(prop) %>%
      round(digits = 2)

    glue("{input$country} accounts for {prop}% of ramen manufacturers internationally.")
  })

}

shinyApp(ui = ui, server = server)
