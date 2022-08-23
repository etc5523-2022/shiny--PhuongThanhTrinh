library(shiny)
library(tidyverse)
library(ggplot2)
library(sf)
library(spData)
library(glue)
library(plotly)
library(leaflet)

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv") %>%
  drop_na(stars)

data(world, package = "spData")

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


countries <- sort(unique(ramen_ratings$country))

brands <- unique(ramen_ratings$brand)


ui <- fluidPage(
  mainPanel(
    htmlOutput("ramen")),
titlePanel(title=div(img(src="ramen.jpg", height = 150, weight = 150, style="display: block; margin-left: auto; margin-right: auto;"))),
  titlePanel(div("Ramen Rating", style="text-align: center;")),

  h3("1. How are ramen varieties distributed across selected countries?"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country",
                  choices = countries)),
    mainPanel(
      leafletOutput("map"),
      textOutput("proportion")
    )),
  h3("2. What are the top 10 the highest average rated kinds of ramen based on countries?"),

  sidebarLayout(
    sidebarPanel(
      helpText("Click here to explore top ramen variety across countries"),
      selectizeInput("guess", "Country of origin",
                     choices = countries)),
    mainPanel(
      plotOutput("top10"))),

  h3("3. How do the styles of ramen affect raters' preference towards a particular brand?"),
    sidebarLayout(
      sidebarPanel(
      radioButtons(
        "style", "Choose a style",
        choices = c("Cup", "Pack", "Tray", "Bowl", "Box", "Restaurant", "Can", "Bar"))),
    mainPanel(plotlyOutput("ramenstyle"))
    ),

  fluidRow(
    column(10,
           div(class = "about",
               uiOutput('about'))
    )
  ),
  includeCSS("styles.css")
)


server <- function(input, output){

  output$map <- renderLeaflet({

    ramen_distribution <- ramen_ratings %>%
      count(country) %>%
      mutate(prop = n/sum(n)*100)

    world <- world %>%
      select(c(name_long, geom)) %>%
      filter(name_long %in% countries)

    cpal <- colorNumeric(palette = "Accent", domain = ramen_distribution$prop)

    leaflet(world) %>%
      addTiles() %>%
      addPolygons(
        color = ~cpal(ramen_distribution$prop),
        stroke = FALSE,
        smoothFactor = 0.3,
        fillOpacity = 0.6) %>%
      addLegend(pal = cpal,
                values = ramen_distribution$prop,
                position = "bottomright")
  })

  output$proportion <- renderText({

    names <- ramen_distribution %>%
      filter(country==input$country)

    prop <- names %>%
      pull(prop) %>%
      round(digits = 2)

   glue("{input$country} accounts for {prop}% of ramen distribution internationally.")
  })

  output$top10 <- renderPlot({

    top10 <- ramen_ratings %>%
      filter(country == input$guess)

    top10 <- top10 %>%
      group_by(variety) %>%
      summarise(average_rate = mean(stars)) %>%
      arrange(desc(average_rate)) %>%
      head(10)

     ggplot(top10, aes(x = average_rate,
                      y = reorder(variety, average_rate))) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(x = "Average rate", y = "Variety") +
      ggtitle("Top 10 highest rated ramen varieties") +
      theme_classic()
  })


  output$ramenstyle <- renderPlotly({

      ratings <- ramen_ratings %>%
        filter(style == input$style)

    ratings <- ratings %>%
      group_by(brand) %>%
      summarise(mean = mean(stars)) %>%
      arrange(desc(mean)) %>%
      head(20)

    e <- ggplot(ratings, aes(x = mean, y = reorder(brand, mean), fill = brand)) +
        geom_bar(stat = "identity") +
      labs(x = "Average ratings",
           y = "Brands") +
      ggtitle("Average ratings of top 20 brands based on selected styles") +
      theme_classic()
    ggplotly(e)
})

  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })
}


shinyApp(ui = ui, server = server)
