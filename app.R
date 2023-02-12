#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load data and libraries

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(shinythemes)) install.packages('shinythemes')
library(shinythemes)

if (!require(openintro)) install.packages('openintro')
library(openintro)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require("rnaturalearth")) install.packages("rnaturalearth")
library("rnaturalearth")

if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
library("rnaturalearthdata")

if (!require(colorspace)) install.packages("colorspace")
library(colorspace)

if (!require(shinyWidgets)) install.packages("shinyWidgets")
library(shinyWidgets)

if (!require(gapminder)) install.packages("gapminder")
library(gapminder)

if (!require(ggthemes)) install.packages("ggthemes")
library(ggthemes)

if (!require(gifski)) install.packages("gifski")
library(gifski)

if (!require(gganimate)) install.packages("gganimate")
library(gganimate)



#Creating world map
world <- ne_countries(scale = "medium", returnclass = "sf")

#importing COVID data 
covid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

my_map_theme <- function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

tmp = unique(covid$continent)
cont_list <- append(tmp,'World',after=0)

tmp1 <- covid %>%
  filter(iso_code != "OWID_AFR" ) %>%
  filter(iso_code != "OWID_ASI") %>%
  filter(iso_code != "OWID_ENG") %>%
  filter(iso_code != "OWID_EUR") %>%
  filter(iso_code != "OWID_EUN") %>%
  filter(iso_code != "OWID_INT") %>%
  filter(iso_code != "OWID_HIC") %>%
  filter(iso_code != "OWID_KOS") %>%
  filter(iso_code != "OWID_LIC") %>%
  filter(iso_code != "OWID_LMC") %>%
  filter(iso_code != "OWID_NAM") %>%
  filter(iso_code != "OWID_CYN") %>%
  filter(iso_code != "OWID_NIR") %>%
  filter(iso_code != "OWID_OCE") %>%
  filter(iso_code != "OWID_SCT") %>%
  filter(iso_code != "OWID_SAM") %>%
  filter(iso_code != "OWID_UMC") %>%
  filter(iso_code != "OWID_WRL") %>%
  filter(iso_code != "OWID_WLS") 

country_list <- unique(tmp1$location)

#variable to create graph on, date of graph, colorblind/normal/B&W, show full world map or filter for country
make_map <- function(map_variable, map_date, n_cb_bw, world_or_filter) {
  
  graph_title <- map_variable
  per_x <- " "
  if(map_variable == "Total Cases - Per Million"){
    map_variable <- "total_cases_per_million"
    per_x <- "Count\nPer Million"
  }
  else if (map_variable == "New Cases - Per Million"){
    map_variable <- "new_cases_per_million"
    per_x <- "Count\nPer Million"
  }
  else if (map_variable == "Total Deaths - Per Million"){
    map_variable <- "total_deaths_per_million"
    per_x <- "Count\nPer Million"
  }
  else if(map_variable == "New Deaths - Per Million"){
    map_variable <- "new_deaths_per_million"
    per_x <- "Count\nPer Million"
  }
  else if(map_variable == "ICU Patients - Per Million"){
    map_variable <- "icu_patients_per_million"
      per_x <- "Count\nPer Million"
  }
  else if(map_variable == "Hospital Patients - Per Million"){
    map_variable <- "hosp_patients_per_million"
    per_x <- "Count\nPer Million"
  }
  else if(map_variable == "Total Tests - Per Thousand"){
    map_variable <- "total_tests_per_thousand"
    per_x <- "Count\nPer Thousand"
  }
  else if(map_variable == "New Tests - Per Thousand"){
    map_variable <- "new_tests_per_thousand"
    per_x <- "Count\nPer Thousand" 
  }
  else if(map_variable == "Total Vaccinations - Per Hundred"){
    map_variable <- "total_vaccinations_per_hundred"
    per_x <- "Count\nPer Hundred"
  }
  else if(map_variable == "People Fully Vaccinated - Per Hundred"){
    map_variable <- "people_fully_vaccinated_per_hundred"
    per_x <- "Count\nPer Hundred" 
  }
  else if(map_variable == "People Vaccinated - Per Hundred"){
    map_variable <- "people_vaccinated_per_hundred"
    per_x <- "Count\nPer Hundred" 
  }
  
  #colorblind accessibility (work in progress!)
  color_palette <- "Heat"
  na <- "white"
  
  if(n_cb_bw == "Color Vision Deficiency (Red Scale)"){
    color_palette <- "Reds 3"
    na <- "gray"
  }
  else if(n_cb_bw == "Black and White"){
    color_palette <- "Light Grays"
    na <- "black"
  }
  else if(n_cb_bw == "Color Vision Deficiency (Orange-Blue)"){
    na <- "gray"
    color_palette <- "Plasma"
  }
  
  #subset of needed info for graph
  covid_subset <- covid %>%
    select(iso_code, continent, location, date, map_variable)
  
  #filter for a specific date 
  covid_subset <- covid_subset %>%
    filter(date == map_date)
  
  #get world info for subtitle
  covid_world_values <- covid_subset %>%
    filter(location == "World")
  
  #Left join between COVID data subset and the map object data
  world_covid <- world %>%
    left_join(covid_subset, by = c("iso_a3" = "iso_code"))
  
  if(world_or_filter != "World"){
    world_covid <- world_covid %>%
      filter(continent.y == world_or_filter)
  }
  
  #get min and max value for variable for color legend
  highest_value_row <- world_covid %>% 
    arrange(desc(world_covid[[map_variable]])) %>%
    head(1)
  highest_value <- highest_value_row[[map_variable]]
  highest_value <- round(highest_value, digits=0)-10
  
  lowest_value_row <- world_covid %>% 
    arrange(world_covid[[map_variable]]) %>%
    head(1)
  lowest_value <- lowest_value_row[[map_variable]]
  lowest_value <- round(lowest_value, digits = 0) + 10
  
  map <- ggplot(world_covid) +
    geom_sf(aes(
      fill=world_covid[[map_variable]]+runif(nrow(world_covid), min = 0.5, max = 0.75),
      text=paste("Count:",world_covid[[map_variable]],"\nCountry:",admin)), 
      color="black") +
    scale_fill_continuous_sequential(palette = color_palette, 
                                     breaks=c(lowest_value
                                              ,highest_value), na.value=na)+
    labs(fill = per_x) +
    ggtitle(graph_title)+
    my_map_theme() +
    theme(plot.title = element_text(hjust=0.5, size = 14)) +
    theme(legend.position="bottom")
  
  ggp_map <- ggplotly(map, tooltip = c("text")) %>%
    style(hoveron="fill")  %>% 
    layout(title = list(text = 
                          paste0(graph_title,'<br>',
                                 '<sup>','World Average: ', covid_world_values[[map_variable]],
                                 '<br>', map_date ,'</sup>')))
  
  ggp_map
}

make_graph <- function(graph_variable, countries = "Italy"){
  
  graph_title <- graph_variable
  per_x <- " "
  if(graph_variable == "Total Cases - Per Million"){
    graph_variable <- "total_cases_per_million"
    per_x <- "Count Per Million"
  }
  else if (graph_variable == "New Cases - Per Million"){
    graph_variable <- "new_cases_per_million"
    per_x <- "Count Per Million"
  }
  else if (graph_variable == "Total Deaths - Per Million"){
    graph_variable <- "total_deaths_per_million"
    per_x <- "Count Per Million"
  }
  else if(graph_variable == "New Deaths - Per Million"){
    graph_variable <- "new_deaths_per_million"
    per_x <- "Count Per Million"
  }
  else if(graph_variable == "ICU Patients - Per Million"){
    graph_variable <- "icu_patients_per_million"
    per_x <- "Count Per Million"
  }
  else if(graph_variable == "Hospital Patients - Per Million"){
    graph_variable <- "hosp_patients_per_million"
    per_x <- "Count Per Million"
  }
  else if(graph_variable == "Total Tests - Per Thousand"){
    graph_variable <- "total_tests_per_thousand"
    per_x <- "Count Per Thousand"
  }
  else if(graph_variable == "New Tests - Per Thousand"){
    graph_variable <- "new_tests_per_thousand"
    per_x <- "Count Per Thousand" 
  }
  else if(graph_variable == "Total Vaccinations - Per Hundred"){
    graph_variable <- "total_vaccinations_per_hundred"
    per_x <- "Count Per Hundred"
  }
  else if(graph_variable == "People Fully Vaccinated - Per Hundred"){
    graph_variable <- "people_fully_vaccinated_per_hundred"
    per_x <- "Count Per Hundred" 
  }
  else if(graph_variable == "People Vaccinated - Per Hundred"){
    graph_variable <- "people_vaccinated_per_hundred"
    per_x <- "Count Per Hundred" 
  }
  
  #get subset of needed data
  covid_subset <- covid %>%
    select(iso_code, continent, location, date, graph_variable)
  
  #filter for countries
  covid_subset <- covid_subset %>%
    filter(location %in% countries)
  
  #make graph
  countries_graph <- covid_subset %>%
    ggplot(mapping = aes(x = as.Date(date), y = covid_subset[[graph_variable]], color = location)) +
    geom_point(aes(text=paste("Date:", as.Date(date), "\nCountry:",location,"\nCases:", covid_subset[[graph_variable]]))) +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle(graph_title) +
    xlab("Date") +
    ylab(per_x) +
    labs(color = "Country") + 
    theme_minimal() +
    scale_color_discrete_sequential(palette = "Heat", nmax = length(countries)) 
  
  
  #make graph interactive
  ggplotly(countries_graph, tooltip = c("id","text")) %>%
    layout(hovermode = "x unified") %>%
    layout(
      showlegend = T, 
      legend = list(orientation = 'v')
    ) 
  
}

var_list <- c("Total Cases - Per Million",
              "New Cases - Per Million",
              "Total Deaths - Per Million",
              "New Deaths - Per Million",
              "ICU Patients - Per Million",
              "Hospital Patients - Per Million",
              "Total Tests - Per Thousand",
              "New Tests - Per Thousand",
              "Total Vaccinations - Per Hundred",
              "People Fully Vaccinated - Per Hundred",
              "People Vaccinated - Per Hundred")

# Define UI for application that makes COVID-19 Graphs
ui <- fluidPage(
    theme = shinytheme("sandstone"),
    
    tabsetPanel(
      
      tabPanel("Map", fluid = TRUE,
        # Application title
        titlePanel("COVID-19 Map Graphs"),
        
        inputPanel(
          selectInput(
            "Variable",
            label = "Select Variable",
            choices = var_list
          ),
          selectInput(
            "Color",
            label = "Select Color Scheme",
            choices = c("Normal","Color Vision Deficiency (Red Scale)","Color Vision Deficiency (Orange-Blue)","Black and White")
          ),
          selectInput(
            "Continent",
            label = "Select Continent",
            choices = cont_list
          )
        ),
    
        # Sidebar with a slider input for date 
        sidebarLayout(
            sidebarPanel(
                sliderInput("date",
                            "Select a date to display:",
                            min = as.Date("2020-02-24", "%Y-%m-%d"),
                            max = as.Date(Sys.Date()-2, "%Y-%m-%d"),
                            value = as.Date("2020-02-24", "%Y-%m-%d"))
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotlyOutput("map")
            )
        )
      ),
      
      tabPanel("Graph", fluid = TRUE,
               # Application title
               titlePanel("COVID-19 Graphs"),
               inputPanel(
                 selectInput(
                   "vars",
                   label = "Select Variable",
                   choices = var_list
                 )
               ),
               sidebarPanel(
                 pickerInput("countries","Countries", choices=country_list, options = list(`actions-box` = TRUE),multiple = T)
               ),
               mainPanel(
                 plotlyOutput("graph")
               )
      )
    ),
    
    
    
    tags$footer("Author: Yarra Abozaed. Data Source: https://github.com/owid/covid-19-data/tree/master/public/data",
    align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:40px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: gainsboro;
              z-index: 1000;")
    
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  g <- ""
  m <- ""
  #CHANGE TO RENDERPLOT IF ITS NOT A PLOTLY OBJECT
    output$map <- renderPlotly({
      m <- make_map(input$Variable , input$date, input$Color, input$Continent)
      m
    })
    
    output$graph <-renderPlotly({
      if(length(input$countries) == 0){
        g <- make_graph(input$vars, c("Italy","Germany"))
        g
      }
      else{
        g <- make_graph(input$vars, input$countries)
        g
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
