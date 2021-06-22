library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinythemes)
df <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/covid_data.csv')
pop <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/state_pop.csv')
df <- df %>% mutate(month = substr(date,1,7))
cases <- df %>%
  group_by(month,state) %>%
  summarize(cc = sum(daily_cases))
death <- df %>%
  group_by(month,state) %>%
  summarize(cd = sum(daily_deaths))
pop_df <- left_join(pop,df)
pop_df <- pop_df %>%
  mutate(cp = daily_cases / population *100000) %>%
  mutate(dp = daily_deaths / population *100000)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme('superhero'),
                titlePanel('Covid cases and deaths by state'),
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = 'state_name',
                                label = 'select a state',
                                choices = sort(unique(df$state)),
                                selected = 'Alabama'),
                    selectInput(inputId = 'bar_color',
                                label = 'choose bar color',
                                choices = c('blue','white',
                                            'cornflowerblue',
                                            'chartreuse',
                                            'darksalmon'),
                                selected = 'blue'),
                    sliderInput(inputId = 'alpha_value',
                                label = 'change transparency',
                                min = 0,
                                max = 1,
                                value = 0.5,
                                step = 0.1)
                  ),
                  # Show a plot of the generated distribution
                  mainPanel(
                    fluidRow(
                      column(6,
                             plotOutput('covid_plot')),
                      column(6,
                             plotOutput('death_plot')),
                    ),
                    br(),
                    fluidRow(
                      column(6,
                             plotOutput('cum_case_plot')),
                      column(6,
                             plotOutput('cum_death_plot')),
                    ),
                    br(),
                    fluidRow(
                      column(12,
                             plotOutput('per_100k_plot'))
                    )
                  )
                )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$covid_plot <- renderPlot({
    st <- input$state_name
    bc <- input$bar_color
    av <- input$alpha_value
    subcases <- cases %>% filter(state == st)
    ggplot(subcases,aes(month,cc)) +
      geom_bar(stat = 'identity',
               fill = bc,
               alpha = av) +
      labs(title = paste0(st,': covid cases')) +
      ggthemes::theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 90))
  })
  output$death_plot <- renderPlot({
    st <- input$state_name
    bc <- input$bar_color
    av <- input$alpha_value
    subdeaths <- death %>% filter(state == st)
    ggplot(subdeaths, aes(month,cd)) +
      geom_bar(stat = 'identity',
               fill = bc,
               alpha = av) +
      labs(title = paste0(st,': covid deaths')) +
      ggthemes::theme_fivethirtyeight() +
      theme(axis.text.x = element_text(angle = 90))
  })
  output$cum_case_plot <- renderPlot({
    st <- input$state_name
    bc <- input$bar_color
    av <- input$alpha_value
    subcum_case <- df %>% filter(state == st)
    ggplot(subcum_case, aes(date,cumsum(daily_cases))) +
      geom_line(color = bc,
                alpha = av) +
      labs(title = paste0(st,': cumulative covid cases')) +
      ggthemes::theme_fivethirtyeight()
  })
  output$cum_death_plot <- renderPlot({
    st <- input$state_name
    bc <- input$bar_color
    av <- input$alpha_value
    subdeath_case <- df %>% filter(state == st)
    ggplot(subdeath_case, aes(date,cumsum(daily_deaths))) +
      geom_line(color = bc,
                alpha = av) +
      labs(title = paste0(st,': cumulative covid deaths'))+
      ggthemes::theme_fivethirtyeight()
  })
  output$per_100k_plot <- renderPlot({
    st <- input$state_name
    bc <- input$bar_color
    av <- input$alpha_value
    subper_case <- pop_df %>% filter(state == st)
    ggplot(subper_case, aes(date,cp)) +
      geom_line(color = bc,
                alpha = av) +
      geom_line(aes(date,dp),
                color = bc,
                alpha = av) +
      labs(title = paste0(st,': covid cases and deaths
                                per 100k')) +
      ggthemes::theme_fivethirtyeight()
  })
}
# Run the application
shinyApp(ui = ui, server = server)