ui <- dashboardPage(
  
  # header ------------------------------------------------------------------
  dashboardHeader(title = "Modelling"),
  
  # sidebar -----------------------------------------------------------------
  
  
  dashboardSidebar(width = 350,
                   sidebarMenu(id = "tabs",
                               menuItem("Covid modelling", tabName = "covid_modelling", icon = icon("th")),
                               menuItem("The Chase", tabName = "the_chase", icon = icon("th"))
                   )
  ),
  
  # body --------------------------------------------------------------------
  
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "covid_modelling",
              
              fluidRow(
                column(7,
                       tags$h4("Number of people with covid by days (black is without any vaccine and orange in with vaccine)"),
                       plotOutput("distPlot"),
                       checkboxInput(inputId = "show_unvaccinated", label = "Show results without vaccine?", 
                                     value = F)
                ),
                column(3,
                       sliderInput("total_pop",
                                   "Population:",
                                   min = 1000000,
                                   max = 1000000000,
                                   value = 5000000,
                                   step = 1000000),
                       sliderInput("infected_count",
                                   "Infected:",
                                   min = 1,
                                   max = 200,
                                   value = 5,
                                   step = 1),
                       sliderInput("vaccine_modifier",
                                   "Vaccine Efficacy:",
                                   min = 0,
                                   max = 1,
                                   value = 0.9,
                                   step = 0.05),
                       sliderInput("vaccine_pop",
                                   "Proportion of Population Vaccinated:",
                                   min = 0,
                                   max = 1,
                                   value = 0.7,
                                   step = 0.05),
                       sliderInput("ro",
                                   "R 0:",
                                   min = 0.1,
                                   max = 10,
                                   value = 4,
                                   step = 0.1),
                       sliderInput("recovery",
                                   "Infectivity period (days):",
                                   min = 1,
                                   max = 30,
                                   value = 8,
                                   step = 1),
                       sliderInput("incubation",
                                   "Incubation period (days):",
                                   min = 1,
                                   max = 30,
                                   value = 4,
                                   step = 1),
                       sliderInput("time_frame",
                                   "Number of days to view:",
                                   min = 1,
                                   max = 500,
                                   value = 15,
                                   step = 1)
                )
              ),
              
              tags$h4("Data showing values for the no vaccine graph"),
              
              dataTableOutput(outputId = "datatab")
              
              
      ),
      tabItem(tabName = "the_chase",
              fluidRow(
                column(7,
                       tags$h4("Probability of winning or losing at the Chase based on the decision made"),
                       plotOutput("chaseplot"),
                       plotOutput("expectedReturnsOffer")
                ),
                column(3,
                       sliderInput("chaserprob",
                                   "Probability Chaser gets question correct:",
                                   min = 0,
                                   max = 1,
                                   value = 0.95,
                                   step = 0.01),
                       sliderInput("playerprob",
                                   "Probability Player gets question correct:",
                                   min = 0,
                                   max = 1,
                                   value = 0.66,
                                   step = 0.01),
                       sliderInput("skillmodifier",
                                   "Adjusted Probability based on offer:",
                                   min = 0,
                                   max = 1,
                                   value = 0.05,
                                   step = 0.01),
                       sliderInput("chaserstart",
                                   "Chaser start position:",
                                   min = 3,
                                   max = 20,
                                   value = 8,
                                   step = 1),
                       sliderInput("playerstart",
                                   "Player start position (default):",
                                   min = 1,
                                   max = 18,
                                   value = 5,
                                   step = 1),
                       sliderInput("moneyaway",
                                   "Offer for stepping away from the Chaser:",
                                   min = -500,
                                   max = 10000,
                                   value = 1000,
                                   step = 100),
                       sliderInput("moneystay",
                                   "Money earned:",
                                   min = 0,
                                   max = 20000,
                                   value = 4000,
                                   step = 1000),
                       sliderInput("moneycloser",
                                   "Offer for stepping closer to the Chaser:",
                                   min = 0,
                                   max = 100000,
                                   value = 20000,
                                   step = 500)
                )
              ),
              dataTableOutput(outputId = "chasetable")
      )
      # plotOutput("hours_plot", click = "hours_plot_click", dblclick = "hours_plot_dbclick", hover = "hours_plot_hover")
    )
  )
)

