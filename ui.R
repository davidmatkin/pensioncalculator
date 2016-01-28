library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)

shinyUI(
  navbarPage(
    'Pension Simulation project', theme = shinytheme("Flatly") ,
    tabPanel('Population Settings',
      column(3,
        absolutePanel(top=50,left = 30,draggable=TRUE,
          numericInput("ea", "Entry Age",min = 25,max = 65,value = 30)        ,
          sliderInput("pop_size", "Population size",min = 500,max = 10000,value = 1000,step = 10),
          selectInput("pop_type","Population type",
            choices = c("Uniform","Over mature",
                        "Under mature","Bell curve")),
          conditionalPanel("input.pop_type=='Bell curve'",uiOutput("ui"))
          )
        ),
      
      column(5,br(),br(),
          div(
            HTML('<center><h3>Population Distribution</h3></center>'),
            plotOutput("pop")
          )
      ),
      column(3,br(),br(),
          HTML('<center><h4>Population Information</h4></center>'),
          tableOutput("population_dist")
      )
    ),
    
    tabPanel('Plan Features', 
      absolutePanel(top=200, left=200,draggable=TRUE,
         textInput("benefit_factor", "Benefit Factor:",0.02),
         textInput("cola", "COLA:", 0.0),
         numericInput("vest", "Vesting Period:", 5),
         sliderInput(
           "act_sal_growth_rate", "Select actual salary growth rate",
           min = 3,max = 8,value = 5.68,step = 0.25
         )
         
        )
      ),
    tabPanel('Actuarial Inputs',
      column(4,  
        absolutePanel(top=50,left = 50, draggable=TRUE,
          selectInput("cost_method","Select cost method",choices = c("EAN","PUC")),
          sliderInput("discount_rate", "Select discount rate",min = 6,max = 9,value = 8,step = 0.25),
          sliderInput("sal_growth_rate", "Select salary growth rate",min = 3,max = 8,value = 5.68,step = 0.25),
          numericInput("amort", "Amortization Period:", 30),
          selectInput(
            "mort","Select Mortality table",
            choices = c(
              "Male RP-2000 Rates","Female RP-2000 Rates","RP-2000 average",
              "Male RP-2000 10 years projection","Female RP-2000 10 years projection",
              "2010 average","Male RP-2014","Female RP-2014","RP-2014 average","GAM-1994 Male",
              "GAM-1994 Female","GAM-1994 average"
                )
              )
          )
          ),
        column(4,br(),br(),
               HTML('<center><h4>Normal Cost</h4></center>'),
               plotOutput('nc')
               ),
      column(4,br(),br(),
             HTML('<center><h4>AAL</h4></center>'),
             plotlyOutput('aal')
          )
        ),
    tabPanel('Funding Level settings',
      absolutePanel(top=150,left = 150, draggable=TRUE,
        sliderInput("percent", "Select Percentage",min = 0,max = 100,value = 85.4,step = 0.01),
        sliderInput("rate", "Select discount rate",min = 6,max = 9, value =8, step = 0.25), 
        sliderInput("rate2", "Select salary growth rate",min = 3,max = 8,value =5.68,step = 0.25)
      )
    ),
    navbarMenu('Sensitivity tests',
        tabPanel('Funding Ratio', br(),br(),
                 HTML('<center><h4>Funding Ratio</h4></center>'),
        column(6,
             plotlyOutput('fr_dr')),
        column(6,
            plotlyOutput('fr_sgr')
          )
        ),
        tabPanel('ARC', br(),br(),
                 HTML('<center><h4>ARC</h4></center>'),
                 column(6,
                        plotlyOutput('arc_dr')),
                 column(6,
                        plotlyOutput('arc_sgr')
                 )
        )
)
)
)
