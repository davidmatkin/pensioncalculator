library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
rm(list = ls())
source("functions.R")


shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      "
.navbar .navbar-brand {padding:10%; position: center;line-height: 50px;}
.navbar .navbar-header {float: center;}
      "
    )
    ))
  ,

  navbarPage(
    #HTML("<span class='glyphicons glyphicons-calculator'></span>Pension Simulation Calculator")
    HTML("<a class='navbar-brand'>
Pension Simulation Calculator</a>"), theme = shinytheme('flatly') , position =
      "static-top",
    windowTitle = "Pension Simulation Calculator",fluid = TRUE,
    
    tabPanel(
      'Population & Plan Features',
      column(3,
             absolutePanel(
               top = 20,left = 20,draggable = FALSE,
               wellPanel(
                 h3('Population'),
                 sliderInput(
                   "pop_size", "Number of Active employees:",min = 500,max = 10000,value = 1000,step = 10
                 ),br(),
                 
                 div(
                   h4('Active Members'),
                   numericInput(
                     "ea", "Entry Age:",min = 30,max = 45,value = 30
                   )        ,
                   selectInput(
                     "pop_type","Population type:",
                     choices = c("Uniform","Over mature",
                                 "Under mature","Customize curve")
                   ),
                   conditionalPanel("input.pop_type=='Customize curve'",uiOutput("ui")),
                   h4('Retirees'),
                   h5('Number Of Retirees:'),textOutput("no_of_r")
                 )
                 
                 
               )
             )),
      
      column(6,br(),br(),
             div(
               HTML('<center><h2>Population Distribution</h2></center>'),
               plotlyOutput("pop")
             )),
      column(3,
             absolutePanel(
               top = 50, left = 20,draggable = FALSE,
               wellPanel(
                 h3('Plan Features'),
                 numericInput(
                   "benefit_factor", "Benefit Factor:",2,step = 0.25,min = 0
                 ),
                 numericInput("cola", "COLA:", 0,step = 0.25,min = 0),
                 numericInput("vest", "Vesting Period:", 5, min = 1,max = 10),
                 numericInput(
                   "afc", "Average Final Contribution:", 5, min = 1,max = 10
                 )
               )
             ))
      
    ),
    tabPanel(
      'Actuarial Inputs',
      column(
        3,
        wellPanel(
          selectInput("cost_method","Select cost method",choices = c("EAN","PUC")),
          sliderInput(
            "inflation_rate", "Select inflation rate",min = 2,max = 4,value = 2,step = 0.25
          ),
         # uiOutput('discount_rate')
         sliderInput("discount_rate", "Select real rate of return",min = 1, max = 7,value = 6,step = 0.25)
          ,
          textOutput('total_dr')
          ,
          br(),
          #uiOutput('salary_rate')
         sliderInput("sal_growth_rate", "Select merit based salary growth rate",min = 1,max = 6,value =3.68,step = 0.25)
          ,
          textOutput('total_sgr')
          ,
          br(),
          numericInput("amort", "Amortization Period:", 30),
          selectInput(
            "mort","Select Mortality table",
            choices = c(
              "Male RP-2000 Rates","Female RP-2000 Rates","RP-2000 average",
              "Male RP-2000 10 years projection","Female RP-2000 10 years projection",
              "2010 average","Male RP-2014","Female RP-2014","RP-2014 average","GAM-1994 Male",
              "GAM-1994 Female","GAM-1994 average"
            ),selected = "2010 average"
          )
          
        )
      ),
      
      column(
        4,
        br(),
        HTML('<center><h4>Normal Cost</h4></center>'),
        plotlyOutput('nc'),br(),
        textOutput('total_nc')
      ),
      column(
        4,
        br(),
        HTML('<center><h4>AAL</h4></center>'),
        plotlyOutput('aal'), br(),
        textOutput('total_aal')
      ),
      absolutePanel(top = 670,left = 500,tags$footer(
        tags$i(
          "* These line graphs represent the normal costs and AAL for a single employee from hire to retirement"
        )
      ))
      
      
    ),
    tabPanel(
      'Funding Level',
      column(
        3,br(),br(),
        wellPanel(
          sliderInput(
            "percent", "Select Percentage",min = 0,max = 100,value = 85.4,step = 0.01
          ),
          sliderInput(
            "rate", "Select discount rate",min = 6,max = 9, value = 8, step = 0.25
          ),
          sliderInput(
            "rate2", "Select salary growth rate",min = 3,max = 8,value = 5.68,step = 0.25
          )
        )
      ),
      column(
        4,br(),br(),
        HTML('<center><h4>Normal Cost</h4></center>'),
        plotlyOutput('nc_pop'),
        textOutput('total_nc_pop')
      ),
      column(
        4,br(),br(),
        HTML('<center><h4>AAL</h4></center>'),
        plotlyOutput('aal_pop'),
        textOutput('total_aal_pop')
      ),
      
      absolutePanel(top = 670,left = 500,tags$footer(
        tags$i(
          "* These line graphs represent the normal costs and AAL for the population from hire to retirement"
        )
      ))
    ),
    navbarMenu(
      'Sensitivity tests',
      tabPanel(
        'Funding Ratio', br(),
        HTML('<center><h4>Funding Ratio</h4></center>'),
        column(6,
               plotlyOutput('fr_dr')),
        column(6,
               plotlyOutput('fr_sgr')),
        absolutePanel(top = 630,left = 500,tags$footer(
          textOutput("asgr_disp_fr")))
        
        
      ),
      tabPanel(
        'ARC', br(),
        HTML('<center><h4>ARC</h4></center>'),
        column(6,
               plotlyOutput('arc_dr')),
        column(6,
               plotlyOutput('arc_sgr')),
        absolutePanel(top = 630,left = 500,tags$footer(
          textOutput("asgr_disp_arc")))
      )
    )
  )
    ))
