library(shiny)
library(shinythemes)
library(plotly)
rm(list = ls())
source("functions.R")


shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      "
      .navbar .navbar-brand {
      padding:10px 750px 10px 0px;
      float:left;
      text-align:left;
      line-height:21px;
      height:60px;
      width:100%
      display:inline;
      }
      .navbar .navbar-header {
      float:right;
      }
      .navbar .navbar-nav{
      float:right;
      }

      #sidebar {
      background-color: white;
      padding: 0 20px 20px 20px;
      }
      "
    )
    ))
  ,
  
  navbarPage(
    HTML(
      "<a><img src='http://icons.iconarchive.com/icons/dtafalonso/android-lollipop/128/Calculator-icon.png'/>&nbsp;&nbsp;&nbsp;&nbsp;Pension Simulation Calculator</a>"
    )
    , theme = shinytheme('flatly') , position =
      "static-top",
    windowTitle = "Pension Simulation Calculator",fluid = TRUE,
    
    tabPanel(
      'Population & Plan Features',
      column(
        3,
          wellPanel(
            h3('Who Participates in the Plan?'),br(),
            
            div(
              h4('Current Employees:'),
              numericInput(
                "pop_size", "How many current employees are in the plan?",min = 10000,max = 10000000,value = 10000, step =
                  1000
              )        ,
              numericInput(
                "ea", "What is the typical age when new employees begin to work for the employer?",min = 25,max = 45,value = 30
              )        ,
              numericInput(
                "retire", "Retirement Age:",min = 55,max = 65,value = 65,step=5
              ),
              radioButtons(
                "pop_type","Select one of the following to describe the age distribution of the current employees:",
                choices = c("Every age group has relatively the same number of workers"='Uniform',"A large portion of current employees are nearing retirement"='Over mature',
                            "A small portion of current employees are nearing retirement"='Under mature',
                            "Customize the age distribution"='Customize curve'),selected =
                  "Uniform"
              ),
              conditionalPanel("input.pop_type=='Customize curve'",uiOutput("ui")),br(),
              h4('Retirees:'),
              HTML('<b>We calculate the following number of retirees (see the graphic for the assumed age distribution for the retirees)</b>'),textOutput("no_of_r")
            )
            
            
          ))
          ,
        column(
          3,
          
          wellPanel(
            h3('The Retirement Plan Benefits'),br(),
            numericInput("benefit_factor", "Benefit Factor (%) - Sometimes called the generosity factor",2,step = 0.25,min = 0),
            numericInput("afc", "Average Final Contribution - This identifies over how many years to average employees' salaries to determine their final compensation", 5, min = 1,max = 10),
            numericInput("cola", "COLA (%) - Cost-of-living adjustment, the rate that annual benefits increase after retirement", 0,step = 0.25,min = 0),
            numericInput("vest", "Vesting Period - How many years of service an employee must contribute before he/she qualifies for full retirement benefits", 5, min = 1,max = 10)),
            
          wellPanel(
            h3('Salary Information:'),br(),
            numericInput("sal_in_ea","On average, what is the salary of new workers who started this year?",min=20000,value=60000,step = 1000),
            br(),HTML('<b>The assumed final salary (AFC) for someone who just retired ($)</b>'),
            textOutput('salary_at_retire'),
            br(),HTML('<b>The expected annual retirement benefit for someone who just retired ($)</b>'),
            textOutput('retire_annuity'),
            br(),HTML('<b>How much of annual salary is replaced by their retirement benefit, for someone who just retired (%)</b>'),
            textOutput('replace_rate'),
            br(),HTML('<b>The total liability for someone who just retired ($)</b>'),
            textOutput('pv_annuity')
          )
        
      ),
      
      column(6,br(),br(),
             div(
               HTML('<center><h2>Age Distribution for Current Workers and Retirees</h2></center>'),
               plotlyOutput("pop")
             ))
        ),
    tabPanel(
      'Actuarial Inputs',
      column(
        3,
        wellPanel(
          selectInput("cost_method","The actuarial cost method (most governments use EAN - Entry Age Normal)",choices = c("EAN","PUC")),
          sliderInput(
            "discount_rate", "What is the expected rate of return on the plan's assets? (the discount rate)",min = 2, max = 7,value =6,step = 0.25)
          ,
          br(),
          sliderInput(
            "sal_growth_rate", "What is the expected salary growth rate? (including cost of living, productivity, and merit increases)",min = 2,max = 6,value =
              3.68,step = 0.25),
          
          br(),
          numericInput(
            "amort", "If the plan has an unfunded liability, over how many years should the liability be paid down? (amortization period)", 30, min = 0, max = 50
          ),
          selectInput(
            "mort","What mortality rate should be used? (The most recent is RP-2014)",
            choices = c(
              "RP2014_Employee_total","RP2000_Employee_total","RP2010_Employee_total"
            ),selected = "RP2010_Employee_total"
          )
          
        )
      ),
      
      column(
        4,
        br(),
        HTML("<center><h5>This figure shows the growth in how much money needs to be set aside every year, in dollar amounts, to cover one employee's retirement benefit</h5></center>"),
        plotlyOutput('nc'),br(),
        textOutput('total_nc'),br(),br(),
        HTML("<center><h5>This figure shows the growth in a single employee's liabilty, in dollar amounts, and the decline in that liability through retirement</h5></center>"),
        plotlyOutput('aal'), br(),
        textOutput('total_aal')
      ),
      column(
        4,
        br(),
        HTML("<center><h5>This figure shows the growth in how much money needs to be set aside every year, as a portion of salary, to cover one employee's retirement benefit</h5></center>"),
        plotlyOutput('nc_payroll'),br(),br(),br(),br(),
        
        HTML("<center><h5>This figure shows the growth in a single employee's liability, as a portion of salary, from the year they begin work to when they retire</h5></center>"),
        plotlyOutput('aal_payroll'),br(),br(),br(),br()
        
      ),
      absolutePanel(top = 1240,left = 500,tags$footer(
        tags$i(
          "* These line graphs represent the normal costs and AAL for a single employee from hire to retirement"
        )
      ))
      
      
    ),
    tabPanel(
      'Funding Level',
      column(4,br(),br(),
             wellPanel(
               sliderInput(
                 "percent", "What is the plan's funding ratio? (What is the value of the plan assets that are set aside to cover the liability?)",min = 0,max = 100,value = 85.4,step = 0.01
               )
              )
             ),
      column(
        8,align="center",br(),br(),
        HTML('<center><h3>Summary</center>'),
        tableOutput('summary'), br(),
        HTML('<center><h3>The share of costs, liabilities, and assets by age group</h3></center>'),br(),
        tableOutput('stats')
        )
        
    ),
    navbarMenu(
      'Sensitivity tests',
      tabPanel(
        'Discount Rate', br(),
        HTML('<center><h4>Discount Rate</h4></center>'),
        br(),
        column(6,align="center","This figure illustrates how the funding ratio will change if the discount rate is increased or decreased",
               plotlyOutput('fr_dr')),
        column(6,align="center", "This figure illustrates how the contribution rate (as a share of payroll) will change if the discount rate is increased or decreased",
               plotlyOutput('arc_dr')),
        
        absolutePanel(top = 630,left = 500,tags$footer(textOutput("asgr_disp_fr")))
        
        
      ),
      tabPanel(
        'Salary Growth Rate', br(),
        HTML('<center><h4>Salary Growth Rate</h4></center>'),
        br(),
        column(6,align="center", "This figure illustrates how the funding ratio will change if the salary growth rate is increased or decreased",
               plotlyOutput('fr_sgr')),
        column(6,align="center","This figure illustrates how the contribution rate (as a share of payroll) will change if the salary growth rate is increased or decreased",
               plotlyOutput('arc_sgr')),
        absolutePanel(top = 630,left = 500,tags$footer(textOutput("asgr_disp_arc")))
      ),
      tabPanel('Mortality Tables',br(),
               plotlyOutput('arc_mort')
      )
    )
  )
    ))
