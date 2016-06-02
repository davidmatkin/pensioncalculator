#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   ui.R contains code for rendering the user interface input elements like sliderinputs,               #
#   numeric inputs, text inputs and rendering outputs like the sensitivity test graphs.                 #
#                                                                                                       #
#   This file takes input from the user and passes the inputs to the server files which calls           #
#   the functions in functions.R (file that contains the model's logic)                                 #
#                                                                                                       #
#########################################################################################################
#########################################################################################################


############################ LIBRARY IMPORT ##############################
# shiny: R library to build interactive web applications.
# Allows creation of pre-built widgets.
# REFERENCE: https://cran.r-project.org/web/packages/shiny/shiny.pdf
##########################################################################

library(shiny)

############################ LIBRARY IMPORT ##############################
# shinythemes: R library to include prebuilt bootstrap themes
# into web applications.
# REFERENCE: https://rstudio.github.io/shinythemes/
##########################################################################

library(shinythemes)

############################ LIBRARY IMPORT ##############################
# plotly: Plotly is an analytics and data visualization tool.
# It offers an API for R to create aesthetic data visualizations
# REFERENCE: https://plot.ly/r/
##########################################################################

library(plotly)

############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################
  
rm(list = ls())
  
#################### Start of shiny UI Definition ########################
### A Shiny application needs a shinyUI and shinyServer element to run as a web application ###
shinyUI(
  ### Start of fluidPage definition ###
  fluidPage(
    ### opening the head element to add customizations ###
    tags$head(
      ### Customizing the shiny theme used by redefining some festuress of the flatly theme css used ###
      tags$style(
        ### HTML function to write HTML Markup for custom positioning calculator logo ###
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
        ) ### end HTML markup ###
      ) ### end style tag ###
    ), ### end head tag ###
  
  ### navbarPage: shiny equivalent for navbar in bootstrap ###
  ### Offers navigation bar. REFERENCE: http://shiny.rstudio.com/reference/shiny/latest/navbarPage.html ###
  navbarPage(
    ### Embedding HTML code to insert Calculator logo and text ###
      HTML(
        "<a><img src='http://icons.iconarchive.com/icons/dtafalonso/android-lollipop/128/Calculator-icon.png'/>&nbsp;&nbsp;&nbsp;&nbsp;Pension Simulation Calculator</a>"
      ), ### end HTML markup ###
    
      ### specifying default boostrap css theme fromthe shinythemes library ###
      theme = shinytheme('flatly'),
      
      ### position attribute specifies position of navbar and visibility while scrolling ###
      position ="static-top",
      
      ### Specifying browser window title ###
      windowTitle = "Pension Simulation Calculator",
      
      ### fluid attribute allows the web page to stretch and contract relative to the user's screen size.###
      fluid = TRUE,
      
      ### The Population & Plan features panel ###
      ### This panel is split into three columns ahead ###
      tabPanel(
        'Population & Plan Features',
        ### Column 1 ###
        column(
          3,
            ### wellPanel is used to include UI elements inside a panel ###
            wellPanel(
              h3('Who Participates in the Plan?'),br(),
              h4('Current Employees:'),
            
              ### numeric input for number of active members in a plan ###
              ### min and max attributes determine range, value is used to set default value and step for defining intervals between min and max ###
              numericInput(
                "pop_size", "How many current employees are in the plan?",min = 10000,max = 10000000,value = 10000, step =
                  1000
              ),
              ### numeric input for entry age of members in a plan ###
              numericInput(
                "ea", "What is the typical age when new employees begin to work for the employer?",min = 25,max = 45,value = 30
              ),
              ### numeric input for retirement age of members in a plan ###
              numericInput(
                "retire", "Retirement Age:",min = 55,max = 65,value = 65,step=5
              ),
              ### group of radio buttons to select population type, choices parameter list the different types of population ###
              radioButtons(
                "pop_type","Select one of the following to describe the age distribution of the current employees:",
                choices = c("Every age group has relatively the same number of workers"='Uniform',"A large portion of current employees are nearing retirement"='Over mature',
                            "A small portion of current employees are nearing retirement"='Under mature',
                            "Customize the age distribution"='Customize curve'),selected ="Uniform"
              ),
              ### a conditional panel to select a median age for the custoize age distribution option ###
              conditionalPanel("input.pop_type=='Customize curve'",uiOutput("ui")),br(),
              h4('Retirees:'),
              HTML('<b>We calculate the following number of retirees (see the graphic for the assumed age distribution for the retirees)</b>'),textOutput("no_of_r")
            )
          ),
        
        ### Column 2 ###
        column(
          3,
          wellPanel(
            h3('The Retirement Plan Benefits'),br(),
            ### numeric input for benefit factor ###
            numericInput("benefit_factor", "Benefit Factor (%) - Sometimes called the generosity factor",2,step = 0.25,min = 0),
            ### numeric input for AFC of a plan ###
            numericInput("afc", "Average Final Contribution - This identifies over how many years to average employees' salaries to determine their final compensation", 5, min = 1,max = 10),
            ### numeric input for COLA of a plan ###
            numericInput("cola", "COLA (%) - Cost-of-living adjustment, the rate that annual benefits increase after retirement", 0,step = 0.25,min = 0),
            ### numeric input for vesting period ###
            numericInput("vest", "Vesting Period - How many years of service an employee must contribute before he/she qualifies for full retirement benefits", 5, min = 1,max = 10)),
            
          wellPanel(
            h3('Salary Information:'),br(),
            ### numeric input salary at entry age of a member in a plan ###
            numericInput("sal_in_ea","On average, what is the salary of new workers who started this year?",min=20000,value=60000,step = 1000),
            br(),HTML('<b>The assumed final salary (AFC) for someone who just retired ($)</b>'),
            ### computed value of salary just after retirement is returned from server.R ###
            textOutput('salary_at_retire'),
            br(),HTML('<b>The expected annual retirement benefit for someone who just retired ($)</b>'),
            ### computed value of expected annual retirement benefit for someone who just retired is returned from server.R ###
            textOutput('retire_annuity'),
            br(),HTML('<b>How much of annual salary is replaced by their retirement benefit, for someone who just retired (%)</b>'),
            ### computed value of replacement rate is returned from server.R ###
            textOutput('replace_rate'),
            br(),HTML('<b>The total liability for someone who just retired ($)</b>'),
            ### computed value of total liability for a retiree is returned from server.R ###
            textOutput('pv_annuity')
          )
      ),
      
      ### Column 3 ###
      column(6,align='center',br(),br(),
             h2('Age Distribution for Current Workers and Retirees'),
             ### graph plotted at server.R is returned to ui.R based on the parameter filled in column 1 ###
             plotlyOutput("pop")
            )
        ),
    
      
    ### The Actuarial Inputs panel ###
    ### This panel is split into three columns ahead ###
    tabPanel(
      'Actuarial Inputs',
      ### Column 1 ###
      column(
        3,
        wellPanel(
          ### a select box to choose between cost methods. Default is EAN ###
          selectInput("cost_method","The actuarial cost method (most governments use EAN - Entry Age Normal)",choices = c("EAN","PUC")),
          ### a slider input to choose a discount rate for the plan ###
          sliderInput(
            "discount_rate", "What is the expected rate of return on the plan's assets? (the discount rate)",min = 2, max = 7,value =6,step = 0.25)
          , br(),
          ### a slider input to choose a salary growth rate for the plan ###
          sliderInput(
            "sal_growth_rate", "What is the expected salary growth rate? (including cost of living, productivity, and merit increases)",min = 2,max = 6,value =
              3.68,step = 0.25),br(),
          ### a numeric input to set amortization period ###
          numericInput(
            "amort", "If the plan has an unfunded liability, over how many years should the liability be paid down? (amortization period)", 30, min = 0, max = 50
          ),
          ### a select box to choose between mortality tables. Default is RP-2014 ###
          selectInput(
            "mort","What mortality rate should be used? (The most recent is RP-2014)",
            choices = c(
              "RP2014_Employee_total","RP2000_Employee_total","RP2010_Employee_total"
            ),selected = "RP2014_Employee_total"
          )
        )
      ),
      
      ### Column 2 ###
      column( align='center', 4, br(),
        h5("This figure shows the growth in how much money needs to be set aside every year, in dollar amounts, to cover one employee's retirement benefit"),
        ### graph plotted for nc at server.R is returned to ui.R based on the parameter filled in column 1 ###
        plotlyOutput('nc'),br(),
        ### total nc computed is returned from the server.R to ui.R ###
        textOutput('total_nc'),br(),br(),
        h5("This figure shows the growth in a single employee's liabilty, in dollar amounts, and the decline in that liability through retirement"),
        ### graph plotted for aal at server.R is returned to ui.R based on the parameter filled in column 1 ###
        plotlyOutput('aal'), br(),
        ### total aal computed is returned from the server.R to ui.R ###
        textOutput('total_aal')
      ),
      
      ### Column 3 ###
      column(align='center',4,br(),
        h5("This figure shows the growth in how much money needs to be set aside every year, as a portion of salary, to cover one employee's retirement benefit"),
        ### graph plotted for nc/payroll at server.R is returned to ui.R based on the parameter filled in column 1 ###
        plotlyOutput('nc_payroll'),br(),br(),br(),br(),
        h5("This figure shows the growth in a single employee's liability, as a portion of salary, from the year they begin work to when they retire"),
        ### graph plotted for aal/payroll at server.R is returned to ui.R based on the parameter filled in column 1 ###
        plotlyOutput('aal_payroll'),br(),br(),br(),br()
      ),
      
      ### A panel with a positioning feature ###
      absolutePanel(top = 1240,left = 500,tags$footer(
        tags$i(
          "* These line graphs represent the normal costs and AAL for a single employee from hire to retirement"
        )
      ))
    ),
    
    ### The Funding panel ###
    ### This panel is split into two columns ahead ###
    tabPanel(
      'Funding Level',
      ### Column 1 ###
      column(4,br(),br(),
             wellPanel(
               ### slider for setting funding level ###
               sliderInput(
                 "percent", "What is the plan's funding ratio? (What is the value of the plan assets that are set aside to cover the liability?)",min = 0,max = 100,value = 85.4,step = 0.01
               )
              )
             ),
      ### Column 2 ###
      column(8,align="center",br(),br(),
        h3('Summary'),
        ### Table output returned by the server.R file to the ui.R for the summary of ARC, Payroll, AAL, NC etc.
        tableOutput('summary'), br(),
        h3('The share of costs, liabilities, and assets by age group'),br(),
        ### Table output returned by the server.R file to the ui.R for the statistics of ARC, Payroll, AAL, NC etc.
        tableOutput('stats')
        )
    ),
    
    ### A submenu in a navbar page ###
    ### Sensitivity tests panel ###
    navbarMenu(
      'Sensitivity tests',
    ### sub panel for discount rate sensitivity tests ###
    ### contains two columns ###
      tabPanel(align='center',
        'Discount Rate', br(),
        h4('Discount Rate'),br(),
        ### Column 1 ###
        column(6,align="center","This figure illustrates how the funding ratio will change if the discount rate is increased or decreased",
        ### graph output returned by server.R for the discount rate sensitivity test for funding ratio ###               
               plotlyOutput('fr_dr')),
        ### Column 2 ###
        column(6,align="center", "This figure illustrates how the contribution rate (as a share of payroll) will change if the discount rate is increased or decreased",
        ### graph output returned by server.R for the discount rate sensitivity test for ARC ###               
               plotlyOutput('arc_dr')),
        absolutePanel(top = 630,left = 500,tags$footer(textOutput("asgr_disp_fr")))
      ),
    
    ### sub panel for salary growth rate sensitivity tests ###
    ### contains two columns ###
      tabPanel(align='center',
        'Salary Growth Rate', br(),
        h4('Salary Growth Rate'),
        br(),
        ### Column 1 ###
        column(6,align="center", "This figure illustrates how the funding ratio will change if the salary growth rate is increased or decreased",
        ### graph output returned by server.R for the salary growth rate sensitivity test for funding ratio ###               
               plotlyOutput('fr_sgr')),
        ### Column 2 ###
        column(6,align="center","This figure illustrates how the contribution rate (as a share of payroll) will change if the salary growth rate is increased or decreased",
        ### graph output returned by server.R for the salary growth rate sensitivity test for ARC ###               
               plotlyOutput('arc_sgr')),
        absolutePanel(top = 630,left = 500,tags$footer(textOutput("asgr_disp_arc")))
      ),
    
    ### sub panel for mortality table sensitivity tests ###
    tabPanel('Mortality Tables',br(),
        ### graph output returned by server.R for the mortality tables sensitivity test for ARC ###               
        plotlyOutput('arc_mort')
      )
    )
  )### end of navbarPage
  )### end of fluidPage
)### end of shinyUI
