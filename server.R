#########################################################################################################
#########################################################################################################
#                                                                                                       #
#                                          DESCRIPTION                                                  #
#   server.R contains code that monitors the user input and makes calls to the model's logic            #
#   dynamically whenever the value set for an UI element (text inputs, numeric inputs, slider bars)     #
#   is changed  and refreshes the output graphs generated                                               #
#   This file creates the actual graph output based on the dataframe or vector retuned by the           #
#   function call made                                                                                  #
#                                                                                                       #
#########################################################################################################
#########################################################################################################



############################ LIBRARY IMPORT ##############################
# plotly: Plotly is an analytics and data visualization tool.
# It offers an API for R to create aesthetic data visualizations
# REFERENCE: https://plot.ly/r/
##########################################################################

library(plotly)


############################ LIBRARY IMPORT ##############################
# BayesBridge: R library for bayesian bridge regression. Used for population generation
# REFERENCE: https://cran.r-project.org/web/packages/BayesBridge/BayesBridge.pdf
##########################################################################

library(BayesBridge)

############################ LIBRARY IMPORT ##############################
# scales: R library to build for scaling data to aesthetics and allow
# dollar and percent conversions
# REFERENCE: https://cran.r-project.org/web/packages/scales/scales.pdf
##########################################################################

library(scales)


############################# R COMMAND ##################################
# rm() removes a variable from the workspace. A list of all the variables 
# in the workspace is passed as a parameter to rm to clear all variables 
# in the workspace to ensure that the model uses only the updated values
# sent by the user for computations
##########################################################################

rm(list = ls())


############################# R COMMAND ##################################
# source() notifies R that the variables and functions present in the
# file passed as an argument to it should be loaded to the r environment   
# and is avaiable for use in this file
##########################################################################
# we source our model logic present in functions.R

source("functions.R")

# Shiny Server definition
shinyServer(function(input,output,clientData,session) {
  
  # global parameters are set here for maximum allowble age in the model and inflation rate which is defaulted to 2%  
  max_age <<- 100
  inflation<<-2
  
  ################## Setting the dollar format for the dollar() function used in the program #################
  dollar_format(prefix = "", suffix = "$",largest_with_cents = 1e-2, big.mark = ",", negative_parens = FALSE)
  
  # rendering a dynamic sliderInput in the population an plan features
  # screen when user selects customize population
  output$ui <- renderUI({
    sliderInput(
      'median','Median Age of Active Participants:',min = input$ea, max = input$retire, step = 1,value = 45
    )
  })
  
  ### median salary growth rate,actual salary growth rate, expected salary growth rate 
  ### is set to the payroll growth rate entered by the user +inflation
  median_sgr <<-
    sgr <<- a_sgr <<-
    reactive({
      input$sal_growth_rate + inflation
    })
  
  ### median discount rate,plan discount rate is set to rate of return entered by the user +inflation
  median_dr <<-
    drate <<- reactive({
      inflation + input$discount_rate
    })
  
  ### payroll_gr is set to the payroll growth rate entered by the user
  payroll_gr <<-reactive({
      input$sal_growth_rate
    })
  
  ### actual salary is stored for some computations ahead
  act_sal<-reactive({ get_act_sal(input$ea,input$retire,a_sgr()/100) })
  
  ### After the function call 'get_retirement_annuity' retire annuity is returned in dollar form at this poin to the ui.R
  output$retire_annuity <-
    renderText({dollar(get_retirement_annuity(input$ea,input$retire,a_sgr()/100,sgr()/100,input$afc,input$benefit_factor/100)*salary_at_retire())})
  
  ### similarly replacement rate and present value of annuity is returned
  output$replace_rate <-
    renderText({percent(get_replacement_rate(input$ea,input$retire,a_sgr()/100,sgr()/100,input$afc,input$benefit_factor/100)/100)})
  
  output$pv_annuity<-renderText({
    dollar(round(get_rPVFBx_after_r(input$ea,input$retire,drate()/100,a_sgr()/100,sgr()/100,input$cola/100,input$afc,input$benefit_factor/100,mort_tab())[2]*salary_at_retire(),2))
      })
  
  ########################### proportion Population code ###############################################
  
  ### populate stores the active population vector returned by functions.R file
  populate <- reactive({
    generate_pop(input$ea,input$retire,input$pop_type,input$pop_size,input$median)
  })
  
  ### populate_retirees stores the retirees population vector returned by functions.R file
  populate_retirees <- reactive({
    population_retirees(input$ea,input$retire,mort_tab(),populate())
  })
  
  ### the population distribution is ploted using the plotly package and using populate and populate_retirees
  output$pop <- renderPlotly({
    plot_ly(
      x = populate(),opacity = 0.7,type = "histogram",name = "Current Employees", xbins =
        list(start = input$ea,end = max_age),borderwidth=9
    ) %>%
      add_trace(x = populate_retirees(),name = "Retirees",type = "histogram") %>%
      layout(
        barmode = "overlay",
        xaxis = list(
          title = 'Age',ticks = 'outside', range = c(input$ea,max_age),showticklabels =
            TRUE,showgrid = TRUE
        ),
        yaxis = list(
          title = 'Count',ticks = 'outside',showticklabels = TRUE
        )
      )
  })
  
  ### the number of retirees is returned to ui.R
  output$no_of_r <-
    renderText({
      a <- table(c(populate(),populate_retirees()))
      sum(a[names(a) > retire])
    })
  
  # Salary at retirement is calculated using the salary inputed by the user at entry age and returned to ui.R
  salary_at_retire<-reactive({get_sal_ca_ea(input$ea,input$retire,input$retire,input$sal_in_ea,inflation,sgr())})
  output$salary_at_retire <- renderText({dollar(salary_at_retire())})


  ################################## Mortality tables ########################################
  ## This code converts the mortality table name selected by the user into column numbers in 
  ## the data frame for ease of access
  ## changes have to be made to this code if the model plans to accomodate more mortality tables
  mort_tab <- reactive({
    switch(
      input$mort,"RP2014_Employee_total" = 2,"RP2000_Employee_total" =
        4,"RP2010_Employee_total" = 6
    )
  })
  
  
  ############################ Normal Cost ##################################
  ## The normal cost for an individual fromm hire to retirement is stored in nc
  nc <- reactive({
    get_NC(
      ea = input$ea,
      retire = input$retire,
      i = drate() / 100,
      a_sgr = a_sgr() / 100,
      sgr = sgr() / 100,
      cola = input$cola / 100,
      afc = as.numeric(input$afc),
      bf = input$benefit_factor / 100,
      cm = input$cost_method,
      mort = mort_tab(),
      vesting = as.numeric(input$vest)
    )
  })
  
  ## a plotly graph output for NC/Payroll is created here using nc and act_sal variables in this file
  output$nc_payroll <- renderPlotly({
    plot_ly(
      x = seq(input$ea,input$retire),
      y = (nc()/act_sal()) * 100,
      name = 'NC/Payroll',
      mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'Normal Cost/Payroll (%)',ticks = 'outside')
      )
  })
  
  ## a plotly graph output for NCis created here using nc
  output$nc <- renderPlotly({
    plot_ly(
      x = seq(input$ea,input$retire),y = nc(),name = 'Normal Cost',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'Normal Cost ($)',ticks = 'outside')
      )
  })
  
  ## Cumulative Normal cost for an individual from hire to retirement is returned as a text output to the ui.R
  output$total_nc <- renderText({
    paste("Normal cost: ", dollar(round(sum(nc()),3)))
  })

  ### bar chart graph for normal cost of the population and an overlay of payroll for the entire population
  ### NOT USED IN THE CURRENT MODEL
  output$nc_pop <- renderPlotly({
    plot_ly(
      x = seq(input$ea,input$retire),y = table(populate()) * act_sal()
      ,name = 'Payroll',type = "bar") %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'Normal Cost & Payroll($)',ticks = 'outside'),
        barmode = "overlay"
      ) %>% add_trace(y = table(populate()) * nc(),name = 'Normal Cost')
  })
  
  ### data for normal cost of different age groups of the population
  ### NOT USED IN THE CURRENT MODEL
  pie_nc_pop <- reactive({
    get_nc_pop(
      pop = table(populate()),
      ea = input$ea,
      retire = input$retire,
      i = drate() / 100,
      a_sgr = a_sgr() / 100,
      sgr = sgr() / 100,
      cola = input$cola / 100,
      afc = as.numeric(input$afc),
      bf = input$benefit_factor / 100,
      cm = input$cost_method,
      mort = mort_tab(),
      vesting = as.numeric(input$vest)
    )
  })
  
  ### pie chart for normal cost of the population 
  ### NOT USED IN THE CURRENT MODEL
  output$nc_pie_pop <- renderPlotly({
    plot_ly(
      labels = pie_nc_pop()[,1],values = pie_nc_pop()[,2],name = 'Normal Cost',type =
        "pie"
    )
  })
  
  ### total normal cost is calculated and returned to ui.R
  total_nc_pop<- reactive({round(sum(table(populate()) * nc()),3)})
  output$total_nc_pop <- renderText({
    paste("Normal cost: ",total_nc_pop(), "$")
  })
  
  ############################ AAL ##################################
  ## The AAL for an individual fromm hire to retirement is stored in nc
  aal <-
    reactive({
      get_AAL(
        ea = input$ea,
        retire = input$retire,
        i = drate() / 100,
        a_sgr = a_sgr() / 100,
        sgr = sgr() / 100,
        cola = input$cola / 100,
        afc = as.numeric(input$afc),
        bf = input$benefit_factor / 100,
        cm = input$cost_method,
        mort = mort_tab(),
        vesting = as.numeric(input$vest)
      )
    })
  
  ## a plotly graph output for AAL/Payroll is created here using aal and act_sal variables in this file
  output$aal_payroll <- renderPlotly({
    plot_ly(
      x = seq(input$ea,max_age),y = 100 * (aal()[1:(length(age))] / act_sal()),name = 'AAL/Payroll',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'AAL/Payroll (%)',ticks = 'outside')
      )
  })
  
  ## a plotly graph output for AAL is created here using aal
  output$aal <- renderPlotly({
    plot_ly(
      x = seq(input$ea,max_age),y = aal(),name = 'AAL',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'AAL ($)',ticks = 'outside')
      )
  })
  
  ## Cumulative AAL for an individual from hire to retirement is returned as a text output to the ui.R
  output$total_aal <- renderText({
    paste("Actuarial Accrued Liability: ", dollar(round(sum(aal()),3)))
  })
  
  
  ### bar chart graph for AAL of the population and an overlay of payroll for the entire population
  ### NOT USED IN THE CURRENT MODEL
  output$aal_pop <- renderPlotly({
    plot_ly(
      x = seq(input$ea,max_age),opacity = 0.8,y = table(populate()) * act_sal(),name = 'Payroll',type = "bar"
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'AAL & Payroll($)',ticks = 'outside'),
        barmode = "overlay"
      ) %>% add_trace(y = table(populate()) * aal()[1:length(table(populate()))],name = 'AAL')
  })
  
  
  ### data for AAL of different age groups of the population
  ### NOT USED IN THE CURRENT MODEL
  pie_aal_pop <- reactive({
    get_aal_pop(
      pop = table(c(populate(),populate_retirees())),
      ea = input$ea,
      retire = input$retire,
      i = drate() / 100,
      a_sgr = a_sgr() / 100,
      sgr = sgr() / 100,
      cola = input$cola / 100,
      afc = as.numeric(input$afc),
      bf = input$benefit_factor / 100,
      cm = input$cost_method,
      mort = mort_tab(),
      vesting = as.numeric(input$vest)
    )
  })
  
  
  ### pie chart for AAL of the population 
  ### NOT USED IN THE CURRENT MODEL
  output$aal_pie_pop <- renderPlotly({
    plot_ly(
      labels = pie_aal_pop()[,1],values = pie_aal_pop()[,2],name = 'AAL',type =
        "pie"
    )
  })
  
  
  ### total AAL is calculated and returned to ui.R
  total_aal_pop<-reactive({sum(c(table(populate()),table(populate_retirees())) * aal())})
  output$total_aal_pop <- renderText({
    paste("Actuarial Accrued Liability: ",total_aal_pop() , "$")
  })
  
  
  ### total UAAL and total ARC for the plan 
  total_uaal_pop<-reactive({total_aal_pop()*(1-(input$percent/100))})
  total_ARC<-reactive({sum(
    get_ARC(pop=table(c(populate(),populate_retirees())),
            ea=input$ea,retire=input$retire,median_p=input$percent/100,median_dr=median_dr()/100,median_sgr=median_sgr()/100,
            i=drate()/100,a_sgr=a_sgr()/100,sgr=sgr()/100,pgr=payroll_gr()/100,cola=input$cola/100,afc=input$afc,bf=input$benefit_factor/100,
            cm=input$cost_method,mort=mort_tab(),vesting=input$vest,amortization=input$amort))})
  
  
  ####################################################################
  ########################## Statistics ##############################
  ####################################################################
  
  #### The data frame returned by the get_stat function is store here and returned as a table to ui.R
  output$stats <- renderTable({
    get_stat(
      ea=input$ea,
      retire=input$retire,
      active=populate(),
      retirees=populate_retirees(),
      i=drate()/100,
      a_sgr=a_sgr()/100,
      sgr=sgr()/100,
      cola=input$cola/100,
      afc=input$afc,
      bf=input$benefit_factor,
      cm=input$cost_method,
      mort=mort_tab(),
      vesting=input$vest)
  })
  
  ############################# Summary ######################################
  #### The data frame returned by the get_summary function is store here and returned as a table to ui.R
  output$summary<- renderTable({
    get_summary(pop=table(c(populate(),populate_retirees())),
                ea=input$ea,retire=input$retire,median_p=input$percent/100,median_dr=median_dr()/100,median_sgr=median_sgr()/100,
                i=drate()/100,a_sgr=a_sgr()/100,sgr=sgr()/100,pgr=payroll_gr()/100,cola=input$cola/100,afc=input$afc,bf=input$benefit_factor/100,
                cm=input$cost_method,mort=mort_tab(),vesting=input$vest,amortization=input$amort)

  })
    
  
  ##############################################################################
  ############################ Discount Rate ##################################
  ##############################################################################
  
  ############################# Funding Ratio ##################################################
  
  # a function to find finding ratio vector for different cost methods wrt discount rates
  # from 2 to 10% with intervals of 0.25%. Each discount rate value is passed indiviually to the
  # get_FR function in functions.R 
  
  funding_ratio_dr <- function(cost_m) {
    fr <-  reactive({
      sapply(seq(2,10,0.25), function(k)
        get_FR(
          pop = table(c(populate(),populate_retirees())),
          ea = input$ea,
          retire = input$retire,
          median_p = input$percent / 100,
          median_dr = median_dr() / 100,
          median_sgr = median_sgr() / 100,
          i = k / 100,
          a_sgr = a_sgr() / 100,
          sgr = sgr() / 100,
          cola = input$cola / 100,
          afc = as.numeric(input$afc),
          bf = input$benefit_factor / 100,
          cm = cost_m,
          mort = mort_tab(),
          vesting = as.numeric(input$vest)
        ))
      
    })
    return(fr())
  }
  
  ### the funding ratio vector is plotted for both cost methods below and returned to
  ### the sensitivity tests page for discount rate in ui.R
  
  output$fr_dr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),
      y = as.numeric(funding_ratio_dr('EAN') ### funding_ratio_dr defined above is used with the cost method EAN
                     * 100),
      name = 'EAN',mode ='markers+lines') %>%
      layout(
        xaxis = list(title = 'Discount Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'Funding Ratio (%)',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(funding_ratio_dr('PUC') ### funding_ratio_dr defined above is used with the cost method PUC
                * 100),name = 'PUC') %>%
      layout(
        # An annotation is made in the plotly graph to indicate median asset condition
        annotations =
          list(
            x = median_dr(),y = input$percent,
            text = 'Median Asset Condition', xref = 'Discount Rate (%)', yref =
              'Funding Ratio (%)', ax = 50, ay=-100
          )
      )
  })
  
  
  ################################ ARC ###############################################33
  ### total payroll for entire population
  total_pay <-
    reactive({
      sum(c(table(populate())) * act_sal())
    })
  
  # Median ARC values for both cost methods are stored here
  arc_dr_median <- reactive({
    sapply(c('EAN','PUC'), function(k)
      sum(
        get_ARC(
          pop = table(c(populate(),populate_retirees())),
          ea = input$ea,
          retire = input$retire,
          median_p = input$percent / 100,
          median_dr = median_dr() / 100,
          median_sgr = median_sgr() / 100,
          i = drate() / 100,
          a_sgr = a_sgr() / 100,
          sgr = sgr() / 100,
          pgr = payroll_gr()/100,
          cola = input$cola / 100,
          afc = as.numeric(input$afc),
          bf = input$benefit_factor / 100,
          cm = k,
          mort = mort_tab(),
          vesting = as.numeric(input$vest),
          amortization = as.numeric(input$amort)
        )
      ))
  })
  
  # a function to find ARC vector for different cost methods wrt discount rates
  # from 2 to 10% with intervals of 0.25%. Each discount rate value is passed indiviually to the
  # get_ARC function in functions.R 
  
  arc_dr <- function(cost_m) {
    arc <- reactive({
      sapply(seq(2,10,0.25), function(k)
        sum(
          get_ARC(
            pop = table(c(populate(),populate_retirees())),
            ea = input$ea,
            retire = input$retire,
            median_p = input$percent / 100,
            median_dr = median_dr() / 100,
            median_sgr = median_sgr() / 100,
            i = k / 100,
            a_sgr = a_sgr() / 100,
            sgr = sgr() / 100,
            pgr = payroll_gr()/100,
            cola = input$cola / 100,
            afc = as.numeric(input$afc),
            bf = input$benefit_factor / 100,
            cm = cost_m,
            mort = mort_tab(),
            vesting = as.numeric(input$vest),
            amortization = as.numeric(input$amort)
          )
        ))
    })
    return(arc())
  }
  
  
  ### the ARC/Payroll vector is plotted for both cost methods below and returned to
  ### the sensitivity tests page for discount rate in ui.R
  
  output$arc_dr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(arc_dr('EAN'))  ### arc_dr defined above is used with the cost method EAN
      / total_pay() * 100,name = 'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Discount Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'ARC/Payroll',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(arc_dr('PUC')) ### arc_dr defined above is used with the cost method PUC
                / total_pay() * 100,name = 'PUC') %>%
      layout(
        # An annotation is made in the plotly graph to indicate median asset condition
        annotations =
          list(
            list(
              #x = input$rate,
              x = median_dr(),
              y = arc_dr_median()[1] / total_pay() * 100,text = 'Median Asset Condition', xref =
                'Discount Rate (%)', yref = 'ARC/Payroll', ax = -50, ay=50
            ),
            list(
              #x = input$rate,
              x = median_dr(),
              y = arc_dr_median()[2] / total_pay() * 100,text = 'Median Asset Condition', xref =
                'Discount Rate (%)', yref = 'ARC/Payroll', ax = -50, ay=-50
            )
          )
      )
  })
  
  ####################################################################################
  ############################ Salary growth rate ##############################
  ################################################################################
  
  ######################## Funding Ratio ###################################
  
  # a function to find funding ratio vector for different cost methods wrt salary growth rates
  # from 2 to 10% with intervals of 0.25%. Each salary growth rate value is passed indiviually to the
  # get_FR function in functions.R 
  
  funding_ratio_sgr <- function(cost_m) {
    fr <-
      reactive({
        sapply(seq(2,10,0.25), function(k)
          get_FR(
            pop = table(c(populate(),populate_retirees())),
            ea = input$ea,
            retire = input$retire,
            median_p = input$percent / 100,
            median_dr = median_dr() / 100,
            median_sgr = median_sgr() / 100,
            i = drate() / 100,
            a_sgr = a_sgr() / 100,
            sgr = k / 100,
            cola = input$cola / 100,
            afc = as.numeric(input$afc),
            bf = input$benefit_factor / 100,
            cm = cost_m,
            mort = mort_tab(),
            vesting = as.numeric(input$vest)
          ))
      })
    return(fr())
  }
  
  
  ### the funding ratio vector is plotted for both cost methods below and returned to
  ### the sensitivity tests page for salary growth rate in ui.R
  
  output$fr_sgr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(funding_ratio_sgr('EAN')### funding_ratio_sgr defined above is used with the cost method EAN 
        * 100),name ='EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Salary Growth Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'Funding Ratio (%)',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(funding_ratio_sgr('PUC') ### funding_ratio_sgr defined above is used with the cost method PUC
      * 100),name = 'PUC') %>%
      layout(
        # An annotation is made in the plotly graph to indicate median asset condition
        annotations = list(
          x = median_sgr(),y = input$percent,
          text = 'Median Asset Condition', xref = 'Salary Growth Rate %', yref =
            'Funding Ratio %', ax = 50
        )
      )
  })
  
  ################################## ARC #########################################

  # Median ARC values for both cost methods are stored here
  
  arc_sgr_median <- reactive({
    sapply(c('EAN','PUC'), function(k)
      sum(
        get_ARC(
          pop = table(c(populate(),populate_retirees())),
          ea = input$ea,
          retire = input$retire,
          median_p = input$percent / 100,
          median_dr = median_dr() / 100,
          median_sgr = median_sgr() / 100,
          i = drate() / 100,
          a_sgr = a_sgr() / 100,
          sgr = sgr() / 100,
          pgr = payroll_gr()/100,
          cola = input$cola / 100,
          afc = as.numeric(input$afc),
          bf = input$benefit_factor / 100,
          cm = k,
          mort = mort_tab(),
          vesting = as.numeric(input$vest),
          amortization = as.numeric(input$amort)
        )
      ))
  })
  
  # a function to find ARC vector for different cost methods wrt salary growth rates
  # from 2 to 10% with intervals of 0.25%. Each salary growth rate value is passed indiviually to the
  # get_ARC function in functions.R 
  
  arc_sgr <- function(cost_m) {
    arc <- reactive({
      sapply(seq(2,10,0.25), function(k)
        sum(
          get_ARC(
            pop = table(c(populate(),populate_retirees())),
            ea = input$ea,
            retire = input$retire,
            median_p = input$percent / 100,
            median_dr = median_dr() / 100,
            median_sgr = median_sgr() / 100,
            i = drate() / 100,
            a_sgr = a_sgr() / 100,
            sgr = k / 100,
            pgr = payroll_gr()/100,
            cola = input$cola / 100,
            afc = as.numeric(input$afc),
            bf = input$benefit_factor / 100,
            cm = cost_m,
            mort = mort_tab(),
            vesting = as.numeric(input$vest),
            amortization = as.numeric(input$amort)
          )
        ))
    })
    return(arc())
  }
  
  
  ### the ARC/Payroll vector is plotted for both cost methods below and returned to
  ### the sensitivity tests page for salary growth rate in ui.R
  
  output$arc_sgr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(arc_sgr('EAN'))### arc_sgr defined above is used with the cost method EAN 
      / total_pay() * 100,name = 'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Salary Growth Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'ARC/Payroll',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(arc_sgr('PUC') ### arc_sgr defined above is used with the cost method PUC 
        / total_pay() * 100),name = 'PUC') %>%
      layout(
        # An annotation is made in the plotly graph to indicate median asset condition
        annotations = list(
          list(
            x = median_sgr(),
            y = arc_sgr_median()[1] / total_pay() * 100,text = 'Median Asset Condition', xref =
              'Salary Growth Rate (%)', yref = 'ARC/Payroll', ax = 50,ay=-100
          ),
          list(
            x = median_sgr(),
            y = arc_sgr_median()[2] / total_pay() * 100,text = 'Median Asset Condition', xref =
              'Salary Growth Rate (%)', yref = 'ARC/Payroll', ax = -50
          )
        )
      )
  })
  
  ############################## Mortality Table ########################################

  # a function to find ARC vector for different cost methods wrt different mortality rate tables
  # Each table is passed indiviually to the get_ARC function in functions.R 
  
  arc_mort <- function(cost_m) {
    arc <- reactive({
      sapply(seq(2,4,6), function(k)
          get_ARC(
            pop = table(c(populate(),populate_retirees())),
            ea = input$ea,
            retire = input$retire,
            median_p = input$percent / 100,
            median_dr = median_dr() / 100,
            median_sgr = median_sgr() / 100,
            i = drate() / 100,
            a_sgr = a_sgr() / 100,
            sgr = sgr() / 100,
            pgr = payroll_gr()/100,
            cola = input$cola / 100,
            afc = as.numeric(input$afc),
            bf = input$benefit_factor / 100,
            cm = cost_m,
            mort = k,
            vesting = as.numeric(input$vest),
            amortization = as.numeric(input$amort)
          )
        )
    })
    return(arc())
  }
  
  ## Plotly graph for different ARC values under differnt mortality tables
  output$arc_mort <- renderPlotly({
    plot_ly(
      x = seq(2,4,6),y = (as.numeric(arc_mort('EAN')) / total_pay()) * 100,name = 'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Mortality tables',ticks = 'outside'),
        yaxis = list(title = 'ARC/Payroll',ticks = 'outside',title="This figure illustrates how the funding ratio will change if the mortality table is changed.")
      ) %>%
      add_trace(y = as.numeric(arc_mort('PUC')),name = 'PUC')
  })
  
})