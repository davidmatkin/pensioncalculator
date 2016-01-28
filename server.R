library(ggplot2)
library(plotly)
library(graphics)
library(BayesBridge)
rm(list = ls())

source("E://SUNYALBANY/Pension_Simulation_Project/Code/functions.R")


shinyServer(function(input,output) {
  output$ui <- renderUI({
    sliderInput(
      'median','Median',min = input$ea, max = 65, step = 1,value = 45
    )
  })
  
  
  ########################### Population ###############################################
  populate <- reactive({
    if (input$pop_type == 'Bell curve')
      pop1 <-
        round(rtnorm(
          input$pop_size,input$median,15,left = input$ea,right = 65
        ))
    if (input$pop_type == 'Over mature')
      pop1 <-
        round(rtnorm(
          input$pop_size,65,15,left = input$ea,right = 65
        ))
    if (input$pop_type == 'Under mature')
      pop1 <-
        round(rtnorm(
          input$pop_size,input$ea,15,left = input$ea,right = 65
        ))
    if (input$pop_type == 'Uniform')
      pop1 <- round(runif(input$pop_size,min = input$ea,max = 65))
    
    return(pop1)
  })
  
  output$pop <- renderPlot({
    qplot(
      x = populate(),binwidth = 5,steps = 5,xlab = 'Age',ylab = 'Count', xlim = c(input$ea,65)
    )
  })
  
  output$population_dist <-
    renderTable({
      table(as.numeric(populate()))
    })
  
  
  ################################## Mortality tables ########################################
  mort_tab <- reactive({
    switch(
      input$mort,"Male RP-2000 Rates" = 2,"Female RP-2000 Rates" = 3,"RP-2000 average" =
        4,
      "Male RP-2000 10 years projection" = 5,"Female RP-2000 10 years projection" =
        6,
      "2010 average" = 7,"Male RP-2014" = 8,"Female RP-2014" = 9,"RP-2014 average" =
        10,"GAM-1994 Male" = 11,
      "GAM-1994 Female" = 12,"GAM-1994 average" = 13
    )
  })
  
  
  ############################ Normal Cost ##################################
  nc <- reactive({
    get_NC(
      ea = input$ea,
      i = input$discount_rate / 100,
      a_sgr = input$act_sal_growth_rate / 100,
      sgr = input$sal_growth_rate / 100,
      cola = as.numeric(input$cola),
      bf = as.numeric(input$benefit_factor),
      cm = input$cost_method,
      mort = mort_tab(),
      vesting = as.numeric(input$vest)
    )
  })
  
  output$nc <- renderPlot({
    qplot(
      y = nc(),x = seq(input$ea,65),color = "red",geom = "jitter",ylab = 'Normal Cost',xlab =
        'Age'
    )
  })
  
  ############################ AAL ##################################
  aal <-
    reactive({
      get_AAL(
        ea = input$ea,
        i = input$discount_rate / 100,
        a_sgr = input$act_sal_growth_rate / 100,
        sgr = input$sal_growth_rate / 100,
        cola = as.numeric(input$cola),
        bf = as.numeric(input$benefit_factor),
        cm = input$cost_method,
        mort = mort_tab(),
        vesting = as.numeric(input$vest)
      )
    })
  
  output$aal <- renderPlotly({
    #aal_df<-data.frame(seq(input$ea,65),aal())
    #colnames(aal_df)<-c('Age','AAL')
    plot_ly(
      x = seq(input$ea,65),y = aal(),name = 'AAL',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'AAL',ticks = 'outside')
      )
  })
  
  
  ############################ FR ##################################
  
  funding_ratio_dr <- function(cost_m) {
    fr <-
      reactive({
        sapply(seq(6,9,0.25), function(k)
          get_FR(
            pop = table(populate()),
            ea = input$ea,
            median_p = input$percent / 100,
            median_dr = input$rate / 100,
            median_sgr = input$rate2 / 100,
            i = k / 100,
            a_sgr = input$act_sal_growth_rate / 100,
            sgr = input$sal_growth_rate / 100,
            cola = as.numeric(input$cola),
            bf = as.numeric(input$benefit_factor),
            cm = cost_m,
            mort = mort_tab(),
            vesting = as.numeric(input$vest)
          ))
        
      })
    return(fr())
  }
  
  funding_ratio_sgr <- function(cost_m) {
    fr <-
      reactive({
        sapply(seq(3,8,0.25), function(k)
          get_FR(
            pop = table(populate()),
            ea = input$ea,
            median_p = input$percent / 100,
            median_dr = input$rate / 100,
            median_sgr = input$rate2 / 100,
            i = input$discount_rate / 100,
            a_sgr = input$act_sal_growth_rate / 100,
            sgr = k / 100,
            cola = as.numeric(input$cola),
            bf = as.numeric(input$benefit_factor),
            cm = cost_m,
            mort = mort_tab(),
            vesting = as.numeric(input$vest)
          ))
      })
    return(fr())
  }
  output$fr_dr <- renderPlotly({
    plot_ly(
      x = seq(6,9,0.25),y = as.numeric(funding_ratio_dr('EAN') * 100),name = 'EAN',mode =
        'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Discount Rate',ticks = 'outside'),
        yaxis = list(title = 'Funding Ratio (%)',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(funding_ratio_dr('PUC') * 100),name = 'PUC')
  })
  
  output$fr_sgr <- renderPlotly({
    plot_ly(
      x = seq(3,8,0.25),y = as.numeric(funding_ratio_sgr('EAN') * 100),name =
        'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Salary Growth Rate',ticks = 'outside'),
        yaxis = list(title = 'Funding Ratio (%)',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(funding_ratio_sgr('PUC') * 100),name = 'PUC')
  })

#################################### ARC ###########################################
  
  arc_dr <- function(cost_m){
    arc<- reactive({
    sapply(seq(6,9,0.25), function(k)
      get_ARC(
        pop=table(populate()),
        ea = input$ea,
        median_p = input$percent / 100,
        median_dr = input$rate / 100,
        median_sgr = input$rate2 / 100,
        i = k / 100,
        a_sgr = input$act_sal_growth_rate / 100,
        sgr = input$sal_growth_rate / 100,
        cola = as.numeric(input$cola),
        bf = as.numeric(input$benefit_factor),
        cm = cost_m,
        mort = mort_tab(),
        vesting = as.numeric(input$vest),
        amortization = as.numeric(input$amort)
      ))
  })
    return(arc())
  }
  
  arc_sgr <- function(cost_m){
    arc<- reactive({
      sapply(seq(3,8,0.25), function(k)
        get_ARC(
          pop=table(populate()),
          ea = input$ea,
          median_p = input$percent / 100,
          median_dr = input$rate / 100,
          median_sgr = input$rate2 / 100,
          i = input$discount_rate / 100,
          a_sgr = input$act_sal_growth_rate / 100,
          sgr = k / 100,
          cola = as.numeric(input$cola),
          bf = as.numeric(input$benefit_factor),
          cm = cost_m,
          mort = mort_tab(),
          vesting = as.numeric(input$vest),
          amortization = as.numeric(input$amort)
        ))
    })
    return(arc())
  }
  
  
  output$arc_dr <- renderPlotly({
    plot_ly(
      x = seq(6,9,0.25),y = as.numeric(arc_dr('EAN')),name = 'EAN',mode = 'markers+lines') %>%
      layout(
        xaxis = list(title = 'Discount Rate',ticks = 'outside'),
        yaxis = list(title = 'ARC',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(arc_dr('PUC')),name = 'PUC')
  })
  
  
  output$arc_sgr <- renderPlotly({
    plot_ly(
      x = seq(3,8,0.25),y = as.numeric(arc_sgr('EAN')),name ='EAN',mode = 'markers+lines') %>%
      layout(
        xaxis = list(title = 'Salary Growth Rate',ticks = 'outside'),
        yaxis = list(title = 'ARC',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(arc_sgr('PUC')),name = 'PUC')
    
  })
  
  
  
  
})
