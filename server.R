library(plotly)
library(BayesBridge)
library(scales)

rm(list = ls())

source("functions.R")

shinyServer(function(input,output,clientData,session) {
  
  max_age <<- 100
  inflation<<-2
  
  dollar_format(prefix = "", suffix = "$",largest_with_cents = 1e-2, big.mark = ",", negative_parens = FALSE)
  
  
  output$ui <- renderUI({
    sliderInput(
      'median','Median Age of Active Participants:',min = input$ea, max = input$retire, step = 1,value = 45
    )
  })
  
  median_sgr <<-
    sgr <<- a_sgr <<-
    reactive({
      input$sal_growth_rate + inflation
    })
  
  median_dr <<-
    drate <<- reactive({
      inflation + input$discount_rate
    })
  
  payroll_gr <<-reactive({
      input$sal_growth_rate
    })
  
  act_sal<-reactive({ get_act_sal(input$ea,input$retire,a_sgr()/100) })
  
  output$retire_annuity <-
    renderText({dollar(get_retirement_annuity(input$ea,input$retire,a_sgr()/100,sgr()/100,input$afc,input$benefit_factor/100)*salary_at_retire())})
  
  output$replace_rate <-
    renderText({percent(get_replacement_rate(input$ea,input$retire,a_sgr()/100,sgr()/100,input$afc,input$benefit_factor/100)/100)})
  
  output$pv_annuity<-renderText({
    dollar(round(get_rPVFBx_after_r(input$ea,input$retire,drate()/100,a_sgr()/100,sgr()/100,input$cola/100,input$afc,input$benefit_factor/100,mort_tab())[2]*salary_at_retire(),2))
      })
  
  ########################### proportion Population code ###############################################
  
  populate <- reactive({
    generate_pop(input$ea,input$retire,input$pop_type,input$pop_size,input$median)
  })
  
  populate_retirees <- reactive({
    population_retirees(input$ea,input$retire,mort_tab(),populate())
  })
  
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
  
  
  output$no_of_r <-
    renderText({
      a <- table(c(populate(),populate_retirees()))
      sum(a[names(a) > retire])
    })
  
  salary_at_retire<-reactive({get_sal_ca_ea(input$ea,input$retire,input$retire,input$sal_in_ea,inflation,sgr())})
  
  output$salary_at_retire <- renderText({dollar(salary_at_retire())})


  ################################## Mortality tables ########################################
  mort_tab <- reactive({
    switch(
      input$mort,"RP2014_Employee_total" = 2,"RP2000_Employee_total" =
        4,"RP2010_Employee_total" = 6
    )
  })
  
  
  ############################ Normal Cost ##################################
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
  
  
  output$nc <- renderPlotly({
    plot_ly(
      x = seq(input$ea,input$retire),y = nc(),name = 'Normal Cost',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'Normal Cost ($)',ticks = 'outside')
      )
  })
  
  output$total_nc <- renderText({
    paste("Normal cost: ", dollar(round(sum(nc()),3)))
  })

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
  
  output$nc_pie_pop <- renderPlotly({
    plot_ly(
      labels = pie_nc_pop()[,1],values = pie_nc_pop()[,2],name = 'Normal Cost',type =
        "pie"
    )
  })
  
  total_nc_pop<- reactive({round(sum(table(populate()) * nc()),3)})
  
  output$total_nc_pop <- renderText({
    paste("Normal cost: ",total_nc_pop(), "$")
  })
  
  ############################ AAL ##################################
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
  
  output$aal_payroll <- renderPlotly({
    plot_ly(
      x = seq(input$ea,max_age),y = 100 * (aal()[1:(length(age))] / act_sal()),name = 'AAL/Payroll',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'AAL/Payroll (%)',ticks = 'outside')
      )
  })
  
  output$aal <- renderPlotly({
    plot_ly(
      x = seq(input$ea,max_age),y = aal(),name = 'AAL',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Age',ticks = 'outside'),
        yaxis = list(title = 'AAL ($)',ticks = 'outside')
      )
  })
  
  output$total_aal <- renderText({
    paste("Actuarial Accrued Liability: ", dollar(round(sum(aal()),3)))
  })
  
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
  
  output$aal_pie_pop <- renderPlotly({
    plot_ly(
      labels = pie_aal_pop()[,1],values = pie_aal_pop()[,2],name = 'AAL',type =
        "pie"
    )
  })
  
  
  total_aal_pop<-reactive({sum(c(table(populate()),table(populate_retirees())) * aal())})
  total_uaal_pop<-reactive({total_aal_pop()*(1-(input$percent/100))})
  total_ARC<-reactive({sum(
    get_ARC(pop=table(c(populate(),populate_retirees())),
            ea=input$ea,retire=input$retire,median_p=input$percent/100,median_dr=median_dr()/100,median_sgr=median_sgr()/100,
            i=drate()/100,a_sgr=a_sgr()/100,sgr=sgr()/100,pgr=payroll_gr()/100,cola=input$cola/100,afc=input$afc,bf=input$benefit_factor/100,
            cm=input$cost_method,mort=mort_tab(),vesting=input$vest,amortization=input$amort))})
      
  output$total_aal_pop <- renderText({
    paste("Actuarial Accrued Liability: ",total_aal_pop() , "$")
  })
  
  ####################################################################
  ########################## Statistics ##############################
  ####################################################################
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
  
  output$fr_dr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(funding_ratio_dr('EAN') * 100),name = 'EAN',mode =
        'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Discount Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'Funding Ratio (%)',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(funding_ratio_dr('PUC') * 100),name = 'PUC') %>%
      layout(
        annotations =
          list(
            #x = input$rate,y=input$percent,
            x = median_dr(),y = input$percent,
            text = 'Median Asset Condition', xref = 'Discount Rate (%)', yref =
              'Funding Ratio (%)', ax = 50, ay=-100
          )
      )
  })
  
  
  ################################ ARC ###############################################33
  total_pay <-
    reactive({
      sum(c(table(populate())) * act_sal())
    })
  
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
  
  
  output$arc_dr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(arc_dr('EAN')) / total_pay() * 100,name = 'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Discount Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'ARC/Payroll',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(arc_dr('PUC')) / total_pay() * 100,name = 'PUC') %>%
      layout(
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
  ####################################################################################
  ############################ Salary growth rate ##############################
  ################################################################################
  
  ######################## Funding Ratio ###################################
  
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
  
  output$fr_sgr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(funding_ratio_sgr('EAN') * 100),name =
        'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Salary Growth Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'Funding Ratio (%)',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(funding_ratio_sgr('PUC') * 100),name = 'PUC') %>%
      layout(
        annotations = list(
          #x = input$rate2,y=input$percent,
          x = median_sgr(),y = input$percent,
          text = 'Median Asset Condition', xref = 'Salary Growth Rate %', yref =
            'Funding Ratio %', ax = 50
        )
      )
  })
  
  ################################## ARC #########################################
  
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
  
  output$arc_sgr <- renderPlotly({
    plot_ly(
      x = seq(2,10,0.25),y = as.numeric(arc_sgr('EAN')) / total_pay() * 100,name = 'EAN',mode = 'markers+lines'
    ) %>%
      layout(
        xaxis = list(title = 'Salary Growth Rate (%)',ticks = 'outside'),
        yaxis = list(title = 'ARC/Payroll',ticks = 'outside')
      ) %>%
      add_trace(y = as.numeric(arc_sgr('PUC') / total_pay() * 100),name = 'PUC') %>%
      layout(
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