
shinyServer(function(input, output, session) {
  

    #my4 <-  input$newcomp
    #my5<-input$newcomp+1
    # my6 <- reactiveVal()
    # my6 <- str_c(c(input$newcomp,input$newcomp+1), collapse = "-")
    # #plot_save_region(data = dat1, region = input$region, variable = input$indexxx)
    # mydat = dat1[dat1$REGION == input$region, ]
    # 
    # stns = reactiveVal()
    # stns = sort(unique(mydat$STN))
    # max_y = ifelse(!all(is.na(mydat[, input$indexxx])), max(mydat[, input$indexxx], na.rm = T), 0.1)
    # 
    # y_limit = reactiveVal()
    # y_limit = c(0, max_y)
    # 
    # variable = reactiveVal()
    # variable = input$indexxx
  

    
  my_var <- function(){
    variable = input$indexxx
    return(variable)
  }
  
  my_stn <- function(i) {
    mydat = dat1[dat1$REGION == input$region, ]
    stns = sort(unique(mydat$STN))
    return(stns[i])
  }
    
  my_lim <- function() {
    mydat = dat1[dat1$REGION == input$region, ]
    max_y = ifelse(!all(is.na(mydat[, input$indexxx])), max(mydat[, input$indexxx], na.rm = T), 0.1)
    y_limit = c(0, max_y)
    return(y_limit)
    
  }
  my_comp <- function() {
    my6 <- str_c(c(input$newcomp,input$newcomp+1), collapse = "-")
    return(my6)
    
  }
  
  
    # mydat1 = dat1[dat1$REGION == input$region, ]
    # stns1 = sort(unique(mydat$STN))
    # 
    # len <- length(stns1)
    
  
  # my_len <- function() {
  #   mydat = dat1[dat1$REGION == "Auckland", ]
  #   stns = sort(unique(mydat$STN))
  #   
  #   len <- length(stns)
  #   return(len)
  # }
  # 
  # ki = my_len()


  output$plots <- renderUI({

    plot_output_list <- lapply(1:8, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname)
      #, height = 580, width = 550
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  for (i in 1:8) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      
      output[[plotname]] <- renderPlot({
        print(my_i)
        plot_station(dat1, station = my_stn(my_i), variable = my_var(), comp_years = my_comp(), y_lim = my_lim())
        # plot(1:my_i, 1:my_i,
        #      xlim = c(1, max_plots),
        #      ylim = c(1, max_plots),
        #      main = paste("1:", my_i, ".  n is ", input$n, sep = "")
        
        #)
      })
    })
  }
  
  output$Assign1 <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(dat01), style = "bootstrap")
  })
  
  output$SummaryA1 <- renderPrint({
    glimpse(dat01)
  })
  
  output$SummaryA2 <- renderPrint({
    summary(dat01)
  })

  
  output$Mosaic <- renderPlot({
  
      
      my1 <-  input$comp 
      my2<-input$comp+1
      my3 <- str_c(c(my1,my2), collapse = "-")
      
      plot_station(dat1, station = input$station, variable = input$indexx, comp_years = my3)
      #print(my3)
    })
    
    
    
   
    #have to change comp_years to be given by user. but how to increment 1 year . if i give input$comp - input$comp+1 it doesnt throw error but doesnt plot that line either. black and red line is not plotted. y?
   #plot_station(dat1, station = input$station, variable = input$indexx, comp_years = "input$comp-input$comp+1")
    #plot_station(dat1, station = input$station, variable = input$indexx, comp_years = "2008-2009")
    
#})
  
  # lapply(input$region, function(Mosaic1) {
  #   output[[Mosaic1]] <- renderPlot({
  #     plot_save_region(data = dat1, region = input$region, variable = input$indexxx)
  #   })
  # })
  
  
  # plot_save_region = function(data, region, variable) {
  #   
  #   mydat = data[data$REGION == region, ]
  #   stns = sort(unique(mydat$STN))
  #   comp.years="2007-2008"
  #   max_y = ifelse(!all(is.na(mydat[, variable])), max(mydat[, variable], na.rm = T), 0.1)
  #   y_limit = c(0, max_y)
  #   
  #   for (i in seq_along(stns)) {
  #     
  #     station_plot = plot_station(dat1, station = stns[i], variable = variable,
  #                                 comp_years = comp.years, y_lim = y_limit)
  #     return(station_plot)
  # 
  #   }
  #   graphics.off()
  #   
  # }
  # 
  
  ##output$Mosaic1 <- renderPlot({
    
    # 
    # plot_save_region <- function(data, region, variable) {
    #   
    #   mydat = data[data$REGION == region, ]
    #   stns = sort(unique(mydat$STN))
    #   #comp.years= input$newcomp
    #   max_y = ifelse(!all(is.na(mydat[, variable])), max(mydat[, variable], na.rm = T), 0.1)
    #   y_limit = c(0, max_y)
    #   
    #   return(stns, y_limit)
    #   
    # 
    #   for (i in seq_along(stns)) {
    #     
    #     station_plot = plot_station(dat1, station = stns[i], variable = variable, comp_years = my6, y_lim = y_limit)
    #     
    #     return(station_plot) 
    #     
    #   }
    #   #graphics.off()
    #   
    # }
    
    
    # my4 <-  input$newcomp
    # my5<-input$newcomp+1
    # my6 <- str_c(c(my4,my5), collapse = "-")
    # #plot_save_region(data = dat1, region = input$region, variable = input$indexxx)
    # mydat = dat1[dat1$REGION == region, ]
    # stns = sort(unique(mydat$STN))
    # #comp.years= input$newcomp
    # max_y = ifelse(!all(is.na(mydat[, input$indexxx])), max(mydat[, input$indexxx], na.rm = T), 0.1)
    # y_limit = c(0, max_y)


    
  })
  

#})

