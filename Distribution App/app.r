library(shiny)


ui <- fluidPage(
  
  # Application title
  titlePanel("Plot of Distributions"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput("distribution", "Distribution Type:",
                  list("Continue" = 
                         list( "Normal" = "normal", 
                               "Beta" = "beta",
                               "Gamma" = "gamma",
                               "Exponential" = "exp",
                               "F Distribution" = "f",
                               "Chisquare" = "chisq",
                               "T Distribution" = "t",
                               "Uniform" = "unif"),
                       
                       "discrete" = 
                         list("Binomial" = "binomial", 
                              "Geometric" = "geom", 
                              "Poisson" = "pois")
                  )
      ),  
      

            
      
      conditionalPanel(
        condition = "input.distribution == 'beta'",
        sliderInput("shape1", "Shape 1: ", min=0.1, max=10, value=5, step=0.05),
        sliderInput("shape2", "Shape 2: ", min=0.1, max=10, value=5, step=0.05)
      ),
      conditionalPanel(
        condition = "input.distribution == 'binomial'",
        sliderInput("size", "Number of trials: ", min=0, max=20, value=15, step=1),
        sliderInput("prob", "Probability of success on each trial: ", min=0, max=1, value=0.3, step=0.01)
      ),      
      conditionalPanel(
        condition = "input.distribution == 'chisq'",
        sliderInput("df", "Degrees of freedom: ", min=0.1, max=5, value = 2, step = 0.05)
      ),
      conditionalPanel(
        condition = "input.distribution == 'exp'",
        sliderInput("rate", "Rate: ", min=0.1, max=5, value=1, step=0.05)
      ),
      conditionalPanel(
        condition = "input.distribution == 'f'",
        sliderInput("df1_f", "Degrees of freedom 1: ", min=0.1, max=10, value=5, step=0.1),
        sliderInput("df2_f", "Degrees of freedom 2: ", min=0.1, max=10, value=5, step=0.1)
      ),
      conditionalPanel(
        condition = "input.distribution == 'gamma'",
        sliderInput("shape", "Shape: ", min=0.1, max=5, value=3, step=0.05),
        sliderInput("scale", "Scale: ", min=0.1, max=5, value=2, step=0.05)
      ),
      conditionalPanel(
        condition = "input.distribution == 'geom'",
        sliderInput("prob_geom", "Probability of success on each trial: ", min=0, max=1, value=0.3, step=0.01)
      ), 
      conditionalPanel(
        condition = "input.distribution == 'normal'",
        sliderInput("mean", "Mean: ", min=-10, max=10, value=3, step = 0.1),
        sliderInput("sd", "Standard Deviation: ", min=1, max=4, value=2, step=0.1)
      ),
      conditionalPanel(
        condition = "input.distribution == 'pois'",
        sliderInput("lambda", "Lambda: ", min=0, max=10, value=3, step = 0.1),
      ),
      conditionalPanel(
        condition = "input.distribution == 't'",
        sliderInput("df_t", "Degrees of freedom: ", min=0.1, max=20, value = 5, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.distribution == 'unif'",
        sliderInput("range", "Lower and Upper limits of the distribution (Min and Max): ", min=-10, max=10, value=c(0,5), step=0.5)
      ),
      
      

            
      # Axes setting
      selectInput("axes", "Fixed X-axis and Y-axis Options:",
                  list("Default" = "default", 
                       "Adjust Value" = "adjust",
                       "Best fit" = "best"
                       )
                                    
      ),
      conditionalPanel(
        condition = "input.axes == 'adjust'",
        numericInput("minx", "Minimum X-axis", value=-1, step=0.05),
        numericInput("maxx", "Maximum X-axis", value=10, step=0.05), 
        numericInput("maxy", "Maximum Y-axis", value=1, step=0.05)  
        
      )      
      
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("density")
    )
  )
)





server <- function(input, output, session) {
  
  xrange <- {reactive({
    
    if(input$distribution == 'beta'){
      min <- qbeta(0.001, shape1=input$shape1, shape2=input$shape2)
      max <- qbeta(0.999, shape1=input$shape1, shape2=input$shape2)
      return(
        seq(from=min(min), to=max(max), length=200)
      )
    } else if (input$distribution == 'binomial'){
      return(
        seq(from=0, to=input$size, by=1)
      )
    } else if (input$distribution == 'chisq'){
      min <- qchisq(0.001, df=input$df)
      max <- qchisq(0.999, df=input$df)
      return(
        seq(from=min, to=max, length=200)
      )
    } else if (input$distribution == 'exp'){
      min <- qexp(0.001, rate=input$rate)
      max <- qexp(0.999, rate=input$rate)
      return(
        seq(from=min, to=max, length=200)
      )
    } else if (input$distribution == 'f'){
      min <- qf(0.001, input$df1_f, input$df2_f)
      max <- qf(0.999, input$df1_f, input$df2_f)
      return(
        seq(from=min, to=max, length=200)
      )
    } else if (input$distribution == 'gamma'){
      min <- qgamma(0.001, shape=input$shape, scale=input$scale)
      max <- qgamma(0.999, shape=input$shape, scale=input$scale)
      return(
        seq(from=min, to=max, length=200)
      )
    } else if(input$distribution == 'geom'){
      min <- qgeom(0.001, input$prob_geom)
      max <- qgeom(0.999, input$prob_geom)
      return(
        round(seq(from=min, to=max, length=200))
      )
    } else if (input$distribution == 'normal'){
      min <- qnorm(0.001, input$mean, input$sd)
      max <- qnorm(0.999, input$mean, input$sd)
      return(
        seq(from=min, to=max, length=200)
      )    
    } else if (input$distribution == 'pois'){
      min <- qpois(0.001, input$lambda)
      max <- qpois(0.999, input$lambda)
      return(
        round(seq(from=min, to=max, length=200))
      )
    } else if (input$distribution == 't'){
      min <- qt(0.001, df=input$df_t)
      max <- qt(0.999, df=input$df_t)
      return(
        seq(from=min, to=max, length=200)
      )
    } else if (input$distribution == 'unif'){
      min <- qunif(0.001, min=input$range[1], max=input$range[2])
      max <- qunif(0.999, min=input$range[1], max=input$range[2])
      return(
        seq(from=min-1, to=max+1, length=200)
      )
    }
  }
  )}
  
  
  y <- reactive({
    
    if (input$distribution == 'beta'){
      return(
        dbeta(xrange(), shape1=input$shape1, shape2=input$shape2)
      )
    } else if (input$distribution == 'binomial'){
      return(
        dbinom(xrange(), p=input$prob, size=input$size)
      )
    } else if (input$distribution == 'chisq'){
      return(
        dchisq(xrange(), df=input$df)
      )      
    } else if (input$distribution == 'exp'){
      return(
        dexp(xrange(), rate=input$rate)
      )   
    } else if (input$distribution == 'f'){
      return(
        df(xrange(), input$df1_f, input$df2_f)
      )
    } else if (input$distribution == 'gamma'){
      return(
        dgamma(xrange(), shape=input$shape, scale=input$scale)
      )
    } else if(input$distribution == 'geom'){
      return(
        dgeom(xrange(), prob=input$prob_geom) 
      )
    } else if (input$distribution == 'normal'){
      return(
        dnorm(xrange(), mean=input$mean, sd=input$sd)
      )
    } else if (input$distribution == 'pois'){
      return(
        dpois(xrange(), lambda=input$lambda)
      )
    } else if (input$distribution == 't'){
      return(
        dt(xrange(), df=input$df_t)
      )      
    } else if (input$distribution == 'unif'){
      return(
        dunif(xrange(), min=input$range[1], max=input$range[2])
      )      
    }
  })
  
  
  
  
  
# Axes setting  
  observeEvent( input$axes== 'adjust', {
    updateNumericInput(session, "minx", value=xl()[1])
    updateNumericInput(session, "maxx", value=xl()[2])
    updateNumericInput(session, "maxy", value=yl()[2])
    
    
    # if (input$distribution == 'binomial'|
    #     input$distribution == 'pois') {
    #   updateNumericInput(session, "minx", value=0)
    #   updateNumericInput(session, "maxx", value=25)
    #   updateNumericInput(session, "maxy", value=1)
    # } else if (input$distribution == 'geom'){
    #   updateNumericInput(session, "minx", value=0)
    #   updateNumericInput(session, "maxx", value=5)
    #   updateNumericInput(session, "maxy", value=1)
    # } else if (input$distribution == 'beta'){
    #   updateNumericInput(session, "minx", value=0)
    #   updateNumericInput(session, "maxx", value=1)
    #   updateNumericInput(session, "maxy", value=12)
    # } else if (input$distribution == 'chisq'){
    #   updateNumericInput(session, "minx", value=0)
    #   updateNumericInput(session, "maxx", value=20)
    #   updateNumericInput(session, "maxy", value=2)
    # } else if (input$distribution == 'exp'){
    #   updateNumericInput(session, "minx", value=0)
    #   updateNumericInput(session, "maxx", value=10)
    #   updateNumericInput(session, "maxy", value=6)
    # } else if (input$distribution == 'f'){
    #   updateNumericInput(session, "minx", value=-0.5)
    #   updateNumericInput(session, "maxx", value=10)
    #   updateNumericInput(session, "maxy", value=1)
    # } else if (input$distribution == 'gamma'){
    #   updateNumericInput(session, "minx", value=-5)
    #   updateNumericInput(session, "maxx", value=50)
    #   updateNumericInput(session, "maxy", value=1)
    # } else if (input$distribution == 'normal'){
    #   updateNumericInput(session, "minx", value=-16)
    #   updateNumericInput(session, "maxx", value=16)
    #   updateNumericInput(session, "maxy", value=0.5)
    # } else if (input$distribution == 't'){
    #   updateNumericInput(session, "minx", value=-16)
    #   updateNumericInput(session, "maxx", value=16)
    #   updateNumericInput(session, "maxy", value=0.5)
    # } else if (input$distribution == 'unif'){
    #   updateNumericInput(session, "minx", value=-10)
    #   updateNumericInput(session, "maxx", value=10)
    #   updateNumericInput(session, "maxy", value=1)
    # }
    
    
  })
  

  

  yl <- reactive({
    
    if (input$axes == 'default'){
      if (input$distribution == 'binomial'| 
          input$distribution == 'pois'|
          input$distribution == 'geom'|
          input$distribution == 'f'|
          input$distribution == 'gamma'|
          input$distribution == 'unif'){
        return(ylim=c(0,1))
      } else if (input$distribution == 'beta'){
        return(ylim=c(0,12))
      } else if (input$distribution == 'chisq'){
        return(ylim=c(0,2))
      } else if (input$distribution == 'exp'){
        return(ylim=c(0,6))
      } else if (input$distribution == 'normal'){
        return(ylim=c(0,0.5))
      } else if (input$distribution == 't'){
        return(ylim=c(0,0.5))
      }
    } else if (input$axes == 'adjust'){
      return(ylim=c(0,input$maxy))
    } else {
    }
  })
  
  
  
  
  
  xl <- reactive({
    
    if (input$axes == 'default'){
      if (input$distribution == 'binomial'| 
          input$distribution == 'pois') {  #
        return(xlim=c(0,25))
      } else if (input$distribution == 'geom'){
        return(xlim=c(0,5))
      } else if (input$distribution == 'beta'){
        return(xlim=c(0,1))
      } else if (input$distribution == 'chisq'){
        return(xlim=c(0,20))
      } else if (input$distribution == 'exp'){
        return(xlim=c(0,10))
      } else if (input$distribution == 'f'){
        return(xlim=c(-0.5,10))
      } else if (input$distribution == 'gamma'){
        return(xlim=c(-5,50))
      } else if (input$distribution == 'normal'){
        return(xlim=c(-16,16))
      } else if (input$distribution == 't'){
        return(xlim=c(-10,10))
      } else if (input$distribution == 'unif'){
        return(xlim=c(-10,10))
      }
      
    } else if (input$axes == 'adjust'){


      return(xlim=c(input$minx,input$maxx))     
      
    } else {
    }
  })
  

  
  
  
  
  output$density <- renderPlot({
    
    if(input$distribution == 'binomial' |
       input$distribution == 'pois' ){
      plot(xrange(), y(), xlim = xl(),  ylim=yl(), type="h", 
           ylab="Probability", xlab="x", lwd = 5, col="Sky Blue")
      
    } else if (input$distribution == 'geom'){
      plot(xrange(), y(), xlim = xl(),  ylim=yl(), type="h", 
           ylab="Probability", xlab="x",  lwd = 5, col="Sky Blue", xaxt='n') 
      axis(side=1, at=c(0,1,2,3,4,5))
      
    } else{
      plot(xrange(), y(), xlim = xl(), ylim=yl(), type="l", 
           ylab="Density", xlab="x", lwd = 2, col="Sky Blue")
    }
  })
  
}

shinyApp(ui = ui, server = server)
