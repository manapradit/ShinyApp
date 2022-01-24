# Distribution App
# I added tabs for binomial and gamma distributions.
# But the random sample's button is still linked to the normal sampling (first tab). 
# So, there is no change when you click on the random button in the binomial and gamma tabs.

library(shiny)
library(moments)

# Function for the random sampling tab
random_tab <- function(){
  wellPanel(
    # Random sample size
    numericInput('n', label='Sample Size', min=0, max=10000, value=50, step=1),
    # Button to run the sample size
    actionButton("do", "Random"),
    # Checkbox for displaying density
    checkboxInput('dens', label='Show the Density Curve', value=FALSE),
    # Condition to display smoothness when selected the checkbox
    conditionalPanel(
      condition="input.dens % 2 == 1",
      sliderInput('dens_smooth', label='Density Smoothness',
                  min=0.1, 5, value=0.5, step=.1))
  )
}


###############################    UI    ####################################
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Normal",
             wellPanel(h2('The Normal Density'),
                       'The effect of the mean and the standard deviation'),
             fluidRow(
               # Input for normal density plot
               column(4, wellPanel(
                 sliderInput('mu', label='Mean', min=0, max=10, value=3, step=.1),
                 sliderInput('sigma', label='Standard Deviation', min=1, 4, value=2, step=.1),
                 checkboxInput('emp', label='Show the Empirical Rule', value=TRUE)
               )),
               # Plot of normal density
               column(8, wellPanel(
                 plotOutput('normaldist')
               )),
               # Input for sampling
               column(4,   wellPanel(
                 # Random sample size
                 numericInput('n', label='Sample Size', min=0, max=10000, value=50, step=1),
                 # Button to run the sample size
                 actionButton("do1", "Random"),
                 # Checkbox for displaying density
                 checkboxInput('dens', label='Show the Density Curve', value=FALSE),
                 # Condition to display smoothness when selected the checkbox
                 conditionalPanel(
                   condition="input.dens % 2 == 1",
                   sliderInput('dens_smooth', label='Density Smoothness',
                               min=0.1, 5, value=0.5, step=.1))
               )),
               # Plot of simulated Sample
               column(8, wellPanel(
                 plotOutput('simulate1'),
                 tableOutput('random_summary')
               ))
             ) 
    ),
    
    
    tabPanel("Binomial",
             wellPanel(h2('The Binomial Density'),
                       'The effect of the number of trials and the probability of success'),
             fluidRow(
               # Input for normal density plot
               column(4, wellPanel(
                 sliderInput('binomx', label='Vector of quantiles', min=-10, max=20, value=c(-5,10), step=1),
                 sliderInput('binomsize', label='Number of trials', min=0, max=10, value=8, step=1),
                 sliderInput('binomprob', label='Probability of success on each trial', min=0, max=1, value=0.5, step=.01),
               )),
               # Plot of normal density
               column(8, wellPanel(
                 plotOutput('binomdist')
               )),
               column(4,   wellPanel(
                 # Random sample size
                 numericInput('n', label='Sample Size', min=0, max=10000, value=50, step=1),
                 # Button to run the sample size
                 actionButton("do2", "Random"),
                 # Checkbox for displaying density
                 checkboxInput('dens', label='Show the Density Curve', value=FALSE),
                 # Condition to display smoothness when selected the checkbox
                 conditionalPanel(
                   condition="input.dens % 2 == 1",
                   sliderInput('dens_smooth', label='Density Smoothness',
                               min=0.1, 5, value=0.5, step=.1))
               )),
               # Plot of simulated Sample
               column(8, wellPanel(
                 plotOutput('simulate2'),
                 #tableOutput('random_summary')
               ))
             )             
    ),
    
    
    tabPanel("Gamma",
             wellPanel(h2('The Gamma Density'),
                       'The effect of the number of trials and the probability of success'),
             fluidRow(
               # Input for normal density plot
               column(4, wellPanel(
                 sliderInput('gammashape', label='Shape', min=0, max=10, value=6, step=0.1),
                 sliderInput('gammascale', label='Scale', min=0, max=10, value=1, step=.1),
               )),
               # Plot of normal density
               column(8, wellPanel(
                 plotOutput('gammadist')
               )),
               column(4,   wellPanel(
                 # Random sample size
                 numericInput('n', label='Sample Size', min=0, max=10000, value=50, step=1),
                 # Button to run the sample size
                 actionButton("do3", "Random"),
                 # Checkbox for displaying density
                 checkboxInput('dens', label='Show the Density Curve', value=FALSE),
                 # Condition to display smoothness when selected the checkbox
                 conditionalPanel(
                   condition="input.dens % 2 == 1",
                   sliderInput('dens_smooth', label='Density Smoothness',
                               min=0.1, 5, value=0.5, step=.1))
               )
               ),
               # Plot of simulated Sample
               column(8, wellPanel(
                 plotOutput('simulate3'),
                 #tableOutput('random_summary')
               ))
             )
    )
  )  
)



##############################   Function  ##################################

# Function for type of the random sampling plot (Without input$do)
plot_random_type <-function(rtype, input1, input2, input3) {
  if (rtype == 'rnormal') {
    rnorm(n=input1, mean=input2, sd=input3) #mean=input$mu, sd=input$sigma)
  }
  else if (rtype == 'rbinomial'){
    rbinom(n=input1, size=input2, prob=input3) #size=input$binomsize, prob=input$binomprob
  }
  else if (rtype == 'rgamma'){
    rgamma(n=input1, shape=input2, rate=input3)
  }
}




# Function v.2 for type of the random sampling plot (With input$do)
plot_random_type2 <-function(rtype, input0, input1, input2, input3) {
  eventReactive(input0, {
    if (rtype == 'rnormal') {
      rnorm(n=input1, mean=input2, sd=input3) #mean=input$mu, sd=input$sigma)
    }
    else if (rtype == 'rbinomial'){
      rbinom(n=input1, size=input2, prob=input3) #size=input$binomsize, prob=input$binomprob
    }
    else if (rtype == 'rgamma'){
      rgamma(n=input1, shape=input2, rate=input3)
    }    
  }, ignoreNULL = FALSE)}



###############################   Server  ####################################

server <- function(input, output) {
  # TAB 1 Output normal distribution
  output$normaldist <- renderPlot({
    x <- seq(from=-12, to=22, by=.1)
    y <- dnorm(x, mean=input$mu, sd=input$sigma)
    plot(x,y, type='l', ylim=c(0, .4),
         ylab='Density', main='Plot of normal density')
    # Checkbox to show Empirical Rule
    if(input$emp == TRUE){
      segments(x0=input$mu+c(-1,1)*input$sigma, y0=0,
               y1=dnorm(input$mu+c(-1,1)*input$sigma, mean=input$mu, sd=input$sigma),
               col='red')
      segments(x0=input$mu+c(-2,2)*input$sigma, y0=0,
               y1=dnorm(input$mu+c(-2,2)*input$sigma, mean=input$mu, sd=input$sigma),
               col='green')
      segments(x0=input$mu+c(-3,3)*input$sigma, y0=0,
               y1=dnorm(input$mu+c(-3,3)*input$sigma, mean=input$mu, sd=input$sigma),
               col='blue')
      legend('topright', title='# sd\'s (%)',
             legend=c('1  (68%)', '2  (95%)', '3  (99.7%)'), lty=1, col=c('red', 'green', 'blue'))
    }
  })
  
  
  # Output of random button
  random_points <- eventReactive(input$do1 , {
    plot_random_type('rnormal', input$n, input$mu, input$sigma)
  }, ignoreNULL = FALSE)
  
  
  
  #random_points <- plot_random_type2('rnormal', input$do, input$n, input$mu, input$sigma)
  
  
  
  # Add output of simulated sample plot and density
  output$simulate1 <- renderPlot({
    #par(oma = c(4, 0, 0, 0)) # to add margin under plot
    # Plot histogram of sample points
    hist(random_points(), freq=FALSE, ylim=c(0, .4), xlim=c(-10,20),
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    #plot_text=paste("Mean = ", round(mean(x),3), ", SD = ", round(sd(x),3)) #To add mean=, sd= under plot
    #mtext(text=plot_text, side=1, line=6, outer = FALSE) #To add mean=, sd= under plot
    
    # Checkbox to show density curve overlayed on the histogram
    if(input$dens == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points(), bw=input$dens_smooth), col="red")
    }
  })
  
  # Output of summary
  output$random_summary <- renderTable({
    data.frame(
      #      Name = c("Mean", "SD"),
      #      Value = as.character(c(round(mean(random_points()),2), 
      #                             round(sd(random_points()),2)
      #                           ))
      N = as.character(input$n),
      Median = as.character(round(median(random_points()),2)),
      Mean = as.character(round(mean(random_points()),2)),
      SD = as.character(round(sd(random_points()),2)),
      Min = as.character(round(min(random_points()),2)),
      Max = as.character(round(max(random_points()),2)),
      Skweeness = as.character(round(skewness(random_points()),2)),
      Kurtosis = as.character(round(kurtosis(random_points()),2))
    )
  }, width = "100%")
  
  
  
  
  
  # TAB 2 Output binomial distribution
  output$binomdist <- renderPlot({
    y <- dbinom(x=input$binomx[1]:input$binomx[2], size=input$binomsize, prob=input$binomprob)
    plot(y, type='l', ylim=c(0, 1), xlim = c(-10,30),
         ylab='Density', main='Plot of Binomial density')
  })   
  
  
  # Output of random button
  random_points2 <- eventReactive(input$do2, {
    plot_random_type('rbinomial', input$n, input$binomsize, input$binomprob)
  }, ignoreNULL = FALSE)
  
  
  
  #random_points2 <- plot_random_type2('rbinomial', input$do, input$n, input$binomsize, input$binomprob)
  
  
  
  output$simulate2 <- renderPlot({
    hist(random_points2(), freq=FALSE, #ylim=c(0, .4), xlim=c(-10,20),
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    
    # Checkbox to show density curve overlayed on the histogram
    if(input$dens == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points(), bw=input$dens_smooth), col="red")
    }
  })
  
  
  
  
  
  # TAB 3 Output gamma distribution
  output$gammadist <- renderPlot({
    x <- seq(0, 100, by = 0.1)
    y <- dgamma(x, shape=input$gammashape, scale=input$gammascale)

    plot(x,y, type='l', #ylim=c(0, 5), xlim = c(-10,50),
         ylab='Density', main='Plot of Gamma density')
  })   
  
  
  # Output of random button
  random_points3 <- eventReactive(input$do3, {
    plot_random_type('rgamma', input$n, input$gammashape, input$gammarate)
  }, ignoreNULL = FALSE) 
  
  output$simulate3 <- renderPlot({
    hist(random_points3(), freq=FALSE, #ylim=c(0, .4), xlim=c(-10,20),
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    
    # Checkbox to show density curve overlayed on the histogram
    if(input$dens == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points(), bw=input$dens_smooth), col="red")
    }
  })
}

shinyApp(ui = ui, server = server, options=list(height=600))