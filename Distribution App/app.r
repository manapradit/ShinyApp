# Distribution App

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
    
    # TAB 1 Normal distribution -----------------------------------------------------    
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
                 checkboxInput('dens1', label='Show the Density Curve', value=FALSE),
                 # Condition to display smoothness when selected the checkbox
                 conditionalPanel(
                   condition="input.dens1 % 2 == 1",
                   sliderInput('dens_smooth', label='Density Smoothness',
                               min=0.1, 5, value=0.5, step=.1))
               )),
               # Plot of simulated Sample
               column(8, wellPanel(
                 plotOutput('simulate1'),
                 tableOutput('random_summary1')
               ))
             ) 
    ),
    
    
    # TAB 2 binomial distribution -----------------------------------------------------
    tabPanel("Binomial",
             wellPanel(h2('The Binomial Density'),
                       'The effect of the number of trials and the probability of success'),
             fluidRow(
               # Input for normal density plot
               column(4, wellPanel(
                 sliderInput('binomsize', label='Number of trials', min=0, max=20, value=8, step=1),
                 sliderInput('binomprob', label='Probability of success on each trial', min=0, max=1, value=0.3, step=.01),
               )),
               # Plot of normal density
               column(8, wellPanel(
                 plotOutput('binomdist')
               )),
               column(4,   wellPanel(
                 # Random sample size
                 numericInput('n2', label='Sample Size', min=0, max=10000, value=50, step=1),
                 # Button to run the sample size
                 actionButton("do2", "Random"),
                 # Checkbox for displaying density
                 checkboxInput('dens2', label='Show the Density Curve', value=FALSE),
                 # Condition to display smoothness when selected the checkbox
                 conditionalPanel(
                   condition="input.dens2 % 2 == 1",
                   sliderInput('dens_smooth2', label='Density Smoothness',
                               min=0.1, 5, value=0.5, step=.1))
               )),
               # Plot of simulated Sample
               column(8, wellPanel(
                 plotOutput('simulate2'),
                 tableOutput('random_summary2')
               ))
             )             
    ),
    
    
    # TAB 3 Gamma distribution --------------------------------------------------------
    tabPanel("Gamma",
             wellPanel(h2('The Gamma Density'),
                       'The effect of the shape and scale'),
             fluidRow(
               # Input for normal density plot
               column(4, wellPanel(
                 sliderInput('gammashape', label='Shape', min=0, max=10, value=3, step=.1),
                 sliderInput('gammascale', label='Scale', min=0, max=10, value=2, step=.1),
               )),
               # Plot of normal density
               column(8, wellPanel(
                 plotOutput('gammadist')
               )),
               column(4,   wellPanel(
                 # Random sample size
                 numericInput('n3', label='Sample Size', min=0, max=10000, value=50, step=1),
                 # Button to run the sample size
                 actionButton("do3", "Random"),
                 # Checkbox for displaying density
                 checkboxInput('dens3', label='Show the Density Curve', value=FALSE),
                 # Condition to display smoothness when selected the checkbox
                 conditionalPanel(
                   condition="input.dens3 % 2 == 1",
                   sliderInput('dens_smooth3', label='Density Smoothness',
                               min=0.1, 5, value=0.5, step=.1))
               )
               ),
               # Plot of simulated Sample
               column(8, wellPanel(
                 plotOutput('simulate3'),
                 tableOutput('random_summary3')
               ))
             )
    )
  )  
)



##############################   Function  ##################################

# Function for type of the random sampling plot (Without input$do)
plot_random_type <- function(rtype, input1, input2, input3) {
  if (rtype == 'rnormal') {
    rnorm(n=input1, mean=input2, sd=input3) #mean=input$mu, sd=input$sigma)
  }
  else if (rtype == 'rbinomial'){
    rbinom(n=input1, size=input2, prob=input3) #size=input$binomsize, prob=input$binomprob
  }
  else if (rtype == 'rgamma'){
    rgamma(n=input1, shape=input2, scale=input3)
  }
}

# Function for summary table of the sample data
summary_table <- function(random_point){
  renderTable({
    data.frame(
      N = as.character(length(random_point())),
      Median = as.character(round(median(random_point()),2)),
      Mean = as.character(round(mean(random_point()),2)),
      SD = as.character(round(sd(random_point()),2)),
      Min = as.character(round(min(random_point()),2)),
      Max = as.character(round(max(random_point()),2)),
      Skweeness = as.character(round(skewness(random_point()),2)),
      Kurtosis = as.character(round(kurtosis(random_point()),2))
    )
  }, width = "100%")
}



###############################   Server  ####################################

server <- function(input, output) {
  # TAB 1 Output normal distribution -----------------------------------------------------
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
  
  
  # Add output of simulated sample plot and density
  output$simulate1 <- renderPlot({
    # Plot histogram of sample points
    hist(random_points(), freq=FALSE, ylim=c(0, .4), xlim=c(-10,20),
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    # Checkbox to show density curve overlayed on the histogram
    if(input$dens1 == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points(), bw=input$dens_smooth), col="red")
    }
  })
  
  # Output of summary
  output$random_summary1 <- summary_table(random_points)
  


  

  # TAB 2 Output binomial distribution -----------------------------------------------------
  output$binomdist <- renderPlot({
    x <- 0:input$binomsize
    y <- dbinom(x, size=input$binomsize, prob=input$binomprob)
    barplot(y,names.arg = x,main="Binomial Distribution",col="lightblue")
  })   
  
  
  # Output of random button
  random_points2 <- eventReactive(input$do2, {
    plot_random_type('rbinomial', input$n2, input$binomsize, input$binomprob)
  }, ignoreNULL = FALSE)
  
  
  output$simulate2 <- renderPlot({
    hist(random_points2(), freq=FALSE, 
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    # Checkbox to show density curve overlayed on the histogram
    if(input$dens2 == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points2(), bw=input$dens_smooth2), col="red")
    }
  })
  
  # Output of summary
  output$random_summary2 <- summary_table(random_points2)
  
  
  
  
  
  # TAB 3 Output gamma distribution ------------------------------------------------------
  output$gammadist <- renderPlot({
    x <- seq(0, 100, by = .1)
    y <- dgamma(x, shape=input$gammashape, scale=input$gammascale)

    plot(x,y, type='l', 
         ylab='Density', main='Plot of Gamma density')
  })   
  
  
  # Output of random button
  random_points3 <- eventReactive(input$do3, {
    plot_random_type('rgamma', input$n3, input$gammashape, input$gammascale)
  }, ignoreNULL = FALSE) 
  
  output$simulate3 <- renderPlot({
    hist(random_points3(), freq=FALSE, xlim=c(0,100),
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    
    if(input$dens3 == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points3(), bw=input$dens_smooth3), col="red")
    }
  })    

  # Output of summary
  output$random_summary3 <- summary_table(random_points3)
    
    

}

shinyApp(ui = ui, server = server, options=list(height=600))