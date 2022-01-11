# Distribution App
library(shiny)
library(moments)
ui <- fluidPage(
  wellPanel(h2('The Normal Density'),
            'The effect of the mean and the standard deviation'),
  fluidRow(
    column(4, wellPanel(
      sliderInput('mu', label='Mean', min=0, max=10, value=3, step=.1),
      sliderInput('sigma', label='Standard Deviation', min=1, 4, value=2, step=.1),
      checkboxInput('emp', label='Show the Empirical Rule', value=TRUE)
    )),
    column(8, wellPanel(
      plotOutput('normaldist')
    )),
    column(4, wellPanel(
      # random sample
      numericInput('n', label='Sample Size', min=0, max=10000, value=50, step=1),
      # buttton to run the sample size
      actionButton("do", "Random"),
      # checkbox for displaying density
      checkboxInput('dens', label='Show the Density Curve', value=FALSE),
      # Condition to display smoothness when selected the checkbox
      conditionalPanel(
        condition="input.dens % 2 == 1",
        sliderInput('dens_smooth', label='Density Smoothness',
                    min=0.1, 5, value=0.5, step=.1))
    )),
    column(8, wellPanel(
      plotOutput('simulate'),
      tableOutput('random_summary')
    ))
  )
)


server <- function(input, output) {
  # Output normal distribution
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
  random_points <- eventReactive(input$do, {
    rnorm(n=input$n, mean=input$mu, sd=input$sigma)
  }, ignoreNULL = FALSE
  ) 
  # Add output of simulated sample plot and density
  output$simulate <- renderPlot({
    # Plot histogram of sample points
    hist(random_points(), freq = FALSE, ylim=c(0, .4), xlim=c(-10,20),
         ylab='Density', xlab='Random Samples', main='Plot of simulated sample')
    # Checkbox to show density curve overlayed on the histogram
    if(input$dens == TRUE){
      # Plot density curve overlayed on the histogram
      lines(density(random_points(), bw=input$dens_smooth), col="red")
    }
  })
  
  # Output of summary
  output$random_summary <- renderTable({
    data.frame(
      N = as.character(length(random_points())),
      Median = as.character(round(median(random_points()),2)),
      Mean = as.character(round(mean(random_points()),2)),
      SD = as.character(round(sd(random_points()),2)),
      Min = as.character(round(min(random_points()),2)),
      Max = as.character(round(max(random_points()),2)),
      Skweeness = as.character(round(moments::skewness(random_points()),2)),
      Kurtosis = as.character(round(moments::kurtosis(random_points()),2))
    )
    
  }, width = "100%"
  )
}

shinyApp(ui = ui, server = server, options=list(height=600))