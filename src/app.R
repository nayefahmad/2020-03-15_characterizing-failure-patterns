
library(shiny)
library(survival)
library(tidyverse)


ui <- fluidPage(    
    
    # Give the page a title
    titlePanel("na"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            
            # input1: sample size  
            sliderInput(inputId = "sample_size",
                        label = "Sample size",
                        min = 100,
                        max = 5000,
                        value = 1000, 
                        step = 100),
            
            # Input2: Weibull shape 
            sliderInput(inputId = "weibull_shape",
                        label = "Weibull shape parameter",
                        min = 0.1,
                        max = 10,
                        value = 1, 
                        step = 0.2),
        ),
        
        
        # Main panel: 
        mainPanel(
            plotOutput("plot1")  
        ),
        
       
        
    )
)


# Define a server for the Shiny app
server <- function(input, output) {
    
    # This expression that generates a plot is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    
    output$plot1 <- renderPlot({
        
        # calculations: 
        v1_sample <- rweibull(input$sample_size, 
                              shape = input$weibull_shape, 
                              scale = 1)
        
        v2_is_censored <- sample(c(0, 1),
                                 size = input$sample_size,
                                 replace = TRUE,
                                 prob = c(0, 1 - 0))
        
        df1_input_data <- data.frame(time = v1_sample, 
                                     is_censored = v2_is_censored)
        
        
        # Fitting and plotting K-M curve 
        km1 <- survfit(Surv(time, is_censored) ~ 1, 
                       data = df1_input_data)
        
        km1_summary <- summary(km1, times = seq(0, 5, param4_step_size)) 
        
        df2_surv_summary <- 
            tibble(time = km1_summary$time, 
                   surv = km1_summary$surv, 
                   cumhaz = km1_summary$cumhaz) %>% 
            mutate(log_surv = log(surv),
                   finite_diff_approx_of_hazard = -1 * ((log_surv - lag(log_surv))/param4_step_size))
        
        df2_surv_summary %>% 
            ggplot(aes(x = time,
                       y = finite_diff_approx_of_hazard)) + 
            geom_point(alpha = .2) + 
            geom_smooth() + 
            geom_hline(yintercept = 1, 
                       col = "red") + 
            scale_y_continuous(limits = c(0, 10))
        
        # autoplot(km1)

            
    })
}




shinyApp(ui = ui, server = server)