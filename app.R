library(shiny)
library(ggplot2)
library(dplyr)
library(stats)

ui <- fluidPage(
    titlePanel("Visualizing Time Series Models"),
    h4("A Simulation App"),
    sidebarLayout(
        sidebarPanel(
            h4("Inputs"),
            textInput("variable_label", "What is the variable that you are plotting?", 
                      value = "GDP"),
            numericInput("time",
                         "Select the number of time periods",
                         value = 100),
            radioButtons("radio", "Select the process type",
                         choices = list("White-noise" = 1,
                                        "Random-walk with drift" = 2,
                                        "AR ((1))" = 3,
                                        "MA ((1))" = 4, 
                                        "Any ARIMA process of the form (ARIMA) ((p,d,q))" = 5), 
                         selected = 1), 
            numericInput("min", "Minimum value displayed on the graph", value = -10),
            numericInput("max", "Maximum value displayed on the graph", value = 10),
            textInput("line_color", "Line color:", value = "black"),
            conditionalPanel("input.radio == 1", 
                             numericInput("mean_1", "Population mean", value = 0),
                             numericInput("std_1", "Population standard deviation", value = 1)
            ),
            conditionalPanel("input.radio == 2", 
                             numericInput("drift", "Drift:", value = 0.05),
                             numericInput("first_value", "Starting value:", value = 0),
                             numericInput("std_2", "Population standard deviation:", value = 0.1)
            ),
            conditionalPanel("input.radio == 3", 
                             numericInput("drift_2", "Drift:", value = 0.1),
                             numericInput("phi", "Phi:", value = 0.99),
                             numericInput("ar_start", "Starting value:", value = 0),
                             numericInput("std_3", "Population standard deviation:", value = 0.5)
            ),
            conditionalPanel("input.radio == 4", 
                             numericInput("mu", "Mu:", value = 0),
                             numericInput("theta", "Theta:", value = 0.8),
                             numericInput("std_4", "Population standard deviation:", value = 1)
            ),
            conditionalPanel("input.radio == 5", 
                             numericInput("p", "P parameter", min = 0, value = 2),
                             numericInput("d", "D parameter", min = 0, value = 0),
                             numericInput("q", "Q parameter", min = 0, value = 2), 
                             textInput("MA_coefficents", "MA coefficents", value = "0.4, 0.2"),
                             textInput("AR_coefficents", "AR coefficents", value = "0.4, 0.1"),
            )
            
        ),
        mainPanel(plotOutput("process"),
                  conditionalPanel("input.radio == 1", 
                                   h4("The mathematical definition of a white-noise process"),
                                   withMathJax(),
                                   tags$div(HTML("<script type='text/x-mathjax-config' > 
                                   MathJax.Hub.Config({
                                   tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                                   });
                                   </script >
                                                 ")),
                                   helpText('A white-noise process follows the equation: $ y_t = \\mu  + \\varepsilon_t$, where: $y$ ~ $ N (\\mu, \\sigma ^ 2 ) $ and 
                                   also $\\varepsilon$ ~ $ N (0, \\sigma ^ 2 ) $. The expectation of the outcome variable is time-invariant and equal to $E[y_t] = \\mu$. '),
                                   h4("Intuition"), 
                                   "An outcome of a white-noise process at a particular point in time is independent from other observations. It is not time-dependent and represents a process, that is random in each period. An example of this would be a filp of a coin. Even though we expect the coin to follow a certain process every time, when we flip the coin four times, the fourth flip does not depend on the outcome of the three flips before."
                  ),
                  conditionalPanel("input.radio == 2", 
                                   h4("Definition:"),
                                   withMathJax(),
                                   helpText('A random-walk with drift follows the equation: $ y_t = \\alpha + y_{t-1}  + \\varepsilon_t$. If there is no drift, then $\\alpha = 0$. As usual, $\\varepsilon$ ~ $ N (0, \\sigma ^ 2 ) $ '),
                                   h4("Intuition"), 
                                   "Random-walk with drift processes illustrate situations, where we know that the process is moving in a certain direction, but we don't know what the shock is going to be. An example of this would be the value of stocks over time. Due to inflation and economic growth, we can be nearly certain economic growth will expand. On the other hand, at any particular year, the economy is subject to certain shocks and deviates slightly from the trend."
                  ),
                  conditionalPanel("input.radio == 3", 
                                   h4("Definition:"),
                                   withMathJax(),
                                   helpText('An autoregressive process of order one follows the equation: $ y_t = d + \\phi y_{t-1}  + \\varepsilon_t$. Again, $\\varepsilon$ ~ $ N (0, \\sigma ^ 2 ) $. d represents the drift term. The higher the value of the $\\phi$ parameter, the more persistent the shock is between periods.'),
                                   h4("Intuition"), 
                                   "An autoregressive process is dependent on the value in the previous period. An example of that would be the growth of my kombucha batch. The more kombucha I produce today, the more scoby I will have tommorow. I throw away a fixed part of scobies and consume some kombucha, hence the value of theta will be lower than 1. Still, the amount I produce is subject to some random shocks. The drift can be thought of as me adding new tea for fermentation every period. "
                  ),
                  conditionalPanel("input.radio == 4", 
                                   h4("Definition:"),
                                   withMathJax(),
                                   helpText('A moving average process of order one  follows the equation: $ y_t = \\mu + \\varepsilon_t + \\theta  \\varepsilon_{t-1}$. Both error terms are distributed with mean zero and variance equal to $\\sigma^2$.  '),
                                   h4("Intuition"), 
                                   "If we would like to simulate the performance of an MMA fighter using a moving-average process, we would have a fixed proportion of matches won by this fighter represented by $\\mu$. A negative shock, e.g. a loss in a fight, will impact the next fight negatively as well. The magnitude of this impact is represented by the $\\theta$ parameter. Both error terms are white noise, so the process is stationary."
                  ),
                  conditionalPanel("input.radio == 5", 
                                   h4("Definition:"),
                                   "ARIMA processes represent a wide class of models that can be used to simulate the behaviour of time series processes. Depending on the specification, we can use the p,d and q terms to include more components relating to a particular model. The more p components, the more AR terms will be included in the equation. The more q terms, the more MA coefficents will be used. The d refers to the degree of differencing, that is if it is 1, the series is $I(1)$. Using d = 0 will result in giving us a standard ARMA model."
                  ),
        )
    ),
)

server <- function(input, output) {
    white_noise <-  reactive ({rnorm(n = input$time, mean = input$mean_1, sd = input$std_1)})
    x_axis <- reactive ({seq(1, input$time, by=1)})
    output$process <- renderPlot({
        if (input$radio == 1) {
            value <- white_noise ()
            time <- x_axis()
            combined_data <- as.data.frame(time, value)
            ggplot(combined_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + xlab ("Time") + ylab(input$variable_label) + ylim (input$min, input$max)
        }
        else {
            if (input$radio == 2) {
                time <- 1:input$time
                value <- noise <- rnorm(n = input$time, mean = 0, sd = input$std_2)
                value[1] <- input$first_value
                for (t in 2:input$time) {
                    value[t] <- input$drift + value[t - 1] + noise[t]
                }
                combined_data <- as.data.frame(time, value)
                ggplot(combined_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + xlab ("Time") + ylab(input$variable_label) + ylim (input$min, input$max)
            }
            else {
                if(input$radio == 3){
                    time <- 1:input$time
                    value <- noise <- rnorm(n = input$time, mean = 0, sd = input$std_3)
                    value[1] <- input$ar_start
                    for (t in 2:input$time) {
                        value[t] <- input$drift_2 + input$phi*value[t - 1] + noise[t]
                    }
                    combined_data <- as.data.frame(time, value)
                    ggplot(combined_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + xlab ("Time") + ylab(input$variable_label) + ylim (input$min, input$max)
                    
                }
                else{
                    if(input$radio ==4)
                    {
                        time <- 1:input$time
                        value <- noise <- rnorm(n = input$time, mean = 0, sd = input$std_4)
                        value[1] <- input$mu
                        for (t in 2:input$time) {
                            value[t] <- input$mu + noise[t] + input$theta * noise [t-1]
                        }
                        combined_data <- as.data.frame(time, value)
                        ggplot(combined_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + xlab ("Time") + ylab(input$variable_label) + ylim (input$min, input$max)
                        
                    }
                    else {
                        time <- 1:input$time
                        MA_coeff <- as.numeric(unlist(strsplit(input$MA_coefficents,",")))
                        AR_coeff <- as.numeric(unlist(strsplit(input$AR_coefficents,",")))
                        value <- arima.sim(n = input$time, model = list (ar = AR_coeff, ma=MA_coeff, order = c(input$p, input$d, input$q)))
                        combined_data <- as.data.frame(time, value)
                        ggplot(combined_data, aes(x=time,y=value)) + geom_line(color = input$line_color) + xlab ("Time") + ylab(input$variable_label) + ylim (input$min, input$max)
                        
                    }
                }
                
            }
        }
        
    })
}

shinyApp(ui = ui, server = server)
