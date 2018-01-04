library(ggplot2)
library(dplyr)
library(tibble)
library(dplyr)
library(shiny)
library(rsconnect)
library(scales)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  headerPanel('Hourly rate calculator by Taras Kaduk'),
  sidebarPanel(

    # I should have probably parametirized the mins and maxs.
    sliderInput('iRate', 'Rate', 70,
                min = 50, max = 200),
    sliderInput('iHours', 'Hours', 8,
                min = 5, max = 12),
    sliderInput('iDays', 'Days', 5,
                min = 4 , max = 7),
    sliderInput('iWeeks', 'Weeks', 48,
                min = 42, max = 52),
    sliderInput('iIncome', 'Income', 145000,
                min = 55000, max = 200000, step = 1000)
  ),
  mainPanel(
    plotOutput('plot1', width = 600, height = 450),
    plotOutput('plot2', width = 600, height = 100)
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  #These are initial inputs
  iRate <- reactive({input$iRate})
  iHours <- 10
  iDays <- 7
  iWeeks <- 48
  iIncome <- 145000
  
  # Colors
  col_need <- 'red4'
  col_value <- 'grey30'
  
  # Calculating annual income based on inputs.
  # BTW, what is all this reactive({ }) stuff? Shiny syntax baby!
  calcIncome <- reactive({input$iRate * input$iHours  * input$iDays * input$iWeeks})
  
  # Creating a vector with input variables.
  # Why vector? I don't remember.
  # Tried switching to a data frame and failed.
  vInput <- reactive({
    c("Rate" = input$iRate, 
      "Hours" = input$iHours, 
      "Days" = input$iDays, 
      "Weeks" = input$iWeeks, 
      "Income" = calcIncome())
  })
  
  # Another 2 vectors, this time for min & max.
  # Still wondering why these are not in a dataframe...
  vMin <- as.integer(c(50,6,4,40,55000))
  vMax <- as.integer(c(200,12,7,52,200000))
  
  # OK, now compiling it all into a dataframe. 
  # I swear I'm not sure why I haven't started with a data frame...
  df <- reactive({data_frame(Param = names(vInput()), 
                             Value = vInput(), 
                             Min = vMin, 
                             Max = vMax, 
                             
                             ## this line below calculates how much you NEED of the current param,
                             ## ...all other things being equal
                             Need = as.integer(input$iIncome/(calcIncome()/vInput())),
                             
                             ## next 4 lines take the real value and scale them to fit into 1 graph
                             ## you don't want days and hours to be displayed to scale with weeks and...
                             ## ...hundreds of thousands of dollars
                             normNeed = 0,
                             normMin = (vMin - input$iIncome/(calcIncome()/vInput()))/(vMax - vMin),
                             normMax = (vMax - input$iIncome/(calcIncome()/vInput()))/(vMax - vMin),
                             normValue = (vMin - input$iIncome/(calcIncome()/vInput()))/
                                         (vMax - vMin) + 
                                         ((vInput()-vMin))/(vMax - vMin)
  )})
  
  # OK, I added this piece of code recently.
  # I wanted to break out the initial data frame.
  # Otherwise, the total income looked weird on the graph. 
  # It was, like, reversed to the other params
  df2 <- reactive({df() %>% filter(Param != 'Income')})
  df3 <- reactive({df() %>% filter(Param == 'Income')})
  
  
  # Building the plot. Plot1 - plot of all params BUT the annual income
  output$plot1 <- renderPlot({
    ggplot(df2(), aes(x = factor(Param, levels = c("Weeks", "Days", "Hours", "Rate")))) +
      
      # Segment between min and max
      # Basically, the grey lines with 2 points at the ends of each line
      geom_segment(aes(xend = Param, y = normMin, yend = normMax), size = 1, col = "grey")+ 
      geom_point(aes(y = normMax), col = "dark grey") +
      geom_point(aes(y = normMin), col = "dark grey") +
      geom_text(aes(y = normMin, label = Min), position = position_nudge(x=-0.1, y=0), size = 4, col = "dark grey")+
      geom_text(aes(y = normMax, label = Max), position = position_nudge(x=-0.1, y=0), size = 4, col = "dark grey")+
      
      #The "need". AKA how much it is supposed to be
      geom_text(aes(y = 0, label = Need), position = position_nudge(x=0.3, y=0.1), size = 6, col = col_need)+
      geom_point(aes(y = normNeed), size = 4, col = col_need) +      
      
      #Your current value
      geom_point(aes(y = normValue), size = 4, col = col_value) +
      geom_text(aes(y = normValue, label = Value), position = position_nudge(x=-0.3), size = 6, col = col_value)+
      
      xlab("")+
      ylab("")+
      labs(
        title = 'Entered and calculated parameters based on other entered parameters'
      ) +
      theme(axis.text.y=element_text(size=14,colour="#535353",face="bold"),
            axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
      coord_flip()
  })
  
  #Plot2 - just the annual income.
  #Same stuff, only dots are reversed.
  #Also, $ formatting applied.
  output$plot2 <- renderPlot({
    ggplot(df3(), aes(x = Param)) +
      
      geom_segment(aes(xend = Param, y = normMin, yend = normMax), size = 1, col = "grey")+ 
      geom_point(aes(y = normMax), col = "dark grey") +
      geom_point(aes(y = normMin), col = "dark grey") +
      geom_text(aes(y = normMin, label = paste0('$',comma(Min))), position = position_nudge(x=-0.1, y=0), size = 4, col = "dark grey")+
      geom_text(aes(y = normMax, label = paste0('$',comma(Max))), position = position_nudge(x=-0.1, y=0), size = 4, col = "dark grey")+      
         
      geom_text(aes(y = 0, label = paste0('$',comma(Need))), position = position_nudge(x=0.3, y=0.1), size = 6, col = col_value)+
      geom_point(aes(y = normNeed), size = 4, col = col_value) +      
      
      
      geom_point(aes(y = normValue), size = 7, col = 'red') +
      geom_text(aes(y = normValue, label = paste0('$',comma(Value))), position = position_nudge(x=-0.3), size = 7, col = 'red')+
      
      xlab("")+
      ylab("")+
      labs(
        title = 'Entered and calculated income based on other entered parameters'
      ) +
      theme(axis.text.y=element_text(size=14,colour="#535353",face="bold"),
            axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
      coord_flip()
  })  
  
}

# Et voila!
shinyApp(ui = ui, server = server)

