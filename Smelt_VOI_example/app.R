#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

consequence_table <-read.csv("ConsequenceTable.csv")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Value of Information Calculation for Delta Smelt Case Study"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("H1_weight", "Hypothesis 1 Weight", 0.25, min = 0.01, max = 0.99),
            numericInput("H2_weight", "Hypothesis 2 Weight", 0.25, min = 0.01, max = 0.99),
            numericInput("H3_weight", "Hypothesis 3 Weight", 0.25, min = 0.01, max = 0.99),
            numericInput("H4_weight", "Hypothesis 4 Weight", 0.25, min = 0.01, max = 0.99),
            sliderInput("fish_weight",
                        "Weight for Delta Smelt objective (the rest will be allocated towards Water Cost objective):",
                        min = 0.01,
                        max = 0.99,
                        value = 0.50)
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(tabPanel("Table",tableOutput("tableSum")),tabPanel("VOI",textOutput("VOI_calc1"),textOutput("VOI_calc2"),textOutput("VOI_calc3")),tabPanel("Plot",plotOutput("Plot"))))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #Pull together data for VOI output
    sum_data <- reactive({df_weight<- consequence_table %>% mutate(hypothesis_weight=case_when(
        hypothesis==1 ~ input$H1_weight,
        hypothesis==2 ~ input$H2_weight,
        hypothesis==3 ~ input$H3_weight,
        hypothesis==4 ~ input$H4_weight)) %>%
        mutate(composite_score=deltasmelt*input$fish_weight+watercost*(1-input$fish_weight))
        })
    # Calculate outcome weighted by hypothesis
    outcome_data <- reactive({sum_data() %>% mutate(weighted_val=composite_score*hypothesis_weight) %>%
        group_by(alt) %>% summarise(weighted_outcome=sum(weighted_val))
        })
    
    # Maximum/best outcome for each hypothesis
    outcome_max <- reactive({sum_data() %>% group_by(hypothesis) %>% 
        summarise(hypothesis_weight=mean(hypothesis_weight),
                  max_outcome=max(composite_score))
        })
    
    # Create data frame for plot
    df_fish <- reactive({sum_data() %>% select(-composite_score) %>% mutate(smelt_comp=deltasmelt*hypothesis_weight) %>%
        group_by(alt) %>% summarise(comp_score=sum(smelt_comp)) %>% mutate(fish_weight=1)})
    
    df_water <- reactive({sum_data() %>% select(-composite_score) %>% mutate(water_comp=watercost*hypothesis_weight) %>%
        group_by(alt) %>% summarise(comp_score=sum(water_comp)) %>% mutate(fish_weight=0)})
    
    # Combine data frame
    df_test_comb <- reactive({bind_rows(df_fish(),df_water())})
    

    
    output$Plot <- renderPlot({
        #Plot here
        print(ggplot(data=df_test_comb(), aes(x=fish_weight, y=comp_score, color=alt)) +
            geom_line() +  geom_vline(xintercept = input$fish_weight, linetype="dotted", color = "red", size=1) +
            theme_minimal())
    })
    
    output$tableSum <- renderTable(sum_data())
    
    output$VOI_calc1 <- renderText({
        paste("Expected Value under Uncertainty:", max(outcome_data()$weighted_outcome))
    })
    output$VOI_calc2 <- renderText({
        paste("Expected Value under Certainty:", sum(outcome_max()$hypothesis_weight*outcome_max()$max_outcome))
    })
    output$VOI_calc3 <- renderText({
        paste("Value of Perfect Information:", sum(outcome_max()$hypothesis_weight*outcome_max()$max_outcome)-max(outcome_data()$weighted_outcome))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

