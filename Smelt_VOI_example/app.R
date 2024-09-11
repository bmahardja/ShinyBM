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
library(rsconnect)

# Load data
consequence_table <-read.csv("ConsequenceTable_2024-09-10.csv")
# Standardize score based on local scale
dsm <- consequence_table %>% filter(Objective=="DeltaSmelt") %>%
  mutate(std_score=(Score-min(Score))/(max(Score)-min(Score)))
water <- consequence_table %>% filter(Objective=="WaterCost") %>%
  mutate(std_score=(max(Score)-Score)/(max(Score)-min(Score)))
# Recombine data
cons_table_std <- bind_rows(dsm, water)
# Custom colors and linetypes
custom_colors <- c("Alt F80" = "black","Alt F74" = "#E69F00", "Alt S74" = "#0072B2", "Alt S74F80"= "#D55E00","Alt NoX2" = "black","Alt NoFlow" = "black")
custom_line <- c("Alt F80" = "solid","Alt F74" = "solid", "Alt S74" = "longdash", "Alt S74F80"= "longdash","Alt NoX2" = "dotted","Alt NoFlow" = "dotted")
## Data for swing weighting
# Initialize an empty data frame
swing_table <- data.frame(Objective = c("Water Cost (in TAF)","Delta Smelt population growth (lambda)"),
                        BenchmarkAlternative = c(max(water$Score),min(dsm$Score)),
                        Alternative_A = c(min(water$Score),min(dsm$Score)),
                        Alternative_B = c(max(water$Score),max(dsm$Score)),
                        stringsAsFactors = FALSE)

# Grab data for the water cost plot
data_plot_water <- cons_table %>% filter(Objective=="WaterCost"&Hypothesis=="H1") %>%
  select(-Hypothesis)
# Convert the 'Category' column to a factor with a specific order
data_plot_water$Alternatives <- factor(data_plot_water$Alternatives, levels = c("Alt F80", "Alt F74", "Alt S74",
                                                                                "Alt S74F80","Alt NoX2"))
# Grab data for the delta smelt plot
data_plot_dsm <- dsm
# Convert the 'Category' column to a factor with a specific order
data_plot_dsm$Alternatives <- factor(data_plot_dsm$Alternatives, levels = c("Alt F80", "Alt F74", "Alt S74",
                                                                            "Alt S74F80","Alt NoX2"))
# Define custom colors for barplots
custom_colors_alt <- c("Alt F80" = "#000000", "Alt F74" = "#E69F00",
                       "Alt S74" = "yellow4" , "Alt S74F80" = "#56B4E9","Alt NoX2"= "#999999")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Decision Analysis for Delta Smelt Summer-Fall X2 Case Study"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            HTML("<strong>Note:</strong> Hypothesis weights must add up to 1."),
            uiOutput("warning"),  # Dynamic warning output
            numericInput("H1_weight", "Hypothesis 1 Weight", 0.2875, min = 0.0, max = 0.99),
            numericInput("H2_weight", "Hypothesis 2 Weight", 0.0875, min = 0.0, max = 0.99),
            numericInput("H3_weight", "Hypothesis 3 Weight", 0.0875, min = 0.0, max = 0.99),
            numericInput("H4_weight", "Hypothesis 4 Weight", 0.0375, min = 0.0, max = 0.99),
            numericInput("H5_weight", "Hypothesis 5 Weight", 0.2875, min = 0.0, max = 0.99),
            numericInput("H6_weight", "Hypothesis 6 Weight", 0.0875, min = 0.0, max = 0.99),
            numericInput("H7_weight", "Hypothesis 7 Weight", 0.0875, min = 0.0, max = 0.99),
            numericInput("H8_weight", "Hypothesis 8 Weight", 0.0375, min = 0.0, max = 0.99),
            sliderInput("fish_weight",
                        "Weight for Delta Smelt objective (the rest will be allocated towards Water Cost objective):",
                        min = 0.01,
                        max = 0.99,
                        value = 0.50),
            h3("Hypothesis Information"),
            p("Hypothesis 1: IBMR v1 with Delta Smelt distribution and food/zooplankton submodels"),
            p("Hypothesis 2: IBMR v1 with just the food/zooplankton submodel"),
            p("Hypothesis 3: IBMR v1 with just the Delta Smelt submodel"),
            p("Hypothesis 4: IBMR v1 with no submodels (only OMR changes between alts per CalSim3)"),
            p("Hypothesis 5: IBMR v2 with Delta Smelt distribution and food/zooplankton submodels"),
            p("Hypothesis 6: IBMR v2 with just the food/zooplankton submodel"),
            p("Hypothesis 7: IBMR v2 with just the Delta Smelt submodel"),
            p("Hypothesis 8: IBMR v2 with no submodels (only OMR changes between alts per CalSim3)"),
            h3("Alternative Information"),
            p("Alt F80: Fall Flow Action, Status Quo (X2 at 80 km for September-October)"),
            p("Alt F74: Fall Flow Action to roughly represent 2008-2009 BiOp (X2 at 74 km for September-October)"),
            p("Alt S74: Summer Flow Action to roughly represent Polansky et al. 2024 hypothesized action (X2 at 74 km for July-August)"),
            p("Alt S74F80: Combined Summer and Fall Flow Action (combination of S74 and F80)"),
            p("Alt NoX2: No Summer-Fall X2/Flow Action"),
            HTML("<strong>Note:</strong> Actions would only occur in W or AN years. Old and Middle River + Suisun Marsh Salinity Control Gate actions are present across all alternatives based on the 2019/2020 BiOps."),
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(tabPanel("Line Plot",plotOutput("Plot")),tabPanel("Utility Score Table",tableOutput("tableSum")),tabPanel("VOI",textOutput("VOI_calc1"),textOutput("VOI_calc2")),
                              
                              tabPanel("Swing Weighting", h3("Instructions"),
                                       p("See hypothethical alternatives below. Choose your preferred scenario.This will be your rank 1 scenario. Assign a score of 100 to your rank 1 scenario in the box below. How important is the swing from worst to best level of rank 2 scenario compared with the swing from worst to best on the rank 1 scenario? Assign a value between 0 and 100 that reflects the relative value of the rank 2 scenario."),
                                       tableOutput("tableSwing"),
                                       numericInput("numInputAlt_A","Enter a score between 0 and 100 for Alternative A",value=100, min=0, max=100),
                                       numericInput("numInputAlt_B","Enter a score between 0 and 100 for Alternative B",value=100, min=0, max=100),actionButton("submit","Submit"),
                                       h4("Your calculated objective weights based on swing weighting:"),
                                       textOutput("swingText_Water"),
                                       textOutput("swingText_DeltaSmelt")),
                              tabPanel("Performance Plots",plotOutput("PlotSmelt"),plotOutput("PlotWater")),
                              tabPanel("Raw Consequence Table",tableOutput("tableRaw"))))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #Pull together line plot data
    line_plot_data <- reactive({cons_table_std %>% mutate(hypo_weight= case_when(Hypothesis == "H1" ~ input$H1_weight,
                                                                                Hypothesis == "H2" ~ input$H2_weight,
                                                                                Hypothesis == "H3" ~ input$H3_weight,
                                                                                Hypothesis == "H4" ~ input$H4_weight,
                                                                                Hypothesis == "H5" ~ input$H5_weight,
                                                                                Hypothesis == "H6" ~ input$H6_weight,
                                                                                Hypothesis == "H7" ~ input$H7_weight,
                                                                                Hypothesis == "H8" ~ input$H8_weight)) %>%
        mutate(score_hyp = hypo_weight*std_score) %>% group_by(Alternatives,Objective) %>%
        summarise(comp_score = sum(score_hyp)) %>% mutate(fish_weight = case_when(Objective == "DeltaSmelt" ~ 1.0,
                                                                                  Objective == "WaterCost" ~ 0))
        })
    
    output$Plot <- renderPlot({
        #Plot here
        print(ggplot(data=line_plot_data(), aes(x=fish_weight, y=comp_score, color=Alternatives,linetype=Alternatives)) +
        geom_line(linewidth= 1.2) +
        geom_vline(xintercept = input$fish_weight, linetype="dotted", color = "red", size=1) +
        theme_minimal()+
        labs(title = NULL,
             x = "Delta Smelt objective weight",
             y = "Composite score (objective-weighted linear value function)") +
        theme(axis.text = element_text(size = 14),  # Increase tick mark font size
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14),
              legend.key.size = unit(1.2, "cm")) +
          scale_color_manual(values = custom_colors) +
          scale_linetype_manual(values = custom_line))
    })
    
    # Create final utility score table
    cons_table_std_new <- reactive({cons_table_std %>% mutate(hypo_weight= case_when(Hypothesis == "H1" ~ input$H1_weight,
                                                                                     Hypothesis == "H2" ~ input$H2_weight,
                                                                                     Hypothesis == "H3" ~ input$H3_weight,
                                                                                     Hypothesis == "H4" ~ input$H4_weight,
                                                                                     Hypothesis == "H5" ~ input$H5_weight,
                                                                                     Hypothesis == "H6" ~ input$H6_weight,
                                                                                     Hypothesis == "H7" ~ input$H7_weight,
                                                                                     Hypothesis == "H8" ~ input$H8_weight)) %>%
        mutate(score_hyp = hypo_weight*std_score) %>% group_by(Alternatives,Objective) %>%
        summarise(comp_score = sum(score_hyp)) %>% mutate(obj_weight = case_when(Objective == "DeltaSmelt" ~ input$fish_weight,
                                                                                  Objective == "WaterCost" ~ 1-input$fish_weight)) %>%
        mutate(comp_score_obj = comp_score*obj_weight) %>% ungroup() %>% group_by(Alternatives) %>%
        summarise(CompositeScore = sum(comp_score_obj))})
  
    output$tableSum <- renderTable(cons_table_std_new() %>% arrange(desc(CompositeScore)))
    
    # Add VOI calculation tables
    cons_table_reconfig <- reactive({cons_table_std %>% mutate(hypo_weight= case_when(Hypothesis == "H1" ~ input$H1_weight,
                                                                                          Hypothesis == "H2" ~ input$H2_weight,
                                                                                          Hypothesis == "H3" ~ input$H3_weight,
                                                                                          Hypothesis == "H4" ~ input$H4_weight,
                                                                                      Hypothesis == "H5" ~ input$H5_weight,
                                                                                      Hypothesis == "H6" ~ input$H6_weight,
                                                                                      Hypothesis == "H7" ~ input$H7_weight,
                                                                                      Hypothesis == "H8" ~ input$H8_weight)) %>%
        dplyr::select(Alternatives, Hypothesis, Objective, std_score, hypo_weight) %>% 
        spread(Objective, std_score) %>% 
        mutate(composite_score = (DeltaSmelt * input$fish_weight) + (WaterCost * (1 - input$fish_weight))) })
      
    certainty_calc <- reactive({ cons_table_reconfig() %>% 
      group_by(Hypothesis) %>% 
      summarise(composite_score = max(composite_score), hypo_weight = mean(hypo_weight)) %>%
      mutate(hypothesis_score = composite_score * hypo_weight) })
    
    uncertainty_calc <- reactive({ cons_table_reconfig() %>% 
      mutate(composite_score_hypo = hypo_weight * composite_score) %>% 
      group_by(Alternatives) %>% 
      summarise(composite_score = sum(composite_score_hypo)) })
    # Show text on VOI
    output$VOI_calc1 <- renderText({
      paste("Value of Perfect Information (Composite Score):",round(sum(certainty_calc()$hypothesis_score)-max(uncertainty_calc()$composite_score),digits=3))
    })
    output$VOI_calc2 <- renderText({
    paste("Value of Perfect Information (% of Best composite Score):",paste(round((sum(certainty_calc()$hypothesis_score)-max(uncertainty_calc()$composite_score))/max(cons_table_std_new()$CompositeScore)*100,digits=3)),"%")
    })
    
    # Show table for swing weighting
    output$tableSwing <- renderTable(swing_table)
    # Results of swing weighting
    observeEvent(input$submit, {
      output$swingText_Water <- renderText({
        paste("Objective Weight for Water Cost:", round(input$numInputAlt_A/(input$numInputAlt_A+input$numInputAlt_B),2))
        })
      output$swingText_DeltaSmelt <- renderText({
        paste("Objective Weight for Delta Smelt:", round(input$numInputAlt_B/(input$numInputAlt_A+input$numInputAlt_B),2))
        })
    })
    # Show warning if the hypothesis weight number exceeds 1
    output$warning <- renderUI({
      if (input$H1_weight+input$H2_weight+input$H3_weight+input$H4_weight+input$H5_weight+input$H6_weight+input$H7_weight+input$H8_weight != 1) {
        tags$div(style = "color: red;", "Warning: The total hypothesis weights do not add up to 1!")
      }
    })
    
    # Plot performance metric bar plots
    # Delta Smelt plot
    output$PlotSmelt <- renderPlot({
      print(ggplot(data_plot_dsm, aes(x=Alternatives, y=Score, fill=Alternatives)) +
              geom_bar(stat = "identity") +
              labs(title = "Delta Smelt Objective",
                   x = "Alternative",
                   y = "Lambda") + 
              facet_grid(cols = vars(Hypothesis)) +
              scale_fill_manual(values = custom_colors_alt,guide="none")  +   # Use a color palette
              theme_bw() +                          # Classic theme 
              coord_cartesian(ylim=c(0.75,1.05)) +
              theme(axis.text.y = element_text(size = 14),
                    axis.text.x = element_text(size = 14,angle = 45, hjust = 1),
                    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                    axis.title.x = element_text(size=14),
                    axis.title.y = element_text(size=14),
                    strip.text = element_text(size = 14)) )
    })
    # Water cost plot
    output$PlotWater <- renderPlot({
    print(ggplot(data_plot_water, aes(x=Alternatives, y=Score, fill=Alternatives)) +
      geom_bar(stat = "identity") +
      labs(title = "Water Cost Objective",
           x = "Alternative",
           y = "Thousand Acre Feet") + 
      scale_fill_manual(values = custom_colors_alt,guide="none")  +   # Use a color palette
      theme_bw() +                          # Classic theme 
      theme(axis.text = element_text(size = 14),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)) )
    })
    
    
    # Raw consequence table
    output$tableRaw <- renderTable(consequence_table)
}

# Run the application 
shinyApp(ui = ui, server = server)

