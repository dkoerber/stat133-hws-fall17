#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggvis)
source('../code/functions.R')

# load clean data
c_scores <- as.data.frame(read.csv('../data/cleandata/cleanscores.csv'))

# order the Grade factor properly
grade_order <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D', 'F')
c_scores$Grade <- factor(c_scores$Grade, levels = grade_order)

# set continuous variables
continuous <- c("HW1", "HW2", "HW3", "HW4", "HW5", "HW6", "HW7", "HW8", "HW9",
                "ATT", "QZ1", "QZ2", "QZ3", "QZ4", "EX1", "EX2", "Test1",
                "Test2", "Homework", "Quiz", "Lab", "Overall")

# table for tab1
grade_table <- as.data.frame(table(c_scores$Grade))
grade_table <- mutate(grade_table, Prop = Freq / nrow(c_scores))
names(grade_table)[1] = "Grade"

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Grade Visualizer"),
   
   # Conditional sidebar layouts
   sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition = "input.activetab == 1",
                         h3("Grade Distribution"),
                         tableOutput("summary")),
        conditionalPanel(condition = "input.activetab == 2",
                        selectInput("tab2_x", "x-axis variable", continuous,
                                    selected = "Test1"),
                        sliderInput("tab2_bins", "Bin Width",
                                    min = 1, max = 10, value = 10),
                        helpText("Summary Statistics"),
                        verbatimTextOutput('stats')),
        conditionalPanel(condition = "input.activetab == 3",
                         selectInput("tab3_x", "x-axis variable", continuous,
                                     selected = "Test1"),
                         selectInput("tab3_y", "y-axis variable", continuous,
                                     selected = "Overall"),
                         sliderInput("tab3_opacity", "Opacity",
                                     min = 0, max = 1, value = 0.5),
                         radioButtons("tab3_line", "Show line",
                                      choices = list("none" = 1,
                                                     "lm" = 2,
                                                     "loess" = 3),
                                      selected = 1),
                         helpText("Correlation:"),
                         verbatimTextOutput("corr"))
   ),
   mainPanel(
     tabsetPanel(type = "tabs",
                 tabPanel("Barchart", value = 1, ggvisOutput("barchart")),
                 tabPanel("Histogram", value = 2, ggvisOutput("histogram")),
                 tabPanel("Scatterplot", value = 3, ggvisOutput("scatterplot")),
                 id = "activetab")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Barchart (for 1st tab)
  vis_barchart <- reactive({
    c_scores %>% 
      ggvis(x = ~Grade, fill := "cornflowerblue") %>% 
      layer_bars(stroke := "cornflowerblue", width = 0.9,
                 fillOpacity := 0.8, fillOpacity.hover := 1) %>%
      add_axis("y", title = "Frequency")
  })
  
  vis_barchart %>% bind_shiny("barchart")
  output$summary <- renderTable(grade_table, digits = 2)
  
  # Histogram (for 2nd tab)
  vis_histogram <- reactive({
    
    # print summary stats
    x <- select(c_scores, input$tab2_x)[[1]]
    output$stats <- renderPrint({print_stats(summary_stats(x))})
    
    # create histogram
    tab2_x <- prop("x", as.symbol(input$tab2_x))
    c_scores %>% 
      ggvis(x = tab2_x, fill := "cornflowerblue") %>% 
      layer_histograms(stroke := 'white',
                        width = input$tab2_bins)
  })

  vis_histogram %>% bind_shiny("histogram")
  
  # Scatterplot (for 3rd tab)
  vis_scatterplot <- reactive({
    
    # show correlation
    x <- select(c_scores, input$tab3_x)[[1]]
    y <- select(c_scores, input$tab3_y)[[1]]
    output$corr <- renderText({cor(x, y)})
    
    #create scatterplot
    tab3_x <- prop("x", as.symbol(input$tab3_x))
    tab3_y <- prop("y", as.symbol(input$tab3_y))
    scatter_plot <- layer_points(ggvis(c_scores, x = tab3_x, y = tab3_y,
                                       fill := "cornflowerblue", 
                                       fillOpacity := input$tab3_opacity))
    # logic for none, lm, and loess lines
    if (input$tab3_line == 1) {
      scatter_plot
    } else if (input$tab3_line == 2) {
      scatter_plot %>%
        layer_model_predictions(model = "lm")
    } else if (input$tab3_line == 3) {
      scatter_plot %>%
        layer_model_predictions(model = "loess")
    }
  })

  vis_scatterplot %>% bind_shiny("scatterplot")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

