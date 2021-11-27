library(shiny)
library(tidyverse)

# Assignment B-3: Create a shinny app. I selected option B
# Here are the three features I used:
# Feature 1: sidebarLayout() -> part of the UI layout
# Feature 2: checkboxGroupInput() -> functional widget that creates check boxes
# feature 3: selectInput() -> functional widget that creates a drop-down menu to select an option

# Load the data and then pivot_longer to make it in a tidy format to plot with ggplot2
dat <- read_csv(here::here("S24_7_subsystems.csv")) %>% 
    pivot_longer(cols = starts_with("M_"), names_to = "strain")

# Define UI for application that draws a heatmap and also makes a table of the heatmap values
ui <- fluidPage(
    # Application title
    titlePanel("S24-7 Strains Subsystem Gene Comparisons"),
    "Look at the number of features (genes) in subsystems predicted for the genomes of different S24-7 strains based on a selected gene category.
    Predictions where done from genome annotations from the RAST Web server.",
    br(),
    
    # This create the layout for both the side pannel (select choices) and the main pannel (plot and table)
    sidebarLayout(
        sidebarPanel(verticalLayout(
            checkboxGroupInput("my_choices", "Select S24-7 Strains", 
                               choices = unique(dat$strain), selected = unique(dat$strain)),
            selectInput("my_category", "Select Gene Category", 
                        choices = unique(dat$Category), selected = "Stress Response")
        )),
        mainPanel(
            plotOutput("my_heatmap"),
            tableOutput("my_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # store 
    filtered <- reactive({
        print(input$my_choices)
        print(input$my_category)
        dat %>%
            filter(strain %in% input$my_choices) %>%
            filter(Category == input$my_category)
    })
    
    # function for the heatmap plot
    output$my_heatmap <- renderPlot(
        filtered() %>%
            ggplot(aes(x = strain, y = Subsystem, fill = value)) +
            geom_tile(color = "black") +
            scale_fill_gradient(low = "gray98", high = "black") + 
            theme(axis.text.x = element_text(angle = 90)) # rotate x labels 90 degrees
    )
    
    # output table with value on the bottom
    output$my_table <- renderTable(
        filtered() %>%
            pivot_wider(names_from = strain, values_from = value) %>%
            select(-Category)
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
