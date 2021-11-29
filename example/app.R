library(shiny)
library(tidyverse)

# Assignment B-3: Create a shinny app. I selected option B
# Here are the three features I used:
# Feature 1: sidebarLayout() -> part of the UI layout
# Feature 2: checkboxGroupInput() -> functional widget that creates check boxes
# feature 3: selectInput() -> functional widget that creates a drop-down menu to select an option

# Load the data and then pivot_longer to make it in a tidy format to plot with ggplot2
dat <- read_csv(here::here("S24_7_data.csv")) %>% 
    pivot_longer(cols = starts_with("M_"), names_to = "strain")

# Define UI for application that draws a heatmap and also makes a table of the heatmap values
ui <- fluidPage(
    # Application title
    titlePanel("M. intestinale Strains Heatmaps"),
    "Look at the number of features (genes) in subsystems predicted for the genomes of different M. intestinale strains based for a selected gene category.
    Subsystem predictions were done from genome annotations using the RAST Web server (https://rast.nmpdr.org/)",
    br(),
    
    # FEATURE 1: This create the layout for both the side panel (select choices) 
    # and the main panel (plot and table). In the sidebarPanel we have the checkboxes so the 
    # user can select different Bacterial strains. Right below it we have a drop-down menu with
    # the different gene categories the user can explore. In the main pannel we have the heatmap
    # that will be rendered, and below it a table with the values for each cell of the heatmap
    sidebarLayout(
        sidebarPanel(verticalLayout(
            
            
            # Feature 2: This creates checkboxes that allows the user to select which strains
            # they want to see in the rended heatmap. The default select all strains
            checkboxGroupInput("my_choices", "Select M. intestinale Strains", 
                               choices = unique(dat$strain), selected = unique(dat$strain)),
            
            # Feature 3: This creates a drop-down menu with the different gene categories that the user
            # can select to make a heatmap about. # defaults is 'Stress Response' 
            selectInput("my_category", "Select Gene Category",
                        choices = unique(dat$Category), selected = "Stress Response"),
            strong("Filter by:"),
            checkboxInput("subcategory", "Subcategory"),
            
            uiOutput("sub"),
            
            
            # conditionalPanel(condition = "input.subcategory == true",
            #                  selectInput("my_subcategory", "Select Gene Subcategory", 
            #                              choices = unique(dat$Subcategory), selected = "Oxidative stress"))
            
        )),
        
        # Here it the main panel that has the plot and table below it
        mainPanel(
            plotOutput("my_heatmap"),
            tableOutput("my_table")
        )
    )
)

# Define server logic required to draw a heatmap
server <- function(input, output) {
    # Filter the data set (dat) with the input parameters given by the used and
    # store it into a variable
    
    filtered_cat <- reactive({
        dat %>%
            filter(strain %in% input$my_choices) %>%
            filter(Category == input$my_category)
    })
    
    filtered <- reactive({
        print(input$my_choices)
        print(input$my_category)
        print(input$subcategory)

        if(input$subcategory == TRUE){
            return(
                filtered_cat() %>%
                filter(Subcategory %in% input$my_subcategory)
            )}
        filtered_cat()
    })
    
    
    output$sub <- renderUI({
        conditionalPanel(condition = "input.subcategory == true",
                         checkboxGroupInput("my_subcategory", "Select Gene Subcategory", 
                                     choices = unique(filtered_cat()$Subcategory), selected = filtered_cat()$Subcategory))
        })
    
    
    # Creates heatmap by using ggplot
    output$my_heatmap <- renderPlot({
        filtered() %>%
            ggplot(aes(x = strain, y = Subsystem, fill = value)) +
            geom_tile(color = "black") + # geom_tile is used to make heatmaps
            scale_fill_gradient(low = "gray98", high = "black") +  # Selects the minimum and maximum color for the cells
            theme(axis.text.x = element_text(angle = 90)) # rotate x labels by 90 degrees 
        
        # if(input$subcategory == TRUE){
        #     filtered2() %>%
        #         ggplot(aes(x = strain, y = Subsystem, fill = value)) +
        #         geom_tile(color = "black") + # geom_tile is used to make heatmaps
        #         scale_fill_gradient(low = "gray98", high = "black") +  # Selects the minimum and maximum color for the cells
        #         theme(axis.text.x = element_text(angle = 90)) # rotate x labels by 90 degrees 
        # }

    })
    
    # output table with value for each cell below the heatmap. since we had to change
    # the dat table to be in tidy format, we are reverting it to the untidy, but more readable format.
    output$my_table <- renderTable(
        filtered() %>%
            pivot_wider(names_from = strain, values_from = value) %>%
            select(-Category, -Subcategory) # This column is redundant
    )
}

# Run the application 
shinyApp(ui = ui, server = server)