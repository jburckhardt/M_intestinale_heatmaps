library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)

# Load R library with functions
source("heatmaps_functionsV3.R") # library for heatmaps

# Assignment B-3: Create a shinny app. I selected option B
# Here are the three features I used:
# Feature 1: sidebarLayout() -> part of the UI layout
# Feature 2: checkboxGroupInput() -> functional widget that creates check boxes
# feature 3: selectInput() -> functional widget that creates a drop-down menu to select an option

# Load the data and then pivot_longer to make it in a tidy format to plot with ggplot2
strains_table <- create_gene_array(directory = "S24_7_genome_annotations/", absolute_count = TRUE)
dat <- create_heatmap(df = strains_table, collapse_by = c("Category", "Subcategory", "Subsystem"), make_heatmap = FALSE) %>%
    pivot_longer(cols = starts_with("M_"), names_to = "strain") %>%
    mutate(text = paste0("Strain: ", strain, "\n", "Row: ", Subsystem, "\n", "Value: ",round(value,0)))



ui <- dashboardPage(
    dashboardHeader(title = "M. intestinale Strains Heatmaps"),
    dashboardSidebar(
        
        # creating tabs for the app
        sidebarMenu(
            menuItem("Heatmap", tabName = "heatmap", icon = icon("fas fa-chart-bar")),
            menuItem("Interactive Heatmap", tabName = "int_heatmap", icon = icon("fas fa-chart-bar")),
            menuItem("Table", icon = icon("fas fa-table"), tabName = "table")
            )
        ),
    
    dashboardBody(
        
        # Set a theme using dashboardthemes
        shinyDashboardThemes(
            theme = "grey_light"
        ),
        
        tabItems(
            tabItem(tabName = "heatmap",
                    h2("M. intestinale strain heatmaps"),
                    br(),
                    # In this fluid row we will have the options to be selected
                    fluidRow(
                        # box containing the plot!
                        box(plotlyOutput("my_heatmap"), status = "warning", width = 6),
                        
                        # columns containing boxes with the different filtering widgets
                        column(width = 6,
                               
                               # box to select different S24-7 strains - checkboxes
                               box(title = "1) Select M. intestinale strains", status = "primary",
                                   prettyCheckboxGroup(
                                       inputId = "my_choices",
                                       label = "Strains",
                                       choices = unique(dat$strain),
                                       selected = unique(dat$strain),
                                       status = "primary",
                                       shape = "curve"
                                       )
                                   ),
                               
                               # box to select different gene categories
                               box(title = "2) Select gene category", status = "primary",
                                   pickerInput("my_category", "Categories",
                                               choices = unique(dat$Category), 
                                               selected = "Stress Response", 
                                               options = list(style = "btn-primary"))
                                   ),
                               
                               # reactive switch that will allow the user to further filter to only include certain subcategories or subsystems
                               box(title = "3) Filter by other gene groupings:", status = "primary",
                                   strong("Filter by:"),
                                   # switch for Subcategory filtering
                                   materialSwitch(inputId = "subcategory", label = "Subcategory",
                                                  value = FALSE, status = "primary"),
                                   
                                   # Switch for Subsystem filtering
                                   materialSwitch(inputId = "subsystem", label = "Subsystem",
                                                  value = FALSE, status = "primary"),
                                   
                                   uiOutput("subcat"),
                                   uiOutput("subsys")
                                   )
                               )
                        ),
                    
                    # box to change color of heatmap
                    box(title = "4) Change Heatmap colors", status = "primary",
                        "Note: Click checkmark icon to update or hit Enter",
                        
                        searchInput(
                            inputId = "low",
                            label = "Lowest value color:",
                            value = "gray98",
                            placeholder = "Type acceptable R color...",
                            btnSearch = icon("check"), 
                            btnReset = icon("remove"),
                            width = "50%"),
                        
                        searchInput(
                            inputId = "high",
                            label = "Maximum value color:",
                            value = "black",
                            placeholder = "Type acceptable R color...",
                            btnSearch = icon("check"), 
                            btnReset = icon("remove"),
                            width = "50%")
                        ),
                    
                    # box to save plot
                    box(title = "5) Save the plot", status = "primary",
                        downloadBttn(outputId = "download_plot",
                                     style = "jelly",
                                     color = "primary")
                        )
            ),
            
            # Tab item for expanded view of interactive plot
            tabItem(tabName = "int_heatmap",
                    h2("Interactive Heatmap"),
                    
                    # plot!
                    box(height = 900, width = 12,
                        # update width and height of the plot
                        dropdownButton(
                            h3("Update Plot Dimentions"),
                    
                            sliderInput(inputId = "plot_width",
                                        label = "Width",
                                        value = 1000,
                                        min = 1,
                                        max = 1800),
                            
                            sliderInput(inputId = "plot_height",
                                        label = "Height",
                                        value = 600,
                                        min = 1,
                                        max = 1000),
                            
                            circle = TRUE, status = "danger",
                            icon = icon("gear"), width = "300px",
                            
                            tooltip = tooltipOptions(title = "Click to adjust plot dimentions!")
                        ),
                        
                        # Plot output
                        plotlyOutput("copy_heatmap")
                    )
                
                
                
            ),
            
            ###### This is for the table of values ###########
            tabItem(tabName = "table",
                    h2("Table of heatmap values"),
                    fluidRow(
                        box(status = "warning", width = 8,
                            tableOutput("my_table"),
                            style='overflow-x: scroll'
                            ),
                        box(title = "Save table:", status = "primary", width = 4,
                            downloadBttn(outputId = "download_table",
                                         style = "jelly",
                                         color = "primary")
                            )
                        )
                    )
            )
        )
)


# Define server logic required to draw a heatmap
server <- function(input, output) {
    # Filter the data set (dat) with the input parameters given by the used and
    # store it into a variable
    
    # store the initial data that will be reactively change based on the strains picked and the category selected
    filtered_cat <- reactive({
        dat %>%
            filter(strain %in% input$my_choices) %>%
            filter(Category == input$my_category)
    })
    
    filtered <- reactive({
        # conditions when subcategory = TRUE and subsystem = FALSE
        if(input$subcategory == TRUE & input$subsystem == FALSE){
            return(
                filtered_cat() %>%
                    filter(Subcategory %in% input$my_subcategory)
            )}
        
        # conditions when subcategory = FASLE and subsystem = TRUE
        if(input$subcategory == FALSE & input$subsystem == TRUE){
            return(
                filtered_cat() %>%
                    filter(Subsystem %in% input$my_subsystem)
            )}

        # conditions when subcategory = TRUE and subsystem = TRUE
        if(input$subcategory == TRUE & input$subsystem == TRUE){
            return(
                filtered_cat() %>%
                    filter(Subcategory %in% input$my_subcategory) %>%
                    filter(Subsystem %in% input$my_subsystem)
            )}
        filtered_cat()
    })
    
    
    output$subcat <- renderUI({
        conditionalPanel(condition = "input.subcategory == true",
                         pickerInput("my_subcategory", "Select Gene Subcategories:", 
                                     choices = unique(filtered_cat()$Subcategory), 
                                     selected = filtered_cat()$Subcategory,
                                     options = list(`actions-box` = TRUE),
                                     multiple = TRUE)
                         )
        })
    
    output$subsys <- renderUI({
        conditionalPanel(condition = "input.subsystem == true",
                         pickerInput("my_subsystem", "Select Gene Subsystems:", 
                                     choices = filtered_cat() %>% filter(Subcategory %in% input$my_subcategory) %>% select(Subsystem) %>% unique() %>% pull, 
                                     selected = filtered_cat() %>% filter(Subcategory %in% input$my_subcategory) %>% select(Subsystem) %>% unique() %>% pull,
                                     options = list(`actions-box` = TRUE),
                                     multiple = TRUE)
                         )
        })
    
    # Create actual plot and save it into a reactive variable
    p <- reactive({
        filtered() %>%
            ggplot(aes(x = strain, y = Subsystem, fill = value, text = text)) +
            geom_tile(color = "black") + # geom_tile is used to make heatmaps
            scale_fill_gradient(low = input$low, high = input$high) +  # Selects the minimum and maximum color for the cells
            theme(axis.text.x = element_text(angle = 90)) # rotate x labels by 90 degrees
    })
    
    # render plot into plotly -> to make it more interactive!
    output$my_heatmap <- renderPlotly({
        ggplotly(p(), tooltip = "text") %>%
            style(xgap = 1, ygap = 1)
        
    })
    
    # render plot into plotly -> to make it more interactive!
    output$copy_heatmap <- renderPlotly({
        ggplotly(p(), tooltip = "text") %>%
            style(xgap = 1, ygap = 1) %>%
            layout(autosize = F, height = input$plot_height, width = input$plot_width)
        
    })
    
    
    # downloads heatmap ggplot into a '.png'
    output$download_plot <- downloadHandler(
        filename = function() {
            paste('heatmap-', Sys.Date(), '.png', sep='')
        },
        content = function(file) {
            ggsave(filename = file, plot = p(), device = "png")
        }
    )
    
    
    
    # create reactive variable for table, botht to render it and to download it
    t <- reactive({
        filtered() %>%
            mutate(value = as.integer(value)) %>%
            select(-text) %>% # need to take out the text for the interactive heatmap - it messed up the 'pivot_wider'
            pivot_wider(names_from = strain, values_from = value) %>%
            select(-Category, -Subcategory) # This column is redundant
    })
    
    # output table with value for each cell below the heatmap. since we had to change
    # the dat table to be in tidy format, we are reverting it to the untidy, but more readable format.
    output$my_table <- renderTable(
        t()
    )
    
    # download table
    output$download_table <- downloadHandler(
        filename = function() {
            paste('table-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
            write_csv(t(), file = file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
