library(shiny)
library(tidyverse)
library(plotly) # to make interactive heatmap
library(shinydashboard) # to create a new aesthetic for the app
library(dashboardthemes) # themes for shinydashboard
library(shinyWidgets) # diffeerent widgets with cool functionalities

# Assignment B-4: update shinny app. Find comments of the different features in the code below!

# Load in the data from the .csv file, arrange it into a tify format using pivot_longer, and 
# add a 'text' column that will create the interactive pop-up in plotly when you hover through the heatmap cells
dat <- read_csv(here::here("S24_7_data.csv")) %>% 
    pivot_longer(cols = starts_with("M_"), names_to = "strain") %>%
    mutate(text = paste0("Strain: ", strain, "\n", "Row: ", Subsystem, "\n", "Value: ",round(value,0)))

# we use dashboardPage instead of fluid page
ui <- dashboardPage(
    
    # Adds a titel to the header bar of the app
    dashboardHeader(title = "M. intestinale Strains Heatmaps"),
    
    # The side bar menu. Here you will have two tabs: 1) heatmap and 2) a table of the heatmap values
    dashboardSidebar(
        
        # creating tabs for the app: add name, an identifying tabName, and an Icon!
        sidebarMenu(
            menuItem("Heatmap", tabName = "heatmap", icon = icon("fas fa-chart-bar")),
            menuItem("Table", icon = icon("fas fa-table"), tabName = "table")
            )
        ),
    
    # dashboard body is where the heatmap and widgets will be included
    dashboardBody(
        
        # Set a theme using dashboardthemes
        shinyDashboardThemes(theme = "grey_light"),
    
        # Because we have two tabs, we need to use tabItems to refere to the different tabs
        tabItems(
            ################# This is the tab for Heatmap #####################
            
            # This tabItem is where the heamap will be shown - many widgets and features are also here
            tabItem(tabName = "heatmap",
                    
                    # add a title and description
                    h2("M. intestinale strain heatmaps"),
                    strong("About:"), "In this shiny app you can compare the gene subsystems present
                    in different M. instestinale strains. Annotations of the genomes were done using the
                    rapid annotation using subsystem technology (RAST) web server (https://rast.nmpdr.org/).
                    To interact with the app, follow steps (1-5) in each of the boxes to update the different
                    gene subsystems projected in the heatmap. The heatmap is also an interactive plotly plot, 
                    so you can zoom-in/out and see the value of each cell. If you also want a table of the heatmap
                    values, you can switch to the 'Table' tab in the sidebar menu.",
                    br(),
                    
                    # In this fluid row we have the heatmap and some features
                    fluidRow(
                        
                        # box containing the plot!
                        box(plotlyOutput("my_heatmap"), status = "warning", width = 6),
                        
                        # columns containing boxes with different filtering widgets
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
                               
                               # box to select the gene category that you want to visualize!
                               box(title = "2) Select gene category", status = "primary",
                                   pickerInput("my_category", "Categories",
                                               choices = unique(dat$Category), 
                                               selected = "Stress Response", 
                                               options = list(style = "btn-primary"))
                                   ),
                               
                               # reactive switch that will allow the user to further filter 
                               # the data to only include certain subcategories or subsystems
                               box(title = "3) Filter by other gene groupings:", status = "primary",
                                   strong("Filter by:"),
                                   # switch for Subcategory filtering (When TRUE, it rendered 'subcat' UI)
                                   materialSwitch(inputId = "subcategory", label = "Subcategory",
                                                  value = FALSE, status = "primary"),
                                   
                                   # Switch for Subsystem filtering (When TRUE, it renders 'subsys' UI)
                                   materialSwitch(inputId = "subsystem", label = "Subsystem",
                                                  value = FALSE, status = "primary"),
                                   
                                   # dynamic UIs that appear based on the above 'materialSwitch'
                                   uiOutput("subcat"),
                                   uiOutput("subsys")
                                   )
                               )
                        ),
                    # This is a new row with other features. Row is placed below the heatmap
                    
                    # box to change color of heatmap
                    box(title = "4) Change Heatmap colors", status = "primary",
                        "Note: Click checkmark icon to update or hit Enter",
                        
                        # This allows the user to insert text and update the lowest value color for the heatmap
                        searchInput(
                            inputId = "low",
                            label = "Lowest value color:",
                            value = "gray98",
                            placeholder = "Type acceptable R color...",
                            btnSearch = icon("check"), 
                            btnReset = icon("remove"),
                            width = "50%"),
                        
                        # This allows the user to insert text and update the highest value color for the heatmap
                        searchInput(
                            inputId = "high",
                            label = "Maximum value color:",
                            value = "black",
                            placeholder = "Type acceptable R color...",
                            btnSearch = icon("check"), 
                            btnReset = icon("remove"),
                            width = "50%")
                        ),
                    
                    # box to save plot -> Contains a download buttom feature!
                    box(title = "5) Save the plot", status = "primary",
                        downloadBttn(outputId = "download_plot",
                                     style = "jelly",
                                     color = "primary")
                        )
            ),
            
            ############## This is the tab for the table of values #############
            tabItem(tabName = "table",
                    h2("Table of heatmap values"),
                    fluidRow(
                        # box where the table will be rendered, added a way to scroll horizontally!
                        box(status = "warning", width = 8,
                            tableOutput("my_table"),
                            style='overflow-x: scroll'
                            ),
                        # download buttom that will allow the user to the dowload the table in .csv format
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


server <- function(input, output) {

    # store the initial data that will be reactively change based on the strains picked and the category selected
    filtered_cat <- reactive({
        dat %>%
            filter(strain %in% input$my_choices) %>%
            filter(Category == input$my_category)
    })
    
    # This reactive variable contains the different possibilities of the final filtered data if the user decides
    # to further filter based on 'Subcategory' or 'Subsystem'
    filtered <- reactive({
        
        # condition when subcategory = TRUE and subsystem = FALSE
        if(input$subcategory == TRUE & input$subsystem == FALSE){
            return(
                filtered_cat() %>%
                    filter(Subcategory %in% input$my_subcategory)
            )}
        
        # condition when subcategory = FASLE and subsystem = TRUE
        if(input$subcategory == FALSE & input$subsystem == TRUE){
            return(
                filtered_cat() %>%
                    filter(Subsystem %in% input$my_subsystem)
            )}

        # condition when subcategory = TRUE and subsystem = TRUE
        if(input$subcategory == TRUE & input$subsystem == TRUE){
            return(
                filtered_cat() %>%
                    filter(Subcategory %in% input$my_subcategory) %>%
                    filter(Subsystem %in% input$my_subsystem)
            )}
        
        # If no condition is met, just return filtered_cat
        filtered_cat()
    })
    
    
    # This is a dynamic UI. If the output of input$subcategory is switched to TRUE, then the conditional panel runs
    # and the UI is rendered
    output$subcat <- renderUI({
        conditionalPanel(condition = "input.subcategory == true",
                         pickerInput("my_subcategory", "Select Gene Subcategories:", 
                                     choices = unique(filtered_cat()$Subcategory), 
                                     selected = filtered_cat()$Subcategory,
                                     options = list(`actions-box` = TRUE),
                                     multiple = TRUE)
                         )
        })
    # This is a dynamic UI. If the output of input$subsystem is switched to TRUE, then the conditional panel runs
    # and the UI is rendered. Here, the choices are also updated based on the Subcategories picked!
    output$subsys <- renderUI({
        conditionalPanel(condition = "input.subsystem == true",
                         pickerInput("my_subsystem", "Select Gene Subsystems:", 
                                     choices = filtered_cat() %>% filter(Subcategory %in% input$my_subcategory) %>% select(Subsystem) %>% unique() %>% pull, 
                                     selected = filtered_cat() %>% filter(Subcategory %in% input$my_subcategory) %>% select(Subsystem) %>% unique() %>% pull,
                                     options = list(`actions-box` = TRUE),
                                     multiple = TRUE)
                         )
        })
    
    # Create heatmap plot with ggplot and store it in a reactive variable
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
            style(xgap = 1, ygap = 1) # I have to add this to have some space in-between cells!
        
    })
    
    
    # downloads heatmap ggplot into a '.png' file
    output$download_plot <- downloadHandler(
        filename = function() {
            paste('heatmap-', Sys.Date(), '.png', sep='')
        },
        content = function(file) {
            ggsave(filename = file, plot = p(), device = "png")
        }
    )
    
    
    
    # create reactive variable for table, both to render it and to download it
    t <- reactive({
        filtered() %>%
            mutate(value = as.integer(value)) %>%
            select(-text) %>% # need to take out the text for the interactive heatmap - it messed up the 'pivot_wider'
            pivot_wider(names_from = strain, values_from = value) %>%
            select(-Category, -Subcategory) # This column are redundant
    })
    
    # output table with value for each cell below the heatmap. since we had to change
    # the dat table to be in tidy format, we are reverting it to the untidy, but more readable format.
    output$my_table <- renderTable(
        t()
    )
    
    # download table into a .csv file!
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
