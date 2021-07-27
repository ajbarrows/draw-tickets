library(shiny)
library(dplyr)

example <- read.csv("example.csv")

random_draw <- function(tickets, num_winners) {
    # Randomly sample from list of subject IDs weighted by the 
    # umber of tickets each subject has. If fewer than 7 subjects,
    # sample with replacement
    
    df <- tickets %>% 
        group_by(subject_id) %>%
        summarize(weights = sum(num_tickets, na.rm = TRUE))
    
    if (nrow(df) >= num_winners) {
       draw <- df %>%
           slice_sample(n = num_winners, weight_by = weights, replace = FALSE)
    } else if(nrow(df) < num_winners) {
        draw <- df %>%
            slice_sample(n = num_winners, weight_by = weights, replace = TRUE)
    }
    
    draw %>% select(subject_id)
}

ui <- fluidPage(

    titlePanel("Variable Incentive Program Drawing"),
    h5("Randomly sample from list of subject IDs weighted 
    by the number of tickets each subject has. 
    If fewer subjects than the selected number of winners are given, 
    sample with replacement."),
    br(),
    
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = "infile", 
                      label = "Upload `tickets.csv`. It should be exactly like
              the example.",
                      accept = ".csv"),
            downloadButton("downloadExample", "Example"),
        ),
    mainPanel(
        numericInput(
            inputId = "num_winners",
            label = "Number of Winners",
            value = 7,
            width = '100px'
        ),
        actionButton(
            inputId = "draw",
            label = "Draw Tickets"
        ),
        tableOutput(
            outputId = "winner_ids"
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    v <- reactiveValues(loaded = FALSE) # switch
    output$downloadExample <- downloadHandler(
        filename = "tickets.csv",
        content = function(file) {
            write.csv(example, file, row.names = FALSE)
        }
    )
    num_winners <- reactive(req(input$num_winners))
    tickets <- reactive({
        file <- input$infile
        req(file)
        ext <- tools::file_ext(file$datapath)
        
 
        validate(need(ext == "csv", "Please upload a csv file"))
        
        read.csv(file$datapath)
    })
    
    observeEvent(input$draw, {
        # flip switch
        v$loaded <- TRUE
    })

    output$winner_ids <- renderTable({
        if (v$loaded) {
            tickets() %>% random_draw(num_winners())
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
