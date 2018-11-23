library(shiny)
library(yonder)
library(tidyverse)
library(shinyalert)


# Funktsioon salvestab tulemuse rds failina
save_data <- function(data) {
    write_rds(x = data, 
              path = file.path("~/Dropbox/DataScience/R/random_number/data/random_1", 
                               str_c(as.integer(Sys.time()), "_",
                                     round(runif(n = 1, 1000, 9999), 0),  ".rds")))
}



ui <- fluidPage(
    
    # skript kasutaja IP aadressi pärimiseks
    tags$head(
        tags$script(src = "getIP.js")
    ),
    
    # useShinyalert(),
    
    # genereeri 10 nuppu
    div(
        map(seq(1, 10), function(x){
            buttonInput(id = str_c("b", as.character(x)),
                        label = x,
                        block = TRUE,
                        style = 'padding:8px; font-size:300%') %>% 
                background("blue")
        })
    )
)       
        

server <- function(input, output) {
    
    # kasutaja IP-aadress
    # https://stackoverflow.com/questions/43888099/get-user-ip-in-shiny
    IP <- reactive({input$getIP})
    
    # genereeri sessiooni id, mis on ühele sessioonile unikaalne
    # https://stackoverflow.com/questions/49729733/is-there-a-unique-session-id-session-key-assigned-to-each-shinyr-session
    session_id <- reactive({ as.character(floor(runif(1) * 1000000)) })
    
    # salvesta nupuvajutuse põhjal number ja kuva teade
    map(seq(1, 10), function(x){
        observeEvent(input[[str_c("b", x)]], {
            if (input[[str_c("b", x)]] > 0) {
                save_data(tibble(value = as.integer(x),
                                 ip = IP()$ip,
                                 session_id = session_id()))
            }
            
            # shinyalert(title = "AITÄH",
            #            type = "success",
            #            closeOnClickOutside = FALSE,
            #            confirmButtonText = "UUESTI",
            #            confirmButtonCol = "#95A5A5")
        })
    })
    
}


shinyApp(ui = ui, server = server)