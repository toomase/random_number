library(shiny)
library(yonder)
library(tidyverse)
library(hrbrthemes)

# Kuna vaikimisi on ggplot graafikute tekstid liiga väikesed, siis tekita eraldi element, 
# mis tuleb igale graafikule juurde liita
theme_text_increase <- theme(
    axis.text.x = element_text(size = rel(4)),
    axis.text.y = element_text(size = rel(4)),
    axis.title.x = element_text(size = rel(4)),
    axis.title.y = element_text(size = rel(4)),
    plot.title = element_text(size = 50),
)


ui <- fluidPage(

    navbarPage(
        title = 'Juhuslik number',
        tabPanel('Juhuslik 1',
                 actionButton("uuenda_1",
                              "uuenda"),
                 
                 plotOutput("random_1",
                            height = "800px")),
        tabPanel('Juhuslik 2',
                 actionButton("uuenda_2",
                              "uuenda"),
                 
                 plotOutput("random_2",
                            height = "800px")))
)

server <- function(input, output) {

    df_1 <- eventReactive(
        
        req(input$uuenda_1), {
            map_df(list.files("~/Dropbox/DataScience/R/random_number/data/random_1/",
                               full.names = TRUE), read_rds) 
        }
    )
    
    output$random_1 <- renderPlot({
        
        df_1() %>% 
            count(value) %>% 
            ggplot(aes(as.factor(value), n)) +
            geom_col(fill = "#2b8cbe") +
            geom_text(aes(label = n), vjust = -0.3, size = 10) +
            theme_ipsum_rc() +
            labs(title = str_c(length(df_1()$value), " valitud numbrit ja ",
                               n_distinct(df_1()$session_id), " vastajat"),
                 x = "",
                 y = "vastuseid") +
            theme_text_increase
    })
    
    df_2 <- eventReactive(
        
        req(input$uuenda_2), {
            map_df(list.files("~/Dropbox/DataScience/R/random_number/data/random_2/",
                               full.names = TRUE), read_rds) 
        }
    )
    
    output$random_2 <- renderPlot({
        
        df_2() %>% 
            count(value) %>% 
            ggplot(aes(as.factor(value), n)) +
            geom_col(fill = "#de2d26") +
            geom_text(aes(label = n), vjust = -0.3, size = 10) +
            theme_ipsum_rc() +
            labs(title = str_c(length(df_2()$value), " valitud numbrit ja ",
                               n_distinct(df_2()$session_id), " vastajat"),
                 x = "",
                 y = "vastuseid") +
            theme_text_increase
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
