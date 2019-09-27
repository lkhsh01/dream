
library(shiny)
library(ggplot2)  # for the diamonds dataset

source("./utils.R", encoding="utf-8")

Sys.setlocale("LC_ALL", "korean")

k_list <- c("ALL","ㄱ","ㄴ","ㄷ","ㄹ","ㅁ","ㅂ","ㅅ","ㅅ","ㅈ","ㅊ","ㅋ","ㅌ","ㅍ","ㅎ")

ui <- fluidPage(
  #title = "Examples of DataTables", 
  headerPanel("드림이 이름 찾기 v1.0"),
  mainPanel(
    tabsetPanel(
       id = 'dataset',
       tabPanel("이름통계", 
                h3(""),
                DT::dataTableOutput("mytable1")),
       tabPanel("자음분류",
                h3(""),
                selectizeInput(inputId = 'show_vars',label="자음선택", choices = k_list), 
                h3(""),
                DT::dataTableOutput("mytable2"))
     )
   )
)

server <- function(input, output) {
  output$mytable1 <- DT::renderDataTable(DT::datatable(
    get_name_stat(input$show_vars),
    options = list(sDom = 'lrtip'),
    rownames = FALSE
  ))
  
  output$mytable2 <- DT::renderDataTable(DT::datatable(
    get_name_cat(input$show_vars),
    options = list(sDom = 'lrtip'),
    rownames = FALSE
  ))
}

shinyApp(ui, server) 

## runGitHub('dream', 'lkhsh01')