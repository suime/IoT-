# global
library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(bslib)

options(repos = c(CRAN = "http://cran.rstudio.com"))
options(shiny.maxRequestSize=200*1024^2) ## 파일 업로드 제한을 200



ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  sidebarLayout(
    position = "right",
   
    sidebarPanel(
     h6("IoT 센서"),
     fileInput("sensor",
               label = "",
               accept = ".xlsx",
               buttonLabel = "찾아보기",
               placeholder = ".xlsx file"
               ),
     textAreaInput("datelist",
                   "공휴일 입력칸",
                   value = ""
                   )
   ), #side
   mainPanel(
     h5(" 주차센서 확인 "),
     tabsetPanel(
       tabPanel("tab1",
        h6(" data table "),
         
       ),
       tabPanel("tab2",
        dataTableOutput("sensor.dt")
         
       ),
     )
   ) # main
  )
)

server <- function(input, output) {
  sensor. <- reactive({
    x = input$sensor
    ext = tools::file_ext(x$datapath)
    req(x)
    validate(need(ext == "xlsx","Error : 정확한 파일을 업로드하세요."))
    return(read_xlsx(x$datapath))
  })
  
  output$sensor.dt <- renderDataTable({
    x = sensor.()
    x %>% rename(일시 = `송신일시(sendDate)`) %>% 
      select(c(센서, 센서값, 일시)) %>% 
      mutate(센서값 = str_remove_all(센서값, "[[a-zA-Z]|[가-힣]|[\\s]]+: ")) %>% 
      separate(센서값, sep = ",", into = c("장치구분", "검지기", "배터리", "장애", "RSSI", "SMR", "동작"), convert = T, extra = "merge")
  })
}

shinyApp(ui = ui, server = server)
