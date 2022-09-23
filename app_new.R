library(tidyverse)
library(shiny)
library(readxl)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(bslib)
library(plotly)
library(DT)
library(shiny)
library(RColorBrewer)
library(data.table)


# ------- generals -------- # 

# theme ----
light <- bs_theme(bootswatch = "simplex")
dark <- bs_theme(bootswatch = "simplex", bg = "black", fg = "white", primary = "purple")


# sample ----
sample_file = read_xlsx("sample2.xlsx")



# ui ----
ui <- navbarPage("주차센서 대시보드", 
                 theme = light,
                 tabPanel("IoT Sensor Data",
                          sidebarLayout(
                            ## sidebar ----
                            sidebarPanel( width = 3,
                                        
                                        ### select_file ----
                                        radioGroupButtons(
                                          inputId = "select_file",
                                          label = "분석 파일 선택하기",
                                          choices = c(
                                            "샘플 파일" = "default",
                                            "사용자 파일" = "file"),
                                          justified = T
                                        ),
                                        
                                        conditionalPanel(
                                          condition = "input.select_file== 'file'",
                                          fileInput("raw_data",
                                                    "사용자 데이터 업로드",
                                                    accept = ".xlsx",
                                                    buttonLabel = "찾아보기",
                                                    placeholder = ".xlsx"),
                                          
                                          actionBttn("file_refresh",
                                                     "새로고침",
                                                     icon = "table",
                                                     style = "material-flat",
                                                     size = "xs"),
                                          ),
                                          
                                        ### select_date_range ----
                                        uiOutput("select_date_range"),
                                       
                                        
                                        ### select_period ----
                                        radioGroupButtons("select_period",
                                                          "분석 파일 선택하기",
                                                          choices = c(
                                                            "일간" = "d",
                                                            "주간" = "w",
                                                            "월간" = "m"),
                                                          justified = T),
                                        
                                        ###  select_location ----
                                        radioGroupButtons("select_location",
                                                          "공간 범위 선택",
                                                          choices = c(
                                                            "전체" = "a",
                                                            "위치별" = "l"),
                                                          justified = T),
                                        
                                        ###  select_stat ---- 
                                        radioGroupButtons("select_stat",
                                                          "분석 단위 선택",
                                                          choices = c(
                                                            "이용특성" = "u",
                                                            "오류" = "e"),
                                                          justified = T),
                                        
                                        tags$span("\n"),
                                        hr(),
                                        
                                        # dark mode checker 
                                        materialSwitch(inputId = "dark_mode",
                                                       label = h6("Dark Mode"),
                                                       status = "info"),
                            ),
                            ## mainpanel ----
                            mainPanel(
                              conditionalPanel( condition = "input.select_stat== 'u'",
                                         fluidRow(
                                           column(8,
                                                  h6("이용률")
                                           ),
                                           column(4,
                                                  h6("이용률 상위 주차면")
                                           )
                                         ), # fluid row 1
                                         
                                         fluidRow(
                                           column(8,
                                                  h6("일간 이용률"),
                                           ),
                                           column(4,
                                                  h6("이용률 상위 주차면"),
                                                  
                                           )
                                         ), # fluid row 2
                                         
                                         
                                         ### heatmap ----
                                         fluidRow(
                                           h6("히트맵"),
                                           plotlyOutput("plot_share")
                                         ), # fluid row 3

                              ), # conditional 1 : 이용특성 
                              
                              # error 요약 
                              conditionalPanel(condition = "input.select_stat== 'e'",
                                               
                                               # 에러 발생 요약
                                               fluidRow(
                                                 column(8,
                                                        h6("에러 발생률")
                                                 ),
                                                 column(4,
                                                        h6("이용률 상위 주차면")
                                                 )
                                               ), # fluid row 1
                                               
                                               fluidRow(
                                                 column(8,
                                                        h6("일간 이용률"),
                                                 ),
                                                 column(4,
                                                        h6("이용률 상위 주차면"),
                                                        
                                                 )
                                               ), # fluid row 2
                                               
                                               
                                               fluidRow(
                                                 h6("히트맵")
                                                 
                                               ), # fluid row 3
                                               
                                               
                       ) # conditional 2 
                 ) # main panel
                 ), # side layout
                 ), # tab1
                 
                 ## dt output ----
                tabPanel("RAW Data",
                         h6("Raw data"),
                         dataTableOutput("share_dt")
                         ) # tab - raw data
)

#server ----
server <- function(input, output, session) {
  
  # Dark mode 
  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode)) dark else light
  ))
  
  # 01 data.tables -----------------
  ## rare ----
  rare <- reactive({
    input$file_refresh
    isolate({
      withProgress({
        setProgress(message = "파일 전처리 중...")
        
        # read file 
        if(input$select_file == "default") {
          data = sample_file 
        } else {
          x = input$raw_data
          ext = tools::file_ext(x$datapath)
          req(x)
          validate(need(ext == "xlsx","Error : 정확한 파일을 업로드하세요."))
          data = read_xlsx(x$datapath) }
        
        # preprocessing to rare 
        data = as.data.table(data)
        
        cols_ = c("장치구분", "검지기", "배터리", "장애", "RSSI", "SMR", "동작")
        data = data[, .(센서, 센서값, 일시 = `송신일시(sendDate)`)
          
          # 텍스트 값 trim 
        ][,센서값 := str_remove_all(센서값, "[[a-zA-Z]|[가-힣]|[\\s]]+: ")
        ][,센서 := str_remove_all(센서, "[주차센서_|졸음쉼터]")
        ][, 졸음쉼터 := str_sub(센서,1,2)  
          
          # 값 분절하기 
        ][,(cols_) := tstrsplit(센서값, ",")
        ][,검지기 := str_remove_all(검지기, "차량")
          
          # 동작 범주화 
        ][검지기 != "초기부팅", 동작 := fcase(
          동작 %in% c("오류","2.0","3.0"), "통신오류",
          str_detect(동작,"LoRa"), "통신오류",
          str_detect(동작,"고장"), "센서오류",
          str_detect(동작,"주기"), "주기보고",
          default = "정상")
          
          # 정렬 및 중복 겵측치 제외 
        ][order(센서,일시)
        ][,오류구분 := fcase(
          센서 == shift(센서, type = 'lead') & 검지기 == "주차" & shift(검지기,type = 'lead') != "출차", 3, # 주차 이후 출차 아닌것 
          센서 == shift(센서, type = 'lag') & 검지기 == "출차" & shift(검지기,type = 'lag') != "주차", 3, # 출차 이전 주차 아닌것
          검지기 == "주차" & 센서 != shift(센서, type = "lead"), 3,
          검지기 == "출차" & 센서 != shift(센서, type = "lag"), 3,
          str_detect(동작,"오류"), 2, # 센서오류나 통신오류 인것 
          동작 == "주기보고", 1, # 주기보고 
          default = 0) # 정상 
          
        ][,.(졸음쉼터, 센서, 검지기, 일시, 오류구분)]
        
      })
      
    })
    showNotification("파일 전처리 완료!")
    return(data)
  })
  
  ### rare_test ----
  output$rare_dt <- renderDataTable({
    rare()
  })
  
  ### min, max date range ----
  date_range <- reactive({
    data = rare()
    min = data$일시 %>% min()
    max = data$일시 %>% max()
    return(list(min, max))
  })
  
  ### ui output date ----
  output$select_date_range <- renderUI({
    data = date_range()
    start = data[[1]] %>% as_date()
    end = data[[2]] %>% as_date()
    
    dateRangeInput("select_date_range",
                   "분석 범위 선택",
                   start = start,
                   end = end,
                   min = start,
                   max = end,
                   format = "yyyy-mm-dd",
                   language = "kr",
                   separator = ":",
                   weekstart = 1)
  })
  
  ## error  -----
  error <- reactive({
    data = rare()
    
    data[,.(정상 = sum(오류구분 == 0),
            주기보고 = sum(오류구분 == 1),
            센서_기기오류 = sum(오류구분 == 2),
            중복오류 = sum(오류구분 == 3)),
         by = .(센서, 일시)]
  })
  
  ### error_ ----
  error_ <- reactive({
    data = error()
    
    min = input$select_date_range[1] %>% as_date()
    max = input$select_date_range[2] %>% as_date()
    
    data[as_date(일시) <= max & as_date(일시) >= min]
  })
  
  ### error_test ----
  output$error_dt <- renderDataTable({
    data = error_() %>% 
      DT::datatable(
        filter = 'top',
        escape = FALSE, 
        selection = 'none', 
        rownames = FALSE,
        options = list(
          pageLength =20
        )
    )
  })
  
  
  ## event ------
  event <- reactive({
    data = rare()
    
    data[오류구분 == 0
        ][,출차시각 := fifelse(검지기 == "주차", shift(일시,type = "lead"),"0")
        ][출차시각 != "0"
        ][,c("주차시각", "출차시각") := lapply(.SD, as_datetime), .SDcols = c("일시", "출차시각") 
        ][,체류시간 := as.numeric(출차시각 - 주차시각)
        ][,.(센서, 주차시각, 출차시각, 체류시간)]
    
  })
  
  ### event_  ------
  event_ <- reactive({
    data = event()
    
    min = input$select_date_range[1] %>% as_date()
    max = input$select_date_range[2] %>% as_date()
    
    data[as_date(주차시각) <= max & as_date(주차시각) >= min]
  })
  
  ### event_test  ------
  output$event_dt <- renderDataTable({
    event()
  })
  
  ## share  -----------------
  share <- reactive({
    data = event()
    
    data = data %>% mutate(t = as.numeric(trunc.POSIXt(출차시각) - trunc.POSIXt(주차시각))/3600 + 1)
    
    share = data %>% map_df(rep, times = data$t) %>% data.table() 
    
    share = share[,t := (sequence(.N)-1), by = .(센서, 주차시각)
    ][,시작시각 := trunc.POSIXt(주차시각,"hours") + 3600*t
    ][,점유시간 := fcase(
      
      # 시작 끝 시간 같으면 그냥 넣기 
      trunc.POSIXt(주차시각,"hours") == trunc.POSIXt(출차시각,"hours"), 체류시간,
      
      # 주차 == 시작 
      t == 0, as.numeric(ceiling_date(주차시각,"hours") - 주차시각),
      t != 0 & trunc.POSIXt(출차시각,'hours') > 시작시각, 3600, 
      
      trunc.POSIXt(출차시각,'hours') == 시작시각, as.numeric(출차시각 - 시작시각),
      default = 0)
      
    ][,.(센서, 시작시각, 점유시간)
    ][,.(점유율 = round(sum(점유시간)/3600*100, digits = 2)), by = .(센서, 시간 = 시작시각)] 
  })
  
  
  ### share_ -----------------
  share_ <- reactive({
    data = share()
    
    min = input$select_date_range[1] %>% as_date()
    max = input$select_date_range[2] %>% as_date()
    
    data[as_date(시간) <= max & as_date(시간) >= min]
  })
  
  ### share_test -----------------
  output$share_dt <- renderDataTable({
    share_()
  })
  
  
  
  # 02 Graph -----
  
  ##  plot_share ----
  output$plot_share <- renderPlotly({
    data = share_() %>% tibble() 
    
    # 기간별 
    if(input$select_period == "d"){
      p = data %>%
        group_by(졸음쉼터 = str_sub(센서,1,2), 기간 = as_date(시간)) %>% 
        summarise(이용률 = mean(점유율))
    } else if(input$select_period == "w"){
      p = data %>%
        group_by(졸음쉼터 = str_sub(센서,1,2), 기간 = weekdays(as_date(시간))) %>% 
        summarise(이용률 = mean(점유율))
    } else {
      p = data %>%
        group_by(졸음쉼터 = str_sub(센서,1,2), 기간 = months(as_date(시간))) %>% 
        summarise(이용률 = mean(점유율))
    }
    
    # 위치별 
    if(input$select_location == "a"){
      p = p %>% 
        group_by(기간) %>% 
        summarise(이용률 = mean(이용률))%>%
        plot_ly() %>% 
        add_trace(type = 'scatter', mode = 'lines',
                  line = list(shape = "spline"),
                  text = ~paste0(기간, "\n" ,이용률, "%"),
                  hoverinfo = text,
                  textposition = "inside",
                  x = ~기간, y = ~이용률) 
    } else {
      p = p %>%
        plot_ly() %>% 
        add_trace(type = 'scatter', mode = 'lines',
                  line = list(shape = "spline"),
                  x = ~기간, y = ~이용률,
                  text = ~paste0(기간, "\n" ,이용률, "%"),
                  hoverinfo = text,
                  textposition = "inside",
                  fill = ~졸음쉼터, color = ~졸음쉼터) 
    }
      
  })
  
}

shinyApp(ui = ui, server = server)
