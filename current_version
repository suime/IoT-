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

colfunc <- colorRampPalette(c("red", "yellow","orange"))
colfunc_gyr = colorRampPalette(c("forestgreen", "yellow","orange","red"))
colors_list_30 <- colfunc(30)
colors_list_5 <- colfunc(10)

options(shiny.maxRequestSize=200*1024^2) ## 파일 업로드 제한을 200

holiday_list = c("2022-01-01", "2022-01-31", "2022-02-01", "2022-02-02",
                 "2022-03-01", "2022-05-05", "2022-05-08", "2022-06-06",
                 "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11",
                 "2022-09-12", "2022-10-03", "2022-10-09", "2022-10-10", "2022-12-25") %>% 
  ymd()

week.name.ko = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
def_data = read_xlsx("def.xlsx")



ui <- navbarPage("주차센서 대시보드", 
                 theme = bs_theme(bootswatch = "flatly"),
                 
                 ## 
                 ## 00. 파일 업로드 
                 tabPanel("파일 읽기",
                          icon = icon("cloud-upload-alt"),
                          sidebarLayout(
                            sidebarPanel(
                              width = 4,
                              radioGroupButtons(
                                inputId = "source",
                                label = "분석 파일 선택하기",
                                choices = c(
                                  "샘플 파일" = "def",
                                  "사용자 파일" = "file"),
                                justified = T
                              ),
                              
                              conditionalPanel(
                                condition = "input.source == 'file'",
                                fileInput("raw_data",
                                          "주차센서 데이터 업로드",
                                          accept = ".xlsx",
                                          buttonLabel = "찾아보기",
                                          placeholder = ".xlsx"),
                                
                              ),
                              fluidRow(
                                width = '100%',
                                actionBttn("file_upload",
                                           label = "새로고침",
                                           icon = icon("sync"),
                                           style = "fill",
                                           no_outline = F,
                                           color = "primary",
                                           size = "sm"),
                                hr()),
                              
                            ),
                            mainPanel(
                              position = "left",
                              dataTableOutput("rare_dt")
                            )
                          ) # sidebar
                 ),#tab
                 
                 
                 ## 01. 일간 이용 현황분석 
                 tabPanel("일간 이용현황 분석",
                          icon = icon("calendar"),
                          sidebarLayout(
                            # 일자 및 옵션 선택 
                            sidebarPanel(width = 5,
                                         fluidRow(
                                           uiOutput("date_range"), 
                                           
                                           hr()
                                         ),
                                         # 이용건 수 등 요약 부분 들어갈 파트 
                                         fluidRow(
                                           #dataTableOutput("event_summary_daily"),
                                         ),
                                         
                                         # 위치별 그래프 들어갈 부분 
                                         fluidRow(
                                           selectInput(inputId = "select_stat_type",
                                                       label = h5("위치별 이용현황"),
                                                       choices = c("이용건수" = "count",
                                                                   "점유시간" = "time",
                                                                   "이용률" = "share"),
                                                       selected = "count",
                                                       width = 400),
                                           plotlyOutput("hist_daily_a")
                                         ),
                            ),
                            
                            # 그래프 들어갈 부분 
                            mainPanel(position = "right",
                                      width = 7,
                                      radioGroupButtons("stat_type",
                                                        label = "분석 단위",
                                                        choices = c(
                                                          "전체" = "all",
                                                          "위치별" = "loc",
                                                          "히트맵" = "heatmap"),
                                                        justified = T,
                                                        width = "100%"),
                                      
                                      conditionalPanel(
                                        condition = "input.stat_type != 'heatmap'",
                                        h6("일간 평균 이용률"),
                                        plotlyOutput("share_plot_daily", width = "100%", height = "230px"),
                                        h6("일간 이용건수"),
                                        plotlyOutput("count_plot_daily", width = "100%", height = "230px"),
                                        h6("일간 평균 체류시간"),
                                        plotlyOutput("time_plot_daily", width = "100%", height = "230px"),
                                      ),
                                      conditionalPanel(
                                        condition = "input.stat_type == 'heatmap'",
                                        plotlyOutput("heatmap_daily", width = "100%", height = "600px")
                                      ),
                            ), # main 
                            
                          )#side
                 ),#tab
                 
                 
                 ## 02. 주간 이용 현황분석 
                 tabPanel("주간 이용현황 분석",
                          icon = icon("calendar-week"),
                          sidebarLayout(
                            # 일자 및 옵션 선택 
                            sidebarPanel(width = 5,
                                         fluidRow(
                                           uiOutput("date_range_w"), 
                                           
                                           hr()
                                         ),
                                         # 이용건 수 등 요약 부분 들어갈 파트 
                                         fluidRow(
                                         ),
                                         
                                         # 위치별 그래프 들어갈 부분 
                                         fluidRow(
                                         ),
                            ),
                            
                            # 그래프 들어갈 부분 
                            mainPanel(position = "right",
                                      width = 7,
                                      radioGroupButtons("stat_type_week",
                                                        label = "분석 단위",
                                                        choices = c(
                                                          "전체" = "all",
                                                          "위치별" = "loc"),
                                                        justified = T,
                                                        width = "100%"),
                                      
                                      conditionalPanel(
                                        condition = "input.stat_type_w != 'heatmap'",
                                        h6("주간 평균 이용률"),
                                        plotlyOutput("share_plot_week", width = "100%", height = "230px"),
                                        h6("주간 이용건수"),
                                        plotlyOutput("count_plot_week", width = "100%", height = "230px"),
                                        h6("주간 평균 체류시간"),
                                        plotlyOutput("time_plot_week", width = "100%", height = "230px"),
                                      ),
                                      conditionalPanel(
                                        condition = "input.stat_type_w == 'heatmap'",
                                        plotlyOutput("heatmap_week", width = "100%", height = "600px")
                                      ),
                            ), # main 
                            
                          )#side
                 ),#tab
                 
                 ## 03. 오류 이용 현황분석 
                 tabPanel("오류 현황",
                          icon = icon("calendar-week"),
                          sidebarLayout(
                            sidebarPanel(width = 2,
                                         radioGroupButtons(
                                           inputId = "error_group",
                                           label = "범위 선택",
                                           choices = c(
                                             "주차쉼터별" = "1",
                                             "주차면별" = "2"),
                                           justified = T
                                         ),
                                         
                            ),
                            
                            mainPanel(
                              position = "left",
                              tableOutput("error_dt"),
                            )
                          ) # sidebar
                 )#tab
                 
                 
) # ui 




##
server <- function(input, output) {
  
  ## raw - rare 파일 
  
  rare_ <- reactive({
    input$file_upload
    isolate({
      withProgress({
        setProgress(message = "파일 전처리 중...")
        
        if(input$source == "def"){
          y = def_data
        } else {
          x = input$raw_data
          ext = tools::file_ext(x$datapath)
          req(x)
          validate(need(ext == "xlsx","Error : 정확한 파일을 업로드하세요."))
          y = read_xlsx(x$datapath)}
        
        # ----- 파일 읽고 나서  ----
        sensor.name = names(table(y$센서))
        
        y %>% rename(일시 = `송신일시(sendDate)`) %>% 
          select(c(센서, 센서값, 일시)) %>% 
          
          #센서값 열 분할 
          mutate(센서값 = str_remove_all(센서값, "[[a-zA-Z]|[가-힣]|[\\s]]+: "),
                    일시 = as_datetime(일시)) %>% 
          separate(센서값, 
                      sep = ",", 
                      into = c("장치구분", "검지기", "배터리", "장애", "RSSI", "SMR", "동작"), 
                      convert = T, 
                      extra = "merge") %>% 
          
          # 값 범주화 
          mutate(장애 = ifelse(장애 == "51.0", "리셋 발생", 장애),
                   동작 = case_when(
                     동작 %in% c("2.0","오류") ~ "LoRa Reset 수신 후 부팅",
                      동작 == "3.0" ~ "LoRa 전송오류 등으로 인한 자체 시스템 Reset",
                      TRUE ~ 동작
                   )) %>% 
          
          # 센서 이름에서 맨 앞의 주차센서 빼기 
          mutate(
            센서 = str_remove(센서, "^주차센서_"),
            센서 = str_remove(센서, "졸음쉼터."),
            졸음쉼터 = str_sub(센서,1,2)) %>% 
          
          # 중복, 결측치 제거 
          distinct() %>% 
          na.omit() %>% 
          
          select(졸음쉼터, 센서, 검지기, 동작, 일시) %>% 
          
          arrange(센서, 일시) %>% 
          
          mutate(
            
            오류구분 = case_when(
              동작 == "주차이벤트" ~ 0, 
              동작 == "주기보고" ~ 1,
              TRUE  ~ 2 ),
            
            오류구분 = case_when(
              # 주기보고 다음 발생한 오류 
              #lag(오류구분, default = 0) == 1  ~  3, 
              
              # 차량 주차 다음에 주기보고나 오류 메시지가 발생한 것 => 3 오류 처리 
              검지기 == "차량주차" & lead(오류구분) != 0 & 오류구분 == 0 ~ 3,
              
              # 주차이고 다음 항목도 주차인 것 => 3 
              센서 == lead(센서, default = last(sensor.name)) & 검지기 == "차량주차" &
                검지기 == lead(검지기, default = "차량주차")  & 오류구분 == 0 ~ 3,
              
              # 출차이고 이전 항목도 출차인 것 => 3 
              센서 == lag(센서, default = first(sensor.name)) & 검지기 == "차량출차" &
                검지기 == lag(검지기, default = "차량출차")   & 오류구분 == 0 ~ 3,
              
              
              센서 == lead(센서, default = last(sensor.name)) & month(as_datetime(일시)) != lead(month(as_datetime(일시))) & 검지기 == "차량주차"  ~ 3,
              센서 == lag(센서, default = first(sensor.name)) & month(as_datetime(일시)) != lag(month(as_datetime(일시))) & 검지기 == "차량출차"  ~ 3,
              
              # 처음 / 마지막 데이터인데 출차 / 주차 인 경우 이벤트 오류 => 3 
              센서 != lag(센서) & 검지기 == "차량출차" & 오류구분 == 0  ~ 3,
              센서 != lead(센서) & 검지기 == "차량주차" & 오류구분 == 0 ~ 3,
              
              
              TRUE ~ 오류구분
            ) 
          ) 
      })
    })
  })
  
  
  output$rare_dt <- renderDataTable({
    data = rare_()
    data %>%
      mutate(
        오류구분 = case_when(
          오류구분 == 3 ~ "중복이벤트",
          오류구분 == 2 ~ "통신 및 기기 오류",
          오류구분 == 1 ~ "주기보고",
          T ~ "정상"
        )
      )
  })
  
  
  
  
  
  
  # 00. 분석 기간 선택 
  date.range <- reactive({
    min = rare_()$일시 %>% as_datetime() %>% min()  
    max = rare_()$일시 %>% as_datetime() %>% max()
    return(list(min, max))
  })
  
  # 00. 센서이름 
  sensor.name <- reactive({
    data = rare_()
    names(table(data$센서))
  })
  
  # 01. 분석 기간 
  output$date_range <- renderText({
    
    a = date.range()[[1]]
    b = date.range()[[2]]
    
    paste0(a, " ~ ", b, "\n", diff.difftime(ymd(b), ymd(a), units = "days"), "일")
  }) 
  
  # 분석기간 동적 슬라이더 일간
  output$date_range <- renderUI({
    min = date.range()[[1]]
    max = date.range()[[2]]
    
    dateRangeInput("date_range_",
                   label = "분석기간 선택",
                   start = min,
                   end = max,
                   min = min,
                   max = max,
                   format = "yyyy-mm-dd",
                   language = "ko",
                   separator = "~"
    )
    
  })  
  
  # 분석기간 동적 슬라이더 주간
  output$date_range_w <- renderUI({
    min = date.range()[[1]]
    max = date.range()[[2]]
    
    dateRangeInput("date_range_w",
                   label = "분석기간 선택",
                   start = min,
                   end = max,
                   min = min,
                   max = max,
                   format = "yyyy-mm-dd",
                   language = "ko",
                   separator = "~"
    )
    
  })
  
  
  # 오류 발생 현황 요약 테이블 -----
  output$error_dt <- renderTable({
    data = rare_()
    y = input$error_group
    
    if(y == "2"){
      x = data
    } else {
      x = data %>% mutate(센서 = str_sub(센서, 1, 2))
    }
    
    x %>% 
      group_by(센서 = str_remove_all(센서,"졸음쉼터")) %>% 
      summarise(
        정상 = sum(동작 == "주차이벤트" & 오류구분 == 0), 
        
        # 주기보고 및 이후 첫번째 이벤트 오류 
        주기보고 = sum(오류구분 == 1), 
        
        # LoRa망 관련 오류 
        통신오류 = sum(str_detect(동작,"^LoRa") & 오류구분 == 2),
        
        #센서오류
        센서오류 = sum(str_detect(동작,"^레이더") & 오류구분 == 2),
        
        #초기부팅 
        초기부팅 = sum(동작 == "초기 부팅"),
        
        
        # 이벤트 오류 
        중복오류 = sum(오류구분 == 3),
        
        # 합계 
        합계 = sum(c_across(정상:중복오류))
      )
  })
  
  
  # 이벤트 테이블 ----
  event <- reactive({
    data = rare_()
    
    data %>% 
      filter(오류구분 == 0) %>% 
      
      # 출차시각 
      mutate(출차시각 = as_datetime(ifelse(검지기 == "차량주차" & 센서 == lead(센서), lead(일시), NA))) %>% 
      
      filter(!is.na(출차시각)) %>% 
      
      rename(주차시각 = 일시) %>% 
      
      mutate(
        점유시간 = 출차시각 - as_datetime(주차시각),
        요일 = weekdays(주차시각),
        기간구분 = ifelse(
          요일 %in% c("토요일", "일요일") | as_date(주차시각) %in% holiday_list, "휴일", "평일")
      ) %>% 
      
      select(졸음쉼터, 센서, 요일, 기간구분, 주차시각, 출차시각, 점유시간) 
    
    
  })
  
  
  ## event_date_filtered 
  event_ <- reactive({
    date.min = input$date_range_[1] %>% as_date()
    date.max = input$date_range_[2] %>% as_date()
    data = event()
    
    data %>% 
      filter(as_date(주차시각) >= date.min & as_date(주차시각) <= date.max)
  })
  
  ## event_week_filtered 
  event_w <- reactive({
    date.min = input$date_range_w[1] %>% as_date() 
    date.max = input$date_range_w[2] %>% as_date()
    data = event()
    
    data %>% 
      filter(as_date(주차시각) >= date.min & as_date(주차시각) <= date.max)
  })
  
  
  
  # 위치별 이용현황 캔들차트 
  output$hist_daily_a <- renderPlotly({
    data = event_()
    if(input$select_stat_type == "count"){
      data %>% 
        group_by(센서) %>% 
        summarise(이용건수 = n()) %>% 
        arrange(desc(이용건수)) %>% 
        head(5) %>% 
        
        
        plot_ly() %>% 
        add_trace(x = ~이용건수, y = ~센서, type = "bar",
                  text = ~paste0(이용건수, "건"),
                  hoverinfo = "text",
                  textposition = "inside",
                  marker = list(color = colors_list_5)
        ) %>% 
        layout(
          title = "이용건수 상위 5",
          xaxis = list(title = "", 
                       fixedrange = T),
          yaxis = list(title = "",
                       categoryorder = "array",
                       categoryarray = ~이용건수,
                       autorange="reversed",
                       fixedrange = T,
                       size=5),
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        ) %>% 
        config(displayModeBar = F)
      
    } else if(input$select_stat_type == "time"){
      data %>% 
        group_by(센서) %>% 
        summarise(체류시간 = round(mean(점유시간)/60,1)) %>% 
        arrange(desc(체류시간)) %>% 
        head(5) %>% 
        
        plot_ly() %>% 
        add_trace(x = ~체류시간, y = ~센서, type = "bar",
                  text = ~paste0(체류시간, "분"),
                  hoverinfo = "text",
                  textposition = "inside",
                  marker = list(color = colors_list_5)
        ) %>% 
        layout(
          title = list (text = "평균 체류시간 상위 5",
                        size = 4),
          
          xaxis = list(title = "", 
                       fixedrange = T),
          yaxis = list(title = "",
                       categoryorder = "array",
                       categoryarray = ~체류시간,
                       autorange="reversed",
                       fixedrange = T,
                       size=5),
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        ) %>% 
        config(displayModeBar = F)
      
    } else if(input$select_stat_type == "share"){
      share_() %>% 
        select(-time) %>% 
        summarise(across(everything(), list(mean))) %>% 
        gather("센서", "이용률") %>% 
        arrange(desc(이용률)) %>% 
        head(5) %>% 
        mutate(
          센서 = str_remove(센서, "_1"),
          센서 = str_remove(센서, "졸음쉼터."),
          이용률 = round(이용률*100, 1)
        ) %>% 
        
        plot_ly() %>% 
        add_trace(x = ~이용률, y = ~센서, type = "bar",
                  text = ~paste0(이용률, "%"),
                  hoverinfo = "text",
                  textposition = "inside",
                  marker = list(color = colors_list_5)
        ) %>% 
        layout(
          title = "평균 이용률 상위 5",
          xaxis = list(title = "", 
                       fixedrange = T),
          yaxis = list(title = "",
                       categoryorder = "array",
                       categoryarray = ~이용률,
                       autorange="reversed",
                       fixedrange = T,
                       size=5),
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        ) %>% 
        config(displayModeBar = F)
    }
    
  })
  
  ## 점유율 데이터 차트 
  share <- reactive({
    evt = event() %>% as.data.table()
    evt[, t := as.numeric(trunc.POSIXt(출차시각,"hours") - trunc.POSIXt(주차시각,"hours"))/3600]
    
    withProgress({
      setProgress(message = "이용률 계산 중...")
      share = evt %>% map_df(rep, times = evt$t + 1)  %>% as.data.table()
      
      share = share[,t := (sequence(.N)-1), by = .(센서, 주차시각)
      ][,시작시각 := trunc.POSIXt(주차시각,"hours") + 3600*t
      ][, 점유시간 := as.numeric(점유시간)
      ][,점유시간 := fcase(
        
        # 시작 끝 시간 같으면 그냥 넣기 
        trunc.POSIXt(주차시각,"hours") == trunc.POSIXt(출차시각,"hours"), as.numeric(점유시간),
        
        # 주차 == 시작 
        t == 0, as.numeric(ceiling_date(주차시각,"hours") - 주차시각),
        t != 0 & trunc.POSIXt(출차시각,'hours') > 시작시각, 3600, 
        
        trunc.POSIXt(출차시각,'hours') == 시작시각, as.numeric(출차시각 - 시작시각),
        default = 0)
        
      ][,.(센서, 시작시각, 점유시간)
      ][,.(점유시간 = round(sum(점유시간)/3600, digits = 2)), by = .(센서, time = 시작시각)] %>% 
        
        dcast(time ~ 센서, value.var = ("점유시간"), fill = 0) %>% tibble()
    })
  })
  
  ## share summary 
  share_ <- reactive({
    date.min = input$date_range_[1] %>% as_date()
    date.max = input$date_range_[2] %>% as_date()
    data = share() 
    data %>% 
      filter(as_date(time) <= date.max & as_date(time) >= date.min) 
  })
  
  ## share summary 
  share_w <- reactive({
    date.min = input$date_range_w[1] %>% as_date()
    date.max = input$date_range_w[2] %>% as_date()
    data = share() 
    data %>% 
      filter(as_date(time) <= date.max & as_date(time) >= date.min) 
  })
  
  # 일간 그래프들 -----------------------------------
  
  # 일간 이용률 
  output$share_plot_daily <- renderPlotly({
    x = share_() %>% 
      group_by(time = hour(as_datetime(time))) %>% 
      summarise(across(everything(), mean)) %>% 
      gather("센서","이용률", -time) %>% 
      group_by(time) %>% 
      summarise(이용률 = round(mean(이용률)*100,1)) %>% 
      rename(시간 = time) 
    
    me = round(mean(x$이용률),1)
    
    if(input$stat_type == "all"){
      p = x %>%
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~이용률,  type = 'scatter', mode = 'lines', fill = 'tozeroy', 
                  line = list(shape = "spline"), 
                  text = ~paste0(이용률,"%"),
                  hoverinfo = "text",
                  textposition = "inside") %>% 
        add_text(x = 1.5, y = me*1.05, text = ~paste0("평균 ", me, "%"),
                 hoverinfo ="text", showlegend = F) %>% 
        add_lines(x = 0:24, y = me, showlegend = F,
                  text = ~paste0("평균"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot'))
    }else { 
      
      x = share_() %>% 
        group_by(time = hour(as_datetime(time))) %>% 
        summarise(across(everything(), mean)) %>% 
        gather("센서","이용률", -time) %>% 
        group_by(졸음쉼터 = str_sub(센서,1,2), time) %>% 
        summarise(이용률 = round(mean(이용률)*100,1)) %>% 
        rename(시간 = time) 
      
      p = x %>%
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~이용률,  type = 'scatter', mode = 'lines', 
                  fill = ~졸음쉼터, 
                  color = ~졸음쉼터,
                  line = list(shape = "spline"), 
                  text = ~paste0(이용률,"%"),
                  hoverinfo = "text",
                  textposition = "inside") %>% 
        add_text(x = 1.5, y = me*1.05, text = ~paste0("평균 ", round(me,1), "%"),
                 hoverinfo ="text", showlegend = F) %>% 
        add_lines(x = 0:24, y = me, showlegend = F,
                  text = ~paste0("평균"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot')) 
    }
    
    p %>%
      layout(
        xaxis = list(title = "",
                     fixedrange = T),
        yaxis = list(title = "",
                     fixedrange = T),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)"
      ) %>% 
      config(displayModeBar = F)
  })
  
  
  
  # 일간 이용건수 
  output$count_plot_daily <- renderPlotly({
    nn = sensor.name() %>% length()
    
    x = event_() %>% 
      group_by(시간 = hour(주차시각)) %>% 
      summarise(이용건수 = n()) 
    
    peak.start = min(which(x$이용건수 > 1.5*mean(x$이용건수)))
    peak.end = max(which(x$이용건수 > 1.5*mean(x$이용건수)))
    
    me = round(mean(x$이용건수),0)
    
    if(input$stat_type == "all"){
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~이용건수,  type="scatter", mode="line",
                  line = list(shape = "spline"),
                  text = ~paste0(시간, "시 \n ", 이용건수, "건"),
                  hoverinfo = "text",
                  textposition = "inside"
        ) %>% 
        layout(shapes = list(type = "rect", fillcolor = "red",
                             line = list(color = "red"),
                             opacity = 0.2,
                             y0 = 0, y1 = ~max(이용건수), x0 = peak.start, x1 = peak.end)) %>% 
        add_lines(x = peak.start, y = ~range(0,max(이용건수)), showlegend = F, color = "red") %>%  
        add_lines(x = peak.end, y = ~range(0,max(이용건수)), showlegend = F, color = "red") %>%  
        add_text(x = (peak.start + peak.end)/2 , y = ~max(이용건수)*.2,
                 hoverinfo ="text",
                 showlegend = FALSE, text = paste0("\n 피크 시간대 \n", peak.start, "시 ~ ", peak.end, "시") ) 
    } else {
      
      x = event_() %>% 
        mutate(졸음쉼터 = str_sub(센서, 1,2)) %>% 
        group_by(졸음쉼터, 시간 = hour(주차시각)) %>% 
        summarise(이용건수 = n()) 
      
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~이용건수,  type="scatter", mode="line", 
                  line = list(shape = "spline"), 
                  fill = ~졸음쉼터,
                  text = ~paste0(졸음쉼터, "\n ", 이용건수, "건"),
                  hoverinfo = "text",
                  textposition = "inside",
                  color = ~졸음쉼터
        ) 
      
    }
    
    p %>%
      add_text(x = 1, y = me*1.05, text = paste0("평균 ",me,"건"), hoverinfo ="text", showlegend = F) %>% 
      add_lines(x = 0:24, y = me, showlegend = F,
                text = ~paste0("평균"),
                hoverinfo ="text",
                textposition = "inside",
                line = list(color = "grey",
                            width = 1,
                            dash = 'dot')) %>% 
      layout(
        xaxis = list(title = "",
                     fixedrange = T),
        yaxis = list(title = "",
                     fixedrange = T),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)"
      ) %>% 
      config(displayModeBar = F)
    
  })
  
  # 일간 이용시간
  output$time_plot_daily <- renderPlotly({
    x = event_() %>% 
      group_by(시간 = hour(주차시각)) %>% 
      summarise(체류시간 = round(mean(점유시간)/60,1))
    me = round(mean(x$체류시간),1)
    
    if(input$stat_type == "all"){
      
      if(x$시간[which(x$체류시간 == max(x$체류시간))] <= 5){
        peak.start = 0
        peak.end = max(which(x$체류시간 > 1.5*mean(x$체류시간) & x$시간 <= 5)) 
        
      } else {
        peak.start = min(which(x$체류시간 > 1.5*mean(x$체류시간)))
        peak.end = max(which(x$체류시간 > 1.5*mean(x$체류시간)))
      }
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~체류시간,  type="scatter", mode="line", 
                  line = list(shape = "spline"), 
                  text = ~paste0("\n ", 체류시간, "분"),
                  hoverinfo = "text",
                  textposition = "inside"
        )  %>% 
        layout(shapes = list(type = "rect", fillcolor = "red",
                             line = list(color = "red"),
                             opacity = 0.2,
                             y0 = 0, y1 = ~max(체류시간), x0 = peak.start, x1 = peak.end)) %>% 
        add_lines(x = peak.start, y = ~range(0,max(체류시간)), showlegend = F, color = "red") %>%  
        add_lines(x = peak.end, y = ~range(0,max(체류시간)), showlegend = F, color = "red") %>%  
        add_text(x = (peak.start + peak.end)/2 , y = ~max(체류시간)*.2,
                 hoverinfo ="text",
                 showlegend = FALSE, text = paste0("\n 피크 시간대 \n", peak.start, "시 ~ ", peak.end, "시") )
    } else {
      x = event_() %>% 
        group_by(졸음쉼터, 시간 = hour(주차시각)) %>% 
        summarise(체류시간 = round(mean(점유시간)/60,1))
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~체류시간,  type="scatter", mode="line", 
                  line = list(shape = "spline"), 
                  fill = ~졸음쉼터,
                  text = ~paste0("\n ", 체류시간, "분"),
                  hoverinfo = "text",
                  textposition = "inside",
                  color = ~졸음쉼터
        ) 
    }
    
    p %>%
      add_text(x = 1.5, y = me*1.05, text = ~paste0("평균 ", me, "분"),
               hoverinfo ="text", showlegend = F) %>% 
      add_lines(x = 0:24, y = me, showlegend = F,
                text = ~paste0("평균"),
                hoverinfo ="text",
                textposition = "inside",
                line = list(color = "grey",
                            width = 1,
                            dash = 'dot')) %>% 
      layout(
        xaxis = list(title = "",
                     fixedrange = T),
        yaxis = list(title = "",
                     fixedrange = T),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)"
      ) %>% 
      config(displayModeBar = F)
    
  })
  
  # 히트맵 
  output$heatmap_daily <- renderPlotly({
    share_ = share_() %>% 
      group_by(time = hour(as_datetime(time))) %>% 
      summarise(across(everything(), mean)) %>% 
      gather("센서","이용률", -time) %>% 
      group_by(센서, time) %>% 
      summarise(이용률 = round(mean(이용률)*100,1)) %>% 
      rename(시간 = time)
    
    for(i in 1:3){
      plot_name = paste0("p",i)
      sensor = c("대신","옥천","영천")
      
      showscale = ifelse(i == 1, T, F)
      plott = share_ %>%
        tibble() %>% 
        filter(str_detect(센서,sensor[i])) %>% 
        plot_ly() %>% 
        add_heatmap(x = ~시간, y = ~센서, z = ~이용률, 
                    showscale = showscale, 
                    zmin = 0, zmax = 55,
                    text = ~paste0(센서," ",시간,"시\n", 이용률, "%"),
                    hoverinfo ="text",
                    colors = colfunc_gyr(16)
        ) %>% 
        layout(
          xaxis = list(fixedrange = T),
          yaxis = list(title = "",
                       fixedrange = T,
                       categoryorder = "array",
                       categoryarray = ~센서,
                       autorange = "reversed"),
          plot_bgcolor  = "rgba(0, 0, 0, 0)",
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        ) 
      assign(plot_name, plott)
    }
    subplot(p1, p2, p3, nrows = 3, shareX = T) 
  })
  
  
  
  
  ### 주간 차트 ------------------------
  
  ## 주간 이용률 
  output$share_plot_week <- renderPlotly({
    if(input$stat_type_week == "all"){
      x = share_w() %>% 
        group_by(요일 = weekdays(as_date(time))) %>% 
        summarise(across(-time, mean)) %>% 
        gather("센서","이용률", -요일) %>% 
        group_by(요일) %>% 
        summarise(이용률 = round(mean(이용률*100),1))
      
      me = round(mean(x$이용률),1)
      p = x %>%
        plot_ly() %>% 
        add_trace(x = ~요일, y = ~이용률,  type = 'bar',
                  text = ~paste0(이용률,"%"),
                  hoverinfo = "text",
                  textposition = "inside") %>% 
        add_text(x = "Monday", y = me*1.05, text = ~paste0("평균 ", me, "%"),
                 hoverinfo ="text", showlegend = F) %>% 
        add_lines(x = week.name.ko, y = me, showlegend = F,
                  text = ~paste0("평균"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot'))
    } else {
      x = share_w() %>% 
        group_by(요일 = weekdays(as_date(time))) %>% 
        summarise(across(-time, mean)) %>% 
        gather("센서","이용률", -요일) %>% 
        group_by(졸음쉼터 = str_sub(센서,1,2),요일) %>% 
        summarise(이용률 = round(mean(이용률*100),1))
      
      me = round(mean(x$이용률),1)
      p = x %>%
        plot_ly() %>% 
        add_trace(x = ~요일, y = ~이용률,  type = 'bar', mode = "group",
                  text = ~paste0(이용률,"%"),
                  color = ~졸음쉼터,
                  hoverinfo = "text",
                  textposition = "inside") %>% 
        add_text(x = "Monday", y = me*1.05, text = ~paste0("평균 ", me, "%"),
                 hoverinfo ="text", showlegend = F) %>% 
        add_lines(x = week.name.ko, y = me, showlegend = F,
                  text = ~paste0("평균"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot'))
    }
    p %>%  layout(
      xaxis = list(title = "",
                   fixedrange = T,
                   categoryorder = "array",
                   categoryarray = week.name.ko),
      yaxis = list(title = "",
                   fixedrange = T),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  })
  
  ## 주간 이용건수 
  output$count_plot_week <- renderPlotly({
    x = event_w() %>% 
      group_by(요일) %>% 
      summarise(이용건수 = n()) 
    
    me = round(mean(x$이용건수),0)
    
    if(input$stat_type_week == "all"){
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~요일, y = ~이용건수,  type="bar",
                  text = ~paste0("\n", 이용건수, "건"),
                  hoverinfo = "text",
                  textposition = "inside"
        ) %>% 
        add_text(x = "Monday", y = me*1.05, text = paste0("평균 ",me,"건"), hoverinfo ="text", showlegend = F) %>% 
        add_lines(x = week.name.ko, y = me, showlegend = F,
                  text = paste0("평균 : ", me, "건"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot')) 
    } else {
      
      x = event_w() %>% 
        group_by(졸음쉼터, 요일) %>% 
        summarise(이용건수 = n()) 
      
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~요일, y = ~이용건수,  type = "bar",
                  mode='group',
                  text = ~paste0(졸음쉼터, "\n ", 이용건수, "건"),
                  hoverinfo = "text",
                  textposition = "inside",
                  color = ~졸음쉼터
        ) %>% 
        add_text(x = "Monday", y = me/3*1.05, text = paste0("평균 ",round(me/3,1),"건"), hoverinfo ="text", showlegend = F) %>% 
        add_lines(x = week.name.ko, y = me/3, showlegend = F,
                  text = paste0("평균 : ", round(me/3,1), "건"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot')) 
      
    }
    p %>%
      layout(
        xaxis = list(title = "",
                     fixedrange = T,
                     categoryorder = "array",
                     categoryarray = week.name.ko),
        yaxis = list(title = "",
                     fixedrange = T),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)"
      )
    
  })
  
  
  ## 주간 이용시간 
  output$time_plot_week <- renderPlotly({
    
    x = event_w() %>% 
      group_by(요일) %>% 
      summarise(체류시간 = round(mean(점유시간)/60),2) 
    
    me = round(mean(x$체류시간),1)
    
    if(input$stat_type_week == "all"){
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~요일, y = ~체류시간,  type="bar",
                  text = ~paste0("\n", 체류시간, "분"),
                  hoverinfo = "text",
                  textposition = "inside"
        ) 
    }  else {
      
      x = event_w() %>% 
        group_by(졸음쉼터, 요일) %>% 
        summarise(체류시간 = round(mean(점유시간)/60),2)
      
      
      p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~요일, y = ~체류시간,  type = "bar",
                  mode='group',
                  text = ~paste0(졸음쉼터, "\n ", 체류시간, "분"),
                  hoverinfo = "text",
                  textposition = "inside",
                  color = ~졸음쉼터
        )
      
    }
    p %>%
      add_text(x = "Monday", y = me*1.05, text = paste0("평균 ",me,"분"), hoverinfo ="text", showlegend = F) %>% 
      add_lines(x = week.name.ko, y = me, showlegend = F,
                text = paste0("평균 : ", me, "분"),
                hoverinfo ="text",
                textposition = "inside",
                line = list(color = "grey",
                            width = 1,
                            dash = 'dot')) %>% 
      layout(
        xaxis = list(title = "",
                     fixedrange = T,
                     categoryorder = "array",
                     categoryarray = week.name.ko),
        yaxis = list(title = "",
                     fixedrange = T),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)"
      )
  })
  
}# 

shinyApp(ui = ui, server = server)
