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

options(shiny.maxRequestSize=200*1024^2) ## 파일 업로드 제한을 200

datelist = c("2022-01-01", "2022-01-31", "2022-02-01", "2022-02-02",
  "2022-03-01", "2022-05-05", "2022-05-08", "2022-06-06",
  "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11",
  "2022-09-12", "2022-10-03", "2022-10-09", "2022-10-10", "2022-12-25") %>% 
  ymd()

def_data = read_xlsx("D:/shinydev/iot_sensor/def.xlsx")


ui <- navbarPage("주차센서 대시보드", 
  theme = bs_theme(bootswatch = "flatly"),
  
## 
## 00. 파일 업로드 
  tabPanel("분석 파일 업로드",
    sidebarLayout(
      sidebarPanel(
        width = 3,
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
                    placeholder = ".xlsx"
          ))
        
      ),
      mainPanel(
        position = "left",
        
        dataTableOutput("rare_dt")
      )
    ) # sidebar
  ),#tab


  ## 01. 일간 이용 현황분석 
  tabPanel("일간 이용현황 분석",
    sidebarLayout(
    # 일자 및 옵션 선택 
    sidebarPanel(width = 4,
           fluidRow(
             box(width = 12, status = "info", solidHeader = TRUE,
                 uiOutput("date_range"))),
      
           fluidRow(
             box(width = 12, status = "info", solidHeader = TRUE,
                  radioGroupButtons("stat_type",
                                   label = "분석 단위",
                                    choices = c(
                                      "이용건수" = "count",
                                      "체류시간" = "time",
                                      "이용률" = "share"),
                                    justified = T))),
           hr(),
           fluidRow(

             dataTableOutput("event_summary_daily")
             
           ),
    ),
    
    # 그래프 들어갈 부분 
    mainPanel(position = "right",
              tabsetPanel(
                tabPanel("전체",
                         h6("이용률"),
                         plotlyOutput("share_plot_daily_a", width = "100%", height = "230px"),
                         h6("일간 이용건수"),
                         plotlyOutput("count_plot_daily_a", width = "100%", height = "230px"),
                         h6("일간 체류시간"),
                         plotlyOutput("time_plot_daily_a", width = "100%", height = "230px"),
                         ),
                tabPanel("쉼터별",
                         h6("일간 이용률"),
                         plotlyOutput("share_plot_daily", width = "100%", height = "230px"),
                         h6("일간 이용건수"),
                         plotlyOutput("count_plot_daily", width = "100%", height = "230px"),
                         h6("일간 체류시간"),
                         plotlyOutput("time_plot_daily", width = "100%", height = "230px"),
                         )
              ),
            ),
    
    )#side
  ),#tab


  ## 02. 주간 이용 현황분석 
  tabPanel("주간 이용현황 분석",
           sidebarLayout(
             sidebarPanel(),
             mainPanel(position = "left",
                       dataTableOutput("share_table")
                       )
           ) # sidebar
  ),#tab
  
  ## 03. 오류 이용 현황분석 
  tabPanel("오류 현황",
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
    
    if(input$source == "def"){
      y = def_data
    } else {
      x = input$raw_data
      ext = tools::file_ext(x$datapath)
      req(x)
      validate(need(ext == "xlsx","Error : 정확한 파일을 업로드하세요."))
      y = read_xlsx(x$datapath)}
    
    y %>% rename(일시 = `송신일시(sendDate)`) %>% 
      select(c(센서, 센서값, 일시)) %>% 
      
      #센서값 열 분할 
      mutate(센서값 = str_remove_all(센서값, "[[a-zA-Z]|[가-힣]|[\\s]]+: ")) %>% 
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
      mutate(센서 = str_remove(센서, "^주차센서_"),
               졸음쉼터 = str_sub(센서,1,2)) %>% 
      
      # 중복, 결측치 제거 
      distinct() %>% 
      na.omit() %>% 
      
      # 정렬 
      arrange(센서, 일시) 
  })
  
  ## raw file 출력 
  output$rare_dt <- renderDataTable({
    rare_()
  })
  
  error_tag <- reactive({
    rare_() %>% 
      select(졸음쉼터, 센서, 검지기, 동작, 일시) %>% 
      
      mutate(오류구분 = case_when(
        동작 == "주차이벤트" ~ 0, 
        동작 == "주기보고" ~ 1,
        TRUE  ~ 2 ))  %>%
      
      mutate(오류구분 = case_when(
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
 
  
  
  
  # 분석 기간 선택 
  date.range <- reactive({
    min = rare_()$일시 %>% as_date() %>% min()  
    max = rare_()$일시 %>% as_date() %>% max()
  return(list(min, max))
    
  })
  
  # 분석기간 동적 슬라이더 
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
  
  
  # 오류 발생 현황 요약 테이블 -----
  output$error_dt <- renderTable({
    data = error_tag()
    y = input$error_group
    
    if(y == "2"){
      x = error_tag()
    } else {
      x = error_tag() %>% mutate(센서 = str_sub(센서, 1, 2))
    }
    
    x %>% group_by(센서 = str_remove_all(센서,"졸음쉼터")) %>% 
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
    data = error_tag() 
    
    data %>% 
      mutate(일시 = as_datetime(일시)) %>% 
      
      filter(오류구분 == 0) %>% 
      
      # 출차시각 
      mutate(출차시각 = as_datetime(ifelse(검지기 == "차량주차" & 센서 == lead(센서), lead(일시), NA))) %>% 
      
      filter(!is.na(출차시각)) %>% 
      
      rename(주차시각 = 일시) %>% 
      
      mutate(
        점유시간 = 출차시각 - 주차시각,
        요일 = weekdays(as_datetime(주차시각)),
        기간구분 = ifelse(
          요일 %in% c("토요일", "일요일") | as_date(주차시각) %in% datelist, "휴일", "평일")
      ) %>% 
      
      select(졸음쉼터, 센서, 요일, 기간구분, 주차시각, 출차시각, 점유시간) 
      
    
  })

  
  ## event_date_filtered 
  event_ <- reactive({
    date.min = input$date_range_[1] %>% as_date()
    date.max = input$date_range_[2] %>% as_date()
    data = event()
    
    data %>% 
      filter(as_date(주차시각) >= date.min & as_date(출차시각) <= date.max)
  })
  
  
  # event summary 
  event_s<- reactive({
    data = event_()
    share = share_s()
    
    x = share %>% group_by(졸음쉼터) %>% 
      summarise(평균_이용률 = mean(평균_이용률), 
                  최대이용률 = max(최대_이용률))
    
    ret = data %>%
      group_by(졸음쉼터) %>% 
      summarise(이용건수 = n(),
                `평균 체류시간(분)` = prettyNum(as.numeric(mean(점유시간)/60), digits = 4),
                `최대 체류시간(분)` = prettyNum(as.numeric(max(점유시간)/60), digits = 4)) %>%
      inner_join(x, by = c("졸음쉼터")) %>% 
      t()
      
    colnames(ret) = ret[1,]
    return(ret[-1,])
  })
  
  
  output$event_summary_daily <- renderDataTable(
    event_s()
  )
  
  
  
  # 일간 그래프들 
  # 
  output$count_plot_daily <- renderPlotly({
    event_() %>% group_by(졸음쉼터, 시간 = hour(주차시각)) %>% 
      summarise(이용건수 = n()) %>% ggplot() +
      geom_col(aes(x = 시간, y = 이용건수, fill = 졸음쉼터),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 이용건수 (건)") +
      theme_bw() 
  })
  
  
  # 
  output$time_plot_daily <- renderPlotly({
    event_() %>% group_by(졸음쉼터, 시간 = hour(주차시각)) %>% 
      summarise(평균점유시간 = mean(as.numeric(점유시간))/60) %>% ggplot() +
      geom_col(aes(x = 시간, y = 평균점유시간, fill = 졸음쉼터),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 체류시간 (분)") +
      theme_bw() 
  })
  
  output$share_plot_daily <- renderPlotly({
    data = share_s() %>% 
      ggplot() +
      geom_col(aes(x = 시간, y = 평균_이용률, fill = 졸음쉼터),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 이용률 (%)") +
      theme_bw() 
  })
  
  # 쉼터 구분 없이 전체 
  output$share_plot_daily_a <- renderPlotly({
    data = share_s() %>%
      group_by(시간) %>%
      summarise(평균_이용률 = mean(평균_이용률)) %>% 
      ggplot() +
      geom_col(aes(x = 시간, y = 평균_이용률),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 이용률 (%)") +
      theme_bw() 
  })
  
  # 
  output$count_plot_daily_a <- renderPlotly({
    event_() %>% group_by(시간 = hour(주차시각)) %>% 
      summarise(이용건수 = n()) %>% ggplot() +
      geom_col(aes(x = 시간, y = 이용건수),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 이용건수 (건)") +
      theme_bw() 
  })
  
  
  # 
  output$time_plot_daily_a <- renderPlotly({
    event_() %>% group_by(시간 = hour(주차시각)) %>% 
      summarise(평균점유시간 = mean(as.numeric(점유시간))/60) %>% ggplot() +
      geom_col(aes(x = 시간, y = 평균점유시간),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 체류시간 (분)") +
      theme_bw() 
  })
  
  ## 점유율 데이터 차트 
  share_ <- reactive({
    data = event() 
    
    sensor.name = names(table(data$센서))
    
    time = seq.POSIXt(
      from = data$주차시각 %>% min(),
      to = data$출차시각 %>% max() + 3600*12,
      by = "hour") %>%
      
      format.Date("%Y-%m-%d %H:00:00") %>%
      unique() %>% 
      as.character()
    
    ret = data.frame(matrix(0,nrow = length(time), ncol = length(sensor.name) + 1))
    colnames(ret) = c("time", sensor.name)
    ret$time = time
    
    for(i in 1:nrow(data)){
      
      sensor = data$센서[i]
      from = data$주차시각[i]
      end = data$출차시각[i]
      stime = data$점유시간[i]
      
      start_hour = format.Date(from, "%Y-%m-%d %H:00:00")
      end_hour = format.Date(end, "%Y-%m-%d %H:00:00")
      
      # 1번 : 주차시각과 출차시각이 같다면 점유시간 그대로 플러스 
      if(start_hour == end_hour){
        ret[time == start_hour, sensor] <- ret[time == start_hour, sensor] + stime
      } 
      # 2번 : 주차시각과 출차시각이 다를 때 
      else if(start_hour != end_hour){
        start_frac = 3600 - {(from %>% format.Date("%M") %>% as.numeric() * 60) + (from %>% format.Date("%S") %>% as.numeric())}
        end_frac = (end %>% format.Date("%M") %>% as.numeric() * 60) + (end %>% format.Date("%S") %>% as.numeric())
        
        # 시작 시간에 
        ret[time == start_hour, sensor] <- ret[time == start_hour, sensor] + start_frac
        
        # 마지막 시간에 
        ret[time == end_hour, sensor] <- ret[time == end_hour, sensor] + end_frac
        
        # 가운데 시간에 + 3600
        if(as.POSIXct(format(from + 3600*2, "%Y-%m-%d %H:00:00"), tz = "UTC") <= end ){
          for(mid_time in seq.POSIXt(as.POSIXct(format(from + 3600, "%Y-%m-%d %H:00:00"), tz = "UTC"),
                                     to = end - 3600,
                                     by = "hour")){
            ret[time == format(mid_time %>% as_datetime(), "%Y-%m-%d %H:00:00"), sensor] <- 3600
          }
        }
        
      }
    }
    ret = ret %>% mutate_at(.cols = -1,.funs = function(x) x/3600)
    return(ret)
  })
  
  ## share summary 
  share_s <- reactive({
    data = share_() 
    nn = ncol(data)
    data %>% group_by(시간 = hour(time)) %>% 
      summarise(across(2:nn, list(mean))) %>% 
      gather(key = "센서", value = "이용률", -시간) %>% 
      group_by(졸음쉼터 = str_sub(센서,1,2), 시간) %>% 
      summarise(평균_이용률 = mean(이용률), 
                  최대_이용률 = max(이용률))
  })
  
}# 







shinyApp(ui = ui, server = server)
