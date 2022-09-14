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

colfunc <- colorRampPalette(c("red", "yellow","orange"))
colors_list_30 <- colfunc(30)
colors_list_5 <- colfunc(10)

options(shiny.maxRequestSize=200*1024^2) ## 파일 업로드 제한을 200

holiday_list = c("2022-01-01", "2022-01-31", "2022-02-01", "2022-02-02",
  "2022-03-01", "2022-05-05", "2022-05-08", "2022-06-06",
  "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11",
  "2022-09-12", "2022-10-03", "2022-10-09", "2022-10-10", "2022-12-25") %>% 
  ymd()

def_data = read_xlsx("D:/shinydev/iot_sensor/def.xlsx")

vline <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

time_share <- function(from, to, diff) {
  diff = as.numeric(diff)
  time.from = floor_date(from, unit = "hour")
  time.to = floor_date(to, unit = "hour")
  time.seq = seq.POSIXt(
    time.from,
    time.to, 
    by = "hour")
  n = length(time.seq) 
  
  frac.start = difftime(ceiling_date(from, unit = "hour"), from, units = 'sec') %>% as.numeric()
  frac.end = difftime(to, floor_date(to, unit = "hour"), units = 'sec') %>% as.numeric()
  
  if(n == 1){
    setNames(c(diff), c(time.from)) %>% as.data.frame()}
  else{
    setNames(c(frac.start, rep(3600, n-2), frac.end), c(time.seq)) %>% as.data.frame()}
}


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
                         h6("일간 이용률"),
                         plotlyOutput("share_plot_daily_all", width = "100%", height = "230px"),
                         h6("일간 이용건수"),
                         plotlyOutput("count_plot_daily", width = "100%", height = "230px"),
                         h6("일간 체류시간"),
                         plotlyOutput("time_plot_daily_all", width = "100%", height = "230px"),
                         ),
                conditionalPanel(
                  condition = "input.stat_type == 'heatmap'",
                         h6("일간 이용률"),
                         plotlyOutput("share_plot_daily_loc", width = "100%", height = "230px"),
                         h6("일간 이용건수"),
                         h6("일간 체류시간"),
                         plotlyOutput("time_plot_daily_loc", width = "100%", height = "230px"),
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
                         uiOutput("date_range_week"), 
                         radioGroupButtons("stat_type_week",
                                           label = "분석 단위",
                                           choices = c(
                                             "전체" = "all",
                                             "졸음쉼터" = "loc",
                                             "주차면" = "lot"),
                                           justified = T,
                                           width = "100%"),
                         hr()
                       ),
                       fluidRow(
                         box(width = 12, status = "info", solidHeader = TRUE,
                         )),
          ),
          
          # 그래프 들어갈 부분 
          mainPanel(position = "right",
                    width = 7,
                    conditionalPanel(
                      condition = "input.stat_type_week == 'all'",
                      h6("주간 이용률"),
                      plotlyOutput("share_plot_week_all", width = "100%", height = "230px"),
                      h6("주간 이용건수"),
                      plotlyOutput("count_plot_week_all", width = "100%", height = "230px"),
                      h6("주간 체류시간"),
                      plotlyOutput("time_plot_week_all", width = "100%", height = "230px"),
                    ),
                    conditionalPanel(
                      condition = "input.stat_type_week == 'loc'",
                      h6("주간 이용률"),
                      plotlyOutput("share_plot_week_loc", width = "100%", height = "230px"),
                      h6("주간 이용건수"),
                      plotlyOutput("count_plot_week_loc", width = "100%", height = "230px"),
                      h6("주간 체류시간"),
                      plotlyOutput("time_plot_week_loc", width = "100%", height = "230px"),
                    ),
                    conditionalPanel(
                      condition = "input.stat_type_week== 'lot'",
                      plotOutput("heatmap_week_lot", width = "100%", height = "250px"),
                      plotlyOutput("count_plot_week_lot", width = "100%", height = "230px"),
                      plotlyOutput("time_plot_week_lot", width = "100%", height = "230px"),
                ),
      ),
                      
      )#side
 ),#tab
  
  ## 03. 오류 이용 현황분석 
  tabPanel("월간",
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
          ),
        일시 = as_datetime(일시, tz = "")
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
  output$date_range_week <- renderUI({
    min = date.range()[[1]]
    max = date.range()[[2]]
    
    dateRangeInput("date_range_week",
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
      mutate(출차시각 = as_datetime(ifelse(검지기 == "차량주차" & 센서 == lead(센서), lead(일시), NA), tz="")) %>% 
      
      filter(!is.na(출차시각)) %>% 
      
      rename(주차시각 = 일시) %>% 
      
      mutate(
        점유시간 = as.difftime(출차시각 - 주차시각, units = "secs"),
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
      filter(as_date(주차시각) >= date.min & as_date(출차시각) <= date.max)
  })
  
  
  # event summary 
  event_s<- reactive({
    data = event_()

  })
  
  
  output$event_summary_daily <- renderDataTable(
    event_s()
  )
  
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
  
  
  
  
  # 일간 그래프들 -----------------------------------
  
  # 일간 이용률 : 전체 
  output$share_plot_daily_all <- renderPlotly({
    data = share_() %>% 
      group_by(시간 = hour(time)) %>%         
      summarise(across(-time, list(mean))) %>% 
      gather(key = "센서", value = "이용률", -시간) %>% 
      group_by(시간) %>% 
      summarise(평균_이용률 = mean(이용률)) %>% 
      ggplot() +
      geom_line(aes(x = 시간, y = 평균_이용률)) +
      ylab("평균 이용률 (%)") +
      scale_fill_brewer(palette="Spectral") +
      theme_bw() 
  })
  
  # 일간 이용률 : 쉼터
  output$share_plot_daily_loc <- renderPlotly({
    share_() %>%
      group_by(시간 = hour(time)) %>%  
      summarise(across(-time, list(mean))) %>% 
      gather(key = "센서", value = "이용률", -시간) %>% 
      group_by(졸음쉼터 = str_sub(센서,1,2), 시간) %>% 
      summarise(평균_이용률 = mean(이용률)) %>% 
      ggplot() +
      geom_line(aes(x = 시간, y = 평균_이용률, color = 졸음쉼터)) +
      ylab("평균 이용률 (%)") +
      scale_fill_brewer(palette="Spectral") +
      theme_bw() 
  })
  
  # 일간 이용률 : 주차면
  output$share_plot_daily_lot <- renderPlotly({
    share_() %>%
      group_by(시간 = hour(time)) %>%  
      summarise(across(-time, list(mean))) %>% 
      gather(key = "센서", value = "이용률", -시간) %>% 
      group_by(주차면 = 센서, 시간) %>% 
      summarise(평균_이용률 = mean(이용률)) %>% 
      ggplot() +
      geom_line(aes(x = 시간, y = 평균_이용률, color = 주차면)) +
      ylab("평균 이용률 (%)") +
      scale_fill_brewer(palette="Spectral") +
      theme_bw() 
  })
  
  # 일간 이용건수 : 전체 
  output$count_plot_daily <- renderPlotly({
    nn = sensor.name() %>% length()
    
    x = event_() %>% 
      group_by(시간 = hour(주차시각)) %>% 
      summarise(이용건수 = n()) 
    
    peak.start = min(which(x$이용건수 > 1.5*mean(x$이용건수)))
    peak.end = max(which(x$이용건수 > 1.5*mean(x$이용건수)))
    
    if(input$stat_type == "all"){
      
     p = x %>% 
        plot_ly() %>% 
        add_trace(x = ~시간, y = ~이용건수,  type="scatter", mode="line",
                  text = ~paste0(시간, "시 \n ", 이용건수, "건"),
                  hoverinfo = "text",
                  textposition = "inside"
        ) %>% 
        add_lines(x = 0:24, y = ~mean(이용건수), showlegend = F,
                  text = ~paste0("평균"),
                  hoverinfo ="text",
                  textposition = "inside",
                  line = list(color = "grey",
                              width = 1,
                              dash = 'dot')) %>% 
        layout(shapes = list(type = "rect", fillcolor = "red",
                             line = list(color = "red"),
                             opacity = 0.2,
                             y0 = 0, y1 = ~max(이용건수), x0 = peak.start, x1 = peak.end)) %>% 
        add_lines(x = peak.start, y = ~range(0,max(이용건수)), showlegend = F, color = "red") %>%  
        add_lines(x = peak.end, y = ~range(0,max(이용건수)), showlegend = F, color = "red") %>%  
        add_text(x = (peak.start + peak.end)/2 , y = ~mean(이용건수)*1.05,
                 hoverinfo ="text",
                 showlegend = FALSE, text = paste0("\n 피크 시간대 \n", peak.start, "시 ~ ", peak.end, "시") ) %>% 
        add_text(x = .5, y = ~mean(이용건수)*1.05, text = "평균", hoverinfo ="text", showlegend = F) 
      } else {
        
        x = event_table %>% 
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
          ) %>% 
          add_lines(x = 0:24, y = ~mean(이용건수), showlegend = F,
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

  

  
  # 히트맵 
  output$heatmap_daily_lot <- renderPlot({
    share_() %>% 
      group_by(시간 = hour(time)) %>% 
      summarise(across(-time, list(mean))) %>%
      select(-시간) %>% 
      t() %>% 
      heatmap(col = colfunc(32),
              revC = T,
              Colv = NA,
              Rowv = NA,
              scale =  "column")
  })
  # 일간 이용시간 : 전체 
  output$time_plot_daily_all <- renderPlotly({
    event_() %>% group_by(시간 = hour(주차시각)) %>% 
      summarise(평균점유시간 = mean(as.numeric(점유시간))/60) %>% ggplot() +
      geom_col(aes(x = 시간, y = 평균점유시간),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 체류시간 (분)") +
      scale_fill_brewer(palette="Spectral") +
      theme_bw() 
  })

  # 일간 이용시간 : 쉼터별
  output$time_plot_daily_loc <- renderPlotly({
    event_() %>% group_by(졸음쉼터, 시간 = hour(주차시각)) %>% 
      summarise(평균점유시간 = mean(as.numeric(점유시간))/60) %>% ggplot() +
      geom_col(aes(x = 시간, y = 평균점유시간, fill = 졸음쉼터),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 체류시간 (분)") +
      theme_bw() 
  })  
  
  # 일간 이용시간 : 주차면별
  output$time_plot_daily_lot <- renderPlotly({
    event_() %>% group_by(센서, 시간 = hour(주차시각)) %>% 
      summarise(평균점유시간 = mean(as.numeric(점유시간))/60) %>% ggplot() +
      geom_col(aes(x = 시간, y = 평균점유시간, fill = 센서),
               position = position_dodge(preserve = 'single')) +
      ylab("평균 체류시간 (분)") +
      theme_bw() 
  })
  
  
  
  ## 점유율 데이터 차트 
  share <- reactive({
    data = event() 
    
    sensor.name = sensor.name()
    tx = data.frame(
      time = seq.POSIXt(
        from = date.range()[[1]] %>% as_datetime(tz="") %>% floor_date("hour"), 
        to = date.range()[[2]] %>% as_datetime(tz=""), 
        by = "hour"
    ) %>% as.character() )
    
    withProgress({
      setProgress(message = "이용률 계산 중...")
      
    for(i in sensor.name){
      
      z = data %>%
        filter(센서 == i) 
      
      ret = pmap_dfr(list(from = z$주차시각, to = z$출차시각, diff = z$점유시간), time_share) %>% 
        rownames_to_column("time") %>% 
        mutate(time = str_remove_all(time, "\\.{3}.*"),
               time = ifelse(str_length(time) == 10, str_c(time, " 00:00:00"), time)) %>% 
        group_by(time) %>% 
        summarise(name = sum(., na.rm = T)) 
      
      
      tx = merge(tx, ret, by = 'time', all = T)
      incProgress(1/length(sensor.name))
    }
      names(tx) = c("time", sensor.name)
  })
    
    tx = tx %>% mutate_at(.cols = -1,.funs = function(x) ifelse(is.na(x), 0,x/3600))
    return(tx)
  })
  
  ## share summary 
  share_ <- reactive({
    date.min = input$date_range_[1] %>% as_date()
    date.max = input$date_range_[2] %>% as_date()
    data = share() 
      data %>% 
        filter(as_date(time) <= date.max & as_date(time) >= date.min) 
  })
  
  # pie oven 

  # 파이 차트 
  output$pie_daily_1 <- renderPlot({
    data = event_()
    stat = input$stat_type
    sensor = "대신"
    
    if(as.character(stat) == "이용건수"){
      x = data %>% 
        filter(졸음쉼터 == sensor) %>% 
        group_by(센서) %>% 
        summarise(이용건수 = n()) %>% 
        arrange(desc(이용건수)) %>% 
        mutate(frac = 이용건수/sum(이용건수),
               ymax = cumsum(frac),
               ymin = c(0, head(ymax, -1)),
               label_position = (ymax + ymin) /2, 
               label = paste0(str_remove_all(센서,"\\D"), " : ",prettyNum(frac*100, digits = 3), "%")) 
      
      }else if(as.character(stat) == "체류시간"){
        x = data %>%  
          filter(졸음쉼터 == sensor) %>% 
          group_by(센서) %>% 
          summarise(점유시간 = as.numeric(mean(점유시간))) %>% 
          arrange(desc(점유시간)) %>% 
          mutate(frac = 점유시간/sum(점유시간),
                 ymax = cumsum(frac),
                 ymin = c(0, head(ymax, -1)),
                 label_position = (ymax + ymin) /2, 
                 label = paste0(str_remove_all(센서,"\\D"), " : ",prettyNum(frac*100, digits = 3), "%")) 
      }
    
    x %>% ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=센서)) +
      scale_fill_brewer(palette="Spectral") +
      geom_rect() +
      geom_label(x=3, aes(y=label_position, label=label), size=3) +
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) +
      theme_void() +
      ggtitle(paste0(sensor," ",stat)) + 
      theme(legend.position = "none",
            panel.background = element_rect(fill= '#F7F7F7', color = '#f7f7f7'), 
            plot.background = element_rect(fill='#F7F7F7', color = '#f7f7f7'))
  })
  
  # 파이 차트 
  output$pie_daily_2 <- renderPlot({
    data = event_()
    stat = input$stat_type
    sensor = "영천"
    
    if(as.character(stat) == "이용건수"){
      x = data %>% 
        filter(졸음쉼터 == sensor) %>% 
        group_by(센서) %>% 
        summarise(이용건수 = n()) %>% 
        arrange(desc(이용건수)) %>% 
        mutate(frac = 이용건수/sum(이용건수),
               ymax = cumsum(frac),
               ymin = c(0, head(ymax, -1)),
               label_position = (ymax + ymin) /2, 
               label = paste0(str_remove_all(센서,"\\D"), " : ",prettyNum(frac*100, digits = 3), "%")) 
      
      }else if(as.character(stat) == "체류시간"){
        x = data %>%  
          filter(졸음쉼터 == sensor) %>% 
          group_by(센서) %>% 
          summarise(점유시간 = as.numeric(mean(점유시간))) %>% 
          arrange(desc(점유시간)) %>% 
          mutate(frac = 점유시간/sum(점유시간),
                 ymax = cumsum(frac),
                 ymin = c(0, head(ymax, -1)),
                 label_position = (ymax + ymin) /2, 
                 label = paste0(str_remove_all(센서,"\\D"), " : ",prettyNum(frac*100, digits = 3), "%")) 
      }
    
    x %>% ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=센서)) +
      scale_fill_brewer(palette="Spectral") +
      geom_rect() +
      geom_label(x=3, aes(y=label_position, label=label), size=3) +
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) +
      theme_void() +
      ggtitle(paste0(sensor," ",stat)) + 
      theme(legend.position = "none",
            panel.background = element_rect(fill= '#F7F7F7', color = '#f7f7f7'), 
            plot.background = element_rect(fill='#F7F7F7', color = '#f7f7f7'))
  })
  
  # 파이 차트 
  output$pie_daily_3 <- renderPlot({
    data = event_()
    stat = input$stat_type
    sensor = "옥천"
    
    if(as.character(stat) == "이용건수"){
      x = data %>% 
        filter(졸음쉼터 == sensor) %>% 
        group_by(센서) %>% 
        summarise(이용건수 = n()) %>% 
        arrange(desc(이용건수)) %>% 
        mutate(frac = 이용건수/sum(이용건수),
               ymax = cumsum(frac),
               ymin = c(0, head(ymax, -1)),
               label_position = (ymax + ymin) /2, 
               label = paste0(str_remove_all(센서,"\\D"))) 
      
      }else if(as.character(stat) == "체류시간"){
        x = data %>%  
          filter(졸음쉼터 == sensor) %>% 
          group_by(센서) %>% 
          summarise(점유시간 = as.numeric(mean(점유시간))) %>% 
          arrange(desc(점유시간)) %>% 
          mutate(frac = 점유시간/sum(점유시간),
                 ymax = cumsum(frac),
                 ymin = c(0, head(ymax, -1)),
                 label_position = (ymax + ymin) /2, 
                 label = paste0(str_remove_all(센서,"\\D"))) 
      }
    
    x %>% ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=센서)) +
      scale_fill_brewer(palette="Spectral") +
      geom_rect() +
      geom_label(x=3, aes(y=label_position, label=label), size=3) +
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) +
      theme_void() +
      ggtitle(paste0(sensor," ",stat)) + 
      theme(legend.position = "none",
            panel.background = element_rect(fill= '#F7F7F7', color = '#f7f7f7'), 
            plot.background = element_rect(fill='#F7F7F7', color = '#f7f7f7'))
  })
  

}# 





shinyApp(ui = ui, server = server)
