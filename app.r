#
# 빅카인즈 플랫폼 시각화용 shiny
# 
#
#
#

#### global
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
library(tm)
library(wordcloud2)
library(visNetwork) 
library(concorR)

options(repos = c(CRAN = "http://cran.rstudio.com"))
options(shiny.maxRequestSize=200*1024^2) ## 파일 업로드 제한을 200
options(dplyr.summarise.inform = FALSE)

## esg 기본 용어 
esg_env = "탄소, 신재생, 그린뉴딜, 충전소, 온실가스, 폐기물, 오염, 유해, 토양, 재활용, 미세먼지, 소음, 친환경"
esg_saf = "산업안전, 교통사고, 교통안전, 중대재해, 처벌, 작업장, 안전사고" 
esg_soc = "동반, 일자리, 공헌, 중소기업, 인권, 지역사회, 장애인, 사회적, 봉사, 공정, 정보보호"
esg_gov = "지배구조, 청렴, 윤리, 감사, 이해충돌, 반부패, 비리, 부정, 이사회, 주주, 경영"


## stopwords 기본 사전 

stopwords = "대한민국, 한국.*, 한국도로공사, 도공, 도로공사, 관계자, 만큼, 박.., 김.., 문.., 이.., 대선, km, 더불어민주당, 국민의힘, .*들, ..장, .씨, 개월, 개소, 시간.*, .*도로"
regions = ", 강원.*, 강릉, 광주, 대구, 부산, 서울, 수도권, 춘천, 울산, .북, 경부.*, .남, 가능성, 그동안, 위원회, 한국.*공사, 인천.*, .*공단"
stopwords = paste0(stopwords, regions)
stopwords
## dupwords 기본 사전 
dupwords = c("국토부, 교통부 :  국토교통부","기재부, 재정부 : 기획재정부",
             "도공 : 한국도로공사", "고속도 : 고속도로", "고속도로순찰대 : 순찰대", "코로나바이러스, 감염증 : 코로나")

title_filter_words = "인사, 부고, 강세, 경기, 영입, 부음"

### functions
words_detect <- function(df, esg_env, esg_soc, esg_saf, esg_gov) {
  res = ifelse(str_detect(df, esg_env), "env", 
               ifelse(str_detect(df, esg_saf), "saf", 
                      ifelse(str_detect(df, esg_soc), "soc", 
                             ifelse(str_detect(df, esg_gov), "gov", "none" 
                             ))))
  
  return(res)
}

news.data_def = read_xlsx("exnews_raw.xlsx")


ui <- navbarPage("빅카인즈 텍스트 분석",
                 theme = bs_theme(bootswatch = "flatly"),
                 id = "nav1",
                 # 파일 입력 및 확인 패널
                 tabPanel("뉴스 데이터 입력",
                          
                          sidebarLayout(
                            
                            mainPanel(
                              position = "left",
                              h6(textOutput("text_n_news")),
                              h6(textOutput("text_n_news1")),
                              h6(textOutput("text_n_news2")),
                              hr(),
                              DT::dataTableOutput("table_news")
                            ),
                            
                            sidebarPanel(
                              tags$a("빅카인즈 바로가기", href = "https://www.bigkinds.or.kr/v2"),
                              
                              radioGroupButtons(
                                inputId = "source",
                                label = "분석 파일 선택하기",
                                choices = c(
                                  "기본 파일" = "def",
                                  "사용자 파일" = "file"),
                                justified = T
                              ),
                              conditionalPanel(
                                condition = "input.source == 'file'",
                                fileInput("news",
                                          "뉴스 파일 업로드",
                                          accept = ".xlsx",
                                          buttonLabel = "찾아보기",
                                          placeholder = "파일 없음.."
                                )),
                              actionBttn("reset_filter", 
                                         label = "파일 불러오기",
                                         style = "pill",
                                         color = "primary"),
                              hr(),
                              
                              radioGroupButtons(
                                inputId = "Opt_keyword",
                                label = "키워드 옵션",
                                size = "sm",
                                choices = c("특성 키워드","전체 키워드"),
                                justified = T,
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
                              ),
                              
                              radioGroupButtons(
                                inputId = "filter_title",
                                label = "제목 필터링",
                                size = "sm",
                                choices = c(
                                  "기본" = "def",
                                  "수정" = "mod"),
                                justified = T
                              ),
                              
                              conditionalPanel(
                                condition = "input.filter_title == 'mod'",
                                textAreaInput("text_title_filter", "제목 필터단어", 
                                              width = "100%",
                                              value = title_filter_words)),
                              
                              radioGroupButtons(
                                inputId = "filter_esg",
                                label = "ESG 용어사전",
                                size = "sm",
                                choices = c(
                                  "기본 용어" = "def",
                                  "용어 편집" = "mod"),
                                justified = T
                              ),
                              conditionalPanel(
                                condition = "input.filter_esg == 'mod'",
                                textAreaInput("text_env","E 환경", 
                                              width = "100%",
                                              value = esg_env),
                                textAreaInput("text_soc","S 사회",
                                              width = "100%",
                                              value = esg_soc),
                                textAreaInput("text_saf", "S 안전",
                                              width = "100%",
                                              value = esg_saf),
                                textAreaInput("text_gov", "G 윤리청렴",
                                              width = "100%",
                                              value = esg_gov))
                              
                              
                            )
                          )
                 ), #tab 1
                 
                 # 2. 추세 분석
                 tabPanel("추세 분석",
                          sidebarLayout(
                            mainPanel(
                              position = "left",
                              h5("ESG 뉴스 추세 분석"),
                              tabsetPanel(
                                tabPanel("bar plot", plotlyOutput("plot_esg")),
                                tabPanel("table", tableOutput("table_trend"))
                              ),
                              hr(),
                            ),
                            
                            sidebarPanel(
                              uiOutput("news.data.date"),
                              radioGroupButtons(
                                inputId = "opt_date_selection",
                                label = "기간 구분",
                                selected = "일",
                                choices = c("년", "월", "일"),
                                justified = TRUE)
                              
                            )
                          )          
                 ), #tab 2
                 
                 
                 # 3. 네트워크 시각화 패널 
                 tabPanel("키워드 분석",
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              
                              # 가중치 설정 
                              radioGroupButtons(
                                inputId = "Opt_weight",
                                label = "단어 가중치 옵션",
                                size = "sm",
                                choices = c("단어 빈도수","TF - IDF"),
                                justified = T,
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-check-square",
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-square-o",
                                              style = "color: steelblue"))
                              ),
                              
                              # 중복어 처리 
                              textAreaInput("text_dupwords","\u2795 중복어 처리",
                                            width = "100%",
                                            height = "auto",
                                            rows = 10,
                                            cols = 2,
                                            value = paste0(as.character(dupwords), collapse = "\n")),  
                              
                              # 불용어 처리  
                              textAreaInput("text_stopwords","\u274C 불용어 처리",
                                            width = "100%",
                                            height = "auto",
                                            rows = 10,
                                            value = stopwords),
                              actionBttn("reset_words", 
                                         label = "\u2714 필터 새로 적용하기",
                                         style = "pill",
                                         color = "primary")
                              
                            ), # side
                            
                            mainPanel(
                              
                              h5("키워드 빈도"),
                              tabsetPanel(
                                tabPanel("\u2601 Wordcloud", wordcloud2Output("wordcloud_termFreq",
                                                                              height =  "700px")),
                                tabPanel("□ Table", tableOutput("table_termFreq"))
                              )
                              
                            ) # main 
                          )
                 ), #tab 3
                 
                 # 4. 네트워크 시각화 패널 
                 tabPanel("네트워크 분석",
                          sidebarLayout(
                            position = "right",
                            sidebarPanel(
                              sliderInput("slider_num_words",
                                          label = "네트워크 단어 수",
                                          min = 64,
                                          max = 128,
                                          value = 128),
                              sliderInput("opt_concor",
                                          label = "나눌 그룹 수",
                                          min = 2,
                                          max = 4,
                                          value = 3),
                              br(),
                              actionBttn("reset_network", 
                                         label = "네트워크 구성하기",
                                         style = "pill",
                                         color = "primary",
                                         icon = icon("cloud"))
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("네트워크 시각화", visNetworkOutput("vis_network", height = "600px")),
                                tabPanel("군집별 단어", dataTableOutput("table_node_group")),
                              ) #tabset
                            )
                          ) # 
                 ), #tab 4
                 
                 tabPanel("매뉴얼",
                          p("매뉴얼이 들어가는 자리 ")
                 ) # 5. tab 매뉴얼
)




### 

server <- function(input, output) {
  
  # 제목 필터 단어 
  text_title <- reactive({
    res = paste0("\\b", input$text_title_filter,"\\b") %>%
      str_replace_all("\\[","\\\\[") %>% 
      str_replace_all("\\]","\\\\]") %>% 
      str_replace_all(",\\s*","\\\\b|\\\\b")
    return(res)
  })
  
  # 각 용어들 반응형 전처리 
  text_env <- reactive({input$text_env %>% str_replace_all(",\\s*", "|")})
  text_soc <- reactive({input$text_soc %>% str_replace_all(",\\s*", "|")})
  text_saf <- reactive({input$text_saf %>% str_replace_all(",\\s*", "|")})
  text_gov <- reactive({input$text_gov %>% str_replace_all(",\\s*", "|")})
  
  
  ## news 파일 
  news.data <- reactive({
    
    if(input$source == "def"){
      y = news.data_def
    } else {
      x = input$news
      ext = tools::file_ext(x$datapath)
      req(x)
      validate(need(ext == "xlsx","Error : 정확한 파일을 업로드하세요."))
      y = read_xlsx(x$datapath)}
    
    y %>% rename(특성 =  "특성추출(가중치순 상위 50개)") %>%
      select(일자, 제목, as.character(option_keyword())) 
  })
  
  ## news filtered 
  news.data.filtered.0 <- reactive({
    news.data() %>% filter(str_detect(제목, as.character(text_title()), negate = T))
  })
  
  news.data.filtered.1 <- reactive({
    key = option_keyword() %>% as.character()
    x = news.data.filtered.0() 
    x %>% mutate(esg = 
                   ifelse(str_detect(x[[key]], text_env()), "env",
                          ifelse(str_detect(x[[key]], text_saf()), "saf",
                                 ifelse(str_detect(x[[key]], text_soc()), "soc",
                                        ifelse(str_detect(x[[key]], text_gov()), "gov", "none"
                                        )))))
  })
  news.data.filtered.2 <- reactive({
    data = news.data.filtered.1()
    if(length(input$UI_date[1])){
      return(data %>% filter(esg != "none") %>% 
               filter(ymd(일자) >= input$UI_date[1] & ymd(일자) < input$UI_date[2]))
    } else if(input$opt_date_selection == "년"){
      filter(ymd(일자) >= input$UI_date[1] & 일자[1:4] <= input$UI_date[2])
    }else{
      return(data %>% filter(esg != "none"))}
  })
  
  date.range <- reactive({
    min = news.data.filtered.0()$일자 %>% min() %>% ymd()
    max = news.data.filtered.0()$일자 %>% max() %>% ymd() + 1
    if(max - min < 365){updateRadioGroupButtons(inputId = "opt_date_selection", disabledChoices = "년")
    } else {updateRadioGroupButtons(inputId = "opt_date_selection", disabledChoices = NULL)}
    if(max - min < 31){updateRadioGroupButtons(inputId = "opt_date_selection", disabledChoices = c("월", "반기", "년") )}
    
    if(input$opt_date_selection == "년"){return(seq(min, max + 365, by = "1 year"))}
    else if(input$opt_date_selection == "월"){return(seq(min, max, by = "1 month"))}
    else {return(seq(min, max, by = "1 day"))}
    
  })
  
  ## 일자 구하는 ui 
  output$news.data.date <- renderUI({
    sliderTextInput("UI_date",
                    label = "분석 기간 선택",
                    choices = date.range(),
                    selected = c(date.range() %>% min(), date.range() %>% max()),
                    grid = T
    )
  })
  
  # 키워드 옵션
  option_keyword <- reactive({
    ifelse(input$Opt_keyword == "특성 키워드","특성","키워드")
  })
  option_weight <- reactive({
    ifelse(input$Opt_weight == "단어 빈도수", T, F)
  })
  
  # 테이블 출력   
  output$table_news <- renderDataTable({
    input$reset_filter
    isolate({
      withProgress({
        setProgress(message = "파일을 읽는 중입니다...")
        return(news.data.filtered.1())
      })
    })
  })  
  
  # 입력된 raw 뉴스 건수 
  output$text_n_news <- renderText({
    return(paste0("전체 뉴스 :", nrow(news.data()),"건"))
  })
  output$text_n_news1 <- renderText({
    return(paste0("제목 필터 : ", nrow(news.data.filtered.1()),"건"))
  })
  output$text_n_news2 <- renderText({
    
    return(paste0("ESG 키워드 필터 : ", nrow(news.data.filtered.1() %>% filter(esg != "none")),"건"))
  })
  
  tokenizer <- function(txt){
    res = txt %>% as.character() %>% str_split(",\\s*") %>% .[[1]]
    dupwords = input$text_dupwords %>% str_split("\\n") %>% .[[1]]
    stop = paste0("\\b", input$text_stopwords, "\\b") %>% str_replace_all(",\\s*","\\\\b|\\\\b")
    
    ## 중복어 합치기 
    for(i in 1:length(dupwords)){
      z = dupwords[i] %>% str_split("\\s*:\\s*")  %>% .[[1]]
      z[1] = paste0("\\b", z[1], "\\b") %>% str_replace_all(",\\s*","\\\\b|\\\\b") %>% .[[1]]
      res = str_replace_all(res, z[1], z[2])
    }
    
    ## 불용어 제거하기 
    
    return(str_remove_all(res, stop)) 
  }
  
  # dtm 
  dtm <- reactive({
    input$reset_words
    corp =  VCorpus(VectorSource(news.data.filtered.2()[[option_keyword()]])) %>% 
      tm_map(removeNumbers)
    isolate({
      withProgress({ 
        setProgress(message = "뉴스 변환 중입니다...")
        if(option_weight()){
          TermDocumentMatrix(corp, control = list(tokenize = tokenizer, wordLengths = c(2,Inf)))
        } else {
          TermDocumentMatrix(corp, control = list(tokenize = tokenizer, wordLengths = c(2,Inf),weighting = weightTfIdf))
        }
      })
    })
  })
  
  # 키워드 분석 빈발 
  table_termfreq <- reactive({
    x = dtm() %>% 
      removeSparseTerms(.995) %>% 
      as.matrix() %>%
      rowSums() %>%
      sort(decreasing = T) %>% 
      .[1:100] %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "word") 
    return(rename(x, freq = `.`))
  })
  
  # 추세분석 플롯 
  esg_trend<- reactive({
    format = switch (input$opt_date_selection,
                     "년" = "%y",
                     "월" = "%y-%m",
                     "일" = "%y-%m")
    data = news.data.filtered.2() %>% 
      mutate(기간 = format(ymd(일자), format)) %>%
      group_by(기간, esg) %>%  
      summarise(기간 = 기간, 태그 = esg, 건수 = n()) %>% 
      distinct() %>% select(기간, esg, 건수)
  })
  
  output$table_trend <- renderTable({
    data = esg_trend() 
    data %>% spread(key = esg, value = 건수)
  })
  
  output$plot_esg <- renderPlotly({
    data = esg_trend()
    ggplotly(ggplot(data = data, aes(x = 기간, y = 건수, fill = esg)) +
               geom_bar(stat="identity", width = 0.7) +
               theme_minimal())
  })
  
  # 워드 클라우드 용 
  output$table_termFreq <- renderTable({
    table_termfreq() %>% head(20)
  })
  output$wordcloud_termFreq <- renderWordcloud2({
    table_termfreq() %>% 
      wordcloud2(size = 1, minSize = 2)
  })
  
  ##  네트워크 분석용 
  # ttm 만들기 
  ttm <- reactive({
    n_words = input$slider_num_words
    names = dtm() %>% as.matrix() %>% rowSums() %>% sort(decreasing = T) %>% head(n_words) %>% names()
    dtm.mat = dtm()[names,] %>% as.matrix()
    return(dtm.mat %*% t(dtm.mat))
  })
  
  
  nodelink <- reactive({
    input$reset_network
    isolate({
      withProgress({
        setProgress(message = "네트워크 구성 중...")
        data = ttm() 
        node = data.frame(id = 1:length(unique(data %>% rownames())), label = sort(unique(data %>% rownames()))) 
        link = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("from", "to", "weight") )
        conc = concor(list(as.matrix(data)), nsplit = input$opt_concor, self_ties = FALSE, cutoff = .99, max_iter = 50)
        node = left_join(node, conc, by = c("label" = "vertex")) %>% rename(group = block)
        
        for(i in 1:nrow(data)){
          for(j in 1:(ncol(data) + 1 - i)){
            if(i < j  & data[i,j] != 0){
              link[nrow(link)+1,] = data.frame(from = node[i,1], to = node[j,1], weight = data[i,j])
            }
          }
        }
        return(list(node, link))
      })
    })
  })
  
  output$vis_network <- renderVisNetwork({
    node = nodelink()[[1]]
    link = nodelink()[[2]]
    visNetwork(node, link, submain = "네트워크 분석") %>%
      visIgraphLayout() %>% 
      visOptions(selectedBy = "group") %>%
      visInteraction(navigationButtons = TRUE)
  })
  
  output$table_node_group <- renderDataTable({
    node = nodelink()[[1]]
    node %>% group_by(group) %>% mutate(단어 = paste0(label, collapse = ", ")) %>%
      select(group, 단어) %>%  distinct()
  })
  
  
  
}# server

# Run the application 
shinyApp(ui = ui, server = server)






