library(data.table)
library(tidyverse)
library(lubridate)

raw_dt = data.table(raw)

cols_ = c("장치구분","검지기","배터리","장애","RSSI","SNR","동작")

raw_dt 

rare = raw_dt[, .(센서, 센서값, 일시 = `송신일시(sendDate)`)
              
             # 텍스트 값 trim 
             ][,센서값 := str_remove_all(센서값, "[[a-zA-Z]|[가-힣]|[\\s]]+: ")
             ][,센서 := str_remove_all(센서, "[주차센서_|졸음쉼터]")
                
               
              # 값 분절하기 
             ][,(cols_) := tstrsplit(센서값, ",")
             ][,검지기 := str_remove_all(검지기, "차량")
             
              # 열 선택 
             ][,.(센서, 검지기, 장애, 동작, 일시)
              
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
              
              ][,.(센서, 검지기, 일시, 오류구분)]



## 에러 요약 error summary 
rare[!duplicated(rare),
     ][,.(정상 = sum(오류구분 == 0),
            주기보고 = sum(오류구분 == 1),
            통신기기오류 =  sum(오류구분 == 2),
            중복오류 = sum(오류구분 == 3)),
          keyby = (졸음쉼터 = str_sub(센서,1,2))]


## 이벤트 테이블로 변환 
evt = rare[오류구분 == 0
                ][,출차시각 := fifelse(검지기 == "주차", shift(일시,type = "lead"),"0")
                ][출차시각 != "0"
                ][,c("주차시각", "출차시각") := lapply(.SD, as_datetime), .SDcols = c("일시", "출차시각") 
                ][,체류시간 := as.numeric(출차시각 - 주차시각)
                ][,.(센서, 주차시각, 출차시각, 체류시간)]

## 이벤트 요약 


## 점유율 구하는 거 
share = evt %>% map_df(rep, times = evt$t + 1) %>% data.table() 

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
      ][,.(점유시간 = round(sum(점유시간)/3600*100, digits = 2)), by = .(센서, 시간 = 시작시각)]

share_ = dcast(share, 시간 ~ 센서, value.var = ("점유시간"), fill = 0)

## share daily

share_daily = share[,.(점유율 = mean(점유시간)), by = .(센서, 시간 = hour(시간))
                    ][order(센서,시간)]

