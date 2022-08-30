library(tidyverse)
library(readxl)
library(lubridate)
# 공휴일 목록 
datelist = c( #"2022-01-01", "2022-01-31", "2022-02-01", "2022-02-02",
  "2022-03-01", "2022-05-05", "2022-05-08", "2022-06-06",
  "2022-08-15", "2022-09-09", "2022-09-10", "2022-09-11",
  "2022-09-12", "2022-10-03", "2022-10-09", "2022-10-10", "2022-12-25") %>% 
  ymd()

#for(f in 1:8){

## 1. 센서 원본 파일 읽기
f = "1-7"
root = "d:/2022/IoT/1. preproc/"
file = paste0(root, "1-7.xlsx")

raw = read_xlsx(file)

rare = raw %>%
  rename(일시 = `송신일시(sendDate)`) %>% 
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
  mutate(센서 = str_remove(센서, "^주차센서_")) %>% 
  
  # 중복, 결측치 제거 
  distinct() %>% 
  na.omit() %>% 
  
  # 정렬 
  arrange(센서, 일시)


write.csv(rare, paste0(root, "rare.csv"), row.names = F)



## 2. 전처리 과정 

sensor.name = rare$센서 %>% table() %>% names()
# 오류 항목 구분 
# 0 : 정상, 1: 주기보고, 2 : 오류 메시지,  3: 중복 항목 및 최초 마지막  4: 주기보고 이후 오류 

error_tagged = rare %>%
  select(센서, 검지기, 동작, 일시) %>% 
  
  mutate(오류구분 = case_when(
    동작 == "주차이벤트" ~ 0, 
    동작 == "주기보고" ~ 1,
    TRUE  ~ 2 ))  %>%
  
  mutate(오류구분 = case_when(
    # 주기보고 다음 발생한 오류 
    lag(오류구분, default = 0) == 1  ~  4, 
    
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

#error_tagged$동작 %>% table()
#error_tagged$오류구분 %>% table()

## 2. Error_Tagged 테이블 출력 
write.csv(error_tagged, paste0(root, "error_tagged.csv"), row.names = F)






## 3. 에러 요약 테이블 
error_summary = error_tagged %>% 
  
  group_by(센서) %>%
  
  summarise(
    정상 = sum(동작 == "주차이벤트" & 오류구분 == 0), 
    
    # 주기보고 및 이후 첫번째 이벤트 오류 
    주기보고 = sum(오류구분 == 1), 
    보고오류 = sum(오류구분 == 4),
    
    # LoRa망 관련 오류 
    통신오류 = sum(str_detect(동작,"^LoRa") & 오류구분 == 2),
    
    #센서오류
    센서오류 = sum(str_detect(동작,"^레이더") & 오류구분 == 2),
    
    #초기부팅 
    초기부팅 = sum(동작 == "초기 부팅"),
    
    
    # 이벤트 오류 
    이벤트오류 = sum(오류구분 == 3),
    
    # 합계 
    합계 = sum(c_across(정상:이벤트오류))
    ) 
  
### 3.1 Error_Summary 테이블 출력 
write.csv(error_summary, paste0(root, "error_summary.csv"), row.names = F)

### 3.2 이벤트 오류 다발 시점 분석용 
error_tagged %>% 
  filter(오류구분 == 3) %>%
  mutate(일자 = 일시 %>% as_date()) %>% 
  group_by(일자,센서) %>% summarise(센서 = 센서, 일자 = 일자, n = n()) %>% 
  distinct() %>%
  write.csv("d:/오류.csv")



## 4. 이벤트 테이블 
event_table = error_tagged %>%
  mutate(일시 = as_datetime(일시)) %>% 
  
  # 정상인 것만 필터
  filter(오류구분 == 0) %>% 
  
  
  # 출차시각 
  mutate(출차시각 = as_datetime(ifelse(검지기 == "차량주차" & 센서 == lead(센서), lead(일시), NA))) %>% 
  
  filter(!is.na(출차시각)) %>% 
  
  rename(주차시각 = 일시) %>% 
  
  mutate(
    점유시간 = 출차시각 - 주차시각,
    요일 = weekdays(as_datetime(주차시각)),
    기간구분 = case_when(
      주차시각 >= ymd("2022.01.28") & 주차시각 <= ymd("2022.02.02") ~ "설연휴",
      요일 %in% c("토요일", "일요일") |  as_date(주차시각) %in% datelist ~ "휴일",
      TRUE ~ "평일"),
    주야구분 = ifelse(hour(주차시각) >= 7 & hour(주차시각) < 19, "낮", "밤")
  ) %>% 

  select(센서, 요일, 기간구분, 주야구분, 주차시각, 출차시각, 점유시간) %>% 
  
  separate(센서, sep = "졸음쉼터_#", 
             into = c("졸음쉼터", "검지기번호"), 
             convert = T, 
             extra = "merge",
             remove = F) 



### 4.1 이벤트 테이블 출력 
write.csv(event_table, paste0(root,"event_table.csv"), row.names = F)

### 4.2 이벤트 요약 테이블 출력 
event_summary = 
  event_table %>%
  group_by(센서) %>% 
  summarise(
    
    # 전체 이벤트 발생
    전체_이벤트_발생수 = n(),
    
    # 이용시간 관련 
    이용시간_평균 = round(mean(점유시간)/60),
    이용시간_최대 = round(max(점유시간)/60),
    이용시간_최소 = round(min(점유시간)/60,1)
  ) %>% 
  bind_cols(
    # 일평균 이용 횟수 
    event_table %>% group_by(센서, as_date(주차시각)) %>% 
      summarise(n = n()) %>% 
      group_by(센서) %>% 
      summarise(
        이용횟수_평균 = round(mean(n)),
        이용횟수_최대 = max(n),
        ) %>% 
      select(starts_with("이용"))
  )
  
write.csv(event_summary, paste0(root,"event_summary.csv"), row.names = F)


## 5.0 점유율 테이블 

# 일단 열을 만들기 

time = seq.POSIXt(
    from = event_table$주차시각 %>% min(),
    to = event_table$출차시각 %>% max() + 60**2,
    by = "hour") %>%
  format.Date("%Y-%m-%d %H:00") %>%
  unique() %>% 
  as.character()


test = data.frame(matrix(0,nrow = length(time), ncol = length(sensor.name) + 1))
colnames(test) = c("time", sensor.name)
test$time = time


for(i in 1:nrow(event_table)) {
  
  sensor = event_table$센서[i]
  from = event_table$주차시각[i]
  end = event_table$출차시각[i]
  stime = event_table$점유시간[i]

  start_hour = format.Date(from, "%Y-%m-%d %H:00")
  end_hour = format.Date(end, "%Y-%m-%d %H:00")
  
  # 1번 : 주차시각과 출차시각이 같다면 점유시간 그대로 플러스 
  if(start_hour == end_hour){
      test[time == start_hour, sensor] <- test[time == start_hour, sensor] + stime
      } 
  # 2번 : 주차시각과 출차시각이 다를 때 
  else if(start_hour != end_hour){
    start_frac = 3600 - {(from %>% format.Date("%M") %>% as.numeric() * 60) + (from %>% format.Date("%S") %>% as.numeric())}
    end_frac = (end %>% format.Date("%M") %>% as.numeric() * 60) + (end %>% format.Date("%S") %>% as.numeric())
    
    # 시작 시간에 
    test[time == start_hour, sensor] <- test[time == start_hour, sensor] + start_frac
    
    # 마지막 시간에 
    test[time == end_hour, sensor] <- test[time == end_hour, sensor] + end_frac
    
    # 가운데 시간에 + 3600
    if(as.POSIXct(format(from + 3600*2, "%Y-%m-%d %H:00:00"), tz = "UTC") <= end ){
      for(mid_time in seq.POSIXt(as.POSIXct(format(from + 3600, "%Y-%m-%d %H:00:00"), tz = "UTC"),
                                 to = end - 3600,
                                 by = "hour")){
        test[time == format(mid_time %>% as_datetime(), "%Y-%m-%d %H:00"), sensor] <- 3600
      }
    }
      
    print(paste(i, "diff_ finished...", start_frac, "  |  ", end_frac))
    
  }
}


test[,-1] = test[,-1]/3600 
test %>% summary

### 5.1 점유율 테이블 출력 
write.csv(test, paste0(root,"share_table.csv"), row.names = F)

share_table = test
## 5.2 점유율 시간 요약 테이블 
share_summary = share_table %>% 
  group_by(t = as_datetime(paste0(time, ":00")) %>% hour()) %>% 
  summarise(across(2:29, list(mean))) %>% t() 

colnames(share_summary) = share_summary[1,]
rownames(share_summary) = rownames(share_summary) %>% str_remove_all("_1")
## 점유율 요약 출력  
write.csv(share_summary[-1,], paste0(root,"1share_summary.csv"), row.names = T,)
