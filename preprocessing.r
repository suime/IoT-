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
file = paste0("d:/2022/IoT/1. preproc/1-7.xlsx")

sensor = read_xlsx(file)

sens1 = sensor %>%
  rename(일시 = `송신일시(sendDate)`) %>% 
  select(c(센서, 센서값, 일시)) %>% 
  
  #센서값 열 분할 
  
  mutate(센서값 = str_remove_all(센서값, "[[a-zA-Z]|[가-힣]|[\\s]]+: ")) %>% 
  separate(센서값, 
              sep = ",", 
              into = c("장치구분", "검지기", "배터리", "장애", "RSSI", "SMR", "동작"), 
              convert = T, 
              extra = "merge") %>% 
  mutate(장애 = ifelse(장애 == "51.0", "리셋발생", 장애),
           동작 = case_when(
             동작 %in% c("2.0","오류") ~ "LoRa Reset 수신 후 부팅",
              동작 == "3.0" ~ "LoRa 전송오류 등으로 인한 자체 시스템 Reset",
              TRUE ~ 동작
           )) %>% 
  arrange(센서, 일시)


## 2. 전처리 과정 


write.csv(sens1, "d:/test_sens1.csv", row.names = F)
# 중복 행 제거 
sens1 = sens1[!duplicated(sens1),]  # 317396 -> 중복된 행 하나 제거 


# 오류 항목 구분 
# 0 : 정상, 1: 주기보고, 2 : 오류 메시지,  3: 중복 항목 및 최초 마지막 
sensor.name = sens1$센서 %>% table() %>% names()

sens1_ = sens1 %>% select(센서, 검지기, 동작, 일시) %>% 
  arrange(센서, 일시) %>% 
  mutate(오류구분 = case_when(
    동작 == "주차이벤트" ~ 0, 
    동작 == "주기보고" ~ 1,
    TRUE  ~ 2 ))  

#sens1_$동작 %>% table()

sens2_ = sens1_ %>% filter(오류구분 != 2) %>% 
  mutate(오류구분 = case_when(
    검지기 == "차량주차" & lead(오류구분) == 1 & 오류구분 == 0 ~ 3,
    lag(오류구분, default = 0) == 1  ~  3, 
    TRUE ~ 오류구분)) %>% 
  filter(오류구분 != 1) %>% 
  mutate(오류구분 = case_when(
    센서 == lead(센서, default = last(sensor.name)) & 검지기 == "차량주차" & 검지기 == lead(검지기, default = "차량주차") ~ 3,
    센서 == lag(센서, default = first(sensor.name)) & 검지기 == "차량출차" & 검지기 == lag(검지기, default = "차량출차") ~ 3,
    센서 == lead(센서, default = last(sensor.name)) & month(as_datetime(일시)) != lead(month(as_datetime(일시))) & 검지기 == "차량주차"  ~ 3,
    센서 == lag(센서, default = first(sensor.name)) & month(as_datetime(일시)) != lag(month(as_datetime(일시))) & 검지기 == "차량출차"  ~ 3,
    센서 != lag(센서) & 검지기 == "차량출차"  ~ 3,
    센서 != lead(센서) & 검지기 == "차량주차"  ~ 3,
    TRUE ~ 오류구분
  )) 

error_1 =  sens1_ %>% group_by(센서) %>% summarise(주기보고 = sum(오류구분 == 1), 에러 = sum(오류구분 == 2)) %>%
  bind_cols(sens2_ %>%  group_by(센서) %>% summarise(정상이벤트 = sum(오류구분 == 0), 중복이벤트 = sum(오류구분 == 3)) %>% select(정상이벤트, 중복이벤트)) %>%
  mutate(합계 = 정상이벤트 + 주기보고 + 에러 + 중복이벤트) %>% 
  select(센서, 정상이벤트, 주기보고, 에러, 중복이벤트, 합계)


## 3. 에러 요약 파일 출력  
write.csv(error_1, paste0("d:/2022/IoT/1. preproc/error_summary_", f,".csv"), row.names = F)

## 4. 점유시간 출력 
sens3 = sens2_ %>%
  filter(오류구분 == 0) %>% 
  mutate(출차시 = ifelse(검지기 == "차량주차" & 센서 == lead(센서), lead(일시), NA)) %>% 
  filter(!is.na(출차시)) %>% 
  mutate(주차시 = as_datetime(일시)) %>% 
  mutate(
    점유시간 = as_datetime(출차시) - 주차시,
    요일 = weekdays(as_datetime(주차시)),
    기간구분 = case_when(
               as_date(주차시) >= ymd("2022.01.28") & as_date(주차시) <= ymd("2022.02.02")   ~ "설연휴",
               요일 %in% c("토요일", "일요일") |  as_date(주차시) %in% datelist ~ "휴일",
               TRUE ~ "평일"),
    주야구분 = ifelse(hour(주차시) >= 7 & hour(주차시) < 19, "낮", "밤")
             ) %>% 
  select(센서, 요일, 기간구분, 주야구분, 주차시, 출차시, 점유시간) %>% 
  separate(센서, sep = "#", 
             into = c("졸음쉼터", "검지기번호"), 
             convert = T, 
             extra = "merge")

write.csv(sens3, paste0("d:/2022/IoT/1. preproc/usage_", f,".csv"), row.names = F)
print(paste0(f," : 완료"))


# 이용횟수를 알아보기 위한 일단위 이용횟수 종합 
sens3.daily = sens2_ %>% 
  filter(오류구분 == 0) %>% 
  mutate(출차시 = ifelse(검지기 == "차량주차" & 센서 == lead(센서), lead(일시), NA)) %>% 
  filter(!is.na(출차시)) %>% 
  mutate(주차시 = as_datetime(일시)) %>% 
  group_by(센서, 일자 = as_date(주차시)) %>% 
  summarise(이용횟수 = n(),
                평균이용시간_분 = mean(as_datetime(출차시) - as_datetime(주차시))/60)  %>% 
  mutate(기간구분 = case_when(
    일자 >= ymd("2022.01.28") & 일자 <= ymd("2022.02.02")   ~ "설연휴",
    weekdays(일자) %in% c("토요일", "일요일") |  일자 %in% datelist ~ "휴일",
    TRUE ~ "평일")) %>% 
  select(일자, 센서, 기간구분, 이용횟수, 평균이용시간_분)


write.csv(sens3.daily, paste0("d:/2022/IoT/1. preproc/usage_daily_summary.csv"), row.names = F, fileEncoding = "utf-8")



sens3 %>% group_by(졸음쉼터, 기간구분, 주야구분) %>%
  summarise(
    횟수 = n(),
    평균시간 = mean(점유시간), 
    중간값 = median(점유시간), 
    최대점유 = max(점유시간), 
    최소점유 = min(점유시간), 
    표준편차 = sd(점유시간)) %>% 
  write.csv("d:/2022/IoT/9. summary/이용요약.csv", row.names = F)

#}

## T 검정 및 분산 분석 

# 주말, 공휴일 / 명절 연휴 (설날) / 주중 
# 밤, 낮 구분해서 차이가 있는지 
# 점유 시간 / 진입 차 기준 

# 주말 | 주중 
t.test(sens3 %>% filter(휴일 == "주말") %>% select(점유시간), sens3 %>% filter(휴일 == "주중") %>% select(점유시간) )

# 휴일 | 설연휴 
t.test(sens3.daily$이용횟수[sens3.daily$기간구분 == "설연휴"], sens3.daily$이용횟수[sens3.daily$기간구분 == "휴일"], var.equal = T)

## 


