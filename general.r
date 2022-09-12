time_share <- function(from, to, diff) {
  diff = as.numeric(diff)
  time.from = floor_date(from, unit = "hour")
  time.to = floor_date(to, unit = "hour")
  time.seq = seq.POSIXt(
             time.from,
             time.to, 
             by = "hour")
  n = length(time.seq) 
  
  frac.start = difftime(ceiling_date(from, unit = "hour"), from, unit = 'sec') %>% as.numeric()
  frac.end = difftime(to, floor_date(to, unit = "hour"), units = 'sec') %>% as.numeric()
  
  if(n == 1){
    setNames(c(diff), c(time.from)) %>% as.data.frame()}
  else{
    setNames(c(frac.start, rep(3600, n-2), frac.end), c(time.seq)) %>% as.data.frame()}
}


pmap_dfr(list(from = event.s$주차시각, to = event.s$출차시각, diff = event.s$점유시간), time_share) %>% 
  rownames_to_column("time") %>% group_by(time = str_sub(time, 1, 19)) %>% 
  summarise(share_time = sum(.,na.rm = T))
