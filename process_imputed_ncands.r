rm(list=ls())
gc()
library(tidyverse)
library(mice)

### first investigations
# ncands_fy<-read_csv("./data/ncands_subyr_xwalk.csv") 
# ncands_fy<-ncands_fy %>% 
#   mutate(st_id = paste(staterr, chid, sep="")) %>% 
#   select(-staterr, -chid) %>% 
#   distinct() 

index_first_investigation<-read_csv("./data/ncands_first_report_index.csv") 

index_first_victim<-read_csv("./data/ncands_first_victim_index.csv") 

files<-list.files("./data")
ncands_imputations<-files[grep("ncands_imps", files)]
ncands_imputations<-paste("./data/", ncands_imputations, sep = "")
ncands_imputations<-ncands_imputations[6:9]

first_investigations<-list()
first_victims<-list()
  
for(i in 1:length(ncands_imputations)){
  load(ncands_imputations[i])
  temp<-mice::complete(imps, action = "long", include = F)
  temp<-temp %>% 
    mutate(st_id = paste(staterr, chid, sep = "")) %>% 
    select(.imp, .id, st_id, chid, staterr, rptfips, rptdt, age, race_ethn, rptvictim) 

  first_investigations[[i]]<-index_first_investigation %>% 
    left_join(temp) %>% 
    filter(!(is.na(chid))) 
  
  first_victims[[i]]<-index_first_victim %>% 
    left_join(temp) %>% 
    filter(!(is.na(chid)))
  
  # first_investigations[[i]]<-temp_first_inv %>% 
  #   group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  #   summarise(first_inv = n()) %>% 
  #   ungroup()
  # 
  # first_victims[[i]]<-temp_first_victim %>% 
  #   filter(rptvictim==1) %>% 
  #   group_by(.imp, subyr, staterr, race_ethn, age) %>% 
  #   summarise(first_victim = n()) %>% 
  #   ungroup()
  # 
  # gc()
}

inv_out<-bind_rows(first_investigations) %>% 
  ungroup() %>%   
  filter(!staterr%in%c("PA", "GA", "RI")) %>% 
  arrange(rptdt) %>% 
  group_by(.imp, st_id) %>% 
  slice(1)

# ### identical number of unique IDs in index and imputed
# inv_out %>% 
#   group_by(.imp) %>% 
#   summarise(n = n())

temp<-expand_grid(unique(inv_out$.imp),
                  unique(inv_out$rptfips),
                  unique(inv_out$subyr),
                  unique(inv_out$age),
                  unique(inv_out$race_ethn)) 

names(temp)<-c(".imp", "rptfips", "subyr", "age", "race_ethn")

temp<-temp %>% 
  arrange(.imp, rptfips, subyr, age, race_ethn)

inv_temp<-inv_out%>% 
  group_by(.imp, subyr, rptfips, race_ethn, age) %>% 
  summarise(first_inv=n()) %>% 
  ungroup() 

inv_temp<-inv_temp%>% 
  right_join(temp) %>% 
  mutate(first_inv = ifelse(is.na(first_inv), 0, first_inv))

victim_out<-bind_rows(first_victims)%>% 
  arrange(rptdt) %>% 
  group_by(.imp, st_id) %>% 
  slice(1)

temp<-expand_grid(unique(victim_out$.imp),
                  unique(victim_out$rptfips),
                  unique(victim_out$subyr),
                  unique(victim_out$age),
                  unique(victim_out$race_ethn)) 

names(temp)<-c(".imp", "rptfips", "subyr", "age", "race_ethn")

victim_temp<-victim_out %>% 
  group_by(.imp, subyr, rptfips, race_ethn, age) %>% 
  summarise(first_victim=n())

victim_temp<-victim_temp %>% 
  ungroup() %>% 
  right_join(temp) %>% 
  mutate(first_victim = ifelse(is.na(first_victim), 0, first_victim))

write_csv(inv_temp, "./data/county_first_inv.csv")
write_csv(victim_temp, "./data/county_first_victim_out.csv")