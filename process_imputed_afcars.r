library(tidyverse)

### first investigations
afcars<-read.csv("./data/afcars_imputed_all_cases.csv", 
                 stringsAsFactors = F) %>% 
  filter(fipscode!=8)
afcars_xwalk<-read.csv("./data/afcars_id_xwalk.csv",
                       stringsAsFactors = F) %>% 
  filter(fipscode!=8)

afcars_fy<-afcars %>% 
  filter(totalrem==1, entered==1) %>% 
  left_join(afcars_xwalk %>% 
              select(fy, stfcid) %>% 
              distinct()) %>% 
  filter(fy>=2014) %>% 
  group_by(.imp, fipscode, fy, age, race_ethn) %>% 
  summarise(first_entry = n()) 
# 
# afcars_rem1<-afcars %>% 
#   filter(totalrem==1, entered==1) %>% 
#   group_by(.imp, state, fipscode, year, age, race_ethn) %>% 
#   summarise(first_entry = n())

afcars_tpr<-afcars %>% 
  filter(istpr==1) %>% 
  left_join(afcars_xwalk %>% 
              select(fy, stfcid) %>% 
              distinct()) %>% 
  group_by(stfcid, .imp) %>% 
  filter(fy>=2014) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct() %>% 
  group_by(.imp, fipscode, fy, age, race_ethn) %>% 
  summarise(tpr = n()) %>% 
  ungroup() 

### complete the zeroes
temp<-expand_grid(unique(afcars_tpr$.imp),
                  unique(afcars_tpr$fipscode),
                  unique(afcars_tpr$fy),
                  unique(afcars_tpr$age),
                  unique(afcars_tpr$race_ethn)) 

names(temp)<-c(".imp", "fipscode", "fy", "age", "race_ethn")

temp_tpr<-left_join(temp,
                afcars_tpr) %>% 
  mutate(tpr = ifelse(is.na(tpr), 0, tpr)) %>% 
  arrange(.imp, fipscode, fy, age, race_ethn)

temp_entry<-left_join(temp,
                      afcars_fy) %>% 
  mutate(first_entry = ifelse(is.na(first_entry), 0, first_entry)) %>% 
  arrange(.imp, fipscode, fy, age, race_ethn)

write_csv(temp_entry, "./data/county_first_fc.csv")
write_csv(temp_tpr, "./data/county_tpr.csv")
