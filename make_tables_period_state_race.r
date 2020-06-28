rm(list=ls()); gc()
library(tidyverse)
library(lubridate)
library(maps)
state_map<-map_data("state")

source("lifetable.r")

pop<-read_fwf("./data/us.1990_2017.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))

afcars<-read_csv("./data/afcars_imputed_all_cases.csv") %>% 
  filter(year>=2008) 

ncands<-read_csv("./data/ncands_imputed.csv")

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AI/AN", 
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age))

### get state pop
pop_st <- pop %>% 
  filter(age<18, year>=2008) %>% 
  group_by(state, st_fips, age, race_ethn) %>% 
  summarise(pop = sum(pop))


### format NCANDS

ncands_temp<-ncands %>% 
  filter(.imp==0)

ncands_temp<-ncands_temp %>% 
  filter(chage<=18) %>% 
  filter(!(staterr%in%c("XX", "PR"))) %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  arrange(rptdt) 

### obtain min(rptdate) when victim == 1
ncands_index<-ncands_temp %>% 
  filter(rptvictim==1) %>% 
  select(st_id, rptdt) %>% 
  group_by(st_id) %>% 
  summarise(rptdt = min(rptdt)) %>% 
  mutate(first_victim = TRUE)

ncands_first_victim<-ncands  %>% 
  filter(.imp!=0) %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  left_join(ncands_index) %>% 
  filter(!(is.na(first_victim))) %>% 
  group_by(.imp, staterr, chage, race_ethn) %>% 
  summarise(var = n())

ncands_first_victim<-ncands_first_victim %>% 
  ungroup() %>% 
  complete(race_ethn, chage, staterr,
           .imp,   
           fill = (list(var = 0))) 

### first investigation
ncands_temp<-ncands_temp %>% 
  filter(chage<=18) %>% 
  filter(!(staterr%in%c("XX", "PR"))) %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  arrange(rptdt) 

### obtain min(rptdate) when victim == 1
ncands_index<-ncands_temp %>% 
  select(st_id, rptdt) %>% 
  group_by(st_id) %>% 
  summarise(rptdt = min(rptdt)) %>% 
  mutate(first_rpt = TRUE)

ncands_first_rpt<-ncands  %>% 
  filter(.imp!=0) %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  left_join(ncands_index) %>% 
  filter(!(is.na(first_rpt))) %>% 
  group_by(.imp, staterr, chage, race_ethn) %>% 
  summarise(var = n())

ncands_first_rpt<-ncands_first_rpt %>% 
  ungroup() %>% 
  complete(race_ethn, chage, staterr,
           .imp, 
           fill = (list(var = 0))) 

### harmonize names
ncands_first_victim<-ncands_first_victim %>% 
  rename(age = chage,
         state = staterr) %>% 
  mutate(varname = "first_victim")

ncands_first_rpt<-ncands_first_rpt %>% 
  rename(age = chage,
         state = staterr) %>% 
  mutate(varname = "first_rpt")

ncands_tab_dat<-bind_rows(ncands_first_rpt,
                          ncands_first_victim) %>% 
  filter(age<18)

################ Set up afcars

afcars_st_year<-afcars %>%
  filter(totalrem==1 & entered==1) %>% # subset to first entries
  filter(age<18) %>% 
  filter(.imp!=0) %>%
  filter(year>=2008) %>% 
  group_by(.imp, state, age, race_ethn) %>%
  summarise(first_fc = n()) %>%
  ungroup() %>%
  complete(.imp, age, state, race_ethn,
           fill = list(first_fc=0)) 

### make tpr measure as first year where istpr=T
afcars_new_tpr<-afcars %>% 
  filter(age<18) %>% 
  filter(.imp>0) %>% 
  filter(year>=2008) %>% 
  filter(istpr==1) %>% 
  group_by(.id, .imp) %>% 
  summarise(newtpr=min(year)) %>% 
  mutate(newtpr=T) %>% 
  select(.id, .imp, newtpr)

afcars_tpr<-afcars %>% 
  left_join(afcars_new_tpr) %>% 
  filter(age<18) %>% 
  filter(.imp!=0) %>%
  filter(year>=2008) %>% 
  group_by(.imp, state, age, race_ethn) %>%
  summarise(tpr = sum(newtpr, na.rm=T)) %>%
  ungroup() %>%
  complete(.imp, age, state, race_ethn,
           fill = list(tpr=0)) 
  
afcars_tpr<-afcars_tpr %>% 
  rename(var = tpr) %>% 
  mutate(varname = "tpr")

afcars_st_year<-afcars_st_year %>% 
  rename(var = first_fc) %>% 
  mutate(varname = "first_fc") %>% 
  bind_rows(afcars_tpr)


######## BIND IT ALL
afcars_st_year<-afcars_st_year %>% 
  filter(state!=72) %>% 
  ungroup() %>% 
  rename(st_fips = state) %>% 
  left_join(pop_st %>% 
              ungroup() %>% 
              mutate(st_fips = as.numeric(st_fips)))

ncands_tab_dat<-ncands_tab_dat %>% 
  left_join(pop_st)

tab_dat<-bind_rows(afcars_st_year,
                   ncands_tab_dat %>% 
                     mutate(st_fips = as.numeric(st_fips)))

# make total
tab_dat<-tab_dat %>%
  group_by(.imp, state, st_fips, varname, age) %>% 
  summarise(var = sum(var), pop = sum(pop)) %>% 
  mutate(race_ethn="Total") %>% 
  ungroup() %>% 
  bind_rows(tab_dat) %>% 
  filter(!(is.na(age)), !(is.na(race_ethn)),
         .imp<6)

### run life tables by imp, race_ethn, sex
vars<-unique(tab_dat$varname)
race<-unique(tab_dat$race_ethn)
state<-unique(tab_dat$state)
tables_out<-list()

counter<-0
for(h in 1:length(vars)){
  for(i in 1:5){
    for(r in 1:length(state)){
      for(y in 1:length(race)){
        counter<-counter + 1
        
        temp<-tab_dat %>%
          filter(.imp == i)
        
        temp<-temp %>% 
                 filter(varname == vars[h])
        
        temp<-temp %>% 
                 filter(race_ethn == race[y])
        
        state_temp<-state[r]
        
        temp<-temp %>% 
          filter(state == state_temp)
        
        tables_out[[counter]]<-make_life_table(temp)
      }
    }
  }
}

tables<-bind_rows(tables_out)

#### combine across imps
tables_within<-tables %>%
  group_by(race_ethn, state, age, varname) %>%
  summarise(c_mn = mean(c),
            v_within = mean(se^2))

tables_between<-tables %>%
  left_join(tables_within) %>%
  group_by(race_ethn, state, age, varname) %>%
  summarise(v_between = mean((c - c_mn)^2))

tables_comb<-tables_within %>%
  left_join(tables_between) %>%
  mutate(se_tot = sqrt(v_within + (1 + 1/5)*v_between)) %>%
  select(state, varname, race_ethn, age, c_mn, se_tot)

tables_comb<-tables_comb %>%
  mutate(c_upr = c_mn + 1.96 * se_tot,
         c_lwr = c_mn - 1.96 * se_tot) %>% 
  filter(age==17)

tables_comb<-tables_comb %>% 
  mutate(varname = case_when(
    varname=="tpr" ~ "Termination of Parental Rights",
    varname=="first_fc" ~ "Foster Care",
    varname=="first_rpt" ~ "Investigation",
    varname=="first_victim" ~ "Confirmed Maltreatment",
    T ~ varname
  )) %>% 
  mutate(varname = factor(varname, 
                          levels = c("Investigation",
                                     "Confirmed Maltreatment",
                                     "Foster Care",
                                     "Termination of Parental Rights")))

write_csv(tables_comb, "./vis/st_tables.csv")

q("no")
