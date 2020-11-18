rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

source("lifetable.r")

pop<-read_fwf("./data/us.1990_2018.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop"))) 

pop<-pop%>%
  mutate(year = as.integer(year)) %>% 
  filter(year>=2014) %>% 
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AI/AN",
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) 

### make county id 
pop_fips<-pop %>%
  mutate(fipscode = as.numeric(
    paste(st_fips, cnty_fips,
          sep = "")))%>% 
  mutate(fipscode =
           case_when(fipscode==36047 ~ 36061,
                     fipscode==36005 ~ 36061,
                     fipscode==36081 ~ 36061,
                     fipscode==36085 ~ 36061,
                     T ~ fipscode)) ## mega new york to match afcars

top_pops<-pop_fips %>%
  ungroup() %>% 
  group_by(fipscode) %>%
  summarise(pop = sum(pop)) %>%
  arrange(desc(pop)) %>%
  ungroup() %>%
  mutate(rank = 1:n()) %>%
  filter(rank<=20) 

write_csv(top_pops, "./data/top_pops.csv")

pop_fips <- pop_fips %>% 
  ungroup() %>% 
  filter(fipscode%in%top_pops$fipscode) %>% 
  filter(age<=18) %>% 
  mutate(age = as.integer(age)) %>% 
  group_by(year, fipscode, age, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

### read ncands data
### make NYC mega county
### to match AFCARS aggregation

inv<-read_csv("./data/county_first_inv.csv") %>% 
  rename(year = subyr, 
         fipscode = rptfips) %>% 
  mutate(fipscode =
           case_when(fipscode==36047 ~ 36061,
                     fipscode==36005 ~ 36061,
                     fipscode==36081 ~ 36061,
                     fipscode==36085 ~ 36061,
                     T ~ fipscode)) %>% 
  group_by(.imp, year, fipscode, race_ethn, age) %>% 
  summarise(first_inv = sum(first_inv)) %>% 
  ungroup()

victim<-read_csv("./data/county_first_victim.csv") %>% 
  rename(year = subyr, 
         fipscode = rptfips) %>% 
  mutate(fipscode =
           case_when(fipscode==36047 ~ 36061,
                     fipscode==36005 ~ 36061,
                     fipscode==36081 ~ 36061,
                     fipscode==36085 ~ 36061,
                     T ~ fipscode)) %>% 
  group_by(.imp, year, fipscode, race_ethn, age) %>% 
  summarise(first_victim = sum(first_victim)) %>% 
  ungroup()

### read afcars data

fc<-read_csv("./data/county_first_fc.csv") %>% 
  rename(year = fy)
tpr<-read_csv("./data/county_tpr.csv") %>% 
  rename(year = fy)

## join to pop
dat<-pop_fips %>% 
  left_join(victim) %>% 
  left_join(inv) %>% 
  left_join(fc) %>% 
  left_join(tpr)


check<-dat %>% 
  filter(.imp==1) %>% 
  group_by(year, fipscode, race_ethn) %>% 
  summarise(pop = sum(pop),
            first_victim = sum(first_victim),
            first_inv = sum(first_inv),
            first_entry = sum(first_entry),
            tpr = sum(tpr))

## set up for lifetable loop
tab_dat<-dat %>% 
  pivot_longer(names_to = "varname",
               values_to = "var",
               cols = first_victim:tpr)

## set up as 5 year total for life table
tab_dat<-tab_dat %>% 
  ungroup() %>% 
  group_by(.imp, fipscode, age, race_ethn, varname) %>% 
  summarise(pop = sum(pop),
            var = sum(var)) %>% 
  ungroup()

tot_dat<-tab_dat %>% 
  group_by(.imp, fipscode, age, varname) %>% 
  summarise(pop = sum(pop),
            var = sum(var)) %>% 
  mutate(race_ethn = "Total")

tab_dat<-tab_dat %>% 
  bind_rows(tot_dat)

### run life tables by imp, race_ethnb, sex
vars<-unique(tab_dat$varname)
race<-unique(tab_dat$race_ethn)
fips<-unique(tab_dat$fipscode)
tables_out<-list()

counter<-0
for(h in 1:length(vars)){
  for(i in 1:max(tab_dat$.imp)){
    for(r in 1:length(fips)){
      for(y in 1:length(race)){
        counter<-counter + 1
        
        temp<-tab_dat %>%
          filter(.imp == i,
                 varname == vars[h],
                 fipscode == fips[r],
                 race_ethn == race[y])
        
        tables_out[[counter]]<-make_life_table(temp)
      }
    }
  }
}

tables<-bind_rows(tables_out)

#### combine across imps
tables_within<-tables %>%
  group_by(race_ethn, fipscode, age, varname) %>%
  summarise(c_mn = mean(c),
            v_within = mean(se^2))

tables_between<-tables %>%
  left_join(tables_within) %>%
  group_by(race_ethn, fipscode, age, varname) %>%
  summarise(v_between = mean((c - c_mn)^2))

tables_comb<-tables_within %>%
  left_join(tables_between) %>%
  mutate(se_tot = sqrt(v_within + (1 + 1/5)*v_between)) %>%
  select(fipscode, varname, race_ethn, age, c_mn, se_tot)

tables_comb<-tables_comb %>%
  mutate(c_upr = c_mn + 1.96 * se_tot,
         c_lwr = c_mn - 1.96 * se_tot) %>% 
  filter(age==18) %>% 
  mutate(c_upr = ifelse(c_upr>1, 1, c_upr),
         c_lwr = ifelse(c_lwr<0, 0, c_lwr))

### format names
tables_comb<-tables_comb %>% 
  mutate(county = 
           case_when(
             fipscode==4013 ~ "AZ: Maricopa",
             fipscode==6001 ~ "CA: Alameda",
             fipscode==6037 ~ "CA: Los Angeles",
             fipscode==6059 ~ "CA: Orange",
             fipscode==6065 ~ "CA: Riverside",
             fipscode==6071 ~ "CA: San Bernadino",
             fipscode==6073 ~ "CA: San Diego",
             fipscode==6085 ~ "CA: Santa Clara",
             fipscode==12011 ~ "FL: Broward",
             fipscode==12086 ~ "FL: Miami-Dade",
             fipscode==17031 ~ "IL: Cook",
             fipscode==25017 ~ "MA: Middlesex",
             fipscode==26163 ~ "MI: Wayne",
             fipscode==32003 ~ "NV: Clark",
             fipscode==36061 ~ "NY: New York",
             fipscode==48029 ~ "TX: Bexar",
             fipscode==48113 ~ "TX: Dallas",
             fipscode==48201 ~ "TX: Harris",
             fipscode==48439 ~ "TX: Tarrant",
             fipscode==53033 ~ "WA: King"
           ))


write_csv(tables_comb, "./vis/county_tables.csv")
