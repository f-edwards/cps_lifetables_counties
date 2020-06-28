rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

source("lifetable.r")

afcars<-read_csv("./data/afcars_imputed",
                 col_types = cols(stfcid = "c"))

afcars<-afcars %>% 
  mutate(race_ethn = 
           ifelse(race_ethn == "AI/AN", 
                  "American Indian / Alaska Native",
                  race_ethn),
         race_ethn = 
           ifelse(race_ethn == "Asian/PI",
                  "Asian / Pacific Islander",
                  race_ethn))

pop<-read_fwf("./data/us.1990_2017.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))
pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "American Indian / Alaska Native",
             race==4 ~ "Asian / Pacific Islander",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age)) %>%
  filter(year>=2000)

pop_nat <- pop %>%
  group_by(year, age,  race_ethn) %>%
  summarise(pop = sum(pop))

afcars_nat_year<-afcars %>%
  filter(.imp!=0) %>%
  group_by(.imp, year, age, race_ethn) %>%
  summarise(first_fc = n()) %>%
  ungroup() %>%
  complete(.imp, year, age, race_ethn,
           fill = list(first_fc=0)) %>%
  left_join(pop_nat)

afcars_nat_year<-afcars_nat_year %>%
  ungroup()

### format afcars for lifetable
dat<-afcars_nat_year%>%
  rename(var = first_fc)
# add total to table
dat<-dat %>%
  bind_rows(afcars_nat_year %>%
              group_by(.imp,year, age) %>%
              summarise(var= sum(first_fc),
                        pop = sum(pop)) %>%
              mutate(race_ethn = "Total"))

### run life tables by imp, race_ethnb, sex
race<-unique(dat$race_ethn)
sex<-unique(dat$sex)
years<-unique(dat$year)
tables_out<-list()

counter<-0
for(i in 1:5){
  for(r in 1:length(race)){
    for(y in 1:length(years)){
      counter<-counter + 1

      temp<-dat %>%
        filter(.imp == i,
               race_ethn == race[r],
               year == years[y])

      tables_out[[counter]]<-make_life_table(temp)
    }
  }
}

tables<-bind_rows(tables_out)

#### combine across imps
tables_within<-tables %>%
  group_by(year, age, race_ethn) %>%
  summarise(c_mn = mean(c),
            v_within = mean(se^2))

tables_between<-tables %>%
  left_join(tables_within) %>%
  group_by(year, age, race_ethn) %>%
  summarise(v_between = mean((c - c_mn)^2))

tables_comb<-tables_within %>%
  left_join(tables_between) %>%
  mutate(se_tot = sqrt(v_within + (1 + 1/5)*v_between)) %>%
  select(year, age, race_ethn, c_mn, se_tot)

tables_comb<-tables %>%
  filter(.imp==1) %>%
  select(-.imp) %>%
  left_join(tables_comb) %>%
  mutate(c_upr = c_mn + 1.96 * se_tot,
         c_lwr = c_mn - 1.96 * se_tot)


write_csv(tables_comb, "./vis/fc_lifetable.csv")

tables_afcars<-tables_comb

#################### NCANDS

ncands<-read_csv("./data/ncands_imputed.csv")

ncands<-ncands %>% 
  mutate(race_ethn = 
           ifelse(race_ethn == "AI/AN", 
                  "American Indian / Alaska Native",
                  race_ethn),
         race_ethn = 
           ifelse(race_ethn == "Asian/PI",
                  "Asian / Pacific Islander",
                  race_ethn))

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
  slice(1) %>% 
  ungroup() %>% 
  mutate(first_victim = TRUE)

ncands_first_victim<-ncands  %>% 
  filter(.imp!=0) %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  left_join(ncands_index) %>% 
  filter(!(is.na(first_victim))) %>% 
  mutate(year = year(rptdt)) %>% 
  select(.imp, st_id, staterr, year, chage, race_ethn, rptdt) %>% 
  distinct() %>% 
  group_by(.imp, staterr, year, chage, race_ethn) %>% 
  summarise(var = n())

### get first screened-in case
ncands_first_victim<-ncands_first_victim %>% 
  ungroup() %>% 
  complete(race_ethn, chage, 
           nesting(.imp, year, staterr), 
           fill = (list(var = 0))) 

### harmonize names
ncands_first_victim<-ncands_first_victim %>% 
  rename(age = chage, state = staterr) 
### subset to complete years
ncands_first_victim<-ncands_first_victim%>% 
  filter(year>=2002, 
         year<2017) %>% 
  filter(age<=18)

### MAKE STATE POP FILE FOR MATCHING BASED ON NCANDS INCLUSION
pop_st<-pop %>% 
  group_by(year, state, race_ethn, age) %>% 
  summarise(pop = sum(pop)) 

ncands_first_victim<- ncands_first_victim %>% 
  left_join(pop_st)

dat<- ncands_first_victim %>% 
  ungroup() %>% 
  group_by(.imp, year,
           race_ethn, age) %>% 
  summarise(var = sum(var, na.rm=TRUE),
            pop = sum(pop, na.rm=TRUE)) 
### make life tables
# add total to table
dat<-dat %>% 
  bind_rows(dat %>% 
              group_by(.imp,year, age) %>% 
              summarise(var= sum(var, na.rm=TRUE),
                        pop = sum(pop, na.rm=TRUE)) %>% 
              mutate(race_ethn = "Total")) 

### run life tables by imp, race_ethnb, sex
race<-unique(dat$race_ethn)
years<-unique(dat$year)
tables_out<-list()

counter<-0
for(i in 1:5){
  for(r in 1:length(race)){
    for(y in 1:length(years)){
      counter<-counter + 1
      
      temp<-dat %>% 
        filter(.imp == i,
               race_ethn == race[r],
               year == years[y])
      
      tables_out[[counter]]<-make_life_table(temp)
    }
  }
}

tables<-bind_rows(tables_out)

#### combine across imps
tables_within<-tables %>% 
  group_by(year, age, race_ethn) %>% 
  summarise(c_mn = mean(c),
            v_within = mean(se^2))

tables_between<-tables %>% 
  left_join(tables_within) %>% 
  group_by(year, age, race_ethn) %>% 
  summarise(v_between = mean((c - c_mn)^2)) 

tables_comb<-tables_within %>% 
  left_join(tables_between) %>% 
  mutate(se_tot = sqrt(v_within + (1 + 1/5)*v_between)) %>% 
  select(year, age, race_ethn, c_mn, se_tot)

tables_comb<-tables %>% 
  filter(.imp==1) %>% 
  select(-.imp) %>% 
  left_join(tables_comb) %>% 
  mutate(c_upr = c_mn + 1.96 * se_tot,
         c_lwr = c_mn - 1.96 * se_tot)

write_csv(tables_comb, "./vis/malt_lifetable.csv")

tables_ncands<-tables_comb

tables_vis<-bind_rows(
  tables_ncands %>% 
    mutate(outcome = "Confirmed Maltreatment"),
  tables_afcars %>% 
    mutate(outcome = "Foster Care Placement")
)


### ### ### ### ### ### ### ### ### ###
### visuals
tables<-tables_vis %>%
  ungroup() %>% 
  mutate(race_ethn = factor(race_ethn, levels = c("Total", 
                                                  "American Indian / Alaska Native",
                                                  "Asian / Pacific Islander",
                                                  "Black",
                                                  "Hispanic",
                                                  "White"))) %>% 
  rename(`Race/ethnicity` = race_ethn)

ggplot(tables %>%
         filter(age==18,
                year>=2004, year<=2016,
                outcome == "Foster Care Placement"), aes(x = year, y = c ,
                                 color = `Race/ethnicity`,
                                 ymin = c_lwr,
                                 ymax = c_upr)) +
  geom_line() +
  geom_linerange() +
  geom_point(size = 0.7) +
  theme_minimal() +
  ylab("Probability of foster care entry by age 18") +
  xlab("Year") +
  ggsave("./vis/fc_cumulative_yr.png")

ggplot(tables %>%
         filter(age==18,
                year>=2004, year<=2016,
                outcome == "Confirmed Maltreatment"), aes(x = year, y = c ,
                                                         color = `Race/ethnicity`,
                                                         ymin = c_lwr,
                                                         ymax = c_upr)) +
  geom_line() +
  geom_linerange() +
  geom_point(size = 0.7) +
  theme_minimal() +
  ylab("Probability of confirmed maltreatment by age 18") +
  xlab("Year") +
  ggsave("./vis/malt_cumulative_yr.png")

