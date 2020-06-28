rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

source("lifetable.r")

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
             race==3 ~ "AI/AN",
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age)) %>%
  filter(year>=2000)

### make county id 
pop_fips<-pop %>%
  mutate(fipscode = as.numeric(
    paste(st_fips, cnty_fips,
          sep = "")))

# top_pops<-pop_fips %>% 
#   group_by(fipscode) %>% 
#   summarise(pop = sum(pop)) %>% 
#   arrange(desc(pop)) %>% 
#   ungroup() %>% 
#   mutate(rank = 1:n()) %>% 
#   filter(rank<=6) %>% 
#   mutate(fipscode = 
#            ifelse(fipscode==36047,
#                   36061,
#                   fipscode))

top_pops<-data.frame(fipscode = c(36061, 6037, 17031,
                                  48201, 4013, 6059),
                     rank = 1:6)

### get county pop
pop_fips <- pop_fips %>% 
  filter(age<18, year>2003) %>% 
  group_by(fipscode, age, race_ethn) %>% 
  summarise(pop = sum(pop))

afcars<-read_csv("./data/afcars_imputed_all_cases.csv") %>% 
  filter(year>2003) 

ncands<-read_csv("./data/ncands_imputed.csv")

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
  mutate(year = year(rptdt)) %>% 
  group_by(.imp, rptfips, chage, race_ethn) %>% 
  summarise(var = n())

ncands_first_victim<-ncands_first_victim %>% 
  ungroup() %>% 
  complete(race_ethn, chage, rptfips,
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
  mutate(year = year(rptdt)) %>% 
  group_by(.imp, rptfips, chage, race_ethn) %>% 
  summarise(var = n())

ncands_first_rpt<-ncands_first_rpt %>% 
  ungroup() %>% 
  complete(race_ethn, chage, rptfips,
           .imp,  
           fill = (list(var = 0))) 
### harmonize names
ncands_first_victim<-ncands_first_victim %>% 
  rename(age = chage,
         fipscode = rptfips) %>% 
  mutate(varname = "first_victim")

ncands_first_rpt<-ncands_first_rpt %>% 
  rename(age = chage,
         fipscode = rptfips) %>% 
  mutate(varname = "first_rpt")

ncands_tab_dat<-bind_rows(ncands_first_rpt,
                          ncands_first_victim) %>% 
  filter(age<18)

### index those counties in the data for the full period
### for inclusion
afcars_index<-afcars %>% 
  filter(.imp==0) %>% 
  group_by(fipscode, year) %>% 
  select(fipscode, year) %>% 
  distinct() %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  group_by(fipscode) %>% 
  summarise(years_in_data = sum(count)) %>% # remove all with <14 years of data
  filter(!(fipscode %in% c(8,9)),
         years_in_data==14,
         !(is.na(fipscode)))


# ### subset to only first entries
# afcars<-afcars%>%
#   select(fy, stfcid, dob, race_ethn, sex,
#          ageatlatrem, rem1dt, totalrem, 
#          entered, state, fipscode) %>%
#   filter(totalrem==1 & entered==1)

afcars_cnty_year<-afcars %>%
  filter(totalrem==1 & entered==1) %>% # subset to first entries
  filter(age<18) %>% 
  filter(fipscode%in%afcars_index$fipscode) %>% 
  filter(.imp!=0) %>%
  filter(year>=2004) %>% 
  group_by(.imp, fipscode, age, race_ethn) %>%
  summarise(first_fc = n()) %>%
  ungroup() %>%
  complete(.imp, age, fipscode, race_ethn,
           fill = list(first_fc=0)) 

### make tpr measure as first year where istpr=T
afcars_new_tpr<-afcars %>% 
  filter(age<18) %>% 
  filter(.imp>0) %>% 
  filter(fipscode%in%afcars_index$fipscode) %>% 
  filter(year>=2004) %>% 
  filter(istpr==1) %>% 
  group_by(.id, .imp) %>% 
  summarise(newtpr=min(year)) %>% 
  mutate(newtpr=T) %>% 
  select(.id, .imp, newtpr)

afcars_tpr<-afcars %>% 
  left_join(afcars_new_tpr) %>% 
  filter(age<18) %>% 
  filter(fipscode%in%afcars_index$fipscode) %>% 
  filter(.imp!=0) %>%
  filter(year>=2004) %>% 
  group_by(.imp, fipscode, age, race_ethn) %>%
  summarise(tpr = sum(newtpr, na.rm=T)) %>%
  ungroup() %>%
  complete(.imp, age, fipscode, race_ethn,
           fill = list(newtpr=0)) 
  

afcars_tpr<-afcars_tpr %>% 
  left_join(pop_fips) %>% 
  rename(var = tpr) %>% 
  mutate(varname = "tpr")

afcars_cnty_year<-afcars_cnty_year %>% 
  left_join(pop_fips) %>% 
  rename(var = first_fc) %>% 
  mutate(varname = "first_fc") %>% 
  bind_rows(afcars_tpr)

ncands_tab_dat<-ncands_tab_dat %>% 
  left_join(afcars_index) %>% 
  filter(!(is.na(years_in_data))) %>% 
  select(-years_in_data) %>% 
  left_join(pop_fips)

tab_dat<-bind_rows(afcars_cnty_year,
                   ncands_tab_dat)

### run life tables by imp, race_ethnb, sex
vars<-unique(tab_dat$varname)
race<-unique(afcars_cnty_year$race_ethn)
fips<-unique(afcars_cnty_year$fipscode)
tables_out<-list()

counter<-0
for(h in 1:length(vars)){
  for(i in 1:5){
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
  filter(age==17)

### bind names
library(maps)
data(county.fips)

county.fips<-county.fips %>% 
  separate(polyname, into = c("state", "county"), sep = ",") %>% 
  filter(county!="pierce:penrose") %>% 
  mutate(county = ifelse(county=="pierce:penrose", "pierce", county)) 

tables_comb <- tables_comb %>% 
  left_join(county.fips %>% 
              rename(fipscode = fips)) %>% 
  mutate(county = ifelse(fipscode==15003,
         "honolulu", county),
         state = ifelse(fipscode==15003,
                        "hawaii",
                        state)) %>% 
  mutate(county = str_to_title(county),
         state = str_to_title(state))

write_csv(tables_comb, "county_afcars_ncands_tables_race.csv")

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

ggplot(tables_comb,
       aes(x = c_mn, fill = race_ethn, color = race_ethn)) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~varname, scales = "free") + 
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank()) +
  ylab("Cumulative risk") + 
  ylab("Risk of event by age 18") +
  theme_minimal() +
  ggsave("race_density.png") 
  
tables_top_pop<-tables_comb %>% 
  ungroup() %>% 
  left_join(top_pops %>% 
              select(fipscode, rank)) %>% 
  filter(!(is.na(rank))) %>% 
  arrange(rank) %>% 
  mutate(county = factor(paste(county, state, sep = ", ")))
  

ggplot(tables_top_pop,
       aes(x= varname, y = c_mn, fill = race_ethn)) +
  geom_col(position = position_dodge(),
           color = "black") +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank())+
  facet_wrap(~county, ncol = 2) +
  theme_minimal() +
  xlab("") + 
  ylab("Risk of event by age 18") +
  theme(legend.title = element_blank()) +
  coord_flip() +
  ggsave("fc_county_race.png")