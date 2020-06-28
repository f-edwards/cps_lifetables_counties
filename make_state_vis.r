library(tidyverse)
library(maps)
library(usmap)

tables_comb<-read_csv("./vis/st_tables.csv")

map_dat<-us_map()

state_dat<-tables_comb %>% 
  left_join(map_dat %>% 
              rename(state=abbr)) %>% 
  arrange(order, group)


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
  ggsave("./vis/st_race_density.png") 

state_dat<-state_dat  %>% 
  mutate(race_ethn = ifelse(race_ethn=="Hispanic",
                            "Latinx",
                            race_ethn)) %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c(
                              "Total", 
                              "AI/AN",
                              "Asian/PI",
                              "Black",
                              "Latinx",
                              "White")))

ggplot(state_dat %>% 
         filter(varname=="Investigation"),
       aes(x=x, y = y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  ggsave("./vis/st_race_investigation.png")

ggplot(state_dat %>% 
         filter(varname=="Confirmed Maltreatment"),
       aes(x=x, y=y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  ggsave("./vis/st_race_malt.png")

ggplot(state_dat %>% 
         filter(varname=="Foster Care"),
       aes(x=x, y=y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  ggsave("./vis/st_race_fc.png")

ggplot(state_dat %>% 
         filter(varname=="Termination of Parental Rights"),
       aes(x=x, y=y, group = group, fill = c_mn)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  ggsave("./vis/st_race_tpr.png")

### make tpr/investigation ratio
tpr_dat<-state_dat %>% 
  filter(grepl("Termination", varname)) %>% 
  rename(tpr=c_mn) %>% 
  select(-se_tot, -c_upr, -c_lwr, -varname)

ratio_dat<-state_dat %>% 
  filter(grepl("Investigation", varname)) %>% 
  left_join(tpr_dat) %>% 
  mutate(tpr_ratio = tpr/c_mn)

ggplot(ratio_dat,
       aes(x=x, y=y, group = group, fill = tpr_ratio)) + 
  geom_polygon() +
  geom_polygon(color = "black") + 
  theme_void() +
  labs(fill = "") +
  theme(legend.position = "bottom") +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~race_ethn) +
  ggsave("./vis/st_race_tpr_ratio.png")