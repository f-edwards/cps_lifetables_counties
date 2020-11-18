library(tidyverse)
library(RColorBrewer)

dat<-read_csv("./vis/county_tables.csv")
rank<-read_csv("./data/top_pops.csv")

### join pop rank onto data for plot order
dat<-dat %>% 
  left_join(rank) %>% 
  mutate(race_ethn = ifelse(race_ethn=="Hispanic", 
                            "Latinx",
                            race_ethn))

theme_set(theme_bw())

dat<-dat %>% 
  mutate(race_ethn = factor(race_ethn,
                            levels = c("Total",
                                       "AI/AN", 
                                       "Asian/PI",
                                       "Black",
                                       "Latinx",
                                       "White")),
         varname = case_when(
           varname=="tpr" ~ "Termination",
           varname=="first_entry" ~ "Foster Care",
           varname=="first_victim" ~ "Confirmed Victim",
           varname=="first_inv" ~ "Investigation"
         ),
         varname = factor(varname,
                          levels = c("Termination",
                                     "Foster Care",
                                     "Confirmed Victim",
                                     "Investigation")))


### all of the data
ggplot(dat %>% 
         filter(varname!="Investigation"),
       aes(x = varname,
           fill = race_ethn,
           y = c_mn)) + 
  geom_point(alpha = 0.7, color = "black", pch =21, size =3) + 
  coord_flip()+
  facet_wrap(~reorder(county, rank)) + 
  xlab("") + 
  ylab("Cumulative Risk of Event") +
  labs(fill = "Race/Ethnicity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  ggsave("./vis/all_data.pdf", width = 8, height =6)

### make each outcome plot

ggplot(dat %>% 
         filter(varname=="Investigation"),
       aes(x = reorder(county, desc(rank)), y = c_mn,
           color = race_ethn)) + 
  geom_point() + 
  facet_wrap(~race_ethn, ncol = 6) + 
  coord_flip() +
  xlab("") + 
  ylab("Cumulative risk of CPS investigation") + 
  labs(color = "") + 
  theme(legend.position = "bottom") +
  ggsave("./vis/inv.pdf", width = 8, height = 6)

ggplot(dat %>% 
         filter(varname=="Investigation"),
       aes(x = reorder(county, desc(rank)), y = c_mn,
           fill = race_ethn)) + 
  geom_point(alpha = 0.7, color = "black", pch =21, size =3) + 
  #facet_wrap(~race_ethn, ncol = 6) + 
  coord_flip() +
  xlab("") + 
  ylab("Cumulative risk of CPS investigation") + 
  ggsave("./vis/inv_color.pdf", width = 8, height = 6)

ggplot(dat %>% 
         filter(varname=="Investigation"),
       aes(x = reorder(county, desc(rank)), y = c_mn,
           ymin = c_lwr, ymax = c_upr)) + 
  geom_pointrange(fatten=0.25) + 
  facet_wrap(~race_ethn, ncol = 6) + 
  coord_flip() +
  xlab("") + 
  ylab("Cumulative risk of CPS investigation") + 
  ggsave("./vis/inv_with_error.pdf", width = 8, height =6)


# ggplot(dat %>% 
#          filter(varname=="Confirmed Victim"),
#        aes(x = reorder(county, desc(county)), y = c_mn)) + 
#   geom_point() + 
#   coord_flip() + 
#   xlab("") + 
#   ylab("Cumulative risk of confirmed CPS case") + 
#   facet_wrap(~race_ethn, ncol = 6) + 
#   ggsave("./vis/victim.pdf", width = 12)
# 
# ggplot(dat %>% 
#          filter(varname=="Foster Care"),
#        aes(x = reorder(county, desc(county)), y = c_mn)) + 
#   geom_point() + 
#   coord_flip() + 
#   xlab("") + 
#   ylab("Cumulative risk of FC entry") + 
#   facet_wrap(~race_ethn, ncol = 6) + 
#   scale_y_continuous(
#     labels = scales::number_format(accuracy = 0.01)) +
#   ggsave("./vis/entry.pdf", width = 12)
# 
# ggplot(dat %>% 
#          filter(varname=="Termination"),
#        aes(x = reorder(county, desc(county)), y = c_mn)) + 
#   geom_point() + 
#   coord_flip() + 
#   xlab("") + 
#   ylab("Cumulative risk of TPR") + 
#   facet_wrap(~race_ethn, ncol = 6) + 
#   ggsave("./vis/tpr.pdf", width = 12)