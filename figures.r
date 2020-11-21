### --------------------------------------------------
###  Figures
### --------------------------------------------------


### --------------------------------------------------
### Libraries
### --------------------------------------------------
library(tidyverse)
library(here)
library(socviz)
library(patchwork)
library(colorspace)

## --------------------------------------------------------------------
## Custom font and theme, omit if you don't have the myriad library
## (https://github.com/kjhealy/myriad) and associated Adobe fonts.
## --------------------------------------------------------------------
library(showtext)
showtext_auto()
library(myriad)
import_myriad_semi()

theme_figs <- function(){
  theme_myriad_semi() +
    theme(panel.border = element_rect(colour = "gray90", fill=NA, size=1),
          axis.title.x = element_text(size = rel(0.8)))
}

theme_set(theme_figs())

### --------------------------------------------------------------------


### --------------------------------------------------
### Local Functions
### --------------------------------------------------



### --------------------------------------------------
### Data
### --------------------------------------------------

## From socviz, for convenience only
county_tmp <- county_data %>%
  as_tibble() %>%
  select(id, name, state, pop) %>%
  rename(fipscode = id)


## Make fipscode a 5-character id, not a dbl
county_df <- read_csv("vis/county_tables.csv") %>%
  mutate(fipscode = stringr::str_pad(fipscode, 5, pad = "0")) %>%
  left_join(county_tmp) %>%
  mutate(varname = recode(varname,
                          tpr = "Termination",
                          first_entry = "Foster Care",
                          first_victim = "Confirmed Victim",
                          first_inv = "Contact"))

## Fix ordering and color palette across Figs 1 and 2
county_ind <- county_df %>%
  group_by(county) %>%
  summarize(est = median(c_mn)) %>%
  mutate(ord = order(est))

df_rc <- county_df %>%
  mutate(county_rc = factor(county, levels = county_ind$county[county_ind$ord], ordered = TRUE),
         race_ethn_rc = recode(race_ethn,
                               Total = "All Groups"),
         race_ethn_rc = factor(race_ethn_rc,
                               levels = c("All Groups", "Black", "Hispanic", "White",
                                          "AI/AN", "Asian/PI"),
                               ordered = TRUE))


## Figure 1
f1_med_risk <- df_rc %>%
  filter(varname == "Contact") %>%
  summarize(mr = median(c_mn)) %>%
  pull(mr)

out_1 <- df_rc %>%
  filter(varname == "Contact") %>%
  ggplot(aes(x = c_mn,
             y = reorder(county, c_mn), color = race_ethn_rc)) +
  geom_vline(aes(xintercept = median(c_mn)), color = "gray50") +
  geom_point(size = 2) +
  facet_wrap(~ race_ethn_rc, nrow = 1) +
  scale_color_discrete_qualitative(palette = "Dark3") +
  scale_x_continuous(labels = scales::percent) +
  guides(color = FALSE) +
  labs(x = "Risk",
       y = NULL,
       title = "Cumulative risk of CPS investigation",
       subtitle = paste0("Vertical lines in each panel show the median risk of ",
                         round(f1_med_risk*100, 1), "% across all groups and counties"))

ggsave(here("vis/fig_1.pdf"), out_1, height = 4.5, width = 10)
ggsave(here("vis/fig_1.png"), out_1, height = 4.5, width = 10, dpi = 200)

###----------------------------------------------------------------------------------



###----------------------------------------------------------------------------------
### Figure 2
###----------------------------------------------------------------------------------

pan_1_lims <- df_rc %>%
  filter(varname == "Confirmed Victim") %>%
  summarize(med_r = round(median(c_mn*100), 1),
            min_r = round(min(c_mn*100), 1),
            max_r = round(max(c_mn*100, 1)))

pan_2_lims <- df_rc %>%
  filter(varname == "Foster Care") %>%
  summarize(med_r = round(median(c_mn*100), 1),
            min_r = round(min(c_mn*100), 1),
            max_r = round(max(c_mn*100, 1)))

pan_3_lims <- df_rc %>%
  filter(varname == "Termination") %>%
  summarize(med_r = round(median(c_mn*100), 1),
            min_r = round(min(c_mn*100), 1),
            max_r = round(max(c_mn*100, 1)))



pan_1 <- df_rc %>%
  filter(varname == "Confirmed Victim") %>%
  ggplot(aes(x = c_mn, y = reorder(county, c_mn), color = race_ethn_rc)) +
  geom_vline(aes(xintercept = median(c_mn)), color = "gray50") +
  geom_point() + facet_wrap(~ race_ethn_rc, nrow = 1) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, 0.1, 0.2, 0.3), limits = c(0, 0.3)) +
  guides(color = FALSE) +
  labs(x = paste0("Risk range for this outcome is ",
                  pan_1_lims$min_r, "% to ",
                  pan_1_lims$max_r, "%, with a median of ",
                  pan_1_lims$med_r, "%"),
       y = NULL, color = "Race/Ethnicity") +
  ggtitle("Confirmed Victim")

pan_2 <- df_rc %>%
  filter(varname == "Foster Care") %>%
  ggplot(aes(x = c_mn, y = reorder(county, c_mn), color = race_ethn_rc)) +
  geom_vline(aes(xintercept = median(c_mn)), color = "gray50") +
  geom_point() + facet_wrap(~ race_ethn_rc, nrow = 1) +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, 0.1, 0.2, 0.3)) +
  guides(color = FALSE) +
  labs(x = paste0("Risk range for this outcome is ",
                  pan_2_lims$min_r, "% to ",
                  pan_2_lims$max_r, "%, with a median of ",
                  pan_2_lims$med_r, "%"),
       y = NULL, color = "Race/Ethnicity") +
  ggtitle("Foster Care")

pan_3 <- df_rc %>%
  filter(varname == "Termination") %>%
  ggplot(aes(x = c_mn, y = reorder(county, c_mn), color = race_ethn_rc)) +
  geom_point() + facet_wrap(~ race_ethn_rc, nrow = 1) +
  geom_vline(aes(xintercept = median(c_mn)), color = "gray50") +
  scale_color_discrete_qualitative(palette = "Dark 3") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0, 0.02, 0.04)) +
  guides(color = FALSE) +
  labs(x = paste0("Risk range for this outcome is ",
                  pan_3_lims$min_r, "% to ",
                  pan_3_lims$max_r, "%, with a median of ",
                  pan_3_lims$med_r, "%"),
       y = NULL, color = "Race/Ethnicity")  +
  ggtitle("Termination of Parental Rights") +
  theme(plot.subtitle = element_text(size = rel(0.8)))


out_patch <- (pan_1 / plot_spacer() / pan_2 / plot_spacer() / pan_3) +
  plot_layout(heights = c(0.32, 0.01, 0.32, 0.01, 0.32)) +
  plot_annotation(title = "Cumulative Risks for Three Events by Race/Ethnicity and County",
                  subtitle = "For each outcome, vertical bars show the median risk across all counties and groups.\nCounties are ordered from high to low within each outcome. Each outcome has its own x-axis scale.")

ggsave(here("vis/fig_2.pdf"), out_patch, height = 11, width = 12)
ggsave(here("vis/fig_2.png"), out_patch, height = 11, width = 12, dpi = 200)






