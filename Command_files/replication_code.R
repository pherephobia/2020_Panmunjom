################################################################################
## Project: Personal 2020                                                     ##
## Title: Persistence and Change in Attitudes toward N.K. and Unification     ##
## Updates: Sep. 12th 2020                                                    ##
## Authors: Sanghoon Park (Univ. of South Carolina)                           ##
##          Jaeyoung Hur  (Yonsei Univ.)                                      ##
################################################################################

## Import necessary packages
library(ggplot2)
library(ezpickr)
library(patchwork)
library(ggeffects)
library(MASS)
library(lmtest)
library(stargazer)
library(knitr)
library(kableExtra)
library(tidyverse)



rm(list=ls())
load("Analysis_data/replication.RData")
## See the data for replication
glimpse(subset)   # Survey
glimpse(approval) # Approval rates
glimpse(gdp)      # GDPpc trends

## Figure 1

grid_gdp <- gdp %>% 
  tidyr::gather(variable, value, -Quarters) %>%
  mutate(
    variable = case_when(
      variable == "Real_GDP" ~ "Nominal GDP",
      variable == "Real_GDP_growth" ~ "Real GDP Growth",
      T ~ NA_character_)
  )

grid_gdp %>%
  ggplot(aes(x=Quarters, y = value)) + geom_col() + 
  facet_wrap(variable~., scales = "free", ncol = 1) + 
  labs(x = "Year-Quarter", y = "",
       caption = "Source: Bank of Korea <National Income>
       Currency: Billion (Won), YoY %") + 
  scale_y_continuous(labels = scales::comma) + theme_bw() + 
  theme(strip.text = element_text(size = 6),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        plot.caption = element_text(size = 7))

## Figure 2

average <- approval %>% summarize()
p_approval <- approval %>% drop_na() %>% mutate(
  media = case_when(
    media == "A" ~ "Gallup Korea",
    media == "B" ~ "Realmeter",
    media == "Average" ~ "Average"
  ) %>% parse_factor(., levels = c("Gallup Korea", "Realmeter", "Average"),
                     include_na = F, ordered = T)
)

p_approval %>% ggplot(aes(x = period, 
                          y = positive, 
                          group = media, 
                          color = media)) + 
  labs(x = "Year - Month", y = "President Approval (%)",
       caption = 
         "Source: Gallup Korea. 2019. Gallup Korea Daily Opinion:\nMonthly Aggregate Statistics & Realmeter. Political Archive.\nThe monthly data of Realmeter is the averages of weekly data.") + 
  scale_color_manual(values =futurevisions::futurevisions("mars")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  geom_point() + geom_vline(xintercept = 11.33, 
                            color = "black", linetype = 2) + 
  geom_curve(
    aes(x = 11.33, y = 82, xend = 10, yend = 84), color = "black",
    arrow = arrow(length = unit(0.03, "npc"))
  ) + 
  geom_text(aes(x=8.5, y=84, label="4.27 Panmunjom\nDeclaration"),
            color="black", 
            size=2) + 
  geom_line() + theme_bw() + 
  theme(axis.title.x = element_text(size = 8),
        axis.text.x = element_text(size = 5),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 6),
        plot.caption = element_text(size = 7),
        legend.title = element_blank(),
        legend.text =  element_text(size = 8),
        legend.position = c(0.5, 0.7),
        legend.background = element_blank() )

subset <- subset %>% mutate(
  Q1 = case_when(
    Q1==1 ~ "Very Dissatisfied",
    Q1==2 ~ "Dissatisfied",
    Q1==3 ~ "Neutral",
    Q1==4 ~ "Satisfied",
    Q1==5 ~ "Very Satisfied",
    T ~ NA_character_
  ) %>% parse_factor(., levels = c("Very Dissatisfied", "Dissatisfied", 
                                   "Neutral", "Satisfied", "Very Satisfied"), 
                     include_na = F, ordered = T),
  Q2 = case_when(
    Q2==1 ~ "Hostile",
    Q2==2 ~ "Vigilant",
    Q2==3 ~ "Competitive",
    Q2==4 ~ "Cooperative",
    Q2==5 ~ "Supportive",
    T ~ NA_character_
  ) %>% parse_factor(., levels = c("Hostile", "Vigilant", "Competitive", 
                                   "Cooperative", "Supportive"), 
                     include_na = F, ordered = T)
)


## Figure 3

p1 <- subset %>% 
  ggplot(aes(x = Q1, fill = Q1)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           position=position_dodge(), show.legend = F) + 
  geom_text(aes(x = Q1,
                label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..)), stat= "count", vjust = 0.5,
            position = position_dodge(1), size = 2) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_fill_grey(start = 0.8, end = 0.4) +
  scale_y_continuous(labels = scales::percent) + 
  labs(subtitle = "Q. What do you think of Jong-Eun Kim’s
       North Korean regime and leadership group?",
       y = "Percent (%)", x = "") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())
p2 <- subset %>% 
  ggplot(aes(x = Q2, fill = Q2)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),
           position=position_dodge(), show.legend = F) + 
  geom_text(aes(x = Q2,
                label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..)), 
            stat= "count", vjust = 0.5, size = 2,
            position = position_dodge(1)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_fill_grey(start = 0.8, end = 0.4) +
  scale_y_continuous(labels = scales::percent) + 
  labs(subtitle = "Q. What do you think of
       North Korea is for South Korea?",
       y = "Percent (%)", x = "") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())


p1 + p2 + plot_layout(ncol = 1)


## Figure 4
subset <- subset %>% 
  mutate(
    Q4_re = 5 - as.numeric(Q4),
    Q4_re = case_when(
      Q4_re==1 ~ "Not\npreferred",
      Q4_re==2 ~ "Status quo",
      Q4_re==3 ~ "Preferred\nwithout\nBurdens",
      Q4_re==4 ~ "Preferred",
      T ~ NA_character_
    ) %>% parse_factor(., levels = c("Not\npreferred","Status quo",
                                     "Preferred\nwithout\nBurdens", "Preferred"), 
                       include_na = F, ordered = T),
    Q4_label = case_when(
      Q4_re=="Not\npreferred" ~ "Not\npreferred\n(n = 89)",
      Q4_re=="Status quo" ~ "Status quo\n(n = 252)",
      Q4_re=="preferred\nwithout\nBurdens" ~ "preferred without\nBurdens\n(n = 455)",
      Q4_re=="preferred" ~ "preferred\n(n = 204)",
      T ~ NA_character_
    ) %>% parse_factor(., levels = c("Not\npreferred\n(n = 89)",
                                     "Status quo\n(n = 252)",
                                     "preferred without\nBurdens\n(n = 455)",
                                     "preferred\n(n = 204)"), 
                       include_na = F, ordered = T),
    Q3 = case_when(
      Q3==1 ~ "Never\nInterested",
      Q3==2 ~ "Not\nInterested",
      Q3==3 ~ "Interested",
      Q3==4 ~ "Very\nInterested",
      T ~ NA_character_
    ) %>% parse_factor(., levels = c("Never\nInterested", "Not\nInterested", 
                                     "Interested","Very\nInterested"), 
                       include_na = F, ordered = T),
    Q9 = case_when(
      Q9==5 ~ "Never",
      Q9==1 ~ "Less\nthan\n1%",
      Q9==2 ~ "Less\nthan\n1~5%",
      Q9==3 ~ "Less\nthan\n5~10%",
      Q9==4 ~ "More\nthan\n10%",
      T ~ NA_character_) %>% 
      parse_factor(., levels = c("Never", "Less\nthan\n1%", "Less\nthan\n1~5%",
                                 "Less\nthan\n5~10%", "More\nthan\n10%"), 
                   include_na = F, ordered = T),
    Q6_fa = case_when(
      Q6==1 ~ "Social\nDisorder",
      Q6==2 ~ "Immigrants from\nthe North",
      Q6==3 ~ "The Burden of\nUnification Costs",
      Q6==4 ~ "Political/Economic\nDisorder",
      Q6==5 ~ "Confusion of\nInternational Relations"
    ) %>% parse_factor(., levels = c("Social\nDisorder","Immigrants from\nthe North",
                                     "The Burden of\nUnification Costs",
                                     "Political/Economic\nDisorder",
                                     "Confusion of\nInternational Relations"))
  )

subset %>% ggplot(aes(x = Q4_re,
                     y = prop.table(stat(count)),
                     fill = Q4_re,
                     label = scales::percent(prop.table(stat(count))))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = F) +
  #  facet_wrap(~ Q4_label, ncol = 2) +
  geom_text(stat = 'count',
            position = position_dodge(.1),
            vjust = 0.5, size = 2)+
  scale_fill_grey(start = 0.8, end = 0.4) +
  scale_y_continuous(labels = scales::percent) + 
  labs(subtitle = "Q. What do you think about unification?",
       y = "Percent (%)", x = "") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())


## Figure 5
subset %>% ggplot(aes(x = Q6_fa,
                     y = prop.table(stat(count)),
                     fill = Q6_fa,
                     label = scales::percent(prop.table(stat(count))))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = F) +
  geom_text(stat = 'count',
            position = position_dodge(.1),
            vjust = 0.5, size = 2)+
  scale_fill_grey(start = 0.8, end = 0.4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(subtitle = "Q. What do you think is most worrying about 
       the unification process?",
       y = "Percent (%)", x = "") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())

## Figure 6
subset %>% dplyr::filter(Q6_fa == "The Burden of\nUnification Costs") %>% 
  ggplot(aes(x = Q9,
             y = prop.table(stat(count)),
             fill = Q9,
             label = scales::percent(prop.table(stat(count))))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = F) + 
  geom_text(stat = 'count',
            position = position_dodge(.9),
            vjust = 1, size = 2)+
  scale_fill_grey(start = 0.8, end = 0.4) +
  scale_y_continuous(labels = scales::percent) + 
  labs(subtitle = "Preferred tax level to cover unification costs
  when respondents think the burden of 
  unification costs are the biggest concern",
       y = "Percent (%)", x = "") + 
  theme_bw() +
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())

## Figure 7

Bivariate <- subset %>% dplyr::select(Q1, Q2, Q4, Q5, age) %>%
  mutate(
    age = case_when(
      age == 1L ~ "20s",
      age == 2L ~ "30s",
      age == 3L ~ "40s",
      age == 4L ~ "50s",
      age == 5L ~ "over 60s",
      T ~ NA_character_) %>%
      parse_factor(., levels = c("20s", "30s", "40s", "50s", "over 60s"),
                   include_na = F, ordered = T),
    Q5 = case_when(
      Q5 == 1L ~ "Less than\n5 years",
      Q5 == 2L ~ "6-10 years",
      Q5 == 3L ~ "11-20 years",
      Q5 == 4L ~ "21-30 years",
      Q5 == 5L ~ "More than\n30 years",
      Q5 == 6L ~ "Never"
    ) %>% parse_factor(., levels = c("Less than\n5 years", "6-10 years", 
                                     "11-20 years", "21-30 years", 
                                     "More than\n30 years", "Never"),
                       include_na = F, ordered = T)
  )

Bivariate_sum <- Bivariate %>% group_by(age) %>% 
  summarise(
    Q1.sum = mean(as.numeric(Q1), na.rm = T),
    Q1.sd = sd(as.numeric(Q1), na.rm = T),
    Q2.sum = mean(as.numeric(Q2), na.rm = T),
    Q2.sd = sd(as.numeric(Q2), na.rm = T),
    Q4.sum = mean(as.numeric(Q4), na.rm = T),
    Q4.sd = sd(as.numeric(Q4), na.rm = T)
  )

plot5a <- Bivariate_sum %>% 
  ggplot(aes(x = age, y = Q1.sum)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = Q1.sum - Q1.sd, ymax = Q1.sum + Q1.sd), width=0.2) + 
  scale_y_continuous(limits = c(0, 5, by = 1)) + 
  geom_hline(yintercept = 2.73, color = "red", linetype = 2) + 
  scale_fill_grey(start = 0.8, end = 0.4) +
  labs(subtitle = "NK ledership",
       y = "Mean value", x = "") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())

plot5b <- Bivariate_sum %>% 
  ggplot(aes(x = age, y = Q2.sum)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = Q2.sum - Q2.sd, ymax = Q2.sum + Q2.sd), width=0.2) + 
  scale_y_continuous(limits = c(0, 5, by = 1)) + 
  geom_hline(yintercept = 3.03, color = "red", linetype = 2) + 
  scale_fill_grey(start = 0.8, end = 0.4) +
  labs(subtitle = "NK general",
       y = "Mean value", x = "Generations") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(size = 7.5),
        axis.text.x = element_text(hjust = 0.5, size = 5),
        axis.title.x = element_text(size = 7.5),
        axis.title.y = element_text(size = 7.5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank())
plot5a + plot5b + patchwork::plot_layout(ncol = 1)

## Figure 8
Bivaraiate_sum2a <- as.data.frame(table(Bivariate$Q4, Bivariate$age))
Bivaraiate_sum2b <- as.data.frame(table(Bivariate$Q5, Bivariate$age))

Bivaraiate_sum2a <- Bivaraiate_sum2a %>% rename(
  Q4 = Var1,
  Generation = Var2,
  Q4.freq = Freq
)

Bivaraiate_sum2b <- Bivaraiate_sum2b %>% rename(
  Q5 = Var1,
  Generation = Var2,
  Q5.freq = Freq
)


Bivaraiate_sum2a <- Bivaraiate_sum2a %>% mutate(
  Q4.Prop = case_when(
    Q4 == "Not\npreferred"              & Generation == "20s" ~ 26/162,
    Q4 == "status quo"                 & Generation == "20s" ~ 71/162,
    Q4 == "preferred\nwithout\nBurdens" & Generation == "20s" ~ 45/162,
    Q4 == "preferred"                   & Generation == "20s" ~ 20/162,
    Q4 == "Not\npreferred"              & Generation == "30s" ~ 25/181,
    Q4 == "status quo"                 & Generation == "30s" ~ 97/181,
    Q4 == "preferred\nwithout\nBurdens" & Generation == "30s" ~ 42/181,
    Q4 == "preferred"                   & Generation == "30s" ~ 17/181,
    Q4 == "Not\npreferred"              & Generation == "40s" ~ 48/208,
    Q4 == "status quo"                 & Generation == "40s" ~ 83/208,
    Q4 == "preferred\nwithout\nBurdens" & Generation == "40s" ~ 63/208,
    Q4 == "preferred"                   & Generation == "40s" ~ 14/208,
    Q4 == "Not\npreferred"              & Generation == "50s" ~ 52/202,
    Q4 == "status quo"                 & Generation == "50s" ~ 92/202,
    Q4 == "preferred\nwithout\nBurdens" & Generation == "50s" ~ 42/202,
    Q4 == "preferred"                   & Generation == "50s" ~ 16/202,
    Q4 == "Not\npreferred"              & Generation == "over 60s" ~ 53/247,
    Q4 == "status quo"                 & Generation == "over 60s" ~ 112/247,
    Q4 == "preferred\nwithout\nBurdens" & Generation == "over 60s" ~ 60/247,
    Q4 == "preferred"                   & Generation == "over 60s" ~ 22/247
  ),
  Q4.Prop.round = round(Q4.Prop, 2),
  `Attitudes toward unification` = case_when(
    Q4 == "Not\npreferred"  ~ "Not preferred",
    Q4 == "status quo"     ~ "status quo",
    Q4 == "preferred\nwithout\nBurdens" ~ "preferred without\nBurdens",
    Q4 == "preferred" ~ "preferred"
  )
)

Bivaraiate_sum2b <- Bivaraiate_sum2b %>% mutate(
  `Prospect of unification` = case_when(
    Q5 == "Less than\n5 years"  & Generation == "20s"      ~ 13/162,
    Q5 == "6-10 years"          & Generation == "20s"      ~ 40/162,
    Q5 == "11-20 years"         & Generation == "20s"      ~ 47/162,
    Q5 == "21-30 years"         & Generation == "20s"      ~ 25/162,
    Q5 == "More than\n30 years" & Generation == "20s"      ~ 19/162,
    Q5 == "Never"              & Generation == "20s"      ~ 18/162,   
    Q5 == "Less than\n5 years"  & Generation == "30s"      ~ 7/181,
    Q5 == "6-10 years"          & Generation == "30s"      ~ 45/181,
    Q5 == "11-20 years"         & Generation == "30s"      ~ 63/181,
    Q5 == "21-30 years"         & Generation == "30s"      ~ 27/181,
    Q5 == "More than\n30 years" & Generation == "30s"      ~ 16/181,
    Q5 == "Never"              & Generation == "30s"      ~ 23/181,   
    Q5 == "Less than\n5 years"  & Generation == "40s"      ~ 9/208,
    Q5 == "6-10 years"          & Generation == "40s"      ~ 57/208,
    Q5 == "11-20 years"         & Generation == "40s"      ~ 85/208,
    Q5 == "21-30 years"         & Generation == "40s"      ~ 22/208,
    Q5 == "More than\n30 years" & Generation == "40s"      ~ 18/162,
    Q5 == "Never"              & Generation == "40s"      ~ 17/162,   
    Q5 == "Less than\n5 years"  & Generation == "50s"      ~ 21/202,
    Q5 == "6-10 years"          & Generation == "50s"      ~ 62/202,
    Q5 == "11-20 years"         & Generation == "50s"      ~ 61/202,
    Q5 == "21-30 years"         & Generation == "50s"      ~ 21/202,
    Q5 == "More than\n30 years" & Generation == "50s"      ~ 11/162,
    Q5 == "Never"              & Generation == "50s"      ~ 26/162,   
    Q5 == "Less than\n5 years"  & Generation == "over 60s" ~ 23/247,
    Q5 == "6-10 years"          & Generation == "over 60s" ~ 63/247,
    Q5 == "11-20 years"         & Generation == "over 60s" ~ 61/247,
    Q5 == "21-30 years"         & Generation == "over 60s" ~ 19/247,
    Q5 == "More than\n30 years" & Generation == "over 60s" ~ 12/162,
    Q5 == "Never"              & Generation == "over 60s" ~ 69/162
  ),
  Q5.Prop.round = round(`Prospect of unification`, 2),
)

plot6a <- Bivaraiate_sum2a %>%
  mutate(pos = cumsum(Q4.Prop.round) - (0.5 * Q4.Prop.round)) %>%
  ggplot(aes(fill=`Attitudes toward unification`, x = Generation, y = Q4.Prop.round)) + 
  geom_bar(position="fill", stat="identity", width = .7, colour="black", lwd=0.1) + 
  geom_text(aes(label=ifelse(Q4.Prop.round != Q4.Prop.round, 
                             paste0(sprintf("%.0f", Q4.Prop.round*100),"%", ""), y = pos)),
            position=position_fill(vjust=0.5), colour="black", size = 3) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(subtitle = "Attitude toward unification",
       fill = "Attitude toward\nunification",
       x = "Generations", y = "") + 
  scale_fill_manual(values = futurevisions::futurevisions("cancri")) + 
  theme_bw() + theme(
    legend.title = element_text(size = 7),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 5.5)) +
  guides(fill=guide_legend(ncol=2))


plot6b <- Bivaraiate_sum2b %>% group_by(Q5,Generation) %>%
  mutate(pos = cumsum(Q5.Prop.round) - (0.5 * Q5.Prop.round)) %>%
  ggplot(aes(fill=Q5, x = Generation, y = Q5.Prop.round)) + 
  geom_bar(position="fill", stat="identity", width = .7, colour="black", lwd=0.1) + 
  geom_text(aes(label=ifelse(Q5.Prop.round != Q5.Prop.round, 
                             paste0(sprintf("%.0f", Q5.Prop.round*100),"%", ""), y = pos)),
            position=position_fill(vjust=0.5), colour="black", size = 3) + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(subtitle = "Prospect of unification",
       fill = "Prospect of\nunification",
       x = "Generations", y = "Percent (%)") + 
  scale_fill_manual(values = futurevisions::futurevisions("cancri")) + 
  theme_bw() + theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 5.5)) +
  guides(fill=guide_legend(ncol=3))

plot6b + plot6a + patchwork::plot_layout(ncol = 1)

## Table 1: Ordered logistic regression: 
## Attitudes toward North Korea after the Panmunjom Declaration
model1 <- polr(as.ordered(Q1) ~ as.factor(age) + Q19 + Honam + PK + 
                 TK + gender + edu + income, data = subset,
               method = "logistic", Hess = TRUE)
model1.null <- polr(as.ordered(Q1) ~ 1, data = subset,
                    method = "logistic", Hess = TRUE)

model1.cut1 <- summary(model1)$coefficients[12, 1] %>% 
  as.numeric %>% round(., 3)
model1.cut1.se <- summary(model1)$coefficients[12, 2] %>% 
  as.numeric %>% round(., 3)
model1.cut2 <- summary(model1)$coefficients[13, 1] %>% 
  as.numeric %>% round(., 3)
model1.cut2.se <- summary(model1)$coefficients[13, 2] %>% 
  as.numeric %>% round(., 3)
model1.cut3 <- summary(model1)$coefficients[14, 1] %>% 
  as.numeric %>% round(., 3)
model1.cut3.se <- summary(model1)$coefficients[14, 2] %>% 
  as.numeric %>% round(., 3)
model1.cut4 <- summary(model1)$coefficients[15, 1] %>% 
  as.numeric %>% round(., 3)
model1.cut4.se <- summary(model1)$coefficients[15, 2] %>% 
  as.numeric %>% round(., 3)

lr1 <- round(model1.null$deviance - model1$deviance, 3)
model2 <- polr(as.ordered(Q2) ~ as.factor(age) + Q19 + Honam + PK + 
                 TK + gender + edu + income, data = subset,
               method = "logistic", Hess = TRUE)
model2.null <- polr(as.ordered(Q2) ~ 1, data = subset,
                    method = "logistic", Hess = TRUE)
model2.cut1 <- summary(model2)$coefficients[12, 1] %>% 
  as.numeric %>% round(., 3)
model2.cut1.se <- summary(model2)$coefficients[12, 2] %>% 
  as.numeric %>% round(., 3)
model2.cut2 <- summary(model2)$coefficients[13, 1] %>% 
  as.numeric %>% round(., 3)
model2.cut2.se <- summary(model2)$coefficients[13, 2] %>% 
  as.numeric %>% round(., 3)
model2.cut3 <- summary(model2)$coefficients[14, 1] %>% 
  as.numeric %>% round(., 3)
model2.cut3.se <- summary(model2)$coefficients[14, 2] %>% 
  as.numeric %>% round(., 3)
model2.cut4 <- summary(model2)$coefficients[15, 1] %>% 
  as.numeric %>% round(., 3)
model2.cut4.se <- summary(model2)$coefficients[15, 2] %>% 
  as.numeric %>% round(., 3)
lr2 <- round(model2.null$deviance - model2$deviance, 3)
stargazer(model1, model2, 
          title = "Ordered Logistic Regressions: Attitudes toward North Korea after the Panmunjom Declaration",
          label = "tab1",
          dep.var.caption = "Attitudes toward North Korea",
          dep.var.labels.include = F,
          single.row = F,
          model.names = T,
          model.numbers = T,
          column.labels = c("Leadership",
                            "General"),
          covariate.labels = c(
            "Generation: 30s", "Generation: 40s", "Generation: 50s", 
            "Generation: 60+", "Evaluation of the Govt.'s NK policy",
            "Region: Honam", "Region: PK", "Region: TK",
            "Socio-demografic: Gender","Socio-demografic: Education",
            "Socio-demografic: Income"),
          header = F,
          keep.stat = c("n"),
          add.lines = list(c("cut1", model1.cut1, model2.cut1),
                           c("", paste0("(", model1.cut1.se,")"), 
                             paste0("(",model2.cut1.se,")")), 
                           c("cut2", model1.cut2, model2.cut2),
                           c("", paste0("(", model1.cut2.se,")"), 
                             paste0("(", model2.cut2.se,")")), 
                           c("cut3", model1.cut3, model2.cut3),
                           c("", paste0("(", model1.cut3.se,")"), 
                             paste0("(", model2.cut3.se),")"), 
                           c("cut4", model1.cut4, model2.cut4),
                           c("", paste0("(", model1.cut4.se,")"), 
                             paste0("(", model2.cut4.se),")"), 
                           c("Log-likelihood ratio", lr1, lr2)),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Standard errors in parentheses.",
                    "PK/TK are abbreviations of the regions:",
                    "PK: Pusan/Ulsan/Kyeongnam",
                    "TK:Taegu/Kyeongbuk"),
          table.placement = "H",
          table.layout = "=lc-tas-n")

## Figure 9
## Simulate coefficiets and cutpoints (King, Tomz, and Wittenberg 2000)
set.seed(1234)
simb <- mvrnorm(n = 1000, mu = c(coef(model1), model1$zeta), 
                Sigma = vcov(model1))
cut <- cbind(-Inf, simb[, 12:15], Inf)
# Create the baseline profile + Calculate the Predicted Probs
# Here, let's assume that the "middle income" means income at the median
x0 <- c(0, 0, 0, 0, 
        mean(subset$Q19, na.rm = T),
        mean(subset$Honam, na.rm = T),
        mean(subset$PK, na.rm = T),
        mean(subset$TK, na.rm = T),
        mean(subset$gender, na.rm = T),
        mean(subset$edu, na.rm = T),
        mean(subset$income, na.rm = T))

x0b <- simb[, 1:11] %*% x0
pr0 <- matrix(NA, nrow = 1000, ncol = 5)
for (i in 1:5){ # for each value of the dependent variable...
  pr0[, i] <- plogis(cut[, i + 1] - x0b) - plogis(cut[, i] - x0b)
}

# Matrix to store the results
out.mat <- matrix(NA, nrow = 5 * 4, ncol = 3)
rownames(out.mat) <- paste0(rep(c("30s", "40s", "50s", "60+"), each = 5), 
                            "-", rep(1:5, times = 3))
colnames(out.mat) <- c("Mean", "Lower", "Upper")
# Then compute the first differences in each covariates
# 20s -> 30s
x1 <- x0
x1[1] <- 1

x1b <- simb[, 1:11] %*% x1
for (i in 1:5){ # for each value of the dependent variable...
  pr1 <- plogis(cut[, i + 1] - x1b) - plogis(cut[, i] - x1b)
  fdiff <- pr1 - pr0[, i]
  out.mat[i, 1] <- mean(fdiff)
  out.mat[i, 2:3] <- quantile(fdiff, probs = c(0.025, 0.975))
}#

# 20s -> 40s
x1 <- x0
x1[2] <- 1

x1b <- simb[, 1:11] %*% x1
for (i in 1:5){ # for each value of the dependent variable...
  pr1 <- plogis(cut[, i + 1] - x1b) - plogis(cut[, i] - x1b)
  fdiff <- pr1 - pr0[, i]
  out.mat[i + 5, 1] <- mean(fdiff)
  out.mat[i + 5, 2:3] <- quantile(fdiff, probs = c(0.025, 0.975))
}#

# 20s -> 50s
x1 <- x0
x1[3] <- 1

x1b <- simb[, 1:11] %*% x1
for (i in 1:5){ # for each value of the dependent variable...
  pr1 <- plogis(cut[, i + 1] - x1b) - plogis(cut[, i] - x1b)
  fdiff <- pr1 - pr0[, i]
  out.mat[i + 10, 1] <- mean(fdiff)
  out.mat[i + 10, 2:3] <- quantile(fdiff, probs = c(0.025, 0.975))
}#

# 20s -> 60s
x1 <- x0
x1[4] <- 1

x1b <- simb[, 1:11] %*% x1
for (i in 1:5){ # for each value of the dependent variable...
  pr1 <- plogis(cut[, i + 1] - x1b) - plogis(cut[, i] - x1b)
  fdiff <- pr1 - pr0[, i]
  out.mat[i + 15, 1] <- mean(fdiff)
  out.mat[i + 15, 2:3] <- quantile(fdiff, probs = c(0.025, 0.975))
}#

# Visualize the results
out.mat <- as.data.frame(out.mat)
out.mat$leadership <- rep(1:5, times = 4)
pr.table <- bind_rows(pr.table40 <- 
                        out.mat[1:5,] %>% mutate(
                          generation = "30s"
                        ),
                      pr.table40 <- 
                        out.mat[6:10,] %>% mutate(
                          generation = "40s"
                        ),
                      pr.table50 <- 
                        out.mat[11:15,] %>% mutate(
                          generation = "50s"),
                      pr.table60 <- 
                        out.mat[16:20,] %>% mutate(
                          generation = "60+"
                        )) %>% 
  mutate(
    generation = generation %>% 
      parse_factor(., levels = c("30s", "40s", "50s", 
                                 "60+"), 
                   include_na = F, ordered = T),
    leadership = case_when(
      leadership == 1L ~ "Very\nDissatisfied",
      leadership == 2L ~ "Dissatisfied",
      leadership == 3L ~ "Neutral",
      leadership == 4L ~ "Satisfied",
      leadership == 5L ~ "Very\nSatisfied"
    ) %>% parse_factor(., levels = c("Very\nDissatisfied",
                                     "Dissatisfied",
                                     "Neutral",
                                     "Satisfied",
                                     "Very\nSatisfied"), ordered = T, 
                       include_na = F)
  )

pr.table %>% dplyr::filter(generation %in% c("40s", "50s")) %>%
  ggplot(aes(x = leadership, y = Mean,
             color = generation, 
             fill = generation)) + 
  geom_point(
    #position = position_dodge2(0.6), 
    show.legend = T,
    size = 1) + 
  geom_pointrange(aes(ymin = Lower, ymax = Upper),
                  #             position = position_dodge2(0.6), 
                  show.legend = F, size = 0.3) +
  labs(x = "", y = "First Differences in Predicted Probabilities") + #ylim(-0.14, 0.09) +
  scale_y_continuous(labels = scales::percent_format()) + 
  facet_wrap(~generation, ncol = 2) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 6))

## Table 2: Ordered logistic regressions: 
## Attitudes toward N.K. after the Panmunjom Declaration

model3 <- polr(as.ordered(Q4) ~ Q3 + Q19 + Honam + PK + 
                 TK + gender + edu + income, data = subset,
               method = "logistic", Hess = TRUE)
model3.null <- polr(as.ordered(Q4) ~ 1, data = subset,
                    method = "logistic", Hess = TRUE)
lr3 <- round(model3.null$deviance - model3$deviance, 3)

model4 <- polr(as.ordered(Q4) ~ I(as.factor(age)) + Q3 + Q19 + Honam + PK + 
                 TK + gender + edu + income, data = subset,
               method = "logistic", Hess = TRUE)
model4.null <- polr(as.ordered(Q4) ~ 1, data = subset,
                    method = "logistic", Hess = TRUE)
lr4 <- round(model4.null$deviance - model4$deviance, 3)

model5 <- polr(as.ordered(Q4) ~ I(Q5_re) + Q3 + Q19 + Honam + PK + 
                 TK + gender + edu + income, data = subset,
               method = "logistic", Hess = TRUE)
model5.null <- polr(as.ordered(Q4) ~ 1, data = subset,
                    method = "logistic", Hess = TRUE)
lr5 <- round(model5.null$deviance - model5$deviance, 3)

model6 <- polr(as.ordered(Q4) ~ I(as.factor(age)) + I(Q5_re) + 
                 I(as.factor(age))*I(Q5_re) +
                 Q3 + Q19 + Honam + PK + 
                 TK + gender + edu + income, data = subset,
               method = "logistic", Hess = TRUE)
model6.null <- polr(as.ordered(Q4) ~ 1, data = subset,
                    method = "logistic", Hess = TRUE)
lr6 <- round(model6.null$deviance - model6$deviance, 3)

model3.cut1 <- summary(model3)$coefficients[9, 1] %>% 
  as.numeric %>% round(., 3)
model3.cut1.se <- summary(model3)$coefficients[9, 2] %>% 
  as.numeric %>% round(., 3)
model3.cut2 <- summary(model3)$coefficients[10, 1] %>% 
  as.numeric %>% round(., 3)
model3.cut2.se <- summary(model3)$coefficients[10, 2] %>% 
  as.numeric %>% round(., 3)
model3.cut3 <- summary(model3)$coefficients[11, 1] %>% 
  as.numeric %>% round(., 3)
model3.cut3.se <- summary(model3)$coefficients[11, 2] %>% 
  as.numeric %>% round(., 3)

model4.cut1 <- summary(model4)$coefficients[13, 1] %>% 
  as.numeric %>% round(., 3)
model4.cut1.se <- summary(model4)$coefficients[13, 2] %>% 
  as.numeric %>% round(., 3)
model4.cut2 <- summary(model4)$coefficients[14, 1] %>% 
  as.numeric %>% round(., 3)
model4.cut2.se <- summary(model4)$coefficients[14, 2] %>% 
  as.numeric %>% round(., 3)
model4.cut3 <- summary(model4)$coefficients[15, 1] %>% 
  as.numeric %>% round(., 3)
model4.cut3.se <- summary(model4)$coefficients[15, 2] %>% 
  as.numeric %>% round(., 3)

model5.cut1 <- summary(model5)$coefficients[10, 1] %>% 
  as.numeric %>% round(., 3)
model5.cut1.se <- summary(model5)$coefficients[10, 2] %>% 
  as.numeric %>% round(., 3)
model5.cut2 <- summary(model5)$coefficients[11, 1] %>% 
  as.numeric %>% round(., 3)
model5.cut2.se <- summary(model5)$coefficients[11, 2] %>% 
  as.numeric %>% round(., 3)
model5.cut3 <- summary(model5)$coefficients[12, 1] %>% 
  as.numeric %>% round(., 3)
model5.cut3.se <- summary(model5)$coefficients[12, 2] %>% 
  as.numeric %>% round(., 3)

model6.cut1 <- summary(model6)$coefficients[18, 1] %>% 
  as.numeric %>% round(., 3)
model6.cut1.se <- summary(model6)$coefficients[18, 2] %>% 
  as.numeric %>% round(., 3)
model6.cut2 <- summary(model6)$coefficients[19, 1] %>% 
  as.numeric %>% round(., 3)
model6.cut2.se <- summary(model6)$coefficients[19, 2] %>% 
  as.numeric %>% round(., 3)
model6.cut3 <- summary(model6)$coefficients[20, 1] %>% 
  as.numeric %>% round(., 3)
model6.cut3.se <- summary(model6)$coefficients[20, 2] %>% 
  as.numeric %>% round(., 3)

stargazer(model3, model4, model5, model6,
          title = "Ordered Logistic Regressions: Attitudes toward North Korea after the Panmunjom Declaration",
          label = "tab2",
          dep.var.caption = "Attitudes toward Unification",
          dep.var.labels.include = F,
          single.row = F,
          model.names = T,
          model.numbers = T,
          column.labels = c("Base", "Generation", "Prospects",
                            "Full"),
          covariate.labels = c(
            "30s", "40s", "50s", "60+", 
            "Prospects",
            "Interests",
            "NK policy Eval.",
            "Honam", "PK", "TK",
            "Gender","Education",
            "Income",
            "30s$\\times$Prospects",
            "40s$\\times$Prospects",
            "50s$\\times$Prospects",
            "60s$\\times$Prospects"),
          header = F,
          keep.stat = c("n"),
          add.lines = list(c("cut1", 
                             model3.cut1, model4.cut1, 
                             model5.cut1, model6.cut1),
                           c("", 
                             paste0("(", model3.cut1.se,")"), 
                             paste0("(", model4.cut1.se,")"),
                             paste0("(", model5.cut1.se,")"),
                             paste0("(", model6.cut1.se,")")), 
                           c("cut2", 
                             model3.cut2, model4.cut2,
                             model5.cut2, model6.cut2),
                           c("", 
                             paste0("(", model3.cut2.se,")"), 
                             paste0("(", model4.cut2.se,")"),
                             paste0("(", model5.cut2.se,")"),
                             paste0("(", model6.cut2.se,")")), 
                           c("cut3", 
                             model3.cut3, model4.cut3,
                             model5.cut3, model6.cut3),
                           c("", 
                             paste0("(", model3.cut3.se,")"), 
                             paste0("(", model4.cut3.se,")"),
                             paste0("(", model5.cut3.se,")"),
                             paste0("(", model6.cut3.se,")")), 
                           c("Log-likelihood ratio", 
                             lr3, lr4, lr5, lr6)),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Standard errors in parentheses."),
          #         table.placement = "H", 
          table.layout = "=lc-tas-n")

## Figure 10

subset$Q4_int <- as.factor(as.numeric(subset$Q4))
model6a <- polr(Q4_int ~ age + Q5_re + age*Q5_re + 
                  Q3 + Q19 + Honam + PK + 
                  TK + gender + edu + income, data = subset,
                method = "logistic", Hess = TRUE)

df <- ggpredict(model6a, terms = c("age", "Q5_re"))

df <- df %>% mutate(
  response.level = as.numeric(response.level)
)

df <- df %>% mutate(
  Generation = case_when(
    x == 1L ~ "20s",
    x == 2L ~ "30s",
    x == 3L ~ "40s",
    x == 4L ~ "50s",
    x == 5L ~ "60+"),
  DV = case_when(
    response.level == 1L ~ "Not\npreferred",
    response.level == 2L ~ "Preferred without\nBurdens",
    response.level == 3L ~ "Status quo",
    response.level == 4L ~ "Preferred",
  ) %>% parse_factor(., levels = c("Not\npreferred","Status quo","Preferred without\nBurdens","Preferred"),
                     ordered = T, include_na = T)
  
)


p50 <- df %>% dplyr::filter(Generation %in% c("50s")) %>%
  ggplot(aes(x = group, y = predicted, 
             color = Generation,
             fill = Generation, group = Generation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(show.legend = T) + 
  facet_wrap(~DV, ncol = 2) + 
  
  #  geom_point(size = 2, show.legend = T,
  #             position = position_dodge(0.5)) + 
  #  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
  #                  show.legend = F, size = 0.7,
  #                  position = position_dodge(0.5)) +
  labs(x = "", y = "Predicted Probabilities") + #ylim(-0.14, 0.09) +
  scale_y_continuous(labels = scales::percent_format()) + 
  # facet_wrap(~generation, ncol = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = futurevisions::futurevisions("mars")[2]) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")[2]) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 6)
  )



p60 <- df %>% dplyr::filter(Generation %in% c("60+")) %>%
  ggplot(aes(x = group, y = predicted, 
             color = Generation,
             fill = Generation, group = Generation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(show.legend = T) + 
  facet_wrap(~DV, ncol = 2) + 
  
  #  geom_point(size = 2, show.legend = T,
  #             position = position_dodge(0.5)) + 
  #  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
  #                  show.legend = F, size = 0.7,
  #                  position = position_dodge(0.5)) +
  labs(x = "", y = "Predicted Probability") + #ylim(-0.14, 0.09) +
  scale_y_continuous(labels = scales::percent_format()) + 
  # facet_wrap(~generation, ncol = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = futurevisions::futurevisions("mars")[3]) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")[3]) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 6)
  )
p50 + p60 + patchwork::plot_layout(ncol = 1)

## Figure 11

p20 <- df %>% dplyr::filter(Generation %in% c("20s")) %>%
  ggplot(aes(x = group, y = predicted, 
             color = Generation,
             fill = Generation, group = Generation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(show.legend = T) + 
  facet_wrap(~DV, ncol = 2) + 
  labs(x = "", y = "Predicted Probability") + #ylim(-0.14, 0.09) +
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = futurevisions::futurevisions("mars")) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 6)
  )


p30 <- df %>% dplyr::filter(Generation %in% c("30s")) %>%
  ggplot(aes(x = group, y = predicted, 
             color = Generation,
             fill = Generation, group = Generation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(show.legend = T) + 
  facet_wrap(~DV, ncol = 2) + 
  
  #  geom_point(size = 2, show.legend = T,
  #             position = position_dodge(0.5)) + 
  #  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
  #                  show.legend = F, size = 0.7,
  #                  position = position_dodge(0.5)) +
  labs(x = "", y = "Predicted Probability") + #ylim(-0.14, 0.09) +
  scale_y_continuous(labels = scales::percent_format()) + 
  # facet_wrap(~generation, ncol = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = futurevisions::futurevisions("mars")[4]) + 
  scale_fill_manual(values = futurevisions::futurevisions("mars")[4]) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 6)
  )

p40 <- df %>% dplyr::filter(Generation %in% c("40s")) %>%
  ggplot(aes(x = group, y = predicted, 
             color = Generation,
             fill = Generation, group = Generation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4) + 
  geom_line(show.legend = T) + 
  facet_wrap(~DV, ncol = 2) + 
  
  #  geom_point(size = 2, show.legend = T,
  #             position = position_dodge(0.5)) + 
  #  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
  #                  show.legend = F, size = 0.7,
  #                  position = position_dodge(0.5)) +
  labs(x = "", y = "Predicted Probability") + #ylim(-0.14, 0.09) +
  scale_y_continuous(labels = scales::percent_format()) + 
  # facet_wrap(~generation, ncol = 1) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_color_manual(values = futurevisions::futurevisions("jupiter")[4]) + 
  scale_fill_manual(values = futurevisions::futurevisions("jupiter")[4]) + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.subtitle = element_text(size = 7.5),
    axis.text.x = element_text(hjust = 0.5, size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 7.5),
    axis.text.y = element_text(size = 6),
    legend.text = element_text(size = 6.5),
    strip.text = element_text(size = 6)
  )


p20 + p30 + p40 + patchwork::plot_layout(ncol = 1)

################################################################################
## Appendix
################################################################################

## Appendix 1: Descriptive Statistics
names(subset)
Sys.setlocale(category ="LC_CTYPE", locale ="ko_KR.UTF-8")
description <- subset %>% 
  dplyr::select(Q1_fa, Q2_fa, Q4_fa, Q3_fa, 
                Q5_fa, Q6, Q9, Q19, Honam,
                PK, TK, gender, edu, income, age)
#moonBook::myhtml(moonBook::mytable(age~., data = description))
