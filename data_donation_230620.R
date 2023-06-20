# -----------------------------------------------------------------
# Analysis code for:
# Keusch et al. (under review). Do you have two minutes to talk about
# your data? Willingness to participate and nonparticipation bias in
# Facebook data donation
# 
# Date: June 20, 2023
# 
# Data available at https://doi.org/10.7802/2524 
# -----------------------------------------------------------------
# Install and load required packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(DescTools)) install.packages("DescTools")
if (!require(margins)) install.packages("margins")
if (!require(ggplot2)) install.packages("ggplot2")

library(dplyr)
library(DescTools)
library(margins)
library(ggplot2)

# Load data
full_data_x <- `PINCET_v1-1-0`
 
# Subset to survey participants in W4
survey_w4 <- full_data_x %>% 
  filter(w4_fb_data_donated == "yes" |
           w4_fb_data_donated == "no")

# Subset to participants data donation task
donation_task <- survey_w4 %>% 
  filter(w4_fb_data_donation == "Ja" |
           w4_fb_data_donation == "Nein")

# Data preparation and descriptive statistics
donation_task <- donation_task %>% 
  mutate(w4_gender = as.factor(w4_gender)) %>% 
  mutate(w4_gender_na = as.factor(ifelse(w4_gender == "männlich",
                                         "männlich",
                                         ifelse(w4_gender == "weiblich",
                                                "weiblich",
                                                NA))))

table(donation_task$w4_gender_na, useNA = "always")
prop.table(table(donation_task$w4_gender_na))

donation_task <- donation_task %>% 
  mutate(w4_age = ((as.numeric(w4_birth_year))-2021)*(-1)) %>% 
  mutate(w4_age_cat = ifelse(w4_age < 30,
                             "under 30",
                             ifelse(w4_age > 29 & w4_age < 50,
                                    "31-50",
                                    "51+"))) %>%
  mutate(w4_age_cat = factor(w4_age_cat, levels = c("under 30",
                                                    "31-50",
                                                    "51+")))

table(donation_task$w4_age_cat, useNA = "always")
prop.table(table(donation_task$w4_age_cat))

donation_task <- donation_task %>% 
  mutate(w4_edu_cat = ifelse(w4_edu == "(Noch) kein Schulabschluss" |
                               w4_edu == "Abschluss einer Förderschule (Sonderschule, Hilfsschule)" |
                               w4_edu == "Volks- oder Hauptschulabschluss bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 8. oder 9. Klasse" |
                               w4_edu == "Anderer Schulabschluss, und zwar",
                             "low",
                             ifelse(w4_edu == "Mittlere Reife, Realschulabschluss, Fachoberschulreife oder mittlerer Schulabschluss bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 10. Klasse",
                                    "medium",
                                    ifelse(w4_edu == "Allgemeine oder fachgebundene Hochschulreife, Abitur",
                                           "high",
                                           NA)))) %>%
  mutate(w4_edu_cat = factor(w4_edu_cat, levels = c("low",
                                                    "medium",
                                                    "high")))

table(donation_task$w4_edu_cat, useNA = "always")
prop.table(table(donation_task$w4_edu_cat))

donation_task <- donation_task %>% 
  mutate(w4_fb_use = factor(w4_fb_use, levels = c("mehrmals im Monat",
                                                  "mehrmals in der Woche",
                                                  "täglich",
                                                  "mehrmals täglich")))

table(donation_task$w4_fb_use, useNA = "always")
prop.table(table(donation_task$w4_fb_use))

donation_task <- donation_task %>% 
  mutate(w4_trust_general_num = ifelse(w4_trust_general == "0 man kann nicht vorsichtig genug sein", "0",
                                   ifelse(w4_trust_general == "10 man kann den meisten Menschen vertrauen", "10", w4_trust_general))) %>% 
  mutate(w4_trust_general_num = as.numeric(w4_trust_general_num))

donation_task %>%
  summarise(mean = mean(w4_trust_general_num, na.rm=T),
            median = median(w4_trust_general_num, na.rm=TRUE),
            sd = sd(w4_trust_general_num, na.rm=TRUE),
            sumNA = sum(is.na(w4_trust_general_num)),
            n = n())

donation_task <- donation_task %>% 
  mutate(w4_trust_dt_fb_num = ifelse(w4_trust_dt_fb == "0 überhaupt nicht",
                                     "0",
                                     ifelse(w4_trust_dt_fb == "10 voll und ganz",
                                            "10",
                                            w4_trust_dt_fb))) %>% 
  mutate(w4_trust_dt_fb_num = as.numeric(w4_trust_dt_fb_num))

donation_task %>%
  summarise(mean = mean(w4_trust_dt_fb_num, na.rm=T),
            median = median(w4_trust_dt_fb_num, na.rm=TRUE),
            sd = sd(w4_trust_dt_fb_num, na.rm=TRUE),
            sumNA = sum(is.na(w4_trust_dt_fb_num)),
            n = n())

donation_task <- donation_task %>% 
  mutate(w4_trust_dt_research_num = ifelse(w4_trust_dt_research == "0 überhaupt nicht",
                                           "0",
                                           ifelse(w4_trust_dt_research == "10 voll und ganz",
                                                  "10",
                                                  w4_trust_dt_research))) %>% 
  mutate(w4_trust_dt_research_num = as.numeric(w4_trust_dt_research_num))

donation_task %>%
  summarise(mean = mean(w4_trust_dt_research_num, na.rm=T),
            median = median(w4_trust_dt_research_num, na.rm=TRUE),
            sd = sd(w4_trust_dt_research_num, na.rm=TRUE),
            sumNA = sum(is.na(w4_trust_dt_research_num)),
            n = n())

donation_task <- donation_task %>% 
  mutate(w4_concern_privacy_num = ifelse(w4_concern_privacy == "überhaupt nicht besorgt", "1",
                                       ifelse(w4_concern_privacy == "wenig besorgt", "2",
                                              ifelse(w4_concern_privacy == "etwas besorgt", "3",
                                                     ifelse(w4_concern_privacy == "sehr besorgt", "4", NA))))) %>% 
  mutate(w4_concern_privacy_num = as.numeric(w4_concern_privacy_num))

donation_task %>%
  summarise(mean = mean(w4_concern_privacy_num, na.rm=T),
            median = median(w4_concern_privacy_num, na.rm=TRUE),
            sd = sd(w4_concern_privacy_num, na.rm=TRUE),
            sumNA = sum(is.na(w4_concern_privacy_num)),
            n = n())

donation_task <- donation_task %>% 
  mutate(w4_data_don_quest_type = factor(donation_task$w4_data_don_quest_type))

donation_task <- donation_task %>% 
  mutate(w4_fb_data_donation = factor(w4_fb_data_donation, levels = c("Nein", "Ja")))

# RQ1. How willing are Facebook users to donate their data?
table(donation_task$w4_fb_data_donation, useNA = "always")
prop.table(table(donation_task$w4_fb_data_donation))

# Reasons for not willing to donate data
table(donation_task$w4_fb_data_don_reason1)
prop.table(table(donation_task$w4_fb_data_don_reason1))

table(donation_task$w4_fb_data_don_reason2)
prop.table(table(donation_task$w4_fb_data_don_reason2))

table(donation_task$w4_fb_data_don_reason3)
prop.table(table(donation_task$w4_fb_data_don_reason3))

# RQ2. How successful are Facebook users donating the data?
donation <- donation_task %>% 
  filter(donation_task$w4_fb_data_donation == "Ja")

donation <- donation %>%
  mutate(w4_fb_data_donation = as.factor(w4_fb_data_donation))

table(donation$w4_fb_data_donated, useNA = "always")
prop.table(table(donation$w4_fb_data_donated))

# Reasons for not donating data
table(donation_task$w4_fb_data_don_success_reason1)
prop.table(table(donation_task$w4_fb_data_don_success_reason1))

# RQ1a. What effect does the framing of the data donation request have on willingness to donate? 
# Willingness
gl_willing <- table(donation_task$w4_fb_data_donation, donation_task$w4_data_don_quest_type)
prop.table(gl_willing, 2)
chisq.test(gl_willing)

# Successful linkage
gl_linkage <- table(donation$w4_fb_data_donated, donation$w4_data_don_quest_type)
prop.table(gl_linkage, 2)
chisq.test(gl_linkage)

# RQ3. RQ3.	What bias does arise from selective willingness to donate and successful donation of Facebook data?
# Willingness
lr_willing <- glm(w4_fb_data_donation ~
                    w4_gender_na +
                    w4_age_cat +
                    w4_edu_cat +
                    w4_fb_use +
                    w4_trust_general_num +
                    w4_trust_dt_fb_num +
                    w4_trust_dt_research_num +
                    w4_concern_privacy_num + 
                    w4_data_don_quest_type,
                  family = binomial(link = "logit"),
                  donation_task)

summary(lr_willing)
PseudoR2(lr_willing, which = NULL)
AME_willing <- margins_summary(lr_willing)
AME_willing

# Successful linkage
lr_linkage <- glm(as.factor(w4_fb_data_donated) ~
                    w4_gender_na +
                    w4_age_cat +
                    w4_edu_cat +
                    w4_fb_use +
                    w4_trust_general_num +
                    w4_trust_dt_fb_num +
                    w4_trust_dt_research_num +
                    w4_concern_privacy_num + 
                    w4_data_don_quest_type,
                  family = binomial(link = "logit"),
                  donation)

summary(lr_linkage)
PseudoR2(lr_linkage, which = NULL)
AME_linkage <- margins_summary(lr_linkage)
AME_linkage

# Create Figure 1.
AME_willing_fig <- data.frame(AME_willing) %>% 
  mutate(type = factor("Willingness to donate"),
         variable = factor) %>%
  select("variable", "AME", "lower", "upper", "type")

AME_empty_will <- data.frame(variable = c("Gender (Ref: Male)",
                                   "Age (Ref: 18-30 years)",
                                   "Educational attainment (Ref: Low)",
                                   "Freq. Facebook use \n(Ref: Several times a month)",
                                   "Framing (Ref: Gain)"),
                        AME = NA_real_,
                        lower = NA_real_,
                        upper = NA_real_,
                        type = ("Willingness to donate"))

AME_linkage_fig <- data.frame(AME_linkage) %>% 
  mutate(type = factor("Successful linking"),
         variable = factor) %>% 
  select("variable", "AME", "lower", "upper", "type")

AME_empty_link <- data.frame(variable = c("Gender (Ref: Male)",
                                        "Age (Ref: 18-30 years)",
                                        "Educational attainment (Ref: Low)",
                                        "Freq. Facebook use \n(Ref: Several times a month)",
                                        "Framing (Ref: Gain)"),
                             AME = NA_real_,
                             lower = NA_real_,
                             upper = NA_real_,
                             type = ("Successful linking"))

AME_fig <- bind_rows(AME_willing_fig,
                     AME_empty_will,
                     AME_linkage_fig,
                     AME_empty_link)

level_changes <- c("w4_age_cat31-50" = "31-50 years",
                   "w4_age_cat51+" = "51 years and older",
                   "w4_concern_privacy_num" = "Privacy concerns",
                   "w4_data_don_quest_typeloss" = "Loss",
                   "w4_edu_cathigh" = "High",
                   "w4_edu_catmedium" = "Medium",
                   "w4_fb_usemehrmals in der Woche" = "Several times a week",
                   "w4_fb_usemehrmals täglich" = "Several times a day",
                   "w4_fb_usetäglich" = "Daily",
                   "w4_gender_naweiblich" = "Female",
                   "w4_trust_dt_fb_num" = "Trust in Facebook",
                   "w4_trust_dt_research_num" = "Trust in researchers",
                   "w4_trust_general_num" = "General trust")

AME_fig <- AME_fig %>% 
  mutate(variable = as.factor(variable),
         type = as.factor(type)) %>% 
  mutate(variable = recode(variable, !!!level_changes)) %>% 
  mutate(variable = factor(variable, levels = c("Gender (Ref: Male)",
                                                "Female",
                                                "Age (Ref: 18-30 years)",
                                                "31-50 years",
                                                "51 years and older",
                                                "Educational attainment (Ref: Low)",
                                                "Medium",
                                                "High",
                                                "Freq. Facebook use \n(Ref: Several times a month)",
                                                "Several times a week",
                                                "Daily",
                                                "Several times a day",
                                                "General trust",
                                                "Trust in Facebook",
                                                "Trust in researchers",
                                                "Privacy concerns",
                                                "Framing (Ref: Gain)",
                                                "Loss"))) %>%
  mutate(type = factor(type, levels = c("Willingness to donate",
                                               "Successful linking")))

ggplot(AME_fig, aes(x = AME, y = variable, xmin = lower, xmax = upper,
                    group = type, shape = type)) +
  geom_point(aes(group = type), position = position_dodge(0.4), size = 2) +
  geom_errorbarh(aes(group = type), position = position_dodge(0.4), height = .2) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "dark grey") +
  scale_y_discrete(limits = rev(levels(AME_fig$variable))) +
  labs(y = NULL) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

ggsave("Figure_1.jpg", last_plot(), width = 5, height = 5, dpi = 300)