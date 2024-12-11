pacman::p_load(
  rio,          # File import
  here,         # File locator
  gtsummary,    # summary statistics and tests
  rstatix,      # summary statistics and statistical tests
  janitor,      # adding totals and percents to tables
  ggrepel,      # adding labels to plots
  tidyverse     # data management + ggplot2 graphics 
)


# Import clean data ----------------------------------------------------------------------------------------
load(here("0.data", "governance_syndrome.RData"))
load(here("0.data", "dpsir_clean_x0008_countriesfiltered241207.RData"))

# Clean data -----------------------------------------------------------------------------------------------
data2016_new <- data2016 %>% 
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))
data2017_new <- data2017 %>% 
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))
data2018_new <- data2018 %>%
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))
data2019_new <- data2019 %>%
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))
data2020_new <- data2020 %>%
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))
data2021_new <- data2021 %>%
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))
data2022_new <- data2022 %>%
  mutate(across(-1, ~ recode(., "A" = 0, "B" = 1, "C" = 2, "D" = 3, "E" = 4)))


# With 2016 and 2017 data, create new variable X6.1_Awareness_raising_general as the mean of X6.1_Awareness_raising_human and X6.2_Awareness_raising_animal to match with data of the following year
data2016_new <- data2016_new %>% 
  mutate(X6.1_Awareness_raising_general = (X6.1_Awareness_raising_human + X6.2_Awareness_raising_animal) / 2) %>% 
  # relocate the new created column before X6.1_Awareness_raising_human
  relocate(X6.1_Awareness_raising_general, .before = X6.1_Awareness_raising_human)
data2017_new <- data2017_new %>%
  mutate(X6.1_Awareness_raising_general = (X6.1_Awareness_raising_human + X6.2_Awareness_raising_animal) / 2) %>% 
  # relocate the new created column before X6.1_Awareness_raising_human
  relocate(X6.1_Awareness_raising_general, .before = X6.1_Awareness_raising_human)

# With 2021 and 2022 data, create new variable X8.2_IPC_animal_average as the mean of X8.2_IPC_animal and X8.2_IPC_animal_aquatic to match with data of the following year, then rename it back to X8.2_IPC_animal
data2021_new <- data2021_new %>% 
  mutate(X8.2_IPC_animal_average = (X8.2_IPC_animal + X8.2_IPC_animal_aquatic) / 2) %>% 
  # relocate the new created column before X8.1_IPC_animal
  relocate(X8.2_IPC_animal_average, .before = X8.2_IPC_animal) %>% 
  select(-X8.2_IPC_animal, -X8.2_IPC_animal_aquatic) %>% 
  rename(X8.2_IPC_animal = X8.2_IPC_animal_average)


data2022_new <- data2022_new %>% 
  mutate(X8.2_IPC_animal_average = (X8.2_IPC_animal + X8.2_IPC_animal_aquatic) / 2) %>% 
  # relocate the new created column before X8.1_IPC_animal
  relocate(X8.2_IPC_animal_average, .before = X8.2_IPC_animal) %>% 
  select(-X8.2_IPC_animal, -X8.2_IPC_animal_aquatic) %>% 
  rename(X8.2_IPC_animal = X8.2_IPC_animal_average)

# recode country name
country_recode <- c(
  "United Kingdom of Great Britain and Northern Ireland (the)" = "United Kingdom",
  "the Republic of Cabo Verde" = "Cabo Verde",
  "North Macedonia" = "The former Yugoslav Republic of Macedonia",
  "Eswatini" = "Swaziland",
  "Dominican Republic (the)" = "Dominican Republic",
  "Czech Republic (the)" = "Czech Republic",
  "C<f4>te d'Ivoire" = "Cote dIvoire", 
  "C\xf4te d'Ivoire" = "Cote dIvoire",
  "Côte d'Ivoire" = "Cote dIvoire",
  "C?te d&#39;Ivoire" = "Cote dIvoire",
  "C?te d'Ivoire" = "Cote dIvoire",
  "Bahamas (the)" = "Bahamas",
  "Democratic People&#39;s Republic of Korea" = "Democratic People's Republic of Korea",
  "Lao People&#39;s Democratic Republic" = "Lao People's Democratic Republic",
  "Comoros (the)" = "Comoros")

data2016_new <- data2016_new %>% mutate(Country = recode(Country, !!!country_recode))
data2017_new <- data2017_new %>% mutate(Country = recode(Country, !!!country_recode))
data2018_new <- data2018_new %>% mutate(Country = recode(Country, !!!country_recode))
data2019_new <- data2019_new %>% mutate(Country = recode(Country, !!!country_recode))
data2020_new <- data2020_new %>% mutate(Country = recode(Country, !!!country_recode))
data2021_new <- data2021_new %>% mutate(Country = recode(Country, !!!country_recode))
data2022_new <- data2022_new %>% mutate(Country = recode(Country, !!!country_recode))



# Merge all data into one, select only question used -------------------------------------------------------
selQ <- c("X4.1_OneHealth", 
          "X5.1_NAP", 
          "X6.1_Awareness_raising_general", "X6.3_Training_human", "X6.4_Training_veterinary", "X6.5_Progress_veterinary_services",
          "X7.1_Surveillance_use_human", "X7.4_Surveillance_AMR_human",
          "X8.1_IPC_human", "X8.2_IPC_animal",
          "X9.1_Stewardship_human_use", "X9.2_Stewardship_animal_use")

groupQ <- c("CrossSector","NAP","Education","Monitoring","Prevention","Regulation")

data2016_new <- data2016_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2016)
data2017_new <- data2017_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2017)
data2018_new <- data2018_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2018)
data2019_new <- data2019_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2019)
data2020_new <- data2020_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2020)
data2021_new <- data2021_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2021)
data2022_new <- data2022_new %>% select(Country, all_of(selQ)) %>% mutate(Year = 2022)

data_final <- bind_rows(data2016_new, data2017_new, data2018_new, data2019_new, data2020_new, data2021_new, data2022_new) %>% 
  arrange(Country, Year) %>% 
  relocate(Year, .after = Country) %>% 
  # merge with key.df to get EORA_a3 and META_ID
  left_join(key.df[,c("EORA_a3","EORA_a2","UN_Country")], by = c("Country" = "UN_Country")) %>% 
  relocate(EORA_a3, EORA_a2, .after = Country) %>% 
  #pivot longer, so all questions are in one column
  pivot_longer(cols = -c(Country, Year, EORA_a3, EORA_a2), names_to = "Question", values_to = "value") %>% 
  # Fill missing values with the value of the same country in the previous year (333 were filled)
  # data_final %>% 
  # select(-Country, -EORA_a3, -EORA_a2, -Year) %>% 
  # summarise_all(~ sum(is.na(.)))
  group_by(EORA_a3) %>% 
  fill(value, .direction = "updown") %>% 
  ungroup() %>% 
  # Add group column based on groupQ
  mutate(Group = case_when(
    Question %in% c("X4.1_OneHealth") ~ "CrossSector",
    Question %in% c("X5.1_NAP") ~ "NAP",
    Question %in% c("X6.1_Awareness_raising_general", "X6.3_Training_human", "X6.4_Training_veterinary", "X6.5_Progress_veterinary_services") ~ "Education",
    Question %in% c("X7.1_Surveillance_use_human", "X7.4_Surveillance_AMR_human") ~ "Monitoring",
    Question %in% c("X8.1_IPC_human", "X8.2_IPC_animal") ~ "Prevention",
    Question %in% c("X9.1_Stewardship_human_use", "X9.2_Stewardship_animal_use") ~ "Regulation")) 



# Aggregated information -----------------------------------------------------------------------------------
### now do aggregation and by type of question and standardize variables and do clustering
data_final.group <- data_final %>% 
  group_by(EORA_a3, Year, Group) %>% 
  summarise(value = mean(value))

data_final.total <- data_final.group %>% 
  group_by(EORA_a3, Year) %>% 
  summarise(value = mean(value)) %>% 
  mutate(Group = "TotalResponse") %>% 
  relocate(Group, .before = Year) %>% 
  bind_rows(data_final.group) %>% 
  arrange(EORA_a3, Year)


data_final.total.wid.comp <- data_final.total %>% 
  pivot_wider(values_from = "value",
              names_from = "Year") %>% 
  ungroup() %>%
  # create a new variable, which is the count of how many NA values in the row
  mutate(na_count = rowSums(is.na(.))) %>% 
  filter(na_count <= 2) %>% select(-na_count) %>% # all countries need to have full years
  filter(EORA_a3 != "KWT") %>%  #remove KWT has 4 response
  # only allow missing maximum 2 years
  # if year 2016 is not missing, take it as init_resp, if missing, take 2017, else 2018
  # if year 2022 is not missing, take it as end_resp, if missing, take 2021
  mutate(init_resp = coalesce(`2016`, `2017`, `2018`),
         end_resp = coalesce(`2022`, `2021`),
         diff_resp = end_resp - init_resp) %>%
  relocate(`2016`, .before = `2017`) 

# check_endpoint_response <- data_final.total.wid.comp %>% 
#   filter(Group == "TotalResponse") %>% 
#   mutate(use_2022 = ifelse(`2022` == end_resp, "Yes", "No"),
#          use_2021 = ifelse(`2021` == end_resp, "Yes", "No"),
#          use_2020 = ifelse(`2020` == end_resp, "Yes", "No"),
#          use_2019 = ifelse(`2019` == end_resp, "Yes", "No"),
#          use_2018 = ifelse(`2018` == end_resp, "Yes", "No"),
#          use_2017 = ifelse(`2017` == end_resp, "Yes", "No"),
#          use_2016 = ifelse(`2016` == end_resp, "Yes", "No"))
# 
# # count number of Yes in columns use_2022, use_2021, etc.
# count_country_response <- dpsir_final %>% 
#   left_join(check_endpoint_response, by = c("ISO3" = "EORA_a3")) %>% 
#   rename(GroupR = Group) %>% 
#   select(name, ISO3, use_2022, use_2021, use_2020, use_2019, use_2018, use_2017, use_2016, change, init_resp, end_resp, diff_resp, `2022`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`) %>% 
#   mutate(syndrome = case_when(
#     change < 0 & diff_resp  > 0 ~ "A",        # Virtuous cycle      - Very good 
#     change > 0 & diff_resp  > 0 ~ "B",        # Meeting challenge   - Good
#     change < 0 & diff_resp <= 0 ~ "C",        # Relaxed response    - Bad
#     change > 0 & diff_resp <= 0 ~ "D")) %>%   # Vicious cycle       - Very bad
#   drop_na(syndrome) %>% select(-change) %>% 
#   distinct(name, .keep_all = TRUE)
  
# only Romania use 2021 survey
  
  
# Reverse back to long format
data_final.total.change <- data_final.total.wid.comp %>% 
  select(c("EORA_a3", "Group", "init_resp","end_resp","diff_resp")) 
# -> this one is comparable with amr.total.change.df


# diff response
dframe.gc.Rstats <- dpsir_final %>% 
  left_join(filter(data_final.total.change, Group == "TotalResponse"), by = c("ISO3" = "EORA_a3")) %>% 
  rename(GroupR = Group) %>% 
  # AMR governance syndrome
  mutate(syndrome = case_when(
    change < 0 & diff_resp  > 0 ~ "A",        # Virtuous cycle      - Very good 
    change > 0 & diff_resp  > 0 ~ "B",        # Meeting challenge   - Good
    change < 0 & diff_resp <= 0 ~ "C",        # Relaxed response    - Bad
    change > 0 & diff_resp <= 0 ~ "D"))       # Vicious cycle       - Very bad



# Data visualization ---------------------------------------------------------------------------------------
annotation <- dframe.gc.Rstats %>% 
  # filter(GroupR == "TotalResponse") %>% # CrossSector Education Monitoring NAP Prevention Regulation
  filter(SHORTNAME %in% c("driverTotal", "resTotal", "useTotal", "DRI")) %>% 
  drop_na(syndrome) %>% 
  mutate(syndrome = factor(syndrome, levels = c("A", "B", "C", "D"))) %>%
  group_by(income, DPSIR, syndrome) %>% summarise(count = n()) %>%
  mutate(freq = count*100 / sum(count),
         freq = round(freq, digits = 1))

annotation <- annotation %>% 
  # filter(income == "HIC") %>% 
  mutate(percent_answers = ifelse(syndrome %in% c("A", "B"), freq/100, -freq/100),
         percent_answers_label = scales::percent(freq/100, accuracy = 1),
         count_reverse = ifelse(syndrome %in% c("A", "B"), count, -count))

p3 <- annotation %>%
  mutate(DPSIR = factor(DPSIR, levels = c("DRI", "RESISTANCE", "USE","DRIVERS"))) %>% 
  mutate(syndrome = factor(syndrome, levels = c("A", "B", "C", "D"))) %>%
  ggplot(aes(x = DPSIR, 
             y = freq,  # percent_answers
             fill = syndrome)) +
  geom_col() +
  geom_text(aes(label = count),       # percent_answers_label # scales::percent(freq/100)
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3) +
  # geom_hline(yintercept = 0) +
  coord_flip() +
  facet_wrap(~income, ncol = 1, nrow = 2) +
  scale_x_discrete() +
  scale_fill_manual(breaks = c("D", "C", "B", "A"),
                    values = c("A" = "#4DAC26",
                               "B" = "#B8E186",
                               "C" = "#F1B6DA", 
                               "D" = "#D01C8B"),
                    labels = c("Vicious cycle", "Relaxed response",
                               "Meeting challenge", "Virtuous cycle")) +
  labs(x = NULL, fill = NULL) +
  theme_minimal() +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        legend.position = "bottom")


data_viz <- dframe.gc.Rstats %>% 
  filter(GroupR == "TotalResponse") %>% # CrossSector Education Monitoring NAP Prevention Regulation
  filter(SHORTNAME %in% c("driverTotal", "resTotal", "useTotal", "DRI")) %>%
  drop_na(syndrome) 


p2 <- data_viz %>% 
  ggplot(aes(y = change, x = diff_resp)) +
  ggdensity::geom_hdr() +
  geom_point(aes(fill = syndrome), shape = 21, size = 3) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  scale_fill_manual(breaks = c("D", "C", "B", "A"),
                    values = c("A" = "#4DAC26", 
                               "B" = "#B8E186", 
                               "C" = "#F1B6DA", 
                               "D" = "#D01C8B")) +
  facet_wrap(~DPSIR) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Linear Trend in ABR Indicators Between 2000-2016 ",
       x = "Difference in Governance Response Between 2016-2022",
       color = "ABR Governance Syndrome") +
  geom_text_repel(data = filter(data_viz, syndrome %in% c("D")),  # Filter points with y value larger than 1
                  aes(label = ISO3),  # Replace "Country" with the actual column name in your data frame
                  box.padding = 0.5, point.padding = 0.1, size = 4, colour = "#D01C8B",
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 30))


cowplot::plot_grid(p2, p3, rel_widths = c(5, 5),
                   ncol = 2, nrow = 1, labels = c('A', 'B'))


#export figure using ggsave
ggsave(here("plots", "new_data_syndrome_08-07_reverse241210.svg"), width = 12, height = 8, dpi = 600)

# Results----------

results <- data_viz %>% select(c(ISO3, income, diff_resp)) %>% distinct()
results %>% summary()
# 56 of 73 countries increased their action (76 %)
results_HIC <- results %>% filter(income== "HIC")
# 26 of 37 HICs increased their action (70%)
results_LMIC <- results %>% filter(income== "LMIC")
# 30 of 36 LMICs increased their action (83%)

