

library(tidyverse)
raw_allegation_data <- read_csv("data/allegations_20200726939.csv")

yearly_counts =raw_allegation_data %>% 
  select(year_received, complaint_id) %>% unique() %>%
  group_by(year_received) %>%
  tally() %>%
  rename(num_received = n, year = year_received) %>%
  left_join(raw_allegation_data %>% 
              select(year_closed, complaint_id) %>% unique() %>%
              group_by(year_closed) %>% tally() %>%
              rename(num_closed = n, year = year_closed)
  ) %>%
  gather(timing, num, -year)

ggplot(data = yearly_counts)  +
  geom_line(aes(y = num, x = year, color = timing)) +
  labs(y = "",
       x = "Year",
       color = "Report Timing",
       title = "Total Reports by Year") +
  theme_classic() +
  theme(text = element_text(size = 18))

yearly_officer_counts =raw_allegation_data %>% 
  select(year_received,unique_mos_id) %>% unique() %>%
  group_by(year_received) %>%
  tally() %>%
  rename(num_received = n, year = year_received) %>%
  left_join(raw_allegation_data %>% 
              select(year_closed,unique_mos_id) %>% unique() %>%
              group_by(year_closed) %>%
              tally() %>%
              rename(num_closed = n, year = year_closed)
  ) %>%
  gather(timing, num, -year)
ggplot(data = yearly_officer_counts)  +
  geom_line(aes(y = num, x = year, color = timing)) +
  labs(y = "",
       x = "Year",
       color = "Report Timing",
       title = "Number of Officers Reported by Year")+
  theme_classic() +
  theme(text = element_text(size = 18))

yearly_counts_ethnicity =raw_allegation_data %>% 
  select(year_received, complaint_id, complainant_ethnicity) %>% 
  unique() %>%
  group_by(year_received, complainant_ethnicity) %>%
  tally() %>%
  rename(num_received = n, year = year_received) %>%
  left_join(raw_allegation_data %>% 
              select(year_closed, complaint_id, complainant_ethnicity) %>% unique() %>%
              group_by(year_closed, complainant_ethnicity) %>% tally() %>%
              rename(num_closed = n, year = year_closed)
  ) %>%
  gather(timing, num, -year, -complainant_ethnicity)

ggplot(data = yearly_counts_ethnicity %>% filter(timing == "num_received"))  +
  geom_area(aes(y = num, x = year, fill = complainant_ethnicity)) +
  labs(y = "",
       x = "Year Received",
       color = "Report Timing",
       title = "Number of Reports by Year and Ethnicity")+
  theme_classic() +
  theme(text = element_text(size = 18))

officer_counts = raw_allegation_data %>% 
  select(year_received, first_name, last_name,
         unique_mos_id, complaint_id) %>%
  filter(year_received > 2009) %>%
  unique() %>%
  group_by(unique_mos_id, first_name, last_name) %>% 
  summarize(count = n()) 
  
ggplot(data = officer_counts)  +
  geom_histogram(aes(x = count), binwidth = 1) +
  labs(y = "",
       x = "Numbers of Allegations from 2010-2020",
       title = "Distribution of Complaints across Police from 2010-2020",
       subtitle = "36k Uniformed in NYPD as of 2020; 3,340 Officers with Complaints") +
  theme_classic() +
  theme(text = element_text(size = 18))
  

ggplot(data = officer_counts)  +
  geom_histogram(aes(x = count), binwidth = 1) +
  geom_vline(xintercept = 4.5) +
  annotate("text", x = 5, y = 600, 
           label = "518 NYPD with 5 or more Complaints from 2010-2020",
           hjust = "left")+
  labs(y = "",
       x = "Numbers of Allegations from 2010-2020",
       title = "Distribution of Complaints across Police from 2010-2020",
       subtitle = "36k Uniformed in NYPD as of 2020; 3,340 Officers with Complaints") +
  theme_classic() +
  theme(text = element_text(size = 18))


officer_counts_race = raw_allegation_data %>% 
  select(year_received, first_name, last_name,
         unique_mos_id, complaint_id, mos_ethnicity) %>%
  filter(year_received > 2009) %>%
  unique() %>%
  mutate(white_officer = mos_ethnicity %in% c("White")) %>%
  group_by(unique_mos_id, white_officer, first_name, last_name) %>% 
  summarize(count = n()) 

officer_counts_race$white_officer = factor(officer_counts_race$white_officer, 
                                           levels = c(FALSE, TRUE),
                                           labels = c("Non-White Officer", "White Officer"))

officer_counts_race %>% group_by(white_officer) %>% filter(count > 4) %>%
  summarise(n())
ggplot(data = officer_counts_race)  +
  geom_histogram(aes(x = count), binwidth = 1) +
  labs(y = "",
       x = "Numbers of Allegations from 2010-2020",
       title = "Distribution of Complaints from 2010-2020",
       subtitle = "36k Uniformed in NYPD as of 2020, 47.1% are White") +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  facet_wrap(~white_officer)

