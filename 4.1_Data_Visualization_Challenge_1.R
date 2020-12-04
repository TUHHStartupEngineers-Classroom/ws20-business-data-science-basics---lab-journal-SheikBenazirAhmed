library(tidyverse)
library(ggthemes)
library(ggplot2)
library(scales)
library(forcats)
library(ggrepel)



covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")


covid_data_tbl_compressed <- covid_data_tbl %>%
  
  filter((countriesAndTerritories == "Europe" |
            countriesAndTerritories == "Germany" | 
            countriesAndTerritories == "United_Kingdom" | 
            countriesAndTerritories == "France" | 
            countriesAndTerritories == "Spain" | 
            countriesAndTerritories == "United_States_of_America") & year == "2020")%>%
  group_by(countriesAndTerritories, month, year) %>%
  summarise(total_case = sum(cases)) %>%
  ungroup()%>%
  mutate(month=lubridate::month(month,label = TRUE,abbr = TRUE))

covid_data_cumulative_case_tbl <- covid_data_tbl_compressed %>% mutate(cumulative_case=cumsum(total_case))

covid_data_cumulative_case_tbl %>%
  
  ggplot(aes(month, cumulative_case, group=countriesAndTerritories, color = countriesAndTerritories)) +
  
  geom_line(size=.5) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €"))+
  theme_minimal() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45)) 