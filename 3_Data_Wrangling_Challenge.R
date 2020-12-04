library(tidyverse)
library(vroom)
library(magrittr)
library(lubridate)
library(data.table)



# DATA IMPORT ----
patent <-  "C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/02_data_wrangling/patent.tsv"
patent_tbl <- fread(patent)
setnames(patent_tbl, "id", "patent_id")

assignee  <-   "C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/02_data_wrangling/assignee.tsv"
assignee_tbl <- fread(assignee)
setnames(assignee_tbl, "id", "assignee_id")

patent_assignee <- "C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/02_data_wrangling/patent_assignee.tsv"
patent_assignee_tbl<- fread(patent_assignee)

uspc <- "C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/02_data_wrangling/uspc.tsv"
uspc_tbl<- fread(uspc)



# 1. Patent Dominance
assignee_patentAssignee_merged <- merge(assignee_tbl, patent_assignee_tbl, by='assignee_id')
na.omit(assignee_patentAssignee_merged, cols="organization")

# US company with most patents
assignee_patentAssignee_merged [, .N, by = organization][order(-N)] %>% head(1)%>%na.omit()

# 10 US companies with most assigned/granted patents
assignee_patentAssignee_merged [, .N, by = organization][order(-N)]%>%na.omit() %>% head(10)




#2. Recent patent activity
assignee_patentAssignee_patent_merged <- merge(assignee_patentAssignee_merged, patent_tbl, by='patent_id') 
assignee_patentAssignee_patent_merged_view <- assignee_patentAssignee_patent_merged[1:2]

# US company with most patents granted in 2019
assignee_patentAssignee_patent_merged [lubridate::year(date) == 2019, .N, by = organization][order(-N)]%>%na.omit() %>% head(1)

# 10 companies with most new granted patents in 2019
assignee_patentAssignee_patent_merged [lubridate::year(date) == 2019 & kind=="B1", .N, by = organization][order(-N)]%>%na.omit() %>% head(10)




# 3. Innovation in Tech
assignee_patentAssignee_uspc_merged <- merge(assignee_patentAssignee_merged, uspc_tbl, by='patent_id')
assignee_patentAssignee_uspc_merged_view <- assignee_patentAssignee_uspc_merged[1:2]

# Most innovative tech sector
patent_tbl[, .N, by = type][order(-N)] %>% head(1)

# Top 5 USPTO main classes of their patents
assignee_patentAssignee_uspc_merged[organization=="International Business Machines Corporation", .N, by = mainclass_id][order(-N)]%>% head(5)
