



library(tidyverse)
library(here)
library(janitor)



lobsters <- read_csv(here("data", "Lobster_Abundance_All_Years_20210412.csv"), na = c("-99999", "")) %>%
  clean_names() %>%
  uncount(count)

# this code reads the csv and stores it under 'lobsters'. *na = c("=99999", "")* removes the placeholder for NA, which in this dataset is -99999. Clean_names puts the column names into lower snake case.

# now creating a summary table to include year, site, total count, and the mean for each site. 

lobsters_tidy <- lobsters %>%
  group_by(year, site) %>%
  summarize(mean= mean(size_mm, na.rm=TRUE),
            count=n())

# and now plotting that data, wow look at that graph. 

ggplot(data=lobsters_tidy, aes(x=year, y=count)) + geom_line(aes(color=site))

#####

# now creating a table showing the counts of lobsters over and under the legal limit for each site. 

legal_limit = 79.76
lobsters_2020 <- lobsters %>%
  filter(year==2020) %>%
  group_by(site) %>%
  mutate(legal=dplyr::case_when(size_mm>legal_limit ~ "legal", size_mm < legal_limit ~ "illegal!")) %>%
  group_by(site, legal) %>%
  summarize(count=n())

# now creating a plot that shows the proportion of legal to illegal lobsters

ggplot(lobsters_2020, aes(site, count)) + geom_col(position="fill", color=legal_limit)

##### 

# moving on! Task 3. 
# filter() practice

# creating a subset with only sites IVEE, CARP, NAPL.

ex_a <- lobsters %>%
  filter(site == c("IVEE", "CARP", "NAPL"))

# creating a subset with only lobsters observed in August. 

ex_b <- lobsters %>%
  filter(month == 8)

# august lobsters only!
# creating a subset of only lobsters from AQUE or ANY lobster over 70mm

ex_c <- lobsters %>%
  filter(size_mm>70) %>%
  filter(site == "AQUE")

# creating a subset that doesn't include NAPL

ex_d <- lobsters %>%
  filter(!site == "NAPL")

# group_by() and summarize() practice

ex_e <- lobsters %>%
  group_by(site) %>%
  summarize(mean = mean(size_mm, na.rm = TRUE), sd(size_mm, na.rm=TRUE))

ex_f <- lobsters %>%
  group_by(month) %>%
  summarize(max_length = max(size_mm, na.rm=TRUE))
 
# please note there is a 600mm lobster loose in this data set. beware!

ex_g <- lobsters %>%
  mutate("size (cm)" = size_mm / 10)
  
# added size in cm.

ex_h <- lobsters %>%
  mutate(site = tolower(site))

# now all site names are lower case

ex_i <- lobsters %>%
  mutate(area=as.character(area))

ex_j <- lobsters %>%
  mutate(case_when("size_bin" = size_mm<= 70 ~ "small", size_mm>70 ~ "large"))

ex_k <- lobsters %>%
  mutate(designation=case_when(site %in% c("IVEE", "NAPL") ~ "MPA", 
                               !(site %in% c("IVEE", "NAPL")) ~ "not MPA"))








