# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-09-24"), 
                                    author = "corb", 
                                    proj_name = "dewclc", 
                                    script_type = "eda", 
                                    notepad = paste0("basic eda of loss costs")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(gt)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# data loaders -----------------------------------------------------

# load a rds file
loader_path1 <- paste0(getwd(), "/etl/ingot", "/dataframe.rds")
clockin()
xx <- readRDS(loader_path1)
clockout()
dim(xx)
head(xx)

df <- xx %>% select(-metadata_tag)

# ^ -----

# visualize 1 -----------------------------------------------

# trend line of loss costs (mean/median) over time
df %>% filter(loss_cost < 20, lc_eff_dt > as.Date('2016-01-01'), 
              hazard_group != 0, hazard_group != "F") %>% 
  group_by(lc_eff_yr) %>% 
  summarise(lc_mean = mean(loss_cost), 
            lc_med = median(loss_cost)) %>% 
  ggplot(aes(x = lc_eff_yr, y = lc_med)) + 
  geom_line(size = 1) + 
  geom_label(aes(label = lc_med)) + 
  theme_minimal() + theme(legend.position = "top") + 
  labs(y = "Median Loss Cost", x = "Loss Cost Effective Year",  
       subtitle = "Class-Level Median Loss Cost Over Time", 
       caption = "Excludes F Haz Group and classes with LC over $10")

# box plot of class level loss costs by year
df %>% filter(loss_cost < 10, lc_eff_dt > as.Date('2016-01-01'), 
              hazard_group != 0, hazard_group != "F") %>% 
  mutate(lc_eff_yr = as.factor(lc_eff_yr)) %>% 
  ggplot(aes(x = lc_eff_yr, y = loss_cost, group = lc_eff_yr, 
             color = lc_eff_yr)) + 
  geom_boxplot(alpha = 0, size = 1) + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(x = "Loss Cost Eff Year", y = "Loss Cost", 
       subtitle = "Loss Cost By Class Distribution Over Time", 
       caption = "Excludes F Haz Group and classes with LC over $10")

# ^ -----

# visualize 2 -----------------------------------------------

# trend line of loss costs (mean/median) over time
df %>% filter(loss_cost < 20, lc_eff_dt > as.Date('2016-01-01'), 
              hazard_group != 0, hazard_group != "F") %>% 
  group_by(lc_eff_yr, hazard_group) %>% 
  summarise(lc_mean = mean(loss_cost), 
            lc_med = median(loss_cost)) %>% 
  ggplot(aes(x = lc_eff_yr, y = lc_med, color = hazard_group)) + 
  geom_line(size = 1) + 
  geom_label(aes(label = lc_med), size = 3) + 
  theme_minimal() + theme(legend.position = "none") + 
  facet_wrap(vars(hazard_group), scales = "free_y") + 
  labs(y = "Median Loss Cost", x = "Loss Cost Effective Year",  
       subtitle = "Class-Level Median Loss Cost Over Time", 
       caption = "Excludes F Haz Group and classes with LC over $10")

# box plot of class level loss costs by year
df %>% filter(loss_cost < 20, lc_eff_dt > as.Date('2016-01-01'), 
              hazard_group != 0, hazard_group != "F") %>% 
  mutate(lc_eff_yr = as.factor(lc_eff_yr)) %>% 
  ggplot(aes(x = lc_eff_yr, y = loss_cost, group = lc_eff_yr, 
             color = hazard_group)) + 
  geom_boxplot(alpha = 0, size = 1) + 
  theme_minimal() + theme(legend.position = "none") + 
  facet_wrap(vars(hazard_group), scales = "free_y") + 
  labs(x = "Loss Cost Eff Year", y = "Loss Cost", 
       subtitle = "Loss Cost By Class Distribution Over Time", 
       caption = "Excludes F Haz Group and classes with LC over $10")

# ^ -----

# visualize 3 ------------------------------------------------

fun_delta_type <- function(vvv) {
  return_me <- case_when(is.na(vvv) ~ 'ERROR', 
                         vvv == 0 ~ 'NO CHANGE', 
                         vvv > 0.2 ~ '+20%', 
                         vvv > 0.1 ~ '10-20%', 
                         vvv > 0.05 ~ '5-10%', 
                         vvv > 0 ~ '0-5%',
                         vvv < -0.2 ~ '-20%', 
                         vvv < -0.1 ~ '-10-20%', 
                         vvv < -0.05 ~ '-10-20%', 
                         vvv < 0 ~ '-0-5%', 
                         TRUE ~ 'ERROR')
  return(return_me)}

fun_code_type <- function(yr1, yr2) {
  return_me <- case_when(is.na(yr1) & !is.na(yr2) ~ 'NEW CODE', 
                         !is.na(yr1) & is.na(yr2) ~ 'RETIRED CODE', 
                         is.na(yr1) & is.na(yr2) ~ 'ANCIENT CODE', 
                         !is.na(yr1) & !is.na(yr2) ~ 'LIVE CODE', 
                         TRUE ~ 'ERROR')
  return(return_me)}

fun_wider_tbl <- function(df_func) {
  return_me <- df_func %>% 
    mutate(lc_eff_yr = as.factor(lc_eff_yr)) %>% 
    group_by(lc_eff_yr, hazard_group, class_code_fct) %>% 
    summarise(loss_cost = min(loss_cost)) %>% 
    pivot_wider(names_from = lc_eff_yr, values_from = loss_cost, 
                names_prefix = 'yr_') %>% 
    mutate(delta_18_19 = yr_2019 / yr_2018 - 1, 
           delta_19_20 = yr_2020 / yr_2019 - 1, 
           delta_20_21 = yr_2021 / yr_2020 - 1) %>% 
    mutate(type_20_21 = fun_delta_type(delta_20_21), 
           class_code_type = fun_code_type(yr_2020, yr_2021))
  return(return_me)}


df %>% filter(loss_cost < 20, lc_eff_dt > as.Date('2016-01-01'), 
              hazard_group != 0, hazard_group != "F") %>% 
  fun_wider_tbl() %>% 
  View()
  mutate(yr_2020 = ifelse(is.na(yr_2020), 0, yr_2020), 
         yr_2021 = ifelse(is.na(yr_2021), 0, yr_2021)) %>% 
  ggplot() + 
  geom_point(aes(x = yr_2021, y = yr_2020, 
                 color = hazard_group), 
             alpha = 0.5) + 
  theme_minimal() + theme(legend.position = 'none') + 
  facet_wrap(vars(hazard_group), scales = 'free')

df %>% filter(loss_cost < 20, lc_eff_dt > as.Date('2016-01-01'), 
              hazard_group != 0, hazard_group != "F") %>% 
  fun_wider_tbl() %>% 
  ggplot() + 
  geom_point(aes(x = yr_2021, y = delta_20_21, 
                 color = hazard_group), 
             alpha = 0.5) + 
  geom_hline(aes(yintercept = median(delta_20_21, na.rm = TRUE)), 
             linetype = 2) + 
  theme_minimal() + theme(legend.position = 'none') + 
  facet_wrap(vars(hazard_group))


# ^ -----