## Chethan Ramprasad, Vincent Major
## Created:   July 7, 2018
## Modified:  January 7, 2019

## load libraries
if(!require(RNHANES)){install.packages("RNHANES")};library(RNHANES)
if(!require(dplyr)){install.packages("dplyr")};library(dplyr)
if(!require(stringr)){install.packages("stringr")};library(stringr)
if(!require(ggplot2)){install.packages("ggplot2")};library(ggplot2)
dir.create("data", showWarnings = FALSE)

#### loading data ----
## This requires the RNHANES package

## all files:
nhanes_files <- nhanes_data_files(components = "all", destination = "data",
                                  cache = TRUE)


## sexual health questionnaire --- SXQ
sxq_files <- nhanes_files %>% 
  filter(str_detect(data_file_name, "SXQ"),
         cycle %in% c("2009-2010", "2011-2012", "2013-2014") ) ## 3
sxq_data <- bind_rows(lapply(sxq_files$cycle, function(cycle) {
  ## check data destination exists  ## if NOT exists, create it
  if( !dir.exists("data") ){dir.create("data") }
  
  df <- nhanes_load_data("sxq", cycle, destination = "data")
  ## add on a cycle column and move it to the far left for viewing.
  df.out <- df %>% mutate(cycle = cycle) %>% select(cycle, everything() )
  return(df.out)
}) )


## demographics questionnaire  --- DEMO
DEMO_files <- nhanes_files %>% 
  filter(str_detect(data_file_name, "DEMO"),
         cycle %in% c("2009-2010", "2011-2012", "2013-2014") ) ## 3
DEMO_data <- bind_rows(lapply(DEMO_files$cycle, function(cycle) {
  ## check data destination exists
  if( !dir.exists("data") ){dir.create("data") } ## if NOT exists, create it
  
  df <- nhanes_load_data("DEMO", cycle, destination = "data")
  ## add on a cycle column and move it to the far left for viewing.
  df.out <- df %>% mutate(cycle = cycle) %>% select(cycle, everything() )
  return(df.out)
}) )


## Medical conditions questionnaire --- MCQ
DIAG_files <- nhanes_files %>% 
  filter(str_detect(data_file_name, "MCQ"),
         cycle %in% c("2009-2010", "2011-2012", "2013-2014") ) ## 3
DIAG_data <- bind_rows(lapply(DIAG_files$cycle, function(cycle) {
  ## check data destination exists
  if( !dir.exists("data") ){dir.create("data") } ## if NOT exists, create it
  
  df <- nhanes_load_data("MCQ", cycle, destination = "data")
  ## add on a cycle column and move it to the far left for viewing.
  df.out <- df %>% mutate(cycle = cycle) %>% select(cycle, everything() )
  return(df.out)
}) )

#### combining each questionnaire ----

## first, extract women and men who admit history of anal sex

## different questions for men/women, compute separately
## WOMEN ANAL YES and NO
# SXQ706 = Ever had anal sex with a man
# 1 --> Yes
# 2 --> No
# 7, 9, . refused, don't know, missing
WOMENANAL <- sxq_data %>% 
  filter(SXQ706 %in% c(1, 2)) %>% ## answered the question
  mutate(flag.analsex = as.integer(SXQ706 == 1) )
WOMENANAL %>% count(flag.analsex) 
## 2203 flag.analsex == 1 from 6596 total answered!

## MEN ANAL YES and NO
MENANAL <- sxq_data %>% 
  filter(!is.na(SXQ836) ) %>% ## answered the question
  mutate(flag.analsex = as.integer(SXQ836 >= 1) )
MENANAL %>% count(flag.analsex) 
## 235 from total 323 answered!
## MEN question is not specfic to receptive, cannot continue with risk analysis.


## second, identify participants that report history of colon or rectal cancer (coded separately in NHANES)
CRC_reported <- DIAG_data %>% 
  select(cycle, SEQN, MCQ220, MCQ230A, MCQ230B, MCQ230C, MCQ230D) %>% 
  #filter(MCQ220 == 1) %>% ## some cancer
  mutate(CRC_report = case_when(
    ## cancer key:
    ## 16 --> colon
    ## 31 --> rectum
    ## A = primary, B = secondary, etc.
    MCQ230A %in% c(16, 31) ~ 'CRC_1',
    MCQ230B %in% c(16, 31) ~ 'CRC_2',
    MCQ230C %in% c(16, 31) ~ 'CRC_3',
    MCQ230D %in% c(16, 31) ~ 'CRC_4',
    T ~ NA_character_
  ),
  flag.crc = as.integer(!is.na(CRC_report)) ) %>% 
  filter(flag.crc == 1)
CRC_reported %>% count(flag.crc)
## 127 CRC cases from 29243


## third, identifying HPV matching
#SXQ753 = HPV
HPV_reported <- sxq_data %>% 
  ## SXQ753, every been told you have HPV
  ## yes --> 1
  mutate(flag.hpv = as.integer(SXQ753 == 1) ) %>% 
  filter(flag.hpv == 1)
HPV_reported %>% count(flag.hpv) 
## 432 cases (all women by survey design)


## combining tables together!
anal.cohort <- bind_rows(WOMENANAL, MENANAL) %>% 
  select(SEQN, cycle, flag.analsex) %>% 
  ## only those who answered the SXQ question
  left_join(CRC_reported %>%
              select(SEQN, flag.crc),
            by = 'SEQN') %>%
  left_join(HPV_reported %>%
              select(SEQN, flag.hpv),
            by = 'SEQN') %>%
  ## fill unanswered/unknown CRC and HPV status with 0 = False
  tidyr::replace_na(list(flag.crc = 0, flag.hpv = 0)) %>% 
  ## join in demographics
  left_join(DEMO_data %>% 
              select(SEQN, 
                     RIAGENDR, 
                     RIDRETH1, ## opting to use RIDETH1 because RIDETH3 was not included in 2009-2010 cycle
                     RIDAGEYR), 
            by = 'SEQN') 

anal.cohort %>% count(flag.analsex)
anal.cohort %>% count(flag.crc)
anal.cohort %>% filter(RIAGENDR == 2) %>% count(flag.crc)
anal.cohort %>% filter(RIAGENDR == 2) %>% count(flag.analsex, flag.crc)

### WOMEN ONLY
anal.women <- anal.cohort %>% 
  filter(RIAGENDR == 2) ## women

anal.women %>% count(RIAGENDR) ## 7781 women
anal.women %>% count(cycle) ## 2721, 2420, 2640 
anal.women %>% count(RIAGENDR, flag.analsex) ## 2203 reporting receptive anal sex

anal.women %>% 
  count(cycle, RIAGENDR, flag.analsex) %>% 
  group_by(cycle, RIAGENDR) %>% mutate(tot = sum(n), prop = n/tot) %>% 
  filter(flag.analsex == 1)
## 789, 632, 782
## 29.0, 26.1, 29.6% 

## proportion and Chi-sq tests for women anal sex vs colorectal cancer
anal.women %>% count(flag.analsex, flag.crc)
# # A tibble: 4 x 3
#   flag.analsex flag.crc     n
#           <int>    <dbl> <int>
# 1            0        0  4379
# 2            0        1    14
# 3            1        0  2198
# 4            1        1     5
m <- matrix(c(5, 14, 2198, 4379), nrow = 2)
prop.table(m, margin = 1)
prop.test(m)
chisq.test(m)
# X-squared = 0.16976, df = 1, p-value = 0.6803


#### pretty graphs ---
## plotting admission of anal sex, race
dict.eth1 <- tribble(
  ~value, ~Ethnicity,
  1, "Mexican American",
  2, "Other Hispanic",
  3, "Non-Hispanic White",
  4, "Non-Hispanic Black",
  5, "Other Race - Including Multi-Racial"
)

anal.women %>%
  left_join(dict.eth1, by = c('RIDRETH1' = 'value') ) %>% 
  mutate(`Receptive Anal Intercourse` = (flag.analsex == 1) ) %>% ## 0/1 into FALSE/TRUE
  ## plot
  ggplot(aes(cycle, fill = Ethnicity)) +
  geom_bar(position = 'dodge') + ## side by side
  ylab("Number of Women") + xlab("Survey Cycle") + 
  ## split anal sex into two panes/facets
  facet_grid(. ~ `Receptive Anal Intercourse`, scales = 'free', labeller = label_both) + 
  ## move the legend to the bottom and give it 2 columns
  theme(legend.position = "bottom") + 
  guides(fill=guide_legend(ncol=2)) + 
  ## save it
  ggsave("figures/20190108_ethnicity_bar_plot.png", width = 6, height = 4)


## age barplot
anal.women %>%
  mutate(`Receptive Anal Intercourse` = (flag.analsex == 1) ) %>%  ## 0/1 into FALSE/TRUE
  ## plot
  ggplot(aes(cycle, RIDAGEYR, fill = `Receptive Anal Intercourse`)) +
  geom_boxplot() + 
  ylab("Age (years)") + xlab("Survey Cycle") + 
  ## move the legend to the bottom and make it horizontal
  theme(legend.direction = "horizontal") +
  theme(legend.position = "bottom") + 
  ## save it
  ggsave("figures/20190108_age_boxplot.png", width = 6, height = 4)


anal.women %>% 
  count(cycle, RIAGENDR, flag.analsex) %>% 
  group_by(cycle, RIAGENDR) %>% mutate(tot = sum(n), prop = n/tot) %>% 
  filter(flag.analsex == 1) %>% ## only anal sex bars
  mutate(`Receptive Anal Intercourse` = (flag.analsex == 1),
         Gender = c("Male", "Female")[RIAGENDR]) %>% 
  ggplot(aes(cycle, prop, fill = Gender)) + ##no need for color
  geom_bar(stat = 'identity', position = 'dodge') + 
  ylab("Proportion of Women Reporting Receptive Anal Intercourse") + xlab("Survey Cycle") + 
  ## remove the legend
  guides(fill=FALSE) + 
  ## extend y axis to show propirtion 0, 1
  ylim(0, 1) + 
  ## save it
  ggsave("figures/20190108_anal_proportion.png", width = 5, height = 5)

anal.women %>% 
  count(cycle, RIAGENDR, flag.analsex) %>% 
  group_by(cycle, RIAGENDR) %>% mutate(tot = sum(n), prop = n/tot) %>% 
  #filter(flag.analsex == 1) %>% ## only anal sex bars
  mutate(`Receptive Anal Intercourse` = (flag.analsex == 1),
         Gender = c("Male", "Female")[RIAGENDR]) %>% 
  ggplot(aes(cycle, prop, fill = `Receptive Anal Intercourse`)) + ##no need for color
  geom_bar(stat = 'identity', position = 'stack') + 
  ylab("Proportion of Women Reporting Receptive Anal Intercourse") + xlab("Survey Cycle") + 
  ## move the legend to the bottom and make it horizontal
  theme(legend.direction = "horizontal") +
  theme(legend.position = "bottom") + 
  ## extend y axis to show propirtion 0, 1
  ylim(0, 1) + 
  ## save it
  ggsave("figures/20190108_anal_proportion_stacked.png", width = 5, height = 5)


## demographics table Jan 8, 2019.

## mean age
anal.women %>% summarize(mean_age = mean(RIDAGEYR))
## 42.40343

anal.women %>% summarize(median_age = median(RIDAGEYR),
                         lower = quantile(RIDAGEYR, 0.25),
                         upper = quantile(RIDAGEYR, 0.75))
#   median_age lower upper
# 1         43    29    55


## age by decades
anal.women %>% 
  mutate(age_bin = floor(RIDAGEYR/10)*10 ) %>%  ## rounding down into decades, 20-29 --> 20
  count(age_bin) %>% 
  mutate(prop = n/sum(n)*100)
# # A tibble: 6 x 3
#   age_bin     n  prop
#     <dbl> <int> <dbl>
# 1      10   421  6.38
# 2      20  1278 19.4 
# 3      30  1189 18.0 
# 4      40  1304 19.8 
# 5      50  1193 18.1 
# 6      60  1211 18.4 

## Ethnicity counts
anal.women %>%
  left_join(dict.eth1, by = c('RIDRETH1' = 'value') ) %>% 
  count(Ethnicity) %>% 
  mutate(prop = n/sum(n)*100)
# # A tibble: 5 x 3
#   Ethnicity                               n  prop
#   <chr>                               <int> <dbl>
# 1 Mexican American                     1005  15.2
# 2 Non-Hispanic Black                   1490  22.6
# 3 Non-Hispanic White                   2615  39.6
# 4 Other Hispanic                        733  11.1
# 5 Other Race - Including Multi-Racial   753  11.4
# 5 Other Race - Including Multi-Racial   753

## anal sex proportions in in age decades
anal.women %>% 
  mutate(age_bin = floor(RIDAGEYR/10)*10 ) %>% ## rounding down into decades, 20-29 --> 20
  count(flag.analsex, age_bin) %>% 
  group_by(age_bin) %>% 
  mutate(prop = n/sum(n)*100)
# # A tibble: 12 x 4
# # Groups:   age_bin [6]
#   flag.analsex age_bin     n  prop
#          <int>   <dbl> <int> <dbl>
# 1            0      10   343  81.5
# 2            0      20   790  61.8
# 3            0      30   677  56.9
# 4            0      40   826  63.3
# 5            0      50   827  69.3
# 6            0      60   930  76.8
# 7            1      10    78  18.5
# 8            1      20   488  38.2
# 9            1      30   512  43.1
# 10           1      40   478  36.7
# 11           1      50   366  30.7
# 12           1      60   281  23.2

## anal sex proportions in ethnicity
anal.women %>%
  left_join(dict.eth1, by = c('RIDRETH1' = 'value') ) %>% 
  count(flag.analsex, Ethnicity) %>% 
  group_by(Ethnicity) %>% 
  mutate(prop = n/sum(n)*100)
# # A tibble: 10 x 4
# # Groups:   Ethnicity [5]
#   flag.analsex Ethnicity                               n  prop
#          <int> <chr>                               <int> <dbl>
# 1            0 Mexican American                      696  69.3
# 2            0 Non-Hispanic Black                   1096  73.6
# 3            0 Non-Hispanic White                   1550  59.3
# 4            0 Other Hispanic                        484  66.0
# 5            0 Other Race - Including Multi-Racial   567  75.3
# 6            1 Mexican American                      309  30.7
# 7            1 Non-Hispanic Black                    394  26.4
# 8            1 Non-Hispanic White                   1065  40.7
# 9            1 Other Hispanic                        249  34.0
# 10           1 Other Race - Including Multi-Racial   186  24.7
