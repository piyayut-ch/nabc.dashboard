################################################################################
# set up -----
pkgs <- c(
  'shiny',
  'glue', 'here',
  'tidyverse', 'reactable',
  'readxl', 'writexl',
  'lubridate',
  'htmltools',
  'sf', 'leaflet'
)
xfun::pkg_attach2(pkgs, message = FALSE)


source(here('utils.R'), encoding = 'UTF-8')
# loadfonts(device = "win", quiet = TRUE)
# Sys.setlocale("LC_CTYPE", "Thai")


################################################################################
# load data and reference table -----

month_th_ref <- read_excel(here("data/month_th_ref.xlsx"))
tha1_ref <- read_excel(here("data/tha1_ref.xlsx"))
month_th_lookup <- set_names(month_th_ref$month_abb_th , month_th_ref$month)
map_tha1 <- readRDS(here("data/tha_adm1_sim.rds"))



################################################################################
# index -----
oae_index_m <- readRDS(here("data/oae_index_m.rds")) %>%
  janitor::clean_names() %>%
  filter(year >= 2009, prod_name != "ข้าวปทุมรวม" & prod_name != "ข้าวขาวรวม" & prod_name != "ข้าวนาปี") %>%
  arrange(-year, -month) %>%
  mutate(
    ci = round(ci, 1),
    ci_price = round(ci_price, 1),
    year_th = year + 543,
    time_label = str_c(month_th_lookup[month], " ", year_th)
  )

oae_index_q <- readRDS(here("data/oae_index_q.rds")) %>%
  janitor::clean_names() %>%
  filter(year >= 2009, prod_name != "ข้าวปทุมรวม" & prod_name != "ข้าวขาวรวม" & prod_name != "ข้าวนาปี") %>%
  arrange(-year, -quarter) %>%
  mutate(
    ci = round(ci, 1),
    ci_price = round(ci_price, 1),
    year_th = year + 543,
    time_label = str_c("Q", quarter, "/", year_th)
  )

oae_index_y <- readRDS(here("data/oae_index_y.rds")) %>%
  janitor::clean_names() %>%
  filter(year >= 2009, prod_name != "ข้าวปทุมรวม" & prod_name != "ข้าวขาวรวม" & prod_name != "ข้าวนาปี") %>%
  arrange(-year) %>%
  mutate(
    ci = round(ci, 1),
    ci_price = round(ci_price, 1),
    year_th = year+543,
    time_label = year_th
  )

prod_names <- oae_index_y %>% filter(!(prod_code %in% c(0, 1e8, 2e8, 3e8))) %>% count(prod_name) %>% pull(prod_name)
province_names <- oae_index_y %>% count(province_nm_th) %>% pull(province_nm_th)
region_names <- oae_index_y %>% count(province_nm_th) %>% pull(province_nm_th)

year_min <- min(oae_index_y$year_th)
year_max <- max(oae_index_y$year_th)



################################################################################
# map -----
oae_fact <- read_rds(here("data/oae_fact.rds"))

oae_fact_y <- oae_fact %>%
  filter(year_th < 2565) %>%
  group_by(year_th, prod_code, prod_name, province_code, province_nm_th) %>%
  summarize(
    production = sum(production),
    price_avg = mean(price_avg, na.rm = TRUE), 
    farm_income = sum(farm_income)
  ) %>% 
  ungroup() %>%
  arrange(-year_th)

oae_fact_y_avg10 <- oae_fact_y %>% 
  group_by(prod_code, prod_name, province_code, province_nm_th) %>% 
  top_n(n = 10) %>%
  summarize(
    production_avg10 = mean(production, na.rm = TRUE),
    price_avg10 = mean(price_avg, na.rm = TRUE),
    farm_income_avg10 = mean(farm_income, na.rm = TRUE)
  )

break_diff <- c(-Inf, -30, -10, 10, 30, Inf)
label_diff <- c("ลดลงมาก", "ลดลงอย่างมีนัยสำคัญ", "ปกติ", "เพิ่มขึ้นอย่างมีนัยสำคัญ", "เพิ่มขึ้นมาก")

oae_fact_y_final <- oae_fact_y %>%
  left_join(oae_fact_y_avg10, by = c("prod_code", "prod_name", "province_code", "province_nm_th")) %>%
  mutate(
    adm1_pcode = str_c("TH", province_code),
    production_diff = (production - production_avg10)*100 / production_avg10,
    price_avg_diff = (price_avg - price_avg10)*100 / price_avg10,
    farm_income_diff = (farm_income - farm_income_avg10)*100 / farm_income_avg10,
    production_diff_cut = cut(production_diff, breaks = break_diff, labels = label_diff),
    price_avg_diff_cut = cut(price_avg_diff, breaks = break_diff, labels = label_diff),
    farm_income_diff_cut = cut(farm_income_diff, breaks = break_diff, labels = label_diff),
  )