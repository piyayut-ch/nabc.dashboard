################################################################################
# set up -----
pkgs <- c(
  'shiny',
  'glue', 'here',
  'tidyverse', 'DT',
  'lubridate',
  'reactable', 'echarts4r', 'htmltools',
  'extrafont', 'hrbrthemes', 'ggsci', 'scales'
)
xfun::pkg_attach2(pkgs, message = FALSE)


source(here('utils.R'), encoding = 'UTF-8')
loadfonts(device = "win", quiet = TRUE)
Sys.setlocale("LC_CTYPE", "Thai")

################################################################################
# load data and reference table -----
oae_index_m <- readRDS(here("data/oae_index_m.rds"))
oae_index_q <- readRDS(here("data/oae_index_q.rds"))
oae_index_y <- readRDS(here("data/oae_index_y.rds"))

data_fmt <- oae_index_y %>%
  count(PROD_NAME) %>%
  mutate(
    color = "black",
    bgcolor = case_when(
      PROD_NAME %in% c("สินค้าเกษตร") ~ "lightskyblue",
      PROD_NAME %in% c("พืชผลสำคัญ", "ปศุสัตว์", "ประมง") ~ "azure",
      TRUE ~ "white"
    )
  )

province_names <- oae_index_y %>% count(PROVINCE_NM_TH) %>% pull(PROVINCE_NM_TH)

year_min <- min(oae_index_y$YEAR)
year_max <- max(oae_index_y$YEAR)


# ################################################################################
# # tidy data -----
# data_price_table <- price_oae_m %>% 
#   mutate(
#     period = case_when(
#       year >= 2558 & year <= 2562 ~ 'last5y',
#       date >= '2020-03-01' & date <= '2020-04-01' ~ 'wave1',
#       date >= '2021-01-01' & date <= '2021-02-01' ~ 'wave2',
#       date >= '2021-04-01' & date <= '2021-06-01' ~ 'wave3'
#     )
#   ) %>% 
#   drop_na(period) %>%
#   group_by(price_name, commod, unit, period) %>%
#   summarize(value = mean(value)) %>% ungroup() %>%
#   mutate(value = ifelse(value > 1000, round(value, 0), round(value, 2))) %>%
#   pivot_wider(1:4, names_from = 'period', values_from = 'value') %>%
#   mutate(
#     wave1_pc = percent_change(wave1, last5y),
#     wave2_pc = percent_change(wave2, last5y),
#     wave3_pc = percent_change(wave3, last5y)
#   )


# data_price_graph <- price_oae_m %>%
#   dplyr::filter(date >= '2015-01-01')


# trade_m <- trade_m %>%
#   mutate(
#     value = case_when(
#       unit == "KGM" ~ value/1e3,
#       unit == "BAHT" ~ value/1e6,
#       unit == "LTR" ~ value/1e6,
#       TRUE ~ value
#     ),
#     unit = case_when(
#       unit == "KGM" ~ 'ตัน',
#       unit == "BAHT" ~ 'ล้านบาท',
#       unit == "LTR" ~ 'ล้านลิตร',
#       TRUE ~ unit
#     ),
#     value = ifelse(value >= 1000, round(value, 0), round(value, 2))
#   )


# data_trade_table <- trade_m %>% 
#   mutate(
#     period = case_when(
#       date >= '2015-01-01' & date <= '2019-12-31' ~ 'last5y',
#       date >= '2020-03-01' & date <= '2020-04-01' ~ 'wave1',
#       date >= '2021-01-01' & date <= '2021-02-01' ~ 'wave2',
#       date >= '2021-04-01' & date <= '2021-06-01' ~ 'wave3'
#     )
#   ) %>% 
#   drop_na(period) %>%
#   group_by(product_name, subproduct_name, impexp, variable, unit, period) %>%
#   summarize(value = mean(value)) %>% ungroup() %>%
#   mutate(value = ifelse(value >= 1000, round(value, 0), round(value, 2))) %>%
#   pivot_wider(1:5, names_from = 'period', values_from = 'value') %>%
#   mutate(
#     wave1_pc = percent_change(wave1, last5y),
#     wave2_pc = percent_change(wave2, last5y),
#     wave3_pc = percent_change(wave3, last5y)
#   )


# data_trade_graph <- trade_m %>%
#   dplyr::filter(date >= '2015-01-01')

# shiny::runApp()