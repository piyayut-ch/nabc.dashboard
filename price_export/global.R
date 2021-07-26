################################################################################
# set up -----
pkgs <- c(
  'shiny',
  'glue', 'here',
  'tidyverse', 'DT',
  'lubridate',
  'reactable', 'echarts4r',
  'extrafont', 'hrbrthemes', 'ggsci', 'scales'
)
xfun::pkg_attach2(pkgs, message = FALSE)



source(here('utils.R'), encoding = 'UTF-8')
# loadfonts(device = "win", quiet = TRUE)
# Sys.setlocale("LC_CTYPE", "Thai")

################################################################################
# load data and reference table -----
price_oae_m <- readRDS(here("data/price_oae_m_TH00.rds"))
ref_price_oae <- readRDS(here('data/ref_price_oae.rds'))

trade_m <- readRDS(here("data/trade_m.rds"))
ref_trade <- readRDS(here('data/ref_trade.rds'))


################################################################################
# tidy data -----
data_price_table <- price_oae_m %>% 
  mutate(
    period = case_when(
      year >= 2558 & year <= 2562 ~ 'last5y',
      date >= '2020-03-01' & date <= '2020-04-01' ~ 'wave1',
      date >= '2021-01-01' & date <= '2021-02-01' ~ 'wave2',
      date >= '2021-04-01' & date <= '2021-06-01' ~ 'wave3'
    )
  ) %>% 
  drop_na(period) %>%
  group_by(price_name, commod, unit, period) %>%
  summarize(value = mean(value)) %>% ungroup() %>%
  mutate(value = ifelse(value > 1000, round(value, 0), round(value, 2))) %>%
  pivot_wider(1:4, names_from = 'period', values_from = 'value') %>%
  mutate(
    wave1_pc = percent_change(wave1, last5y),
    wave2_pc = percent_change(wave2, last5y),
    wave3_pc = percent_change(wave3, last5y)
  )


data_price_graph <- price_oae_m %>%
  dplyr::filter(date >= '2015-01-01')


trade_m <- trade_m %>%
  mutate(
    value = case_when(
      unit == "KGM" ~ value/1e3,
      unit == "BAHT" ~ value/1e6,
      unit == "LTR" ~ value/1e6,
      TRUE ~ value
    ),
    unit = case_when(
      unit == "KGM" ~ 'ตัน',
      unit == "BAHT" ~ 'ล้านบาท',
      unit == "LTR" ~ 'ล้านลิตร',
      TRUE ~ unit
    ),
    value = ifelse(value >= 1000, round(value, 0), round(value, 2))
  )


data_trade_table <- trade_m %>% 
  mutate(
    period = case_when(
      date >= '2015-01-01' & date <= '2019-12-31' ~ 'last5y',
      date >= '2020-03-01' & date <= '2020-04-01' ~ 'wave1',
      date >= '2021-01-01' & date <= '2021-02-01' ~ 'wave2',
      date >= '2021-04-01' & date <= '2021-06-01' ~ 'wave3'
    )
  ) %>% 
  drop_na(period) %>%
  group_by(product_name, subproduct_name, impexp, variable, unit, period) %>%
  summarize(value = mean(value)) %>% ungroup() %>%
  mutate(value = ifelse(value >= 1000, round(value, 0), round(value, 2))) %>%
  pivot_wider(1:5, names_from = 'period', values_from = 'value') %>%
  mutate(
    wave1_pc = percent_change(wave1, last5y),
    wave2_pc = percent_change(wave2, last5y),
    wave3_pc = percent_change(wave3, last5y)
  )


data_trade_graph <- trade_m %>%
  dplyr::filter(date >= '2015-01-01')

# shiny::runApp()