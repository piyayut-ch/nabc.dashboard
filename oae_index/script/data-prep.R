###############################################################################
# STEP 0: SETUP -----
pkgs <- c(
  'fs', 'glue', 'here',
  'tidyverse', 'janitor',
  'DBI', 'vroom', 'readxl', 'writexl',
  'lubridate', 'tsibble', 'tsbox', 'timetk', 'xts',  
  'stringr', 'forcats'
)
xfun::pkg_attach2(pkgs, message = FALSE)



###############################################################################
# STEP 1: Price  -----
con <- dbConnect(
  RPostgres::Postgres(),
  host = '192.168.4.133',
  user = 'postgres',
  password = 'smxK9T',
  dbname = 'db_production'
)

price_oae_m_region <- dbReadTable(con, 'price_oae_m_region')
price_oae_m_TH00 <- price_oae_m_region %>% 
  filter(province_code == "TH00") %>%
  mutate(
    date = ymd(glue('{year-543}-{month}-01')),
    price_name = str_replace(price_name, 'น้ำยางข้น', 'น้ำยางสด')
  ) %>%
  arrange(date, commod, subcommod, price_name)

price_oae_m_TH00 %>% saveRDS(here('data/price_oae_m_TH00.rds'))
price_oae_m_TH00 %>% write_xlsx(here('data/price_oae_m_TH00.xlsx'))


ref_price_oae <- price_oae_m_TH00 %>% count(price_name, commod, subcommod, unit)
ref_price_oae %>% saveRDS(here('data/ref_price_oae.rds'))
ref_price_oae %>% write_xlsx(here('data/ref_price_oae.xlsx'))



###############################################################################
# STEP 2: Export  -----
trade_m <- read_excel(here("data/trade_m.xlsx"))
trade_m <- trade_m %>% 
  pivot_longer(
    -c(product_name:unit),
    names_to = "date",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(date = as.Date(date))

trade_m %>% saveRDS(here('data/trade_m.rds'))

ref_trade <- trade_m %>% count(product_name, subproduct_name, variable, unit)
ref_trade %>% saveRDS(here('data/ref_trade.rds'))
