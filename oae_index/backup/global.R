################################################################################
# set up -----
pkgs <- c(
  'shiny',
  'glue', 'here',
  'tidyverse', 'DT',
  'lubridate',
  'htmltools',
  'extrafont', 'hrbrthemes', 'ggsci', 'scales'
)
xfun::pkg_attach2(pkgs, message = FALSE)


source(here('utils.R'), encoding = 'UTF-8')
# loadfonts(device = "win", quiet = TRUE)
# Sys.setlocale("LC_CTYPE", "Thai")

month_th <- c(
  "1" = "มค",
  "2" = "กพ",
  "3" = "มีค",
  "4" = "เมย",
  "5" = "พค",
  "6" = "มิย",
  "7" = "กค",
  "8" = "สค",
  "9" = "กย",
  "10" = "ตค",
  "11" = "พย",
  "12" = "ธค"
)

################################################################################
# load data and reference table -----
oae_index_m <- readRDS(here("data/oae_index_m.rds")) %>%
  filter(PROD_NAME != "ข้าวปทุมรวม" & PROD_NAME != "ข้าวขาวรวม" & PROD_NAME != "ข้าวนาปี") %>%
  mutate(
    MONTH_LABEL = month_th[MONTH],
    YEAR_TH = YEAR + 543,
    PROD_NAME_LABEL = case_when(
      PROD_CODE == 0 ~ as.character(PROD_NAME),
      PROD_CODE %in% c(100000000, 200000000, 300000000) ~ str_c("  ", PROD_NAME),
      TRUE ~ str_c("    ", PROD_NAME),
    )
  )
oae_index_q <- readRDS(here("data/oae_index_q.rds")) %>%
  filter(PROD_NAME != "ข้าวปทุมรวม" & PROD_NAME != "ข้าวขาวรวม" & PROD_NAME != "ข้าวนาปี") %>%
  mutate(
    QUARTER_LABEL = paste0("Q", QUARTER),
    YEAR_TH = YEAR + 543,
    PROD_NAME_LABEL = case_when(
      PROD_CODE == 0 ~ as.character(PROD_NAME),
      PROD_CODE %in% c(100000000, 200000000, 300000000) ~ str_c("  ", PROD_NAME),
      TRUE ~ str_c("    ", PROD_NAME),
    )
  )
oae_index_y <- readRDS(here("data/oae_index_y.rds")) %>%
  filter(PROD_NAME != "ข้าวปทุมรวม" & PROD_NAME != "ข้าวขาวรวม" & PROD_NAME != "ข้าวนาปี") %>%
  mutate(
    YEAR_TH = YEAR + 543,
    PROD_NAME_LABEL = case_when(
      PROD_CODE == 0 ~ as.character(PROD_NAME),
      PROD_CODE %in% c(100000000, 200000000, 300000000) ~ str_c("  ", PROD_NAME),
      TRUE ~ str_c("    ", PROD_NAME),
    )
  )



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

province_names <- oae_index_y %>% filter() %>% count(PROVINCE_NM_TH) %>% pull(PROVINCE_NM_TH)
region_names <- oae_index_y %>% count(PROVINCE_NM_TH) %>% pull(PROVINCE_NM_TH)

# province_names <- oae_index_y %>% count(PROVINCE_NM_TH) %>% pull(PROVINCE_NM_TH)

year_min <- min(oae_index_y$YEAR_TH)
year_max <- max(oae_index_y$YEAR_TH)