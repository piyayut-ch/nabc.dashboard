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