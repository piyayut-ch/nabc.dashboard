# -*- coding: utf-8 -*-
# +
pkgs <- c(
  'tidyverse',
  'openxlsx',
  'lubridate',
  'reticulate',
  'fs',
  'glue',
  'here'
)
suppressMessages(xfun::pkg_attach2(pkgs))

source(here("R/utils.R"))
# -

xls2xlsx <- import('xls2xlsx')
pd <- import('pandas')

# # Production: Area, Production, Yield

# +
product_code <- "113.1"
product_name <- "ข้าวนาปี"
path <- here(
  "data-raw/01_production_oae/",
  "ส่วนพืชไร่",
  glue("{product_code}_{product_name}"),
  "01_apy_adm1.xls"
)

excelfile <- pd$ExcelFile(path)

df_apy <- excelfile$sheet_names %>%
  map(function(x){
    excelfile$parse(x, skiprows = 0:2) %>%
    mutate(year = glue(25, x) %>% as.integer(), .before = 1)
  }) %>% reduce(bind_rows) %>%
  mutate(
    product_code = product_code, 
    product_name = product_name,
    .before = 2
  ) %>% as_tibble()

# +
df_2560 <- df %>% filter(year == 2560)

write.xlsx(df_2560, file = "df_2560.xlsx")
write.xlsx(df_2560, file = "df_2560tbl.xlsx", asTable = TRUE)

# +
df_2561 <- df %>% filter(year == 2561)
df_2562 <- df %>% filter(year == 2562)


l <- list("2561" = df_2561, "2562" = df_2562)
write.xlsx(l, file = "df_2561-2562.xlsx")

# +
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
options("openxlsx.dateFormat" = "mm/dd/yyyy")
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
# options("openxlsx.numFmt" = NULL) ## For default style rounding of numeric columns

df <- data.frame("Date" = Sys.Date()-0:19, "LogicalT" = TRUE,
                 "Time" = Sys.time()-0:19*60*60,
                 "Cash" = paste("$",1:20), "Cash2" = 31:50,
                 "hLink" = "https://CRAN.R-project.org/",
                 "Percentage" = seq(0, 1, length.out=20),
                 "TinyNumbers" = runif(20) / 1E9,  stringsAsFactors = FALSE)

class(df$Cash) <- "currency"
class(df$Cash2) <- "accounting"
class(df$hLink) <- "hyperlink"
class(df$Percentage) <- "percentage"
class(df$TinyNumbers) <- "scientific"

write.xlsx(df, "writeXLSX3.xlsx", overwrite = TRUE)

# +
hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                  halign = "center", valign = "center", textDecoration = "Bold",
                  border = "TopBottomLeftRight", textRotation = 45)

write.xlsx(iris, file = "writeXLSX4.xlsx", borders = "rows", headerStyle = hs)
write.xlsx(iris, file = "writeXLSX5.xlsx", borders = "columns", headerStyle = hs)

write.xlsx(
  iris, "writeXLSXTable4.xlsx", 
  asTable = FALSE, 
  headerStyle = createStyle(textRotation = 45)
)
# -

wb <- write.xlsx(iris, "writeXLSX6.xlsx", overwrite = TRUE)
setColWidths(wb, sheet = 1, cols = 1:5, widths = 20)
modifyBaseFont(wb, fontSize = 16, fontName = "TH Sarabun New")
saveWorkbook(wb, "writeXLSX6.xlsx", overwrite = TRUE)

crop_year <- "2556/57"
title <- glue(
  product_name,
  "เนื้อที่เพาะปลูก เนื้อที่เก็บเกี่ยว ผลผลิต และผลผลิตต่อไร่",
  "ปีเพาะปลูก {crop_year} ที่ความชื้น 15%"
)
title_style <- createStyle(fontSize=16, textDecoration=c("bold"))
header_style <- createStyle(
  fontSize = 16, textDecoration=c("bold"),
  halign = "center", valign = "center",
  border = "TopBottomLeftRight",
  borderColour = "#000000"
)
# bold_style <- createStyle(
#   fontSize = 16, textDecoration=c("bold"),
#   border = "TopBottomLeftRight",
#   borderColour = "#000000"
# )

# +
make_report_apy <- function(df, output = ) {
  
}

# +
class(df_2562$area_plant)   <- "comma"
class(df_2562$area_harvest) <- "comma"
class(df_2562$production)   <- "comma"

wb <- createWorkbook()
options("openxlsx.borderColour" = "#000000")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 16, fontName = "TH Sarabun New")
addWorksheet(wb, sheetName = "2562", gridLines = FALSE)
freezePane(wb, 1, firstActiveRow = 4, firstActiveCol = NULL)

writeData(wb, 1, x = title, 
          startCol = 1, startRow = 1, colNames = FALSE)

writeData(wb, 1, x = 'ภาค/จังหวัด', 
          startCol = 1, startRow = 2, colNames = FALSE)
writeData(wb, 1, x = 'เนื้อที่เพาะปลูก (ไร่)', 
          startCol = 2, startRow = 2, colNames = FALSE)
writeData(wb, 1, x = 'เนื้อที่เก็บเกี่ยว (ตัน)', 
          startCol = 3, startRow = 2, colNames = FALSE)
writeData(wb, 1, x = 'ผลผลิต (ตัน)', 
          startCol = 4, startRow = 2, colNames = FALSE)
writeData(wb, 1, x = 'ผลผลิตเฉลี่ยต่อไร่ (กก.)', 
          startCol = 5, startRow = 2, colNames = FALSE)
writeData(wb, 1, x = 'ปลูก', 
          startCol = 5, startRow = 3, colNames = FALSE)
writeData(wb, 1, x = 'เก็บ', 
          startCol = 6, startRow = 3, colNames = FALSE)

writeData(wb, 1, x = df_2562 %>% select(4:9), 
          borders = "columns",
          startCol = 1, startRow = 4, colNames = FALSE)

mergeCells(wb, 1, cols = 1, rows = 2:3)
mergeCells(wb, 1, cols = 2, rows = 2:3)
mergeCells(wb, 1, cols = 3, rows = 2:3)
mergeCells(wb, 1, cols = 4, rows = 2:3)
mergeCells(wb, 1, cols = 5:6, rows = 2)

setColWidths(wb, 1, cols = 1, widths = 25)
setColWidths(wb, 1, cols = 2:4, widths = 20)
setColWidths(wb, 1, cols = 5:6, widths = 15)

addStyle(wb, 1, style = title_style, rows = 1, cols = 1)
addStyle(wb, 1, style = header_style, rows = 2, cols = 1:6)
addStyle(wb, 1, style = header_style, rows = 3, cols = 1:6)

saveWorkbook(wb, "report_apy.xlsx", overwrite = TRUE)
# -

xls2xlsx <- import('xls2xlsx')

xls2xlsx$XLS2XLSX

# # Prodiction: Month

# +
product_code <- "113.1"
product_name <- "ข้าวนาปี"
path <- here(
  "data-raw/01_production_oae/",
  "ส่วนพืชไร่",
  glue("{product_code}_{product_name}"),
  "08_production_m.xls"
)

excelfile <- pd$ExcelFile(path)

df_month <- excelfile$sheet_names %>%
  map(function(x){
    excelfile$parse(x, skiprows = 0:2) %>%
    mutate(
      across(-1, as.numeric),
      province = as.character(province) %>% na_if("NaN")
    ) %>%
    fill(province) %>%
    group_by(province) %>%
    mutate(
      year = glue(25, x) %>% as.integer(),
      id = row_number(),
      variable = ifelse(id == 1, "ร้อยละ", "ปริมาณ"),
      .before = 1
    ) %>%
    select(-id) %>%
    pivot_longer(-c(province, variable, year)) %>%
    filter(!(name %in% c('total') | str_detect(name, 'Unnamed'))) %>% 
    as_tibble()
  }) %>%
  reduce(bind_rows) %>%
  mutate(
    product_code = product_code, 
    product_name = product_name,
    .before = 2
  )
# -

df_month


