################################################################################
# utility functions -----
# percent_change <- function(new, old, digit = 2) round((new-old)*100/old, digit)

# with_tooltip <- function(value, tooltip) {
#   tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
#             title = tooltip, value)
# }

# e_common(
#   font_family = "Athiti",
#   theme = NULL
# )

################################################################################

gen_sketch <- function(df, tf) {
  meta <- colnames(df)[-c(1:4)] %>% str_split_fixed("_", 2) %>% as.data.frame()
  colnames(meta) <- c("x1", "x2")
  meta_by_year <- meta %>% count(x1)
  
  
  tr1 <- list()
  tr1[[1]] <- if(tf == "Y") {
    tags$th('รายการ')
  } else {
    tags$th(rowspan = 2, 'รายการ')
  }

  for(i in 1:nrow(meta_by_year)){
    tr1[[i+1]] = tags$th(colspan = meta_by_year[[i,2]], meta_by_year[[i,1]])
  }
  
  tr2 <- list()
  for(i in 1:nrow(meta)){
    tr2[[i]] = tags$th(meta[[i,2]])
  }
  
  if(tf != "Y") {
    tags$table(class = "display",
      tags$thead(
        tags$tr(tr1),
        tags$tr(tr2)
      )
    )
  } else {
    tags$table(class = "display",
      tags$thead(
        tags$tr(tr1)
      )
    )
  }
}


DT_index <- function(df, tf){
  col_names <- df %>% select(-c(1:4)) %>% colnames() %>% str_sub(1, 4) %>% as.numeric()
  year_begin <- min(col_names)
  year_end <- max(col_names)
  n_cols = length(col_names) + 1
  
  sketch = gen_sketch(df, tf)
  
  datatable(
    df[, -c(1:3)],
    container = sketch,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
      dom = 'Bt',
      ordering = FALSE,
      pageLength = 40,
      initComplete = JS(
        "function(settings, json) {",
        "$('body').css({'font-family': 'Athiti'});",
        "$('div.dt-buttons').css({'float' : 'right'});",
        "}"
      ),
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  ) %>%
    formatStyle(
      1:n_cols, valueColumns = 1,
      backgroundColor = styleEqual(
        data_fmt$PROD_NAME, data_fmt$bgcolor
      )
    )
}



# # table price -----
# tbl_price <- function(df) {
#   reactable::reactable(
#     df,
#     height = '500px',
#     filterable = FALSE,
#     searchable = FALSE,
#     highlight = TRUE,
#     columns = list(
#       price_name = reactable::colDef(
#         name = 'ราคา', width = 400,
#         style = list(borderRight = "1px solid rgba(0, 0, 0, 0.1)"),
#         cell = function(value, index) {
#           commod <- df$commod[index]
#           unit <- df$unit[index]
#           htmltools::tagList(
#             htmltools::div(style = list(fontWeight = 500), value),
#             htmltools::div(style = list(fontSize = 12), unit)
#           )
#         }
#       ),
#       commod = reactable::colDef(name = 'สินค้า', show = FALSE),
#       unit = reactable::colDef(name = 'หน่วย', show = FALSE),
#       last5y = reactable::colDef(
#         name = "ราคาเฉลี่ย 2558-2562", width = 100,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave1 = reactable::colDef(
#         name = "ราคาเฉลี่ย", width = 120,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave1_pc = reactable::colDef(
#         header = with_tooltip("%", "ร้อยละการเปลี่ยนแปลงเทียบกับราคาเฉลี่ย 5 ปี"),
#         width = 70,
#         cell = function(value, index) {
#           v = df$wave1_pc[[index]]
#           ifelse(v >= 0, paste0("+", v), v)
#         },
#         style = function(value, index) {
#           v = df$wave1_pc[[index]]
#           color = ifelse(v > 0, "#008000", "#e00000")
#           list(color = color)
#         }
#       ),
#       wave2 = reactable::colDef(
#         name = "ราคาเฉลี่ย", width = 120,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave2_pc = reactable::colDef(
#         header = with_tooltip("%", "ร้อยละการเปลี่ยนแปลงเทียบกับราคาเฉลี่ย 5 ปี"),
#         width = 70,
#         cell = function(value, index) {
#           v = df$wave2_pc[[index]]
#           ifelse(v >= 0, paste0("+", v), v)
#         },
#         style = function(value, index) {
#           v = df$wave2_pc[[index]]
#           color = ifelse(v > 0, "#008000", "#e00000")
#           list(color = color)
#         }
#       ),
#       wave3 = reactable::colDef(
#         name = "ราคาเฉลี่ย", width = 120,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave3_pc = reactable::colDef(
#         header = with_tooltip("%", "ร้อยละการเปลี่ยนแปลงเทียบกับราคาเฉลี่ย 5 ปี"),
#         width = 70,
#         cell = function(value, index) {
#           v = df$wave3_pc[[index]]
#           ifelse(v >= 0, paste0("+", v), v)
#         },
#         style = function(value, index) {
#           v = df$wave3_pc[[index]]
#           color = ifelse(v > 0, "#008000", "#e00000")
#           list(color = color)
#         }
#       )

#     ),
#     columnGroups = list(
#       reactable::colGroup(
#         name = "การระบาดรอบ 1", 
#         columns = c("wave1", "wave1_pc")
#       ),
#       reactable::colGroup(
#         name = "การระบาดรอบ 2", 
#         columns = c("wave2", "wave2_pc")
#       ),
#       reactable::colGroup(
#         name = "การระบาดรอบ 3", 
#         columns = c("wave3", "wave3_pc")
#       )
#     )
#   )  
# }



# ################################################################################
# # plot price -----
# plot_price <- function(df, price, ref) {

#   meta <- ref %>% dplyr::filter(price_name == price) %>% as.list()
#   title <- glue::glue('ราคา{price} ที่เกษตรกรขายได้ ณ ไร่นา')

#   df %>%
#     dplyr::filter(price_name %in% price) %>%
#     dplyr::select(date, price_name, commod, unit, value) %>%
#     ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = price_name)) +
#     ggplot2::geom_line(size = 1.5) +
#     ggsci::scale_color_d3() +
#     ggplot2::expand_limits(y = 0) +
#     ggplot2::scale_y_continuous(label = comma) +
#     ggplot2::labs(
#       title = title,
#       x = "ปี",
#       y = meta$unit
#     ) +
#     # hrbrthemes::theme_ipsum(
#     #   base_size = 16,
#     #   base_family = "Athiti Light",
#     #   axis_title_size = 16
#     # ) +
#     ggplot2::annotate(
#       "rect", fill = "red", alpha = 0.5, 
#       xmin = as.Date('2020-03-01'), xmax = as.Date('2020-04-01'),
#       ymin = -Inf, ymax = Inf
#     ) +
#     ggplot2::annotate(
#       "rect", fill = "red", alpha = 0.5, 
#       xmin = as.Date('2021-01-01'), xmax = as.Date('2021-02-01'),
#       ymin = -Inf, ymax = Inf
#     ) +
#     ggplot2::annotate(
#       "rect", fill = "red", alpha = 0.5, 
#       xmin = as.Date('2021-04-01'), xmax = as.Date('2021-06-01'),
#       ymin = -Inf, ymax = Inf
#     ) +
#     ggplot2::theme(
#       legend.position = "none"
#     ) +
#     ggplot2::scale_x_date(
#       date_breaks = '1 years',
#       labels = function(x) year(x)+543
#     )
# }



# ################################################################################
# # table export -----
# tbl_trade <- function(df, .subproduct_name, .impexp = "ส่งออก") {
#   df_filtered <- df %>% filter(subproduct_name == .subproduct_name, impexp == .impexp)
  
#   reactable::reactable(
#     df_filtered,
#     height = '500px',
#     filterable = FALSE,
#     searchable = FALSE,
#     highlight = TRUE,
#     columns = list(
#       subproduct_name = reactable::colDef(
#         name = 'สินค้า', width = 400,
#         cell = function(value, index) {
#           commod <- df_filtered$product_name[index]
#           unit <- df_filtered$unit[index]
#           htmltools::tagList(
#             htmltools::div(style = list(fontWeight = 500), value),
#             htmltools::div(style = list(fontSize = 12), commod)
#           )
#         }
#       ),
#       product_name = reactable::colDef(name = 'สินค้าหลัก', show = FALSE),
#       unit = reactable::colDef(name = 'หน่วย', show = FALSE),
#       impexp = reactable::colDef(name = 'การค้า', show = FALSE),
#       variable = reactable::colDef(
#         name = '', width = 70,
#         style = list(borderRight = "1px solid rgba(0, 0, 0, 0.1)"),
#         cell = function(value, index) {
#           unit <- df_filtered$unit[index]
#           htmltools::tagList(
#             htmltools::div(style = list(fontWeight = 500, `text-align` = 'right'), value),
#             htmltools::div(style = list(fontSize = 12, `text-align` = 'right'), unit)
#           )
#         }
#       ),
#       last5y = reactable::colDef(
#         name = "ค่าเฉลี่ย 2558-2562", width = 100,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave1 = reactable::colDef(
#         name = "ค่าเฉลี่ย", width = 120,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave1_pc = reactable::colDef(
#         header = with_tooltip("%", "ร้อยละการเปลี่ยนแปลงเทียบกับค่าเฉลี่ย 5 ปี"),
#         width = 70,
#         cell = function(value, index) {
#           v = df_filtered$wave1_pc[[index]]
#           ifelse(v >= 0, paste0("+", v), v)
#         },
#         style = function(value, index) {
#           v = df_filtered$wave1_pc[[index]]
#           color = ifelse(v > 0, "#008000", "#e00000")
#           list(color = color)
#         }
#       ),
#       wave2 = reactable::colDef(
#         name = "ค่าเฉลี่ย", width = 120,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave2_pc = reactable::colDef(
#         header = with_tooltip("%", "ร้อยละการเปลี่ยนแปลงเทียบกับค่าเฉลี่ย 5 ปี"),
#         width = 70,
#         cell = function(value, index) {
#           v = df_filtered$wave2_pc[[index]]
#           ifelse(v >= 0, paste0("+", v), v)
#         },
#         style = function(value, index) {
#           v = df_filtered$wave2_pc[[index]]
#           color = ifelse(v > 0, "#008000", "#e00000")
#           list(color = color)
#         }
#       ),
#       wave3 = reactable::colDef(
#         name = "ค่าเฉลี่ย", width = 120,
#         format = reactable::colFormat(separators = TRUE)
#       ),
#       wave3_pc = reactable::colDef(
#         header = with_tooltip("%", "ร้อยละการเปลี่ยนแปลงเทียบกับค่าเฉลี่ย 5 ปี"),
#         width = 70,
#         cell = function(value, index) {
#           v = df_filtered$wave3_pc[[index]]
#           ifelse(v >= 0, paste0("+", v), v)
#         },
#         style = function(value, index) {
#           v = df_filtered$wave3_pc[[index]]
#           color = ifelse(v > 0, "#008000", "#e00000")
#           list(color = color)
#         }
#       )
#     ),
#     columnGroups = list(
#       reactable::colGroup(
#         name = "การระบาดรอบ 1", 
#         columns = c("wave1", "wave1_pc")
#       ),
#       reactable::colGroup(
#         name = "การระบาดรอบ 2", 
#         columns = c("wave2", "wave2_pc")
#       ),
#       reactable::colGroup(
#         name = "การระบาดรอบ 3", 
#         columns = c("wave3", "wave3_pc")
#       )
#     )
#   )
# }


# ################################################################################
# # plot trade -----
# plot_trade <- function(df, .subproduct, .impexp, .variable) {

#   df <- df %>%
#     dplyr::filter(subproduct_name == .subproduct, impexp == .impexp, variable == .variable) %>%
#     dplyr::select(date, subproduct_name, impexp, variable, unit, value)
  
#   meta <- df %>% slice(1) %>% as.list()
#   title <- glue::glue('{.variable}{.impexp} {.subproduct}')

#   df %>%
#     ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = subproduct_name)) +
#     ggplot2::geom_line(size = 1.5) +
#     ggsci::scale_color_d3() +
#     ggplot2::expand_limits(y = 0) +
#     ggplot2::scale_y_continuous(label = comma) +
#     ggplot2::labs(
#       title = title,
#       x = "ปี",
#       y = meta$unit
#     ) +
#     # hrbrthemes::theme_ipsum(
#     #   base_size = 16,
#     #   base_family = "Athiti Light",
#     #   axis_title_size = 16
#     # ) +
#     ggplot2::annotate(
#       "rect", fill = "red", alpha = 0.5, 
#       xmin = as.Date('2020-03-01'), xmax = as.Date('2020-04-01'),
#       ymin = -Inf, ymax = Inf
#     ) +
#     ggplot2::annotate(
#       "rect", fill = "red", alpha = 0.5, 
#       xmin = as.Date('2021-01-01'), xmax = as.Date('2021-02-01'),
#       ymin = -Inf, ymax = Inf
#     ) +
#     ggplot2::annotate(
#       "rect", fill = "red", alpha = 0.5, 
#       xmin = as.Date('2021-04-01'), xmax = as.Date('2021-06-01'),
#       ymin = -Inf, ymax = Inf
#     ) +
#     ggplot2::theme(
#       legend.position = "none"
#     ) +
#     ggplot2::scale_x_date(
#       date_breaks = '1 years',
#       labels = function(x) year(x)+543
#     )
# }


# echart_price <- function(df, price, ref) {
#   meta <- ref %>% dplyr::filter(price_name == price) %>% as.list()
#   title <- glue::glue('ราคา{price} ที่เกษตรกรขายได้ ณ ไร่นา')
#   df <- df %>%
#     dplyr::filter(price_name %in% price) %>%
#     dplyr::select(date, price_name, commod, unit, value)
  
#   df %>%
#     e_charts(x = date) %>% 
#     e_line(
#       serie = value, name = '', symbol = 'none',
#       markArea = list(
#         itemStyle = list(color = 'rgba(255, 173, 177, 0.4)'),
#         data = list(
#           list(list(name = "ระบาดรอบ 1", xAxis = as.Date('2020-03-01')), list(xAxis = as.Date('2020-04-01'))),
#           list(list(name = "รอบ 2", xAxis = as.Date('2021-01-01')), list(xAxis = as.Date('2021-02-01'))),
#           list(list(name = "รอบ 3", xAxis = as.Date('2021-04-01')), list(xAxis = as.Date('2021-06-01')))
#         )
#       )
#     ) %>%
#     e_x_axis(name = "ปี") %>%
#     e_title(title, meta$unit) %>%
#     e_toolbox_feature(feature = "saveAsImage") %>%
#     e_grid(left = "50", right = "5%")
# }


# echart_trade <- function(df, .subproduct, .impexp, .variable) {
#   df <- df %>%
#     dplyr::filter(subproduct_name == .subproduct, impexp == .impexp, variable == .variable) %>%
#     dplyr::select(date, subproduct_name, impexp, variable, unit, value)
  
#   meta <- df %>% slice(1) %>% as.list()
#   title <- glue::glue('{.variable}{.impexp} {.subproduct}')
  
#   df %>%
#     e_charts(x = date) %>% 
#     e_line(
#       serie = value, name = '', symbol = 'none',
#       markArea = list(
#         itemStyle = list(color = 'rgba(255, 173, 177, 0.4)'),
#         data = list(
#           list(list(name = "ระบาดรอบ 1", xAxis = as.Date('2020-03-01')), list(xAxis = as.Date('2020-04-01'))),
#           list(list(name = "รอบ 2", xAxis = as.Date('2021-01-01')), list(xAxis = as.Date('2021-02-01'))),
#           list(list(name = "รอบ 3", xAxis = as.Date('2021-04-01')), list(xAxis = as.Date('2021-06-01')))
#         )
#       )
#     ) %>%
#     e_x_axis(name = "ปี") %>%
#     e_title(title, meta$unit) %>%
#     e_toolbox_feature(feature = "saveAsImage") %>%
#     e_grid(left = "70", right = "5%")
# }