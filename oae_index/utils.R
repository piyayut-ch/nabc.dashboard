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
