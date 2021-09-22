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
index_wider <- function(df, .variable = "ci") {
  df %>%
    pivot_wider(
      prod_code:province_nm_th,
      names_from = "time_label",
      values_from = .variable
    ) %>%
    select(province_nm_th, prod_name, everything()) %>%
    arrange(province_nm_th, prod_name)
}

reactable_index <- function(df) {
  reactable(
    df,
    highlight = TRUE,
    sortable = FALSE,
    bordered = TRUE,
    defaultPageSize = 38,
    defaultColDef = colDef(
  #     cell = function(value) format(value, nsmall = 1),
      minWidth = 90,
      headerStyle = list(background = "#000753", color = "white", textAlign = "center")
    ),
    columns = list(
      province_nm_th = colDef(
        name = "ภาค/จังหวัด",
        width = 100,
        sticky = "left",
      ),
      prod_name = colDef(
        name = "สินค้า",
        width = 120,
        sticky = "left",
      ),
      province_code = colDef(show = FALSE),
      prod_code = colDef(show = FALSE)
    ),
    rowStyle = function(index) {
      if (df[index, "prod_code"] == 0) {
        list(background = "lightskyblue")
      } else if (df[index, "prod_code"] %in% c(1e8, 2e8, 3e8)) {
        list(background = "azure")
      }
    },
    rowClass = function(index) {
      if (df[index, "province_code"] == 0) {
        "bold"
      }
    }
  )
}


################################################################################
map_diff <- function(df_map, 
                     .variable = "production", 
                     .pal = c("#ca0020", "#f4a582", "#f7f7f7", "#92c5de", "#0571b0")) {
  
  .variable_diff <- paste0(.variable, "_diff_cut")
  .variable_label <- case_when(
    .variable == "production" ~ "ผลผลิต",
    .variable == "price_avg" ~ "ราคา",
    TRUE ~ "รายได้",
  )
  .variable_unit <- case_when(
    .variable == "production" ~ "ตัน",
    .variable == "price_avg" ~ "บาท",
    TRUE ~ "บาท",
  )
  pal <- colorFactor(
    .pal,
    df_map[[.variable_diff]]
  )

  var1 <- list(
    label = .variable_label,
    value = format(
      round(df_map[[.variable]], 0), 
      big.mark = ',', nsmall = 0
    )
  )

  lab <- glue::glue(
      "<strong>{df_map$adm1_th}</strong><br/>
      {var1$label}: {var1$value} {.variable_unit} <br/>
    ") %>% lapply(htmltools::HTML)

  leaflet(df_map) %>%
    addProviderTiles(providers$CartoDB.Voyager) %>%
    addPolygons(
      fillColor = ~pal(df_map[[.variable_diff]]),
      weight = 1,
      opacity = 1,
      color = "dimgrey",
      dashArray = "2",
      fillOpacity = 1,
      label = lab,
      labelOptions = labelOptions(textsize = "14px"),
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~df_map[[.variable_diff]],
      opacity = 0.7,
      title = "",
      na.label = "ไม่มีข้อมูล",
      position = "bottomleft"
    )
}