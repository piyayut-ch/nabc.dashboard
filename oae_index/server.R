function(input, output, session) {
  ##############################################################################
  # ui
  observe({
    x <- input$year_max
    updateSelectInput(session, "year_max", "ถึง", choices = input$year_min:year_max, x)
  })
  
  
  ##############################################################################
  # index
  # yearly -----
  data_y <- reactive({
    oae_index_y %>%
      filter(province_nm_th %in% input$province) %>%
      filter(year_th >= input$year_min & year_th <= input$year_max) %>%
      index_wider(.variable = input$index_type)
  })
  
  output$tbl_y = renderReactable(
    reactable_index(data_y())
  )
  
  
  # quarterly -----
  data_q <- reactive({
    oae_index_q %>%
      filter(province_nm_th %in% input$province) %>%
      filter(year_th >= input$year_min & year_th <= input$year_max) %>%
      index_wider(.variable = input$index_type)
  })
  
  output$tbl_q = renderReactable(
    reactable_index(data_q())
  )
  

  # monthly -----
  data_m <- reactive({
    oae_index_m %>%
      filter(province_nm_th %in% input$province) %>%
      filter(year_th >= input$year_min & year_th <= input$year_max) %>%
      index_wider(.variable = input$index_type)
  })
  
  output$tbl_m = renderReactable(
    reactable_index(data_m())
  )

  # data for download
  data_to_download <- reactive({
    if(input$oae_index == 1) {
      data_y()
    } else if (input$oae_index == 2) {
      data_q()
    } else if (input$oae_index == 3) {
      data_m()
    }
  })
    
  output$download_excel <- downloadHandler(
    filename = function(){"oae_index.xlsx"},
    content = function(con) {
      write_xlsx(data_to_download(), con)
    },
    contentType="application/xlsx"
  )
  output$download_csv <- downloadHandler(
    filename = function(){"oae_index.csv"},
    content = function(con) {
      write.csv(data_to_download(), con)
    },
    contentType="text/csv"
  )
  
  
  ##############################################################################
  # map
  data_map <- reactive({
    map_tha1 %>%
      left_join(
        oae_fact_y_final %>% filter(year_th == input$year_calendar, prod_name == input$product), 
        by = c("adm1_pcode" = "adm1_pcode"))
  })
  
  output$map_production <- renderLeaflet({
    map_diff(data_map(), "production")
  })
  
  output$map_price <- renderLeaflet({
    map_diff(data_map(), "price_avg")
  })
  
  output$map_farm_income <- renderLeaflet({
    map_diff(data_map(), "farm_income")
  })
}