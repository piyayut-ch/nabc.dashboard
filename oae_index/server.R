function(input, output, session) {
  ##############################################################################
  # ui
#   commod_price_filtered <- reactive({
#     filter(ref_price_oae, commod == input$commod_price)
#   })
#   observeEvent(commod_price_filtered(), {
#     choices <- unique(commod_price_filtered()$price_name)
#     updateSelectInput(session, inputId = "price_name", choices = choices)
#   })
  observe({
    x <- input$year_max
    updateSelectInput(session, "year_max", "ปีสุดท้าย", choices = input$year_min:year_max, x)
  })

  
  # monthly -----
  data_DT_M <- reactive({
    oae_index_m %>%
      filter(PROVINCE_NM_TH == input$province) %>%
      filter(YEAR >= input$year_min & YEAR <= input$year_max) %>%
      mutate(
        CI = round(CI,0)
      ) %>%
      arrange(YEAR, MONTH) %>%
      pivot_wider(
        c("PROVINCE_CODE", "PROVINCE_NM_TH", "PROD_CODE", "PROD_NAME"),
        names_from = c("YEAR", "MONTH"),
        values_from = "CI"
      ) %>%
      arrange(PROVINCE_NM_TH, PROD_NAME)    
  })
  
  output$tbl_m = renderDataTable(
    DT_index(data_DT_M(), "M")
  )
  
  
  # quarterly -----
  data_DT_Q <- reactive({
    oae_index_q %>%
      filter(PROVINCE_NM_TH == input$province) %>%
      filter(YEAR >= input$year_min & YEAR <= input$year_max) %>%
      mutate(
        CI = round(CI,0)
      ) %>%
      arrange(YEAR, QUARTER) %>%
      pivot_wider(
        c("PROVINCE_CODE", "PROVINCE_NM_TH", "PROD_CODE", "PROD_NAME"),
        names_from = c("YEAR", "QUARTER"),
        values_from = "CI"
      ) %>%
      arrange(PROVINCE_NM_TH, PROD_NAME)
  })
  
  output$tbl_q = renderDataTable(
    DT_index(data_DT_Q(), "Q")
  )
  
  
  # yearly -----
  data_DT_Y <- reactive({
    oae_index_y %>%
      filter(PROVINCE_NM_TH == input$province) %>%
      filter(YEAR >= input$year_min & YEAR <= input$year_max) %>%
      mutate(
        CI = round(CI,0)
      ) %>%
      arrange(YEAR) %>%
      pivot_wider(
        c("PROVINCE_CODE", "PROVINCE_NM_TH", "PROD_CODE", "PROD_NAME"),
        names_from = "YEAR",
        values_from = "CI"
      ) %>%
      arrange(PROVINCE_NM_TH, PROD_NAME)  
  })
  
  output$tbl_y = renderDataTable(
    DT_index(data_DT_Y(), "Y")
  )
  
#   # plot
#   output$plot_price <- renderEcharts4r({
#     echart_price(data_price_graph, input$price_name, ref_price_oae)
#   })

#   # table
#   data_price_table_filtered <- reactive({
#     filter(data_price_table, commod == input$commod_price)
#   })
#   output$tbl_price <- renderReactable({
#     tbl_price(data_price_table_filtered() %>% filter(price_name == input$price_name))
#   })

#   # raw table


#   ##############################################################################
#   # export -----
#   # ui
#   commod_export_filtered <- reactive({
#     filter(ref_trade, product_name == input$commod_export)
#   })
#   observeEvent(commod_export_filtered(), {
#     choices <- unique(commod_export_filtered()$subproduct_name)
#     updateSelectInput(session, inputId = "subcommod_export", choices = choices) 
#   })

#   # plot
#   output$plot_trade <- renderEcharts4r({
#     echart_trade(data_trade_graph, input$subcommod_export, input$impexp_export, input$variable_export)
#   })

#   # table
#   output$tbl_trade <- renderReactable({
#     tbl_trade(data_trade_table, input$subcommod_export)
#   })

#   # raw table
  
}