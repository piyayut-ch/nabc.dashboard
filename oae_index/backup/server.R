function(input, output, session) {
  ##############################################################################
  # ui
  observe({
    x <- input$year_max
    updateSelectInput(session, "year_max", "ปีสุดท้าย", choices = input$year_min:year_max, x)
  })

  
  # monthly -----
  data_DT_M <- reactive({
    oae_index_m %>%
      filter(PROVINCE_NM_TH == input$province) %>%
      filter(YEAR_TH >= input$year_min & YEAR_TH <= input$year_max) %>%
      mutate(
        CI = round(CI,0)
      ) %>%
      arrange(YEAR_TH, MONTH) %>%
      pivot_wider(
        c("PROVINCE_CODE", "PROVINCE_NM_TH", "PROD_CODE", "PROD_NAME"),
        names_from = c("YEAR_TH", "MONTH_LABEL"),
        values_from = "CI"
      ) %>%
      arrange(PROVINCE_NM_TH, PROD_CODE)    
  })
  
  output$tbl_m = renderDataTable(
    DT_index(data_DT_M(), "M")
  )
  
  
  # quarterly -----
  data_DT_Q <- reactive({
    oae_index_q %>%
      filter(PROVINCE_NM_TH == input$province) %>%
      filter(YEAR_TH >= input$year_min & YEAR_TH <= input$year_max) %>%
      mutate(
        CI = round(CI,0)
      ) %>%
      arrange(YEAR_TH, QUARTER) %>%
      pivot_wider(
        c("PROVINCE_CODE", "PROVINCE_NM_TH", "PROD_CODE", "PROD_NAME"),
        names_from = c("YEAR_TH", "QUARTER_LABEL"),
        values_from = "CI"
      ) %>%
      arrange(PROVINCE_NM_TH, PROD_CODE)
  })
  
  output$tbl_q = renderDataTable(
    DT_index(data_DT_Q(), "Q")
  )
  
  
  # yearly -----
  data_DT_Y <- reactive({
    oae_index_y %>%
      filter(PROVINCE_NM_TH == input$province) %>%
      filter(YEAR_TH >= input$year_min & YEAR_TH <= input$year_max) %>%
      mutate(
        CI = round(CI,0)
      ) %>%
      arrange(YEAR_TH) %>%
      pivot_wider(
        c("PROVINCE_CODE", "PROVINCE_NM_TH", "PROD_CODE", "PROD_NAME"),
        names_from = "YEAR_TH",
        values_from = "CI"
      ) %>%
      arrange(PROVINCE_NM_TH, PROD_CODE)  
  })
  
  output$tbl_y = renderDataTable(
    DT_index(data_DT_Y(), "Y")
  )

}