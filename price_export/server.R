function(input, output, session) {
  ##############################################################################
  # price -----
  # ui
  commod_price_filtered <- reactive({
    filter(ref_price_oae, commod == input$commod_price)
  })
  observeEvent(commod_price_filtered(), {
    choices <- unique(commod_price_filtered()$price_name)
    updateSelectInput(session, inputId = "price_name", choices = choices)
  })

  # plot
  output$plot_price <- renderEcharts4r({
    echart_price(data_price_graph, input$price_name, ref_price_oae)
  })

  # table
  data_price_table_filtered <- reactive({
    filter(data_price_table, commod == input$commod_price)
  })
  output$tbl_price <- renderReactable({
    tbl_price(data_price_table_filtered() %>% filter(price_name == input$price_name))
  })

  # raw table


  ##############################################################################
  # export -----
  # ui
  commod_export_filtered <- reactive({
    filter(ref_trade, product_name == input$commod_export)
  })
  observeEvent(commod_export_filtered(), {
    choices <- unique(commod_export_filtered()$subproduct_name)
    updateSelectInput(session, inputId = "subcommod_export", choices = choices) 
  })

  # plot
  output$plot_trade <- renderEcharts4r({
    echart_trade(data_trade_graph, input$subcommod_export, input$impexp_export, input$variable_export)
  })

  # table
  output$tbl_trade <- renderReactable({
    tbl_trade(data_trade_table, input$subcommod_export)
  })

  # raw table
  
}