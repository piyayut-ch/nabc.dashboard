library(shiny)
library(shinydashboard)
library(writexl)

ui <- dashboardPage(
  dashboardHeader(title = "excel download"),
  dashboardSidebar(
    h4("Download:"),
    downloadLink("download_excel", "excel"),
    downloadLink("download_csv", "csv")
  ),
  dashboardBody(),
  skin = "purple"
)

server <- function(input, output) {
  data <- mtcars
  output$download_excel <- downloadHandler(
    filename = function(){"mtcars.xlsx"},
    content = function(con) {
      write_xlsx(data, con)
    },
    contentType="application/xlsx"
  )
  output$download_csv <- downloadHandler(
    filename = function(){"mtcars.csv"},
    content = function(con) {
      write.csv(data, con)
    },
    contentType="text/csv"
  )
}

shinyApp(ui = ui, server = server)