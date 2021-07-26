library(shiny)
library(shinyWidgets)
library(htmltools)
library(bslib)
library(leaflet)

products <- c("ข้าว", "ข้าวโพดเลี้ยงสัตว์", "มันสำปะหลัง", "ปาล์มน้ำมัน", "ยางพารา")

dropdown_btn <- absolutePanel(
  fixed = TRUE,
  draggable = TRUE,
  top = 160,
  left = 20,
  right = "auto",
  bottom = "auto",
  height = "auto",
  dropdown( 
    tags$h4("ตัวกรองข้อมูล"),
    selectInput("product", "สินค้า", products, "ข้าว"),     
    style = "unite", 
    status = "danger",
    right = FALSE,
    icon = icon("gear"), 
    width = "300px"
    # animate = animateOptions(
    #   enter = animations$fading_entrances$fadeInRightBig
    # )
  )
)


fluidPage(
  theme = bs_theme(
    bg = "#d8d7d7", 
    fg = "#111E6C", 
    primary = "#4C516D", 
    base_font = font_google("Bai Jamjuree"),
    heading_font = font_google("Kanit"),
    code_font = font_google("Roboto Mono")
  ),
  h2('ราคาสินค้าเกษตรรายวัน'),
  fluidRow(
    column(4,
      leafletOutput("mymap", height = 750),
      dropdown_btn
    ),
    column(8,
      tabsetPanel(
        tabPanel("สถานการณ์", "Plot"), 
        tabPanel("กราฟราคา", "Summary"), 
        tabPanel("ตาราง", "Table")
      )
    )
  )
)

