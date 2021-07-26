commod_price <- ref_price_oae$commod
commod_export <- ref_trade$product_name

sidebar_price <- sidebarLayout(
  sidebarPanel(
    selectInput("commod_price", "สินค้า", choices = commod_price),
    br(),
    selectInput("price_name", "ราคา", choices = NULL),
    width = 2
  ),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("ภาพรวม",
               echarts4rOutput("plot_price", height = "400px"),
               reactableOutput("tbl_price")
              ),
      tabPanel("ข้อมูลดิบ", )
    ),
    width = 10
  )
)



sidebar_export <- sidebarLayout(
  sidebarPanel(
    selectInput("commod_export", "สินค้า", choices = commod_export),
    br(),
    selectInput("subcommod_export", "สินค้าย่อย", choices = NULL),
    br(),
    radioButtons("impexp_export", "การค้า", c("ส่งออก" = "ส่งออก", "นำเข้า" = "นำเข้า")),
    br(),
    radioButtons("variable_export", "ตัวแปร", c("ปริมาณ" = "ปริมาณ", "มูลค่า" = "มูลค่า")),
    width = 2
  ),
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("ภาพรวม",
               echarts4rOutput("plot_trade", height = "400px"),
               reactableOutput("tbl_trade")
              ),
      tabPanel("ข้อมูลดิบ", )
    ),
    width = 10
  )
)



tab_price <- tabPanel(
  "ราคา",
  sidebar_price
)



tab_export <- tabPanel(
  "การส่งออก",
  sidebar_export
)



navbarPage(
  "ราคาและการส่งออกสินค้าเกษตร",
  collapsible = TRUE,
  id = "nav",
  tab_price,
  tab_export
)