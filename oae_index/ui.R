fluidPage(
  sidebarPanel(
    h3("ตัวเลือก"),
    br(),
    selectizeInput("province", "ภาค/จังหวัด", choices = province_names),
    br(),
    selectInput("year_min", "ปีเริ่มต้น", choices = year_min:year_max, year_min),
    br(),
    selectInput("year_max", "ปีสุดท้าย", choices = year_min:year_max, year_max),
    width = 2
  ),
  mainPanel(
    h2("ดัชนีเศรษฐกิจการเกษตร"),
    tabsetPanel(
      type = "tabs",
      tabPanel("รายปี", DTOutput('tbl_y')),
      tabPanel("รายไตรมาส", DTOutput('tbl_q')),
      tabPanel("รายเดือน", DTOutput('tbl_m'))
    ),
    width = 10
  )
)