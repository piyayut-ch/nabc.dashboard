sidebar_index <- sidebarLayout(
  sidebarPanel(
    h3("ตัวเลือก"),
    br(),
    radioButtons("index_type", "ประเภทดัชนี:", inline = TRUE,
                 choices = c("ผลผลิต" = "ci", "ราคา" = "ci_price", "รายได้" = "fi"), 
                 selected = "ci"),
    selectizeInput("province", "ภาค/จังหวัด:", choices = province_names, selected = "เฉลี่ยประเทศ", multiple = TRUE),
    selectInput("year_min", "ตั้งแต่:", choices = year_min:year_max, selected = year_min),
    selectInput("year_max", "ถึง:", choices = year_min:year_max, selected = year_max),
#     actionButton("submit", "Submit", 
#                  style="display: inline-block; margin: 0 auto; width: 100%; background-color: #000753; color: white"),
    br(),
    tags$b("Download:"),
    p(
      downloadButton("download_excel", "excel", 
                     style="display: inline-block; margin: auto; width: 100px; color: #000753;"),
      downloadButton("download_csv", "csv", 
                     style="display: inline-block; margin: auto; width: 100px; color: #000753;")
    ),
    width = 2
  ),
  mainPanel(
    h2("ดัชนีเศรษฐกิจการเกษตร"),
    tabsetPanel(
      id = "oae_index",
      type = "tabs",
      tabPanel("รายปี", reactableOutput("tbl_y"), value = 1),
      tabPanel("รายไตรมาส", reactableOutput("tbl_q"), value = 2),
      tabPanel("รายเดือน", reactableOutput("tbl_m"), value = 3)
    ),
    width = 10
  )
)

sidebar_situation <- sidebarLayout(
  sidebarPanel(
    h3("ตัวเลือก"),
    br(),
    selectizeInput("product", "สินค้า:", choices = prod_names),
    selectInput("year_calendar", "ปีปฏิทิน:", choices = 2564:2555, selected = 2564),
    width = 2
  ),
  mainPanel(
    width = 10,
    h2("ภาวะเศรษฐกิจการเกษตรระดับจังหวัด"),
    fluidRow(
      column(4, h4("แนวโน้มผลผลิตเมื่อเทียบกับค่าเฉลี่ย 10 ปี"), leafletOutput("map_production", height=500)),
      column(4, h4("แนวโน้มราคาเมื่อเทียบกับค่าเฉลี่ย 10 ปี"), leafletOutput("map_price", height=500)),
      column(4, h4("แนวโน้มรายได้เมื่อเทียบกับค่าเฉลี่ย 10 ปี"), leafletOutput("map_farm_income", height=500))
    ),
    h3("บทวิเคราะห์:"),
    tags$p("ปริมาณผลผลิตในปี .... เพิ่มขึ้น ร้อยละ ... เนื่องจาก ... ราคาที่เกษตรกรขายได้ ณ ไร่ นา ในปี ... เพิ่มขึ้น ร้อยละ ... เนื่องจาก .... รายได้เกษตรกร ... เพิ่มขึ้น ร้อยละ ... เนื่องจาก ...")
  )
)


tab_index <- tabPanel(
  "ดัชนีเศรษฐกิจการเกษตร",
  sidebar_index
)



tab_situation <- tabPanel(
  "ภาวะเศรษฐกิจการเกษตร",
  sidebar_situation
)



navbarPage(
  "ระบบติดตามภาวะเศรษฐกิจการเกษตรระดับจังหวัด",
  collapsible = TRUE,
  id = "nav",
  tab_index,
  tab_situation
)