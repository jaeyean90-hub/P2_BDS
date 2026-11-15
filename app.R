# [0] Setting ----

library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(maps)
source("engine.R")
source("our_shoes.R")

# [1] TAB2: Explain PCA ----
ve <- pca$sdev^2 / sum(pca$sdev^2)
cv <- cumsum(ve)
k80 <- which(cv >= 0.80)[1]
k90 <- which(cv >= 0.90)[1]
k95 <- which(cv >= 0.95)[1]

cat_f    <- as.factor(meta$category)
cat_cols <- adjustcolor(seq_len(nlevels(cat_f)), 0.5)

# 음수 값이 있는 그림(Step 2, 4)용: 0을 기준으로 대칭 색
show_diff <- function(v) {
  m <- max(abs(v))
  image(matrix(v, 64, 64)[, 64:1], col = hcl.colors(101, "Blue-Red 3"),
        zlim = c(-m, m), axes = FALSE, asp = 1)
}

# 정사각형 + 밝기 고정(0~255)
show_shoe_fixed <- function(x, zlim = c(0, 255)) {
  x <- pmin(pmax(x, zlim[1]), zlim[2])
  image(matrix(x, 64, 64)[, 64:1], col = gray(0:255 / 255),
        zlim = zlim, axes = FALSE, asp = 1)
}

# K=1에서도 에러가 나지 않는 복원 함수
rebuild_safe <- function(z, K) {
  V <- pca$rotation[, 1:K, drop = FALSE]
  as.numeric(pca$center + V %*% z[1:K])
}

# [1] TAB3: Our Shoes ----
our_photos <- load_our_shoes()
register_our_shoes_resources()


# [1] TAB4: Data 1 ----
# 데이터 불러오기
df <- read.csv(
  "water_temp.csv",
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

# 연도와 월 분리
df <- df %>%
  mutate(
    연도 = ym %/% 100,
    월 = ym %% 100
  )

# 전국 17개 시도 대표 위도·경도 좌표 테이블
korea_coords <- data.frame(
  시도 = c(
    "서울특별시", "부산광역시", "대구광역시", "인천광역시", "광주광역시",
    "대전광역시", "울산광역시", "세종특별시", "경기도", "강원도",
    "충청북도", "충청남도", "전라북도", "전라남도", "경상북도",
    "경상남도", "제주특별자치도"
  ),
  단축명 = c(
    "서울", "부산", "대구", "인천", "광주",
    "대전", "울산", "세종", "경기", "강원",
    "충북", "충남", "전북", "전남", "경북",
    "경남", "제주"
  ),
  lat = c(
    37.5665, 35.1796, 35.8714, 37.4563, 35.1595,
    36.3504, 35.5384, 36.4800, 37.2750, 37.8228,
    36.6357, 36.6588, 35.8202, 34.8161, 36.5760,
    35.2383, 33.4996
  ),
  lon = c(
    126.9780, 129.0756, 128.6014, 126.7052, 126.8526,
    127.3845, 129.3114, 127.2890, 127.0095, 128.1555,
    127.4912, 126.6728, 127.1088, 126.4629, 128.5056,
    128.6922, 126.5312
  ),
  stringsAsFactors = FALSE
)

# [2] UI ----

ui <- page_navbar(
  title = "빅데시",
  fillable = FALSE,
  # Tab1: Shoe Search 
  nav_panel("Shoe Search",
            layout_columns(
              col_widths = c(6, 6),
              
              # ── 왼쪽: 내가 넣은 것 ──
              card(
                card_header("Your photo", style = "font-size: 20px; font-weight: 700;"),
                radioGroupButtons("src", span("STEP1: How would you like to choose a photo?",
                                              style = "font-size: 18px; font-weight: 600;"),
                                  choices = c("Catalogue", "Self-check", "Upload"),
                                  selected = character(0), justified = TRUE),
                conditionalPanel("input.src == 'Catalogue'",
                                 numericInput("cat_id", "Type a catalogue id", value = 1, min = 1, max = 2000)),
                conditionalPanel("input.src == 'Self-check'",
                                 selectInput("sc_file", "Select a self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
                conditionalPanel("input.src == 'Upload'",
                                 fileInput("photo", "Upload a photo", accept = c(".jpg", ".jpeg", ".png"))),
                numericInput("K", span("STEP2: Choose the number of PCs (K)",
                                       style = "font-size: 18px; font-weight: 600;"), value = 100, min = 0, max = 300, step = 50),
                sliderInput("K_slider", NULL, min = 0, max = 300, value = 100),
                layout_columns(
                  col_widths = c(6, 6),
                  div(h5("Your Photo"), plotOutput("ss_query", width = "100%", height = "auto")),
                  div(h5("Rebuilt with K PCs"), plotOutput("ss_rebuild", width = "100%", height = "auto"))
                )
              ),
              
              # ── 오른쪽: 찾아낸 것 ──
              card(
                card_header("Search result", style = "font-size: 20px; font-weight: 700;"),
                h5("Top 5"),
                tableOutput("ss_top5"),
                uiOutput("ss_pred"),
                layout_columns(
                  col_widths = c(6, 6),
                  div(h5("Closest catalogue photo"), plotOutput("ss_best", width = "100%", height = "auto")),
                  div(h5("Average shoe"),            plotOutput("ss_mean", width = "100%", height = "auto"))
                )
              )
            )
  ),
  # TAB2: Explain PCA
  nav_panel("Explain PCA",
            p("For the photo chosen in Shoe Search, these steps show how PCA turns a shoe into a few numbers and uses them to find the match."),
            
            h4("Step 1. Average shoe"),
            p("The average of all 2,000 catalogue shoes. Darker means more shoes have a dark pixel there."),
            plotOutput("p1", height = 280, width = 280),
            
            h4("Step 2. Photo − average"),
            p("Your photo minus the average shoe. Blue = darker than the average, red = brighter. PCA starts from this difference."),
            plotOutput("p2", height = 280, width = 280),
            
            h4("Step 3. Score map"),
            p("Every catalogue shoe placed by its PC1 and PC2 scores. Shoes of the same category tend to sit together, but some categories (e.g. Sandals and Slippers) overlap in these two PCs. Star = your photo, circles = its Top 5."),
            plotOutput("p3", height = 420),
            
            h4("Step 4. Eigenshoes"),
            p("Each PC is a pattern of change away from the average shoe. Blue and red parts change in opposite directions. The title shows the share of variance the PC explains."),
            plotOutput("p4", height = 360),
            p(strong("What each PC captures (our reading):")),
            tags$ul(
              tags$li("PC1 (30.3%): appears to capture the overall size of the shoe body."),
              tags$li("PC2 (15.7%): appears to track the ankle/upper part, i.e. high-top boots vs low shoes."),
              tags$li("PC3 (7.0%): appears to contrast the upper part with the sole area."),
              tags$li("PC4 and later: finer details such as toe, heel and outlines, explaining less and less variance.")
            ),
            
            h4("Step 5. Scores"),
            p("The first K PC scores of your photo (the chart shows up to the first 50; the table lists all K, up to 300). These K numbers replace the 4,096 pixels when we measure distance to other shoes."),
            plotOutput("p5", height = 260),
            div(style = "max-height:300px; overflow-y:auto;", tableOutput("t5")),
            
            h4("Step 6. Rebuild"),
            p("The photo rebuilt from the average shoe plus the first K eigenshoes. Larger K gives a sharper copy, but gains shrink after a while."),
            p(em("Top: the original photo vs the copy rebuilt with the K chosen in the slider.")),
            plotOutput("p6k", height = 300, width = 600),
            p(em("Bottom: fixed K values (1, 5, and the K needed to keep 80%, 90% and 95% of the variance).")),
            plotOutput("p6", height = 240),
            
            h4("More: cumulative explained variance"),
            p("How many PCs are needed to keep 80%, 90% and 95% of the information. The red line is the chosen K."),
            plotOutput("p7", height = 360),
            p(strong("Why we added this:"),
              sprintf(" It shows that about %d PCs keep 90%% of the information, so we do not need all 4,096 pixels. It also helps us choose K: a small K is fast, but too small loses detail.", k90))),
  # TAB3: Our Shoes
  nav_panel("Our Shoes", our_shoes_ui("os", our_photos)),
  # TAB4: Data 1
  nav_panel("Data 1",
            br(),
            sidebarLayout(
              
              # 사이드바: 데이터 필터
              sidebarPanel(
                width = 3,
                h4("Data Filter Settings (데이터 필터 설정)"),
                hr(),
                
                # 지역 선택
                selectInput(
                  inputId = "region_filter",
                  label = "Region (지역 선택) (시도)",
                  choices = sort(unique(df$시도)),
                  selected = "서울특별시",
                  multiple = TRUE
                ),
                
                # 연도 범위 선택
                sliderInput(
                  "year_range",
                  "Year (연도 선택)",
                  min = min(df$연도, na.rm = TRUE),
                  max = max(df$연도, na.rm = TRUE),
                  value = range(df$연도, na.rm = TRUE),
                  step = 1,
                  sep = ""
                ),
                
                # 평균기온 범위 선택
                sliderInput(
                  "temp_range",
                  "Average Temperature (평균기온 범위) (°C)",
                  min = floor(min(df$평균기온, na.rm = TRUE)),
                  max = ceiling(max(df$평균기온, na.rm = TRUE)),
                  value = range(df$평균기온, na.rm = TRUE),
                  step = 0.5
                )
              ),
              
              # 메인 패널: 계절, 지역, 폭염 탭
              mainPanel(
                width = 9,
                
                # 필터링 개수 출력
                wellPanel(
                  style = "padding: 8px 15px; margin-bottom: 15px;",
                  textOutput("filter_n")
                ),
                
                tabsetPanel(
                  
                  # --- 계절별 차이 ---
                  tabPanel(
                    "Season 계절",
                    br(),
                    h3("Seasonal Differences in Bottled Water Sales"),
                    hr(),
                    fluidRow(
                      column(6,
                             h4("Descriptive Statistical Analysis Results"),
                             tableOutput("finding_A")
                      ),
                      column(6,
                             h4("계절별 평균 생수 매출 비교"),
                             plotOutput("finding_A_plot", height = "350px")
                      )
                    ),
                    hr(),
                    wellPanel(
                      style = "background-color: #f8f9fa; border-left: 5px solid #E69F00;",
                      h4(style = "margin-top: 0; font-weight: bold;", "계절 기준 통계량 결과"),
                      tags$ol(
                        tags$li(tags$b("답을 찾고자 하는 질문 (The question): "), "여름철(6~8월)과 그 외 기간(9~5월) 사이에 생수 매출액과 전체 매출 대비 생수비중은 유의미한 차이를 보이는가?"),
                        tags$li(tags$b("해당 결과를 보여주는 입력 조건 (The inputs): "), "매출 결측치를 제외한 유효 관측치(전체 기준 593개)를 대상으로 사이드바의 지역·연도·기온 필터를 적용한 뒤, 월(Month) 변수를 기준으로 '여름철(6~8월)'과 '그 외 기간'으로 그룹화하여 비교."),
                        tags$li(tags$b("숫자로 나타낸 결과 (The result in numbers): "), uiOutput("finding_A_numbers", inline = TRUE)),
                        tags$li(tags$b("해당 결과가 중요한 이유 (Why it matters): "), "여름철에 절대적인 생수 매출 규모는 약 11% 증가하지만, 전체 매출 내 생수비중(약 1.74%)은 거의 변하지 않습니다. 이는 여름철에 생수만 단독으로 증가하는 것이 아니라 전체 음료·편의점 총매출이 함께 상승함을 의미합니다."),
                        tags$li(tags$b("다음 분석에 주는 도움 (How this finding helps): "), "단순 달력 기준의 '월(6~8월)' 구분만으로는 생수비중의 급등 구간을 포착하기 어려우므로, 다음 프로젝트에서는 단순 계절이 아닌 '지역별 특성(지역 탭)' 및 '폭염·고온 같은 극단적 기상 조건(폭염 탭)'으로 분석 초점을 좁혀야 함을 시사합니다.")
                      )
                    )
                  ),
                  
                  # --- 지역별 차이 (지도 히트맵 포함) ---
                  tabPanel(
                    "Region 지역",
                    br(),
                    h3("Regional Differences in Bottled Water Sales and Sales Proportion 
                "),
                    hr(),
                    fluidRow(
                      column(6,
                             h4("지역별 그룹 요약표 (생수비중 내림차순)"),
                             tableOutput("finding_B")
                      ),
                      column(6,
                             h4("대한민국 지도 히트맵 (색상: 생수비중 / 크기: 평균매출)"),
                             plotOutput("finding_B_map", height = "480px"),
                             hr(),
                             h4("지역별 생수비중 순위 막대그래프"),
                             plotOutput("finding_B_plot", height = "380px")
                      )
                    ),
                    hr(),
                    wellPanel(
                      style = "background-color: #f8f9fa; border-left: 5px solid #2B5C8F;",
                      h4(style = "margin-top: 0; font-weight: bold;", "지역 기준 통계량 결과"),
                      tags$ol(
                        tags$li(tags$b("답을 찾고자 하는 질문 (The question): "), "전국 시도별로 생수 매출 규모와 총매출 대비 생수비중에는 어떤 지역적 격차가 존재하는가?"),
                        tags$li(tags$b("해당 결과를 보여주는 입력 조건 (The inputs): "), "매출 결측치를 제외한 데이터를 '시도(Region)' 단위로 그룹화하고, 평균 생수비중을 기준으로 내림차순 정렬 및 지도 좌표와 매핑(사이드바 필터 연동)."),
                        tags$li(tags$b("숫자로 나타낸 결과 (The result in numbers): "), uiOutput("finding_B_numbers", inline = TRUE)),
                        tags$li(tags$b("해당 결과가 중요한 이유 (Why it matters): "), "지도 히트맵에서 확인되듯 매장 수와 인구가 많은 수도권(경기·서울)은 원의 크기(총 매출액)는 크지만, 색상 농도(생수비중)는 전남·충남·전북·제주 등 비수도권 및 관광·도서 지역이 최대 2.3배 이상 진하게 나타났습니다."),
                        tags$li(tags$b("다음 분석에 주는 도움 (How this finding helps): "), "전국 데이터를 단일 기준으로 모델링할 경우 수도권의 큰 매출 규모에 편향될 수 있으므로, 이후 회귀분석이나 수요 예측 시 '지역(시도)' 또는 '매장 수당 매출' 변수를 반드시 통제 변수로 포함해야 함을 알려줍니다.")
                      )
                    )
                  ),
                  
                  # --- 폭염 여부별 차이 ---
                  tabPanel(
                    "Heatwave 폭염",
                    br(),
                    h3("Differences in Bottled Water Sales by Heatwave Occurrence 
                 "),
                    hr(),
                    wellPanel(
                      style = "background-color: #fcf8e3; border-color: #faebcc;",
                      h4(style = "margin-top: 0; color: #8a6d3b; font-weight: bold;", "평균 생수 매출 차이 요약"),
                      htmlOutput("finding_C_diff_text")
                    ),
                    fluidRow(
                      column(6,
                             h4("기술통계 분석 결과"),
                             tableOutput("finding_C")
                      ),
                      column(6,
                             h4("폭염 여부별 평균 생수 매출 비교"),
                             plotOutput("finding_C_plot", height = "350px")
                      )
                    ),
                    hr(),
                    wellPanel(
                      style = "background-color: #f8f9fa; border-left: 5px solid #D95F02;",
                      h4(style = "margin-top: 0; font-weight: bold;", "폭염 발생 기준 통계량 결과"),
                      tags$ol(
                        tags$li(tags$b("답을 찾고자 하는 질문 (The question): "), "폭염일수가 하루라도 발생한 기간(폭염일수 > 0)과 전혀 발생하지 않은 기간(폭염일수 = 0) 사이에 생수 매출은 얼마나 차이가 나는가?"),
                        tags$li(tags$b("해당 결과를 보여주는 입력 조건 (The inputs): "), "매출 유효 관측치를 '폭염일수 > 0 (폭염 발생)'과 '폭염일수 == 0 (폭염 미발생)' 두 그룹으로 이분화하여 평균기온, 평균·중앙 생수매출, 생수비중을 산출."),
                        tags$li(tags$b("숫자로 나타낸 결과 (The result in numbers): "), uiOutput("finding_C_numbers", inline = TRUE)),
                        tags$li(tags$b("해당 결과가 중요한 이유 (Why it matters): "), "단순히 여름철(6~8월)로 구분했을 때(약 11.1% 증가)보다 '폭염 발생 여부'로 구분했을 때 매출 증가폭(약 14.9% 증가)이 더 크게 나타났습니다. 즉, 달력상의 계절보다 실제 체감하는 기온(폭염)이 생수 구매를 직접적으로 상승시키는 요인임을 보여줍니다."),
                        tags$li(tags$b("다음 분석에 주는 도움 (How this finding helps): "), "폭염 발생 여부가 생수 매출 상승의 중요 설명 변수임이 확인되었으므로, 이후 프로젝트에서는 단순 평균기온 외에 '폭염일수', '최고기온평균', '평균습도' 등 기상 파생변수를 활용한 다중회귀 및 머신러닝 매출 예측 모델로 확장할 수 있습니다.")
                      )
                    )
                  )
                  
                ) # tabsetPanel (계절, 지역, 폭염)
              ) # mainPanel
            ) # sidebarLayout)
  ),
  # TAB5: AI
  nav_panel("AI",
            h4("Ethics statement"),
            p("We understand the rules on data and AI use for this project, including following the license and privacy rules",
              "of every dataset we used, understanding all of our code, and knowing that presenting AI output as our own work may be",
              "academic misconduct. We certify that we have followed all of them. Any violation is our responsibility,",
              "and we agree to receive an F in this course if we have not."),
            p("Signed: Eunseong Kim, Jeayeun Park, Yikyeong Yu, Nakyeong Lee, Boyoon Lee"),
            h4("What AI did and what we did"),
            tableOutput("ai_table"),
            h4("Reflection"),
            tableOutput("ai_reflection")
  )
)


# [3] Server ----


server <- function(input, output, session) {
  # Tab1: Shoe Search
  # [1] Best Match
  # 복원: 사용자가 K_slider값을 바꿨을 때 K를 씽크해주기
  observeEvent(input$K_slider, {
    if (!identical(input$K, input$K_slider)) {
      updateNumericInput(session, "K", value = input$K_slider)
    }
  })
  
  # 복원: 사용자가 K값을 바꿨을 때 K_slider를 씽크해주기
  observeEvent(input$K, {
    if (!identical(input$K, input$K_slider)) {
      updateSliderInput(session, "K_slider", value = input$K)
    }
  })
  
  # 선택된 사진의 정보 모으기
  query <- reactive({
    req(input$src)
    # case1. 카탈로그 사진
    if (input$src == "Catalogue") {
      id <- input$cat_id
      validate(need(!is.na(id) && id >= 1 && id <= 2000,
                    "Catalogue id는 1부터 2000 사이로 입력해주세요"))
      list(x = read_shoe(sprintf("shoes/%04d.png", id)),
           zq = pca$x[id, ],
           exclude = id,
           answer = meta$category[id])
    }
    # case2. self-check 사진
    else if (input$src == "Self-check") {
      path <- file.path("selfcheck", input$sc_file)
      list(x = read_shoe(path),
           zq = photo_score(path),
           exclude = NULL,
           answer = selfcheck_labels$category[selfcheck_labels$photo == input$sc_file])
    }
    # case3. 업로드한 사진
    else {
      req(input$photo)
      path <- input$photo$datapath
      list(x = read_shoe(path),
           zq = photo_score(path),
           exclude = NULL,
           answer = NA)
    }
  })
  
  output$ss_query <- renderPlot({
    show_shoe(query()$x)
  }, height = function() session$clientData$output_ss_query_width)
  
  output$ss_rebuild <- renderPlot({
    validate(need(!is.na(input$K) && input$K >= 1 && input$K <= 300,
                  "K는 1부터 300 사이로 입력해주세요"))
    show_shoe(rebuild(query()$zq, input$K))
  },height = function() session$clientData$output_ss_rebuild_width)
  
  res <- reactive({
    validate(need(!is.na(input$K) && input$K >= 1 && input$K <= 300,
                  "K는 1부터 300 사이로 입력해주세요"))
    search_shoe(query()$zq, input$K, exclude_id = query()$exclude)
  })
  
  output$ss_mean <- renderPlot({
    show_shoe(pca$center)
  }, height = function() session$clientData$output_ss_mean_width)
  
  # [2] Top5
  output$ss_top5 <- renderTable({
    data.frame(
      Rank = 1:5,
      id = res()$top5,
      Category = res()$category,
      Distance = res()$dist
    )
  })
  
  output$ss_best <- renderPlot({
    show_shoe(read_shoe(sprintf("shoes/%04d.png", res()$top5[1])))
  },height = function() session$clientData$output_ss_best_width)
  
  # [3] Prediction
  output$ss_pred <- renderUI({
    if (is.na(query()$answer)) {
      pred <- res()$pred
      msg <- paste("Predicted category of the shoe: ", pred)
      color <- "#1A4FA0"
      bg <- "#EAF2FF"
    }
    else {
      pred <- res()$pred
      ans <- query()$answer
      line1 <- paste("Predicted category of the shoe: ", pred)
      line2 <- paste("And, the actual category: ", ans)
      if (pred == ans) {
        line3 <- "So the prediction is CORRECT!"
        color <- "#1E7B34"
        bg <- "#E8F5EC"
      }
      else {
        line3 <- "So the prediction is WRONG!"
        color <- "#B42318"
        bg <- "#FDECEA"
      }
      msg <- paste(line1, line2, line3, sep="<br>")
    }
    div(HTML(msg),
        style = paste0("font-size: 20px; font-weight: 600; padding: 12px 16px; border-radius: 8px;",
                       "color: ", color, "; background-color: ", bg, "; border: 2px solid ", color, ";"))
  })
  
  # TAB2: Explain PCA
  x <- reactive(query()$x)
  z <- reactive(query()$zq)
  K <- reactive({ req(input$K >= 1); min(input$K, 300) })
  
  output$p1 <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    show_shoe_fixed(pca$center)
    title("Average shoe")
  })
  
  output$p2 <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    show_diff(x() - pca$center)
    title("Photo - average")
  })
  
  output$p3 <- renderPlot({
    t5 <- res()$top5
    plot(pca$x[, 1], pca$x[, 2], col = cat_cols[cat_f], pch = 16,
         xlab = "PC1", ylab = "PC2", main = "Score map")
    points(pca$x[t5, 1], pca$x[t5, 2], pch = 1, cex = 2.2, lwd = 2)  # Top 5
    points(z()[1], z()[2], pch = 8, cex = 2.5, lwd = 3)              # 내 사진
    legend("topright", legend = c(levels(cat_f), "Your photo", "Top 5"),
           col = c(cat_cols, "black", "black"),
           pch = c(rep(16, nlevels(cat_f)), 8, 1))
  })
  
  output$p4 <- renderPlot({
    par(mfrow = c(2, 4), mar = c(1, 1, 3, 1))
    for (k in 1:8) {
      show_diff(pca$rotation[, k])
      title(sprintf("PC%d (%.1f%%)", k, 100 * ve[k]))
    }
  })
  
  output$p5 <- renderPlot({
    n <- min(K(), 50)   # 막대가 너무 많으면 읽기 어려워 최대 50개
    barplot(z()[1:n], names.arg = 1:n, xlab = "PC number", ylab = "PC score",
            main = sprintf("First %d PC scores of the photo", n),
            col = "grey60", border = NA)
    abline(h = 0)
  })
  
  output$t5 <- renderTable({
    n <- min(K(), length(z()))
    data.frame(`PC number` = paste0("PC", 1:n),
               `PC score`  = round(z()[1:n], 2),
               check.names = FALSE)
  })
  
  output$p6k <- renderPlot({
    par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
    show_shoe_fixed(x())
    title("Original photo")
    show_shoe_fixed(rebuild_safe(z(), K()))
    title(paste("Rebuilt with K =", K()))
  })
  
  output$p6 <- renderPlot({
    ks <- sort(unique(c(1, 5, k80, k90, k95)))
    par(mfrow = c(1, length(ks)), mar = c(1, 1, 3, 1))
    for (k in ks) {
      show_shoe_fixed(rebuild_safe(z(), k))
      title(paste("K =", k))
    }
  })
  
  output$p7 <- renderPlot({
    n <- min(300, length(cv))
    plot(cv[1:n], type = "l", xlab = "K", ylab = "Cumulative variance",
         main = "Cumulative explained variance")
    abline(h = c(0.8, 0.9, 0.95), lty = 2)
    abline(v = K(), col = "red", lwd = 2)
  })
  
  # TAB3: Our Shoes
  our_shoes_server("os", our_photos)
  
  # TAB4: Data 1
  filtered_data <- reactive({
    req(input$region_filter)
    req(input$year_range)
    req(input$temp_range)
    
    df %>%
      filter(
        시도 %in% input$region_filter,
        연도 >= input$year_range[1],
        연도 <= input$year_range[2],
        평균기온 >= input$temp_range[1],
        평균기온 <= input$temp_range[2]
      )
  })
  
  # 매출 결측치(206개)를 제외한 분석용 데이터
  sales_data <- reactive({
    filtered_data() %>%
      filter(
        !is.na(생수매출),
        !is.na(생수비중)
      )
  })
  
  # 필터링 결과 관측치 수 표시 (한글)
  output$filter_n <- renderText({
    paste0(
      "분석 가능 관측치 수 (매출 결측치 제외): ",
      nrow(sales_data()),
      "개 / 전체 선택된 관측치 수: ",
      nrow(filtered_data()),
      "개"
    )
  })
  
  # ----------------------------------------------------------------------------
  # 계절별 비교
  # ----------------------------------------------------------------------------
  finding_A_df <- reactive({
    sales_data() %>%
      mutate(
        기간 = if_else(월 %in% 6:8, "여름철 (6~8월)", "그 외 기간")
      ) %>%
      group_by(기간) %>%
      summarise(
        관측치수 = n(),
        평균생수매출 = round(mean(생수매출), 0),
        중앙생수매출 = round(median(생수매출), 0),
        평균생수비중 = round(mean(생수비중), 3),
        .groups = "drop"
      )
  })
  
  output$finding_A <- renderTable({
    finding_A_df()
  })
  
  output$finding_A_numbers <- renderUI({
    data_a <- finding_A_df()
    summer <- data_a %>% filter(기간 == "여름철 (6~8월)")
    other <- data_a %>% filter(기간 == "그 외 기간")
    
    if (nrow(summer) > 0 && nrow(other) > 0) {
      diff_amt <- summer$평균생수매출 - other$평균생수매출
      diff_pct <- round((diff_amt / other$평균생수매출) * 100, 1)
      HTML(paste0(
        "여름철(n=", summer$관측치수, ") 평균 생수매출은 <b>", format(summer$평균생수매출, big.mark = ","), "원</b>(중앙값 ", format(summer$중앙생수매출, big.mark = ","), "원, 생수비중 ", summer$평균생수비중, "%)으로, ",
        "그 외 기간(n=", other$관측치수, ") 평균 <b>", format(other$평균생수매출, big.mark = ","), "원</b>(중앙값 ", format(other$중앙생수매출, big.mark = ","), "원, 생수비중 ", other$평균생수비중, "%) 대비 ",
        "<b>", format(diff_amt, big.mark = ","), "원(", diff_pct, "%)</b> 더 높게 나타남."
      ))
    } else {
      HTML("현재 필터 조건에서는 두 계절 그룹을 모두 비교할 수 없습니다.")
    }
  })
  
  output$finding_A_plot <- renderPlot({
    data_a <- finding_A_df()
    req(nrow(data_a) > 0)
    
    min_val <- min(data_a$평균생수매출)
    max_val <- max(data_a$평균생수매출)
    diff_val <- max(1, max_val - min_val)
    
    y_min <- max(0, min_val - diff_val * 0.3)
    y_max <- max_val + diff_val * 0.2
    
    ggplot(data_a, aes(x = 기간, y = 평균생수매출, fill = 기간)) +
      geom_col(width = 0.4, show.legend = FALSE) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      scale_fill_manual(values = c("여름철 (6~8월)" = "#E69F00", "그 외 기간" = "#56B4E9")) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Mean Bottled Water Sales by Season",
        x = "",
        y = "Mean Sales (KRW)"
      ) +
      theme_minimal(base_family = "sans") +
      theme(text = element_text(size = 12))
  })
  
  # ----------------------------------------------------------------------------
  # 지역별 비교 & 지도 히트맵
  # ----------------------------------------------------------------------------
  finding_B_df <- reactive({
    sales_data() %>%
      group_by(시도) %>%
      summarise(
        관측치수 = n(),
        평균생수매출 = round(mean(생수매출), 0),
        중앙생수매출 = round(median(생수매출), 0),
        평균생수비중 = round(mean(생수비중), 3),
        .groups = "drop"
      ) %>%
      arrange(desc(평균생수비중))
  })
  
  output$finding_B <- renderTable({
    finding_B_df()
  })
  
  # 대한민국 지도 히트맵 출력
  output$finding_B_map <- renderPlot({
    data_b <- finding_B_df() %>%
      inner_join(korea_coords, by = "시도")
    req(nrow(data_b) > 0)
    
    korea_map <- map_data("world", region = "South Korea")
    
    ggplot() +
      # 1. 대한민국 영토 배경 지도
      geom_polygon(
        data = korea_map,
        aes(x = long, y = lat, group = group),
        fill = "#F2F4F7",
        color = "#B0BAC5",
        linewidth = 0.5
      ) +
      # 2. 히트맵 열기 효과 (부드러운 후광 레이어)
      geom_point(
        data = data_b,
        aes(x = lon, y = lat, color = 평균생수비중),
        size = 18,
        alpha = 0.25,
        show.legend = FALSE
      ) +
      # 3. 지역별 핵심 데이터 포인트 (색상 = 생수비중, 크기 = 평균생수매출)
      geom_point(
        data = data_b,
        aes(x = lon, y = lat, fill = 평균생수비중, size = 평균생수매출),
        shape = 21,
        color = "white",
        stroke = 1.2,
        alpha = 0.92
      ) +
      # 4. 지역명 및 생수비중(%) 라벨 표시
      geom_text(
        data = data_b,
        aes(x = lon, y = lat, label = paste0(단축명, "\n", 평균생수비중, "%")),
        size = 3.2,
        fontface = "bold",
        color = "#1D2939",
        vjust = -1.1
      ) +
      scale_fill_gradientn(
        colors = c("#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026"),
        name = "평균 생수비중 (%)"
      ) +
      scale_color_gradientn(
        colors = c("#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
      ) +
      scale_size_continuous(
        range = c(5, 16),
        labels = scales::comma,
        name = "평균 생수매출 (원)"
      ) +
      coord_fixed(ratio = 1.2, xlim = c(125.8, 129.8), ylim = c(33.1, 38.6)) +
      labs(
        title = "South Korea Bottled Water Heatmap",
        subtitle = "Color = Mean Share (%) | Bubble Size = Mean Sales (KRW)"
      ) +
      theme_void(base_family = "sans") +
      theme(
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, color = "#475467", hjust = 0.5, margin = margin(b = 8)),
        legend.position = "right",
        legend.title = element_text(size = 9.5, face = "bold"),
        legend.text = element_text(size = 8.5),
        panel.background = element_rect(fill = "#EBF3FA", color = NA)
      )
  })
  
  output$finding_B_numbers <- renderUI({
    data_b <- finding_B_df()
    if (nrow(data_b) >= 2) {
      top_share <- data_b[1, ]
      low_share <- data_b[nrow(data_b), ]
      top_sales <- data_b %>% arrange(desc(평균생수매출)) %>% slice(1)
      
      HTML(paste0(
        "평균 생수비중 1위는 <b>", top_share$시도, "(", top_share$평균생수비중, "%)</b>, 최하위는 <b>", low_share$시도, "(", low_share$평균생수비중, "%)</b>로 약 ",
        round(top_share$평균생수비중 / low_share$평균생수비중, 2), "배 격차를 보임. ",
        "한편, 평균 생수매출액 규모 1위는 <b>", top_sales$시도, "(", format(top_sales$평균생수매출, big.mark = ","), "원)</b>로 매출 규모와 비중 순위가 상이함."
      ))
    } else if (nrow(data_b) == 1) {
      HTML(paste0("선택된 지역(", data_b$시도, ")의 평균 생수매출은 ", format(data_b$평균생수매출, big.mark = ","), "원, 평균 생수비중은 ", data_b$평균생수비중, "%입니다."))
    } else {
      HTML("표시할 지역 데이터가 없습니다.")
    }
  })
  
  output$finding_B_plot <- renderPlot({
    data_b <- finding_B_df()
    req(nrow(data_b) > 0)
    
    min_val <- min(data_b$평균생수비중)
    max_val <- max(data_b$평균생수비중)
    diff_val <- max(0.01, max_val - min_val)
    
    y_min <- max(0, min_val - diff_val * 0.15)
    y_max <- max_val + diff_val * 0.1
    
    ggplot(data_b, aes(x = reorder(시도, 평균생수비중), y = 평균생수비중)) +
      geom_col(fill = "#2B5C8F", width = 0.7) +
      coord_flip(ylim = c(y_min, y_max)) +
      labs(
        title = "Bottled Water Sales Share by Region",
        x = "Region",
        y = "Mean Share (%)"
      ) +
      theme_minimal(base_family = "sans") +
      theme(text = element_text(size = 11))
  })
  
  # ----------------------------------------------------------------------------
  # 폭염 여부별 데이터 및 차이 계산
  # ----------------------------------------------------------------------------
  finding_C_df <- reactive({
    sales_data() %>%
      mutate(
        폭염구분 = if_else(폭염일수 > 0, "폭염 발생", "폭염 미발생")
      ) %>%
      group_by(폭염구분) %>%
      summarise(
        관측치수 = n(),
        평균기온 = round(mean(평균기온), 2),
        평균생수매출 = round(mean(생수매출), 0),
        중앙생수매출 = round(median(생수매출), 0),
        평균생수비중 = round(mean(생수비중), 3),
        .groups = "drop"
      )
  })
  
  output$finding_C_diff_text <- renderUI({
    data_c <- finding_C_df()
    
    heatwave_sales <- data_c %>% filter(폭염구분 == "폭염 발생") %>% pull(평균생수매출)
    no_heatwave_sales <- data_c %>% filter(폭염구분 == "폭염 미발생") %>% pull(평균생수매출)
    
    if (length(heatwave_sales) > 0 && length(no_heatwave_sales) > 0) {
      diff_amount <- heatwave_sales - no_heatwave_sales
      diff_percent <- round((diff_amount / no_heatwave_sales) * 100, 1)
      
      HTML(paste0(
        "폭염 발생 기간의 평균 생수 매출은 폭염 미발생 기간보다 <b>",
        format(diff_amount, big.mark = ","), "원 (약 ", diff_percent, "% ", 
        if_else(diff_amount >= 0, "증가", "감소"), ")</b> 차이가 나타납니다.<br>",
        "<small>* 폭염 발생 평균: ", format(heatwave_sales, big.mark = ","), "원 | 폭염 미발생 평균: ", format(no_heatwave_sales, big.mark = ","), "원</small>"
      ))
    } else {
      HTML("필터링 조건 내에 폭염 발생 또는 미발생 그룹 데이터가 부족하여 차이를 계산할 수 없습니다.")
    }
  })
  
  output$finding_C_numbers <- renderUI({
    data_c <- finding_C_df()
    hw <- data_c %>% filter(폭염구분 == "폭염 발생")
    no_hw <- data_c %>% filter(폭염구분 == "폭염 미발생")
    
    if (nrow(hw) > 0 && nrow(no_hw) > 0) {
      diff_amt <- hw$평균생수매출 - no_hw$평균생수매출
      diff_pct <- round((diff_amt / no_hw$평균생수매출) * 100, 1)
      HTML(paste0(
        "폭염 발생 기간(n=", hw$관측치수, ", 평균기온 ", hw$평균기온, "°C)의 평균 생수매출은 <b>", format(hw$평균생수매출, big.mark = ","), "원</b>(생수비중 ", hw$평균생수비중, "%)이며, ",
        "폭염 미발생 기간(n=", no_hw$관측치수, ", 평균기온 ", no_hw$평균기온, "°C)은 <b>", format(no_hw$평균생수매출, big.mark = ","), "원</b>(생수비중 ", no_hw$평균생수비중, "%)으로 ",
        "폭염 발생 시 평균 <b>", format(diff_amt, big.mark = ","), "원(", diff_pct, "%)</b> 더 높게 나타남."
      ))
    } else {
      HTML("현재 필터 조건에서는 폭염 발생/미발생 두 그룹을 모두 비교할 수 없습니다.")
    }
  })
  
  output$finding_C <- renderTable({
    finding_C_df()
  })
  
  output$finding_C_plot <- renderPlot({
    data_c <- finding_C_df()
    req(nrow(data_c) > 0)
    
    min_val <- min(data_c$평균생수매출)
    max_val <- max(data_c$평균생수매출)
    diff_val <- max(1, max_val - min_val)
    
    y_min <- max(0, min_val - diff_val * 0.3)
    y_max <- max_val + diff_val * 0.2
    
    ggplot(data_c, aes(x = 폭염구분, y = 평균생수매출, fill = 폭염구분)) +
      geom_col(width = 0.4, show.legend = FALSE) +
      coord_cartesian(ylim = c(y_min, y_max)) +
      scale_fill_manual(values = c("폭염 발생" = "#D95F02", "폭염 미발생" = "#7570B3")) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Sales Comparison: Heatwave vs No Heatwave",
        x = "",
        y = "Mean Sales (KRW)"
      ) +
      theme_minimal(base_family = "sans") +
      theme(text = element_text(size = 12))
  })
  
  # TAB5: AI
  output$ai_table <- renderTable({
    data.frame(
      Member = c("Jeayeun Park",
                 "Yikyeong Yu, Boyoon Lee",
                 "Nakyeong Lee",
                 "Eunseong Kim"),
      Tab = c("Shoe Search", "Explain PCA", "Our Shoes", "Data 1"),
      `What AI did` = c(
        "Explained PCA and Shiny concepts when I got stuck, and translated the project description. Pointed out the causes of errors in my engine and Shiny code, and suggested a cleaned-up version of engine.R and some Shiny code (layout, square plots, the colored prediction box), which I reviewed and edited. Guided me through merging the tabs into one app.R and deploying it with GitHub and Posit Connect Cloud, and found two bugs during the merge (search_shoe failing at K = 1, and Our Shoes crashing when its CSV is missing). Also helped translate and polish the text in this tab.",
        "AI wrote the code for the Explain PCA tab. When we ran into errors, it helped debug errors. Also, it helped write and refine the explanatory sentences for each stage.",
        "AI helped write the initial code for the Our Shoes tab. It also provided guidance on connecting the tab to the existing PCA engine.",
        "Overall R coding, and providing answers on why those results are important and how they help with the next steps of the analysis. Helping me further study topics in which my knowledge is lacking. And translating this text."
      ),
      `What we did` = c(
        "Wrote the engine functions (read_shoe, show_shoe, photo_score, rebuild, search_shoe) step by step and debugged them, including excluding the query photo itself from the catalogue search. Designed the Shoe Search tab (two-step input, K box and slider kept in sync, result colors and wording) and wrote its code. Tested the search with the self-check photos (4/4 correct at K = 50) and my own photos, and studied why a very large K makes new photos worse. Merged all tabs into one app: connected Explain PCA to the photo and K chosen in Shoe Search, fixed the engine for K = 1, and deployed the app.",
        "We ran the AI's code and found errors (stretched image proportions, a missing parenthesis, and errors at large K). We also asked for the fixed test photo to be replaced so the plots update with the photo the user chooses, and for K to be limited to a maximum of 300. We changed x and z, which were fixed to the first self-check photo, into reactives that follow user input, and deleted K_default because the slider already sets K.",
        "I collected and organized shoe photos from our team members and assigned their actual categories. I reviewed the generated code, checked the assignment requirements, and prepared the image files and metadata for the app. I also worked on integrating the Our Shoes function with the team's PCA search functions.",
        "Deciding which variables to use for calculating descriptive statistics and creating visualizations (the questions I wanted to find answers to), selecting columns to use as data filters, deciding on the visualization plots, and revising the text/descriptions. Further studying the meaning of the AI-generated code and descriptive statistics."
      ),
      check.names = FALSE
    )
  })
  
  output$ai_reflection <- renderTable({
    data.frame(
      Member = c("Jeayeun Park", "Yikyeong Yu", "Boyoon Lee", "Nakyeong Lee", "Eunseong Kim"),
      Reflection = c(
        "I built the main PCA engine that runs the Shiny app (engine.R), and I ran PCA on the 2,000 catalogue photos and saved the result as an RDS file (pca.rds). For the tabs, I was in charge of the Shoe Search tab and completed it. I also collected the tabs made by each teammate, merged them into one app, and deployed it. Through Project 2, I learned how the flow of execution differs between R and R Shiny. In plain R, code usually runs in the order the user runs it, but in R Shiny the UI and the server work together interactively, and I remember that getting used to this was difficult. While building the Shoe Search tab and the engine, I also learned how an engine is actually driven through functions. One limitation of our app is that it compares the target photo with the catalogue photos pixel by pixel at exactly the same positions. I confirmed that when the shoe in an uploaded photo sits at different pixel positions from where shoes usually sit in the catalogue photos, the shoe search does not work well. Throughout the process, AI helped by laying out the overall structure of how the code should be written and by giving me problems to solve, so that I could write the code myself. However, when I was writing the engine code, AI did not catch a subtle but critical bug: when K = 1, the matrix turned into a vector and the calculation could not run. My teammates working on the other tabs covered for this. I learned that AI is helpful for writing code itself, but the user still needs to understand the code AI writes and be able to fix it actively.",
        " I worked on the Explain PCA tab with one other teammate. My task was to implement the app using the engine.R that another teammate had built. AI wrote the initial code for Steps 1–6, the cumulative variance graph, and the blue-red plots, and it helped me translate my writing into English. However, the generated code had problems when we ran it: the shoe proportions looked stretched, and a very high K caused an error, so we adjusted the aspect ratios and capped K at 300. The plots also used a fixed test photo at first, and we changed this after feedback from my teammate so they update with the user’s chosen image.
  Watching how PC scores and the top K eigenshoes rebuild a photo step by step helped me understand PCA much better than formulas alone. I also learned that AI can overlook details such as input limits and image sizing, so it is essential to understand the code and test many inputs. Finally, direct feedback from my partner caught problems that AI alone did not, which showed me that teamwork is important for improving a project from a real user’s perspective.",
        "I was in charge of the Explain PCA part. AI wrote the first version of the Explain PCA code, which included the plotting functions, the explained-variance calculations, and the step-by-step layout. My role was to check that code for errors and adapt it to our app. During the review, I found that x and z were fixed to the first self-check photo, and that the number of PCs was set by K_default. Since the app has to use the photo and K coming from the Shoe Search tab, I changed these values to reactives that respond to user input. I also deleted K_default, because the slider already sets K. Working on this taught me that a Shiny app is divided into a UI and a server. In a console script, a plot appears as soon as the code runs, but in the app the server must create the plot with renderPlot and the UI must place it on the screen with plotOutput, and the two must share the same name to be connected. I also learned that values that change with user input, such as the selected photo or K, must be made reactive.",
        "I was in charge of the Our Shoes tab. AI helped me implement the Shiny interface and understand how to connect it to the existing PCA engine. However, I learned that AI-generated code still needs to be reviewed carefully, especially when handling image files and category labels. I also realized that PCA search results can be affected by differences in photo backgrounds and shoe positions. Through this project, I gained a better understanding of how PCA can be applied to real images and how important data preparation and testing are for building a reliable application.",
        "AI has an excellent ability to translate my ideas into code and does a great job of compensating for areas where I lack knowledge, but additional studying was necessary to make that knowledge my own and to answer the professor's questions. Also, I believe visualizations should show their intended message at a glance, yet I lacked insight or planning skills regarding how and what to visualize, what I was trying to say, and what value it holds, and there were also shortcomings in the design aspect. Seeing that the output varies depending on the user's depth of knowledge made me feel even more acutely the need for self-improvement."
      )
    )
  })
  
}



shinyApp(ui, server)
