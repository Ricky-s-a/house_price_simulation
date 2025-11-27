library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(bsicons)
library(DT)
library(RSQLite)
library(DBI)
library(shinyjs)

# -------------------------------------------------------------------------
# 0. モジュール読み込み & DB初期化
# -------------------------------------------------------------------------
list.files("R", full.names = TRUE, pattern = "\\.R$") %>% walk(source)
init_db()

# -------------------------------------------------------------------------
# 1. UI 定義
# -------------------------------------------------------------------------
my_theme <- bs_theme(
  bootswatch = "flatly",
  base_font = font_google("Noto Sans JP"),
  heading_font = font_google("Noto Sans JP")
)

ui <- page_sidebar(
  title = "不動産投資シミュレーター (Ver 43.1)",
  theme = my_theme,
  useShinyjs(),
  
  sidebar = sidebar(
    width = 350,
    accordion(
      accordion_panel("1. 物件・賃貸条件", icon=icon("building"),
                      selectizeInput("sim_name_select", "シミュレーション名 (履歴選択 or 入力)", choices=NULL, options=list(create=TRUE, placeholder="履歴から選択 または 新規名称を入力")),
                      hr(),
                      
                      # --- 物件スペック ---
                      layout_columns(col_widths = c(6, 6),
                                     numericInput("price", "物件価格 (万円)", value=6000, step=100),
                                     numericInput("area_m2", "専有面積 (m2)", value=65, step=1)
                      ),
                      
                      # --- ランニングコスト ---
                      layout_columns(col_widths=c(6,6),
                                     numericInput("mgmt_fee", "管理費 (万円)", value=1.3, step=0.1),
                                     numericInput("repair_fund", "修繕積立 (万円)", value=2.0, step=0.1)
                      ),
                      helpText(textOutput("cost_per_sqm_display_main"), class="text-primary small", style="margin-top: -10px; margin-bottom: 10px;"),
                      
                      # --- 賃料 & 損益分岐点 ---
                      numericInput("monthly_rent", "想定月額賃料 (初期値)", value=23.6, step=0.5),
                      uiOutput("break_even_rent_ui_main"),
                      
                      hr(),
                      
                      # --- 【復活】詳細設定 (折りたたみ) ---
                      tags$details(
                        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; border: 1px solid #ddd;",
                        tags$summary("詳細設定 (諸経費・リスク)", style = "cursor: pointer; font-weight: bold; color: #7f8c8d;"),
                        
                        div(style = "margin-top: 10px;",
                            numericInput("initial_cost_rate", "購入諸経費率 (%)", value=7, step=0.5),
                            
                            tags$label("賃料変動・稼働率 (リスク)", class="control-label", style="margin-top: 10px;"),
                            div(class="p-2 bg-white border rounded",
                                sliderInput("rent_decline_rate", "賃料変動率 (年率 %)", min=-2.0, max=10.0, value=1.0, step=0.1),
                                sliderInput("occupancy_rate", "入居率 (稼働率 %)", min=50, max=100, value=100, step=1)
                            )
                        )
                      )
      ),
      
      accordion_panel("2. 借入条件", icon=icon("bank"),
                      numericInput("down_payment", "頭金 (万円)", value=0, step=100),
                      sliderInput("interest_rate", "金利 (%)", min=0.1, max=5.0, value=1.0, step=0.1),
                      sliderInput("loan_years", "借入期間 (年)", min=10, max=50, value=35, step=1),
                      radioButtons("repayment_method", "返済方式:", choices=c("元利均等返済", "元本均等返済"))
      ),
      
      accordion_panel("3. 市場予測", icon=icon("chart-line"),
                      radioButtons("drop_type", "価格変動モデル:", choices=c("定額法 (毎年 坪4万円下落)"="fixed_amount","定率法 (毎年 %で下落)"="rate","定率法 (毎年 %で上昇)"="appreciation","価格指定 (X年後に〇万円)"="target_price")),
                      conditionalPanel(condition="input.drop_type=='rate'", sliderInput("depreciation_rate", "年間価格下落率 (%)", min=0, max=10.0, value=1.5, step=0.1)),
                      conditionalPanel(condition="input.drop_type=='appreciation'", sliderInput("appreciation_rate", "年間価格上昇率 (%)", min=0, max=10.0, value=1.0, step=0.1)),
                      conditionalPanel(condition="input.drop_type=='target_price'", helpText("何年後にいくらで売れるかを予想します。"), numericInput("target_year", "目標年数", value=10, min=1), numericInput("target_price_val", "予想価格 (万)", value=5500, step=50))
      )
    ),
    br(),
    actionButton("calc", "計算実行", class="btn-primary w-100", icon=icon("calculator"), onclick="window.scrollTo({top: 0, behavior: 'smooth'});"),
    div(style="height: 10px;"),
    actionButton("save_db", "結果をDB保存", class="btn-dark w-100", icon=icon("save"))
  ),
  
  layout_columns(
    col_widths = c(12, 12),
    kpiUI("kpi_module"),
    analysisUI("analysis_module")
  )
)

# -------------------------------------------------------------------------
# 2. Server 定義
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # A. シミュレーションロジック
  sim_data <- eventReactive(input$calc, {
    params <- list(
      price = input$price, down_payment = input$down_payment,
      initial_cost_rate = input$initial_cost_rate, monthly_rent = input$monthly_rent,
      rent_decline_rate = input$rent_decline_rate, occupancy_rate = input$occupancy_rate,
      mgmt_fee = input$mgmt_fee, repair_fund = input$repair_fund,
      interest_rate = input$interest_rate, loan_years = input$loan_years, repayment_method = input$repayment_method,
      drop_type = input$drop_type, depreciation_rate = input$depreciation_rate,
      appreciation_rate = input$appreciation_rate, target_year = input$target_year, target_price_val = input$target_price_val,
      area_m2 = input$area_m2
    )
    run_simulation_logic(params)
  }, ignoreNULL = FALSE)
  
  input_params <- reactive({
    list(
      price = input$price, area_m2 = input$area_m2, monthly_rent = input$monthly_rent, 
      initial_cost_rate = input$initial_cost_rate, mgmt_fee = input$mgmt_fee, repair_fund = input$repair_fund,
      rent_decline_rate = input$rent_decline_rate, occupancy_rate = input$occupancy_rate,
      down_payment = input$down_payment, interest_rate = input$interest_rate, 
      loan_years = input$loan_years, repayment_method = input$repayment_method,
      drop_type = input$drop_type, depreciation_rate = input$depreciation_rate, 
      appreciation_rate = input$appreciation_rate, target_year = input$target_year, target_price_val = input$target_price_val
    )
  })
  
  # B. サイドバー用計算・表示
  output$cost_per_sqm_display_main <- renderText({
    cost_per_m2 <- (input$repair_fund * 10000) / input$area_m2
    paste0("※ 修繕積立金単価: ", round(cost_per_m2, 0), " 円/m2")
  })
  
  output$break_even_rent_ui_main <- renderUI({
    df <- sim_data()
    monthly_pay <- df$Total_Payment_Year[1] / 12
    monthly_running <- (input$mgmt_fee + input$repair_fund) * 10000
    be_rent <- monthly_pay + monthly_running
    current_rent <- input$monthly_rent * 10000
    margin <- current_rent - be_rent
    color_class <- if(margin >= 0) "text-success" else "text-danger"
    margin_sign <- if(margin >= 0) "+" else ""
    div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 5px; margin-bottom: 5px;",
        div(style = "font-weight: bold; font-size: 0.9rem;", "損益分岐点賃料 (返済+維持費): ", span(format(round(be_rent/10000, 2), nsmall=2), " 万円")),
        div(class = paste("small", color_class), paste0("安全余裕幅: ", margin_sign, format(round(margin/10000, 2), nsmall=2), " 万円"), if(margin < 0) " (※ 現状赤字です)" else "")
    )
  })
  
  # C. モジュール連携
  kpiServer("kpi_module", sim_data, input_params)
  loaded_data <- analysisServer("analysis_module", sim_data, input_params, reactive(input$sim_name_select))
  
  # D. DB保存処理
  db_trigger <- reactiveVal(0)
  
  observeEvent(input$save_db, {
    req(sim_data())
    current_val <- input$sim_name_select
    save_memo <- "名称未設定"
    if(grepl("^[0-9]+$", current_val)) {
      old_data <- get_scenario_by_id(current_val)
      if(nrow(old_data)>0) save_memo <- old_data$memo
    } else {
      save_memo <- current_val
    }
    tryCatch({
      save_scenario_to_db(input_params(), sim_data(), save_memo)
      showNotification(paste0("「", save_memo, "」を保存しました"), type="message")
      db_trigger(db_trigger() + 1)
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type="error")
    })
  })
  
  # E. 履歴リスト更新 (表示形式: メモ (価格万 vs 家賃万))
  observe({
    db_trigger()
    df <- get_scenario_history()
    if(nrow(df) > 0) {
      labels <- paste0(df$memo, " (", df$price, "万 vs ", df$monthly_rent, "万)")
      choices <- setNames(df$id, labels)
      selected_val <- isolate(input$sim_name_select)
      updateSelectizeInput(session, "sim_name_select", choices = choices, selected = selected_val, server = TRUE)
    }
  })
  
  # F. 履歴ロード
  observeEvent(input$sim_name_select, {
    req(input$sim_name_select)
    val <- input$sim_name_select
    if(grepl("^[0-9]+$", val)) {
      d <- get_scenario_by_id(val)
      if(nrow(d) > 0) {
        updateNumericInput(session, "price", value = d$price)
        updateNumericInput(session, "area_m2", value = d$area_m2)
        updateNumericInput(session, "initial_cost_rate", value = d$initial_cost_rate)
        updateNumericInput(session, "monthly_rent", value = d$monthly_rent)
        updateSliderInput(session, "rent_decline_rate", value = d$rent_decline_rate)
        updateSliderInput(session, "occupancy_rate", value = d$occupancy_rate)
        updateNumericInput(session, "mgmt_fee", value = d$mgmt_fee)
        updateNumericInput(session, "repair_fund", value = d$repair_fund)
        updateNumericInput(session, "down_payment", value = d$down_payment)
        updateSliderInput(session, "interest_rate", value = d$interest_rate)
        updateSliderInput(session, "loan_years", value = d$loan_years)
        updateRadioButtons(session, "repayment_method", selected = d$repayment_method)
        updateRadioButtons(session, "drop_type", selected = d$drop_type)
        
        if(!is.na(d$depreciation_rate)) updateSliderInput(session, "depreciation_rate", value = d$depreciation_rate)
        if(!is.na(d$appreciation_rate)) updateSliderInput(session, "appreciation_rate", value = d$appreciation_rate)
        if(!is.na(d$target_year)) updateNumericInput(session, "target_year", value = d$target_year)
        if(!is.na(d$target_price_val)) updateNumericInput(session, "target_price_val", value = d$target_price_val)
        
        showNotification(paste0("「", d$memo, "」を復元しました"), type="message")
        click("calc")
      }
    }
  })
}

shinyApp(ui, server)