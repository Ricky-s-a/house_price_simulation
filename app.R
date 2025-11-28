library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(bsicons)
library(DT)
library(RPostgres)
library(DBI)
library(shinyjs)

# -------------------------------------------------------------------------
# 0. モジュール読み込み & DB初期化
# -------------------------------------------------------------------------
list.files("R", full.names = TRUE, pattern = "\\.R$") %>% walk(source)
# DB初期化 (テーブルがない場合のみ作成)
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
  title = "不動産投資シミュレーター (Ver 5.0)",
  theme = my_theme,
  useShinyjs(),
  
  sidebar = sidebar(
    width = 350,
    
    # --- 物件選択エリア ---
    div(class="p-2 bg-light border-bottom",
        h5("1. 物件選択", icon("building")),
        selectizeInput("property_select", "物件を選択", choices=NULL, options=list(placeholder="選択してください", onInitialize = I('function() { this.setValue(""); }'))),
        textInput("property_name", "物件名 (新規/編集)", placeholder="例: 区分マンションA"),
        actionButton("new_property_btn", "新規作成", icon=icon("plus"), class="btn-sm btn-outline-primary mb-2")
    ),
    
    accordion(
      open = c("物件スペック", "シミュレーション条件"), # デフォルトで開くパネル
      
      # --- 物件スペック (Properties) ---
      accordion_panel("物件スペック", icon=icon("info-circle"),
                      layout_columns(col_widths = c(6, 6),
                                     numericInput("price", "物件価格 (万円)", value=6000, step=100),
                                     numericInput("area_m2", "専有面積 (m2)", value=65, step=1)
                      ),
                      layout_columns(col_widths=c(6,6),
                                     numericInput("mgmt_fee", "管理費 (万円)", value=1.3, step=0.1),
                                     numericInput("repair_fund", "修繕積立 (万円)", value=2.0, step=0.1)
                      ),
                      helpText(textOutput("cost_per_sqm_display_main"), class="text-primary small", style="margin-top: -10px;")
      ),
      
      # --- シミュレーション条件 (Simulations) ---
      accordion_panel("シミュレーション条件", icon=icon("sliders"),
                      textInput("sim_name", "シミュレーション名", value="標準プラン", placeholder="例: フルローン案"),
                      
                      # 賃料設定
                      numericInput("monthly_rent", "想定月額賃料 (万円)", value=23.6, step=0.5),
                      uiOutput("break_even_rent_ui_main"),
                      
                      # 諸費用
                      tags$label("諸経費設定", class="control-label mt-2"),
                      radioButtons("initial_cost_mode", NULL, choices=c("率指定 (%)"="rate", "金額指定 (万円)"="amount"), inline=TRUE, selected="rate"),
                      conditionalPanel("input.initial_cost_mode == 'rate'", numericInput("initial_cost_rate", NULL, value=7, step=0.5)),
                      conditionalPanel("input.initial_cost_mode == 'amount'", numericInput("initial_cost_amount", NULL, value=400, step=10)),
                      
                      # 借入条件
                      hr(),
                      tags$label("ローン設定", class="control-label"),
                      checkboxInput("include_cost_in_loan", "諸費用もローンに含める (オーバーローン)", value = TRUE),
                      layout_columns(col_widths=c(6,6),
                                     numericInput("down_payment", "頭金 (万円)", value=0, step=100),
                                     sliderInput("interest_rate", "金利 (%)", min=0.1, max=5.0, value=1.0, step=0.1)
                      ),
                      sliderInput("loan_years", "期間 (年)", min=10, max=50, value=35, step=1),
                      radioButtons("repayment_method", NULL, choices=c("元利均等返済", "元本均等返済"), inline=TRUE),
                      
                      # 詳細設定 (アコーディオン内アコーディオンは避けるため details を使用)
                      tags$details(
                        style = "margin-top: 10px; border-top: 1px solid #ddd; padding-top: 10px;",
                        tags$summary("詳細リスク設定 (空室・下落率)", style="cursor: pointer; color: #7f8c8d;"),
                        div(class="mt-2",
                            sliderInput("rent_decline_rate", "賃料変動率 (年率 %)", min=-2.0, max=10.0, value=1.0, step=0.1),
                            sliderInput("occupancy_rate", "入居率 (%)", min=50, max=100, value=100, step=1),
                            selectInput("drop_type", "価格変動モデル", choices=c("定額法"="fixed_amount","定率法(下落)"="rate","定率法(上昇)"="appreciation","価格指定"="target_price")),
                            conditionalPanel("input.drop_type=='rate'", sliderInput("depreciation_rate", "下落率 (%)", min=0, max=5.0, value=1.5, step=0.1)),
                            conditionalPanel("input.drop_type=='appreciation'", sliderInput("appreciation_rate", "上昇率 (%)", min=0, max=5.0, value=1.0, step=0.1)),
                            conditionalPanel("input.drop_type=='target_price'", numericInput("target_year", "年後", value=10), numericInput("target_price_val", "価格(万)", value=5500))
                        )
                      )
      )
    ),
    
    div(class="d-grid gap-2 mt-3",
        actionButton("calc", "計算実行", class="btn-primary", icon=icon("calculator")),
        actionButton("save_db", "保存 (物件+条件)", class="btn-success", icon=icon("save"))
    ),
    
    # 履歴リスト (全物件)
    div(class="mt-3",
        h6("全シミュレーション履歴"),
        uiOutput("sim_history_list")
    )
  ),
  
  layout_columns(
    col_widths = 12,
    analysisUI("analysis_module")
  )
)

# -------------------------------------------------------------------------
# 2. Server 定義
# -------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Reactive Values ---
  current_property_id <- reactiveVal("new") # "new" or ID
  db_trigger <- reactiveVal(0) # DB更新通知用
  
  # --- A. 初期化 & 物件リスト更新 ---
  observe({
    db_trigger()
    props <- get_property_list()
    
    # 現在の選択状態を維持するためのロジック
    selected_val <- current_property_id()
    
    if(nrow(props) > 0) {
      choices <- setNames(props$id, props$name)
      choices <- c("新規作成" = "new", choices)
      updateSelectizeInput(session, "property_select", choices = choices, selected = selected_val)
    } else {
      updateSelectizeInput(session, "property_select", choices = c("新規作成" = "new"), selected = "new")
    }
  })
  
  # --- B. 物件選択時の挙動 ---
  observeEvent(input$property_select, {
    val <- input$property_select
    # "new" が選択された場合、または空の場合
    if(val == "new" || val == "") {
      current_property_id("new")
      updateTextInput(session, "property_name", value = "")
      updateNumericInput(session, "price", value = 6000) # デフォルト値
      updateNumericInput(session, "area_m2", value = 65)
      updateNumericInput(session, "mgmt_fee", value = 1.3)
      updateNumericInput(session, "repair_fund", value = 2.0)
    } else {
      current_property_id(val)
      # DBから物件情報を取得してセット
      p <- get_property_by_id(val)
      if(nrow(p) > 0) {
        updateTextInput(session, "property_name", value = p$name)
        updateNumericInput(session, "price", value = p$price)
        updateNumericInput(session, "area_m2", value = p$area_m2)
        updateNumericInput(session, "mgmt_fee", value = p$mgmt_fee)
        updateNumericInput(session, "repair_fund", value = p$repair_fund)
      }
    }
  }, ignoreInit = TRUE) # 初期化時は A の observe が処理するので無視
  
  # 新規作成ボタン (強制リセット)
  observeEvent(input$new_property_btn, {
    updateSelectizeInput(session, "property_select", selected = "new")
    current_property_id("new")
    updateTextInput(session, "property_name", value = "")
    updateNumericInput(session, "price", value = 6000)
    updateNumericInput(session, "area_m2", value = 65)
    updateNumericInput(session, "mgmt_fee", value = 1.3)
    updateNumericInput(session, "repair_fund", value = 2.0)
    showNotification("新規作成モードに切り替えました", type="message")
  })
  
  # --- C. シミュレーション履歴リスト表示 (全件) ---
  output$sim_history_list <- renderUI({
    db_trigger()
    sims <- get_all_simulation_list()
    
    if(nrow(sims) == 0) return(div(class="text-muted small", "履歴なし"))
    
    # リストグループとして表示
    tags$div(class="list-group",
             lapply(1:nrow(sims), function(i) {
               row <- sims[i, ]
               # priceカラムがない古いデータへの対応
               disp_price <- if("price" %in% names(row) && !is.na(row$price)) row$price else 0
               
               tags$button(
                 type="button",
                 class="list-group-item list-group-item-action d-flex justify-content-between align-items-center",
                 onclick = sprintf("Shiny.setInputValue('load_sim_id', %d, {priority: 'event'})", row$id),
                 div(
                   div(style="font-weight: bold;", paste0(row$prop_name, " - ", row$sim_name)),
                   div(style="font-size: 0.8rem;", 
                       paste0(format(disp_price, big.mark=","), "万 vs ", row$monthly_rent, "万 / IRR: ", sprintf("%.1f", row$max_irr*100), "%")
                   )
                 ),
                 span(class="badge bg-primary rounded-pill", icon("chevron-right"))
               )
             })
    )
  })
  
  # --- D. シミュレーション履歴ロード ---
  observeEvent(input$load_sim_id, {
    sid <- input$load_sim_id
    d <- get_simulation_by_id(sid)
    if(nrow(d) > 0) {
      # 1. 物件情報のロード
      pid <- d$property_id
      prop <- get_property_by_id(pid)
      
      if(nrow(prop) > 0) {
        # UI更新
        updateSelectizeInput(session, "property_select", selected = pid)
        current_property_id(pid)
        
        updateTextInput(session, "property_name", value = prop$name)
        updateNumericInput(session, "area_m2", value = prop$area_m2)
        updateNumericInput(session, "mgmt_fee", value = prop$mgmt_fee)
        updateNumericInput(session, "repair_fund", value = prop$repair_fund)
        
        # 価格: シミュレーションに保存された価格があればそれを優先、なければ物件価格
        sim_price <- if("price" %in% names(d) && !is.na(d$price)) d$price else prop$price
        updateNumericInput(session, "price", value = sim_price)
      }
      
      # 2. シミュレーション条件のロード
      updateTextInput(session, "sim_name", value = d$name)
      updateNumericInput(session, "monthly_rent", value = d$monthly_rent)
      
      # 諸費用
      updateRadioButtons(session, "initial_cost_mode", selected = d$initial_cost_mode)
      if(d$initial_cost_mode == "rate") {
        updateNumericInput(session, "initial_cost_rate", value = d$initial_cost_val)
      } else {
        updateNumericInput(session, "initial_cost_amount", value = d$initial_cost_val)
      }
      
      updateNumericInput(session, "down_payment", value = d$down_payment)
      updateCheckboxInput(session, "include_cost_in_loan", value = d$include_cost_in_loan)
      updateSliderInput(session, "interest_rate", value = d$interest_rate)
      updateSliderInput(session, "loan_years", value = d$loan_years)
      updateRadioButtons(session, "repayment_method", selected = d$repayment_method)
      
      updateSliderInput(session, "rent_decline_rate", value = d$rent_decline_rate)
      updateSliderInput(session, "occupancy_rate", value = d$occupancy_rate)
      
      updateSelectInput(session, "drop_type", selected = d$drop_type)
      if(!is.na(d$depreciation_rate)) updateSliderInput(session, "depreciation_rate", value = d$depreciation_rate)
      if(!is.na(d$appreciation_rate)) updateSliderInput(session, "appreciation_rate", value = d$appreciation_rate)
      if(!is.na(d$target_year)) updateNumericInput(session, "target_year", value = d$target_year)
      if(!is.na(d$target_price_val)) updateNumericInput(session, "target_price_val", value = d$target_price_val)
      
      showNotification(paste0("「", prop$name, " - ", d$name, "」を読み込みました"), type="message")
      click("calc")
    }
  })
  
  # --- E. 計算ロジック ---
  sim_data <- eventReactive(input$calc, {
    # 諸費用計算
    ic_val <- if(input$initial_cost_mode == "rate") {
      (input$price * 10000) * (input$initial_cost_rate / 100)
    } else {
      input$initial_cost_amount * 10000
    }
    
    params <- list(
      price = input$price, 
      area_m2 = input$area_m2,
      down_payment = input$down_payment, 
      include_cost_in_loan = input$include_cost_in_loan,
      initial_cost_yen = ic_val,
      monthly_rent = input$monthly_rent,
      rent_decline_rate = input$rent_decline_rate, 
      occupancy_rate = input$occupancy_rate,
      mgmt_fee = input$mgmt_fee, 
      repair_fund = input$repair_fund,
      interest_rate = input$interest_rate, 
      loan_years = input$loan_years, 
      repayment_method = input$repayment_method,
      drop_type = input$drop_type, 
      depreciation_rate = input$depreciation_rate,
      appreciation_rate = input$appreciation_rate, 
      target_year = input$target_year, 
      target_price_val = input$target_price_val
    )
    run_simulation_logic(params)
  }, ignoreNULL = FALSE)
  
  input_params <- reactive({
    # UI表示用パラメータ (KPIモジュール等で使用)
    list(
      price = input$price, area_m2 = input$area_m2, monthly_rent = input$monthly_rent, 
      initial_cost_rate = if(input$initial_cost_mode=="rate") input$initial_cost_rate else (input$initial_cost_amount/input$price*100),
      mgmt_fee = input$mgmt_fee, repair_fund = input$repair_fund,
      rent_decline_rate = input$rent_decline_rate, occupancy_rate = input$occupancy_rate,
      down_payment = input$down_payment, include_cost_in_loan = input$include_cost_in_loan, interest_rate = input$interest_rate, 
      loan_years = input$loan_years, repayment_method = input$repayment_method,
      drop_type = input$drop_type, depreciation_rate = input$depreciation_rate, 
      appreciation_rate = input$appreciation_rate, target_year = input$target_year, target_price_val = input$target_price_val
    )
  })
  
  # --- F. 保存処理 ---
  observeEvent(input$save_db, {
    req(sim_data())
    tryCatch({
      # 1. 物件保存
      prop_inputs <- list(
        property_name = input$property_name,
        price = input$price,
        area_m2 = input$area_m2,
        mgmt_fee = input$mgmt_fee,
        repair_fund = input$repair_fund
      )
      pid <- save_property(prop_inputs, id = if(current_property_id() == "new") NULL else current_property_id())
      
      # ID更新 (新規作成だった場合)
      current_property_id(pid)
      
      # 2. シミュレーション保存
      sim_inputs <- list(
        monthly_rent = input$monthly_rent,
        rent_decline_rate = input$rent_decline_rate,
        occupancy_rate = input$occupancy_rate,
        initial_cost_mode = input$initial_cost_mode,
        initial_cost_rate = input$initial_cost_rate,
        initial_cost_amount = input$initial_cost_amount,
        down_payment = input$down_payment,
        include_cost_in_loan = input$include_cost_in_loan,
        interest_rate = input$interest_rate,
        loan_years = input$loan_years,
        repayment_method = input$repayment_method,
        drop_type = input$drop_type,
        depreciation_rate = input$depreciation_rate,
        appreciation_rate = input$appreciation_rate,
        target_year = input$target_year,
        target_price_val = input$target_price_val,
        price = input$price # Yield計算用
      )
      
      save_simulation(pid, sim_inputs, sim_data(), input$sim_name)
      
      showNotification("保存しました", type="message")
      db_trigger(db_trigger() + 1) # リスト更新
      
    }, error = function(e) {
      showNotification(paste("保存エラー:", e$message), type="error")
    })
  })
  
  # --- G. その他表示 ---
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
        div(style = "font-weight: bold; font-size: 0.9rem;", "損益分岐点賃料: ", span(format(round(be_rent/10000, 2), nsmall=2), " 万円")),
        div(class = paste("small", color_class), paste0("安全余裕幅: ", margin_sign, format(round(margin/10000, 2), nsmall=2), " 万円"))
    )
  })
  
  # モジュール連携
  analysisServer("analysis_module", sim_data, input_params, reactive(input$sim_name))
}

shinyApp(ui, server)