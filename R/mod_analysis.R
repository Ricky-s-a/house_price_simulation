# R/mod_analysis.R

# UI
analysisUI <- function(id) {
  ns <- NS(id)
  
  # 数式表示を有効化するために tagList でラップ
  tagList(
    withMathJax(),
    
    navset_card_underline(
      title = "詳細分析レポート",
      full_screen = TRUE,
      
      # [0] サマリー (Dashboard)
      nav_panel(
        title = "サマリー (Dashboard)",
        icon = icon("dashboard"),
        
        card_body(
          fill = FALSE,
          card_header("投資ハイライト"),
          
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            value_box(
              title = "初期投下資金", 
              value = textOutput(ns("summary_initial_cash")), 
              showcase = bsicons::bs_icon("wallet-fill"), 
              theme = "danger", 
              min_height = "180px",
              p("諸経費 + 頭金")
            ),
            value_box(
              title="損益分岐 (IRRプラス転換)", 
              value=uiOutput(ns("summary_breakeven")), 
              showcase=bsicons::bs_icon("sunrise"), 
              theme="warning", 
              min_height = "180px",
              p("元本回収完了のタイミング")
            )
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE,
            value_box(
              title="最高効率 (Max IRR)", 
              value=uiOutput(ns("summary_max_irr")), 
              showcase=bsicons::bs_icon("stars"), 
              theme="primary", 
              min_height = "180px",
              p("資金効率が最大となる売り時")
            ),
            value_box(
              title = "最大利益 (Max Profit)", 
              value = uiOutput(ns("summary_max_profit")), 
              showcase = bsicons::bs_icon("graph-up-arrow"), 
              theme = "success", 
              min_height = "180px",
              p("累積CF + 売却益が最大となる時点"),
              p("※金利・管理費等を引いた純利益", style="font-size: 0.7rem; color: #ecf0f1;")
            )
          ),
          
          hr(),
          card_header("基本指標 (Basic Indicators)"),
          
          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            value_box(
              title = "表面利回り (Gross)",
              value = textOutput(ns("summary_gross_yield")),
              showcase = bsicons::bs_icon("percent"),
              theme = "primary", # Dark blue-ish equivalent if possible, or primary
              min_height = "180px",
              p("※初期家賃ベース")
            ),
            value_box(
              title = "実質利回り (Net/NOI)",
              value = textOutput(ns("summary_net_yield")),
              showcase = bsicons::bs_icon("clipboard-check"),
              theme = "primary",
              min_height = "180px",
              p("経費・空室リスク控除後")
            ),
            value_box(
              title = "月次収支 (初年度)",
              value = textOutput(ns("summary_monthly_cf")),
              showcase = bsicons::bs_icon("piggy-bank"),
              theme = "success", # Green
              min_height = "180px",
              p("実効家賃 - (返済 + 管理修繕)")
            )
          ),
          
          layout_columns(
            col_widths = c(4, 4, 4),
            fill = FALSE,
            value_box(
              title = "初回 月々返済額",
              value = textOutput(ns("summary_monthly_payment")),
              showcase = bsicons::bs_icon("wallet2"),
              theme = "warning", # Orange
              min_height = "180px",
              p("元本 + 利息")
            ),
            value_box(
              title = "10年後 累積収益",
              value = uiOutput(ns("summary_profit_10y")),
              showcase = bsicons::bs_icon("graph-up"),
              theme = "secondary", # Grey
              min_height = "180px",
              p("賃料総額 - (購入 - 売却)")
            ),
            value_box(
              title = "完済時 (35年後) 収益",
              value = uiOutput(ns("summary_profit_payoff")),
              showcase = bsicons::bs_icon("trophy"),
              theme = "success", # Green
              min_height = "180px",
              p("賃料総額 - (購入 - 売却)")
            )
          )
        )
      ),

      # [1] 収支・現金 (Cash/PL)
      nav_panel(
        title = "収支・現金 (Cash/PL)",
        icon = icon("money-bill-wave"),
        
        # 【修正ポイント 1】 fill = FALSE を指定して、画面高さに合わせて潰れないようにする
        card_body(
          fill = FALSE, # これが重要です。スクロールを許可します。
          
          card_header("手元資金の推移 (累積キャッシュフロー)"),
          
          layout_columns(
            col_widths = c(4, 4, 4),
            # 【修正ポイント 2】 fill = FALSE で高さをコンテンツに合わせる
            fill = FALSE, 
            
            value_box(
              title = "初期投下資金 (購入時)", 
              value = textOutput(ns("initial_cash_val")), 
              showcase = bsicons::bs_icon("wallet-fill"), 
              theme = "danger", 
              min_height = "200px", 
              p("諸経費 + 頭金")
            ),
            value_box(
              title="IRRプラス転換 (損益分岐)", 
              value=uiOutput(ns("irr_breakeven_detail_cash")), 
              showcase=bsicons::bs_icon("sunrise"), 
              theme="warning", 
              min_height="200px", 
              p("これ以前に売却すると元本割れ", style="font-size: 0.8rem; color: #ffffff;")
            ),
            value_box(
              title = "最終到達額 (CFピーク時)", 
              value = uiOutput(ns("max_cf_detail")), 
              showcase = bsicons::bs_icon("arrow-up-circle"), 
              theme = "success", 
              min_height = "200px", 
              p("計算式: 累積CF + (売却額 - 残債)", style="font-size: 0.8rem; margin-top: 5px; color: #ecf0f1;"),
              p("※金利・管理費等を引いた手残り", style="font-size: 0.7rem; color: #ecf0f1;")
            )
          ),
          
          plotOutput(ns("cf_plot"), height = "300px"),
          p(class = "text-muted small", "※ 棒グラフがプラス(青)になれば投資回収完了。マイナス(灰)の間は持ち出し状態です。"),
          
          hr(),
          
          card_header("月次収支の推移 (家賃変動の影響)"),
          card(class="bg-light", markdown("**初年度 月次収支の内訳:**"), textOutput(ns("cf_breakdown_text"))),
          plotOutput(ns("monthly_cf_plot"), height = "250px")
        )
      ),
      
      # [2] 資産・効率 (BS/IRR)
      nav_panel(
        title = "資産・効率 (BS/IRR)",
        icon = icon("chart-pie"),
        
        layout_columns(
          col_widths = c(6, 6),
          
          # 左カラム
          tagList(
            card(card_header("資産価値 vs ローン残債 (B/S)"), plotOutput(ns("bs_plot"), height="400px"), card_footer(class="text-muted small", "青線(資産)が赤線(借金)より上にあれば健全です。")),
            card(card_header("変化率の推移"), plotOutput(ns("rate_plot"), height="250px"))
          ),
          
          # 右カラム
          tagList(
            card(card_header("売却時の内部収益率 (IRR) 推移"), plotOutput(ns("irr_plot"), height="350px")),
            
            # ここも潰れないように fill = FALSE
            # ここも潰れないように fill = FALSE
            layout_columns(
              col_widths = c(4, 4, 4),
              fill = FALSE, 
              
              value_box(
                title="IRRプラス転換 (損益分岐)", 
                value=uiOutput(ns("irr_breakeven_detail")), 
                showcase=bsicons::bs_icon("sunrise"), 
                theme="warning", 
                min_height="200px", 
                p("これ以前に売却すると元本割れ", style="font-size: 0.8rem; color: #ffffff;")
              ),
              value_box(
                title="最高効率点 (Max IRR)", 
                value=uiOutput(ns("irr_max_detail")), 
                showcase=bsicons::bs_icon("stars"), 
                theme="primary", 
                min_height="200px", 
                p("資金効率が最も良くなる売り時", style="font-size: 0.8rem; color: #ecf0f1;")
              ),
              value_box(
                title = "最終到達額 (Max IRR時)", 
                value = uiOutput(ns("max_irr_exit_detail_bs")), 
                showcase = bsicons::bs_icon("check-circle-fill"), 
                theme = "success", 
                min_height = "200px", 
                p("最高効率点で売却した場合の手残り", style="font-size: 0.8rem; margin-top: 5px; color: #ecf0f1;"),
                p("※金利・管理費等を引いた純利益", style="font-size: 0.7rem; color: #ecf0f1;")
              )
            )
          )
        )
      ),
      

      
      # その他のタブ
      nav_panel(title="総合収益 (Return)", card_header("マンション収益推移"), plotOutput(ns("profit_plot"), height="400px"), textOutput(ns("cagr_info"))),
      nav_panel(title="返済額推移 (Payment)", card_header("月々の返済額"), plotOutput(ns("payment_plot"), height="400px")),
      nav_panel(title="データ一覧 (Table)", card_header("詳細データ & 売却時IRR"), DTOutput(ns("raw_table"))),
      
      # [3] 解説・ロジック (Docs) - NEW
      nav_panel(
        title = "解説・ロジック (Docs)",
        icon = icon("book"),
        
        accordion(
          open = TRUE, # デフォルトですべて開く設定（必要に応じてFALSEや特定のID指定も可）
          
          # セクション1: 📊 重要な指標の定義と見方
          accordion_panel(
            "📊 重要な指標の定義と見方",
            
            # パネル1: IRR
            card(
              card_header("1. IRR (内部収益率) とは？"),
              card_body(
                p("説明文: 投資期間中の資金効率を示す指標。銀行預金の「複利利回り」に相当します。「お金をどれだけ効率よく増やせたか」を年率で表したものです。"),
                p(style="font-weight: bold;", "数式:"),
                div("$$\\sum_{t=1}^{N} \\frac{CF_t}{(1+r)^t} - InitialCost = 0$$"),
                tags$ul(
                  tags$li("プラスなら成功、マイナスなら元本割れ"),
                  tags$li("グラフの頂点が最適売り時")
                ),
                div(class="alert alert-warning", style="margin-top: 15px; font-size: 0.9rem;",
                  tags$strong("⚠️ 注意点: 頭金が少ない場合"),
                  p("IRRは「初期投資額に対してどれだけ増えたか」を計算する指標です。そのため、頭金がゼロに近い（フルローン等の）場合、計算の分母が極端に小さくなり、数値が異常に高く出たり（数千%など）、計算不能になることがあります。"),
                  p("本シミュレーターでは「諸経費」を初期投資とみなして計算しますが、頭金が少ない場合は数値が敏感に変動するため、あくまで参考値としてご覧ください。")
                )
              )
            ),
            
            # パネル2: 損益分岐点賃料
            card(
              card_header("2. 損益分岐点賃料 (Break-even Rent)"),
              card_body(
                p("説明文: ローン返済と維持費を賄うための最低限必要な家賃。"),
                div("$$Rent_{min} = PMT(Loan) + MgmtFee + RepairFund$$")
              )
            ),
            
            # パネル3: 表面利回り
            card(
              card_header("3. 表面利回り (Gross Yield)"),
              card_body(
                p("説明文: 物件価格に対する年間家賃収入の割合。最も基本的な指標ですが、空室や経費を考慮していないため、実際の収益性はこれより低くなります。"),
                div("$$Yield_{gross} = \\frac{MonthlyRent \\times 12}{Price} \\times 100$$")
              )
            ),

            # パネル4: 実質利回り
            card(
              card_header("4. 実質利回り (NOI Yield)"),
              card_body(
                p("説明文: 表面利回りから空室リスクとコストを引いた現実的な利回り。"),
                div("$$Yield_{net} = \\frac{(Rent \\times Occupancy) - (Mgmt + Repair)}{Price} \\times 100$$")
              )
            ),
            
            # パネル5: マンション収益 (完済時収益)
            card(
              card_header("5. 完済時収益 / マンション収益 (Total Profit)"),
              card_body(
                p("説明文: 投資期間終了（ローン完済時など）までに得られる「トータルの純利益」です。"),
                tags$ul(
                  tags$li("「毎月の家賃収入の積み上げ (インカム)」と「売却時の手残り (キャピタル)」の合計です。"),
                  tags$li("初期投資（頭金・諸費用）はすでに回収した上での、純粋なプラス分を表します。"),
                  tags$li("つまり、この金額がプラスであれば「投資によってこれだけ資産が増えた（お得だった）」と言えます。")
                ),
                hr(),
                p(style="font-weight: bold;", "よくある疑問: 「家賃総額 + (売却額 - 購入額)」と何が違う？"),
                p("単純な売買差益や家賃総取りではなく、以下の「見えないコスト」をすべて差し引いた【真の手残り】を計算しています。"),
                tags$ul(
                  tags$li(tags$strong("ローン金利 (Interest):"), " 借入期間中の利息支払い合計。長期ローンでは大きな金額になります。"),
                  tags$li(tags$strong("管理費・修繕積立金 (Running Cost):"), " 毎月の支出として家賃収入から差し引かれます。"),
                  tags$li(tags$strong("購入時諸費用 (Initial Cost):"), " 仲介手数料や登記費用などもコストとして計上されます。")
                ),
                div("$$Profit = CumulativeCF + (Price_{sell} - Loan_{balance})$$"),
                p(class="text-muted small", "※ 税引前の金額です。実際の利益はここから譲渡所得税等が引かれます。")
              )
            )
          ),
          
          # セクション2: 🧮 シミュレーションの計算ロジック
          accordion_panel(
            "🧮 シミュレーションの計算ロジック",
            card(
              card_header("前提条件"),
              card_body(
                markdown("
- **キャッシュフロー**: 収入（実効賃料）ー 支出（返済+管理修繕）。税金は簡易的に管理費に含む想定。
- **資産価値**: 定額法、定率法、またはターゲット価格に基づくCAGRで推移。
- **出口戦略**: 税引前（Pre-tax）の計算であること。売却益には別途税金がかかる旨の注記。
                ")
              )
            )
          )
        )
      )
    )
  )
}

# Server
analysisServer <- function(id, sim_data, input_params, sim_name_trigger) {
  moduleServer(id, function(input, output, session) {
    
    # --- Helper Functions ---
    render_detail_box <- function(row, main_val_suffix) {
      cum_cf <- row$Cumulative_Cash_Flow
      debt <- row$Liability
      asset <- row$Asset_Value
      total_exit <- cum_cf + (asset - debt)
      main_display <- if(stringr::str_detect(main_val_suffix, "%")) main_val_suffix else paste0(format(round(cum_cf / 10000, 1), big.mark=","), " ", main_val_suffix)
      
      # 文字色を白に固定
      HTML(paste0(
        "<div style='font-size: 1.2rem; font-weight: bold; color: white;'>", main_display, " <small>(", row$Year, "年後)</small></div>",
        "<div style='font-size: 0.8rem; margin-top: 5px; color: white; opacity: 0.9;'>",
        "ローン残債: ▲", format(round(debt / 10000, 0), big.mark=","), " 万円<br>",
        "売却想定額: ", format(round(asset / 10000, 0), big.mark=","), " 万円<br>",
        "<hr style='margin: 3px 0; border-top: 1px solid rgba(255,255,255,0.3);'>",
        "<strong>売却時トータル手残り: ", format(round(total_exit / 10000, 0), big.mark=","), " 万円</strong>",
        "</div>"
      ))
    }
    
    # --- Outputs ---
    output$initial_cash_val <- renderText({ 
      p <- input_params()
      
      # 諸費用計算
      initial_cost_yen <- if(p$initial_cost_rate > 0) {
        p$price * 10000 * p$initial_cost_rate / 100
      } else {
        0
      }
      
      # オーバーローンの場合は頭金のみ、そうでなければ諸費用+頭金
      initial_cash_out <- if(isTRUE(p$include_cost_in_loan)) {
        p$down_payment * 10000
      } else {
        initial_cost_yen + (p$down_payment * 10000)
      }
      
      paste0("-", format(round(initial_cash_out / 10000, 1), big.mark=","), " 万円") 
    })
    
    output$max_cf_detail <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Cumulative_Cash_Flow == max(Cumulative_Cash_Flow)) %>% dplyr::slice(1); render_detail_box(max_row, "万円") })
    output$max_cf_detail_bs <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Cumulative_Cash_Flow == max(Cumulative_Cash_Flow)) %>% dplyr::slice(1); render_detail_box(max_row, "万円") })
    output$max_irr_exit_detail_bs <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Estimated_IRR == max(Estimated_IRR, na.rm=TRUE)) %>% dplyr::slice(1); if(nrow(max_row) == 0) return(NULL); render_detail_box(max_row, "万円") })
    
    output$cf_breakdown_text <- renderText({ df <- sim_data(); p <- input_params(); m_rent <- p$monthly_rent * 10000 * (p$occupancy_rate / 100); m_pay <- df$Total_Payment_Year[1] / 12; m_cost <- (p$mgmt_fee + p$repair_fund) * 10000; paste0(format(round(m_rent,0),big.mark=",")," (実効家賃) - ", format(round(m_pay,0),big.mark=",")," (返済) - ", format(round(m_cost,0),big.mark=",")," (管理修繕) = ", format(round(m_rent-m_pay-m_cost,0),big.mark=",")," 円") })
    
    output$irr_breakeven_detail <- renderUI({ df <- sim_data(); pos_row <- df %>% dplyr::filter(Estimated_IRR > 0) %>% dplyr::slice(1); if(nrow(pos_row) == 0) return(HTML("<div style='font-size: 0.9rem; color: white;'>期間内黒字化なし</div>")); irr_val <- paste0(sprintf("%.2f", pos_row$Estimated_IRR * 100), "%"); render_detail_box(pos_row, irr_val) })
    output$irr_breakeven_detail_cash <- renderUI({ df <- sim_data(); pos_row <- df %>% dplyr::filter(Estimated_IRR > 0) %>% dplyr::slice(1); if(nrow(pos_row) == 0) return(HTML("<div style='font-size: 0.9rem; color: white;'>期間内黒字化なし</div>")); irr_val <- paste0(sprintf("%.2f", pos_row$Estimated_IRR * 100), "%"); render_detail_box(pos_row, irr_val) })
    
    output$irr_max_detail <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Estimated_IRR == max(Estimated_IRR, na.rm=TRUE)) %>% dplyr::slice(1); if(nrow(max_row) == 0) return(NULL); irr_val <- paste0(sprintf("%.2f", max_row$Estimated_IRR * 100), "%"); render_detail_box(max_row, irr_val) })
    
    # Summary Tab Outputs (Duplicate logic for independent rendering)
    output$summary_initial_cash <- renderText({ 
      p <- input_params()
      initial_cost_yen <- if(p$initial_cost_rate > 0) { p$price * 10000 * p$initial_cost_rate / 100 } else { 0 }
      initial_cash_out <- if(isTRUE(p$include_cost_in_loan)) { p$down_payment * 10000 } else { initial_cost_yen + (p$down_payment * 10000) }
      paste0("-", format(round(initial_cash_out / 10000, 1), big.mark=","), " 万円") 
    })
    output$summary_breakeven <- renderUI({ df <- sim_data(); pos_row <- df %>% dplyr::filter(Estimated_IRR > 0) %>% dplyr::slice(1); if(nrow(pos_row) == 0) return(HTML("<div style='font-size: 0.9rem; color: white;'>期間内黒字化なし</div>")); irr_val <- paste0(sprintf("%.2f", pos_row$Estimated_IRR * 100), "%"); render_detail_box(pos_row, irr_val) })
    output$summary_max_irr <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Estimated_IRR == max(Estimated_IRR, na.rm=TRUE)) %>% dplyr::slice(1); if(nrow(max_row) == 0) return(NULL); irr_val <- paste0(sprintf("%.2f", max_row$Estimated_IRR * 100), "%"); render_detail_box(max_row, irr_val) })
    output$summary_max_profit <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Cumulative_Cash_Flow == max(Cumulative_Cash_Flow)) %>% dplyr::slice(1); render_detail_box(max_row, "万円") })
    
    # --- Summary: Basic Indicators ---
    output$summary_gross_yield <- renderText({
      p <- input_params()
      gross_yield <- (p$monthly_rent * 12) / p$price * 100
      paste0(sprintf("%.2f", gross_yield), " %")
    })
    
    output$summary_net_yield <- renderText({
      p <- input_params()
      # NOI = (Rent * Occupancy) - (Mgmt + Repair)
      annual_rent <- p$monthly_rent * 12 * 10000
      effective_rent <- annual_rent * (p$occupancy_rate / 100)
      annual_cost <- (p$mgmt_fee + p$repair_fund) * 12 * 10000
      noi <- effective_rent - annual_cost
      net_yield <- noi / (p$price * 10000) * 100
      paste0(sprintf("%.2f", net_yield), " %")
    })
    
    output$summary_monthly_cf <- renderText({
      df <- sim_data()
      val <- df$Monthly_Net_Cash_Flow[1]
      paste0(format(round(val, 0), big.mark=","), " 円")
    })
    
    output$summary_monthly_payment <- renderText({
      df <- sim_data()
      val <- df$Monthly_Payment_Example[1]
      paste0(format(round(val, 0), big.mark=","), " 円")
    })
    
    output$summary_profit_10y <- renderUI({
      df <- sim_data()
      target_row <- df %>% dplyr::filter(Year == 10)
      if(nrow(target_row) == 0) return(HTML("<div style='color:white;'>データなし</div>"))
      
      val <- target_row$Mansion_Profit[1]
      HTML(paste0("<div style='font-size: 1.5rem; font-weight: bold; color: white;'>", format(round(val/10000, 0), big.mark=","), " 万円</div>"))
    })
    
    output$summary_profit_payoff <- renderUI({
      df <- sim_data()
      p <- input_params()
      # 完済時（ローン期間終了時）またはシミュレーション最終年
      target_year <- p$loan_years
      target_row <- df %>% dplyr::filter(Year == target_year)
      
      # もしローン期間より短いシミュレーションなら最終行
      if(nrow(target_row) == 0) {
        target_row <- df %>% dplyr::slice(n())
        label_text <- paste0(target_row$Year, "年後")
      } else {
        label_text <- paste0(target_year, "年後")
      }
      
      val <- target_row$Mansion_Profit[1]
      HTML(paste0("<div style='font-size: 1.5rem; font-weight: bold; color: white;'>", format(round(val/10000, 0), big.mark=","), " 万円</div>"))
    })
    
    output$cagr_info <- renderText({ p <- input_params(); if(p$drop_type=="target_price"){ paste0("CAGR: ", sprintf("%.2f", attr(sim_data(),"implied_rate")*100), "%") }else "" })
    
    # CSV Download
    output$download_csv <- downloadHandler(filename = function() { paste0("sim_result_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv") }, content = function(file) { write.csv(sim_data(), file, row.names = FALSE, fileEncoding = "CP932") })
    
    # --- DB Logic (履歴表示用) ---
    db_trigger <- reactiveVal(0)
    
    # 保存はサイドバーで行うが、更新通知を受け取るためにリセット等はここで管理
    observeEvent(input$reset_db, {
      tryCatch({
        reset_db()
        showNotification("DBを初期化しました", type="warning")
        shinyjs::runjs("history.go(0)")
      }, error = function(e) { showNotification(paste("エラー:", e$message), type="error") })
    })
    
    history_df <- reactive({ input$reset_db; get_scenario_history() })
    output$history_table <- renderDT({ df <- history_df(); if(nrow(df)==0) return(NULL); datatable(df, selection="single", options=list(pageLength=5, scrollX=TRUE, order=list(0, 'desc'))) })
    
    # --- Plots ---
    output$monthly_cf_plot <- renderPlot({ df <- sim_data() %>% dplyr::mutate(S=ifelse(Monthly_Net_Cash_Flow>=0,"P","M")); ggplot(df, aes(Year, Monthly_Net_Cash_Flow)) + geom_col(aes(fill=S), alpha=0.8) + scale_fill_manual(values=c("P"="#2980b9","M"="#c0392b")) + geom_hline(yintercept=0) + scale_y_continuous(labels=comma) + labs(y="月次収支 (円)", x="経過年数") + theme_minimal(base_family="Noto Sans JP") + theme(legend.position="none") })
    output$cf_plot <- renderPlot({ df <- sim_data() %>% dplyr::mutate(S=ifelse(Cumulative_Cash_Flow>=0,"P","N")); ggplot(df, aes(Year, Cumulative_Cash_Flow/10000)) + geom_col(aes(fill=S), alpha=0.8) + scale_fill_manual(values=c("P"="#3498db","N"="#95a5a6")) + geom_hline(yintercept=0) + scale_y_continuous(labels=comma) + labs(y="累積CF (万円)", x="経過年数") + theme_minimal(base_family="Noto Sans JP") + theme(legend.position="none") })
    output$irr_plot <- renderPlot({ df <- sim_data(); ggplot(df, aes(x=Year, y=Estimated_IRR)) + geom_line(color="#e67e22", size=1.5) + geom_point(size=3, color="#e67e22") + geom_hline(yintercept=0, linetype="dashed") + scale_y_continuous(labels=percent) + labs(y="想定IRR", x="経過年数") + theme_minimal(base_family="Noto Sans JP") })
    output$rate_plot <- renderPlot({ df_r <- sim_data() %>% select(Year, Asset_Change_Rate, Liability_Reduction_Rate) %>% pivot_longer(-Year); ggplot(df_r, aes(Year, value, color=name)) + geom_line(size=1.5) + scale_color_manual(values=c("Liability_Reduction_Rate"="#3498db", "Asset_Change_Rate"="#e74c3c"), labels=c("負債減少率", "資産変動率")) + labs(y="年間変化率 (%)", x="経過年数") + theme_minimal(base_family="Noto Sans JP") + theme(legend.position="bottom") })
    output$bs_plot <- renderPlot({ df_l <- sim_data() %>% select(Year, Asset_Value, Liability) %>% pivot_longer(-Year); ggplot(df_l, aes(Year, value/10000, color=name)) + geom_line(size=1.2) + scale_color_manual(values=c("Asset_Value"="#2c3e50", "Liability"="#e74c3c"), labels=c("物件価値", "ローン残債")) + scale_y_continuous(labels=comma) + labs(y="金額 (万円)", x="経過年数") + theme_minimal(base_family="Noto Sans JP") + theme(legend.position="bottom") })
    output$profit_plot <- renderPlot({ ggplot(sim_data(), aes(Year, Mansion_Profit/10000)) + geom_col(fill="#27ae60", alpha=0.7) + geom_hline(yintercept=0) + scale_y_continuous(labels=comma) + labs(y="総合収益 (万円)", x="経過年数") + theme_minimal(base_family="Noto Sans JP") })
    output$payment_plot <- renderPlot({ ggplot(sim_data(), aes(Year, Monthly_Payment_Example)) + geom_line(color="#8e44ad", size=1.2) + scale_y_continuous(labels=comma) + labs(y="月々返済額 (円)", x="経過年数") + theme_minimal(base_family="Noto Sans JP") })
    output$raw_table <- renderDT({ df <- sim_data() %>% mutate(実効家賃_万=round(Effective_Annual_Rent/10000,1), 月次収支_円=round(Monthly_Net_Cash_Flow,0), 累積CF_万=round(Cumulative_Cash_Flow/10000,1), 残債_万=round(Liability/10000,1), 売却手残り_万=round(Net_Exit_Cash_Sale/10000,1), 総合収益_万=round(Mansion_Profit/10000,1), 想定IRR=paste0(sprintf("%.2f",Estimated_IRR*100)," %")) %>% select(Year, 実効家賃_万, 月次収支_円, 累積CF_万, 残債_万, 売却手残り_万, 総合収益_万, 想定IRR); datatable(df, options=list(pageLength=10, scrollX=TRUE)) })
    
    return(reactive(NULL))
  })
}
