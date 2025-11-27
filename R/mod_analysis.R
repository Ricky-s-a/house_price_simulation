# R/mod_analysis.R

# UI
analysisUI <- function(id) {
  ns <- NS(id)
  navset_card_underline(
    title = "詳細分析レポート",
    full_screen = TRUE,
    
    # [1] 収支・現金 (Cash/PL)
    nav_panel(
      title = "収支・現金 (Cash/PL)",
      icon = icon("money-bill-wave"),
      
      # 【修正ポイント 1】 fill = FALSE を指定して、画面高さに合わせて潰れないようにする
      card_body(
        fill = FALSE, # これが重要です。スクロールを許可します。
        
        card_header("手元資金の推移 (累積キャッシュフロー)"),
        
        layout_columns(
          col_widths = c(6, 6),
          # 【修正ポイント 2】 fill = FALSE で高さをコンテンツに合わせる
          fill = FALSE, 
          
          value_box(
            title = "初期投下資金 (購入時)", 
            value = textOutput(ns("initial_cash_val")), 
            showcase = bsicons::bs_icon("wallet-fill"), 
            theme = "danger", 
            min_height = "180px", # heightではなくmin_heightに変更
            p("諸経費 + 頭金")
          ),
          value_box(
            title = "最終到達額 (CFピーク時)", 
            value = uiOutput(ns("max_cf_detail")), 
            showcase = bsicons::bs_icon("arrow-up-circle"), 
            theme = "success", 
            min_height = "180px", # heightではなくmin_heightに変更
            p("計算式: 累積CF + (売却額 - 残債)", style="font-size: 0.8rem; margin-top: 5px; color: #ecf0f1;")
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
          layout_columns(
            col_widths = c(6, 6),
            fill = FALSE, 
            
            value_box(
              title="IRRプラス転換 (損益分岐)", 
              value=uiOutput(ns("irr_breakeven_detail")), 
              showcase=bsicons::bs_icon("sunrise"), 
              theme="warning", 
              min_height="200px", # min_heightに変更
              p("これ以前に売却すると元本割れ", style="font-size: 0.8rem; color: #ffffff;"), 
              p("計算式: 累積CF + (売却額 - 残債)", style="font-size: 0.7rem; margin-top: 2px; color: #ffffff;")
            ),
            value_box(
              title="最高効率点 (Max IRR)", 
              value=uiOutput(ns("irr_max_detail")), 
              showcase=bsicons::bs_icon("stars"), 
              theme="primary", 
              min_height="200px", # min_heightに変更
              p("資金効率が最も良くなる売り時", style="font-size: 0.8rem; color: #ecf0f1;"), 
              p("計算式: 累積CF + (売却額 - 残債)", style="font-size: 0.7rem; margin-top: 2px; color: #ecf0f1;")
            )
          )
        )
      )
    ),
    
    # その他のタブ
    nav_panel(title="総合収益 (Return)", card_header("マンション収益推移"), plotOutput(ns("profit_plot"), height="400px"), textOutput(ns("cagr_info"))),
    nav_panel(title="返済額推移 (Payment)", card_header("月々の返済額"), plotOutput(ns("payment_plot"), height="400px")),
    nav_panel(title="データ一覧 (Table)", card_header("詳細データ & 売却時IRR"), DTOutput(ns("raw_table")))
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
    output$initial_cash_val <- renderText({ p <- input_params(); initial_cost <- (p$price * 10000 * p$initial_cost_rate / 100) + (p$down_payment * 10000); paste0("-", format(round(initial_cost / 10000, 1), big.mark=","), " 万円") })
    output$max_cf_detail <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Cumulative_Cash_Flow == max(Cumulative_Cash_Flow)) %>% dplyr::slice(1); render_detail_box(max_row, "万円") })
    output$cf_breakdown_text <- renderText({ df <- sim_data(); p <- input_params(); m_rent <- p$monthly_rent * 10000 * (p$occupancy_rate / 100); m_pay <- df$Total_Payment_Year[1] / 12; m_cost <- (p$mgmt_fee + p$repair_fund) * 10000; paste0(format(round(m_rent,0),big.mark=",")," (実効家賃) - ", format(round(m_pay,0),big.mark=",")," (返済) - ", format(round(m_cost,0),big.mark=",")," (管理修繕) = ", format(round(m_rent-m_pay-m_cost,0),big.mark=",")," 円") })
    output$irr_breakeven_detail <- renderUI({ df <- sim_data(); pos_row <- df %>% dplyr::filter(Estimated_IRR > 0) %>% dplyr::slice(1); if(nrow(pos_row) == 0) return(HTML("<div style='font-size: 0.9rem; color: white;'>期間内黒字化なし</div>")); irr_val <- paste0(sprintf("%.2f", pos_row$Estimated_IRR * 100), "%"); render_detail_box(pos_row, irr_val) })
    output$irr_max_detail <- renderUI({ df <- sim_data(); max_row <- df %>% dplyr::filter(Estimated_IRR == max(Estimated_IRR, na.rm=TRUE)) %>% dplyr::slice(1); if(nrow(max_row) == 0) return(NULL); irr_val <- paste0(sprintf("%.2f", max_row$Estimated_IRR * 100), "%"); render_detail_box(max_row, irr_val) })
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
