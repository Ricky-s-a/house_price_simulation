# R/mod_kpi.R

kpiUI <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 4, 4, 4, 4, 4),
    value_box(title = "表面利回り (Gross)", value = textOutput(ns("gross_yield")), showcase = bsicons::bs_icon("percent"), theme = "primary", p("※初期家賃ベース", style = "font-size: 0.8rem;")),
    value_box(title = "実質利回り (Net/NOI)", value = textOutput(ns("net_yield")), showcase = bsicons::bs_icon("clipboard-check"), theme = "primary", p("経費・空室リスク控除後", style = "font-size: 0.8rem;")),
    value_box(title = "月次収支 (初年度)", value = textOutput(ns("monthly_cf_display")), showcase = bsicons::bs_icon("piggy-bank"), theme = "teal", p("実効家賃 - (返済 + 管理修繕)", style = "font-size: 0.8rem;"), p("※家賃変動により推移します", style = "font-size: 0.7rem; color: #ecf0f1;")),
    value_box(title = "初回 月々返済額", value = textOutput(ns("monthly_payment_display")), showcase = bsicons::bs_icon("wallet2"), theme = "warning", p("元本 + 利息", style = "font-size: 0.8rem;")),
    value_box(title = "10年後 累積収益", value = textOutput(ns("profit_10y")), showcase = bsicons::bs_icon("graph-up"), theme = "secondary", p("賃料総額 - (購入-売却)", style = "font-size: 0.8rem;")),
    value_box(title = textOutput(ns("maturity_title")), value = textOutput(ns("profit_maturity")), showcase = bsicons::bs_icon("trophy-fill"), theme = "success", p("賃料総額 - (購入-売却)", style = "font-size: 0.8rem;"))
  )
}

kpiServer <- function(id, sim_data, input_params) {
  moduleServer(id, function(input, output, session) {
    
    output$gross_yield <- renderText({
      p <- input_params()
      annual_rent <- p$monthly_rent * 12
      yield <- annual_rent / p$price * 100
      paste0(sprintf("%.2f", yield), " %")
    })
    
    output$net_yield <- renderText({
      p <- input_params()
      annual_rent <- p$monthly_rent * 12 * (p$occupancy_rate / 100)
      annual_cost <- (p$mgmt_fee + p$repair_fund) * 12
      yield <- (annual_rent - annual_cost) / p$price * 100
      paste0(sprintf("%.2f", yield), " %")
    })
    
    output$monthly_cf_display <- renderText({
      df <- sim_data()
      paste0(format(round(df$Yearly_Cash_Flow[1] / 12, 0), big.mark=","), " 円")
    })
    
    output$monthly_payment_display <- renderText({
      df <- sim_data()
      paste0(format(round(df$Monthly_Payment_Example[1], 0), big.mark=","), " 円")
    })
    
    output$profit_10y <- renderText({
      df <- sim_data()
      val <- df %>% dplyr::filter(Year == 10) %>% dplyr::pull(Mansion_Profit)
      if(length(val)==0) val <- dplyr::tail(df$Mansion_Profit, 1)
      paste0(format(round(val / 10000, 0), big.mark=","), " 万円")
    })
    
    output$maturity_title <- renderText({
      p <- input_params()
      paste0("完済時 (", p$loan_years, "年後) 収益")
    })
    
    output$profit_maturity <- renderText({
      df <- sim_data()
      p <- input_params()
      val <- df %>% dplyr::filter(Year == p$loan_years) %>% dplyr::pull(Mansion_Profit)
      if(length(val)==0) val <- dplyr::tail(df$Mansion_Profit, 1)
      paste0(format(round(val / 10000, 0), big.mark=","), " 万円")
    })
  })
}