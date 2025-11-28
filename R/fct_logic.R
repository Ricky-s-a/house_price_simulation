# R/fct_logic.R

calculate_amortization <- function(principal, interest_rate, years, method) {
  monthly_rate <- interest_rate / 12 / 100
  n_payments <- years * 12
  
  schedule <- data.frame(
    Month = 1:n_payments,
    Payment = numeric(n_payments),
    Interest = numeric(n_payments),
    Principal_Paid = numeric(n_payments),
    Balance = numeric(n_payments)
  )
  
  balance <- principal
  
  fixed_principal_payment <- if(method == "元本均等返済") principal / n_payments else 0
  
  fixed_total_payment <- if(method == "元利均等返済") {
    if (monthly_rate == 0) principal / n_payments
    else principal * (monthly_rate * (1 + monthly_rate)^n_payments) / ((1 + monthly_rate)^n_payments - 1)
  } else 0
  
  for (i in 1:n_payments) {
    interest_payment <- balance * monthly_rate
    
    if (method == "元利均等返済") {
      payment <- fixed_total_payment
      principal_payment <- payment - interest_payment
    } else { 
      principal_payment <- fixed_principal_payment
      payment <- principal_payment + interest_payment
    }
    
    balance <- balance - principal_payment
    
    schedule$Payment[i] <- payment
    schedule$Interest[i] <- interest_payment
    schedule$Principal_Paid[i] <- principal_payment
    schedule$Balance[i] <- max(0, balance)
  }
  
  schedule %>%
    dplyr::mutate(Year = ceiling(Month / 12)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      Balance_End = dplyr::last(Balance),
      Total_Principal_Paid = sum(Principal_Paid),
      Total_Interest_Paid = sum(Interest),
      Total_Payment_Year = sum(Payment),
      Monthly_Payment_Example = dplyr::first(Payment),
      .groups = 'drop'
    )
}

run_simulation_logic <- function(p) {
  # 諸費用ローン込の場合、借入額に諸費用を加算
  loan_amount <- (p$price * 10000) - (p$down_payment * 10000)
  if(isTRUE(p$include_cost_in_loan)) {
    loan_amount <- loan_amount + p$initial_cost_yen
  }
  if(loan_amount < 0) loan_amount <- 0
  
  loan_schedule <- calculate_amortization(
    principal = loan_amount,
    interest_rate = p$interest_rate,
    years = p$loan_years,
    method = p$repayment_method
  )
  
  implied_rate <- 0
  if (p$drop_type == "target_price") {
    P0 <- p$price * 10000
    Pt <- p$target_price_val * 10000
    t <- max(1, p$target_year)
    implied_rate <- (Pt / P0)^(1/t) - 1
  }
  
  # 初期投資額: 諸費用ローン込なら頭金のみ、そうでなければ頭金+諸費用
  initial_cash_out <- if(isTRUE(p$include_cost_in_loan)) {
    (p$down_payment * 10000)
  } else {
    p$initial_cost_yen + (p$down_payment * 10000)
  }
  
  df <- loan_schedule %>%
    dplyr::mutate(
      Asset_Value = dplyr::case_when(
        p$drop_type == "fixed_amount" ~ {
          tsubo <- p$area_m2 / 3.30578
          annual_drop_yen <- tsubo * 40000
          pmax(0, (p$price * 10000) - (annual_drop_yen * Year))
        },
        p$drop_type == "rate" ~ {
          (p$price * 10000) * ((1 - p$depreciation_rate / 100) ^ Year)
        },
        p$drop_type == "appreciation" ~ {
          (p$price * 10000) * ((1 + p$appreciation_rate / 100) ^ Year)
        },
        p$drop_type == "target_price" ~ {
          (p$price * 10000) * ((1 + implied_rate) ^ Year)
        },
        TRUE ~ (p$price * 10000)
      ),
      Liability = Balance_End,
      
      Base_Annual_Rent = (p$monthly_rent * 10000) * 12 * ((1 - p$rent_decline_rate / 100)^(Year - 1)),
      Effective_Annual_Rent = Base_Annual_Rent * (p$occupancy_rate / 100),
      Annual_Running_Cost = ((p$mgmt_fee + p$repair_fund) * 10000) * 12,
      
      Yearly_Cash_Flow = Effective_Annual_Rent - Total_Payment_Year - Annual_Running_Cost,
      Monthly_Net_Cash_Flow = Yearly_Cash_Flow / 12,
      Cumulative_Cash_Flow = cumsum(Yearly_Cash_Flow) - initial_cash_out,
      
      Mansion_Profit = Cumulative_Cash_Flow + (Asset_Value - Liability),
      Monthly_Dividend = Mansion_Profit / (Year * 12),
      
      Prev_Asset = dplyr::lag(Asset_Value, default = p$price * 10000),
      Prev_Liability = dplyr::lag(Liability, default = loan_amount),
      Asset_Change_Rate = (Prev_Asset - Asset_Value) / Prev_Asset * 100,
      Liability_Reduction_Rate = dplyr::if_else(Prev_Liability > 0, Total_Principal_Paid / Prev_Liability * 100, 0),
      
      Total_Exit_Wealth = Mansion_Profit + initial_cash_out,
      Estimated_IRR = (Total_Exit_Wealth / initial_cash_out)^(1/Year) - 1,
      Net_Exit_Cash_Sale = Asset_Value - Liability,
      Total_Return_Amount = Cumulative_Cash_Flow + Net_Exit_Cash_Sale
    )
  
  attr(df, "implied_rate") <- implied_rate
  return(df)
}