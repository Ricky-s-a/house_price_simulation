# R/fct_db.R
library(RSQLite)
library(DBI)

db_file <- "investment_sim.db"

# R/fct_db.R
library(RSQLite)
library(DBI)

db_file <- "investment_sim.db"

# --- DB初期化 ---
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(con))
  
  query <- "
    CREATE TABLE IF NOT EXISTS scenarios (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp TEXT,
      memo TEXT,
      price REAL,
      area_m2 REAL,
      initial_cost_rate REAL,
      monthly_rent REAL,
      rent_decline_rate REAL,
      occupancy_rate REAL,
      mgmt_fee REAL,
      repair_fund REAL,
      down_payment REAL,
      include_cost_in_loan INTEGER,
      interest_rate REAL,
      loan_years INTEGER,
      repayment_method TEXT,
      drop_type TEXT,
      depreciation_rate REAL,
      appreciation_rate REAL,
      target_year INTEGER,
      target_price_val REAL,
      gross_yield REAL,
      profit_maturity REAL,
      max_irr REAL
    )
  "
  dbExecute(con, query)
  
  # 既存テーブルへのカラム追加（マイグレーション）
  tryCatch({
    dbExecute(con, "ALTER TABLE scenarios ADD COLUMN include_cost_in_loan INTEGER DEFAULT 0")
  }, error = function(e) {
    # カラムが既に存在する場合は無視
  })
}

# --- DBリセット ---
reset_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(con))
  if (dbExistsTable(con, "scenarios")) {
    dbRemoveTable(con, "scenarios")
  }
  init_db()
}

# --- 保存 (INSERT) ---
save_scenario_to_db <- function(inputs, results, memo) {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(con))
  
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  get_val <- function(key, default = 0) {
    val <- inputs[[key]]
    if (is.null(val)) return(default)
    return(val)
  }
  
  df <- data.frame(
    timestamp = ts,
    memo = memo,
    price = get_val("price"),
    area_m2 = get_val("area_m2"),
    initial_cost_rate = get_val("initial_cost_rate"),
    monthly_rent = get_val("monthly_rent"),
    rent_decline_rate = get_val("rent_decline_rate"),
    occupancy_rate = get_val("occupancy_rate"),
    mgmt_fee = get_val("mgmt_fee"),
    repair_fund = get_val("repair_fund"),
    down_payment = get_val("down_payment"),
    include_cost_in_loan = as.integer(get_val("include_cost_in_loan", FALSE)),
    interest_rate = get_val("interest_rate"),
    loan_years = get_val("loan_years"),
    repayment_method = get_val("repayment_method", "元利均等返済"),
    drop_type = get_val("drop_type", "rate"),
    depreciation_rate = get_val("depreciation_rate"),
    appreciation_rate = get_val("appreciation_rate"),
    target_year = get_val("target_year"),
    target_price_val = get_val("target_price_val"),
    gross_yield = (get_val("monthly_rent") * 12) / get_val("price") * 100,
    profit_maturity = results %>% dplyr::filter(Year == get_val("loan_years")) %>% dplyr::pull(Mansion_Profit) %>% tail(1),
    max_irr = max(results$Estimated_IRR, na.rm=TRUE)
  )
  
  dbWriteTable(con, "scenarios", df, append = TRUE)
}

# --- 履歴一覧 (SELECT List) ---
get_scenario_history <- function() {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(con))
  # 【変更】monthly_rent を追加
  dbGetQuery(con, "SELECT id, timestamp, memo, price, monthly_rent, gross_yield FROM scenarios ORDER BY id DESC")
}

# --- 単一ロード (SELECT Single) ---
get_scenario_by_id <- function(id) {
  con <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(con))
  d <- dbGetQuery(con, "SELECT * FROM scenarios WHERE id = ?", params = list(id))
  # include_cost_in_loan を論理値に変換
  if("include_cost_in_loan" %in% names(d)) {
    d$include_cost_in_loan <- as.logical(d$include_cost_in_loan)
  } else {
    d$include_cost_in_loan <- FALSE
  }
  return(d)
}