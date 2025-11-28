# R/fct_db.R
library(RPostgres)
library(DBI)
library(dplyr)

# --- DB接続ヘルパー ---
get_db_connection <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_HOST"),
    dbname = Sys.getenv("SUPABASE_DB", "postgres"),
    user = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    port = Sys.getenv("SUPABASE_PORT", 5432)
  )
}

# --- DB初期化 ---
init_db <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  # 1. 物件テーブル (Properties)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS properties (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      price DOUBLE PRECISION,
      area_m2 DOUBLE PRECISION,
      mgmt_fee DOUBLE PRECISION,
      repair_fund DOUBLE PRECISION,
      address TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # 2. シミュレーションテーブル (Simulations)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS simulations (
      id SERIAL PRIMARY KEY,
      property_id INTEGER REFERENCES properties(id) ON DELETE CASCADE,
      name TEXT,
      
      -- 賃貸条件 (シミュレーションごとに変更可能)
      monthly_rent DOUBLE PRECISION,
      rent_decline_rate DOUBLE PRECISION,
      occupancy_rate DOUBLE PRECISION,
      
      -- 諸費用
      initial_cost_mode TEXT,
      initial_cost_val DOUBLE PRECISION,
      
      -- 借入条件
      down_payment DOUBLE PRECISION,
      include_cost_in_loan INTEGER,
      interest_rate DOUBLE PRECISION,
      loan_years INTEGER,
      repayment_method TEXT,
      
      -- 市場予測
      drop_type TEXT,
      depreciation_rate DOUBLE PRECISION,
      appreciation_rate DOUBLE PRECISION,
      target_year INTEGER,
      target_price_val DOUBLE PRECISION,
      
      -- 結果キャッシュ
      gross_yield DOUBLE PRECISION,
      profit_maturity DOUBLE PRECISION,
      max_irr DOUBLE PRECISION,
      
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # ユニーク制約の追加 (マイグレーション)
  tryCatch({
    dbExecute(con, "ALTER TABLE properties ADD CONSTRAINT properties_name_key UNIQUE (name)")
  }, error = function(e) {})
  
  # simulationsにpriceカラム追加 (マイグレーション)
  tryCatch({
    dbExecute(con, "ALTER TABLE simulations ADD COLUMN price DOUBLE PRECISION")
  }, error = function(e) {})
}

# --- DBリセット ---
reset_db <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  if (dbExistsTable(con, "simulations")) dbRemoveTable(con, "simulations")
  if (dbExistsTable(con, "properties")) dbRemoveTable(con, "properties")
  init_db()
}

# --- データ操作関数 ---

# 物件一覧取得
get_property_list <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT id, name, price, area_m2, updated_at FROM properties ORDER BY updated_at DESC")
}

# 物件詳細取得
get_property_by_id <- function(id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT * FROM properties WHERE id = $1", params = list(id))
}

# 全シミュレーション一覧取得 (物件名付き)
get_all_simulation_list <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  sql <- "
    SELECT s.id, s.name as sim_name, p.name as prop_name, 
           COALESCE(s.price, p.price) as price, 
           s.monthly_rent, s.max_irr, s.gross_yield, s.created_at, s.property_id
    FROM simulations s
    JOIN properties p ON s.property_id = p.id
    ORDER BY s.created_at DESC
  "
  dbGetQuery(con, sql)
}

# シミュレーション一覧取得 (物件ID指定) - 互換性のため残す
get_simulation_list <- function(property_id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT id, name, max_irr, gross_yield, created_at FROM simulations WHERE property_id = $1 ORDER BY created_at DESC", params = list(property_id))
}

# シミュレーション詳細取得
get_simulation_by_id <- function(id) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  d <- dbGetQuery(con, "SELECT * FROM simulations WHERE id = $1", params = list(id))
  
  # 論理値変換
  if("include_cost_in_loan" %in% names(d)) {
    d$include_cost_in_loan <- as.logical(d$include_cost_in_loan)
  }
  return(d)
}

# 物件保存 (新規 or 更新)
save_property <- function(inputs, id = NULL) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  name <- inputs$property_name
  if(is.null(name) || name == "") stop("物件名を入力してください")
  
  # 重複チェック
  sql_check <- "SELECT id FROM properties WHERE name = $1"
  params_check <- list(name)
  
  if (!is.null(id) && id != "new") {
    sql_check <- paste0(sql_check, " AND id != $2")
    params_check <- list(name, id)
  }
  
  existing <- dbGetQuery(con, sql_check, params = params_check)
  if (nrow(existing) > 0) {
    stop(paste0("物件名「", name, "」は既に登録されています。別の名前を指定してください。"))
  }
  
  if (is.null(id) || id == "new") {
    # 新規作成
    sql <- "INSERT INTO properties (name, price, area_m2, mgmt_fee, repair_fund) VALUES ($1, $2, $3, $4, $5) RETURNING id"
    res <- dbGetQuery(con, sql, params = list(
      name, inputs$price, inputs$area_m2, inputs$mgmt_fee, inputs$repair_fund
    ))
    return(res$id)
  } else {
    # 更新
    sql <- "UPDATE properties SET name=$1, price=$2, area_m2=$3, mgmt_fee=$4, repair_fund=$5, updated_at=CURRENT_TIMESTAMP WHERE id=$6"
    dbExecute(con, sql, params = list(
      name, inputs$price, inputs$area_m2, inputs$mgmt_fee, inputs$repair_fund, id
    ))
    return(id)
  }
}

# シミュレーション保存
save_simulation <- function(property_id, inputs, results, sim_name) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  if(is.null(sim_name) || sim_name == "") sim_name <- "シミュレーション"
  
  # 諸費用値の取得 (rate or amount)
  ic_mode <- inputs$initial_cost_mode
  ic_val <- if(ic_mode == "rate") inputs$initial_cost_rate else inputs$initial_cost_amount
  
  sql <- "
    INSERT INTO simulations (
      property_id, name, price,
      monthly_rent, rent_decline_rate, occupancy_rate,
      initial_cost_mode, initial_cost_val,
      down_payment, include_cost_in_loan, interest_rate, loan_years, repayment_method,
      drop_type, depreciation_rate, appreciation_rate, target_year, target_price_val,
      gross_yield, profit_maturity, max_irr
    ) VALUES (
      $1, $2, $3,
      $4, $5, $6,
      $7, $8,
      $9, $10, $11, $12, $13,
      $14, $15, $16, $17, $18,
      $19, $20, $21
    )
  "
  
  params <- list(
    property_id, sim_name, inputs$price,
    inputs$monthly_rent, inputs$rent_decline_rate, inputs$occupancy_rate,
    ic_mode, ic_val,
    inputs$down_payment, as.integer(inputs$include_cost_in_loan), inputs$interest_rate, inputs$loan_years, inputs$repayment_method,
    inputs$drop_type, inputs$depreciation_rate, inputs$appreciation_rate, inputs$target_year, inputs$target_price_val,
    (inputs$monthly_rent * 12) / inputs$price * 100, # Gross Yield
    results %>% dplyr::filter(Year == inputs$loan_years) %>% dplyr::pull(Mansion_Profit) %>% tail(1), # Profit
    max(results$Estimated_IRR, na.rm=TRUE) # IRR
  )
  
  # NULL補正
  params <- lapply(params, function(x) if(is.null(x)) NA else x)
  
  dbExecute(con, sql, params = params)
}