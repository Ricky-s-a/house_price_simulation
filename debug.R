# 1. 現在のアカウントとアプリ名を確認（デプロイ設定があるフォルダで実行）
# ※ app.R があるディレクトリにいることを確認してください
deployments <- rsconnect::deployments(".")
app_name <- deployments$name[1]
account_name <- deployments$account[1]

# 2. サーバー上で動いているタスクの一覧を取得
tasks <- rsconnect::tasks(account = account_name)
print(tasks)

# 3. リストの中に "running" や "building" などのステータスのタスクがあるはずです。
# そのタスクIDを指定して強制終了します。
# (例: タスクIDが 123456 の場合)
# rsconnect::terminateTask(taskId = 123456, account = account_name)

# 自動で最新のタスクをkillする場合のワンライナー:
if(nrow(tasks) > 0) {
  rsconnect::terminateTask(taskId = tasks$task_id[1], account = account_name)
  message("Task terminated.")
} else {
  message("No active tasks found.")
}