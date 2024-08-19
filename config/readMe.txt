config {
  user {
  repeatCount = "3"
  helpMenu = "Hello! I am echo-bot.\nPossible command : /help, /repeat\nWhat about me? Good to     meet you!"
  repeatMenu = "Number of repeats = "
  }
  url {
    apiPath = "/bot"
    botHost = "api.telegram.org"
    timeout = "10"
    offset = "-1"
    token = "_"
    port = "443"
    method = "GET"
    secure = on
  } 
  telegramMode = off
  lvlLog = "Debug"
}
