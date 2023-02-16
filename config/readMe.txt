fill the gap and rename this file to "bot.cfg"

config {
  user {
  repeatcount = "3"
  helpmenu = "Hello! I am echo-bot.\nPossible command : /help, /repeat\nWhat about me? Good to     meet you!"
  repeatmenu = "Number of repeats = "
  }
  url {
    apipath = "/bot"
    bothost = "api.telegram.org"
    timeout = "10"
    offset = "-1"
    token = "_"
    port = "443"
    method = "GET"
    secure = on
  } 
  telegrammode = off
  lvlLog = "Debug"
}
