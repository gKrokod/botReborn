module Config (loadConfig) where

import Data.Bool (bool)
import qualified Data.Configurator as C (Worth (Required), load, lookupDefault)
import Data.Text as T (Text, toLower, unpack)
import qualified Data.Text.Encoding as E (encodeUtf8)
import Types (Config (..), Log (..), Mode (ConsoleBot, TelegramBot))

loadConfig :: IO Config
loadConfig = do
  conf <- C.load [C.Required "config/bot.cfg"]
  rcount <- C.lookupDefault "NotFoundRepeatCount.cfg" conf "config.user.repeatCount"
  helpmenu <- C.lookupDefault "NotFoundHelpMenu.cfg" conf "config.user.helpMenu"
  repeatmenu <- C.lookupDefault "NotFoundRepeatMenu.cfg" conf "config.user.repeatMenu"
  apipath <- C.lookupDefault "NotFoundApiPath.cfg" conf "config.url.apiPath"
  bothost <- C.lookupDefault "NotFoundBotHost.cfg" conf "config.url.botHost"
  timeout <- C.lookupDefault "NotFoundTimeOut.cfg" conf "config.url.timeout"
  offset <- C.lookupDefault "NotFoundOffset.cfg" conf "config.url.offset"
  token <- C.lookupDefault "NotFoundToken.cfg" conf "config.url.token"
  port <- C.lookupDefault "NotFoundPort.cfg" conf "config.url.port"
  method <- C.lookupDefault "NotFoundMethod.cfg" conf "config.url.method"
  secure <- C.lookupDefault False conf "config.url.secure"
  mode <- C.lookupDefault False conf "config.telegramMode"
  lvlLog <- C.lookupDefault "DEBUG" conf "config.lvlLog"
  return
    Config
      { cRepeatCount = read $ T.unpack rcount,
        cTextMenuHelp = helpmenu,
        cTextMenuRepeat = repeatmenu,
        cApiPath = E.encodeUtf8 apipath,
        cBotHost = E.encodeUtf8 bothost,
        cTimeOut = E.encodeUtf8 timeout,
        cOffset = E.encodeUtf8 offset,
        cToken = E.encodeUtf8 token,
        cPort = read $ T.unpack port,
        cMethod = E.encodeUtf8 method,
        cSecure = secure,
        cMode = bool ConsoleBot TelegramBot mode,
        cLvlLog = lvlLogFromText lvlLog
      }

lvlLogFromText :: T.Text -> Log
lvlLogFromText t = case T.toLower t of
  "debug" -> Debug
  "warning" -> Warning
  "error" -> Error
  "fatal" -> Fatal
  _ -> error "config file"
