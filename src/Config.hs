module Config (loadConfig) where
import Types (Config (..), Mode (ConsoleBot, TelegramBot), RepeatCount)
import qualified Data.Configurator as C (load, lookupDefault, Worth (Required))
import Data.Text as T (unpack, Text)
import qualified Data.Text.Encoding as E (encodeUtf8)
import Data.Bool (bool)

loadConfig :: IO Config
loadConfig = do
  conf <- C.load [C.Required "config/bot.cfg"]
  rcount<- C.lookupDefault "NotFoundRepeatCount.cfg" conf ("config.user.repeatcount")
  helpmenu <- C.lookupDefault "NotFoundHelpMenu.cfg" conf ("config.user.helpmenu")
  repeatmenu <- C.lookupDefault "NotFoundRepeatMenu.cfg" conf ("config.user.repeatmenu")
  apipath <- C.lookupDefault "NotFoundApiPath.cfg" conf ("config.url.apipath")
  bothost <- C.lookupDefault "NotFoundBotHost.cfg" conf ("config.url.bothost")
  timeout <- C.lookupDefault "NotFoundTimeOut.cfg" conf ("config.url.timeout")
  offset <- C.lookupDefault "NotFoundOffset.cfg" conf ("config.url.offset")
  token <- C.lookupDefault "NotFoundToken.cfg" conf ("config.url.token")
  port <- C.lookupDefault "NotFoundPort.cfg" conf ("config.url.port")
  method <- C.lookupDefault "NotFoundMethod.cfg" conf ("config.url.method")
  secure <- C.lookupDefault False conf ("config.url.secure") -- сделать здесь строку вида "False" для единообразия?
  mode <- C.lookupDefault False conf ("config.telegrammode") -- сделать здесь строку вида "False" для единообразия
  return   Config { 
                    cRepeatCount = read $ T.unpack rcount 
                  , cTextMenuHelp = helpmenu
		  , cTextMenuRepeat = repeatmenu
		  , cApiPath = E.encodeUtf8 apipath
		  , cBotHost = E.encodeUtf8 bothost
		  , cTimeOut = E.encodeUtf8 timeout
		  , cOffset = E.encodeUtf8 offset
                  , cToken = E.encodeUtf8 token
                  , cPort = read $ T.unpack port
                  , cMethod = E.encodeUtf8 method
                  , cSecure = secure
		  , cMode = bool ConsoleBot TelegramBot mode
		  }
