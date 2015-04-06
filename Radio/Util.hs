{-# LANGUAGE OverloadedStrings #-}
module Radio.Util(
    getElementPosition
  , styleBlock
  , getMousePosition
  , cbutton
  , cbuttonM
  ) where

import Haste
import Haste.Foreign
import Haste.App (MonadIO)
import Haste.Prim
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Haste.Perch (ToElem, Perch, nelem, child)
import Haste.HPlay.View hiding (head)
import Prelude hiding (id)

newtype JQuery = JQuery JSAny
newtype JPosition = JPosition JSAny
newtype JMouse = JMouse JSAny

js_jquery :: JSString -> IO JQuery
js_jquery v = JQuery <$> ffi "(function(e) {return $(e);})" v

js_offset :: JQuery -> IO JPosition
js_offset (JQuery jsany) = JPosition <$> ffi "(function(jq) {return jq.offset();})" jsany

js_left :: JPosition -> IO Int
js_left (JPosition jsany) = ffi "(function(jp) {return jp.left;})" jsany

js_top :: JPosition -> IO Int
js_top (JPosition jsany) = ffi "(function(jp) {return jp.top;})" jsany

js_mouse :: IO JMouse
js_mouse = JMouse <$> ffi "(function() {return mouse;})"

js_mouse_x :: JMouse -> IO Int
js_mouse_x (JMouse jsany) = ffi "(function(m) {return m.x;})" jsany

js_mouse_y :: JMouse -> IO Int
js_mouse_y (JMouse jsany) = ffi "(function(m) {return m.y;})" jsany

-- | Since we can't name it '$', let's just call it 'j'.
j :: JSString -> (JQuery -> IO a) -> IO a
j s action = js_jquery s >>= action

-- | Returns element position
getElementPosition :: String -> IO (Int, Int)
getElementPosition sel = j (toJSStr sel) $ \jq -> do
  pos <- js_offset jq
  xpos <- js_left pos
  ypos <- js_top pos
  return (xpos, ypos) 

getMousePosition :: IO (Int, Int)
getMousePosition = do
  m <- js_mouse
  x <- js_mouse_x m 
  y <- js_mouse_y m
  return (x, y)

styleBlock :: ToElem a => a -> Perch 
styleBlock cont = nelem  "style" `child` cont

-- | active button. When clicked, return the first parameter
cbutton :: a -> String -> Widget a
cbutton x slabel= static $ do
        button slabel ! id slabel ! atr "class" "btn btn-primary" ! atr "type" "button" ! atr "style" "margin-right: 10px; margin-left: 10px; margin-top: 3px" `pass` OnClick
        return x
      `continuePerch` slabel

cbuttonM :: IO a -> String -> Widget a
cbuttonM x slabel= static $ do
        button slabel ! id slabel ! atr "class" "btn btn-primary" ! atr "type" "button" ! atr "style" "margin-right: 10px; margin-left: 10px; margin-top: 3px" `pass` OnClick
        liftIO x
      `continuePerch` slabel