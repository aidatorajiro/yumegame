{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Todo ( todo ) where

import Data.String.QQ
import Data.Text

todo :: Text
todo = [s|
たいまつ Lv 0
てんき Lv 1
きをつくる Lv 0
かくちょう Lv 0
あいてむあつめ Lv 0
てきすと Lv 1
おと Lv 1
|]
