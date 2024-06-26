{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Todo ( todo ) where

import Data.String.QQ
import Data.Text

todo :: Text
todo = [s|
たいまつ Lv 1 (Y ぼたんで たいまつを おく)
てんき Lv 1 (じかんがたつと　くらくなったり　あかるくなったり　する)
きをつくる Lv 0
かくちょう Lv 2 (きょうかいが　せっていされた)
あいてむあつめ Lv 0
てきすと Lv 1 (X ぼたんで めにゅー)
おと Lv 1
|]
