{-# LANGUAGE TemplateHaskell #-}
module LensDemo where

import Control.Lens


data BottomItem = BottomItem { _str :: String }
  deriving (Show, Eq)
data MiddleItem = MiddleItem { _bottomItem :: BottomItem }
  deriving (Show, Eq)
data TopItem = TopItem { _middleItem :: MiddleItem }
  deriving (Show, Eq)
  
makeLenses ''BottomItem
makeLenses ''MiddleItem
makeLenses ''TopItem

container = [ TopItem (MiddleItem (BottomItem "ABC"))
            , TopItem (MiddleItem (BottomItem "CDE"))]

expected = [ TopItem (MiddleItem (BottomItem "XYZ"))
           , TopItem (MiddleItem (BottomItem "XYZ"))]
            
bottomItemLens = traverse.middleItem.bottomItem.str
            
container' = set bottomItemLens "XYZ" container
test = print (expected == container')