-- The Gilded Rose
-- A rather silly exercise where different categories of items decay at different rates

data Category = Legendary | Generic | Aged

data Item = Item {
  name        :: String,   -- What we call it
  sellIn      :: Int,      -- Days before the item must be sold
  initQuality :: Int,      -- Intial quality or value
  category    :: Category, -- What kind is it?
  isConjured  :: Bool      -- Conjured items decay twice as quickly
}

main = do
  let agedBrie = Item "Brie" 10 10 Aged False
  putStrLn $ show $ quality agedBrie 5

-- Compute the present value of an item after a number of days
quality :: Item -> Int -> Int

-- Legendary items retain their value
quality item@(Item {category=Legendary, initQuality=q}) _ = q

-- Generic items decay daily
quality item@(Item {category=Generic, sellIn=d, initQuality=q, isConjured=c}) daysPassed =
  let
    multiplier       = if c then 2 else 1
    daysPastSellBy   = daysPassed - d
    daysBeforeSellBy = daysPassed - daysPastSellBy
    presentQuality   = q - (daysBeforeSellBy * multiplier) - (daysPastSellBy * 2 * multiplier)
  in if presentQuality < 0 then 0 else presentQuality

-- Aged items gain value until they expire
quality item@(Item {category=Aged, sellIn=d, initQuality=q}) daysPassed
  | daysPassed > d      = 0
  | d - daysPassed < 5  = q + 3
  | d - daysPassed < 10 = q + 2
  | otherwise           = q