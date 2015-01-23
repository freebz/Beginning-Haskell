--
-- Chapter 6
-- Knowing Your Clients Using Monads
--

-- Discovering Monads

-- Watching out for Incomplete Data

meanPurchase :: Integer -- the client identifier
             -> Double  -- the mean purchase
meanPurchase clientId = let p = purchasesByClientId clientId
                         in foldr (+) 0.0 $ catMaybes $ map purchasesValue p


purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  case numberItemsByPurchaseId purchaseId of
    Nothing -> Nothing
    Just n  -> case productIdByPurchaseId purchaseId of
                 Nothing        -> Nothing
                 Just productId -> case priceByProductId productId of
                                     Nothing    -> Nothing
                                     Just price -> Just $ (fromInteger n) * price


thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _  = Nothing
thenDo (Just x) f = f x

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
  numberItemsByPurchaseId purchaseId `thenDo` (\n ->
  productIdByPurchaseId purchaseId   `thenDo` (\productId ->
  priceByProductId productId         `thenDo` (\price ->
  Just $ fromInteger n * price       )))
