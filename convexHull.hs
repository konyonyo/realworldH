import Data.List

data Point2D = Point2D { x::Int, y::Int } deriving (Eq, Show)
data Direction = CounterClockWise | ClockWise | Collinier deriving (Eq, Show)

-- 線分abの延長線上に点cがあるならば、Collinier
-- 線分abの延長線の左側に点cがあるならば、CounterClockWise
-- 線分abの延長線の右側に点cがあるならば、ClockWise
-- ベクトルabとベクトルbcの外積のz座標の符号で上記を判定する
direction :: Point2D -> Point2D ->Point2D -> Direction
direction a b c
    | crossProductZ > 0 = CounterClockWise
    | crossProductZ < 0 = ClockWise
    | otherwise = Collinier
    where
        crossProductZ = abx * bcy - aby * bcx
            where
               abx = (x b) - (x a)
               aby = (y b) - (y a)
               bcx = (x c) - (x b)
               bcy = (y c) - (y b)

-- 平面上の点の集合の凸包をGraham Scanアルゴリズムで求める
convexHull :: [Point2D] -> [Point2D]
convexHull xs = convexHullSub ys
    where
        ys = [p] ++ sortForGrahamScan p pRemoved ++ [p]
            where
                p = startPoint xs
                pRemoved = filter (\x->x/=p) xs

convexHullSub :: [Point2D] -> [Point2D]
convexHullSub (a:b:c:xs)
    | direction a b c == CounterClockWise = a:(convexHullSub (b:c:xs))
    | otherwise = convexHullSub (a:c:xs)
convexHullSub xs = xs

-- 平面上の点をソートする
-- 点pとソートする点を通る直線とx軸の成す角度の昇順でソートする
sortForGrahamScan :: Point2D -> [Point2D] -> [Point2D]
sortForGrahamScan p xs = sortBy compareAngle xs
    where
        compareAngle a b
            | cosVal a < cosVal b = GT
            | otherwise = LT
            where
                cosVal point = (fromIntegral pointX) / sqrt (fromIntegral (pointX * pointX + pointY * pointY))
                    where
                        pointX = (x point) - (x p)
                        pointY = (y point) - (y p)

-- 点pを求める
-- 点pはy座標が最小の点の中で、x座標が最小の点である
startPoint :: [Point2D] -> Point2D
startPoint xs = head (sortByCoordinates xs)
    where
        sortByCoordinates = sortBy compareXY
            where
                compareXY a b
                    | (y a) < (y b) = LT
                    | (y a) > (y b) = GT
                    | (x a) < (x b) = LT
                    | otherwise = GT
