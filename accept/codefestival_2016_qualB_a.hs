main = interact $
        show . foldr (\(l,r) a ->
            if l /= r then succ a else a) 0
            . (zip "CODEFESTIVAL2016")
