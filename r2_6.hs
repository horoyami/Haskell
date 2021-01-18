antiantiintercalate :: [(Int, a)] -> [a]
antiantiintercalate [] = []
antiantiintercalate ((k, x):as) = take k (repeat x) ++ antiantiintercalate as