-- Exercícios de patterns
teams :: [a] -> ([a], [a])
teams [] = ([], [])
teams [x] = ([], [])
teams (x:y:xs) = (x:lxs, y:rxs)
        where (lxs, rxs) = teams xs

