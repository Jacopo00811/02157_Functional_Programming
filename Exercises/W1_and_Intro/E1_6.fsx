let rec sum = function
 | (m, 0) -> m
 | (m, n) -> sum(m,n-1)+(m+n);;