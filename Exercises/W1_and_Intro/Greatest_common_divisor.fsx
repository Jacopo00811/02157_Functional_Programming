let rec gcd = function
| (0,n) -> n
| (m,n) -> gcd(n % m,m);;
