let solve(a,b,c) =
    let d = b*b-4.0*a*c
    if d < 0.0 || a = 0.0
    then failwith "Discriminant is negative or a=0.0"
    else let sqrtD = sqrt d
         ((-b+sqrtD)/(2.0*a),(-b-sqrtD)/(2.0*a));;