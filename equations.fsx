let e = 2.718281
let eps = 0.0000001

let rec dichotomy f a b = 
    let mid = (a + b) / 2.0
    if abs(f mid) < eps then mid
    else
        if f(a) * f(b) < 0.0 then 
            if f(mid) * f(a) > 0.0 then dichotomy f mid b
            else dichotomy f a mid
        else mid

let rec iterations phi x0 =
    if abs(x0 - phi(x0)) < eps then x0
    else iterations phi (phi x0)

let rec newthon f f' x0 =
    let phi x= x - f(x)/f'(x)
    iterations phi x0

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = 2.0 * x * sin(x) - cos(x)
let f2 x = e ** x + (1.0 + e ** (2.0 * x)) ** 0.5 - 2.0
let f3 x = log(x) - x + 1.8

let f1' x = 3.0 * sin(x) + 2.0 * x * cos(x)
let f2' x = (e ** x * (e ** (2.0 * x) + 1.0) ** 0.5 + e ** (2.0 * x)) / ((e ** (2.0 * x) + 1.0) ** 0.5)
let f3' x = - (x - 1.0) / x

let phi1 x = x - f1(x) / f1'(x)
let phi2 x = x - f2(x) / f2'(x)
let phi3 x = x - f3(x) / f3'(x)

let main = 
    printfn " | %10.5f | %10.5f | %10.5f|" (dichotomy f1 0.4 1.) (iterations phi1 1.) (newthon f1 f1' 1.)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f2 -1. 0.) (iterations phi2 0.) (newthon f2 f2' 0.)
    printfn " |%10.5f  | %10.5f | %10.5f|" (dichotomy f3 2. 3.) (iterations phi3 3.) (newthon f3 f3' 3.)

 
