let f x = sin(x) * sin(x)

let a = 0.0
let b = 1.0
let n = 10
let eps = 0.000001

let factorial num =
    [1..num] |> Seq.fold (fun acc n -> acc * n) 1
// Define a function to compute f using naive taylor series method
let taylor_naive x = 
    let rec naive n acc term =
        if abs term < eps then acc, n
        else
            let term: float = ((-1.0) ** (n - 1.0)) * (2.0 ** (2.0 * n - 1.0)) * (x ** (2.0 * n)) / float(factorial (2 * int n))
            naive (n + 1.0) (acc + term) term
    naive 1. (0.) (2.0 * x ** 2.0 / 2.0)

let taylor x = 
    let rec taylor_0 n acc term = 
        if abs term < eps then acc, n
        else
          let term = term * (-1.0) * (4.0) * (x ** 2.0) / (2.0 * n + 1.0) / (2.0 * n + 2.0)
          taylor_0 (n + 1.0) (acc + term) term
    taylor_0 1. (2.0 * x ** 2.0 /  2.0) (2.0 * x ** 2.0 / 2.0)

let main =
   for i = 0 to n do
    let x = a + (float i) / (float n) * (b-a)
    let naive, terms1 = taylor_naive x
    let taylor, terms2 = taylor x
    printfn "%5.2f   %10.6f  %10.6f  %.0f  %10.6f  %.0f" x (f x) naive terms1 taylor terms2

main