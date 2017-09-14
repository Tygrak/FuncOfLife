module Mandel

open System
open System.Numerics

let rec iterateNum maxIterNum iterNum (z : Numerics.Complex) (c : Numerics.Complex) =
    if (z.Imaginary+z.Real) > 2.0 then iterNum
    elif iterNum > maxIterNum then -1
    else iterateNum maxIterNum (iterNum+1) (z*z+c) c

let mandelbrot sizeX sizeY maxIterNum (c : Numerics.Complex) (zoom : float) =
    let mand = Array2D.init sizeX sizeY (fun x y -> (0))
    for i = 0 to (Array2D.length1 mand)-1 do
        for j = 0 to (Array2D.length2 mand)-1 do
            mand.[i, j] <- iterateNum maxIterNum 0
             (Complex(c.Real-zoom/2.0+(zoom/(float (Array2D.length1 mand))*float i), 0.0))
             (Complex(c.Imaginary-zoom/2.0+(zoom/(float (Array2D.length2 mand))*float j), 0.0))
    mand

let neatPrintMandel arr =
    Console.Clear ()
    for i = 0 to (Array2D.length1 arr)-1 do
        for j = 0 to (Array2D.length2 arr)-1 do
            match arr.[i, j] with
            | -1 -> printf "."
            | _ -> printf "_"
        printf "\n"