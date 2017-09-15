module Mandel

open System
open System.Numerics

let rec iterateNum maxIterNum iterNum (z:Numerics.Complex) (c:Numerics.Complex) =
    if (z.Imaginary+z.Real) > 2.0 then iterNum
    elif iterNum > maxIterNum then -1
    else iterateNum maxIterNum (iterNum+1) (z*z+c) c

let mapNum (min1:float) (max1:float) (min2:float) (max2:float) (value:float) =
    (value-min1)/(max1-min1) * (max2-min2) + min2

let mandelbrot sizeX sizeY maxIterNum (c:Numerics.Complex) (zoom:float) =
    let mand = Array2D.init sizeX sizeY (fun x y -> (0))
    for i = 0 to (Array2D.length1 mand)-1 do
        for j = 0 to (Array2D.length2 mand)-1 do
            mand.[i, j] <- iterateNum maxIterNum 0 (Complex(0.0, 0.0))
             (Complex(mapNum 0.0 (float (Array2D.length1 mand)) (c.Real-(zoom/2.0)) (c.Real+(zoom/2.0)) (float i), mapNum 0.0 (float (Array2D.length2 mand)) (c.Imaginary-(zoom/2.0)) (c.Imaginary+(zoom/2.0)) (float j)))
            //value = (value-min1)/(max1-min1) * (max2-min2) + min2;
    mand

let neatPrintMandelbrot arr =
    Console.Clear ()
    for j = 0 to (Array2D.length1 arr)-1 do
        for i = 0 to (Array2D.length2 arr)-1 do
            match arr.[i, j] with
            | -1 -> printf "*"
            | _ -> printf " "