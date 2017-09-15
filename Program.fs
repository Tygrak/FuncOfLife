open System

let mutable a = Array2D.init 50 50 (fun x y -> (0))

let randomFillArray arr =
    let rand = System.Random()
    arr |> Array2D.map (fun v -> (rand.Next 2))

let getSquare (arr:int[,]) (x:int) (y:int) =
    if (x<0 || x>=(Array2D.length1 arr) || y<0 || y>=(Array2D.length2 arr)) then 0
    else arr.[x, y]

let countNeighbors (arr:int[,]) (x:int) (y:int) =
    getSquare arr (x+1) (y+1) + getSquare arr (x+1) (y) + getSquare arr (x+1) (y-1) + getSquare arr (x) (y-1)
    + getSquare arr (x-1) (y-1) + getSquare arr (x-1) (y) + getSquare arr (x-1) (y+1) + getSquare arr (x) (y+1)

let squareNextGeneration neighbors previous =
    if (neighbors < 2 || neighbors > 3) then 0
    elif (neighbors = 3) then 1
    else previous

let arrayNeighborCounts arr =
    Array2D.init (Array2D.length1 arr) (Array2D.length2 arr) (fun x y -> (countNeighbors arr x y))

let nextGeneration arr =
    arr
    |> arrayNeighborCounts 
    |> (fun (arr1:int[,]) (arr2:int[,]) -> Array2D.init (Array2D.length1 arr1) (Array2D.length2 arr1) (fun x y -> (squareNextGeneration arr2.[x,y] arr1.[x,y]))) arr

let neatPrintGame arr =
    Console.Clear ()
    for i = 0 to (Array2D.length1 arr)-1 do
        for j = 0 to (Array2D.length2 arr)-1 do
            match arr.[i, j] with
            | 0 -> printf " "
            | _ -> printf "▓"
        printf "\n"
        
[<EntryPoint>]
let main argv =
    //Mandel.mandelbrot 60 60 1000 (Numerics.Complex(-0.775, 0.0)) 2.5 |> Mandel.neatPrintMandelbrot
    
    a <- randomFillArray a
    for i = 0 to 10000 do
        a <- nextGeneration a
        neatPrintGame a
        Threading.Thread.Sleep(500)
    
    
    0