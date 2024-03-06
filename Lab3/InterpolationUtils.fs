module InterpolationUtils

let private lagrangeInterpolation (points: list<double * double>) j x =
    points
    |> Seq.mapi (fun i (xi, _) -> if i <> j then (x - xi) / (fst points[j] - xi) else 1.0)
    |> Seq.fold (*) 1.0

let private linearInterpolation (x1: double, y1: double) (x2: double, y2: double) x =
    if x1 = x2 then
        failwith "Division by zero"
    else
        let slope = (y2 - y1) / (x2 - x1)
        let y = y1 + slope * (x - x1)
        y

let lagrangeRun (points: list<double * double>) (targetX: double) =
    points
    |> Seq.mapi (fun j (_, yi) -> yi * lagrangeInterpolation points j targetX)
    |> Seq.sum

let linearRun (points: list<double * double>) (targetX: double) =
    linearInterpolation points[0] points[1] targetX
