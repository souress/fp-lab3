# Функциональное программирование. Лабораторная работа №3.


## Цель
Получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой.

## Требования
Требования:

- обязательно должна быть реализована линейная интерполяция (отрезками, link); 
- настройки алгоритма аппроксимирования и выводимых данных должны задаваться через аргументы командной строки:
  - какие алгоритмы использовать (в том числе два сразу); 
  - частота дискретизации результирующих данных; 
  - и т.п.; 
- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x; 
- выходные данные должны подаваться на стандартный вывод; 
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

## Реализация
- Парсинг аргументов коммандной строки
```fsharp
type CommandLineOptions =
    { Step: double
      Window: int
      Algorithm: AlgorithmOption }

let parseAlgorithm (algorithmStringValue: string) : AlgorithmOption =
    match algorithmStringValue with
    | "Lagrange" -> Lagrange
    | "Linear" -> Linear
    | "Both" -> Both
    | _ -> invalidArg "algorithm" "Unexpected algorithm! Possible values: [Lagrange; Linear; Both]"

let parseCommandLine (args: string array) : CommandLineOptions =
    match args with
    | [| step; window; algorithm |] ->
        assert (double step > 0)
        assert (int window >= 2)

        { Step = double step
          Window = int window
          Algorithm = parseAlgorithm algorithm }
    | _ -> failwith $"Expected 3 commandline arguments. Got {args.Length} arguments"
```

- Линейная интерполяция:
```fsharp
let private linearInterpolation (x1: double, y1: double) (x2: double, y2: double) x =
    if x1 = x2 then
        failwith "Division by zero"
    else
        let slope = (y2 - y1) / (x2 - x1)
        let y = y1 + slope * (x - x1)
        y

let linearRun (points: list<double * double>) (targetX: double) =
    linearInterpolation points[0] points[1] targetX
```

- Интерполяция методом Лагранжа:
```fsharp
let private lagrangeInterpolation (points: list<double * double>) j x =
    points
    |> Seq.mapi (fun i (xi, _) -> if i <> j then (x - xi) / (fst points[j] - xi) else 1.0)
    |> Seq.fold (*) 1.0

let lagrangeRun (points: list<double * double>) (targetX: double) =
    points |> Seq.mapi (fun j (_, yi) -> yi * lagrangeInterpolation points j targetX) |> Seq.sum
```

## Опции запуска
```fsharp
dotnet run <step> <window> <Linear|Lagrange|Both>
```

## Ввод в реальном времени
```shell
dotnet run 0.2 4 Both
1;1
2;4
Linear result:           
x               y        
(1.200000;      1.600000)
(1.400000;      2.200000)
(1.600000;      2.800000)
(1.800000;      3.400000)
(2.000000;      4.000000)

3;9
Linear result:           
x               y        
(2.200000;      5.000000)
(2.400000;      6.000000)
(2.600000;      7.000000)
(2.800000;      8.000000)
(3.000000;      9.000000)

4;16
Linear result:            
x               y         
(3.200000;      10.400000)
(3.400000;      11.800000)
(3.600000;      13.200000)
(3.800000;      14.600000)
(4.000000;      16.000000)
                          
Lagrange result:          
x               y         
(1.200000;      1.440000) 
(1.400000;      1.960000)
(1.600000;      2.560000)
(1.800000;      3.240000)
(2.000000;      4.000000)
(2.200000;      4.840000)
(2.400000;      5.760000)
(2.600000;      6.760000)
(2.800000;      7.840000)
(3.000000;      9.000000)
(3.200000;      10.240000)
(3.400000;      11.560000)
(3.600000;      12.960000)
(3.800000;      14.440000)
(4.000000;      16.000000)



```


## Ввод из файла
```shell
cat .\data.txt | dotnet run 0.2 4 Both  

Linear result: 
x               y
(1.200000;      2.200000)
(1.400000;      2.400000)
(1.600000;      2.600000)
(1.800000;      2.800000)
(2.000000;      3.000000)
(2.200000;      3.200000)
(2.400000;      3.400000)
(2.600000;      3.600000)
(2.800000;      3.800000)
(3.000000;      4.000000)

Linear result:
x               y
(3.200000;      4.200000)
(3.400000;      4.400000)
(3.600000;      4.600000)
(3.800000;      4.800000)
(4.000000;      5.000000)
(4.200000;      5.200000)
(4.400000;      5.400000)
(4.600000;      5.600000)
(4.800000;      5.800000)
(5.000000;      6.000000)

Linear result:
x               y
(5.200000;      6.200000)
(5.400000;      6.400000)
(5.600000;      6.600000)
(5.800000;      6.800000)
(6.000000;      7.000000)
(6.200000;      7.200000)
(6.400000;      7.400000)
(6.600000;      7.600000)
(6.800000;      7.800000)
(7.000000;      8.000000)

Lagrange result:
x               y
(1.200000;      2.200000)
(1.400000;      2.400000)
(1.600000;      2.600000)
(1.800000;      2.800000)
(2.000000;      3.000000)
(2.200000;      3.200000)
(2.400000;      3.400000)
(2.600000;      3.600000)
(2.800000;      3.800000)
(3.000000;      4.000000)
(3.200000;      4.200000)
(3.400000;      4.400000)
(3.600000;      4.600000)
(3.800000;      4.800000)
(4.000000;      5.000000)
(4.200000;      5.200000)
(4.400000;      5.400000)
(4.600000;      5.600000)
(4.800000;      5.800000)
(5.000000;      6.000000)
(5.200000;      6.200000)
(5.400000;      6.400000)
(5.600000;      6.600000)
(5.800000;      6.800000)
(6.000000;      7.000000)
(6.200000;      7.200000)
(6.400000;      7.400000)
(6.600000;      7.600000)
(6.800000;      7.800000)
(7.000000;      8.000000)

Linear result:
x               y
(7.200000;      8.200000)
(7.400000;      8.400000)
(7.600000;      8.600000)
(7.800000;      8.800000)
(8.000000;      9.000000)
(8.200000;      9.200000)
(8.400000;      9.400000)
(8.600000;      9.600000)
(8.800000;      9.800000)
(9.000000;      10.000000)

Lagrange result:
(4.600000;      5.600000)
(4.800000;      5.800000)
(5.000000;      6.000000)
(5.200000;      6.200000)
(5.400000;      6.400000)
(5.600000;      6.600000)
(5.800000;      6.800000)
(6.000000;      7.000000)
(6.200000;      7.200000)
(6.400000;      7.400000)
(6.600000;      7.600000)
(6.800000;      7.800000)
(7.000000;      8.000000)
(7.200000;      8.200000)
(7.400000;      8.400000)
(7.600000;      8.600000)
(7.800000;      8.800000)
(8.000000;      9.000000)
(8.200000;      9.200000)
(8.400000;      9.400000)
(8.600000;      9.600000)
(8.800000;      9.800000)
(9.000000;      10.000000)
```