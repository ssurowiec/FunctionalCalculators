open System

type Operator =
    | Add
    | Subtract
    | Multiply
    | Divide

module Operator =
    let toChar =
        function
        | Add -> '+'
        | Subtract -> '-'
        | Multiply -> '*'
        | Divide -> '/'


printfn "Set calculation: "
let calculation = Console.ReadLine()
let trimmed = calculation.Trim()
type Evaluation = int -> Operator -> int -> int
module Evaluation =
    let compute: Evaluation =
        fun a op b ->
            match op with
            | Add -> a + b
            | Divide -> a / b
            | Subtract -> a - b
            | Multiply -> a * b
let rec getAllCalculationOperators (operators: Operator list) (rawComputation: string) =
    let charListToString (l:char list) =
        System.String.Concat(l |> List.toSeq);
    match rawComputation |> Seq.toList with
    | [] -> operators
    | '+' :: rest -> getAllCalculationOperators (operators @ [ Operator.Add ]) (rest |> charListToString)
    | '-' :: rest -> getAllCalculationOperators (operators @ [ Operator.Subtract ]) (rest |> charListToString)
    | '*' :: rest -> getAllCalculationOperators (operators @ [ Operator.Multiply ]) (rest |> charListToString)
    | '/' :: rest -> getAllCalculationOperators (operators @ [ Operator.Divide ]) (rest |> charListToString)
    | _ :: rest -> getAllCalculationOperators operators (rest |> charListToString)

let getAllNumbers (rawComputation: string) (operators: Operator list) : int list =
    let rawOperators = operators |> List.map (Operator.toChar)

    let rawNumbers = rawComputation.Split(rawOperators |> List.toArray)
    rawNumbers |> Array.map int |> Array.toList

let operators = getAllCalculationOperators List.empty trimmed
let numbers = getAllNumbers trimmed operators

let firstNumber = numbers |> List.head
let allComputations = List.zip operators (numbers |> List.tail)

let result =
    allComputations
    |> List.fold (fun a (op, b) -> Evaluation.compute a op b) firstNumber


printfn "result is : %i" result
