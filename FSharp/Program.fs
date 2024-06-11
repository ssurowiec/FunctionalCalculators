open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

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
type Evaluation = int -> Operator -> int -> int

module Evaluation =
    let compute: Evaluation =
        fun a op b ->
            match op with
            | Add -> a + b
            | Subtract -> a - b
            | Multiply -> a * b
            | Divide -> a / b

type Computation = Evaluation list
let charListToString (l: char list) = String.Concat(l |> List.toSeq)

let rec computeCalculation (rawCalculation: string) =
    let splitComputation =
        rawCalculation.Replace(" ", String.Empty)

    let rec computeEvaluation (numberBuffer: string) (prevEval: int -> int) (c: string) =
        match c |> Seq.toList with
        | '+' :: rest ->
            let number = numberBuffer |> int
            let calculation = Evaluation.compute (prevEval number) Operator.Add
            computeEvaluation String.Empty calculation (rest |> charListToString)
        | '-' :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Subtract
            computeEvaluation String.Empty ev (rest |> charListToString)
        | '*' :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Multiply
            computeEvaluation String.Empty ev (rest |> charListToString)
        | '/' :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Divide
            computeEvaluation String.Empty ev (rest |> charListToString)
        | '(' :: rest ->
            let nestedCalculation = computeCalculation (rest |> charListToString)
            let findClosestParentheses = rest |> List.findIndex (fun c -> c = ')')
            let a = rest |> List.splitAt findClosestParentheses |> snd

            computeEvaluation (nestedCalculation.ToString()) prevEval (a |> List.tail |> charListToString)
        | ')' :: _ -> numberBuffer |> int |> prevEval
        | d :: rest when Char.IsDigit(d) -> computeEvaluation $"{numberBuffer}{d}" prevEval (rest |> charListToString)
        | _ :: rest -> computeEvaluation numberBuffer prevEval (rest |> charListToString)
        | [] -> numberBuffer |> int |> prevEval

    computeEvaluation String.Empty id splitComputation

let result2 = computeCalculation calculation
printfn "Second result is: %i" result2
