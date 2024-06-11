open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Operator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Power

module Operator =
    module Signs =
        [<Literal>]
        let Add = '+'

        [<Literal>]
        let Subtract = '-'

        [<Literal>]
        let Multiply = '*'

        [<Literal>]
        let Divide = '/'

        [<Literal>]
        let Power = '^'

        [<Literal>]
        let OpenBracket = '('

        [<Literal>]
        let CloseBracket = ')'

    let toChar =
        function
        | Add -> Signs.Add
        | Subtract -> Signs.Subtract
        | Multiply -> Signs.Multiply
        | Divide -> Signs.Divide
        | Power -> Signs.Power


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
            | Power -> pown a b

let rec computeCalculation (rawCalculation: string) =
    let splitComputation = rawCalculation.Replace(" ", String.Empty)

    let rec computeEvaluation (numberBuffer: string) (prevEval: int -> int) (c: char list) =
        match c with
        | Operator.Signs.Add :: rest ->
            let number = numberBuffer |> int
            let calculation = Evaluation.compute (prevEval number) Operator.Add
            computeEvaluation String.Empty calculation rest
        | Operator.Signs.Subtract :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Subtract
            computeEvaluation String.Empty ev rest
        | Operator.Signs.Multiply :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Multiply
            computeEvaluation String.Empty ev rest
        | Operator.Signs.Divide :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Divide
            computeEvaluation String.Empty ev rest
        | Operator.Signs.Power :: rest ->
            let number = numberBuffer |> int
            let ev = Evaluation.compute (prevEval number) Operator.Power
            computeEvaluation String.Empty ev rest
        | Operator.Signs.OpenBracket :: rest ->
            let nestedCalculation = computeCalculation (String.Join(String.Empty, rest))

            rest
            |> List.findIndex (fun c -> c = Operator.Signs.CloseBracket)
            |> (fun index -> rest |> List.splitAt index)
            |> snd
            |> List.tail
            |> computeEvaluation (nestedCalculation.ToString()) prevEval
        | Operator.Signs.CloseBracket :: _ -> numberBuffer |> int |> prevEval
        | d :: rest when Char.IsDigit(d) -> computeEvaluation $"{numberBuffer}{d}" prevEval rest
        | _ :: rest -> computeEvaluation numberBuffer prevEval rest
        | [] -> numberBuffer |> int |> prevEval

    computeEvaluation String.Empty id (splitComputation |> Seq.toList)

let result2 = computeCalculation calculation
printfn "Result is: %i" result2
