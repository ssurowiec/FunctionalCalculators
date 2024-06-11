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
type Evaluation = int -> Operator -> int -> Result<int, string>

module Evaluation =
    let compute: Evaluation =
        fun a op b ->
            match op with
            | Add -> (a + b) |> Ok
            | Subtract -> (a - b) |> Ok
            | Multiply -> (a * b) |> Ok
            | Divide ->
                match b = 0 with
                | true -> Error "Cannot divide by zero"
                | false -> (a / b) |> Ok
            | Power -> (pown a b) |> Ok

let rec computeCalculation (rawCalculation: string) =
    let splitComputation = rawCalculation.Replace(" ", String.Empty)

    let rec computeEvaluation (numberBuffer: string) (prevEval: int -> Result<int, string>) (c: char list) =
        match c with
        | Operator.Signs.Add :: rest ->
            let number = numberBuffer |> int

            let ev =
                fun x -> prevEval number |> Result.bind (fun r -> Evaluation.compute r Operator.Add x)

            computeEvaluation String.Empty ev rest
        | Operator.Signs.Subtract :: rest ->
            let number = numberBuffer |> int

            let ev =
                fun x ->
                    prevEval number
                    |> Result.bind (fun r -> Evaluation.compute r Operator.Subtract x)

            computeEvaluation String.Empty ev rest
        | Operator.Signs.Multiply :: rest ->
            let number = numberBuffer |> int

            let ev =
                fun x ->
                    prevEval number
                    |> Result.bind (fun r -> Evaluation.compute r Operator.Multiply x)

            computeEvaluation String.Empty ev rest
        | Operator.Signs.Divide :: rest ->
            let number = numberBuffer |> int

            let ev =
                fun x -> prevEval number |> Result.bind (fun r -> Evaluation.compute r Operator.Divide x)

            computeEvaluation String.Empty ev rest
        | Operator.Signs.Power :: rest ->
            let number = numberBuffer |> int

            let ev =
                fun x -> prevEval number |> Result.bind (fun r -> Evaluation.compute r Operator.Power x)

            computeEvaluation String.Empty ev rest
        | Operator.Signs.OpenBracket :: rest ->
            let nestedCalculation = computeCalculation (String.Join(String.Empty, rest))

            nestedCalculation
            |> Result.bind (fun nestedResult ->
                rest
                |> List.tryFindIndex (fun c -> c = Operator.Signs.CloseBracket)
                |> Option.map (fun index ->
                    rest
                    |> List.splitAt index
                    |> snd
                    |> List.tail
                    |> computeEvaluation (nestedResult.ToString()) prevEval)
                |> Option.defaultValue (Error "Missing bracket"))
        | Operator.Signs.CloseBracket :: _ -> numberBuffer |> int |> prevEval
        | d :: rest when Char.IsDigit(d) -> computeEvaluation $"{numberBuffer}{d}" prevEval rest
        | _ :: rest -> computeEvaluation numberBuffer prevEval rest
        | [] -> numberBuffer |> int |> prevEval

    computeEvaluation String.Empty Ok (splitComputation |> Seq.toList)

let result2 = computeCalculation calculation

match result2 with
| Error error -> $"Error: {error}"
| Ok ok -> $"Result is: %i{ok}"
|> printfn "%s"
