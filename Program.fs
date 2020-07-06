// Learn more about F# at http://fsharp.org

open System
open System.IO;

type SymbolCategory =
    | NumberLiteral of terminating : bool
    | CharLiteral of terminating : bool
    | Operator
    | Variable
    | Function
    | Invalid

type ExpressionInput = { 
    Text: string; 
    Category: SymbolCategory;
}

type ExpressionResult = 
    | Int of int
    | Float of float
    | String of string

type Operator =
    | Add
    | Multiply

let CategoriseSymbol (exp: string) =
    match (exp.Substring(0, 2), Seq.last exp) with
    | ("no", 'T') -> NumberLiteral true
    | ("no", 't') -> NumberLiteral false
    | ("nO", 'T') -> CharLiteral true
    | ("nO", 't') -> CharLiteral false
    | ("NO", 'T') -> Operator
    | ("No", 't') -> Variable
    | ("No", 'T') -> Function
    | _ -> Invalid

// Binary: uppercase O = 1 and lowercase o = 0

// Integer Literals: lowercase n, lowercase o, two's complement binary, uppercase T
// Floating Point Literals: lowercase n, lowercase o, 
//  Mantissa: first digit is sign, uppercase O for negative, 
//      binary starting with 1/2 and getting smaller, lowercase t
//  Exponent: two's complement integer literal as before. 

// String / Char Literals: lowercase n, uppercase O, binary charCode,
// | uppercase T for 'Terminate' i.e end of literal
// | lowercase 't' means next noot is next char

// Variables: uppercase N, lowercase o, identifier, lowercase t
// Functions: uppercase N, lowercase o, identifier, uppercase T
//  Variable declarations following a function declaration are the parameters
//  Functions may contain statements but must end with an expression to return
//  As code is executed in a top-level program, a function must be declared before it is used

// Inbuilt Operators: uppercase N, uppercase O, identifier, uppercase T
// NOoT: Sets the value of a variable or function parameter, first argument is the name, second argument is the value expression
// NOOoT: Adds two values: (Int | Float) + (Int | Float); String + String
// NOOOT: Multiplies two numeric values: (Int | Float) x (Int | Float)

let ParseInteger (text: string) (signed: bool) =
    Seq.sum (Seq.mapi (fun i x -> 
        match x with 
        | 'O' when i > 1 -> int ( 2. ** float (text.Length - 2 - i)) * (if i = 2 && signed then -1 else 1)
        | _ -> 0) text)

let ParseFloat (mantissaText: string) (exponentText: string) =
    Seq.sum (Seq.mapi (fun i x -> 
        match x with 
        | 'O' when i > 2 -> (0.5 ** float (i - 2))
        | _ -> 0.) mantissaText) 
            * (2. ** float (ParseInteger exponentText true)) 
            * (if mantissaText.ToCharArray().[2] = 'O' then -1. else 1.)

let ParseString inputs = Seq.map (fun (input: string) -> ParseInteger input false |> Convert.ToChar |> string) inputs |> Seq.fold (+) ""

let toOperator input =
    match (input.Category, input.Text) with
    | (Operator, "NOOoT") -> Some Add
    | (Operator, "NOOOT") -> Some Multiply
    | _ -> None

let toInt expResult =
    match expResult with 
    | Int i -> Some i
    | _ -> None

let mutable inputs: seq<ExpressionInput> = Seq.empty
let mutable pendingOperator: option<Operator> = None
let mutable operands: seq<int> = Seq.empty

let processNoot noot = do
    let category = CategoriseSymbol noot
    printfn "%A" (category)
    let input = {Category = category; Text = noot}
    inputs <- Seq.append inputs (Seq.singleton input)
    let result : option<ExpressionResult> = 
        match category with
        | NumberLiteral terminating when terminating && Seq.length inputs = 1 -> ParseInteger noot true |> Int |> Some
        | NumberLiteral terminating when terminating && Seq.length inputs = 2 -> ParseFloat (Seq.head inputs).Text noot |> Float |> Some
        | CharLiteral terminating when terminating -> Seq.map (fun x -> x.Text) inputs |> ParseString |> String |> Some
        | _ -> None
    if result.IsSome then do
        inputs <- Seq.empty
        let printResult = 
            match result.Value with
            | Int i -> printfn "%i" i
            | Float f -> printfn "%f" f
            | String s -> printfn "%s" s
        printResult
        let intResult = toInt result.Value
        if pendingOperator.IsSome && intResult.IsSome then do
            operands <- Seq.append operands (Seq.singleton intResult.Value)
            let opResult : option<int> = 
                match (pendingOperator.Value, Seq.length operands) with
                | (Add, 2) -> Seq.sum operands |> Some
                | (Multiply, 2) -> Seq.fold ( * ) 1 operands |> Some
                | _ -> None
            if(opResult.IsSome) then
                printfn "Operation Result %i" opResult.Value
                operands <- Seq.empty
                pendingOperator <- None
    let newOperator = toOperator input
    if pendingOperator.IsNone && newOperator.IsSome then do
        printfn "New operator set: %O" newOperator.Value
        pendingOperator <- newOperator
        inputs <- Seq.empty

[<EntryPoint>]
let main argv =
    //use streamReader = new StreamReader(@".\test.noot");
    //let text = streamReader.ReadLine();
    printfn "NootScript"
    let mutable text = stdin.ReadLine()
    while (text.Length > 0) do
        let noots = if text.Contains(' ') then text.Split(' ') |> seq<string> else Seq.singleton text
        Seq.iter processNoot noots
        printfn ""
        text <- stdin.ReadLine()
    0 // return an integer exit code

