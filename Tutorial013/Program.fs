#light

(* 
 * A parser for DirectX .X files.
 * Copyright (c) 2008 Johann Deneux

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

 *)
 
open XParser
open TextLexer
open System.IO
open Microsoft.FSharp.Text.Lexing

let tokenStream filename =
    seq {
        use stream = File.OpenRead(filename)
        use reader = new StreamReader(stream)
        reader.ReadLine() |> ignore
        let lexbuf = Lexing.from_text_reader (new System.Text.ASCIIEncoding()) reader
        while not lexbuf.IsPastEndOfStream do
            match TextLexer.token lexbuf with
            | XLexers.EOF as value -> yield! [value]
            | token -> yield token
        }
    |> LazyList.of_seq
    

// A lexer working on a lazy list of Tokens. The list is constructed on the fly as the file is read
let nextTokenLazyList (debug : bool) (src : LazyList<XLexers.Token>) =
    match src with
    | LazyList.Nil -> None
    | LazyList.Cons(head, rest) ->
        if debug then printfn "%A" head;
        Some(head, rest)
    

let LazyLexer = new XLexers.LexerFuncs<_>(nextTokenLazyList false)

        
type ApplicationArgs = {
    filename : string option
    printHelp : bool
}


let printHelp () =
    eprintfn "dumpx [-h|--help] -f <filename>"


let invalidUsage (error_msg : string option) =
    eprintf "Incorrect usage"
    match error_msg with
    | Some err -> eprintfn ": %s" err
    | None -> eprintfn ""
    printHelp()
    System.Environment.Exit(1)
    

let dump top_level =
    let rec work incr items =
        match items with
        | [] -> ()
        | (Some name, value) :: rest -> printfn "%s: %A" name value; work incr rest
        | (None, value) :: rest -> printfn "%A" value; work incr rest

    work 0 top_level

    
let main (filename : string) =
    use file = File.OpenRead filename
    use reader = new StreamReader(file)
    let first_line = reader.ReadLine()
    if first_line <> null then
        match first_line.Split([|' '|]) |> List.of_array  with
        | "xof" :: version_and_format :: float_size when version_and_format.EndsWith("txt") ->                    
            match parse LazyLexer (tokenStream filename) with
            | Some(top_level) -> dump top_level
            | None -> eprintfn "Failed to parse file"
        | _ -> eprintfn "Unrecognized or unsupported file format"
    else
        eprintfn "Failed to parse file"
        
    
let rec parseCmdLine cmd_args (args : ApplicationArgs) =
    match cmd_args with
    | [] -> Some args
    | head :: rest ->
        match head with
        | "-h" | "--help" -> parseCmdLine rest {args with printHelp = true}
        | "-f" | "--file" -> parseFilename rest args
        | _ -> None


and parseFilename cmd_args (args : ApplicationArgs) =
    match cmd_args with
    | [] -> invalidUsage(Some "Missing filename after -f or --file"); None
    | filename :: rest ->
        match args.filename with
        | None -> parseCmdLine rest {args with filename = Some filename}
        | _ -> invalidUsage(Some "Only one filename may be provided"); None
        

match List.of_array(System.Environment.GetCommandLineArgs()) with
| _ :: args ->
    match parseCmdLine args {filename = None; printHelp = false} with
    | Some args ->
        if args.printHelp then printHelp(); System.Environment.Exit(1)
        match args.filename with
        | Some filename -> main(filename)
        | None -> invalidUsage(Some "No file provided")
    | None -> invalidUsage(None)
| [] -> failwith "Empty command line argument list"
