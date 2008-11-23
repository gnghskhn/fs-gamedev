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
 
// Format of the .X file
type FormatType =
    | TEXT
    | BINARY
    | TEXT_ZIP
    | BINARY_ZIP


// All tokens    
type Token =
    | NAME of string
    | STRING of string 
    | INTEGER of int
    | FLOATVAL of float
    | UUID of string
    | INTEGER_LIST of int list   // Not used ?!
    | FLOAT_LIST of float list   // Not used ?!
    | OBRACE | CBRACE | OPAREN | CPAREN
    | OBRACKET | CBRACKET | OANGLE | CANGLE
    | DOT | COMMA | SEMICOLON
    | TEMPLATE
    | WORD | DWORD
    | FLOAT | DOUBLE
    | CHAR | UCHAR
    | SWORD | SDWORD
    | VOID
    | LPSTR
    | UNICODE
    | CSTRING
    | NSTRING
    | ARRAY
    | EOF


// Type of functions returning tokens
type Lexer<'Src> = 'Src -> (Token * 'Src) option


// Expect a sequence of tokens
let expect nextToken (src : 'Src) (tokens : Token list) =
    let rec work src tokens =
        match tokens with
        | [] -> Some src
        | tok :: toks ->
            match nextToken src with
            | Some(head, rest) when head = tok -> work rest toks
            | _ -> None
    work src tokens


// Expect a sequence of tokens, passing data along.
let maybeExpect nextToken (tokens : Token list) (in_data : ('a * 'Src) option) =
    match in_data with
    | Some(smthing, src) ->
        match expect nextToken src tokens with
        | Some(src) -> Some(smthing, src)
        | None -> None
    | None -> None
    

// A lexer working on a list of Tokens. Useful for testing and debugging the parser.    
let nextTokenList src =
    match src with
    | [] -> None
    | head :: rest -> Some(head, rest)


type LexerFuncs<'Src>(lexer : Lexer<'Src>) = 
    member x.NextToken src = lexer src
    member x.Expect src tokens = expect lexer src tokens
    member x.MaybeExpect tokens in_data = maybeExpect lexer tokens in_data


let DebugLexer = new LexerFuncs<_>(nextTokenList)