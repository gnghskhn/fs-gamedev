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

 *
 * TODO:
 * Using UUID for references.
 * .NET-friendly API to initiate parsing and access the parsed data.
 *)

open XLexers


// Type of functions parsing parts of data.
type ComponentParser<'Src> = LexerFuncs<'Src> -> 'Src -> (obj * 'Src) option


// Used to represent data.
type Record = {
    type_name  : string
    components : Map<string, obj>
    children   : SubRecord list
}
and SubRecord =
    | RefByName of string
    | RefByUUID of string           // Not implemented
    | FullRef   of string * string  // Not implemented
    | Nested    of Record

type Record with
    static member empty = {type_name = ""; components = Map.empty; children = []}
    member x.SetName(n) = { x with type_name = n }
    member x.Add(k, v : obj)  = let c = x.components.Add(k, v) in { x with components = c }
    member x.TryFind(k) = x.components.TryFind(k)
    member x.AddChild(child) = let children = child :: x.children in { x with children = children }
    

// Type of functions parsing data.    
type DataParser<'Src> = LexerFuncs<'Src> -> 'Src -> Record -> (Record * 'Src) option


(*
 * The values below are basic DataParsers, which are combined while parsing templates.
 *)
 
// Does no parsing, return the record untouched
let EmptyDataParser : DataParser<'Src> =
    fun lexer src record -> Some(record, src)


// Parses a ";", returns the record untouched
let SemiColonParser : DataParser<'Src> =
    fun lexer src record ->
        match lexer.Expect src [SEMICOLON] with
        | Some(src) -> Some(record, src)
        | None -> None
        

// Parses exactly one piece of data, which must be of one of the types in 'restriction'
let RestrictionParser (restriction : Map<string, DataParser<'Src>>) : DataParser<'Src> =
    fun lexer src record ->
        match lexer.NextToken src with
        // Nested data
        | Some(NAME(name), src) ->
            printfn ">>> RestrictionParser %s ?" name
            match lexer.Expect src [OBRACE] with
            | Some(src) ->
                match restriction.TryFind(name) with
                | Some(parser) ->
                    printfn ">>> RestrictionParser found %s" name
                    match parser lexer src (Record.empty.SetName(name)) with
                    | Some(child, src) -> Some(record.AddChild(Nested(child)), src) |> lexer.MaybeExpect [CBRACE]
                    | _                -> None
                | None -> None
            | None -> None
        // Data reference using the name
        | Some(OBRACE, src) ->
            match lexer.NextToken src with
            | Some(NAME(name), src) -> Some(record.AddChild(RefByName(name)), src) |> lexer.MaybeExpect [CBRACE]
            | _ as value -> printfn "Expected OBRACE got %A" value; None
        // TODO: Data reference using the UUID, or using name + UUID
        | _ -> None
    

// Parses any number of data pieces, of any known type.
let OpenParser (env : Map<string, DataParser<'Src>>) : DataParser<'Src> =
    fun lexer src record ->
        let parser = RestrictionParser env
        let rec loop src record =
            match parser lexer src record with
            | None -> Some(record, src)
            | Some(record, src) -> loop src record
        loop src record
            

// Makes a parser that executes two parsers one after another.                
let ComposeDataParsers (parser1 : DataParser<'Src>) (parser2 : DataParser<'Src>) : DataParser<'Src> =
    fun lexer src record ->
        match parser1 lexer src record with
        | Some(record, src) -> parser2 lexer src record
        | None              -> None


// Infix operator for ComposeDataParsers   
let (>>>>) parser1 parser2 =
    ComposeDataParsers parser1 parser2
    

(*
 * The values below are ComponentParsers, used in parseTemplateContent.
 *)
 
// Parses an int, returns it, boxed.            
let IntParser : ComponentParser<'Src> =
    fun lexer src ->
        match lexer.NextToken src with
        | Some(INTEGER(value), src) -> Some (box value, src)
        | _ -> None
        
// Parses a float, returns it, boxed.             
let FloatParser : ComponentParser<'Src> =
    fun lexer src ->
        match lexer.NextToken src with
        | Some(FLOATVAL(value), src) -> Some (box value, src)
        | _ -> None
        
// Parses a string, returns it, boxed.             
let StringParser : ComponentParser<'Src> =
    fun lexer src ->
        match lexer.NextToken src with
        | Some(STRING(value), src) -> Some(box value, src)
        | _ -> None

// Parses an array, returns it, boxed.
let ArrayParser (cell_parser_func : ComponentParser<'Src>) : ComponentParser<'Src> =
    fun lexer src ->
        let rec loop src values =
            match lexer.NextToken src with
            | Some(SEMICOLON, _) -> Some(values, src)
            | Some(COMMA, src) ->
                match cell_parser_func lexer src with
                | Some(value, src) -> loop src (value :: values)
                | None             -> None
            | _ -> None

        let maybe_rev_src =         
            match lexer.NextToken src with
            | Some(SEMICOLON, _) -> Some([], src)
            | _ ->
                match cell_parser_func lexer src with
                | Some(value, src) -> loop src [value]
                | None             -> None
                
        match maybe_rev_src with
        | Some(rev, src) ->
            let arr = rev |> List.rev |> List.to_array
            Some(box arr, src)
        | None -> None
        

// Convert a DataParser to a ComponentParser, used for nested data.                 
let DataToComponentParser template_name (parser: DataParser<'Src>) : ComponentParser<'Src> =
    fun lexer src ->
        match parser lexer src (Record.empty.SetName(template_name)) with
        | Some(record, src) -> Some(box record, src)
        | None              -> None
        

// Turn a ComponentParser to a DataParser. The boxed value is added in the Record with name 'name'        
let NamedComponentParser name (comp_parser_func : ComponentParser<'Src>) : DataParser<'Src> =
    fun lexer src record ->
        match comp_parser_func lexer src with
        | Some(value, src) -> Some(record.Add(name, value), src)
        | None             -> None


// Parses the body of a template definition. Returns a DataParser, which is used in the second phase when data is parsed.
let rec parseTemplateContent (lexer: LexerFuncs<'Src>) src (env : Map<string, DataParser<'Src>>) =
    (*
     * Various helper functions and values. See below "Execution starts here" to see where things happen.
     *)
    // Parses one line.
    let parseLine item_parser_func src =
        match lexer.NextToken src with
        | Some(NAME(name), src) ->
            Some(NamedComponentParser name item_parser_func, src)
        | _ -> None
    
    // Parses a line starting with WORD, DWORD... any integral type.
    let mkIntItemParser = parseLine IntParser
    
    // Parses a line starting with FLOAT, DOUBLE.
    let mkFloatItemParser = parseLine FloatParser
    
    // Parses a line starting with CSTRING.
    let mkStringItemParser = parseLine StringParser
    
    // Parses a line starting with the name of a template.
    let mkUserItemParser user_type =
        match env.TryFind(user_type) with
        | Some(p) -> parseLine (DataToComponentParser user_type p)
        | None -> fun _ -> None

    // Parses an array declaration.
    let parseArray src =
        // Create a parser for the cell type.
        let cell_parser_and_src =
            match lexer.NextToken src with
            | Some(WORD, src) -> Some(IntParser, src)
            | Some(DWORD, src) -> Some(IntParser, src)
            | Some(SWORD, src) -> Some(IntParser, src)
            | Some(SDWORD, src) -> Some(IntParser, src)
            | Some(FLOAT, src) -> Some(FloatParser, src)
            | Some(DOUBLE, src) -> Some(FloatParser, src)
            | Some(NSTRING, src) -> Some(StringParser, src)
            | Some(NAME(user_type), src) ->
                match env.TryFind(user_type) with
                | Some(p) -> Some(DataToComponentParser user_type p, src)
                | None -> Some((fun lexer -> fun src -> None), src)
            | _ -> None
        
        // Rest of the declaration: some_name[42]
        match cell_parser_and_src with
        | Some(cell_parser, src) ->
            match lexer.NextToken src with
            | Some(NAME(name), src) ->
                match lexer.Expect src [OBRACKET] with
                | Some(src) ->
                    match lexer.NextToken src with
                    // TODO: The size of the array is ignored. Any token is accepted here.
                    | Some(_, src) ->
                        match lexer.Expect src [CBRACKET] with
                        | Some(src) ->
                            match lexer.Expect src [OBRACKET] with
                            | Some(_) -> failwith "multi-dimensional arrays are not supported"
                            | _ -> ()
                            Some(NamedComponentParser name (ArrayParser cell_parser), src)
                        | _ -> None
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

    // Parses a restriction [...] or [name1, name2...]
    let parseRestriction src =
        let rec loop src restriction =
            match lexer.NextToken src with
            | Some(CBRACKET, src) -> Some(restriction, src)
            | Some(COMMA, src) -> parseName src restriction
            | _ -> None

        and parseName src (restriction : Map<string,_>) =
            let after_name =
                match lexer.NextToken src with
                | Some(NAME(name), src) ->
                    match lexer.NextToken src with
                    // Ignore the optional <UUID> part
                    | Some(OANGLE, src) ->
                        match lexer.NextToken src with
                        | Some(UUID(_), src) -> 
                            match lexer.Expect src [CANGLE] with
                            | Some(src) -> Some(name, src)
                            | None -> None
                        | _ -> None
                    | Some(_, _) -> Some(name, src)
                    | None -> None
                | _ -> None
            
            match after_name with
            | Some(name, src) ->
                match env.TryFind(name) with
                | Some(parser) -> loop src (restriction.Add(name, parser))
                | None -> None
            | None -> None
            
            
        match lexer.NextToken src with
        | Some(DOT, src) ->
            match lexer.Expect src [DOT; DOT; CBRACKET] with
            | Some(src) -> Some(OpenParser env, src)
            | _ -> None
        | _ ->
            match parseName src Map.empty with
            | Some(restriction, src) -> Some(RestrictionParser restriction, src)
            | _ -> None

            
    // Function calling parseTemplateContent. It is not tail-recursive, but that should not be a problem.        
    let next maybe_parser_src =
        match maybe_parser_src with
        | Some(parser, src) ->
            match parseTemplateContent lexer src env with
            | Some(parser_rest, src) -> Some((parser >>>> SemiColonParser >>>> parser_rest), src)
            | _ -> None
        | _ -> None

    // Execution starts here.        
    match lexer.NextToken src with
    | Some(CBRACE, src) -> Some(EmptyDataParser, src)
    | Some(WORD, src) -> mkIntItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(DWORD, src) -> mkIntItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(SWORD, src) -> mkIntItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(SDWORD, src) -> mkIntItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(FLOAT, src) -> mkFloatItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(DOUBLE, src) -> mkFloatItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(NSTRING, src) -> mkStringItemParser src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(ARRAY, src) -> parseArray src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(NAME(user_type), src) -> mkUserItemParser user_type src |> lexer.MaybeExpect [SEMICOLON] |> next
    | Some(OBRACKET, src) -> parseRestriction src |> lexer.MaybeExpect [CBRACE]
    | _ -> None
        

// Parses a template. Calls parseTemplateContent, which does all the work.
let parseTemplate (lexer: LexerFuncs<'Src>) src env =
    match lexer.NextToken src with
    | Some(TEMPLATE, src) ->
        match lexer.NextToken src with
        | Some(NAME(name), src) ->            
            match lexer.Expect src [OBRACE; OANGLE] with
            | Some(src) ->
                match lexer.NextToken src with
                | Some (UUID(_), src) ->
                    match lexer.Expect src [CANGLE] with
                    | Some(src) -> 
                        match parseTemplateContent lexer src env with
                        | Some(tpl, src) -> Some(env.Add(name, tpl), src)
                        | _ -> None
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None


// Parse data. env is a mapping from template names to DataParsers, created in parseTemplateContent.
let parseData (lexer : LexerFuncs<'Src>) src (env : Map<string, DataParser<'Src>>) : ((string option * Record) list * 'Src) option=
    let rec loop src records =
        match lexer.NextToken src with
        | Some(NAME(name), src) ->
            let data_name, src =
                match lexer.NextToken src with
                | Some(NAME(n), src2) -> Some(n), src2
                | _ -> None, src
            match lexer.Expect src [OBRACE] with
            | Some(src) ->
                match env.TryFind(name) with
                | Some(parser) ->
                    match parser lexer src (Record.empty.SetName(name)) |> lexer.MaybeExpect [CBRACE] with
                    | Some(record, src) -> loop src ((data_name, record) :: records)
                    | None -> None
                | None -> None
            | None -> None
        | _ -> Some(records, src)
    
    match loop src [] with
    | Some(l, src) -> Some(List.rev l, src)
    | None -> None 


// Parse the entire file.
let parse (lexer : LexerFuncs<'Src>) src : (string option * Record) list option =
    // Function parsing all templates
    let rec loop src parsers =
        match parseTemplate lexer src parsers with
        | Some(parsers, src) -> loop src parsers
        | None -> parsers, src
    
    // First parse all templates
    let env, src = loop src Map.empty
    
    // Then parse the data
    match parseData lexer src env |> lexer.MaybeExpect [EOF] with
    | Some(value, _) -> Some value
    | None -> None
    
            
let testTemplate src1 src2 =
    let lexer = DebugLexer
    
    let rec loop src env =
        match src with
        | [] -> Some(env, src)
        | _ ->
            match parseTemplate lexer src env with
            | Some(env, src) -> loop src env
            | _ -> None
    
    let p = loop src1 Map.empty
    
    match p with
    | Some(env, _) ->
        let data = parseData lexer src2 env
        printfn "%A" data
    | _ -> ()


let test1 _ =
    let src = [TEMPLATE; NAME("template"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               CBRACE]
    let data_src = [NAME("template"); OBRACE; CBRACE]
    testTemplate src data_src
    

let test2 _ =
    let src = [TEMPLATE; NAME("template"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               FLOAT; NAME("u"); SEMICOLON;
               DWORD; NAME("v"); SEMICOLON;
               CBRACE]
    let data_src = [NAME("template"); OBRACE;
                    FLOATVAL(42.123); SEMICOLON;
                    INTEGER(42); SEMICOLON;
                    CBRACE]
    testTemplate src data_src

    
let test3 _ =
    let src = [TEMPLATE; NAME("template"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               DWORD; NAME("n"); SEMICOLON;
               ARRAY; FLOAT; NAME("arr"); OBRACKET; NAME("n"); CBRACKET; SEMICOLON;
               CBRACE]
    let data_src = [NAME("template"); OBRACE;
                    INTEGER(42); SEMICOLON;
                    FLOATVAL(1.1); COMMA; FLOATVAL(2.3); COMMA; FLOATVAL(3.14); SEMICOLON
                    CBRACE]
    testTemplate src data_src


let test4 _ =
    let src = [TEMPLATE; NAME("Vec2D"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               FLOAT; NAME("x"); SEMICOLON;
               FLOAT; NAME("y"); SEMICOLON;
               CBRACE;
               TEMPLATE; NAME("Vecs"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               WORD; NAME("n_items"); SEMICOLON;
               ARRAY; NAME("Vec2D"); NAME("items"); OBRACKET; NAME("n_items"); CBRACKET; SEMICOLON;
               CBRACE]
    let data_src = [NAME("Vecs"); OBRACE;
                    INTEGER(2); SEMICOLON;
                    FLOATVAL(0.0); SEMICOLON; FLOATVAL(0.0); SEMICOLON; COMMA;
                    FLOATVAL(1.0); SEMICOLON; FLOATVAL(0.0); SEMICOLON; SEMICOLON;
                    CBRACE]
    testTemplate src data_src


let test5 _ =
    let src = [TEMPLATE; NAME("Data1"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               FLOAT; NAME("x"); SEMICOLON;
               CBRACE;
               TEMPLATE; NAME("Data2"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               FLOAT; NAME("y"); SEMICOLON;
               CBRACE;
               TEMPLATE; NAME("Open"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               OBRACKET; DOT; DOT; DOT; CBRACKET;
               CBRACE]
    let data_src = [NAME("Open"); OBRACE;
                    CBRACE;
                    NAME("Open"); OBRACE;
                        NAME("Data1"); OBRACE;
                            FLOATVAL(3.14);
                            SEMICOLON;
                        CBRACE;
                        NAME("Data2"); OBRACE;
                            FLOATVAL(2.77);
                            SEMICOLON;
                        CBRACE;
                        OBRACE; NAME("Ref"); CBRACE;
                    CBRACE]
               
    testTemplate src data_src


let test6 _ =
    let src = [TEMPLATE; NAME("Data1"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               FLOAT; NAME("x"); SEMICOLON;
               CBRACE;
               TEMPLATE; NAME("Data2"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               FLOAT; NAME("y"); SEMICOLON;
               CBRACE;
               TEMPLATE; NAME("Restricted"); OBRACE;
               OANGLE; UUID("UUID"); CANGLE;
               OBRACKET; NAME("Data1"); COMMA; NAME("Data2"); CBRACKET;
               CBRACE]
    let data_src = [NAME("Restricted"); NAME("Named"); OBRACE;
                        NAME("Data1"); OBRACE;
                            FLOATVAL(1.1);
                            SEMICOLON;
                        CBRACE;
                    CBRACE;
                    NAME("Restricted"); OBRACE;
                        NAME("Data2"); OBRACE;
                            FLOATVAL(2.2);
                            SEMICOLON;
                        CBRACE;
                    CBRACE]
               
    testTemplate src data_src

