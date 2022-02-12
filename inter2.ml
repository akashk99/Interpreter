(*
Honor code comes here:

First Name: Akash 
Last Name: Kadakia  
BU ID: U80203377

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)


open Printf

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)


type 'a parser = Parser of (string -> ('a * string) list)

let parse p s = 
  match p with
    Parser f -> f s

let charP = 
  Parser (
    fun s -> match (explode s) with
      |[] -> []
      |h::t -> [(h,implode t)]

  )

let returnP a = 
  Parser (
    fun s -> [(a,s)]
  )


let failP = 
  Parser
    (
      fun s->[]
    )

let (>>=) p f = 
  Parser (
    fun s -> match parse p s with
      | [] -> []
      | (a,rest)::_ -> let parser2 = f a in 
        match parse parser2 rest with
        |[] ->[]
        |(b,rest2)::_ -> [(b,rest2)]
  )

let (<|>) a b =
  Parser (
    fun s -> match parse a s with
      |[] -> parse b s
      |r-> r


  )

let satcP c = 
  charP >>= fun a -> 
  if a=c then returnP c else failP



let digitP =
  charP >>= fun a -> 
  if (a >= '0' && a <= '9')then returnP a else failP


let lettersP =
  charP >>= fun a -> 
  if (Char.code a >= 97 && Char.code a <= 122) || (Char.code a >= 65 && Char.code a <= 90) then returnP a else failP

let asciiP = 
  charP >>= fun a -> 
  if (Char.code a != 34) then returnP a else failP  


let rec many0 p = 
  (p >>= fun a ->
   many0 p >>= fun b ->
   returnP (a::b))
  <|>
  returnP []

let rec many1 p =
  p >>= fun a -> 
  many0 p >>= fun b-> 
  returnP (a::b)


let whitespaceP = 
  satcP ' ' <|> satcP '\t' <|> satcP '\n' <|> satcP ';'


let natNumP = 
  many1 digitP >>= fun a ->
  returnP (int_of_string (implode a))

let integerP = 
  natNumP
  <|>
  (satcP '-' >>= fun _->
   natNumP >>= fun v -> 
   returnP ((-1)*v)) 

let satsP s = 
  if s="" then failP else
    let rec asats (s:string)= 
      match (explode s) with 
        h::rest->satcP h >>=fun _->asats (implode rest)
      |
        []-> returnP([])
    in 
    asats (s:string)

let boolP = 
  (satcP '<' >>= fun _ ->
   satsP "true" >>= fun a ->
   satcP '>' >>= fun _ -> returnP (true))
  <|>
  (satcP '<' >>= fun _ ->
   satsP "false" >>= fun a ->
   satcP '>' >>= fun _ -> returnP (false))

let unitP = 
  (satcP '<' >>= fun _ ->
   satsP "unit" >>= fun a ->
   satcP '>' >>= fun _ -> returnP ())

let stringP = 
  satcP '"' >>= fun _ ->
  many0 asciiP >>= fun a ->
  satcP '"' >>= fun _ -> returnP (implode a)

let nameP = 
  many1 (digitP <|> lettersP <|> satcP '_' <|> satcP '\'') >>= fun a -> returnP (implode a)



type consts = 
    String of string | Int of int | Bool of bool | Unit of unit | Name of string
and command = 
    Push of consts 
  | Swap | Pop | Log 
  | Add | Sub | Mul | Div | Rem | Neg 
  | Cat 
  | And | Or | Not | Eq | Lte | Lt | Gte | Gt 
  | Let | Ask 
  | Begin of (command list)
  | If of (command * command)
  | Throw
  |Try of (command list * command list)

(* Add consts name and add to list of commands, add push name *)
(* Add commands (begin and if) *)

let pushintP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Push" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (integerP)>>= fun a -> 
  many0 whitespaceP >>= fun _ -> returnP (Push (Int a))

let pushboolP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Push" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (boolP)>>= fun a -> 
  many0 whitespaceP >>= fun _ -> returnP (Push (Bool a))

let pushstringP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Push" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (stringP)>>= fun a -> 
  many0 whitespaceP >>= fun _ -> returnP (Push (String a))

let pushunitP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Push" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (unitP)>>= fun a -> 
  many0 whitespaceP >>= fun _ -> returnP (Push (Unit ()))

let pushnameP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Push" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (nameP)>>= fun a -> 
  many0 whitespaceP >>= fun _ -> returnP (Push (Name a))


let addP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Add" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Add)

let popP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Pop" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Pop)

let logP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Log" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Log)

let swapP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Swap" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Swap)


let mulP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Mul" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Mul)

let divP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Div" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Div)

let subP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Sub" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Sub)

let remP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Rem" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Rem)

let negP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Neg" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Neg)

let catP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Cat" >>= fun _ ->
  many1 whitespaceP >>= fun _ ->
  returnP (Cat)

let andP = 
  many0 whitespaceP >>= fun _ ->
  satsP "And" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (And)

let orP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Or" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Or)

let notP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Not" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Not)

let eqP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Eq" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Eq)

let lteP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Lte" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Lte)

let ltP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Lt" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Lt)

let gteP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Gte" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Gte)

let gtP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Gt" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Gt)

let letP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Let" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Let)

let askP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Ask" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Ask)

let throwP = 
  many0 whitespaceP >>= fun _ ->
  satsP "Throw" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  returnP (Throw)

let rec commandP () = 
  addP <|> pushintP <|> pushboolP <|> pushstringP <|> pushunitP 
  <|>  popP <|> pushnameP <|> logP <|> swapP <|> subP <|> mulP <|> divP <|> remP <|> negP
  <|> catP <|> andP <|> orP <|> notP <|> eqP <|> lteP <|> ltP <|> gteP <|> gtP 
  <|> letP <|> askP <|> pushnameP <|> ifP () <|> beginP () <|> throwP <|> tryP ()

and commandsP ()= 
  many1 (commandP ())

and ifP ()= 
  many0 whitespaceP >>= fun _ ->
  satsP "If" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (commandP ())>>= fun a -> 
  many0 whitespaceP >>= fun _ -> 
  satsP "Else" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (commandP ())>>= fun b -> 
  many0 whitespaceP >>= fun _ -> 
  satsP "End" >>= fun _ ->
  returnP (If (a,b))

and beginP () = 
  many0 whitespaceP >>= fun _ ->
  satsP "Begin" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (many0 (commandP ()))>>= fun a -> 
  many0 whitespaceP >>= fun _ -> 
  satsP "End" >>= fun _ ->
  returnP (Begin (a))

and tryP ()= 
  many0 whitespaceP >>= fun _ ->
  satsP "Try" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (commandsP ())>>= fun a -> 
  many0 whitespaceP >>= fun _ -> 
  satsP "Catch" >>= fun _ ->
  many0 whitespaceP >>= fun _ ->
  (commandsP ())>>= fun b -> 
  many0 whitespaceP >>= fun _ -> 
  satsP "End" >>= fun _ ->
  returnP (Try (a,b))


let push a stack = 
  a::stack

let pop stack =
  match stack with
  | [] -> []
  | _ :: st -> st


let peek stack =
  match stack with
  | [] -> None
  | x :: _ -> Some x

let rec find_value name bind_env = 
  match bind_env with
  |(Name a,b)::t -> if (a = name) then b else find_value name t
  |_ -> None

let add_binding name value bind_env = 
  (name,Some value)::bind_env

let reverse l = 
  let rec aux accum l = 
    match l with
      []->accum
    |
      h::t -> aux (h::accum) t
  in aux [] l

let string_of_const a =
  match a with
  |Int x -> string_of_int x
  |String s -> "\""^s^"\""
  |Bool b -> "<"^string_of_bool b^">"
  |Unit u -> "<unit>"
  |Name n -> n


let interpreter (s : string) : string list * int = 
  let rec aux commands stack log error bind_env =
    match commands with
    | [] -> (log,error) 
    | ([],_)::_ -> (log,error) 
    | (com::t,rest)::_ -> (match com with
        |Push x -> aux [(t,rest)] (push x stack) log 0 bind_env
        |Pop -> (match stack with
            |[] -> (log,2) 
            |h1::_ -> aux [(t,rest)] (pop stack) log 0 bind_env
          )
        |Log -> (match peek stack with
            |None -> (log,2) 
            |Some x -> aux [(t,rest)] (pop stack) (log@[(string_of_const x)]) 0 bind_env
          )
        |Swap -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |a, b -> aux [(t,rest)] (push b (push (a) (pop (pop stack)))) log 0 bind_env
              ))
        |Sub -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Int (a-b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Mul -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Int (a*b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Div -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> 
                  if a != 0 then aux [(t,rest)] (push (Int (a/b)) (pop (pop stack))) log 0 bind_env
                  else (log,3)
                |_ -> (log,1)
              ))
        |Rem -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> 
                  if a != 0 then aux [(t,rest)] (push (Int (a mod b)) (pop (pop stack))) log 0 bind_env
                  else (log,3)
                |_ -> (log,1)
              ))
        |Neg -> (match stack with
            |[] -> (log,2) 
            |h1::_ -> (match h1 with
                |Int a-> aux [(t,rest)] (push (Int (-1*a)) (pop stack)) log 0 bind_env
                |_ -> (log,1)
              ))
        |Add -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Int (a+b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Cat -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |String a, String b -> aux [(t,rest)] (push (String (a^b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |And -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Bool a, Bool b -> aux [(t,rest)] (push (Bool (a&&b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Or -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Bool a, Bool b -> aux [(t,rest)] (push (Bool (a||b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Not -> (match stack with
            |[] -> (log,2) 
            |h1::_ -> (match h1 with
                |Bool a -> aux [(t,rest)] (push (Bool (not a)) ((pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Lte -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Bool (a<=b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Eq -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Bool (a=b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Lt -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Bool (a<b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Gte -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Bool (a>=b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Gt -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Int a, Int b -> aux [(t,rest)] (push (Bool (a>b)) (pop (pop stack))) log 0 bind_env
                |_ -> (log,1)
              ))
        |Ask -> (match stack with
            |[] -> (log,2) 
            |h1::_ -> (match h1 with
                |Name a-> (match find_value a bind_env with
                    |Some v -> aux [(t,rest)] (push (v) ((pop stack))) log 0 bind_env
                    |None -> (log,4)
                  )
                |_ -> (log,1)
              ))
        |Let -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::h2::_ -> (match h1,h2 with
                |Name a, b -> aux [(t,rest)] stack log 0 (add_binding (Name a) b bind_env)
                |_ -> (log,1)
              ))
        |Begin l -> let aux2 innerstack comList inner_bind_env =
                      match (aux ([(l,"")]) innerstack [] error bind_env) with
                      |(inner_log,b) -> (match reverse inner_log,b with
                          |top_of_innerstack::_,0 -> (match parse (pushintP <|> pushboolP <|> pushstringP <|> pushunitP <|> pushnameP) ("Push "^top_of_innerstack) with
                              |(x,y)::_ -> aux [(x::t,rest)] stack (log@inner_log) error bind_env
                              |[] -> aux [(t,rest)] stack  (log@inner_log) error bind_env
                            )
                          |[],0 -> aux [(t,rest)] stack (log@inner_log) error bind_env
                          |_,b -> (log@inner_log,b)
                        )
          in aux2 [] (l@[Log]) bind_env

        |If (a,b) -> (match stack with
            |[] -> (log,2) 
            |[x] -> (log,2)
            |h1::_ -> (match h1 with
                |Bool x-> (match x with
                    |true -> aux [(a::t,rest)] (((pop stack))) log 0 bind_env
                    |false -> aux [(b::t,rest)] (((pop stack))) log 0 bind_env
                  )
                |_ -> (log,1)
              ))
        |Throw -> (match stack with
            |[] -> (log,2) 
            |h1::_ -> (match h1 with
                |Int a-> (log,a)
                |_ -> (log,1)
              ))
        |Try (a,b)-> let aux2 innerstack comList inner_bind_env =
                       match (aux ([(a,"")]) stack [] error bind_env) with
                       |(_,e) -> (match e with
                           |0 -> aux [(a@t,rest)] stack log 0 bind_env
                           |i -> aux [([Push (Int i)]@b@t,rest)] stack log 0 bind_env
                         )
          in aux2 [] (a) bind_env
      )

  in aux (parse (commandsP ()) s) [] [] 0 []

(* Figure out how to create a binding enviroment *)
(* Figure out how to build up inner stack in begin without copy and pasting all the commands *)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) =
  let s = readlines file in
  interpreter s
