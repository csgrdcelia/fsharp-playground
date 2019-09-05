let stringToCharList (s:string) = Seq.toList s
let charListToString (chars: char list) = System.String(List.toArray chars)

let shiftChar (n: int) (c: char) = 
    let charIndex = int c - int 'a'
    let shiftedIndex = (charIndex + n + 26) % 26
    char (int 'a' + shiftedIndex)


// /!\ stack overflow > 10000
//let rec shiftCharList (charList: char list) =
//    match charList with
//    | [] -> []
//    | firstChar :: otherChars ->
//        shiftChar firstChar :: shiftCharList otherChars

// bof pour l'acc
//let rec shiftCharList (n: int) (charList: char list) acc =
//    match charList with
//    | [] -> List.rev acc
//    | firstChar :: otherChars ->
//        shiftCharList n otherChars (shiftChar n firstChar :: acc)

let shiftCharList (n:int) (charList: char list) =
    [
        for c in charList do
        yield shiftChar n c
    ]

let rot (n: int) (s: string) =
  let charList = stringToCharList s
  let shiftedCharList = shiftCharList n charList
  charListToString shiftedCharList 

rot 13 "sample" 
rot 13 "fnzcyr"
