let stringToCharList (s:string) = Seq.toList s
let charListToString (chars: char list) = System.String(List.toArray chars)

let shiftChar (c: char) = 
    let charIndex = int c - int 'a'
    let shiftedIndex = (charIndex + 13) % 26
    char (int 'a' + shiftedIndex)

shiftChar 'a'

//let rec shiftCharList (charList: char list) =
//    match charList with
//    | [] -> []
//    | firstChar :: otherChars ->
//        shiftChar firstChar :: shiftCharList otherChars

let rec shiftCharList (charList: char list) acc =
    match charList with
    | [] -> List.rev acc
    | firstChar :: otherChars ->
        shiftCharList otherChars (shiftChar firstChar :: acc)

let rot13 (s: string) =
  let charList = stringToCharList s
  let shiftedCharList = shiftCharList charList []
  charListToString shiftedCharList

rot13 "sample"
rot13 "fnzcyr"