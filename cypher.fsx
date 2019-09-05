let stringToCharList (s:string) = Seq.toList s
let charListToString (chars: char list) = System.String(List.toArray chars)

let shiftChar (n: int) (c: char) = 
    let charIndex = int c - int 'a'
    let shiftedIndex = (charIndex + n + 26) % 26
    char (int 'a' + shiftedIndex)

let rec encodeCharList (fullPasswordCharList: char list) (passwordCharList: char list) (messageCharList: char list) acc = 
    match messageCharList, passwordCharList with 
    | [], _ -> List.rev acc
    | _, [] -> 
        encodeCharList fullPasswordCharList fullPasswordCharList messageCharList acc
    | messageFirstChar :: messageOtherChars, passwordFirstChar :: passwordOtherChars ->
        let shift = int passwordFirstChar - int 'a'
        let shiftedChar = shiftChar shift messageFirstChar
        encodeCharList fullPasswordCharList passwordOtherChars messageOtherChars (shiftedChar :: acc)

let encode (password:string) (message:string) = 
    let messageCharList = stringToCharList message
    let passwordCharList = stringToCharList password
    let shiftedCharList = encodeCharList passwordCharList passwordCharList messageCharList []
    charListToString shiftedCharList

encode "test" "aaaa"