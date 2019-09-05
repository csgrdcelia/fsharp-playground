// /!\ Stack Overflow /!\ 

//let rec reverse list = 
//    match list with 
//    | [] -> []
//    | firstItem :: otherItems -> 
//        reverse otherItems @ [ firstItem ]

//reverse [1 .. 10000] 

let rec reverse list acc = 
    match list with 
    | [] -> acc
    | firstItem :: otherItems -> 
        reverse otherItems ( firstItem :: acc )

reverse [1;2;3] []