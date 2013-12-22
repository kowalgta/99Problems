exception ValuesNotEqual

let assertEqual v1 v2 =
    if v1 = v2 then ()
    else raise ValuesNotEqual

// -- Working with lists --

// 1) Write a function last : 'a list -> 'a option that returns the last element of a list.

let rec last xs =
    match xs with
    | [] -> None
    | [x] -> Some x
    | _::xs' -> last xs'

assertEqual (last ["a";"b";"c";"d"]) (Some "d")
// type specification needed because of a value restriction
assertEqual (last ([]: string list)) None

// 2) Find the last but one (last and penultimate) elements of a list.

let rec lastTwo xs =
    match xs with
    | [] | [_] -> None     
    | [x;y] -> Some (x,y)
    | _::xs' -> lastTwo xs'

assertEqual (lastTwo ["a";"b";"c";"d"]) (Some ("c","d"))
assertEqual (lastTwo ["c";"d"]) (Some ("c", "d"))
assertEqual (lastTwo ["a"]) None
assertEqual (lastTwo ([]: string list)) None

// 3) Find the k'th element of a list.

let rec at k xs =
    match (k, xs) with
    | (_, []) -> None
    | (1, x::_) -> Some x
    | (_, x::xs') -> at (k - 1) xs'

assertEqual (at 3 ["a";"b";"c";"d";"e"]) (Some "c")
assertEqual (at 3 ["a"]) None
assertEqual (at 3 ([]: string list)) None

// 4) Find the number of elements of a list

let length xs =
    let rec inn sum =
        function
        | [] -> sum
        | _::xs' -> inn (sum + 1) xs'
    inn 0 xs

assertEqual (length ["a";"b";"c"]) 3
assertEqual (length []) 0

// 5) Reverse a list

// Simple buit quite inefficient version with complexity ~ n^2
// Appending two lists takes time proportional to the length of the first list
let rec rev xs =
    match xs with
    | [] -> []
    | x::xs' -> (rev xs') @ [x]

// Improved version
let rev2 xs =
    let rec inn acc =
        function
        | [] -> acc
        | x::xs' -> inn (x::acc) xs'
    inn [] xs

assertEqual (rev2 ["a";"b";"c"]) ["c";"b";"a"]
assertEqual (rev2 []) []

// 6) Find out whether a list is a palindrome

let isPalindrome xs =
    rev2 xs = xs

assertEqual (isPalindrome ["x";"a";"m";"a";"x"]) true
assertEqual (isPalindrome ["a";"b"]) false

// 7) Flatten a nested list structure (M)

type Node<'a> =
    | One of 'a
    | Many of 'a Node list

// First version - very straightforward, but not ideal because of @ and 
// lack of tail recursion
let rec flatten xs =
    match xs with
    | [] -> []
    | (One x)::xs' -> x::(flatten xs')
    | (Many y)::xs' -> (flatten y) @ (flatten xs')

assertEqual 
    (flatten [One "a"; Many [One "b"; Many[One "c"; One "d"]; One "e"]]) 
    ["a";"b";"c";"d";"e"]

// Second version - all nice and tail recursive but needs elements to be 
// reversed at the end
let flatten2 xs =
    let rec inn aux =
        function
        | [] -> aux
        | (One x)::xs' -> inn (x::aux) xs'
        | (Many y)::xs' -> inn (inn aux y) xs'
    inn [] xs |> rev2

assertEqual 
    (flatten2 [One "a"; Many [One "b"; Many[One "c"; One "d"]; One "e"]]) 
    ["a";"b";"c";"d";"e"]

// 8) Eliminate consecutive duplicates of list elements

let rec compress xs =
    match xs with
    | [] -> []
    | x::y::xs' when x = y -> compress (y::xs')
    | x::xs' -> x::(compress xs')

assertEqual 
    (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
    ["a";"b";"c";"a";"d";"e"]

// 9) Pack consecutive duplicates of list elements into sublists

let pack xs =
    let rec inn group acc =
        function
        | [] -> []
        | [x] -> (x::group)::acc
        | x::y::xs' when x = y 
            -> inn (x::group) acc (y::xs')
        | x::xs' -> inn [] ((x::group)::acc) xs'

    inn [] [] xs |> rev2

assertEqual
    (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"])
    [["a";"a";"a";"a"];["b"];["c";"c"];["a";"a"];["d";"d"];["e";"e";"e";"e"]]

// 10) Run-length encoding of a list

let encode xs =
    let packed = pack xs
    let rec inn =
        function
        | [] -> []
        | x::xs' -> (length x, x.[0])::(inn xs')
    inn packed

assertEqual
    (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
    [(4,"a");(1,"b");(2,"c");(2,"a");(1,"d");(4,"e")]












