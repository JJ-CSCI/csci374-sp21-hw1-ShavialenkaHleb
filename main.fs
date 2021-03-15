module Assignment

// Problem 1
let rec tribn n =
    if n=0 then 0
    else if n=1 then 1
    else if n=2 then 1
    else if n=3 then 2
    else tribn (n - 1) + tribn (n - 2) + tribn (n - 3) + tribn (n - 4)
 
    
// Problem 2
let tribn2 n =
   let rec t a4 a3 a2 a1 =function
   |n when n=0->a4
   |n when n=1->a3
   |n when n=2->a2
   |n when n=3->a1
   |n->t  a3 a2 a1(a1 + a2 + a3 + a4) (n - 1)
   t 0 1 1 2 n

// Problem 3
let rec last lst =
   match lst with
  []->raise(Failure "Empty list")
  |[x]->x
  |x::xs->(last xs);;

// Problem 4
let fourth (lst:int list) = function
    | [] -> None
    | h :: t -> if lst = 1 then Some h else fourth (lst - 1) t

// Problem 5
let everyfourth (lst:int list) =
    // write your code here
    []

// Problem 6
let rec take n (lst:int list) =
    // write your code here
    []

// Problem 7
let rec drop n (lst:int list) =
    let rec aux i = function
      | [] -> []
      | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
    aux 1 lst
 