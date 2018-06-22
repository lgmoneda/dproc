fun soma(l1, l2) = 
    let
    fun step (l1 , l2 , acc) = 
            case (l1, l2) of
              ([], []) => acc
            | ([], _ ) => []
            | (_, [])  => []
            | (h1 :: t1 , h2 :: t2) => step(t1, t2,(h1+h2)::acc)
	in
  		List.rev (step (l1 , l2 ,[]))
        
	end;

fun prod(l1, l2) = 
    let
    fun  step (l1 , l2 , acc) = 
            case (l1, l2) of
              ([], []) => acc
            | ([], _ ) => []
            | (_, [])  => []
            | (h1 :: t1 , h2 :: t2) => step(t1, t2,(h1*h2)::acc)
	in
  		List.rev(step (l1 , l2 ,[]))
	end;

fun sub(l1, l2) = 
    let
    fun step (l1 , l2 , acc) = 
            case (l1, l2) of
              ([], []) => acc
            | ([], _ ) => []
            | (_, [])  => []
            | (h1 :: t1 , h2 :: t2) => step(t1, t2,(h1-h2)::acc)
	in
  		List.rev(step (l1 , l2 ,[]))
	end;

fun divi(l1, l2) = 
    let
    fun step (l1 , l2 , acc) = 
            case (l1, l2) of
              ([], []) => acc
            | ([], _ ) => []
            | (_, [])  => []
            | (h1 :: t1 , h2 :: t2) => step(t1, t2,(h1/h2)::acc)
	in
  		List.rev(step (l1 , l2 ,[]))
	end;

fun maximo [] = raise Empty 
 | maximo [x] = x
 | maximo (x::xs) =
      let 
        val y = maximo xs
      in
        if x > y then x else y
      end;


fun minimo [] = raise Empty 
 | minimo [x] = x
 | minimo (x::xs) =
      let 
        val y = minimo xs
      in
        if x > y then y else x
      end;

 fun insert(l1: list of list, l2: list , head: of string ) = 
    let
     val coluna = [head] @ l2
     l1 @ coluna
        
	end;