datatype Val = Int of int
	| String of string
	| Bool of bool
	| TupI of int list
	| TupF of real list
	| LookupError
	| EndList;

datatype Val = Ast.Value;

val record = ref [("", EndList)];

fun isEndList(EndList) = true
      | isEndList(_) = false

fun isNil(x:(string * Val) list):bool =
	let val (_,value) = hd(x) in
		if isEndList(value) then true else false
	end

fun insert(label, exp) = (record := [(label, exp)] @ !record);

fun look_up(label:string, rcd:(string * Val) list):Val = 
	let val (var_label, var_record) = hd(rcd)
	in 
		if var_label = label then var_record else 
			if isNil(tl(rcd)) then  LookupError else look_up(label, tl(rcd))
	end