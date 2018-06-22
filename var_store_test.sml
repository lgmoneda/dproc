val record = ref [("", Ast.EndList)];

fun isEndList(Ast.EndList) = true
      | isEndList(_) = false

fun isNil(x:(string * Ast.Exp) list):bool =
	let val (_,value) = hd(x) in
		if isEndList(value) then true else false
	end

fun insert(label, exp) = (record := [(label, exp)] @ !record);

fun look_up(label:string, rcd:(string * Ast.Exp) list):Ast.Exp = 
	let val (var_label, var_record) = hd(rcd)
	in 
		if var_label = label then var_record else 
			if isNil(tl(rcd)) then  Ast.LookupError else look_up(label, tl(rcd))
	end