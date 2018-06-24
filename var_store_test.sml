exception VarLookupError

val record = ref [ ref ("", Ast.EndList)];

fun isEndList(Ast.EndList) = true
      | isEndList(_) = false

fun isNil(x:(string * Ast.Value) ref list):bool =
	let val (_,value) = !(hd(x)) in
		if isEndList(value) then true else false
	end

fun insert(label, exp) = (record := ([ ref (label, exp)] @ !record));

fun look_up(label:string, rcd: (string * Ast.Value) ref list):Ast.Value = 
	let val (var_label, var_record) = !(hd(rcd))
	in 
		if var_label = label then var_record else 
			if isNil(tl(rcd)) then raise VarLookupError else look_up(label, tl(rcd))
	end;

fun look_up_ref(label:string, rcd: (string * Ast.Value) ref list):(string * Ast.Value) ref = 
	let val (var_label, var_record) = !(hd(rcd)); val possible_match = hd(rcd)
	in 
		if var_label = label then possible_match else 
			if isNil(tl(rcd)) then raise VarLookupError else look_up_ref(label, tl(rcd))
	end;

fun update(label:string, v:Ast.Value) = 
	let val match = look_up_ref(label, !record) in
		match := (label, v)
	end; 


insert("x", Ast.Int_v (3));
val i = look_up("x", !record);