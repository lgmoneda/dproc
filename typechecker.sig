signature TYPECHECKER = sig

type typenv

exception StaticTypeError of string

val empty_typenv:typenv

val get_type: AbstractSyntaxTree.exp * typenv -> AbstractSyntaxTree.typ

end