signature EVALUATOR = sig

  val empty_env:AbstractSyntaxTree.environment

  val eval_binop: AbstractSyntaxTree.exp
                 * AbstractSyntaxTree.binop
                 * AbstractSyntaxTree.exp
                 * AbstractSyntaxTree.environment -> AbstractSyntaxTree.value

  val eval_unop: AbstractSyntaxTree.unop
                * AbstractSyntaxTree.exp
                * AbstractSyntaxTree.environment -> AbstractSyntaxTree.value

  val eval: AbstractSyntaxTree.exp
           * AbstractSyntaxTree.environment -> AbstractSyntaxTree.value

end