
module rec ClangExpr': (ClangExpr.S with type stmt = ClangStmt'.stmt and type decl = ClangDecl'.decl) = ClangExpr.Make(ClangDecl')(ClangStmt')
and ClangStmt': (ClangStmt.S with type expr = ClangExpr'.expr and type decl = ClangDecl'.decl)  = ClangStmt.Make(ClangExpr')(ClangDecl')
and ClangDecl': (ClangDecl.S with type expr = ClangExpr'.expr and type stmt = ClangStmt'.stmt)  = ClangDecl.Make(ClangExpr')(ClangStmt')

module ClangExpr = ClangExpr'
module ClangStmt = ClangStmt'
module ClangDecl = ClangDecl'

include ClangExpr
include ClangStmt
include ClangDecl
