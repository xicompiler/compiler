type file_error =
  [ `NotXiFile of string
  | `NoSuchFile of string
  ]

type parsing_error =
  [ `LexicalError
  | `SyntaxError
  ]

type semantic_error = [ `SemanticError ]

type t =
  [ file_error
  | parsing_error
  | semantic_error
  ]
