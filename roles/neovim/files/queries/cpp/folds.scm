[
  (comment)
  (preproc_if)
  (preproc_elif)
  (preproc_else)
  (field_declaration_list)
  (namespace_definition)
  (parameter_list)
  (enum_specifier)
  (compound_statement)
] @fold


; [
;   ;; c
;   (for_statement)
;   (if_statement)
;   (while_statement)
;   (switch_statement)
;   (case_statement)
;   ; (function_definition)
;   (struct_specifier)
;   (enum_specifier)
;   (comment)
;   (preproc_if)
;   (preproc_elif)
;   (preproc_else)
;   (preproc_ifdef)
;   (initializer_list)
;   ;; cpp
;   (for_range_loop)
;   (class_specifier)
;   (field_declaration
;     type: (enum_specifier)
;     default_value: (initializer_list))
;   ; (template_declaration)
;   (namespace_definition)
;   (try_statement)
;   (catch_clause)
;   (lambda_expression)
;   ; (compound_statement)
; ] @fold
;
; (compound_statement
;   (compound_statement) @fold)
