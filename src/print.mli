val parens :
  ?max_level:int -> ?at_level:int -> Format.formatter ->
  ('a, Format.formatter, unit, unit, unit, unit) CamlinternalFormatBasics.format6 -> 'a

val message :
  ?loc:Location.t -> string -> ('a, Format.formatter, unit, unit) format4 -> 'a
