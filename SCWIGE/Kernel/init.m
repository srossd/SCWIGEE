(* Wolfram Language Init File *)

Get[ "SCWIGE`Usage`"]
Get[ "SCWIGE`SCWIGE`"]

Print[Panel[Grid[{{"", "", Style["Setup Wizard", 20], 
    Style["Editing: ", 16], Checkbox[Dynamic[SCWIGE`Private`$editing]]}, {"", "", 
    Row[{Style["R-symmetry: ", 16], 
      Dynamic[InputField[Dynamic[SCWIGE`Private`$RSymmetry], Enabled -> SCWIGE`Private`$editing]]}],
     "", ""}, {"", "", 
    Dynamic[DisplayMultiplet["EditMode" -> SCWIGE`Private`$editing]], "", 
    ""}}, Alignment -> Center, Spacings -> {Automatic, 2}, 
  Dividers -> {True, All}], Background -> Lighter[Gray, 0.9]]]