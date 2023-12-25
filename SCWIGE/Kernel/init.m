(* Wolfram Language Init File *)

Block[{Print},Quiet[BeginPackage["SCWIGE`",{"TensorTools`","GroupMath`"}]]]

Get[ "SCWIGE`Usage`"];

Begin["`Private`"];

SetOptions[EvaluationNotebook[], CommonDefaultFormatTypes -> {"Output" -> TraditionalForm}]

Get[ "SCWIGE`Definitions`"]
Get[ "SCWIGE`Utilities`"]
Get[ "SCWIGE`Formatting`"]
Get[ "SCWIGE`SetupWizard`"]
Get[ "SCWIGE`SUSYVariations`"]
Get[ "SCWIGE`RStructures`"]
Get[ "SCWIGE`SpacetimeStructures`"]
Get[ "SCWIGE`ConformalCheck`"]
Get[ "SCWIGE`Expansions`"]
Get[ "SCWIGE`Ward`"]

End[]

EndPackage[]