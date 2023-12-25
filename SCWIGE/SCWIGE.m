(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 8, 2023 *)

Block[{Print},Quiet[BeginPackage["SCWIGE`",{"TensorTools`","GroupMath`"}]]]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

SetOptions[EvaluationNotebook[], CommonDefaultFormatTypes -> {"Output" -> TraditionalForm}]

Get["Definitions.m"];

Get["Utilities.m"];

Get["Formatting.m"];

Get["SetupWizard.m"];

Get["SUSYVariations.m"];

Get["RStructures.m"];

Get["SpacetimeStructures.m"];

Get["ConformalCheck.m"];

Get["Expansions.m"];

Get["Ward.m"];
  
End[]

EndPackage[]

