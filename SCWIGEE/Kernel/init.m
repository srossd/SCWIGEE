(* ::Package:: *)

(* Wolfram Language Init File *)

Block[{Print},Quiet[BeginPackage["SCWIGEE`",{"TensorTools`","GroupMath`"}]]]

Get[ "SCWIGEE`Usage`"];

Begin["`Private`"];

$consoleMode = ($FrontEnd === Null);
If[!$consoleMode, SetOptions[EvaluationNotebook[], CommonDefaultFormatTypes -> {"Output" -> TraditionalForm}]];

Get[ "SCWIGEE`Definitions`"]
Get[ "SCWIGEE`Utilities`"]
Get[ "SCWIGEE`Formatting`"]
Get[ "SCWIGEE`SetupWizard`"]
Get[ "SCWIGEE`SUSYVariations`"]
Get[ "SCWIGEE`RStructures`"]
Get[ "SCWIGEE`SpacetimeStructures`"]
Get[ "SCWIGEE`ConformalCheck`"]
Get[ "SCWIGEE`Expansions`"]
Get[ "SCWIGEE`Ward`"]

End[]

EndPackage[]
