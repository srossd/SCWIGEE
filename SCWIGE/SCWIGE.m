(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Mar 8, 2023 *)

Block[{Print},Quiet[BeginPackage["SCWIGE`",{"TensorTools`","GroupMath`"}]]]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

SetOptions[EvaluationNotebook[], CommonDefaultFormatTypes -> {"Output" -> TraditionalForm}]

SetRSymmetry[group_] := (
	$RSymmetry = group;
	$QTensor = Tensor[{{"Q", Raised[RIndex[fundRep[$RSymmetry]]], Lowered[Spinor]}}];
	$QBarTensor = Tensor[{{"\!\(\*OverscriptBox[\(Q\), \(~\)]\)", Lowered[RIndex[fundRep[$RSymmetry]]], Lowered[DottedSpinor]}}];
);

Options[QTensor] = {"QBar" -> False};
QTensor[OptionsPattern[]] := If[TrueQ[OptionValue["QBar"]], $QBarTensor, $QTensor];

SetSymmetries[ops_] := Do[
	DeclareTensorSymmetry[op[[1]], Join[
		Table[{Cycles[{{i, i+1}}], 1}, {i, 2, 2 + 2 Spin[op][[1]] - 2}],
		Table[{Cycles[{{i, i+1}}], 1}, {i, 2 + 2 Spin[op][[1]], 2 + 2 Spin[op][[1]] + 2 Spin[op][[2]] - 2}]
	]],
	{op, ops}];

$multipletIndices = {};
SetMultiplet[ops_, name_, sc_, i_ : 1] := (
	SetSymmetries[Flatten[ops]]; 
	$multipletIndices = Union[$multipletIndices, {i}]; 
	$multiplet[i] = ops;
	$multipletName[i] = name;
	$multipletSC[i] = sc;
);

RSymmetry[] := $RSymmetry;
Multiplet[i_] := $multiplet[i];

$signatureFactor = 1;
SignatureFactor[] := $signatureFactor;

Format[Field[name_, rep_, dim_, {j1_, j2_}, y_], TraditionalForm] := 
  With[{indices = 
     Join[{"\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]"}[[;; 
        2 j1]], {"\!\(\*OverscriptBox[\(\[Alpha]\), \(.\)]\)", 
        "\!\(\*OverscriptBox[\(\[Beta]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Gamma]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Delta]\), \(.\)]\)"}[[;; 2 j2]]]},
     Subsuperscript[If[StringQ[name], ToExpression[name, TraditionalForm, HoldForm], name], If[indices == {}, "", Row[ToExpression[#, TraditionalForm, HoldForm] & /@ indices]],RepName[$RSymmetry, rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]]
  ];

ScalingDimension[Field[name_, rep_, dim_, {j1_, j2_}, y_]] := dim;
Spin[Field[name_, rep_, dim_, {j1_, j2_}, y_]] := {j1, j2};
RRep[Field[name_, rep_, dim_, {j1_, j2_}, y_]] := rep;

whichMultiplet[f_Field] := whichMultiplet[f[[1]]];
whichMultiplet[name_] := SelectFirst[$multipletIndices, MemberQ[Flatten[Multiplet[#]][[;;,1]], name] &];

multipletOf[f_Field] := With[{mm = Multiplet[whichMultiplet[f]]},
   If[MatchQ[mm[[1]], _List], SelectFirst[mm, MemberQ[#, f] &], mm]
];

Field /: Conjugate[f : Field[name_, rep_, dim_, js_, y_]] := 
 With[{r = ConjugateIrrep[$RSymmetry, rep]},
    First@SortBy[Select[Flatten[Multiplet[whichMultiplet[f]]], SimpleRepInputConversion[$RSymmetry, #[[2]]] == r && #[[3]] == dim && #[[4]] == Reverse[js] && #[[5]] == -y &], # === f &] 
 ];
 
makeConjugate[f : Field[name_, rep_, dim_, js_, y_]] := Field[
   With[{nameExp = ToExpression[name, TraditionalForm]}, 
      If[FreeQ[nameExp, OverBar], 
         nameExp /. x_ :> ToString[OverBar[x], TraditionalForm],
         nameExp /. x_ :> ToString[x /. OverBar -> Identity, TraditionalForm]
      ]
   ],
   ConjugateIrrep[$RSymmetry, rep],
   dim, Reverse[js], -y];
 
name2field[name_] := SelectFirst[Flatten[Multiplet[whichMultiplet[name]]], #[[1]] == name &];

extraPos[mult_] := 
  With[{pos = DeleteDuplicates[List @@@ mult[[;; , {5, 3}]]]}, 
   Complement[
    Join[pos, # + {1, 1/2} & /@ pos, # + {-1, 1/2} & /@ pos], pos]];
fundRep[grp_] := 
  If[IsSimpleGroupQ[grp], 
   If[grp == {}, 1, PadRight[{1}, Length[grp]]], fundRep /@ grp];
singRep[grp_] := 
  If[IsSimpleGroupQ[grp], 
   If[grp == {}, 0, PadRight[{}, Length[grp]]], singRep /@ grp];
   
$RSymmetry = Null;
$editing = True;
$viewingMultiplet = 1;
$multiplet = <||>;
$multipletName = <||>;
$multipletSC = <||>;

plusIcon = 
  Graphics[{Darker@Green, Disk[], Thickness[.15], White, 
    Line[{{-.5, 0}, {.5, 0}}], Line[{{0, -.5}, {0, .5}}]}];
   
tikzLine[{{x1_, y1_}, {x2_, y2_}}] := StringForm["\\draw (``, ``) -- (``, ``);", x1, y1, x2, y2];
tikzNode[exprs_, style_, {x1_, y1_}] := StringForm["\\node[``] at (``, ``) {$``$};", style, x1, y1, StringRiffle[exprs, " \\quad "]];
tikzMultiplet[i_] := With[{grp = GroupBy[Flatten[$multiplet[i]], List @@ (#[[{5, 3}]]) &]},
   StringRiffle[Flatten[{
   "\\begin{tikzpicture}[box/.style = {draw,black,thick,rectangle,fill=blue!10,inner sep=.1cm},scale=1.5]",
   tikzLine /@ Map[{-1, -2} # &, Select[Subsets[Keys[grp], {2}],Abs[Subtract @@ #] == {1, 1/2} &], {2}],
   Table[tikzNode[ToString@*TeXForm /@ vals, "box", {-vals[[1,5]], -2 vals[[1,3]]}], {vals, Values[grp]}],
   Table[tikzNode[{"\\Delta = "<>ToString[TeXForm[dim]]}, "", {Min[-Keys[grp][[;;,1]]] - 1, -2 dim}], {dim, Keys[grp][[;;,2]] // DeleteDuplicates}],
   "\\end{tikzpicture}"
}], "\n"]];

Options[DisplayMultiplet] := {"EditMode" -> False};
DisplayMultiplet[i_, OptionsPattern[]] := 
  (With[{grp = 
     If[OptionValue["EditMode"] && Length[Flatten[$multiplet[i]]] <= 30, 
       Merge[{#, AssociationMap[Function[x, {}], extraPos@Flatten[$multiplet[i]]]}, 
         Flatten@*Join] &, # &]@
      GroupBy[Flatten[$multiplet[i]], List @@ (#[[{5, 3}]]) &]},
   If[(! OptionValue["EditMode"]) || Length[Flatten[$multiplet[i]]] > 30,
    Graphics[{
      Line /@ Map[{-1, -2} # &, 
  Select[Subsets[Keys[grp], {2}], 
   Abs[Subtract @@ #] == {1, 1/2} &], {2}], 
      Text[Style[
          Framed[Row[If[Length[#] > 1 && Length[Flatten[$multiplet[i]]] > 30, {Style[Length[#], Italic]}, TraditionalForm /@ #], Spacer[2]], 
           Background -> LightBlue], 20, 
          FontFamily -> "CMU Serif"], {-#[[1, 5]], -2 #[[1, 3]]}] & /@ 
       Values[grp],
       If[!$multipletSC[i], {Thick, Dashed, Line[{{0, -2 Min[ScalingDimension /@ Flatten[$multiplet[i]]] + .5}, {0, -2 Max[ScalingDimension /@ Flatten[$multiplet[i]]] - .5}}]}, Nothing],
       Table[Text[Style["\[CapitalDelta] = "<>ToString[dim,TraditionalForm], 14, FontFamily -> "CMU Serif"], {Min[-Keys[grp][[;;,1]]] - 1, -2 dim}], {dim, Keys[grp][[;;,2]] // DeleteDuplicates}]
      }, 
     ImageSize -> (75 (Max[Keys[grp][[;; , 1]]] - 
          Min[Keys[grp][[;; , 1]]]))],
    If[Length[grp] == 0,
     Graphics[{
       Text[
        Style[Framed[
          Button[plusIcon, 
           With[{res = 
              opDialog[{}, "New Operator", 
               Field[Null, 1, Null, {0, 0}, If[$multipletSC[i], 0, Null]]]},
               If[FailureQ[Enclose[ConfirmQuiet[RepName[RSymmetry[], RRep[res]]]]], MessageDialog[StringForm["`` is not a valid R-symmetry representation.", RRep[res]]],
            If[MatchQ[res, _Field], $multiplet[i] = If[$multipletSC[i], {res}, ReverseSortBy[{{res}, {makeConjugate[res]}}, #[[1,-1]]&]]]]],
           Method -> "Queued", ImageSize -> Small, 
           Appearance -> "Frameless", Enabled -> ($RSymmetry =!= Null)]], 20, 
         FontFamily -> "CMU Serif"], {0, 0}]}],
     Graphics[{ 
       Table[Text[Style["\[CapitalDelta] = "<>ToString[dim,TraditionalForm], 14, FontFamily -> "CMU Serif"], {Min[-.75 Keys[grp][[;;,1]]] - 1, -2 dim}], {dim, Keys[grp][[;;,2]] // DeleteDuplicates}],
       Line /@ (Select[Subsets[Keys[grp], {2}], 
           Abs[Subtract @@ #] == {1, 1/2} &] . {{-.75, 0}, {0, -2}}), 
       Text[Style[Framed[Grid[{Append[
               Function[x, Button[
                   Style[TraditionalForm[x], 18], 
                   With[{res = 
                    opDialog[
                    DeleteDuplicates[
                    Flatten[Table[
                    ReduceRepProduct[$RSymmetry, {If[diff[[1]] == 1, 
                    ConjugateIrrep[$RSymmetry, fundRep[$RSymmetry]], 
                    fundRep[$RSymmetry]], RRep[f]}][[;; , 
                    1]], {diff, {{-1, -1/2}, {1, -1/2}}}, {f, 
                    If[KeyExistsQ[grp, #[[1]] + diff], 
                    grp[#[[1]] + diff], {}]}], 2]], "Edit Operator", 
                    x]}, If[MatchQ[res, _Field] && StringQ[res[[1]]], 
                    $multiplet[i] = $multiplet[i] /. {x -> res, makeConjugate[x] -> makeConjugate[res]}]],
                   Method -> "Queued", Appearance -> "Frameless"
                   ]] /@ #[[2]],
               
               Button[plusIcon, 
                With[{res = 
                   opDialog[
                    DeleteDuplicates[
                    Flatten[Table[
                    ReduceRepProduct[$RSymmetry, {If[diff[[1]] == 1, 
                    ConjugateIrrep[$RSymmetry, fundRep[$RSymmetry]], 
                    fundRep[$RSymmetry]], RRep[f]}][[;; , 
                    1]], {diff, {{-1, -1/2}, {1, -1/2}}}, {f, 
                    If[KeyExistsQ[grp, #[[1]] + diff], 
                    grp[#[[1]] + diff], {}]}], 2]], "New Operator", 
                    Field[Null, 1, #[[1, 2]], {0, 0}, #[[1, 1]]]]}, 
                 If[MatchQ[res, _Field] && StringQ[res[[1]]], 
                    If[$multipletSC[i], 
                       AppendTo[$multiplet[i], res];
                       If[res[[-1]] != 0, AppendTo[$multiplet[i], makeConjugate[res]]], 
                       $multiplet[i] = {
                          Append[$multiplet[i][[1]], If[res[[-1]] >= 0, res, makeConjugate[res]]],
                       	  Append[$multiplet[i][[2]], If[res[[-1]] >= 0, makeConjugate[res], res]]
                       };
                    ]
                 ]
                ],
                Method -> "Queued", ImageSize -> Small, 
                Appearance -> "Frameless"]
               ]}, Alignment -> {Center, Center}, Spacings -> .5], 
            Background -> LightBlue], 20, 
           FontFamily -> 
            "CMU Serif"], {-.75 #[[1, 1]], -2 #[[1, 2]]}] & /@ 
        Normal[grp]}, 
      ImageSize -> (75 (Max[Keys[grp][[;; , 1]]] - 
           Min[Keys[grp][[;; , 1]]]))]
     ]
    ]
   ] /. Global`\[CapitalDelta] -> 0);
   
opDialog[reps_, label_, 
   init_ : Field[Null, 1, 2, {0, 0}, 0]] := ($nfName = 
    init[[1]]; $nfRep = init[[2]]; $nfL = 2 init[[4, 1]]; $nfLb = 
    2 init[[4, 2]]; $nfDim = init[[3]]; $nfRCharge = If[init[[5]] === Null, Null, init[[5]]/2];
   DialogInput[
    DialogNotebook[{Framed@
       Panel[Grid[{{"Operator name:", 
           InputField[Dynamic[$nfName]]}, {"R-symmetry rep:", 
           If[Length[reps] == 0, InputField[Dynamic[$nfRep]], 
            RadioButtonBar[Dynamic[$nfRep], 
             AssociationMap[RepName[$RSymmetry, #] &, 
              reps]]]}, {"Spinor indices:", 
           InputField[Dynamic[$nfL], 
            Number]}, {"Dotted spinor indices:", 
           InputField[Dynamic[$nfLb], Number]}, 
          If[init[[3]] === Null, {"Scaling dimension:", 
            InputField[Dynamic[$nfDim], Number]}, Nothing],
          If[init[[5]] === Null, {"R-charge:", 
            InputField[Dynamic[$nfRCharge], Number]}, Nothing]}], 
        Style[label, 20], Top, Background -> LightBlue], 
      DefaultButton[
       DialogReturn[
        Field[ReleaseHold[
          ToString[#, TraditionalForm] & /@ 
           Hold[$nfName]], $nfRep, $nfDim, {$nfL, $nfLb}/2, 
         2 $nfRCharge]]]}]]);
         
multipletDialog[] := ($multName = Null; $multSC = True;
   DialogInput[
    DialogNotebook[{Framed@
       Panel[Grid[{{"Multiplet name:", 
           InputField[Dynamic[$multName]]}, {"Self-conjugate:", 
           Checkbox[Dynamic[$multSC]]}}], 
        Style["Add Multiplet", 20], Top, Background -> LightBlue], 
      DefaultButton[DialogReturn[{$multName, $multSC}]]}]]);
      
conventionButton[eq_] := With[{s = ToString@TeXForm[eq]},
   Tooltip[Button[
  Panel[Style[Format[eq, TraditionalForm], 14, 
    FontFamily -> "CMU Serif"], FrameMargins -> {30, 30}, 
   Background -> 
    Dynamic[If[CurrentValue["MouseOver"], LightGray, White]], Appearance -> "Frameless"], 
  CopyToClipboard[s], 
  Appearance -> "Frameless"], "Copy to TeX"]
]
      
conventions[1] := {\[Eta]Lower == MatrixForm[Components[\[Eta]Lower]], \[Epsilon]Lower ==
   MatrixForm@Components[\[Epsilon]Lower], \[Epsilon]LowerDot == 
  MatrixForm@Components[\[Epsilon]LowerDot], \[Sigma]Lower == 
  MatrixForm /@ Components[\[Sigma]Lower], \[Sigma]BarLower == 
  MatrixForm /@ Components[\[Sigma]BarLower], 
 Subscript["\[Epsilon]", Row[{0, 1, 2, 3}]] == 
  Components[\[Epsilon]Spacetime][[1, 2, 3, 4]]} /. (x_ == y_) :> conventionButton[x == y];
  
conventions[2] := {MyInactive[StructureI]["i", "j"] == StructureI["i", "j"], 
 MyInactive[StructureI]["i", "j", "k", "l"] == 
  StructureI["i", "j", "k", "l"], 
 MyInactive[StructureJ]["i", "j", "k"] == 
  StructureJ["i", "j", "k"], 
 MyInactive[StructureK]["i", "j", "k"] == 
  StructureK["i", "j", "k"], 
 MyInactive[StructureKBar]["i", "j", "k"] == 
  StructureKBar["i", "j", "k"], 
 MyInactive[StructureL]["i", "j", "k", "l"] == 
  StructureL["i", "j", "k", "l"], 
 MyInactive[StructureLBar]["i", "j", "k", "l"] == 
  StructureLBar["i", "j", "k", "l"]} /. {
  "i" -> "\!\(TraditionalForm\`i\)",
  "j" -> "\!\(TraditionalForm\`j\)",
  "k" -> "\!\(TraditionalForm\`k\)",
  "l" -> "\!\(TraditionalForm\`l\)"
  } /. (x_ == y_) :> conventionButton[x == y];
  
conventionsPanel[] := Row[{Column[conventions[1], Spacings -> 2]//TraditionalForm, 
   Spacer[20], 
   Column[conventions[2], Spacings -> 1]//TraditionalForm
}];
         
wizardPanel[] := Panel[Grid[{{"", "", Style["Setup Wizard", 20], 
    Row[{Style["Editing: ", 16], Spacer[5], 
      Checkbox[Dynamic[$editing]]}]}, {"", "", 
    Row[{Style["Signature: ", 14], 
      Dynamic[RadioButtonBar[
        Dynamic[$signatureFactor], {1 -> Style["Lorentzian", 12], 
         I -> Style["Euclidean", 12]}, Enabled -> $editing]]}], 
    Dynamic[PopupWindow[Button["View Conventions"], conventionsPanel[], WindowSize -> {1000, 400}, WindowFloating -> False, WindowTitle -> "Conventions"]]}, {"", "", 
    Row[{Style["R-symmetry: ", 16], 
      Dynamic[InputField[
        Dynamic[$RSymmetry, {Automatic, SetRSymmetry[#1] &}], 
        Enabled -> $editing, FieldSize -> {20, 1}]]}], "", ""}, {"", 
    "", Dynamic[
     If[Length[$multipletIndices] == 0, 
      Style["Set the R-symmetry, and then add a multiplet.", 11, 
       Italic], 
      TabView[Table[$multipletName[i] -> 
         Column[{Framed@DisplayMultiplet[i, "EditMode" -> $editing], 
           Button[Style["Copy as TeX", 14], 
            CopyToClipboard[tikzMultiplet[$viewingMultiplet]], 
            ImageSize -> 200]}, 
          Alignment -> Center], {i, $multipletIndices}], 
       Dynamic[$viewingMultiplet], Alignment -> Center, 
       ImageSize -> Automatic]]], 
    Button["Add Multiplet", 
     With[{res = multipletDialog[], 
       new = If[Length[$multipletIndices] == 0, 0, 
          Max[$multipletIndices]] + 1}, 
      If[Length[res] === 2, AppendTo[$multipletIndices, new];
       $multiplet[new] = If[res[[2]], {}, {{}, {}}];
       $multipletName[new] = res[[1]];
       $multipletSC[new] = res[[2]];]], Method -> "Queued", 
     ImageSize -> {Automatic, 30}, 
     Enabled -> Dynamic[$RSymmetry =!= Null]]}}, Alignment -> Center, 
  Spacings -> {Automatic, 2}, Dividers -> {True, All}], 
 Background -> Lighter[Gray, 0.9]];
         
NontrivialPermutations[t_] := 
  Select[Permutations[Range@TensorRank[Components[t]]], 
   Indices[t] === Indices[t][[#]] && 
     With[{grps = 
        Cases[TensorSymmetry[Components[t]], 
         Symmetric[xs_] | Antisymmetric[xs_] | Cycles[{xs_}] :> xs, 
         All]},
      AllTrue[Table[#[[i]], {i, grps}], OrderedQ]
      ]
    &];
    
indQ[basis_, vec_] := Quiet@Check[LinearSolve[Transpose[basis], vec]; False, True];

Options[IndependentSet] = {"Rules" -> {}, "MonitorProgress" -> False, "MaxIndependent" -> 0, "Indices" -> False};
IndependentSet[{}] := {};
IndependentSet[tensors_, OptionsPattern[]] := If[!ArrayQ[tensors[[1]]] && Indices[tensors[[1]]] == {},
   If[TrueQ[OptionValue["Indices"]], {1}, tensors[[{1}]]],
   With[{indices = Module[{runningComps = SparseArray[{}, {Length[tensors], If[!ArrayQ[tensors[[1]]], Times @@ (First@*IndexData@*First /@ Indices[tensors[[1]]]), Length[Flatten[tensors[[1]]]]]}]},
	If[TrueQ[OptionValue["MonitorProgress"]] || ArrayQ[OptionValue["MonitorProgress"]],
		ResourceFunction["MonitorProgress"][
			Fold[
			   If[OptionValue["MaxIndependent"] == 0 || Length[#1] < OptionValue["MaxIndependent"],
				    With[{comp = Flatten@Normal@CanonicallyOrderedComponents[tensors[[#2]]] /. OptionValue["Rules"]},
						If[Length[#1] == 0 || indQ[runningComps[[;;Length[#1]]], comp], 
							runningComps[[Length[#1] + 1]] = comp; Append[#1, #2],
							#1
						]
				    ],
				    #1
				] &, 
				{}, 
				Range@Length[tensors]
			], Sequence @@ If[ArrayQ[OptionValue["MonitorProgress"]], OptionValue["MonitorProgress"], {}],
			"CurrentDisplayFunction" -> None 	
		],
		Fold[
		    If[OptionValue["MaxIndependent"] == 0 || Length[#1] < OptionValue["MaxIndependent"],
			    With[{comp = Flatten@Normal@CanonicallyOrderedComponents[tensors[[#2]]] /. OptionValue["Rules"]},
					If[Length[#1] == 0 || indQ[runningComps[[;;Length[#1]]], comp], 
						runningComps[[Length[#1] + 1]] = comp; Append[#1, #2],
						#1
					]
			    ],
			    #1
			] &, 
			{}, 
			Range@Length[tensors]
		]
	]
]}, If[TrueQ[OptionValue["Indices"]], indices, tensors[[indices]]]
]];

IndexData[Spinor] = Index[2, "Greek", 1];
IndexData[DottedSpinor] = Index[2, "Greek", 1, OverDot];
IndexData[SpaceTime] = Index[4, "Greek", 12];
IndexData[RIndex[rep_]] := 
  Index[Times @@ DimR[RSymmetry[], rep], "Latin", 9, Subscript[#, RepName[RSymmetry[], rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]] &];
RIndex[rep_Integer] /; RSymmetry[] =!= U1 := RIndex[SimpleRepInputConversion[RSymmetry[], rep]];

\[Epsilon]Lower = 
  Tensor[{{"\[Epsilon]", Lowered[Spinor], Lowered[Spinor]}}];
BuildTensor[{"\[Epsilon]", Lowered[Spinor], Lowered[Spinor]}] = 
  SparseArray@LeviCivitaTensor[2];

\[Epsilon]Upper = 
  Tensor[{{"\[Epsilon]", Raised[Spinor], Raised[Spinor]}}];
BuildTensor[{"\[Epsilon]", Raised[Spinor], Raised[Spinor]}] = 
  SparseArray@LeviCivitaTensor[2];

\[Epsilon]LowerDot = 
  Tensor[{{"\[Epsilon]", Lowered[DottedSpinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\[Epsilon]", Lowered[DottedSpinor], 
    Lowered[DottedSpinor]}] = SparseArray@LeviCivitaTensor[2];

\[Epsilon]UpperDot = 
  Tensor[{{"\[Epsilon]", Raised[DottedSpinor], Raised[DottedSpinor]}}];
BuildTensor[{"\[Epsilon]", Raised[DottedSpinor], 
    Raised[DottedSpinor]}] = SparseArray@LeviCivitaTensor[2];
    
DeclareTensorSymmetry["\[Epsilon]", {{Cycles[{{1,2}}], -1}}];
    
Unprotect[Tensor];
Tensor[{x___, f_Field, y___}] := Tensor[{x, ToTensor[f][[1, 1]], y}];
Protect[Tensor];

EpsilonProducts[tensor_, {dj1_, dj2_}] := 
With[{products = 
     Table[TensorProduct[TensorPermute[tensor, p], 
       Switch[dj1, 1, \[Epsilon]Lower, -1, \[Epsilon]Upper,
          2, TensorProduct[\[Epsilon]Lower, \[Epsilon]Lower],
          -2, TensorProduct[\[Epsilon]Upper, \[Epsilon]Upper], 
        0, ## &[]], 
       Switch[dj2, 1, \[Epsilon]LowerDot, -1, \[Epsilon]UpperDot,
          2, TensorProduct[\[Epsilon]LowerDot, \[Epsilon]LowerDot],
          -2, TensorProduct[\[Epsilon]UpperDot, \[Epsilon]UpperDot],  
        0, ## &[]]], {p, NontrivialPermutations[tensor]}]},
   With[{pairs = 
      Flatten[{
      	If[dj1 <= -1, 
       		Transpose[{Position[Indices[products[[1]]], Lowered[Spinor]][[;; -2 dj1, 1]], Position[Indices[products[[1]]], Raised[Spinor]][[;; -2 dj1, 1]]}], 
         	Nothing
         ], 
        If[dj2 <= -1, 
        	Transpose[{Position[Indices[products[[1]]], Lowered[DottedSpinor]][[;; -2 dj2, 1]], Position[Indices[products[[1]]], Raised[DottedSpinor]][[;; -2 dj2, 1]]}], 
        	Nothing
      	]
      }, 1]
    },
    Flatten@
     Table[With[{c = Contract[prod, pairs]}, 
       Table[TensorPermute[c, perm], {perm, 
         NontrivialPermutations[c]}]], {prod, products}]
    ]
];

OperatorsWithQuantumNumbers[multiplet_, rep_, dim_, {j1_, j2_}, y_] :=
    DeleteDuplicates@Flatten[{
        EpsilonProducts[Tensor[{#}], {j1, j2} - Spin[#]] & /@ 
         Select[multiplet, 
             SimpleRepInputConversion[$RSymmetry, rep] == SimpleRepInputConversion[$RSymmetry, RRep[#]] && 
             y == #[[5]] && 
             ScalingDimension[#] == dim && 
             AllTrue[Spin[#] - {j1, j2}, IntegerQ] &
         ],
        EpsilonProducts[Tensor[{{"\[PartialD]", Lowered[Spinor], Lowered[DottedSpinor]}, #}], {j1 - 1/2, j2 - 1/2} - Spin[#]] & /@ 
         Select[multiplet, 
             SimpleRepInputConversion[$RSymmetry, rep] == SimpleRepInputConversion[$RSymmetry, RRep[#]] && 
             y == #[[5]] && 
             ScalingDimension[#] == dim - 1 && 
             AllTrue[Spin[#] - {j1 - 1/2, j2 - 1/2}, IntegerQ] &
         ]
        }
    ];
    
TwoPtRInvariant[rep1_, rep2_] :=
   Tensor[{{"\[Delta]", Raised[RIndex[rep1]], Raised[RIndex[rep2]]}}];
ConjugateTwoPtRInvariant[rep1_, rep2_] :=
   Tensor[{{"\[Delta]", Lowered[RIndex[rep1]], Lowered[RIndex[rep2]]}}];

ThreePtRInvariant[{rep1_, rep2_}, target_] :=
   Tensor[{{"C", Raised[RIndex[target]], Lowered[RIndex[rep1]], 
      Lowered[RIndex[rep2]]}}];
ConjugateThreePtRInvariant[{rep1_, rep2_}, target_] :=
   Tensor[{{"C", Lowered[RIndex[target]], Raised[RIndex[rep1]], 
      Raised[RIndex[rep2]]}}];

ThreePtRInvariant[r1_, r2_, r3_] :=
   Tensor[{{"C", Raised[RIndex[r1]], Raised[RIndex[r2]], Raised[RIndex[r3]]}}];
ConjugateThreePtRInvariant[r1_, r2_, r3_] :=
   Tensor[{{"C", Lowered[RIndex[r1]], Lowered[RIndex[r2]], Lowered[RIndex[r3]]}}];
     
BuildTensor[arg : {"\[Delta]", Raised@RIndex[r1_], Raised@RIndex[r2_]}] := twopt[r1, r2];

BuildTensor[arg : {"\[Delta]", Lowered@RIndex[r1_], Lowered@RIndex[r2_]}] := SparseArray@Inverse[twopt[r2, r1]];

$customInvariants = False;
     
twopt::undefined = "The two-point invariant for representations (``, ``) has not been defined.";
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 =!= SimpleRepInputConversion[RSymmetry[], r1] := twopt[SimpleRepInputConversion[RSymmetry[], r1], r2];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r2 =!= SimpleRepInputConversion[RSymmetry[], r2] := twopt[r1, SimpleRepInputConversion[RSymmetry[], r2]];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && !OrderedQ[{r1, r2}] := Transpose[twopt[r2, r1]];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && OrderedQ[{r1, r2}] := 
If[$customInvariants,
   Message[twopt::undefined, RepName[RSymmetry[], r1], RepName[RSymmetry[], r2]];,
   SparseArray[IrrepInProduct[$RSymmetry, {r1, r2}, singRep[$RSymmetry], TensorForm -> True][[1, 1, ;; , ;; , 1]]]
];
 
threept::undefined = "The three-point invariant for representations (``, ``, ``) has not been defined.";
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 =!= SimpleRepInputConversion[RSymmetry[], r1] :=
   threept[SimpleRepInputConversion[RSymmetry[], r1], r2, r3];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r2 =!= SimpleRepInputConversion[RSymmetry[], r2] :=
   threept[r1, SimpleRepInputConversion[RSymmetry[], r2], r3];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r3 =!= SimpleRepInputConversion[RSymmetry[], r3] :=
   threept[r1, r2, SimpleRepInputConversion[RSymmetry[], r3]];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && r3 === SimpleRepInputConversion[RSymmetry[], r3] && !OrderedQ[{r1, r2, r3}] := 
   TensorTranspose[threept @@ Sort[{r1,r2,r3}], Ordering[{r1,r2,r3}]];
   
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && r3 === SimpleRepInputConversion[RSymmetry[], r3] && OrderedQ[{r1, r2, r3}] := 
If[$customInvariants,
   Message[threept::undefined, RepName[RSymmetry[], r1], RepName[RSymmetry[], r2], RepName[RSymmetry[], r3]];,
   SparseArray[IrrepInProduct[$RSymmetry, {r1, r2}, r3, ConjugateTargetRep -> True, TensorForm -> True][[1, 1]]]
];

SetTwoPtRInvariant[r1_, r2_, mat_] := Module[{reps = SimpleRepInputConversion[RSymmetry[], #] & /@ {r1, r2}, sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = Ordering[reps];
 	twopt[sorted[[1]], sorted[[2]]] = SparseArray@TensorTranspose[mat, order];
]

SetThreePtRInvariant[r1_, r2_, r3_, mat_] := Module[{reps = SimpleRepInputConversion[RSymmetry[], #] & /@ {r1, r2, r3}, sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = InversePermutation@Ordering[reps];
 	threept[sorted[[1]], sorted[[2]], sorted[[3]]] = SparseArray@TensorTranspose[mat, order];
]

BuildTensor[
   arg : {"C", Raised@RIndex[target_], Raised@RIndex[rep1_], 
     Raised@RIndex[rep2_]}] := threept[target, rep1, rep2];

BuildTensor[
   arg : {"C", Lowered@RIndex[target_], Raised@RIndex[rep1_], 
     Raised@RIndex[rep2_]}] :=
    Components[ConjugateTwoPtRInvariant[target, ConjugateIrrep[$RSymmetry, target]]] . threept[ConjugateIrrep[$RSymmetry, target], rep1, rep2];

BuildTensor[
   arg : {"C", Raised@RIndex[target_], Lowered@RIndex[rep1_], 
     Lowered@RIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Lowered[RIndex[ConjugateIrrep[$RSymmetry, target]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep1]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep2]]]}}], 
       TwoPtRInvariant[ConjugateIrrep[$RSymmetry, target], target],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep1], rep1],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
    
BuildTensor[
   arg : {"C", Lowered@RIndex[target_], Lowered@RIndex[rep1_], 
     Lowered@RIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[RIndex[ConjugateIrrep[$RSymmetry, target]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep1]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep2]]]}}], 
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, target], target],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep1], rep1],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
      
buildExpression[LoopInvariant[edges_]] := 
 Module[{internals = SortBy[DeleteDuplicates@Cases[edges, _Internal, All], FirstCase[edges, {#, External[i_], _} :> i] &], pi, cs, deltas},
  pi = PositionIndex[internals];
  cs = Association@
    Thread[internals -> 
      Table[ThreePtRInvariant[{FirstCase[edges, {_Internal, i, rep_} :> ConjugateIrrep[$RSymmetry, rep]], 
         FirstCase[edges, {i, _Internal, rep_} :> rep]}, 
        FirstCase[edges, {i, _External, rep_} :> rep]], {i, 
        internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtRInvariant[rep, ConjugateIrrep[$RSymmetry, rep]]}];
  Contract[
   TensorProduct[Sequence @@ cs, Sequence @@ (deltas[[;; , 3]])], 
   Flatten[Table[{{3 (pi[deltas[[i, 1]]][[1]] - 1) + 3, 
       3 Length[internals] + 2 i - 
        1}, {3 (pi[deltas[[i, 2]]][[1]] - 1) + 2, 
       3 Length[internals] + 2 i}}, {i, Length[deltas]}], 1]]
  ];

fixedSignaturePermutation[perm_, stuff_] := 
  Table[With[{inds = Position[stuff, stuff[[i]], 1][[;; , 1]]}, 
    With[{inds2 = Position[perm, #, 1][[1, 1]] & /@ inds}, 
     inds[[Position[Sort[inds2], inds2[[Position[inds, i][[1, 1]]]]][[
       1, 1]]]]]], {i, Length[perm]}];

buildExpression[TreeInvariant[edges_]] := 
 Module[{internals = 
    SortBy[DeleteDuplicates@Cases[edges, _Internal, All], 
     FirstCase[edges, {#, External[i_], _} :> i] &], pi, cs, deltas},
  pi = PositionIndex[internals];
  cs = Association@
    Thread[internals -> 
      Table[ConjugateThreePtRInvariant[
        Cases[edges, {i, _External, rep_} :> rep], 
        If[Cases[edges, {i, _Internal, _}] != {}, 
         FirstCase[edges, {i, _Internal, rep_} :> rep], 
         FirstCase[
          edges, {_Internal, i, rep_} :> 
           ConjugateIrrep[$RSymmetry, rep]]]], {i, internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtRInvariant[rep, ConjugateIrrep[$RSymmetry, rep]]}];
  TensorPermute[
   Contract[
    TensorProduct[Sequence @@ cs, Sequence @@ (deltas[[;; , 3]])], 
    Flatten[Table[{{3 (pi[deltas[[i, 1]]][[1]] - 1) + 1, 
        3 Length[internals] + 2 i - 
         1}, {3 (pi[deltas[[i, 2]]][[1]] - 1) + 1, 
        3 Length[internals] + 2 i}}, {i, Length[deltas]}], 1]], 
   InversePermutation@
    fixedSignaturePermutation[
     Join @@ Table[
       Cases[edges, {i, External[j_], _} :> j], {i, internals}], 
     Join @@ Table[
       Cases[edges, {i, _External, rep_} :> rep], {i, internals}]]]
  ]
repTree[reps_] := 
 Graph[Flatten[
   Table[{i, r}, {i, 0, Length[reps]}, {r, allReps[]}], 1], 
  Flatten[Table[
    DirectedEdge[{i, r}, {i + 1, rp}],
    {i, 0, 3}, {r, allReps[]},
    {rp, 
     Intersection[
      Select[ReduceRepProduct[
         $RSymmetry, {r, reps[[i + 1]]}], #[[2]] == 1 &][[;; , 1]], 
      allReps[]]}
    ]], VertexCoordinates -> 
   Flatten[Table[{i, (allReps[])[[j]]} -> {j, i}, {i, 0, 
      Length[reps]}, {j, Length[allReps[]]}], 1]]
loopGraphs[reps_] := Flatten@Table[
    LoopInvariant[
     Join[Table[{Internal[i], External[i], reps[[i]]}, {i, 
        Length[reps]}], 
      Table[{Internal[perm[[i]]], Internal[perm[[Mod[i, 4] + 1]]], 
        path[[i + 1, 2]]}, {i, Length[reps]}]]],
    {perm, {{1, 2, 3, 4}, {1, 3, 4, 2}, {1, 4, 2, 3}}},
    {start, 
     SimpleRepInputConversion[$RSymmetry, #] & /@ allReps[]},
    {path, 
     FindPath[
      repTree[reps[[perm]]], {0, start}, {Length[reps], start}, {4}, 
      All]}];
treeGraphs[reps_] := Flatten@Table[
   TreeInvariant[{{Internal[1], External[p[[1]]], 
      reps[[p[[1]]]]}, {Internal[1], External[p[[2]]], 
      reps[[p[[2]]]]}, {Internal[2], External[p[[3]]], 
      reps[[p[[3]]]]}, {Internal[2], External[p[[4]]], 
      reps[[p[[4]]]]}, {Internal[1], Internal[2], rep}}],
   {p, {{1, 2, 3, 4}, {1, 3, 4, 2}, {1, 4, 2, 3}}},
   {rep, 
    Intersection[allReps[], 
     Select[ReduceRepProduct[$RSymmetry, reps[[p[[;; 2]]]]], #[[2]] == 1 &][[;; , 1]], 
     ConjugateIrrep[$RSymmetry, #] & /@ (Select[ReduceRepProduct[$RSymmetry, reps[[p[[3 ;;]]]]], #[[2]] == 1 &][[;; , 1]])]}
   ];
   
numInvariants[reps_] := If[# == {}, 0, #[[1, 2]]] &@ Select[ReduceRepProduct[RSymmetry[], reps], #[[1]] == singRep[RSymmetry[]] &];

InvariantFourPtGraphs[reps_] /; Length[reps] == 4 := InvariantFourPtGraphs[reps] = 
 With[{graphs = SortBy[Join[treeGraphs[reps], loopGraphs[reps]], {Max[Cases[#[[1]], {_Internal, _Internal, rep_} :> Times @@ DimR[RSymmetry[], rep]]], 
  Length[Cases[#[[1]], _Internal, All]]} &]},
    graphs[[IndependentSet[buildExpression /@ graphs, "MaxIndependent" -> numInvariants[reps], "Indices" -> True]]]
  ]
   
InvariantFourPts[reps_] /; Length[reps] == 4 := InvariantFourPts[reps] = With[{order = Ordering[SimpleRepInputConversion[$RSymmetry, #]& /@ reps]},
   SparseArray@(TensorTranspose[CanonicallyOrderedComponents@buildExpression[#], order] & /@ InvariantFourPtGraphs[reps])
];

allReps[] := DeleteDuplicates[RRep /@ Flatten[Multiplet /@ $multipletIndices]];

cols = {Darker@Green, Blue, Black, Orange, Red, Purple, Yellow};
arrow[{p1_, p2_}] := {Line[{p1, p2}], Arrow[{p1, .4 p1 + .6 p2}]};
reverseArrow[{p1_, p2_}] := arrow[{p2, p1}];
repStyles[multreps_] := 
 Association@With[{reps = 
    SortBy[#, Function[rep, {Times @@ DimR[RSymmetry[], rep], MatchQ[RepName[RSymmetry[], rep], _OverBar]}]] & /@ 
     GroupBy[DeleteDuplicates[DeleteCases[multreps, singRep[RSymmetry[]]]], 
      ConjugateIrrep[RSymmetry[], #] === 
        SimpleRepInputConversion[RSymmetry[], #] &]}, 
  Join[{singRep[RSymmetry[]] -> {White, Line}},Thread[
    reps[True] -> 
     Table[{cols[[i]], Thick, Dashed, Line}, {i, Length[reps[True]]}]],
   Thread[
    reps[False] -> 
     Table[{cols[[Ceiling[i/2]]], Thick, 
       If[OddQ[i], arrow, reverseArrow]}, {i, Length[reps[False]]}]]
   ]
  ];

Format[TreeInvariant[edges_], TraditionalForm] := 
 Module[{vertices = 
    DeleteDuplicates@Cases[edges, _Internal | _External, All], 
   potential = 100 #^2 + 10/#^2 &, pos, sol},
  pos = Association@
    Thread[vertices -> (vertices /. {Internal[i_] :> {x[i], y[i]}, 
         External[
           i_] :> {Cos[(2 Pi (i - 1/2))/Count[vertices, _External]], 
           Sin[(2 Pi (i - 1/2))/Count[vertices, _External]]}})];
  sol = NMinimize[
     Sum[If[MatchQ[e[[1]], _External] || MatchQ[e[[2]], _External], 3,
         1] Boole[!MatchQ[Last[e], singRep[RSymmetry[]] | 1]] potential[Norm[pos[e[[1]]] - pos[e[[2]]]]], {e, edges}], 
     Cases[Values[pos], _x | _y, All]];
  Graphics[{PointSize[.05], Arrowheads[.1], 
    Join[Table[{Sequence @@ 
        Most[repStyles[allReps[]][
          SimpleRepInputConversion[$RSymmetry, 
           If[MatchQ[e[[1]], _External] || MatchQ[e[[2]], _External], 
            e[[3]], ConjugateIrrep[$RSymmetry, e[[3]]]]]]], 
       Tooltip[Last[
           repStyles[allReps[]][
            SimpleRepInputConversion[$RSymmetry, 
             If[MatchQ[e[[1]], _External] || 
               MatchQ[e[[2]], _External], e[[3]], 
              ConjugateIrrep[$RSymmetry, e[[3]]]]]]][{pos[e[[1]]], 
           pos[e[[2]]]}], RepName[$RSymmetry, e[[3]]]] /. sol[[2]]}, {e, edges}], 
     Table[{If[MatchQ[v, _External], Black, Gray], 
       Tooltip[Point[pos[v] /. sol[[2]]], v]}, {v, vertices}]]}, 
   ImageSize -> 150]
  ]

Format[LoopInvariant[edges_], TraditionalForm] := 
  Format[TreeInvariant[edges], TraditionalForm];
  
FourPtRInvariant[reps_, i_] := Tensor[{{RInvariant[i], Sequence @@ Table[Raised[RIndex[r]], {r, reps}]}}];
BuildTensor[{RInvariant[i_], Raised[RIndex[r1_]], Raised[RIndex[r2_]], Raised[RIndex[r3_]], Raised[RIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];
Format[RInvariant[i_], TraditionalForm] := Subscript["C", i];

ToTensor[f_Field] := 
  Tensor[{{f[[1]], Raised[RIndex[RRep[f]]], 
     Sequence @@ Table[Lowered[Spinor], 2 Spin[f][[1]]], 
     Sequence @@ Table[Lowered[DottedSpinor], 2 Spin[f][[2]]]}}];
     
BuildTensor[{name_String, idxs___}] := 
   With[{perms = SymmetryPermutations[TensorSymmetries[name], "Minimal" -> False]},
    SparseArray[
     Flatten@Table[($i /@ Range@Length[{idxs}]) :> 
       Evaluate[First[Sort[
          Table[p[[2]] (Component[name] @@ ($i /@ Range@Length[{idxs}])[[PermutationList[p[[1]], Length[{idxs}]]]]), {p, perms}]]]], 
       Evaluate[
        Sequence @@ 
         Table[{$i[ii], IndexData[{idxs}[[ii, 1]]][[1]]}, {ii, 
           Length[{idxs}]}]]], 
     Table[IndexData[{idxs}[[ii, 1]]][[1]], {ii, Length[{idxs}]}]]
   ];
   
symmetryInconsistent[gen1_, gen2_] := Max[Length /@ Values[GroupBy[DeleteDuplicates@Join[SymmetryPermutations[gen1], SymmetryPermutations[gen2]], First]]] > 1;

Options[PossibleQActions] = {"QBar" -> False};
PossibleQActions[f : Field[_, rep_, dim_, {j1_, j2_}, y_], opt: OptionsPattern[]] := PossibleQActions[f, opt] = If[TrueQ[OptionValue["QBar"]],
   qToQBar /@ PossibleQActions[Conjugate[f], "QBar" -> False],
   DeleteDuplicates[(SymmetryReduce /@
    Select[
     With[ {reps = ReduceRepProduct[$RSymmetry, {fundRep[$RSymmetry], rep}][[;; , 1]]},
        Flatten[
         Table[
             Contract[TensorProduct[ConjugateThreePtRInvariant[{fundRep[$RSymmetry], rep}, rep2], op], {{1, 3 + Position[Indices[op], _Raised][[1, 1]]}}], 
            {rep2, reps}, 
            {op, OperatorsWithQuantumNumbers[multipletOf[f], rep2, dim + 1/2, {j1 + 1/2, j2}, y + 1]}
         ]
        ]
     ], Function[t, With[{si = Position[Indices[t], Lowered[Spinor]][[2;;,1]], dsi = Position[Indices[t], Lowered[DottedSpinor]][[;;,1]]},
      	Length[Cases[TensorSymmetries[t], {Cycles[{{x1_, x2_}}], -1} /; (MemberQ[si, x1] && MemberQ[si, x2]) || (MemberQ[dsi, x1] && MemberQ[dsi, x2])]] == 0  
     ]     
     ]]
   ) /. a_ b_ /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] :> b ]
];
            
Options[SUSYCoefficient] = {"QBar" -> False};
Format[SUSYCoefficient[name_, idx_, opt : OptionsPattern[]], TraditionalForm] := Subscript[
	If[OptionValue[SUSYCoefficient, opt, "QBar"], "\!\(\*OverscriptBox[\(\[ScriptA]\), \(_\)]\)", "\[ScriptA]"],
   Row[{name, ",", idx}]];
   
fieldPerm[t_] := With[{inds = 
     Position[
       Indices[t], (Lowered | Raised)[
        Spinor | DottedSpinor]][[If[
         Symbolic[t][[2, 1]] == "\[PartialD]", 3, 1] ;; If[Symbolic[t][[-1,1]] == "\[Epsilon]", If[Symbolic[t][[-2,1]] == "\[Epsilon]", -5, -3], -1], 1]], 
    f = name2field[If[Symbolic[t][[2, 1]] == "\[PartialD]", Symbolic[t][[3, 1]], 
         Symbolic[t][[2, 1]]]]}, 
	PermutationPower[Cycles[{inds}], 2 Spin[f][[1]]]
];
(* specific to QAnsatz expressions *)
qToQBar[expr_] := expr /. t_Contract :> qToQBar[t]/. SUSYCoefficient[name_, idx_, "QBar" -> False] :> SUSYCoefficient[Conjugate[name2field[name]][[1]], idx, "QBar" -> True];
qToQBar[Contract[t_, pairs_]] := With[{fp = fieldPerm[t]},
   Contract[qToQBar[t], If[Symbolic[t][[2,1]] == "\[PartialD]", pairs /. {1 -> 3, 4 -> 5, 5 -> 4}, pairs /. 1 -> 3] /. Thread[Range@Length[Indices[t]] -> PermutationList[InversePermutation[fp], Length@Indices[t]]]]
];
qToQBar[TensorPermute[t_, perm_]] := 
  With[{fp = fieldPerm[t]}, 
   TensorPermute[qToQBar[t], PermutationList[
     PermutationProduct[fp, 
      If[Symbolic[t][[2, 1]] == "\[PartialD]", 
       PermutationProduct[Cycles[{{4, 5}}], perm, Cycles[{{4, 5}}]], 
       perm], InversePermutation[fp]], 
     Length[perm]]]];
qToQBar[t_Tensor] := (*If[
   Indices[t][[3]] =!= Raised[RIndex[ConjugateIrrep[$RSymmetry, fundRep[$RSymmetry]]]], 
   Identity, 
   TensorPermute[#, PermutationList[Cycles[{{1,2}}], Length[Indices[t]]]] &] @ *)(
t /. {{"C", Lowered[RIndex[i_]], Raised[RIndex[j_]], Raised[RIndex[k_]]} :> {"C", Raised[RIndex[ConjugateIrrep[$RSymmetry, k]]], Lowered[RIndex[j]], Lowered[RIndex[ConjugateIrrep[$RSymmetry, i]]]},
   {"\[Epsilon]", Raised[Spinor], Raised[Spinor]} -> {"\[Epsilon]", Raised[DottedSpinor], Raised[DottedSpinor]},
   {"\[Epsilon]", Lowered[Spinor], Lowered[Spinor]} -> {"\[Epsilon]", Lowered[DottedSpinor], Lowered[DottedSpinor]},
   {"\[Epsilon]", Raised[DottedSpinor], Raised[DottedSpinor]} -> {"\[Epsilon]", Raised[Spinor], Raised[Spinor]},
   {"\[Epsilon]", Lowered[DottedSpinor], Lowered[DottedSpinor]} -> {"\[Epsilon]", Lowered[Spinor], Lowered[Spinor]},
   {name_, idxs___} /; MemberQ[Flatten[$multiplet /@ $multipletIndices][[;;,1]], name] :> Symbolic[ToTensor[Conjugate[name2field[name]]]][[1]]
});

Options["QAnsatz"] = {"QBar" -> False, "Symmetrized" -> True};
QAnsatz[f_Field, opt : OptionsPattern[]] := 
  QAnsatz[f, opt] = If[OptionValue["Symmetrized"],
   If[OptionValue["QBar"],
      QAnsatz[Conjugate[f], "QBar" -> False] /. SUSYCoefficient[name_, idx_, "QBar" -> False] t_ :> SUSYCoefficient[Conjugate[name2field[name]][[1]], idx, "QBar" -> True] qToQBar[t],
	   With[{unsym = QAnsatz[f, "QBar" -> OptionValue["QBar"], "Symmetrized" -> False]},
	      Which[unsym === 0, 0,
	         Head[unsym] =!= Plus, unsym, 
	         True, Module[{terms = Cases[unsym, coeff_SUSYCoefficient t_ :> {coeff, t}], groups, rules},
	          	  groups = Values[#[[;;, 1]] & /@ GroupBy[terms, With[{si = Position[Indices[#[[2]]], Lowered[Spinor]][[2;;, 1]], dsi = Position[Indices[#[[2]]], Lowered[DottedSpinor]][[;;, 1]]},
	          	  	{Symbolic[#[[2]]][[;;,1]], First@Sort[Flatten[Table[TensorPermutation[#[[2]]] /. Thread[si -> si[[p]]] /. Thread[dsi -> dsi[[p2]]], {p, Permutations[Range[Length[si]]]}, {p2, Permutations[Range[Length[dsi]]]}], 1]]}  
	          	  ] &]];
	          	  rules = Flatten[Table[g[[i]] -> g[[1]], {g, groups}, {i, 2, Length[g]}]];
	          	  unsym /. rules
	         ]
	      ]
	   ]
   ]
   ,
   With[{terms = PossibleQActions[f, "QBar" -> OptionValue["QBar"]]}, 
    Sum[SUSYCoefficient[f[[1]], i, "QBar" -> OptionValue["QBar"]] terms[[i]], {i, Length[terms]}]
   ]
];

\[Eta]Upper = 
  Tensor[{{"\[Eta]", Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\[Eta]", Raised[SpaceTime], Raised[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[{-SignatureFactor[]^2, 1, 1, 1}]];

\[Eta]Lower = 
  Tensor[{{"\[Eta]", Lowered[SpaceTime], Lowered[SpaceTime]}}];
BuildTensor[{"\[Eta]", Lowered[SpaceTime], Lowered[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[{-SignatureFactor[]^2, 1, 1, 1}]];

\[Sigma]Lower = 
  Tensor[{{"\[Sigma]", Lowered[SpaceTime], Lowered[Spinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\[Sigma]", Lowered[SpaceTime], Lowered[Spinor], 
    Lowered[DottedSpinor]}] := 
  SparseArray[{-SignatureFactor[], 1, 1, 1} (PauliMatrix /@ Range[0, 3])];
\[Sigma]BarLower = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Lowered[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Lowered[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}] := 
  SparseArray[{SignatureFactor[], 1, 1, 1} (PauliMatrix /@ Range[0, 3])];

\[Sigma]Upper = 
  Tensor[{{"\[Sigma]", Raised[SpaceTime], Lowered[Spinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\[Sigma]", Raised[SpaceTime], Lowered[Spinor], 
    Lowered[DottedSpinor]}] := 
  Components@
   Contract[TensorProduct[\[Eta]Upper, \[Sigma]Lower], {{2, 3}}];

\[Sigma]BarUpper = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Raised[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Raised[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}] := 
  Components@
   Contract[TensorProduct[\[Eta]Upper, \[Sigma]BarLower], {{2, 3}}];

\[Sigma]CommLower = 
  Tensor[{{"\[Sigma]", Lowered[SpaceTime], Lowered[SpaceTime], 
     Lowered[Spinor], Lowered[Spinor]}}];
BuildTensor[{"\[Sigma]", Lowered[SpaceTime], Lowered[SpaceTime], 
    Lowered[Spinor], Lowered[Spinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Sigma]Lower, \[Sigma]BarLower, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Sigma]Lower, \[Sigma]BarLower, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}], {3, 2, 1, 4}]], 
    InversePermutation@
     Ordering[{Lowered[SpaceTime], Lowered[SpaceTime], 
       Lowered[Spinor], Lowered[Spinor]}]];

\[Sigma]CommLowerDot = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Lowered[SpaceTime], Lowered[SpaceTime], Lowered[DottedSpinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Lowered[SpaceTime], Lowered[SpaceTime], Lowered[DottedSpinor], 
    Lowered[DottedSpinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarLower, \
\[Sigma]Lower], {{2, 4}, {5, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarLower, \
\[Sigma]Lower], {{2, 4}, {5, 7}}], {1, 3, 2, 4}]], 
    InversePermutation@
     Ordering[{Lowered[SpaceTime], Lowered[SpaceTime], 
       Lowered[DottedSpinor], Lowered[DottedSpinor]}]];

\[Sigma]CommUpper = 
  Tensor[{{"\[Sigma]", Raised[SpaceTime], Raised[SpaceTime], 
     Lowered[Spinor], Lowered[Spinor]}}];
BuildTensor[{"\[Sigma]", Raised[SpaceTime], Raised[SpaceTime], 
    Lowered[Spinor], Lowered[Spinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Sigma]Upper, \[Sigma]BarUpper, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Sigma]Upper, \[Sigma]BarUpper, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}], {3, 2, 1, 4}]], 
    InversePermutation@
     Ordering[{Raised[SpaceTime], Raised[SpaceTime], Lowered[Spinor], 
       Lowered[Spinor]}]];

\[Sigma]CommUpperDot = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Raised[SpaceTime], Raised[SpaceTime], Lowered[DottedSpinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Raised[SpaceTime], Raised[SpaceTime], Lowered[DottedSpinor], 
    Lowered[DottedSpinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarUpper, \
\[Sigma]Upper], {{2, 4}, {5, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarUpper, \
\[Sigma]Upper], {{2, 4}, {5, 7}}], {1, 3, 2, 4}]], 
    InversePermutation@
     Ordering[{Raised[SpaceTime], Raised[SpaceTime], 
       Lowered[DottedSpinor], Lowered[DottedSpinor]}]];
       
Format[x[i_, j_], TraditionalForm] := Superscript[Subscript[x, i], j];
Format[SpacetimePoint[i_], TraditionalForm] := Subscript["\!\(TraditionalForm\`x\)", i];
Format[SpacetimeSeparation[i_, j_], TraditionalForm] := Subscript["\!\(TraditionalForm\`x\)", Row[{i, j}]];

XX[i_] := Tensor[{{SpacetimePoint[i], Raised[SpaceTime]}}];
BuildTensor[{SpacetimePoint[i_], Raised[SpaceTime]}] := SparseArray@Table[x[i, k], {k, 4}];

XX[i_, j_] := Tensor[{{SpacetimeSeparation[i, j], Raised[SpaceTime]}}];
BuildTensor[{SpacetimeSeparation[i_, j_], Raised[SpaceTime]}] := SparseArray@Table[x[i, k] - x[j, k], {k, 4}];

SetAttributes[XXSquared, Orderless];
AppendTo[TensorTools`Private`explicitRules, 
  XXSquared[i_] :> 
   Components[
    Contract[
     TensorProduct[XX[i], \[Eta]Lower, XX[i]], {{1, 2}, {3, 4}}]]];
AppendTo[TensorTools`Private`explicitRules, 
  XXSquared[i_, j_] :> 
   Components[
    Contract[
     TensorProduct[XX[i, j], \[Eta]Lower, 
      XX[i, j]], {{1, 2}, {3, 4}}]]];
Format[XXSquared[i_], TraditionalForm] := (Subscript["\!\(TraditionalForm\`x\)", Row[{i}]])^2;
Format[XXSquared[i_, j_], TraditionalForm] := (Subscript["\!\(TraditionalForm\`x\)", 
   Row[{i, j}]])^2;
MakeBoxes[Power[XXSquared[xs__], n_], TraditionalForm] := 
  If[n == 1/2, 
   TemplateBox[{SubscriptBox["x", StringJoin @@ (ToString /@ {xs})]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["x", StringJoin @@ (ToString /@ {xs})], 2 n]];
    
SpinorX[i_, j_] := Contract[TensorProduct[XX[i, j], \[Sigma]Lower], {{1, 2}}];

AddTensorHead[TensorDerivative];
TensorDerivative[0, _] := 0;

Symbolic[
   TensorDerivative[
    a_. t : (_Tensor | _Contract | _TensorPermute | 
        TensorProduct[x_, y__] | _Correlator), i_]] := 
  Join[{{("\[PartialD]")^Row[{"(", i, ")"}], 
     Lowered[SpaceTime]}, {"("}, If[a =!= 1, {a}, Nothing]}, 
   Symbolic[t], {{")"}}];
Symbolic[TensorDerivative[a_, i_]] /; FreeQ[a, Tensor] := 
  Join[{{("\[PartialD]")^Row[{"(", i, ")"}], 
     Lowered[SpaceTime]}, {"("}, 
    If[a =!= 1, {a}, Nothing]}, {{")"}}];
Indices[TensorDerivative[t_, i_]] := 
  Prepend[Indices[t], Lowered[Spacetime]];
Permutation[TensorDerivative[t_, i_]] := Join[{1}, 1 + Permutation[t]];
DisplayTemplate[td_TensorDerivative] := 
  DisplayTemplate[Symbolic[td]];
Format[td_TensorDerivative, TraditionalForm] := 
  DisplayTemplate[td] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];
   
InactiveComponents[TensorDerivative[t_, i_]] := 
  With[{comps = Components[t]}, If[ArrayQ[comps],
    SparseArray[
     Flatten@Table[
       ArrayRules@comps /. 
        HoldPattern[a_ -> b_] :> Prepend[a, k] -> D[b, x[i, k]], {k, 
        4}], Prepend[Dimensions[comps], 4]],
    SparseArray[Table[D[comps, x[i, k]], {k, 4}]]
    ]
   ];

TensorDerivative[a_. Contract[t_, pairs_], i_] := 
  Contract[TensorDerivative[a t, i], pairs + 1];
TensorDerivative[a_. TensorPermute[t_, perm_, OptionsPattern[]], i_] :=
   TensorPermute[TensorDerivative[a t, i], Join[{1}, perm + 1]];
TensorSpinorDerivative[t_, i_] :=(*1/Sqrt[2]*)
  Contract[
   TensorProduct[\[Sigma]Upper, TensorDerivative[t, i]], {{1, 4}}];

RPart[Tensor[names_]] := 
  Tensor[Select[names, 
    MatchQ[#[[1]], RInvariant[_]] || 
      MemberQ[{"C", "\[Delta]"}, #[[1]]] &]];
NonRPart[Tensor[names_]] := 
  Tensor[Select[
    names, ! (MatchQ[#[[1]], RInvariant[_]] || 
        MemberQ[{"C", "\[Delta]"}, #[[1]]]) &]];

RPart[TensorPermute[t_, perm_]] := 
  With[{rInds = 
     Position[Indices[t], Raised[_RIndex] | Lowered[_RIndex]][[;; , 
       1]]}, TensorPermute[RPart[t], 
    Ordering[InversePermutation[perm][[rInds]]]]];
NonRPart[TensorPermute[t_, perm_]] := 
  With[{nonrInds = 
     Position[Indices[t], 
       Raised[Except[_RIndex]] | Lowered[Except[_RIndex]]][[;; , 1]]},
    TensorPermute[NonRPart[t], 
    Ordering[InversePermutation[perm][[nonrInds]]]]
   ];

RPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], 
       Raised[Except[_RIndex]] | Lowered[Except[_RIndex]]][[;; , 1]], 
    inner = RPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[pairs, 
         MatchQ[Indices[t][[#[[1]], 1]], _RIndex] &] /. 
        n_Integer :> 
         pi[[ip[[n]] - Length@Select[dels, # < ip[[n]] &]]])]
    ]
   ];
NonRPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], Raised[_RIndex] | Lowered[_RIndex]][[;; , 
       1]], inner = NonRPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[
         pairs, ! MatchQ[Indices[t][[#[[1]], 1]], _RIndex] &] /. 
        n_Integer :> 
         pi[[ip[[n]] - Length@Select[dels, # < ip[[n]] &]]])]
    ]
   ];

RPart[TensorProduct[a_, b__]] := 
  TensorProduct[RPart[a], RPart[TensorProduct[b]]];
NonRPart[TensorProduct[a_, b__]] := 
  TensorProduct[NonRPart[a], NonRPart[TensorProduct[b]]];

RPart[TensorDerivative[t_, _]] := RPart[t];
NonRPart[TensorDerivative[t_, i_]] := TensorDerivative[NonRPart[t], i];

RPart[a_ b_] /; FreeQ[a, Tensor] := RPart[b];
NonRPart[a_ b_] /; FreeQ[a, Tensor] := NonRPart[b];

uvpt = {x[1, i_] :> -Boole[i == 4], x[2, _] :> 0, 
   x[3, i_] :> Boole[i == 4], 
   x[4, i_] :> 
    Which[i == 3, -(
      Sqrt[-u^2 - (-1 + v)^2 + 2 u (1 + v)]/(-1 + 2 u + 2 v)), 
     i == 4, (-u + v)/(-1 + 2 u + 2 v), True, 0]};
sct[x_, b_] := (x - b x . DiagonalMatrix[{-1, 1, 1, 1}] . x)/(1 - 
     2 b . DiagonalMatrix[{-1, 1, 1, 1}] . 
       x + (b . DiagonalMatrix[{-1, 1, 1, 1}] . b) (x . 
        DiagonalMatrix[{-1, 1, 1, 1}] . x));
sctuvpt[z_] = 
  Simplify@
   Flatten@Table[
     x[i, j] -> 
      sct[Table[x[i, kk] /. uvpt, {kk, 4}], {0, z, 0, 0}][[j]], {i, 
      4}, {j, 4}];
uvAssumptions = (1/2 < u < 2 && 1/2 < v < 2);

KinematicPrefactor[{\[CapitalDelta]1_, \[CapitalDelta]2_}, {{l1_, lb1_}, {l2_, lb2_}}] := 1/XXSquared[1, 2]^(\[CapitalDelta]1 + l1 + lb1);
KinematicPrefactor[deltas_, spins_] /; Length[deltas] == 3 := With[{kappas = deltas + Total /@ spins},
   XXSquared[1,2]^((kappas[[3]] - kappas[[1]] - kappas[[2]])/2) XXSquared[1,3]^((-kappas[[3]] - kappas[[1]] + kappas[[2]])/2) XXSquared[2,3]^((-kappas[[3]] + kappas[[1]] - kappas[[2]])/2)
];
KinematicPrefactor[deltas_, spins_] /; Length[deltas] == 4 := With[{kappas = deltas + Total /@ spins},
   (XXSquared[2, 4]/XXSquared[1, 4])^((kappas[[1]] - kappas[[2]])/2) (XXSquared[1, 4]/XXSquared[1, 3])^((kappas[[3]] - kappas[[4]])/2) XXSquared[1, 2]^(-(kappas[[1]] + kappas[[2]])/2) XXSquared[3, 4]^(-(kappas[[3]] + kappas[[4]])/2)
];


AddTensorHead[Correlator];
Correlator[x_?NumericQ] := x;
Correlator[a_ b_] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := a Correlator[b];
Correlator[a_ + b_] := Correlator[a] + Correlator[b];

readyToCorrelate[names_, already_] := 
  With[{poss = 
     Flatten[If[Position[names, #] =!= {}, 
         Position[names, #][[;; , 1]], {}] & /@ already]}, 
   poss === Range@Length[poss]];
findSwap[{1, ys___}, n_] := findSwap[{ys} - 1, n + 1];
findSwap[{x_, ys___}, n_] := {n + x, n + 1};

Correlator[t_Tensor | t_TensorPermute | t_Contract] /; !readyToCorrelate[Symbolic[t], {"C"}] := 
  With[{swap = findSwap[Position[Symbolic[t], "C"][[;; , 1]], 0]}, 
   Correlator[SwapFactors[t, swap[[1]], swap[[2]]]]];
Correlator[t_Tensor | t_TensorPermute | t_Contract] /; readyToCorrelate[Symbolic[t], {"C"}] && !readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]"}] := 
  With[{swap = 
     findSwap[Position[Symbolic[t], "C" | "\[Epsilon]"][[;; , 1]], 
      0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]]]];
Correlator[t_Tensor | t_TensorPermute | t_Contract] /; readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]"}] && ! readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]", "\[Delta]"}] :=
   With[{swap = 
     findSwap[
      Position[Symbolic[t], "C" | "\[Epsilon]" | "\[Delta]"][[;; , 
        1]], 0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]]]];
Correlator[
   Tensor[{{s : "C" | "\[Epsilon]" | "\[Delta]", rest__}, y___}]] := 
  TensorProduct[Tensor[{{s, rest}}], Correlator[Tensor[{y}]]];
Correlator[
    Tensor[{x___, {"\[PartialD]", Lowered[Spinor], 
       Lowered[DottedSpinor]}, {f_, idxs___}, y___}]] /; 
   f =!= "\[PartialD]" := 
  Correlator[
   Tensor[{x, {"\[PartialD]" <> f, Lowered[Spinor], 
      Lowered[DottedSpinor], idxs}, y}]];
Correlator[Contract[t_, pairs_, opt : OptionsPattern[]]] /; 
   readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]", "\[Delta]"}] := 
  Contract[Correlator[t], pairs];
Correlator[TensorPermute[t_, perm_, OptionsPattern[]]] /; 
   readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]", "\[Delta]"}] := 
  TensorPermute[Correlator[t], perm];

Format[Correlator[t_], TraditionalForm] := 
  Row[{"\[LeftAngleBracket]", t, "\[RightAngleBracket]"}];
Symbolic[Correlator[t_]] := {{"\[LeftAngleBracket]"}, 
   Sequence @@ Symbolic[t], {"\[RightAngleBracket]"}};
DisplayTemplate[Correlator[t_]] := 
  Row[{"\[LeftAngleBracket]", DisplayTemplate[t], 
    "\[RightAngleBracket]"}];
Indices[Correlator[t_]] := Indices[t];
Permutation[Correlator[t_]] := Permutation[t];

Format[SpacetimeStructure[dims_, ls_, derivs_, derivtype_, perm_, i_], 
   TraditionalForm] := 
\!\(\*SubsuperscriptBox[\("\<\[ScriptCapitalS]\>"\), \
\(Row[{derivtype, \(Count[derivs, #] &\) /@ 
      Range@Length[ls], "\<;\>", perm, "\<;\>", i}]\), \(Row[
    Riffle[Thread[{dims, ls}], "\<;\>"]]\)]\);

BuildTensor[{SpacetimeStructure[dims_, ls : {{l1_, lb1_}, {l2_, lb2_}}, {}, "\[PartialD]", perm : {1, 2}, 1], idxs___}] := 
With[{
	prodInds = Join[Flatten[Table[{{1, Spinor}, {2, DottedSpinor}}, 2 l1], 1], Flatten[Table[{{2, Spinor}, {1, DottedSpinor}}, 2 lb1], 1]],
	myInds = Flatten[Table[ls[[idx]] /. {a_, b_} :> {Table[{idx, Spinor}, 2 a], Table[{idx, DottedSpinor}, 2 b]}, {idx, 2}], 2]
  },
  With[{
	prod = KinematicPrefactor[dims, ls] I^(2 (l1 - lb1)) TensorProduct[Sequence @@ Table[StructureI[1, 2], 2 l1], Sequence @@ Table[StructureI[2, 1], 2 lb1]], 
	indperm = FindPermutation[prodInds, myInds], 
  	perms = Select[Permutations[Range[Length[{idxs}]]], myInds[[#]] === myInds &]
  },
  If[Length[{idxs}] == 0,
  	CanonicallyOrderedComponents[prod] /. x[ii_, j_] :> x[perm[[ii]], j],
  	With[{unsym = 
     TensorTranspose[
      TensorTranspose[
       SparseArray[ArrayRules[CanonicallyOrderedComponents[prod]] /. x[ii_, j_] :> x[perm[[ii]], j], Table[2, Length[{idxs}]]], 
      Ordering@prodInds[[;; , 2]]], 
     indperm]
    },
   1/Sqrt[Length[perms]] Sum[TensorTranspose[unsym, p], {p, perms}]
   ]
  ]
 ]
];

BuildTensor[{SpacetimeStructure[dims_, {{l1_, lb1_}, {l2_, lb2_}}, derivs_, "\[PartialD]", {1, 2}, 1], idxs___}] /; derivs =!= {} :=
  With[{bd = SpacetimeStructures[dims, {{l1, lb1}, {l2, lb2}}, {}, "\[PartialD]", {1, 2}][[1]]},
   TensorTranspose[
    CanonicallyOrderedComponents@
     TensorPermute[
      Fold[TensorSpinorDerivative, bd, ReverseSort[derivs]], 
      PermutationList[
       InversePermutation@
        PermutationPower[
         Cycles[{Join[
            Range[2 Count[derivs, 1] + 1, 2 Length[derivs], 2], 
            2 Length[derivs] + Range[2 l1]], 
           Join[Range[2 Count[derivs, 1] + 2, 2 Length[derivs], 2], 
            2 Length[derivs] + 2 l1 + Range[2 l2]]}], 
         Count[derivs, 2]], Length@Indices[bd] + 2 Length[derivs]]], 
    Ordering@{idxs}]
   ];
   
ExpandCorrelator[Correlator[Tensor[names_]]] /; (AllTrue[names, MemberQ[Flatten[$multiplet /@ $multipletIndices][[;;, 1]], StringDrop[#[[1]], Count[Characters[#[[1]]], "\[PartialD]"]]] &] && Length[names] == 2) :=
With[{name2field = Association@Table[f[[1]] -> f, {f, Flatten[$multiplet /@ $multipletIndices]}]}, 
  With[{fields = 
     name2field[
        StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names, 
    derivs = 
     Flatten@Table[
       i, {i, 2}, {j, 
        Count[Characters[names[[i, 1]]], "\[PartialD]"]}]},
   If[ToExpression[Conjugate[fields[[1]]][[1]], InputForm, Hold] =!= 
     ToExpression[fields[[2, 1]], InputForm, Hold], 0,
    TensorProduct[
     TwoPtRInvariant[RRep[fields[[1]]], RRep[fields[[2]]]], 
     SpacetimeStructures[ScalingDimension /@ fields, Spin /@ fields, derivs, "\[PartialD]", {1, 2}][[1]]]
    ]
   ]
];
ExpandCorrelator[0, OptionsPattern[]] = 0;

CorrelatedFields[Correlator[t_]] := Range@Length[Symbolic[t]];
CorrelatedFields[t_Tensor] := {};
CorrelatedFields[TensorPermute[t_, _]] := CorrelatedFields[t];
CorrelatedFields[Contract[t_, _]] := CorrelatedFields[t];
CorrelatedFields[TensorProduct[t1_, t2_]] := 
  Join[CorrelatedFields[t1], 
   Length[Symbolic[t1]] + CorrelatedFields[t2]];
CorrelatedFields[TensorDerivative[t_, _]] := 1 + CorrelatedFields[t];
CorrelatedFields[a_ b_] /; FreeQ[a, Tensor] := CorrelatedFields[b];

ExpandCorrelator[expr : Except[_Correlator], opt : OptionsPattern[]] /;
    Length[Cases[expr, _Correlator, All]] == 1 := 
  With[{swap = 
     ExpandCorrelator[First@Cases[expr, _Correlator, All], opt]},
   SwapIn[
    expr /. Correlator -> Identity, {Min[#], Max[#]} &@
     CorrelatedFields[expr], swap]
   ];
ExpandCorrelator[a_ + b_, opt : OptionsPattern[]] := 
  ExpandCorrelator[a, opt] + ExpandCorrelator[b, opt];
ExpandCorrelator[a_ b_, opt : OptionsPattern[]] /; FreeQ[a, Tensor] :=
   a ExpandCorrelator[b, opt];

SymbolicSpacetimeRelations[largebasis_] := If[# === {}, {}, 
   FullSimplify[
    RowReduce[#, 
     ZeroTest -> (Function[expr, Simplify[expr, uvAssumptions] === 0])], 
    uvAssumptions]] &@
 FullSimplify[
  NullSpace[
   Transpose[
     ArrayFlatten[
      Table[Flatten[
        CanonicallyOrderedComponents[largebasis[[i]]]], {i, 
        Length[largebasis]}]]] /. sctuvpt[2]], uvAssumptions];
        
safeUVs = {{1, 36/25}, {1, 100/169}, {1, 256/289}, {1, 1600/841}, {1, 
    3136/2809}, {1, 4356/4225}, {1, 5184/7225}, {1, 6084/7921}, {1, 
    9216/5329}, {1, 14400/11881}, {9/16, 25/16}, {9/16, 1369/
    2704}, {16/9, 1369/1521}, {16/9, 8281/12321}, {25/16, 9/16}, {25/
    36, 25/36}, {36/25, 1}, {64/81, 7225/13689}, {81/64, 7225/
    10816}, {100/169, 1}, {169/100, 169/100}, {169/196, 225/
    196}, {169/225, 196/225}, {196/169, 225/169}, {196/225, 169/
    225}, {196/361, 10201/9025}, {225/169, 196/169}, {225/196, 169/
    196}, {256/289, 1}, {289/256, 289/256}, {289/441, 5776/
    11025}, {361/625, 4356/7225}, {400/441, 841/441}, {441/289, 5776/
    7225}, {625/361, 4624/9025}, {625/841, 1296/841}, {625/1156, 1521/
    1156}, {841/441, 400/441}, {841/1225, 2304/1225}, {841/1600, 841/
    1600}, {1296/841, 625/841}, {1369/1521, 16/9}, {1369/2704, 9/
    16}, {1521/1156, 625/1156}, {1521/1369, 2704/1369}, {1521/1681, 
    2500/1681}, {1521/2500, 1681/2500}, {1600/841, 1}, {1681/1521, 
    2500/1521}, {1681/2500, 1521/2500}, {1681/2601, 3364/2601}, {2304/
    1225, 841/1225}, {2304/3025, 5329/3025}, {2500/1521, 1681/
    1521}, {2500/1681, 1521/1681}, {2500/4761, 5329/4761}, {2601/2704,
     2809/2704}, {2601/2809, 2704/2809}, {2704/1369, 1521/
    1369}, {2704/2601, 2809/2601}, {2704/2809, 2601/2809}, {2704/5329,
     5625/5329}, {2809/2601, 2704/2601}, {2809/2704, 2601/
    2704}, {2809/3136, 2809/3136}, {3136/2809, 1}, {3136/3721, 5625/
    3721}, {3136/5625, 3721/5625}, {3249/4225, 4624/4225}, {3249/4624,
     4225/4624}, {3364/2601, 1681/2601}, {3600/5329, 8281/
    5329}, {3721/3136, 5625/3136}, {3721/5476, 7569/5476}, {3721/5625,
     3136/5625}, {3721/6400, 11881/6400}, {4225/3249, 4624/
    3249}, {4225/4356, 4225/4356}, {4225/4624, 3249/4624}, {4225/5184,
     9409/5184}, {4225/5776, 7569/5776}, {4225/7569, 5776/
    7569}, {4225/7569, 7744/7569}, {4225/7744, 7569/7744}, {4356/4225,
     1}, {4356/7225, 361/625}, {4489/7225, 13456/7225}, {4624/3249, 
    4225/3249}, {4624/4225, 3249/4225}, {4624/5625, 5929/5625}, {4624/
    5929, 5625/5929}, {4624/7569, 9025/7569}, {4624/9025, 625/
    361}, {4624/9025, 7569/9025}, {5184/7225, 1}, {5329/3025, 2304/
    3025}, {5329/4761, 2500/4761}, {5329/9216, 5329/9216}, {5625/3136,
     3721/3136}, {5625/3721, 3136/3721}, {5625/4624, 5929/
    4624}, {5625/5329, 2704/5329}, {5625/5929, 4624/5929}, {5625/7396,
     9409/7396}, {5625/9409, 7396/9409}, {5776/4225, 7569/
    4225}, {5776/7225, 441/289}, {5776/7569, 4225/7569}, {5776/11025, 
    289/441}, {5929/4624, 5625/4624}, {5929/5625, 4624/5625}, {6084/
    7921, 1}, {6084/9025, 9409/9025}, {6084/9409, 9025/9409}, {6084/
    10201, 13225/10201}, {7225/5184, 7225/5184}, {7225/5776, 11025/
    5776}, {7225/8649, 13456/8649}, {7225/10816, 81/64}, {7225/13456, 
    8649/13456}, {7225/13689, 64/81}, {7396/5625, 9409/5625}, {7396/
    9409, 5625/9409}, {7569/4225, 5776/4225}, {7569/4225, 7744/
    4225}, {7569/4624, 9025/4624}, {7569/5476, 3721/5476}, {7569/5776,
     4225/5776}, {7569/7744, 4225/7744}, {7569/9025, 4624/
    9025}, {7744/4225, 7569/4225}, {7744/7569, 4225/7569}, {7921/6084,
     7921/6084}, {7921/9801, 10000/9801}, {7921/10000, 9801/
    10000}, {8100/9409, 14161/9409}, {8100/14161, 9409/14161}, {8281/
    5329, 3600/5329}, {8281/12321, 16/9}, {8281/13225, 13456/
    13225}, {8281/13456, 13225/13456}, {8464/12321, 14161/
    12321}, {8464/14161, 12321/14161}, {8649/7225, 13456/7225}, {8649/
    13456, 7225/13456}, {9025/4624, 7569/4624}, {9025/6084, 9409/
    6084}, {9025/7569, 4624/7569}, {9025/9409, 6084/9409}, {9216/5329,
     1}, {9409/5184, 4225/5184}, {9409/5625, 7396/5625}, {9409/6084, 
    9025/6084}, {9409/7396, 5625/7396}, {9409/8100, 14161/
    8100}, {9409/9025, 6084/9025}, {9409/14161, 8100/14161}, {9801/
    7921, 10000/7921}, {9801/10000, 7921/10000}, {10000/7921, 9801/
    7921}, {10000/9801, 7921/9801}, {10201/9025, 196/361}, {10816/
    7225, 13689/7225}, {11025/5776, 7225/5776}, {11236/12769, 14161/
    12769}, {11236/14161, 12769/14161}, {11881/6400, 3721/
    6400}, {11881/14400, 11881/14400}, {12321/8464, 14161/
    8464}, {12321/14161, 8464/14161}, {12769/11236, 14161/
    11236}, {12769/14161, 11236/14161}, {13225/8281, 13456/
    8281}, {13225/10201, 6084/10201}, {13225/13456, 8281/
    13456}, {13456/7225, 4489/7225}, {13456/7225, 8649/7225}, {13456/
    8281, 13225/8281}, {13456/8649, 7225/8649}, {13456/13225, 8281/
    13225}, {13689/7225, 10816/7225}, {14161/8100, 9409/8100}, {14161/
    8464, 12321/8464}, {14161/9409, 8100/9409}, {14161/11236, 12769/
    11236}, {14161/12321, 8464/12321}, {14161/12769, 11236/
    12769}, {14400/11881, 1}};
    
spacetimePtData[structs_, len_] := 
 spacetimePtData[structs, len] = 
  With[{structComps = 
     Simplify[
      ArrayFlatten[
        Flatten@*CanonicallyOrderedComponents /@ structs] /. 
       sctuvpt[2], uvAssumptions]},
   With[{idxs = Sort[Length[structs] + 1 - IndependentSet[Reverse@structComps, "Rules" -> Thread[{u, v} -> safeUVs[[-2]]], "Indices" -> True]]}, {idxs, 
     ResourceFunction["MonitorProgress"][
      Table[Simplify@
        Quiet@Check[{safeUVs[[ii]], 
           Table[LinearSolve[
             Transpose[structComps[[idxs]]] /. {u -> safeUVs[[ii, 1]],
                v -> safeUVs[[ii, 2]]}, 
             structComps[[reli]] /. {u -> safeUVs[[ii, 1]], 
               v -> safeUVs[[ii, 2]]}], {reli, 
             Complement[Range@Length[structs], idxs]}]}, 
          Nothing], {ii, len}], 
      "Label" -> "Generating spacetime structure relation data",
		"CurrentDisplayFunction" -> None]}
    ]
   ];
   
Options[fitRational] = {"Prefactors" -> {1}};
fitRational[data_, step_, maxDeg_, opt : OptionsPattern[]] := 
  fitRational[data, 0, step, maxDeg, opt];
fitRational[data_, deg_, step_, maxDeg_, opt : OptionsPattern[]] := 
  If[AllTrue[data[[;; , 3]], # === 0 &], 0, 
   With[{ansatzes = 
      OptionValue[
        "Prefactors"] (Sum[\[Beta]up[k, l] u^(k step) v^(l step), {k, 
           0, deg/step}, {l, 0, deg/step - k}]/
         Sum[\[Beta]down[k, l] u^(k step) v^(l step), {k, 0, 
           deg/step}, {l, 0, deg/step - k}]), 
     vars = Flatten[
       Table[{\[Beta]up[k, l], \[Beta]down[k, l]}, {k, 0, 
         deg/step}, {l, 0, deg/step - k}]], 
     rat = FirstCase[data[[;; , 3]], 
       x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
          Cases[x, Power[y_, 1/2 | -1/2] :> Sqrt[y], All])]},
    Module[{mats = 
       Table[(Denominator[ansatz] Simplify[pt[[3]]/rat] - 
            Numerator[ansatz] /. {u -> pt[[1]], v -> pt[[2]]}) /. 
         Thread[vars -> IdentityMatrix[Length[vars]]], {ansatz, 
         ansatzes}, {pt, data}], found = False, ans = 0},
     Do[
      If[MatrixRank[N@mats[[i]]] != Min[Dimensions[mats[[i]]]],
       With[{null = NullSpace[mats[[i]]]},
        If[null =!= {},
         found = True;
         ans = rat ansatzes[[i]] /. Thread[vars -> null[[1]]];
         Break[];
         ]
        ]
       ],
      {i, Length[mats]}
      ];
     If[found, ans,
      If[deg < maxDeg, 
       fitRational[data, deg + step, step, maxDeg, opt], Print["!"]; 
       Null]
      ]
     ]
    ]
   ];
   
relations[structs_, len_, deg_] := 
 With[{data = spacetimePtData[structs, len]},
  With[{ind = data[[1]], 
    other = Complement[Range@Length[structs], data[[1]]]},
   ResourceFunction["MonitorProgress"][
    Table[SparseArray[
      Append[Table[{ind[[i]]} -> (-Simplify@
            fitRational[
             data[[2]] /. {{u_, v_}, b_} :> {u, v, b[[j, i]]}, 1, deg,
              "Prefactors" -> {1, Sqrt[u], Sqrt[v], Sqrt[u v]}]), {i, 
         Length[ind]}], {other[[j]]} -> 1], {Length[structs]}], {j, 
      Length[other]}], "Label" -> "Fitting rational functions",
		"CurrentDisplayFunction" -> None]
   ]
  ]
SpacetimeRelations[structs_] := 
  If[Length[structs] > 5 && 
    First@Cases[structs, s_SpacetimeStructure :> Length[s[[1]]], All] == 4, 
   relations[structs, 188, 6], SymbolicSpacetimeRelations[structs]];
   
Options[RRelations] = {"MonitorProgress" -> False};
RRelations[largebasis_, OptionsPattern[]] := Module[{relations, choice, tmp},
	If[OptionValue["MonitorProgress"], tmp = PrintTemporary["Calculating R-symmetry structure relations..."]];
	choice = 
	  With[{size = Length@Flatten@CanonicallyOrderedComponents[largebasis[[1]]]}, 
	   RandomSample[Range@size, Min[5000, size]]];
	relations = 
	  If[# === {}, {}, RowReduce@#] &@
	   NullSpace[
	    Transpose[
	     ArrayFlatten[
	       If[OptionValue["MonitorProgress"],
	       Monitor[Table[
	         Flatten[CanonicallyOrderedComponents[largebasis[[ri]]]][[
	          choice]], {ri, Length[largebasis]}], 
	        StringForm["``/``", ri, Length[largebasis]]],
	        Table[
	         Flatten[CanonicallyOrderedComponents[largebasis[[ri]]]][[
	          choice]], {ri, Length[largebasis]}]]]]];
	If[OptionValue["MonitorProgress"], NotebookDelete[tmp]];
	relations
];

expansion[structure_, basis_, relations_] := 
  With[{idx = Position[basis, structure, 1][[1, 1]], 
    pivots = 
     Association@
      KeyValueMap[Rule @@ Reverse[{#1, #2}] &, 
       Sort[#][[1, 1, 2]] & /@ 
        KeySelect[GroupBy[ArrayRules[relations], #[[1, 1]] &], 
         IntegerQ]]},
   If[KeyExistsQ[pivots, 
     idx], -relations[[pivots[idx], 
      Complement[Range@Length[basis], Keys@pivots]]], 
    Table[Boole[i == idx], {i, Length[basis]}][[
     Complement[Range@Length[basis], Keys@pivots]]]]
   ];

Options[ExpansionComponents] = {"MonitorProgress" -> False};
ExpansionComponents[a_ b_, rest___, opt: OptionsPattern[]] /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] := Explicit[a] ExpansionComponents[b, rest, opt];
ExpansionComponents[a_, rest___, OptionsPattern[]] /; FreeQ[a, Tensor | TensorDerivative] := Explicit[a];
   
ExpansionComponents[t : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), rExpansion_, spExpansion_, OptionsPattern[]] := SparseArray@TensorProduct[
 expansion[RPart[t], ##] & @@ rExpansion, 
 expansion[NonRPart[t], ##] & @@ spExpansion
];

ExpansionComponents[Plus[a_, rest__], opt: OptionsPattern[]] := With[{allterms = List @@ Expand[Plus[a, rest]]},
   With[{largeRBasis = DeleteDuplicates[RPart /@ allterms], largeSTBasis = SortBy[DeleteDuplicates[NonRPart /@ allterms], First@Cases[#, s_SpacetimeStructure :> {Length[s[[3]]], s[[5]], -s[[6]]}, All] &]},
      With[{RRels = RRelations[largeRBasis, opt], STRels = SpacetimeRelations[largeSTBasis]},
      	Sum[ExpansionComponents[t /. u[perm_] :> (u^#1 v^#2 & @@ uvpowers[1, perm]) /. v[perm_] :> (u^#1 v^#2 & @@ uvpowers[2, perm]), {largeRBasis, RRels}, {largeSTBasis, STRels}], {t, List @@ Expand[Plus[a, rest]]}]
      ]
   ]
];
ExpansionComponents[t : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), OptionsPattern[]] := SparseArray[{{1,1} -> 1},{1,1}];

$susyRules = {};

Options[DeclareAlgebra] = {"MonitorProgress" -> True, "MaxDepth" -> 0};
DeclareAlgebra[OptionsPattern[]] := Module[{},
	DeclareAnnihilator["Q"]; DeclareAnnihilator["\!\(\*OverscriptBox[\(Q\), \(~\)]\)"];
	
	If[OptionValue["MonitorProgress"],
	    Do[
			ResourceFunction["MonitorProgress"][
				Do[
					If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> False];
					If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QBarTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> True];,
					{op, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, 2 OptionValue["MaxDepth"] - 1}, {k, 0, 2 OptionValue["MaxDepth"] - 1 - j}]]]}
				],
				"Label" -> "Determining SUSY ansatzes ("<>$multipletName[i]<>")",
				"CurrentDisplayFunction" -> None
			],
		{i, $multipletIndices}],
		Do[
			If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> False];
			If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QBarTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> True];,
			{i, $multipletIndices},
			{op, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, 2 OptionValue["MaxDepth"] - 1}, {k, 0, 2 OptionValue["MaxDepth"] - 1 - j}]]]}
		]
	];
	
	Commutator[$QTensor, Tensor[{{"C", ___}}]] = 0;
	Commutator[$QBarTensor, Tensor[{{"C", ___}}]] = 0;
	Commutator[$QTensor, Tensor[{{"\[Epsilon]", ___}}]] = 0;
	Commutator[$QBarTensor, Tensor[{{"\[Epsilon]", ___}}]] = 0;
	Commutator[$QTensor, Tensor[{{"\[PartialD]", ___}}]] = 0;
	Commutator[$QBarTensor, Tensor[{{"\[PartialD]", ___}}]] = 0;
];

quadraticZero[op_] := quadraticZero[op] = NormalOrder[TensorProduct[$QTensor, $QBarTensor, Tensor[{op}]] + TensorProduct[$QBarTensor, $QTensor, Tensor[{op}]], "Vacuum" -> True] - 
 2 I TensorProduct[Kronecker[RIndex[fundRep[$RSymmetry]]], Tensor[{{"\[PartialD]", Lowered[Spinor], Lowered[DottedSpinor]}}], Tensor[{op}]];

solveGroups[grps_, vars_, rules_, assum_] := 
 ResourceFunction["MonitorProgress"][Fold[
   With[{sol = Quiet[Assuming[assum, Simplify@First@Solve[#2 /. #1, vars]]]},
     Sort@Select[Join[Simplify[#1 /. sol, assum], sol], ! SameQ @@ # &]
     ] &,
   {}, grps
   ], "Label" -> "Solving equations", 
  "CurrentDisplayFunction" -> None
  ]

Options[opGroup] = {"Conjugate" -> False};
opGroup[m_, i_, j_, OptionsPattern[]] := With[{bottom = First@MinimalBy[If[$multipletSC[m], Multiplet[m], Multiplet[m][[If[OptionValue["Conjugate"], 2, 1]]]], ScalingDimension]},
   Select[If[$multipletSC[m], Multiplet[m], Multiplet[m][[If[OptionValue["Conjugate"], 2, 1]]]], ScalingDimension[#] - ScalingDimension[bottom] == (i + j)/2 && Last[#] - Last[bottom] == i - j & ]
];

groupOf[f_Field] := Module[{mult, conj, bottom, dimdiff, ydiff},
   mult = whichMultiplet[f];
   conj = (! $multipletSC[mult] && MemberQ[Multiplet[mult][[2]], f]);
   bottom = First@opGroup[mult, 0, 0, "Conjugate" -> conj];
   dimdiff = ScalingDimension[f] - ScalingDimension[bottom];
   ydiff = Last[f] - Last[bottom];
   {dimdiff + ydiff/2, dimdiff - ydiff/2, conj}
];
   
Options[linearEquations] = {"MaxDepth" -> 0};
linearEquations[i_, OptionsPattern[]] := DeleteCases[Reduce /@ Thread[Flatten @ ResourceFunction["MonitorProgress"][
		Table[
		   ExpansionComponents@ExpandCorrelator@Correlator@NormalOrder[TensorProduct[QTensor["QBar" -> qbar], Tensor[{op1, op2}]], "Vacuum" -> True],
		   {op1, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"] - 1}, {k, 0, OptionValue["MaxDepth"] - 1 - j}]]]}, 
		   {qbar, {False, True}}, 
		   {op2, With[{grp = groupOf[op1]}, opGroup[i, Sequence @@ Reverse[grp[[;;2]] + If[qbar, {0, 1}, {1, 0}]], "Conjugate" -> ! grp[[3]]]]}
		],
		"Label" -> "Generating linear equations",
		"CurrentDisplayFunction" -> None
	] == 0], True];
	
Options[quadraticEquations] = {"MaxDepth" -> 0};
quadraticEquations[i_, OptionsPattern[]] := DeleteDuplicates@DeleteCases[Simplify[Reduce[#], _SUSYCoefficient != 0] & /@ Thread[Flatten @ ResourceFunction["MonitorProgress"][
		Table[
		   ExpansionComponents@ExpandCorrelator@Correlator[TensorProduct[quadraticZero[op1], Tensor[{op2}]]],
		   {op1, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"] - 1}, {k, 0, OptionValue["MaxDepth"] - 1 - j}]]]}, 
		   {shift, {{-1,-1},{0,0},{1,1}}}, 
		   {op2, With[{grp = groupOf[op1]}, opGroup[i, Sequence @@ Reverse[grp[[;;2]] + shift], "Conjugate" -> ! grp[[3]]]]}
		],
		"Label" -> "Generating quadratic equations",
		"CurrentDisplayFunction" -> None
	] == 0], True];

Options[SUSYRules] = {"EquationGroupSize" -> 10, "MaxDepth" -> 0};
SUSYRules[i_, opt: OptionsPattern[]] := SUSYRules[i, opt] = Module[{linears, linsol, quadratics, norms, normsol, vars, rvars, quadgroups, quadsol, partialsol},
	DeclareAlgebra["MaxDepth" -> OptionValue["MaxDepth"]];
	
	linears = linearEquations[i, "MaxDepth" -> OptionValue["MaxDepth"]];
	linsol = First[Solve[linears]];
	
	quadratics = quadraticEquations[i, "MaxDepth" -> OptionValue["MaxDepth"]];
	
	quadgroups = Partition[DeleteCases[DeleteDuplicates[Simplify[#, SUSYCoefficient[__] != 0] & /@ (quadratics /. linsol)], True], UpTo[OptionValue["EquationGroupSize"]]];
	
	vars = DeleteDuplicates@Cases[quadgroups, _SUSYCoefficient, All];
	
	quadsol = solveGroups[quadgroups, vars, {}, Thread[vars != 0]];
	
	partialsol = Join[linsol /. quadsol, quadsol];
	
	rvars = DeleteDuplicates[Cases[partialsol[[;;, 2]], _SUSYCoefficient, All]];	
	norms = DeleteCases[Simplify[Cases[partialsol[[;; , 1]], 
	   s : SUSYCoefficient[a_, b_, "QBar" -> qb_] :> (Abs[s] == Abs[SUSYCoefficient[Conjugate[name2field[a]][[1]], b, "QBar" -> !qb]]), 
	   All] /. partialsol /. Thread[rvars -> Array[\[Alpha], Length[rvars]]], \[Alpha][_] > 0], 
	x_ /; ! FreeQ[x, SUSYCoefficient]];
	
	normsol = Last[Solve[norms]];
	
	Thread[Join[partialsol[[;;, 1]], rvars] -> (Join[partialsol[[;;, 2]], rvars] /. Thread[rvars -> Array[\[Alpha], Length[rvars]]] /. normsol /. Thread[Array[\[Alpha], Length[rvars]] -> rvars])]
];

Options[susyTable] = {"Solved" -> True, "MaxDepth" -> 0};
susyTable[ops_, OptionsPattern[]] := With[{g = Grid[Table[{TensorProduct[$QTensor, ToTensor[op]], 
    QAnsatz[op, "QBar" -> False] /. If[TrueQ[OptionValue["Solved"]], If[OptionValue["MaxDepth"] == 0, SUSYRules[whichMultiplet[ops[[1]]]], SUSYRules[whichMultiplet[ops[[1]]], "MaxDepth" -> OptionValue["MaxDepth"]]], {}], 
    TensorProduct[$QBarTensor, ToTensor[Conjugate[op]]], 
    QAnsatz[Conjugate@op, "QBar" -> True] /.  If[TrueQ[OptionValue["Solved"]], If[OptionValue["MaxDepth"] == 0, SUSYRules[whichMultiplet[ops[[1]]]], SUSYRules[whichMultiplet[ops[[1]]], "MaxDepth" -> OptionValue["MaxDepth"]]], {}]}, {op, ops}], 
  Dividers -> All, Alignment -> Left]},
  	Column[{g // TraditionalForm,
 		Button[Style["Copy as TeX", 14], CopyToClipboard[ToString[TeXForm[g]]], ImageSize -> 200]
  	}, Alignment -> Center 
	]
  ];

Options[DisplaySUSYVariations] = {"Solved" -> True, "MaxDepth" -> 0};
DisplaySUSYVariations[opt:OptionsPattern[]] := TabView[
   Table[$multipletName[i] -> susyTable[If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"]}, {k, 0, OptionValue["MaxDepth"] - j}]]],opt], {i, $multipletIndices}], 
   Alignment -> Center, ImageSize -> Automatic];
  
\[Epsilon]Spacetime = 
  Tensor[{{"\[Epsilon]", Lowered[SpaceTime], Lowered[SpaceTime], 
     Lowered[SpaceTime], Lowered[SpaceTime]}}];
\[Epsilon]SpacetimeUpper = 
  Tensor[{{"\[Epsilon]", Raised[SpaceTime], Raised[SpaceTime], 
     Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\[Epsilon]", Lowered[SpaceTime], Lowered[SpaceTime], 
    Lowered[SpaceTime], Lowered[SpaceTime]}] := -SignatureFactor[]^2 LeviCivitaTensor[4];
BuildTensor[{"\[Epsilon]", Raised[SpaceTime], Raised[SpaceTime], 
    Raised[SpaceTime], Raised[SpaceTime]}] := LeviCivitaTensor[4];

SpinorX[{i_, j_}, {k_, l_}, {m_, n_}] := 
  Contract[
   TensorProduct[\[Epsilon]Spacetime, XX[i, j], XX[k, l], 
    XX[m, n], \[Sigma]Upper], {{1, 5}, {2, 6}, {3, 7}, {4, 8}}];

MyInactive /: MakeBoxes[MyInactive[StructureI][i_, j_], TraditionalForm] :=
   SuperscriptBox[OverscriptBox["I", "^"], 
   RowBox@{ToString[i], ",", ToString[j]}];
MyInactive /: 
  MakeBoxes[MyInactive[StructureI][i_, j_, k_, l_], TraditionalForm] := 
  SubsuperscriptBox[OverscriptBox["I", "^"], 
   RowBox@{ToString[k], ",", ToString[l]}, 
   RowBox@{ToString[i], ",", ToString[j]}];
MyInactive /: 
  MakeBoxes[MyInactive[StructureJ][k_, i_, j_], TraditionalForm] := 
  SubsuperscriptBox[OverscriptBox["J", "^"], 
   RowBox@{ToString[i], ",", ToString[j]}, RowBox@{ToString[k]}];
MyInactive /: 
  MakeBoxes[MyInactive[StructureK][i_, j_, k_], TraditionalForm] := 
  SubsuperscriptBox[OverscriptBox["K", "^"], 
   RowBox@{ToString[k]}, RowBox@{ToString[i], ",", ToString[j]}];
MyInactive /: 
  MakeBoxes[MyInactive[StructureKBar][i_, j_, k_], TraditionalForm] := 
  SubsuperscriptBox[
   OverscriptBox[OverscriptBox["K", "_"], "^"], 
   RowBox@{ToString[k]}, RowBox@{ToString[i], ",", ToString[j]}];
MyInactive /: 
  MakeBoxes[MyInactive[StructureL][i_, j_, k_, l_], TraditionalForm] := 
  SubsuperscriptBox[OverscriptBox["L", "^"], 
   RowBox@{ToString[j], ",", ToString[k], ",", ToString[l]}, 
   RowBox@{ToString[i]}];
MyInactive /: 
  MakeBoxes[MyInactive[StructureLBar][i_, j_, k_, l_], 
   TraditionalForm] := 
  SubsuperscriptBox[
    OverscriptBox[OverscriptBox["L", "_"], "^"], 
   RowBox@{ToString[j], ",", ToString[k], ",", ToString[l]}, 
   RowBox@{ToString[i]}];
       
StructureI[i_, j_] := SpinorX[i, j];
StructureI[i_, j_, k_, l_] /; i === j := StructureJ[i, k, l];
StructureI[i_, j_, k_, l_] /; i =!= j := 1/(2 XXSquared[k, 
      l]) ((XXSquared[i, k] SpinorX[j, l] - 
        XXSquared[i, l] SpinorX[j, k]) + (XXSquared[j, k] SpinorX[i, 
          l] - XXSquared[j, l] SpinorX[i, k]) - 
      XXSquared[i, j] SpinorX[k, l] - XXSquared[k, l] SpinorX[i, j] - 
      (2 I / SignatureFactor[]) SpinorX[{i, k}, {l, j}, {l, k}]);

StructureJ[k_, i_, j_] := (XXSquared[i, k] XXSquared[j, k])/
   XXSquared[i, 
    j] (SpinorX[i, k]/XXSquared[i, k] - SpinorX[j, k]/XXSquared[j, k]);
StructureK[i_, j_, k_] := 
  1/2 Sqrt[XXSquared[i, j]]/
   Sqrt[XXSquared[i, k] XXSquared[j, 
      k]] ((XXSquared[i, k] + XXSquared[j, k] - 
        XXSquared[i, j]) \[Epsilon]Lower - 
     4 Contract[
       TensorProduct[XX[i, k], 
        XX[j, k], \[Sigma]CommLower], {{1, 3}, {2, 4}}]);
StructureKBar[i_, j_, k_] := 
  1/2 Sqrt[XXSquared[i, j]]/
   Sqrt[XXSquared[i, k] XXSquared[j, 
      k]] ((XXSquared[i, k] + XXSquared[j, k] - 
        XXSquared[i, j]) \[Epsilon]LowerDot - 
     4 Contract[
       TensorProduct[XX[i, k], 
        XX[j, k], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}]);
StructureL[i_, j_, k_, l_] := 
  2/Sqrt[XXSquared[j, k] XXSquared[k, l] XXSquared[l, 
      j]] (XXSquared[i, j] Contract[
       TensorProduct[XX[k, l], 
        XX[i, l], \[Sigma]CommLower], {{1, 3}, {2, 4}}] + 
     XXSquared[i, k] Contract[
       TensorProduct[XX[l, j], 
        XX[i, j], \[Sigma]CommLower], {{1, 3}, {2, 4}}] + 
     XXSquared[i, l] Contract[
       TensorProduct[XX[j, k], 
        XX[i, k], \[Sigma]CommLower], {{1, 3}, {2, 4}}]);
StructureLBar[i_, j_, k_, l_] := 
  2/Sqrt[XXSquared[j, k] XXSquared[k, l] XXSquared[l, 
      j]] (XXSquared[i, j] Contract[
       TensorProduct[XX[k, l], 
        XX[i, l], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}] + 
     XXSquared[i, k] Contract[
       TensorProduct[XX[l, j], 
        XX[i, j], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}] + 
     XXSquared[i, l] Contract[
       TensorProduct[XX[j, k], 
        XX[i, k], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}]);
        
numSTStructures[ls_] := 
 Which[Length[ls] == 2, 1, Length[ls] == 3, 
  With[{js = 
     Tuples[Range[Abs[#[[1]] - #[[2]]], #[[1]] + #[[2]]] & /@ ls]},
   Count[
    js, {x_?NumericQ, y_, z_} /; 
     Total[Sort[{x, y, z}][[;; 2]]] >= Max[{x, y, z}] && 
      IntegerQ[x + y + z]]
   ], Length[ls] == 4, 
  Length@Select[Tuples[Range[-#, #] & /@ Flatten[ls]], 
    Total[#[[{1, 3, 5, 7}]]] == Total[#[[{2, 4, 6, 8}]]] &]]

validStruct[MyInactive[StructureI][i_, j_]] := i != j;
validStruct[
   MyInactive[StructureI][i_, j_, k_, l_]] := (k > l && 
    Length[DeleteDuplicates[{i, j, k, l}]] == 4);
validStruct[
   MyInactive[StructureJ][i_, j_, k_]] := (j < k && 
    Length[DeleteDuplicates[{i, j, k}]] == 3);
validStruct[
   MyInactive[StructureK][i_, j_, k_]] := (i < j && 
    Length[DeleteDuplicates[{i, j, k}]] == 3);
validStruct[
   MyInactive[StructureKBar][i_, j_, k_]] := (i < j && 
    Length[DeleteDuplicates[{i, j, k}]] == 3);
validStruct[
   MyInactive[StructureL][i_, j_, k_, l_]] := (j < k < l && 
    Length[DeleteDuplicates[{i, j, k, l}]] == 4);
validStruct[
   MyInactive[StructureLBar][i_, j_, k_, l_]] := (j < k < l && 
    Length[DeleteDuplicates[{i, j, k, l}]] == 4);

charges[MyInactive[StructureI][i_, j_], n_] := 
  SparseArray[{{i, 2} -> 1/2, {j, 1} -> 1/2}, {n, 2}];
charges[MyInactive[StructureI][i_, j_, k_, l_], n_] := 
 SparseArray[{{i, 2} -> 1/2, {j, 1} -> 1/2}, {n, 2}]; 
charges[MyInactive[StructureJ][i_, j_, k_], n_] := 
 SparseArray[{{i, 2} -> 1/2, {i, 1} -> 1/2}, {n, 2}]; 
charges[MyInactive[StructureK][i_, j_, k_], n_] := 
 SparseArray[{{i, 1} -> 1/2, {j, 1} -> 1/2}, {n, 2}]; 
charges[MyInactive[StructureKBar][i_, j_, k_], n_] := 
 SparseArray[{{i, 2} -> 1/2, {j, 2} -> 1/2}, {n, 2}]; 
charges[MyInactive[StructureL][i_, j_, k_, l_], n_] := 
 SparseArray[{{i, 1} -> 1}, {n, 2}]; 
charges[MyInactive[StructureLBar][i_, j_, k_, l_], n_] := 
 SparseArray[{{i, 2} -> 1}, {n, 2}];

structTypes[n_] := 
  Which[n == 2, {MyInactive[StructureI][1, 2], 
    MyInactive[StructureI][2, 1]}, n == 3, 
   Select[Flatten@
     Join[MyInactive[StructureI] @@@ Tuples[Range[3], 2], 
      MyInactive[StructureJ] @@@ Permutations[Range[3]], 
      MyInactive[StructureK] @@@ Permutations[Range[3]], 
      MyInactive[StructureKBar] @@@ Permutations[Range[3]]], 
    validStruct], n == 4, 
   Select[Flatten@
     Join[
      MyInactive[StructureI] @@@ Tuples[Range[4], 2], 
      MyInactive[StructureI] @@@ Permutations[Range[4]], 
      MyInactive[StructureJ] @@@ Tuples[Range[4], 3], 
      MyInactive[StructureK] @@@ Tuples[Range[4], 3], 
      MyInactive[StructureKBar] @@@ Tuples[Range[4], 3], 
      MyInactive[StructureL] @@@ Permutations[Range[4]], 
      MyInactive[StructureLBar] @@@ Permutations[Range[4]]], 
    validStruct]];

structBasis[n_] := Normal[charges[#, n]] -> # & /@ structTypes[n];

maxMultiplier[basisElem_, target_] := 
  Floor@Min[Part[target, Sequence @@ #[[1]]]/#[[2]] & /@ 
    Select[ArrayRules[basisElem], TrueQ[#[[2]] > 0] &]];
sumsToProducts[{}, target_] := If[Total@Flatten[target] === 0, {1}, {}];
sumsToProducts[basis_, target_] := 
  Flatten@Table[
    Thread[TensorProduct[TensorProduct[Sequence @@ Table[basis[[1,2]], n]], sumsToProducts[Rest[basis], target - n basis[[1, 1]]]]], {n,
     maxMultiplier[basis[[1, 1]], target], 0, -1}];
     
structToIndexRules = {
   MyInactive[StructureI][i_, j_, ___] :> {{j, Spinor}, {i, DottedSpinor}},
   MyInactive[StructureJ][i_, j_, k_] :> {{i, Spinor}, {i, DottedSpinor}},
   MyInactive[StructureK][i_, j_, k_] :> {{i, Spinor}, {j, Spinor}},
   MyInactive[StructureKBar][i_, j_, k_] :> {{i, DottedSpinor}, {j, DottedSpinor}},
   MyInactive[StructureL][i_, j_, k_, l_] :> {{i, Spinor}, {i, Spinor}},
   MyInactive[StructureLBar][i_, j_, k_, l_] :> {{i, DottedSpinor}, {i, DottedSpinor}},
   TensorProduct -> Join
};

constructStructure[expr_, perm_] := With[{inds = (expr /. structToIndexRules)},
	With[{unsym = TensorTranspose[CanonicallyOrderedComponents[expr /. MyInactive -> Identity], Ordering[inds[[;; , 2]]]], syms = Select[Permutations[Range@Length[inds]], inds[[#]] === inds &]},
	   If[syms == {}, unsym,
		(1/Length[syms]) TensorTranspose[Sum[TensorTranspose[unsym, p], {p, syms}], perm]
	   ]
	]
]

SpacetimeStructureExpressions::num = "Only 2-, 3-, and 4-point structures are supported.";
Options[SpacetimeStructureExpressions] = {"Overcomplete" -> False};
SpacetimeStructureExpressions[ls_, opt : OptionsPattern[]] := SpacetimeStructureExpressions[ls, opt] = If[!(2 <= Length[ls] <= 4),
   Message[SpacetimeStructureExpressions::num],
   If[TrueQ[OptionValue["Overcomplete"]],
   	With[{s2p = sumsToProducts[structBasis[Length[ls]], ls], cinds = Flatten[Table[{i, If[j == 1, Spinor, DottedSpinor]}, {i, Length[ls]}, {j, 2}, {k, 2 ls[[i, j]]}], 2]},
        {#, FindPermutation[If[Total[Flatten[ls]] == 0, {}, # /. structToIndexRules], cinds]}& /@ s2p
   	],
   	With[{structs = SpacetimeStructureExpressions[ls, "Overcomplete" -> True]},
	   	With[{comps = constructStructure @@@ structs},
	   		structs[[IndependentSet[comps, "Rules" -> (sctuvpt[2] /. Thread[{u, v} -> safeUVs[[2]]]), "MaxIndependent" -> numSTStructures[ls], "Indices" -> True]]]
	   	]
   	]
   ]
];

SpacetimeStructures[dims_, ls_, derivs_, derivtype_, perm_] := Table[Tensor[{{SpacetimeStructure[dims, ls, derivs, derivtype, perm, i],
   Sequence @@ Flatten[Table[
      {Table[{Lowered[Spinor], Lowered[DottedSpinor]}, Count[derivs, j]], Table[Lowered[Spinor], 2 ls[[j, 1]]], Table[Lowered[DottedSpinor], 2 ls[[j, 2]]]},
      {j, Length[dims]}]]}}], {i, numSTStructures[ls]}];
      
BuildTensor[{SpacetimeStructure[dims_, ls_, {}, derivtype_, perm_, i_], idxs___}] := With[{comps = Explicit[KinematicPrefactor[dims, ls]] (constructStructure @@ SpacetimeStructureExpressions[ls][[i]])},
 	If[Length[{idxs}] == 0, 
 	   comps /. x[ii_, j_] :> x[perm[[ii]], j],
 	   SparseArray[ArrayRules[comps] /. x[ii_, j_] :> x[perm[[ii]], j]]
 	]    
];

BuildTensor[{SpacetimeStructure[dims_, ls_, 
      derivs_, derivtype_, perm_, i_], idxs___}] /; derivs =!= {} := 
  With[{bd = 
     SpacetimeStructures[dims, ls, {}, "\[PartialD]", perm][[i]]},
   With[{dsis = 
      Table[2 Length[derivs] + 
        Join @@ (Range[#1 + 1, #2] & @@@ 
           Partition[Accumulate[2 Flatten[ls[[;; ii]]]], 2]), {ii, 
        Length[dims] - 1}],
     sis = 
      Table[2 Length[derivs] + 
        Join @@ (Range[#1 + 1, #2] & @@@ 
           Partition[Accumulate[Prepend[2 Flatten[ls[[;; ii]]], 0]], 
            2]), {ii, Length[dims] - 1}], 
     pd = perm[[#]] & /@ derivs},
    TensorTranspose[
     CanonicallyOrderedComponents@Fold[TensorPermute, Switch[derivtype,
        "\[PartialD]", 
        Fold[TensorSpinorDerivative, bd, ReverseSort[pd]],
        "u", 
        TensorProduct[
         Fold[TensorSpinorDerivative, u[perm], ReverseSort[pd]], bd],
        "v", 
        TensorProduct[
         Fold[TensorSpinorDerivative, v[perm], ReverseSort[pd]], bd]
        ], 
       Table[PermutationList[
         InversePermutation@
          PermutationPower[
           Cycles[{Join[
              Range[2 Count[derivs, x_ /; x <= (Length[dims] - ii)] + 
                1, 2 Count[derivs, 
                 x_ /; x <= (Length[dims] + 1 - ii)], 2], 
              sis[[Length[dims] - ii]]], 
             Join[Range[
               2 Count[derivs, x_ /; x <= (Length[dims] - ii)] + 2, 
               2 Count[derivs, x_ /; x <= (Length[dims] + 1 - ii)], 
               2], dsis[[Length[dims] - ii]]]}], 
           Count[derivs, Length[dims] + 1 - ii]], 
         Length@Indices[bd] + 2 Length[pd]], {ii, Length[dims] - 1}]],
      Ordering@{idxs}]
    ]
   ];

RotationSpinor = 
  Tensor[{{"M", Raised[SpaceTime], Lowered[SpaceTime], 
     Lowered[Spinor], Raised[Spinor]}}];
RotationDottedSpinor = 
  Tensor[{{"M", Raised[SpaceTime], Lowered[SpaceTime], 
     Lowered[DottedSpinor], Raised[DottedSpinor]}}];

BuildTensor[{"M", Raised[SpaceTime], Lowered[SpaceTime], 
    Lowered[Spinor], Raised[Spinor]}] := 
  1/2 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Eta]Upper, \[Sigma]Lower, \[Sigma]BarLower], \
{{2, 3}, {5, 7}}] - 
      Contract[
       TensorProduct[\[Eta]Upper, 
        TensorPermute[
         TensorProduct[\[Sigma]Lower, \[Sigma]BarLower], {4, 2, 3, 1, 
          5, 6}]], {{2, 3}, {5, 7}}]], 
     Ordering[{Raised[SpaceTime], Lowered[SpaceTime], Lowered[Spinor],
        Raised[Spinor]}]];

BuildTensor[{"M", Raised[SpaceTime], Lowered[SpaceTime], 
    Lowered[DottedSpinor], Raised[DottedSpinor]}] := - 1/
   2 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Eta]Upper, \[Sigma]BarLower, \[Sigma]Lower], \
{{2, 3}, {5, 7}}] - 
      Contract[
       TensorProduct[\[Eta]Upper, 
        TensorPermute[
         TensorProduct[\[Sigma]BarLower, \[Sigma]Lower], {4, 2, 3, 1, 
          5, 6}]], {{2, 3}, {5, 7}}]], 
     Ordering[{Raised[SpaceTime], Lowered[SpaceTime], 
       Lowered[DottedSpinor], Raised[DottedSpinor]}]];

ConformalCheck[t : Tensor[{{SpacetimeStructure[deltas_, spins_, {}, "\[PartialD]", perm_, i_], idxs___}}]] := ConformalCheck[deltas, spins, perm, t];

ConformalCheck[deltas_, spins_, perm_, t_] := 
  With[{xs = 
      Flatten[Table[{perm[[i]], 
         2 Total[Flatten@spins[[;; i - 1]]] + j}, {i, 
         Length[deltas]}, {j, 2 spins[[i, 1]]}], 1], 
     xsDot = Flatten[
       Table[{perm[[i]], 
         2 Total[Flatten@spins[[;; i - 1]]] + 2 spins[[i, 1]] + 
          j}, {i, Length[deltas]}, {j, 2 spins[[i, 2]]}], 1]},
    Sum[2 Contract[
         TensorProduct[XX[i], XX[i], 
          TensorDerivative[t, i]], {{2, 3}}] - 
       XXSquared[i] Contract[
         TensorProduct[\[Eta]Upper, 
          TensorDerivative[t, i]], {{2, 3}}], {i, Length[deltas]}] + 
     2 Sum[deltas[[InversePermutation[perm][[i]]]] TensorProduct[
         XX[i], t], {i, Length[deltas]}] +
     Sum[
      TensorPermute[
       Contract[
        TensorProduct[XX[xs[[idx, 1]]], RotationSpinor, 
         t], {{1, 3}, {5, 5 + xs[[idx, 2]]}}], 
       InversePermutation@
        PermutationList[Cycles[{Prepend[2 + xs[[;; idx - 1, 2]], 2]}],
          1 + 2 Total[Flatten[spins]]]], {idx, Length[xs]}] + 
     Sum[TensorPermute[
       Contract[
        TensorProduct[XX[xsDot[[idx, 1]]], RotationDottedSpinor, 
         t], {{1, 3}, {5, 5 + xsDot[[idx, 2]]}}], 
       InversePermutation@
        PermutationList[
         Cycles[{Prepend[2 + xsDot[[;; idx - 1, 2]], 2]}], 
         1 + 2 Total[Flatten[spins]]]], {idx, Length[xsDot]}]
    ];
    
AppendTo[TensorTools`Private`explicitRules, 
  u[{ii_, jj_, kk_, ll_}] :> (XXSquared[ii, jj] XXSquared[kk, ll])/(
   XXSquared[ii, kk] XXSquared[jj, ll])];
AppendTo[TensorTools`Private`explicitRules, 
  v[{ii_, jj_, kk_, ll_}] :> (XXSquared[ii, ll] XXSquared[jj, kk])/(
   XXSquared[ii, kk] XXSquared[jj, ll])];
   
uvpowers[i_, perm_] := 
 uvpowers[i, perm] = 
  With[{p1 = {1, -1, 0, 0, -1, 1}, p2 = {0, -1, 1, 1, -1, 0}, 
    q = (XXSquared[##] D[If[i == 1, u, v][perm] /. TensorTools`Private`explicitRules, 
         XXSquared[##]])/(If[i == 1, u, v][perm] /. TensorTools`Private`explicitRules) & @@@
       Subsets[Range[4], {2}]},
   SolveValues[
     Thread[Flatten[q - \[Alpha]1 p1 - \[Alpha]2 p2] == 
       0], {\[Alpha]1, \[Alpha]2}][[1]]
   ]
   
Format[g[fields_, i_, j_], TraditionalForm] := 
\!\(\*SubsuperscriptBox[\("\<g\>"\), \(Row[{ToString[i], 
     ToString[j]}]\), \(StringJoin @@ \((First /@ fields)\)\)]\);
     
Format[\[Lambda][fields_, i_, j_], TraditionalForm] := 
\!\(\*SubsuperscriptBox[\("\<\[Lambda]\>"\), \(Row[{ToString[i], 
     ToString[j]}]\), \(StringJoin @@ \((First /@ fields)\)\)]\);
     
Format[u[perm_], TraditionalForm] := 
  "U"^#1 "V"^#2 & @@ uvpowers[1, perm];
Format[v[perm_], TraditionalForm] := "U"^#1 "V"^#2 & @@ uvpowers[2, perm];
     
(*fieldOrder = (ScalingDimension[#1] < 
      ScalingDimension[#2]) || (ScalingDimension[#1] == 
       ScalingDimension[#2] && 
      Abs[Last[#1]] > Abs[Last[#2]]) || (ScalingDimension[#1] == 
       ScalingDimension[#2] && Abs[Last[#1]] == Abs[Last[#2]] && 
      Last[#1] >= Last[#2]) &;*)
      
fieldOrder = OrderedQ[{{N@ScalingDimension[#1], Reverse@Spin[#1], Abs[Last[#1]], -Last[#1]}, {N@ScalingDimension[#2], Reverse@Spin[#2], Abs[Last[#2]], -Last[#2]}}] &;
      
crossingPermutationST[t_Tensor, order_] := 
  With[{ordered = SwapFactors[t, order]},
   InversePermutation@
    Ordering[
     TensorPermutation[ordered][[
      Select[Range@
        Length[Indices[ordered]], ! FreeQ[Indices[ordered][[#]], Spinor | DottedSpinor] &]]]]
   ];
crossingPermutationR[t_Tensor, order_] := 
  With[{ordered = SwapFactors[t, order]},
   InversePermutation@
    Ordering[
     TensorPermutation[ordered][[
      Select[Range@
        Length[Indices[ordered]], ! FreeQ[Indices[ordered][[#]], RIndex] &]]]]
   ];

ExpandCorrelator[Correlator[Tensor[names_]], 
    OptionsPattern[]] /; (AllTrue[names, 
      !MissingQ[name2field@StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] &] && 
     3 <= Length[names] <= 4) := 
  With[{fields = 
     name2field[
        StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names, 
    derivs = 
     Flatten@Table[
       i, {i, Length[names]}, {j, 
        Count[Characters[names[[i, 1]]], "\[PartialD]"]}]},
   With[{sfields = Sort[fields, fieldOrder], 
     order = Ordering[fields, All, fieldOrder]},
    Module[{numinvs = numInvariants[RRep /@ sfields], 
      numsts = numSTStructures[Spin /@ sfields],
      sign = 
       Signature@
        InversePermutation[order][[
         Select[Range@
           Length@fields, ! IntegerQ[ScalingDimension[fields[[#]]]] &]]], 
      STindperm = crossingPermutationST[Tensor[names], order], 
      Rindperm = crossingPermutationR[Tensor[names], order]},
     sign Sum[
       If[Length[names] == 3, \[Lambda][sfields,i,j], g[sfields, i, j][u[order], v[order]]] TensorProduct[
          TensorPermute[
           If[Length[names] == 4,
           	FourPtRInvariant[{##}, i] & @@ (RRep /@ sfields),
           	ThreePtRInvariant @@ (RRep /@ sfields)
           ], 
           Rindperm], 
          TensorPermute[
           SpacetimeStructures[ScalingDimension /@ sfields, Spin /@ sfields, 
             InversePermutation[order][[#]] & /@ derivs, 
             "\[PartialD]", order][[j]], STindperm]] + 
        If[derivs === {} || Length[names] == 3, 0,
         
         Derivative[1, 0][g[sfields, i, j]][u[order], 
            v[order]] TensorProduct[
            TensorPermute[
             FourPtRInvariant[{##}, i] & @@ (RRep /@ sfields), 
             Rindperm], 
            TensorPermute[
             SpacetimeStructures[ScalingDimension /@ sfields, 
               Spin /@ sfields, 
               InversePermutation[order][[#]] & /@ derivs, "u", 
               order][[j]], STindperm]] +
          
          Derivative[0, 1][g[sfields, i, j]][u[order], 
            v[order]] TensorProduct[
            TensorPermute[
             FourPtRInvariant[{##}, i] & @@ (RRep /@ sfields), 
             Rindperm], 
            TensorPermute[
             SpacetimeStructures[ScalingDimension /@ sfields, 
               Spin /@ sfields, 
               InversePermutation[order][[#]] & /@ derivs, "v", 
               order][[j]], STindperm]]
         ],
       {i, numinvs}, {j, numsts}
       ]
     ]
    ]
   ];

$ArbitraryFunctions = {};
DeclareArbitraryFunction[head_] := 
  If[! MemberQ[$ArbitraryFunctions, head], 
   AppendTo[$ArbitraryFunctions, head]];

$SolvedCorrelators = {};
SolvedCorrelators[] := $SolvedCorrelators;

  
Options[WardEquations] = {"QBar" -> False};
WardEquations[names : {Except[_Field]..}, opt : OptionsPattern[]] := WardEquations[name2field /@ (ToString[ToExpression[#], TraditionalForm] & /@ names), opt];
WardEquations[fields_, opt: OptionsPattern[]] := WardEquations[fields, opt] = With[{expr = Correlator@
             NormalOrder[TensorProduct[QTensor["QBar" -> OptionValue["QBar"]], Tensor[fields]], 
              "Vacuum" -> True]},
    If[expr === 0, {},
    DeleteCases[Thread[Flatten[
        ExpansionComponents[
         ExpandCorrelator@expr
             /. Join@@ (SUSYRules /@ $multipletIndices),
          "MonitorProgress" -> True]] == 0], True]
    ]
];

spQ[f_Field] := MemberQ[MinimalBy[multipletOf[f], ScalingDimension], f];
spCorrelatorQ[g[ffs_, __][__]] := AllTrue[ffs, spQ];
spCorrelatorQ[Derivative[__][g[ffs_, __]][__]] := AllTrue[ffs, spQ];

Options[SolveWard] = {"QBar" -> False, "Fit" -> False};
SolveWard[names : {Except[_Field]..}, opt : OptionsPattern[]] :=
   SolveWard[name2field /@ (ToString[ToExpression[#], TraditionalForm] & /@ names), opt];
SolveWard[fields : {_Field..}, OptionsPattern[]] := Module[{eqs, vars, bm},
   eqs = DeleteCases[CrossingSimplify[WardEquations[fields, "QBar" -> OptionValue["QBar"]] /. Normal[First /@ SolvedCorrelators[]]], True];
   vars = SortBy[Select[DeleteDuplicates@Cases[eqs, g[__][__], All], !spCorrelatorQ[#]&], Total[Table[Boole[IntegerQ[i]], {i, #[[0,1]]}]] &];
   bm = CoefficientArrays[eqs, vars];
	 If[OptionValue["Fit"],
	    wardSolveFit[eqs, vars],
	   If[ AllTrue[bm[[1]], # === 0 &],
	      Thread[vars -> 0],
	      Sort[solveGroups[Partition[eqs /. u -> Glaisher /. v -> EulerGamma, UpTo[1]], vars /. u -> Glaisher /. v -> EulerGamma, {}, {}] /. Glaisher -> u /. EulerGamma -> v]
	   ]
	 ]
   ];
   
Clear[solveData]; 
solveData[m_, b_, num_] := 
 solveData[m, b, num] = 
  With[{terms = 
     DeleteDuplicates@
      Cases[b, (Alternatives @@ 
           $ArbitraryFunctions)[__] | 
        Derivative[__][(Alternatives @@ 
            $ArbitraryFunctions)][__], All], 
    crM = clearRadicals[m]}, 
   ResourceFunction["MonitorProgress"][
    Table[{uv, 
      Simplify@
        LinearSolve[
         crM[[1]] /. Thread[{u, v} -> uv] /. _\[Theta] -> 0, 
         b /. Thread[terms -> Array[\[Alpha], Length[terms]]] /. 
           Thread[{u, v} -> uv] /. 
          Thread[Array[\[Alpha], Length[terms]] -> 
            terms]] crM[[2]]}, {uv, safeUVs}], 
    "Label" -> "Generating solution data", 
         "CurrentDisplayFunction" -> None]
   ];
  
clearRadicals[mat_] := 
  With[{factors = 
     Table[FirstCase[mat[[;; , i]], 
       x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
          Cases[x, Power[y_?NumericQ, 1/2 | -1/2] :> Sqrt[y], 
           All])], {i, Dimensions[mat][[2]]}]},
   {mat . DiagonalMatrix[factors], factors}
   ];

indQ[basis_, vec_] := 
  Quiet@Check[LinearSolve[Transpose[basis], vec]; False, True];

wardSolveFit[eqs_, vars_] := 
  With[{bm = CoefficientArrays[eqs, vars]}, 
   With[{b = -bm[[1]], m = bm[[2]]}, 
    With[{crM = clearRadicals[m]}, 
     With[{indIdxs = 
        ResourceFunction["MonitorProgress"][
         Fold[If[Length[#1] < Length[vars] && 
             indQ[
              crM[[1, #1]] /. 
               Thread[{u, v} -> safeUVs[[25]]], 
              crM[[1, #2]] /. 
               Thread[{u, v} -> safeUVs[[25]]]], 
            Append[#1, #2], #1] &, {}, Range[Length[m]]], 
         "Label" -> "Finding set of independent equations", 
         "CurrentDisplayFunction" -> None]},
      
      With[{sd = 
         solveData[m[[indIdxs]], b[[indIdxs]], 7]},
       Thread[
        vars -> ResourceFunction["MonitorProgress"][
          Table[With[{terms = 
              DeleteDuplicates@
               Cases[sd[[;; , 2, 
                  vari]], (Alternatives @@ 
                    $ArbitraryFunctions)[__] | 
                 Derivative[__][(Alternatives @@ 
                    $ArbitraryFunctions)][__], All]}, 
            terms . Table[
              Simplify@
               fitRational[(sd /. {{u_?NumericQ, v_}, 
                    data_} :> {u, v, 
                    Coefficient[data[[vari]], term]}), 1, 6, 
                "Prefactors" -> {1, Sqrt[u], Sqrt[v], 
                  Sqrt[u v]}], {term, terms}]], {vari, Length[vars]}],
           "Label" -> "Fitting rational functions", 
         "CurrentDisplayFunction" -> None]]]]]]];

crosses = {{u -> u, v -> v}, {u -> u/v, v -> 1/v}, {u -> 1/u, 
    v -> v/u}, {u -> v/u, v -> 1/u}, {u -> 1/v, v -> u/v}, {u -> v, 
    v -> u}};
    
AddSolutions[soln_] := With[{uvVersions = Select[Flatten[Table[s /. cross, {s, soln}, {cross, crosses}]], MatchQ[#[[1]], g[__][u,v]]&] /. HoldPattern[a_ -> b_] :> (Head[a] -> Function[{u,v}, b])},
  $SolvedCorrelators = Merge[{$SolvedCorrelators, Association[uvVersions]}, Flatten @* List];
  $SolvedCorrelators = Association @@ Table[k -> Table[Function[{u,v}, Evaluate[val /. (First /@ $SolvedCorrelators)]], {val, $SolvedCorrelators[k]}], {k, Keys[$SolvedCorrelators]}]
];    

AddSolutions[soln_] := 
  Block[{tmp}, 
   With[{uvVersions = 
      Values /@ 
       GroupBy[Select[
          Flatten[Table[
            s /. cross, {s, soln}, {cross, crosses}]], 
          MatchQ[#[[1]], g[__][u, v]] &] /. 
         HoldPattern[
           a_ -> b_] :> (Head[
             a] -> (Function[{u, v}, tmp] /. tmp -> b)), First]},
    $SolvedCorrelators = 
     Merge[{$SolvedCorrelators, Association[uvVersions]}, 
      DeleteDuplicates@*Flatten@*List];
    $SolvedCorrelators = 
     Association @@ 
      Table[k -> 
        Table[Function[{u, v}, 
          Evaluate[Simplify[CrossingSimplify[val[u, v] /. (First /@ $SolvedCorrelators)], uvAssumptions]]], {val, $SolvedCorrelators[k]}], {k, 
        Keys[$SolvedCorrelators]}];
    ]
   ];

$crossingRules = <||>;

DeclareCrossingRule::unknown = 
  "The function `` has not been declared using \
DeclareArbitraryFunction.";
DeclareCrossingRule::invalid = 
  "The function `` is not related by crossing to ``[u, v].";
DeclareCrossingRule[head_[arg1_, arg2_], rhs_] := 
  If[! MemberQ[crosses[[;; , ;; , 2]], {arg1, arg2}], 
   Message[DeclareCrossingRule::invalid, head[arg1, arg2], head],
   If[! MemberQ[$ArbitraryFunctions, head], 
    Message[DeclareCrossingRule::unknown, head],
    $crossingRules[head[arg1, arg2]] = Function[{$x, $y}, Evaluate[rhs /. Solve[$x == arg1 && $y == arg2, {u,v}][[1]]]]
   ]
  ];

CrossingSimplify[expr_] := 
  expr /. tterm : (Alternatives @@ 
        Table[f[args__] | 
          Derivative[__][f][args__], {f, $ArbitraryFunctions}]) /; 
     Length[{args}] == 2 && {args} =!= {u, v} :> (tterm /. 
      Table[f -> $crossingRules[f[args]], {f, $ArbitraryFunctions}]);
  
End[]

EndPackage[]

