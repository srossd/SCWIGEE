(* Wolfram Language package *)

SetRSymmetry[group_] := (
	$RSymmetry = group;
	$QTensor = Tensor[{{"Q", Raised[RIndex[fundRep[$RSymmetry]]], Lowered[Spinor]}}];
	$QBarTensor = Tensor[{{"\!\(\*OverscriptBox[\(Q\), \(~\)]\)", Lowered[RIndex[fundRep[$RSymmetry]]], Lowered[DottedSpinor]}}];
);

SetDefectRSymmetry::manyembed = "There is not a unique embedding of `1` into `2`; cannot proceed.";
SetDefectRSymmetry::noembed = "There is no embedding of `1` into `2`; cannot proceed.";
SetDefectRSymmetry[group_] := Module[{embeddings, defectFund},
	$DefectRSymmetry = group;
	embeddings = Embeddings[$RSymmetry, $DefectRSymmetry];
	If[embeddings == {}, Message[SetDefectRSymmetry::noembed, CMtoName[$DefectRSymmetry], CMtoName[$RSymmetry]]; Return[]];
	If[Length[embeddings] > 1, Message[SetDefectRSymmetry::manyembed, CMtoName[$DefectRSymmetry], CMtoName[$RSymmetry]]; Return[]];
	defectFund = decomposeRepDefect[fundRep[$RSymmetry]];
	$QTensorDefect = Tensor[{{"Q", Raised[DefectRIndex[defectFund]], Lowered[Spinor]}}];
	$QBarTensorDefect = Tensor[{{"\!\(\*OverscriptBox[\(Q\), \(~\)]\)", Lowered[DefectRIndex[defectFund]], Lowered[DottedSpinor]}}];
];

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
DefectRSymmetry[] := $DefectRSymmetry;
Multiplet[i_] := $multiplet[i];

$signatureFactor = 1;
SignatureFactor[] := $signatureFactor;

SetSignature["Lorentzian"] := ($signatureFactor = 1);
SetSignature["Euclidean"] := ($signatureFactor = I);

SetSignature::badsig = "The signature `` is not recognized; use \"Lorentzian\" or \"Euclidean\".";
SetSignature[sig_] := Message[SetSignature::badsig, sig];

SetDefectCodimension::invalid = "The codimension `` needs to be an integer between 1 and 3 inclusive, or None.";
SetDefectCodimension[q_] := If[MemberQ[{None,1,2,3},q], ($qdefect = q), Message[SetDefectCodimension::invalid, q]];

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
$DefectRSymmetry = Null;
$editing = True;
$qdefect = None;
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
           Hold[$nfName]], SimpleRepInputConversion[RSymmetry[], $nfRep], $nfDim, {$nfL, $nfLb}/2, 
         2 $nfRCharge]]]}]]);
         
multipletDialog[] := ($multName = Null; $multSC = True;
   DialogInput[
    DialogNotebook[{Framed@
       Panel[Grid[{{"Multiplet name:", 
           InputField[Dynamic[$multName]]}, {"Self-conjugate:", 
           Checkbox[Dynamic[$multSC]]}}], 
        Style["Add Multiplet", 20], Top, Background -> LightBlue], 
      DefaultButton[DialogReturn[{ToString[$multName], $multSC}]]}]]);
      
conventionButton[eq_] := With[{s = ToString@TeXForm[eq]},
   Button[ClickToCopy[
  Style[eq, {"Output", "TraditionalForm"}], Null], 
 CopyToClipboard@s, 
 Appearance -> "Frameless"]
]
      
conventions[1] := {\[Eta]Lower == MatrixForm[Components[\[Eta]Lower]], \[Epsilon]Lower ==
   MatrixForm@Components[\[Epsilon]Lower], \[Epsilon]LowerDot == 
  MatrixForm@Components[\[Epsilon]LowerDot], \[Sigma]Lower == 
  MatrixForm /@ Components[\[Sigma]Lower], \[Sigma]BarLower == 
  MatrixForm /@ Components[\[Sigma]BarLower], 
 Subscript["\[Epsilon]", Row[{0, 1, 2, 3}]] == 
  Components[\[Epsilon]Spacetime][[1, 2, 3, 4]]} /. (x_ == y_) :> conventionButton[x == y];
  
conventions[2] := Flatten[Table[
   MyInactive[Structure[struct, $qdefect]] @@ (FromCharacterCode /@ Range[105, 105 + m - 1]) == 
   Structure[struct, $qdefect] @@ (FromCharacterCode /@ Range[105, 105 + m - 1]),  
   {m, maxPts[$qdefect]}, {struct, allStructures[m, $qdefect]}]] /. (x_ == y_) :> conventionButton[x == y];
  
conventionsPanel[] := Row[{Column[Prepend[conventions[1], Style["General conventions", Bold, 16]], Spacings -> 2]//TraditionalForm, 
   Spacer[20], 
   Column[{Style["Spacetime structure building blocks", Bold, 16], Pane[Column[conventions[2]] //TraditionalForm, ImageSize -> {700, 500}, Scrollbars -> True]}, Spacings -> 1]
}, Alignment -> Top];
         
wizardPanel[] := Panel[Grid[{{"", "", Style["Setup Wizard", 20], 
    Row[{Style["Editing: ", 16], Spacer[5], 
      Checkbox[Dynamic[$editing]]}]}, {"", "", 
    Grid[{{Style["Signature: ", 14], 
      Dynamic[RadioButtonBar[
        Dynamic[$signatureFactor], {1 -> Style["Lorentzian", 12], 
         I -> Style["Euclidean", 12]}, Enabled -> $editing]]},
      {Style["Defect: ", 14], Dynamic[RadioButtonBar[
        Dynamic[$qdefect], {None -> Style["None", 12], 
         3 -> Tooltip[Style["\!\(\*FormBox[\(q\), TraditionalForm]\) = 3", 12], "Defect codimension 3"]}, Enabled -> $editing]]}}, Alignment -> Left], 
    Dynamic[PopupWindow[Button["View Conventions"], conventionsPanel[], WindowSize -> {1100, 800}, WindowFloating -> False, WindowTitle -> "Conventions"]]}, {"", "", 
    Dynamic[Grid[{{Style["R-symmetry: ", 16], 
      Dynamic[InputField[
        Dynamic[$RSymmetry, {Automatic, SetRSymmetry[#1] &}], 
        Enabled -> $editing, FieldSize -> {20, 1}]]},
        If[$qdefect =!= None, {Style["Defect R-symmetry: ",16],Dynamic[InputField[
        Dynamic[$DefectRSymmetry, {Automatic, SetDefectRSymmetry[#1] &}], 
        Enabled -> $editing, FieldSize -> {20, 1}]]},Nothing]}, Alignment -> Right]], "", ""}, {"", 
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

wizardCell = CellPrint[ExpressionCell[SCWIGE`Private`wizardPanel[], TextAlignment -> Center]]