(* Wolfram Language package *)

Format[Field[name_, rep_, dim_, {j1_, j2_}, y_], TraditionalForm] := 
  With[{indices = 
     Join[{"\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]"}[[;; 
        2 j1]], {"\!\(\*OverscriptBox[\(\[Alpha]\), \(.\)]\)", 
        "\!\(\*OverscriptBox[\(\[Beta]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Gamma]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Delta]\), \(.\)]\)"}[[;; 2 j2]]]},
     Subsuperscript[If[StringQ[name], ToExpression[name, TraditionalForm, HoldForm], name], If[indices == {}, "", Row[ToExpression[#, TraditionalForm, HoldForm] & /@ indices]],RepName[$RSymmetry, rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]]
  ];
       
Format[x[i_, j_], TraditionalForm] := Superscript[Subscript[x, i], j];
Format[SpacetimePoint[i_], TraditionalForm] := Subscript["\!\(TraditionalForm\`x\)", i];
Format[SpacetimePointDefect[i_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)" i];
Format[SpacetimePointTransverse[i_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", i];
Format[SpacetimeSeparation[i_, j_], TraditionalForm] := Subscript["\!\(TraditionalForm\`x\)", Row[{i, j}]];
Format[SpacetimeSeparationDefect[i_, j_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", Row[{i, j}]];
Format[SpacetimeSeparationTransverse[i_, j_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", Row[{i, j}]];

Format[XXSquared[i_], TraditionalForm] := (Subscript["\!\(TraditionalForm\`x\)", Row[{i}]])^2;
Format[XXSquared[i_, j_], TraditionalForm] := (Subscript["\!\(TraditionalForm\`x\)", 
   Row[{i, j}]])^2;
MakeBoxes[Power[XXSquared[xs__], n_], TraditionalForm] := 
  If[n === 1/2, 
   TemplateBox[{SubscriptBox["x", StringJoin @@ (ToString /@ {xs})]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["x", StringJoin @@ (ToString /@ {xs})], 2 n]];
    
Format[XXSquaredDefect[i_], TraditionalForm] := (Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", Row[{i}]])^2;
Format[XXSquaredDefect[i_, j_], TraditionalForm] := (Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", 
   Row[{i, j}]])^2;
MakeBoxes[Power[XXSquaredDefect[xs__], n_], TraditionalForm] := 
  If[n === 1/2, 
   TemplateBox[{SubscriptBox["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", StringJoin @@ (ToString /@ {xs})]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", StringJoin @@ (ToString /@ {xs})], 2 n]];
    
Format[XXSquaredTransverse[i_], TraditionalForm] := (Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", Row[{i}]])^2;
Format[XXSquaredTransverse[i_, j_], TraditionalForm] := (Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", 
   Row[{i, j}]])^2;
MakeBoxes[Power[XXSquaredTransverse[xs__], n_], TraditionalForm] := 
  If[n === 1/2, 
   TemplateBox[{SubscriptBox["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", StringJoin @@ (ToString /@ {xs})]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", StringJoin @@ (ToString /@ {xs})], 2 n]];
    
Format[XXDot[i_, j_], TraditionalForm] := Row[{"(",Subscript["\!\(TraditionalForm\`x\)", i], " \[CenterDot] ", Subscript["\!\(TraditionalForm\`x\)", j], ")"}];
Format[XXDotDefect[i_, j_], TraditionalForm] := Row[{"(",Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", i], " \[CenterDot] ", Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", j], ")"}];
Format[XXDotTransverse[i_, j_], TraditionalForm] := Row[{"(",Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", i], " \[CenterDot] ", Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", j], ")"}];

Format[SUSYCoefficient[name_, idx_, opt : OptionsPattern[]], TraditionalForm] := Subscript[
	If[OptionValue[SUSYCoefficient, opt, "QBar"], "\!\(\*OverscriptBox[\(\[ScriptA]\), \(_\)]\)", "\[ScriptA]"],
   Row[{name, ",", idx}]];

Format[Correlator[t_], TraditionalForm] := 
  Row[{"\[LeftAngleBracket]", t, "\[RightAngleBracket]"}];
  
Format[RInvariant[i_], TraditionalForm] := Subscript["C", i];

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

Format[SpacetimeStructure[dims_, ls_, derivs_, derivtype_, perm_, i_], 
   TraditionalForm] := 
\!\(\*SubsuperscriptBox[\("\<\[ScriptCapitalS]\>"\), \
\(Row[{derivtype, \(Count[derivs, #] &\) /@ 
      Range@Length[ls], "\<;\>", perm, "\<;\>", i}]\), \(Row[
    Riffle[Thread[{dims, ls}], "\<;\>"]]\)]\);
   
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
     
Format[u[perm_], TraditionalForm] := 
  "U"^#1 "V"^#2 & @@ uvpowers[1, perm];
Format[v[perm_], TraditionalForm] := "U"^#1 "V"^#2 & @@ uvpowers[2, perm];
   
Format[g[fields_, i_, j_], TraditionalForm] := 
\!\(\*SubsuperscriptBox[\("\<g\>"\), \(Row[{ToString[i], 
     ToString[j]}]\), \(StringJoin @@ \((First /@ fields)\)\)]\);
     
Format[\[Lambda][fields_, i_, j_], TraditionalForm] := 
\!\(\*SubsuperscriptBox[\("\<\[Lambda]\>"\), \(Row[{ToString[i], 
     ToString[j]}]\), \(StringJoin @@ \((First /@ fields)\)\)]\);