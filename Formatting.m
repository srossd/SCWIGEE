(* Wolfram Language package *)

Format[Operator[name_, rep_, dim_, {j1_, j2_}, y_], TraditionalForm] := 
  With[{indices = 
     Join[{"\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]"}[[;; 
        2 j1]], {"\!\(\*OverscriptBox[\(\[Alpha]\), \(.\)]\)", 
        "\!\(\*OverscriptBox[\(\[Beta]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Gamma]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Delta]\), \(.\)]\)"}[[;; 2 j2]]]},
     Subsuperscript[If[StringQ[name], ToExpression[name, TraditionalForm, HoldForm], name], If[indices == {}, "", Row[ToExpression[#, TraditionalForm, HoldForm] & /@ indices]],repName[rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]]
  ];
       
Format[x[i_, j_], TraditionalForm] := Subsuperscript[x, i, j];
MakeBoxes[Power[x[i_,j_], n_], TraditionalForm] := SuperscriptBox[RowBox[{"(", SubsuperscriptBox["x",i,j], ")"}], ToBoxes[n, TraditionalForm]]
Format[SpacetimePoint[i_], TraditionalForm] := Subscript["\!\(TraditionalForm\`x\)", i];
Format[SpacetimePointDefect[i_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)" i];
Format[SpacetimePointTransverse[i_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", i];
Format[SpacetimeSeparation[i_, j_], TraditionalForm] := Subscript["\!\(TraditionalForm\`x\)", i, j];
Format[SpacetimeSeparationDefect[i_, j_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", i, j];
Format[SpacetimeSeparationTransverse[i_, j_], TraditionalForm] := Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", i, j];

Format[XXSquared[xs__], TraditionalForm] := (Subscript["\!\(TraditionalForm\`x\)", StringJoin @@ (ToString /@ {xs})])^2;
MakeBoxes[Power[XXSquared[xs__], n_], TraditionalForm] := 
  If[n === 1/2, 
   TemplateBox[{SubscriptBox["x", StringJoin @@ (ToString /@ {xs})]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["x", StringJoin @@ (ToString /@ {xs})], ToBoxes[2 n, TraditionalForm]]];
    
Format[XXSquaredDefect[xs___], TraditionalForm] := (Subscript["(\!\(\*SuperscriptBox[\(x\), \(\[DoubleVerticalBar]\)]\))", StringJoin @@ (ToString /@ {xs})])^2;
MakeBoxes[Power[XXSquaredDefect[xs__], n_], TraditionalForm] := 
  If[n === 1/2, 
   TemplateBox[{SubscriptBox[SuperscriptBox["x", "\[DoubleVerticalBar]"], RowBox[ToString /@ {xs}]]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["(\!\(\*SuperscriptBox[\(x\), \(\[DoubleVerticalBar]\)]\))", StringJoin @@ (ToString /@ {xs})], ToBoxes[2 n, TraditionalForm]]];
    
Format[XXSquaredTransverse[xs__], TraditionalForm] := (Subscript["(\!\(\*SuperscriptBox[\(x\), \(\[UpTee]\)]\))", StringJoin @@ (ToString /@ {xs})])^2;
MakeBoxes[Power[XXSquaredTransverse[xs__], n_], TraditionalForm] := 
  If[n === 1/2, 
   TemplateBox[{SubscriptBox[SuperscriptBox["x", "\[UpTee]"], StringJoin @@ (ToString /@ {xs})]}, 
    "Abs"], SuperscriptBox[
    SubscriptBox["(\!\(\*SuperscriptBox[\(x\), \(\[UpTee]\)]\))", RowBox[ToString /@ {xs}]], ToBoxes[2 n, TraditionalForm]]];
    
Format[XXDot[i_, j_], TraditionalForm] := Row[{"(",Subscript["\!\(TraditionalForm\`x\)", i], "\[CenterDot]", Subscript["\!\(TraditionalForm\`x\)", j], ")"}];
Format[XXDotDefect[i_, j_], TraditionalForm] := Row[{"(",Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", i], "\[CenterDot]", Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[DoubleVerticalBar]\)]\)", j], ")"}];
Format[XXDotTransverse[i_, j_], TraditionalForm] := Row[{"(",Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", i], "\[CenterDot]", Subscript["\!\(\*SuperscriptBox[\(TraditionalForm\`x\), \(\[UpTee]\)]\)", j], ")"}];

Format[SUSYCoefficient[name_, idx_, opt : OptionsPattern[]], TraditionalForm] := Subscript[
	If[OptionValue[SUSYCoefficient, opt, "QBar"], "\!\(\*OverscriptBox[\(\[ScriptA]\), \(_\)]\)", "\[ScriptA]"],
   Row[{name, ",", idx}]];
   
Format[\[Sigma]LowerTensor[i_], TraditionalForm] := Row[{"(",Subscript["\[Sigma]", i],")"}];
Format[\[Sigma]UpperTensor[i_], TraditionalForm] := Row[{"(",Superscript["\[Sigma]", i],")"}];
Format[\[Sigma]BarLowerTensor[i_], TraditionalForm] := Row[{"(",Subscript["\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", i],")"}];
Format[\[Sigma]BarUpperTensor[i_], TraditionalForm] := Row[{"(",Superscript["\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", i],")"}];

Format[SU2BreakingTensor[], TraditionalForm] := "\[Tau]";

Format[Correlator[t_, opt: OptionsPattern[]], TraditionalForm] := 
  Row[{"\[LeftAngleBracket]", t, If[OptionValue[Correlator, "Defect"], "\!\(\*SubscriptBox[\(\[RightAngleBracket]\), \(\[ScriptCapitalD]\)]\)", "\[RightAngleBracket]"]}];
  
Format[GlobalInvariant[i_], TraditionalForm] := Subscript["C", i];

Format[RepWithMultiplicity[rep_, i_], TraditionalForm] := Subscript[repName[rep], i]; 

cols = {Darker@Green, Blue, Black, Orange, Red, Purple, Yellow};
arrow[{p1_, p2_}] := {Line[{p1, p2}], Arrow[{p1, .4 p1 + .6 p2}]};
reverseArrow[{p1_, p2_}] := arrow[{p2, p1}];
repStyles[multreps_] := 
 Association@With[{reps = 
    SortBy[#, Function[rep, {Times @@ repDim[rep], MatchQ[repName[rep], _OverBar]}]] & /@ 
     GroupBy[DeleteDuplicates[DeleteCases[multreps, singRep[GlobalSymmetry[]] | singRep[DefectGlobalSymmetry[]]]], 
      conjRep[#] === dynkin[#] &]}, 
  Join[{singRep[appropriateGroup[reps[[1,1]]]] -> {White, Line}},Thread[
    reps[True] -> 
     Table[{cols[[i]], Thick, Dashed, Line}, {i, Length[reps[True]]}]],
   Thread[
    reps[False] -> 
     Table[{cols[[Ceiling[i/2]]], Thick, 
       If[OddQ[i], arrow, reverseArrow]}, {i, Length[reps[False]]}]]
   ]
  ];

Format[TreeInvariant[edges_], TraditionalForm] := 
 Module[{
    vertices = DeleteDuplicates@Cases[edges, _Internal | _External, All],
    internalPos,
    externalPos,
    pos},
  externalPos = Association@Thread[Cases[vertices, External[i_] :> (External[i] -> {Cos[(2 Pi (i - 1/2))/Count[vertices, _External]], Sin[(2 Pi (i - 1/2))/Count[vertices, _External]]})]];
  internalPos = Association@Thread[Cases[vertices, Internal[i_] :> (Internal[i] -> If[# == {0,0}, # + {.5 (-1)^i, 0}, #] & @ Mean[Flatten[Table[externalPos[e], {e, Keys[externalPos]}, {n, 1 + 2 Count[edges, {Internal[i], e, _}]}], 1]])]];
  pos = Merge[{externalPos, internalPos}, Mean];
  Graphics[{PointSize[.05], Arrowheads[.1], 
    Join[Table[{Sequence @@ 
        Most[repStyles[allReps[]][
          dynkin[
           If[MatchQ[e[[1]], _External] || MatchQ[e[[2]], _External], 
            e[[3]], conjRep[e[[3]]]]]]], 
       Tooltip[Last[
           repStyles[allReps[]][
            dynkin[
             If[MatchQ[e[[1]], _External] || 
               MatchQ[e[[2]], _External], e[[3]], 
              conjRep[e[[3]]]]]]][{pos[e[[1]]], 
           pos[e[[2]]]}], repName[e[[3]]]]}, {e, edges}], 
     Table[{If[MatchQ[v, _External], Black, Gray], 
       Tooltip[Point[pos[v]], v]}, {v, vertices}]]}, 
   ImageSize -> 150]
  ]

Format[LoopInvariant[edges_], TraditionalForm] := 
  Format[TreeInvariant[edges], TraditionalForm];

Format[SpacetimeStructure[dims_, ls_, derivs_, perm_, q_, i_], TraditionalForm] := 
   Subsuperscript[
      Row[Append[Table[
         If[d[[1]] == "\[PartialD]",
            Superscript["\[PartialD]","("<>ToString[perm[[d[[2]]]]]<>")"],
            Row[{Superscript["\[PartialD]","("<>ToString[perm[[d[[2]]]]]<>")"],"(",ToExpression[d[[1]]][perm],")"}]
         ],
         {d, derivs}
      ], "\[ScriptCapitalS]"]], 
      Row[{perm, ";", If[q =!= None, Sequence @@ {"q = ",q,";"}, Nothing], i}], 
      Row[Riffle[Thread[{dims, ls}], ";"]]
   ];
   
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
     
Format[u[perm : {_,_,_,_}], TraditionalForm] := 
  "U"^#1 "V"^#2 & @@ uvpowers[1, perm];
Format[v[perm : {_,_,_,_}], TraditionalForm] := "U"^#1 "V"^#2 & @@ uvpowers[2, perm];

Format[u[perm : {_,_}], TraditionalForm] := "U";
Format[v[perm : {_,_}], TraditionalForm] := "V";
   
Format[g[fields_, i_, j_], TraditionalForm] := Subsuperscript["g", Row[{i, ",", j}], Row[Table[If[field[[2]] === GlobalRep[name2field[field[[1]]]], field[[1]], Subscript[field[[1]], repName[field[[2]]]]], {field, fields}]]];
     
Format[\[Lambda][fields_, i_, j_], TraditionalForm] := Subsuperscript["\[Lambda]", Row[{i, ",", j}], Row[First /@ fields]];