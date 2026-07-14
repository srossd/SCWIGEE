(* Wolfram Language package *)

$undottedLetters = {"\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]", "\[Epsilon]", "\[Zeta]", "\[Eta]", "\[Theta]"};
$dottedLetters = {"\!\(\*OverscriptBox[\(\[Alpha]\), \(.\)]\)",
   "\!\(\*OverscriptBox[\(\[Beta]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Gamma]\), \(.\)]\)",
   "\!\(\*OverscriptBox[\(\[Delta]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Epsilon]\), \(.\)]\)",
   "\!\(\*OverscriptBox[\(\[Zeta]\), \(.\)]\)", "\!\(\*OverscriptBox[\(\[Eta]\), \(.\)]\)",
   "\!\(\*OverscriptBox[\(\[Theta]\), \(.\)]\)"};

(* A d = 6 operator carries a + 2b + 3c indices of a SINGLE chirality -- dotted when a < c --
   rather than an undotted block followed by a dotted one. *)
operatorIndexLetters[spin_] :=
   Which[
      SpacetimeDimension[] === 6 && validDynkinQ[spin],
      Take[If[dottedQ6[spin], $dottedLetters, $undottedLetters], boxes6[spin]],
      ListQ[spin],
      Join[$undottedLetters[[;; 2 spin[[1]]]], $dottedLetters[[;; 2 spin[[2]]]]],
      True,
      $undottedLetters[[;; 2 spin]]
   ];

(* How an operator's indices are grouped, as {block length, sign}: +1 for a symmetrized
   block, -1 for an antisymmetrized one.  In d = 2, 3, 4 an operator is symmetric within its
   undotted block and within its dotted one.  In d = 6 the index block is Young-projected, so
   the grouping is read off the diagram: each ROW is symmetric, and a shape that is a single
   COLUMN (the vector [0,1,0], say) is totally antisymmetric -- exactly the symmetry
   SetSymmetries declares, so the notation cannot drift from what the code actually uses. *)
operatorIndexBlocks[spin_] :=
   Which[
      SpacetimeDimension[] === 6 && validDynkinQ[spin],
      With[{lambda = partition6[spin]},
         If[Length[lambda] > 1 && Max[lambda] == 1,
            {{Total[lambda], -1}},
            {#, 1} & /@ lambda
         ]
      ],
      ListQ[spin], {{2 spin[[1]], 1}, {2 spin[[2]], 1}},
      True, {{2 spin, 1}}
   ];

(* (...) for a symmetrized block, [...] for an antisymmetrized one; a lone index gets neither *)
bracketIndices[letters_, blocks_] := Module[{pos = 0},
   Row[Table[
      With[{n = b[[1]], sgn = b[[2]]},
         With[{grp = letters[[pos + 1 ;; pos + n]]},
            pos += n;
            Which[
               n == 0, Nothing,
               n == 1, Row[grp],
               sgn === 1, Row[{"(", Row[grp], ")"}],
               True, Row[{"[", Row[grp], "]"}]
            ]
         ]
      ],
      {b, blocks}]]
];

Format[Operator[name_, rep_, dim_, spin_, y_], TraditionalForm] :=
  With[{indices = operatorIndexLetters[spin], blocks = operatorIndexBlocks[spin]},
     Subsuperscript[If[StringQ[name], ToExpression[name, TraditionalForm, HoldForm], name],
        If[indices == {}, "",
           bracketIndices[ToExpression[#, TraditionalForm, HoldForm] & /@ indices, blocks]],
        repName[rep] /. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :>
  ToString[ToExpression[s, TraditionalForm]]]
  ];

Format[SUSYCoefficient[name_, idx_, opt : OptionsPattern[]], TraditionalForm] := Subscript[
	If[OptionValue[SUSYCoefficient, opt, "QBar"], "\!\(\*OverscriptBox[\(\[ScriptA]\), \(_\)]\)", "\[ScriptA]"],
   Row[{name, ",", idx}]];

Format[SU2BreakingTensor[], TraditionalForm] := "\[Tau]";

Format[Correlator[t_, opt: OptionsPattern[]], TraditionalForm] := 
  Row[{"\[LeftAngleBracket]", t, If[OptionValue[Correlator, "Defect"], "\!\(\*SubscriptBox[\(\[RightAngleBracket]\), \(\[ScriptCapitalD]\)]\)", "\[RightAngleBracket]"]}];
  
Format[GlobalInvariant[i_], TraditionalForm] := Subscript["C", i];

Format[RepWithMultiplicity[rep_, i_], TraditionalForm] := Subscript[repName[rep], i]; 

cols = Join[{Darker@Green, Blue, Black, Orange, Red, Purple, Yellow}, ColorData[90, "ColorList"]];
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
   
Format[g[fields_, i_, j_], TraditionalForm] := Subsuperscript["g", Row[{i, ",", j}], Row[Table[If[field[[2]] === GlobalRep[name2field[field[[1]]]], field[[1]], Subscript[field[[1]], repName[field[[2]]]]], {field, fields}]]];
     
Format[\[Lambda][fields_, i_, j_], TraditionalForm] := Subsuperscript["\[Lambda]", Row[{i, ",", j}], Row[First /@ fields]];