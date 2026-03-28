(* Wolfram Language package *)

$ArbitraryFunctions = {};
DeclareArbitraryFunction[head_] := 
  If[! MemberQ[$ArbitraryFunctions, head], 
   AppendTo[$ArbitraryFunctions, head];
   Do[DeclareCrossingRule[(head @@ crossRatios[SpacetimeDimension[], $qdefect]) /. cross, (head @@ crossRatios[SpacetimeDimension[], $qdefect]) /. cross], {cross, Select[crosses[SpacetimeDimension[], $qdefect], (crossRatios[SpacetimeDimension[], $qdefect] /. #) =!= crossRatios[SpacetimeDimension[], $qdefect] &]}];
   ];

$SolvedCorrelators = {};
SolvedCorrelators[] := $SolvedCorrelators;

defectSupercharge[3, param_] := QTensor[] + Exp[I param] Contract[TensorProduct[TwoPtGlobalInvariant[QGlobalRep[], QGlobalRep[]], BasisVector[4, "DefectCodimension" -> 3, "Defect" -> True], SigmaTensor[4], WeylChargeConjugationMatrix[4, "Raised" -> True, "Dotted" -> True], QTensor["QBar" -> True]], {{2, 9}, {3, 4}, {6, 7}, {8, 10}}];
defectSupercharge[2, {4, 0}, True] := QTensor["QBar" -> True] - I Contract[TensorProduct[LeviCivita[4, "DefectCodimension" -> 2, "Defect" -> True, "Raised" -> True], RotationGenerators[4, "Weyl" -> True, "Dotted" -> True], QTensor["QBar" -> True]], {{1, 3}, {2, 4}, {6, 8}}]
defectSupercharge[2, {4, 0}, False] := QTensor[] - I Contract[TensorProduct[LeviCivita[4, "DefectCodimension" -> 2, "Defect" -> True, "Raised" -> True], RotationGenerators[4, "Weyl" -> True], QTensor[]], {{1, 3}, {2, 4}, {6, 8}}]
defectSupercharge[2, {2, 2}, True] :=QTensor["QBar"->True] - I Contract[TensorProduct[LeviCivita[4, "Defect" -> True, "DefectCodimension" -> 2, "Raised" -> True], RotationGenerators[4, "Weyl"->True, "Dotted"->True], SU2Breaker["Mixed" -> True], QTensor["QBar"->True]], {{1, 3}, {2, 4}, {6, 10}, {7, 9}}];
defectSupercharge[2, {2, 2}, False] := QTensor[] - I Contract[TensorProduct[LeviCivita[4, "Defect" -> True, "DefectCodimension" -> 2, "Raised" -> True], RotationGenerators[4, "Weyl"->True], SU2Breaker["Mixed" -> True], QTensor[]], {{1, 3}, {2, 4}, {6, 10}, {8, 9}}];
defectSupercharge[1, param_] := QTensor[] + Exp[I param] Contract[TensorProduct[SU2Breaker[], BasisVector[4, "DefectCodimension" -> 1, "Transverse" -> True], SigmaTensor[4], WeylChargeConjugationMatrix[4, "Raised" -> True, "Dotted" -> True], QTensor["QBar" -> True]], {{2, 9}, {3, 4}, {6, 7}, {8, 10}}]
  
Options[WardEquations] = {"QBar" -> False, "Defect" -> False, "UseSUSYRules" -> True};
WardEquations[names : {Except[_Operator]..}, opt : OptionsPattern[]] := WardEquations[name2field /@ names, opt];
WardEquations[fields_, opt: OptionsPattern[]] := WardEquations[fields, opt] = Module[{expr, eqs},
    DeclareAlgebra[];
    expr = Correlator[
     NormalOrder[TensorProduct[
         If[!OptionValue["Defect"],
            QTensor["QBar" -> OptionValue["QBar"]],
            Which[
               $qdefect == 3, 
               defectSupercharge[3, $q3angle],
               $qdefect == 2,
               defectSupercharge[2, $q2type, OptionValue["QBar"]],
               $qdefect == 1,
               defectSupercharge[1, $q1angle]
            ]
         ]
     , Tensor[fields]], 
      "Vacuum" -> True], "Defect" -> OptionValue["Defect"]];
    eqs = If[expr === 0, {},
    DeleteCases[Thread[Flatten[
        ExpansionComponents[
         ExpandCorrelator@expr
             /. If[OptionValue["UseSUSYRules"], Join@@ (SUSYRules /@ $multipletIndices), {}],
          "MonitorProgress" -> True]] == 0], True]
    ];
   
   eqs
   
];

superprimaryQ[f_Operator] := MemberQ[MinimalBy[multipletOf[f], ScalingDimension][[;;,1]], f[[1]]];
superprimaryCorrelatorQ[\[Lambda][ffs_, __]] := AllTrue[ffs, superprimaryQ];
superprimaryCorrelatorQ[g[ffs_, __][__]] := AllTrue[ffs, superprimaryQ];
superprimaryCorrelatorQ[Derivative[__][g[ffs_, __]][__]] := AllTrue[ffs, superprimaryQ];

swSimplify[expr_, assum_ : {}] := With[{swapRules = Thread[{u, v, z, zb} -> {Catalan, EulerGamma, Pi, E}]},
   Collect[
      expr,
      _\[Alpha] | g[__][__] | \[Lambda][__] | (Alternatives @@ $ArbitraryFunctions)[__] | Derivative[__][(Alternatives @@ $ArbitraryFunctions)][__],
      Simplify[Simplify[#, ConformalStructures`Private`crossRatioAssumptions[SpacetimeDimension[], $qdefect]] /. swapRules, assum] /. (Reverse /@ swapRules) &
   ]
];

prepareEquations[eqs_] := DeleteCases[
      monitorProgress[Table[swSimplify[CrossingSimplify[eqs[[i]] //. Normal[First /@ SolvedCorrelators[]]] //. Normal[First /@ SolvedCorrelators[]] (* after CrossingSimplify for derivative relations *)], {i, Length[eqs]}], "Label" -> "Preparing equations", "CurrentDisplayFunction" -> None]
    , True];

Options[SolveWard] = {"QBar" -> False, "Defect" -> False, "UseSUSYRules" -> True, "EquationGroupSize" -> 10};
SolveWard[names : {Except[_Operator]..}, opt : OptionsPattern[]] := SolveWard[name2field /@ names, opt];
SolveWard[fields : {_Operator..}, OptionsPattern[]] := Module[{eqs, vars, eqsReplaced, bm},
   eqs = prepareEquations[WardEquations[fields, "QBar" -> OptionValue["QBar"], "Defect" -> OptionValue["Defect"], "UseSUSYRules" -> OptionValue["UseSUSYRules"]]];
   vars = SortBy[Select[DeleteDuplicates@Cases[eqs, g[__][__] | \[Lambda][__], All], !superprimaryCorrelatorQ[#]&], Total[Table[Boole[IntegerQ[i]], {i, (# /. g[params__][__] :> g[params])[[1]]}]] &];
   eqsReplaced = SortBy[DeleteDuplicates[eqs /. Thread[vars -> Array[\[Alpha], Length[vars]]]], Count[#, _\[Alpha], All] &];
   bm = CoefficientArrays[eqs, vars];
   If[ AllTrue[bm[[1]], # === 0 &],
      Thread[vars -> 0],
      Sort[solveGroups[Partition[eqsReplaced, UpTo[OptionValue["EquationGroupSize"]]], Array[\[Alpha], Length[vars]], "Transformation" -> swSimplify, "TempRules" -> Thread[{u, v, z, zb} -> {Catalan, EulerGamma, Pi, E}], "RemoveRedundant" -> True] /. Thread[Array[\[Alpha], Length[vars]] -> vars]] /. Thread[{Catalan, EulerGamma, Pi, E} -> {u, v, z, zb}]
   ]
];
  
clearRadicals[mat_] := 
  With[{factors = 
     Table[FirstCase[mat[[;; , i]], 
       x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
          Cases[x, Power[y_?NumericQ, 1/2 | -1/2] :> Sqrt[y], 
           All])], {i, Dimensions[mat][[2]]}]},
   {mat . DiagonalMatrix[factors], factors}
   ];

crosses[dim_, _Integer] = {{u -> u, v -> v}};
crosses[dim_, None] /; dim > 2 = {{u -> u, v -> v}, {u -> u/v, v -> 1/v}, {u -> 1/u, v -> v/u}, {u -> v/u, v -> 1/u}, {u -> 1/v, v -> u/v}, {u -> v, v -> u}}; 
crosses[2, None] = {{z -> z, zb -> zb}, {z -> 1/z, zb -> 1/zb}, {z -> 1-z, zb -> 1-zb}, {z -> 1/(1-z), zb -> 1/(1-zb)}, {z -> z/(z-1), zb -> zb/(zb-1)}, {z -> (z-1)/z, zb -> (zb-1)/zb}};

AddSolutions[soln_] := 
  Block[{tmp, unknowns}, 
   With[{uvVersions = Values /@ GroupBy[
         Select[
          Flatten[Table[s /. cross, {s, soln}, {cross, crosses[SpacetimeDimension[], $qdefect]}]], 
          MatchQ[#[[1]], h_[Sequence @@ crossRatios[SpacetimeDimension[], $qdefect]] | Derivative[__][h_][Sequence @@ crossRatios[SpacetimeDimension[], $qdefect]] /; (MemberQ[$ArbitraryFunctions, h] || MatchQ[h, _g])] &
         ] /. HoldPattern[a_ -> b_] :> (Head[a] -> (Function[Evaluate[crossRatios[SpacetimeDimension[], $qdefect]], tmp] /. tmp -> b)), 
      First]
    },
    $SolvedCorrelators = Merge[{$SolvedCorrelators, Association[uvVersions]}, DeleteDuplicates@*Flatten@*List];
    $SolvedCorrelators = Association @@ Table[k -> 
        Table[
           Function[
              Evaluate[crossRatios[SpacetimeDimension[], $qdefect]], 
           	  Evaluate[swSimplify[CrossingSimplify[(val @@ crossRatios[SpacetimeDimension[], $qdefect]) /. (First /@ $SolvedCorrelators)]]]
           ], 
        {val, $SolvedCorrelators[k]}], 
        {k, Keys[$SolvedCorrelators]}];
    ];
    (*unknowns = Sort[DeleteDuplicates@Cases[Values[SolvedCorrelators[]], g[__], All, Heads -> True]];
    $SolvedCorrelators = KeySelect[$SolvedCorrelators, !MemberQ[unknowns, #] &];*)
   ];

DeclareCrossingRule::invalid = "The function `` is not related by crossing to ``[``, ``].";
DeclareCrossingRule[head_[arg1_, arg2_], rhs_] := 
  If[! MemberQ[crosses[SpacetimeDimension[], $qdefect][[;; , ;; , 2]], {arg1, arg2}], 
   Message[DeclareCrossingRule::invalid, head[arg1, arg2], head, Sequence @@ crossRatios[SpacetimeDimension[], $qdefect]],
   DeclareArbitraryFunction[head];
   $crossingRules[head[arg1, arg2]] = Function[{$x, $y}, Evaluate[rhs /. Solve[$x == arg1 && $y == arg2, crossRatios[SpacetimeDimension[], $qdefect]][[1]]]];
  ];

crossIt[expr: head_[__]] := crossIt[expr] = Simplify[expr /. head -> $crossingRules[expr]];
crossIt[expr: Derivative[__][head_][args__]] := crossIt[expr] = Simplify[expr /. head -> $crossingRules[head[args]]];

CrossingSimplify[expr_] := 
  expr /. tterm : (Alternatives @@ 
        Table[f[args__] | 
          Derivative[__][f][args__], {f, $ArbitraryFunctions}]) /; 
     Length[{args}] == 2 && {args} =!= crossRatios[SpacetimeDimension[], $qdefect] :> crossIt[tterm];
     
Options[CrossingRelations] = {"Solved" -> True, "PermuteOperators" -> False};
CrossingRelations[ops_, OptionsPattern[]] := If[OptionValue["PermuteOperators"],
	Module[{alleqs, vars},
		alleqs = Flatten[CrossingRelations[#, "Solved" -> False, "PermuteOperators" -> False] & /@ Permutations[ops]];
		vars = DeleteDuplicates@Cases[eqs, h_[args__] /; (MemberQ[$ArbitraryFunctions, h] || MatchQ[h, _g | _\[Lambda]]) && {args} =!= crossRatios[SpacetimeDimension[], $qdefect], All];
		If[OptionValue["Solved"],
			First@Quiet@Solve[alleqs, vars],
			alleqs	
		]
	],	
	Module[{realops, sortedops, order, sortedp, start, ninds, eqs, vars, sign, STindperm, Rindperm},
	   realops = Replace[ops, s_String :> name2field[s], {1}];
	   start = ExpandCorrelator@Correlator[Tensor[ops]];
	   sortedops = First@Cases[start, g[s_, __][__] :> s, All];
	   order = InversePermutation@PermutationList[FindPermutation[sortedops, realops], 4];
	   ninds = Length[Indices[(List @@ start)[[1]]]];
	   eqs = Thread[DeleteCases[Simplify@Flatten@Table[
	   		sortedp = order[[p[[InversePermutation[order]]]]];
	   		sign = Signature@InversePermutation[p][[Select[Range@Length@ops, ! IntegerQ[ScalingDimension[realops[[#]]]] &]]];
	      	STindperm = crossingPermutationST[Tensor[sortedops], sortedp]; 
	      	Rindperm = crossingPermutationR[Tensor[sortedops], sortedp];
	          Normal@ExpansionComponents[
	             start -
	              sign (TensorPermute[ExpandCorrelator@Correlator[Tensor[ops]],
	                  Join[Rindperm, Length[ops] + STindperm]] /. 
	                {
	                	u[dim_, {i_Integer, j_Integer, k_Integer, l_Integer}] :> u[dim, p[[{i,j,k,l}]]],
	                	v[dim_, {i_Integer, j_Integer, k_Integer, l_Integer}] :> v[dim, p[[{i,j,k,l}]]],
	                	z[2, {i_Integer, j_Integer, k_Integer, l_Integer}] :> z[2, p[[{i,j,k,l}]]],
	                	zb[2, {i_Integer, j_Integer, k_Integer, l_Integer}] :> zb[2, p[[{i,j,k,l}]]],
	                	correlator[dim_, \[CapitalDelta]s_, spins_, derivs_, {i_Integer, j_Integer, k_Integer, l_Integer}, y___] :> correlator[dim, \[CapitalDelta]s, spins, derivs, p[[{i, j, k, l}]], y]
	                })
	             ] /. Normal[First /@ SolvedCorrelators[]],
	          {p, 
	           Select[Permutations[Range[Length[ops]]], ops[[#]] === ops &]}
	          ], 0] == 0];
	   If[!OptionValue["Solved"],
	   	eqs,
	   vars = 
	    DeleteDuplicates@
	     Cases[eqs, 
	      h_[args__] /; (MemberQ[$ArbitraryFunctions, h] || MatchQ[h, _g | _\[Lambda]]) && {args} =!= crossRatios[SpacetimeDimension[], $qdefect], All];
	   First@Solve[eqs, vars]
	   ]
	]
];