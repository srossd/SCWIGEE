(* Wolfram Language package *)

$ArbitraryFunctions = {};
DeclareArbitraryFunction[head_] := 
  If[! MemberQ[$ArbitraryFunctions, head], 
   AppendTo[$ArbitraryFunctions, head]];

$SolvedCorrelators = {};
SolvedCorrelators[] := $SolvedCorrelators;

defectSupercharge[3, param_] := QTensor[] + Exp[I param] Contract[TensorProduct[TwoPtGlobalInvariant[QGlobalRep[], QGlobalRep[]], \[Sigma]LowerSingle[4], \[Epsilon]UpperDot, QTensor["QBar" -> True]], {{1, 7}, {4, 5}, {6, 8}}];
defectSupercharge[2, {4, 0}, True] := QTensor["QBar" -> True] - I Contract[TensorProduct[\[Epsilon]Defect[2], \[Sigma]CommUpperDot, \[Epsilon]UpperDot, QTensor["QBar" -> True]], {{1, 3}, {2, 4}, {6, 7}, {8, 10}}];
defectSupercharge[2, {4, 0}, False] := QTensor[] - I Contract[TensorProduct[\[Epsilon]Defect[2], \[Sigma]CommUpper, \[Epsilon]Upper, QTensor[]], {{1, 3}, {2, 4}, {6, 7}, {8, 10}}];
defectSupercharge[2, {2, 2}, True] := QTensor["QBar" -> True] - I Contract[TensorProduct[\[Epsilon]Defect[2], \[Sigma]CommUpperDot, \[Epsilon]UpperDot, SU2Breaker["Mixed" -> True], QTensor["QBar" -> True]], {{1, 3}, {2, 4}, {6, 7}, {8, 12}, {9, 11}}];
defectSupercharge[2, {2, 2}, False] := QTensor[] - I Contract[TensorProduct[\[Epsilon]Defect[2], \[Sigma]CommUpper, \[Epsilon]Upper, SU2Breaker["Mixed" -> True], QTensor[]], {{1, 3}, {2, 4}, {6, 7}, {8, 12}, {10, 11}}];
defectSupercharge[1, param_] := QTensor[] + Exp[I param] Contract[TensorProduct[SU2Breaker[], \[Sigma]LowerSingle[1], \[Epsilon]UpperDot, QTensor["QBar"->True]], {{1, 7}, {4, 5}, {6, 8}}];
  
Options[WardEquations] = {"QBar" -> False, "Defect" -> False, "UseSUSYRules" -> True};
WardEquations[names : {Except[_Operator]..}, opt : OptionsPattern[]] := WardEquations[name2field /@ (ToString[ToExpression[#], TraditionalForm] & /@ names), opt];
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
superprimaryCorrelatorQ[g[ffs_, __][__]] := AllTrue[ffs, superprimaryQ];
superprimaryCorrelatorQ[Derivative[__][g[ffs_, __]][__]] := AllTrue[ffs, superprimaryQ];

swSimplify[expr_, assum_ : {}] := With[{swapRules = Thread[{u, v} -> {Catalan, EulerGamma}]},
   Collect[
      expr,
      _\[Alpha] | g[__][__] | \[Lambda][__] | (Alternatives @@ $ArbitraryFunctions)[__] | Derivative[__][(Alternatives @@ $ArbitraryFunctions)][__],
      Simplify[# /. swapRules, assum] /. (Reverse /@ swapRules) &
   ]
]; 

Options[SolveWard] = {"QBar" -> False, "Defect" -> False, "UseSUSYRules" -> True, "EquationGroupSize" -> 10};
SolveWard[names : {Except[_Operator]..}, opt : OptionsPattern[]] :=
   SolveWard[name2field /@ (ToString[ToExpression[#], TraditionalForm] & /@ names), opt];
SolveWard[fields : {_Operator..}, OptionsPattern[]] := Module[{eqs, vars, eqsReplaced, bm},
   eqs = DeleteCases[
      swSimplify[
         CrossingSimplify[WardEquations[fields, "QBar" -> OptionValue["QBar"], "Defect" -> OptionValue["Defect"], "UseSUSYRules" -> OptionValue["UseSUSYRules"]] //. Normal[First /@ SolvedCorrelators[]]]
      ]
    , True];
   vars = SortBy[Select[DeleteDuplicates@Cases[eqs, g[__][__], All], !superprimaryCorrelatorQ[#]&], Total[Table[Boole[IntegerQ[i]], {i, #[[0,1]]}]] &];
   eqsReplaced = eqs /. Thread[vars -> Array[\[Alpha], Length[vars]]];
   bm = CoefficientArrays[eqs, vars];
   If[ AllTrue[bm[[1]], # === 0 &],
      Thread[vars -> 0],
      Sort[solveGroups[Partition[SortBy[eqsReplaced, ByteCount], UpTo[OptionValue["EquationGroupSize"]]], Array[\[Alpha], Length[vars]], "Transformation" -> swSimplify, "TempRules" -> Thread[{u, v} -> {Catalan, EulerGamma}]] /. Thread[Array[\[Alpha], Length[vars]] -> vars]]
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

crosses[_Integer] = {{u -> u, v -> v}};
crosses[None] = {{u -> u, v -> v}, {u -> u/v, v -> 1/v}, {u -> 1/u, 
    v -> v/u}, {u -> v/u, v -> 1/u}, {u -> 1/v, v -> u/v}, {u -> v, 
    v -> u}}; 

AddSolutions[soln_] := 
  Block[{tmp}, 
   With[{uvVersions = Values /@ GroupBy[
         Select[
          Flatten[Table[s /. cross, {s, soln}, {cross, crosses[$qdefect]}]], 
          MatchQ[#[[1]], g[__][Sequence @@ crossRatios[$qdefect]] | Derivative[__][g[__]][Sequence @@ crossRatios[$qdefect]]] &
         ] /. HoldPattern[a_ -> b_] :> (Head[a] -> (Function[Evaluate[crossRatios[$qdefect]], tmp] /. tmp -> b)), 
      First]
    },
    $SolvedCorrelators = Merge[{$SolvedCorrelators, Association[uvVersions]}, DeleteDuplicates@*Flatten@*List];
    $SolvedCorrelators = Association @@ Table[k -> 
        Table[
           Function[
              Evaluate[crossRatios[$qdefect]], 
           	  Evaluate[swSimplify[CrossingSimplify[(val @@ crossRatios[$qdefect]) /. (First /@ $SolvedCorrelators)]]]
           ], 
        {val, $SolvedCorrelators[k]}], 
        {k, Keys[$SolvedCorrelators]}];
    ]
   ];

DeclareCrossingRule::unknown = "The function `` has not been declared using DeclareArbitraryFunction.";
DeclareCrossingRule::invalid = "The function `` is not related by crossing to ``[u, v].";
DeclareCrossingRule[head_[arg1_, arg2_], rhs_] := 
  If[! MemberQ[crosses[$qdefect][[;; , ;; , 2]], {arg1, arg2}], 
   Message[DeclareCrossingRule::invalid, head[arg1, arg2], head],
   If[! MemberQ[$ArbitraryFunctions, head], 
    Message[DeclareCrossingRule::unknown, head],
    $crossingRules[head[arg1, arg2]] = Function[{$x, $y}, Evaluate[rhs /. Solve[$x == arg1 && $y == arg2, {u,v}][[1]]]]
   ]
  ];

crossIt[expr: head_[__]] := crossIt[expr] = Simplify[expr /. head -> $crossingRules[expr]];
crossIt[expr: Derivative[__][head_][args__]] := crossIt[expr] = Simplify[expr /. head -> $crossingRules[head[args]]];

CrossingSimplify[expr_] := 
  expr /. tterm : (Alternatives @@ 
        Table[f[args__] | 
          Derivative[__][f][args__], {f, $ArbitraryFunctions}]) /; 
     Length[{args}] == 2 && {args} =!= crossRatios[$qdefect] :> crossIt[tterm];