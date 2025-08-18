(* Wolfram Language package *)
         
NontrivialPermutations[t_] := 
  Select[Permutations[Range@Length[Indices[t]]], 
   Indices[t] === Indices[t][[#]] && 
     With[{grps = 
        Cases[TensorSymmetries[t], 
         Symmetric[xs_] | Antisymmetric[xs_] | Cycles[{xs_}] :> xs, 
         All]},
      AllTrue[Table[#[[i]], {i, grps}], OrderedQ]
      ]
    &];

(* D = 3 *)
CProducts[tensor_, dj_] :=
With[{products = Table[TensorProduct[
        TensorPermute[tensor, p],
        TensorProduct @@ Table[ChargeConjugationMatrix[3, "Raised" -> (Sign[dj] == -1)], Abs[dj]]], {p, NontrivialPermutations[tensor]}]},
   With[{pairs = 
      Flatten[{
      	If[dj <= -1, 
       		Transpose[{Position[Indices[products[[1]]], Lowered[DiracSpinor[3]]][[;; -2 dj, 1]], Position[Indices[products[[1]]], Raised[DiracSpinor[3]]][[;; -2 dj, 1]]}], 
         	Nothing
         ]
      }, 1]
    },
    DeleteDuplicates@Flatten@Table[With[{c = Contract[prod, pairs]}, 
       Table[TensorPermute[c, perm], {perm, NontrivialPermutations[c]}]], {prod, products}]
    ]
];

(* D = 4 *)
CProducts[tensor_, {dj1_, dj2_}] := 
With[{products = 
     Table[TensorProduct[
        TensorPermute[tensor, p],
        TensorProduct @@ Table[WeylChargeConjugationMatrix[4, "Raised" -> (Sign[dj1] == -1)], Abs[dj1]],
        TensorProduct @@ Table[WeylChargeConjugationMatrix[4, "Raised" -> (Sign[dj2] == -1), "Dotted" -> True], Abs[dj2]]], {p, NontrivialPermutations[tensor]}]},
   With[{pairs = 
      Flatten[{
      	If[dj1 <= -1, 
       		Transpose[{Position[Indices[products[[1]]], Lowered[WeylSpinor[4]]][[;; -2 dj1, 1]], Position[Indices[products[[1]]], Raised[WeylSpinor[4]]][[;; -2 dj1, 1]]}], 
         	Nothing
         ], 
        If[dj2 <= -1, 
        	Transpose[{Position[Indices[products[[1]]], Lowered[DottedWeylSpinor[4]]][[;; -2 dj2, 1]], Position[Indices[products[[1]]], Raised[DottedWeylSpinor[4]]][[;; -2 dj2, 1]]}], 
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

(* D = 3 *)
OperatorsWithQuantumNumbers[multiplet_, rep_, dim_, j_, y_] :=
    DeleteCases[DeleteDuplicates[(SymmetryReduce /@ Flatten[{
        CProducts[Tensor[{#}], j - Spin[#]] & /@ 
         Select[multiplet, 
             dynkin[rep] == dynkin[GlobalRep[#]] && 
             ScalingDimension[#] == dim && 
             AllTrue[Spin[#] - j, IntegerQ] &
         ],
        CProducts[Tensor[{{"\[PartialD]", Lowered[DiracSpinor[3]], Lowered[DiracSpinor[3]]}, #}], j - 1 - Spin[#]] & /@ 
         Select[multiplet, 
             dynkin[rep] == dynkin[GlobalRep[#]] && 
             ScalingDimension[#] == dim - 1 && 
             AllTrue[Spin[#] - (j - 1), IntegerQ] &
         ]
        }
    ]) /. a_ b_ /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] :> b], 0];

(* D = 4 *)
OperatorsWithQuantumNumbers[multiplet_, rep_, dim_, {j1_, j2_}, y_] :=
    DeleteCases[DeleteDuplicates[(SymmetryReduce /@ Flatten[{
        CProducts[Tensor[{#}], {j1, j2} - Spin[#]] & /@ 
         Select[multiplet, 
             dynkin[rep] == dynkin[GlobalRep[#]] && 
             y == #[[5]] && 
             ScalingDimension[#] == dim && 
             AllTrue[Spin[#] - {j1, j2}, IntegerQ] &
         ],
        CProducts[Tensor[{{"\[PartialD]", Lowered[WeylSpinor[4]], Lowered[DottedWeylSpinor[4]]}, #}], {j1 - 1/2, j2 - 1/2} - Spin[#]] & /@ 
         Select[multiplet, 
             dynkin[rep] == dynkin[GlobalRep[#]] && 
             y == #[[5]] && 
             ScalingDimension[#] == dim - 1 && 
             AllTrue[Spin[#] - {j1 - 1/2, j2 - 1/2}, IntegerQ] &
         ]
        }
    ]) /. a_ b_ /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] :> b], 0];

ToTensor[f_Operator] := Tensor[{{f[[1]], Raised[GlobalIndex[GlobalRep[f]]], 
     Sequence @@ If[ListQ[Spin[f]],
        Join[Table[Lowered[WeylSpinor[SpacetimeDimension[]]], 2 Spin[f][[1]]], Table[Lowered[DottedWeylSpinor[SpacetimeDimension[]]], 2 Spin[f][[2]]]],
        Table[Lowered[DiracSpinor[SpacetimeDimension[]]], 2 Spin[f]]
     ]}}];
     
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
PossibleQActions::noqbar = "No \!\(\*OverscriptBox[\(Q\), \(_\)]\) in odd dimensions";
PossibleQActions[f : Operator[_, rep_, dim_, spin_, y_], opt: OptionsPattern[]] := PossibleQActions[f, opt] = If[TrueQ[OptionValue["QBar"]],
   If[ListQ[spin], qToQBar /@ PossibleQActions[Conjugate[f], "QBar" -> False], Message[PossibleQActions::noqbar]],
   DeleteDuplicates[(SymmetryReduce /@
    Select[
     With[ {reps = ReduceRepProduct[GlobalSymmetry[], {QGlobalRep[], rep}][[;; , 1]]},
        Flatten[
         Table[
             Contract[TensorProduct[ConjugateThreePtGlobalInvariant[{QGlobalRep[], rep}, rep2], op], {{1, 3 + Position[Indices[op], _Raised][[1, 1]]}}], 
            {rep2, reps}, 
            {op, OperatorsWithQuantumNumbers[multipletOf[f], rep2, dim + 1/2, spin + If[ListQ[spin], {1/2, 0}, 1/2], y + 1]}
         ]
        ]
     ], Function[t, With[{si = Position[Indices[t], Lowered[If[ListQ[spin], WeylSpinor[4], DiracSpinor[3]]]][[2;;,1]], dsi = If[ListQ[spin], Position[Indices[t], Lowered[DottedWeylSpinor[4]]][[;;,1]], {}]},
      	Length[Cases[TensorSymmetries[t], {Cycles[{{x1_, x2_}}], -1} /; (MemberQ[si, x1] && MemberQ[si, x2]) || (MemberQ[dsi, x1] && MemberQ[dsi, x2])]] == 0  
     ]     
     ]]
   ) /. a_ b_ /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] :> b ]
];
            
Options[SUSYCoefficient] = {"QBar" -> False};
   
fieldPerm[t_] := With[{inds = 
     Position[
       Indices[t], (Lowered | Raised)[
        WeylSpinor[4] | DottedWeylSpinor[4]]][[If[
         Symbolic[t][[2, 1]] == "\[PartialD]", 3, 1] ;; If[Symbolic[t][[-1,1]] === chargeconj[], If[Symbolic[t][[-2,1]] === chargeconj[], -5, -3], -1], 1]], 
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
     
qToQBar[t_Tensor] := t /. {{"C", Lowered[GlobalIndex[i_]], Raised[GlobalIndex[j_]], Raised[GlobalIndex[k_]]} :> {"C", Raised[GlobalIndex[ConjugateIrrep[$RSymmetry, k]]], Lowered[GlobalIndex[j]], Lowered[GlobalIndex[ConjugateIrrep[$RSymmetry, i]]]},
   {chargeconj[], Raised[WeylSpinor[4]], Raised[WeylSpinor[4]]} -> {chargeconj[], Raised[DottedWeylSpinor[4]], Raised[DottedWeylSpinor[4]]},
   {chargeconj[], Lowered[WeylSpinor[4]], Lowered[WeylSpinor[4]]} -> {chargeconj[], Lowered[DottedWeylSpinor[4]], Lowered[DottedWeylSpinor[4]]},
   {chargeconj[], Raised[DottedWeylSpinor[4]], Raised[DottedWeylSpinor[4]]} -> {chargeconj[], Raised[WeylSpinor[4]], Raised[WeylSpinor[4]]},
   {chargeconj[], Lowered[DottedWeylSpinor[4]], Lowered[DottedWeylSpinor[4]]} -> {chargeconj[], Lowered[WeylSpinor[4]], Lowered[WeylSpinor[4]]},
   {name_, idxs___} /; MemberQ[Flatten[$multiplet /@ $multipletIndices][[;;,1]], name] :> Symbolic[ToTensor[Conjugate[name2field[name]]]][[1]]
};

fingerprint[Contract[t_, pairs_]] := Module[{indpos},
   indpos = Position[Symbolic[t], _Raised | _Lowered];
   Delete[Symbolic[t], indpos[[Flatten[pairs]]]]
];
fingerprint[Contract[TensorPermute[t_, perm_], pairs_]] := Module[{indpos},
   indpos = Position[Symbolic[t], _Raised | _Lowered];
   Delete[Symbolic[t], indpos[[Flatten[pairs] /. x_Integer :> InversePermutation[perm][[x]]]]]
];

Options["QAnsatz"] = {"QBar" -> False, "Symmetrized" -> True};
QAnsatz[f_Operator, opt : OptionsPattern[]] := 
  QAnsatz[f, opt] = If[OptionValue["Symmetrized"],
   If[OptionValue["QBar"],
      QAnsatz[Conjugate[f], "QBar" -> False] /. SUSYCoefficient[name_, idx_, "QBar" -> False] t_ :> SUSYCoefficient[Conjugate[name2field[name]][[1]], idx, "QBar" -> True] qToQBar[t],
	   With[{unsym = QAnsatz[f, "QBar" -> OptionValue["QBar"], "Symmetrized" -> False]},
	      Which[unsym === 0, 0,
	         Head[unsym] =!= Plus, unsym, 
	         True, Module[{terms = Cases[unsym, coeff_SUSYCoefficient t_ :> {coeff, t}], groups, rules},
	          	  groups = Values[#[[;;, 1]] & /@ GroupBy[terms, With[{si = Position[Indices[#[[2]]], Lowered[If[ListQ[Spin[f]], WeylSpinor[4], DiracSpinor[3]]]][[2;;, 1]], dsi = If[ListQ[Spin[f]], Position[Indices[#[[2]]], Lowered[DottedWeylSpinor[4]]][[;;, 1]], {}]},
	          	  	{fingerprint[#[[2]]], First@Sort[Flatten[Table[TensorPermutation[#[[2]]] /. Thread[si -> si[[p]]] /. Thread[dsi -> dsi[[p2]]], {p, Permutations[Range[Length[si]]]}, {p2, Permutations[Range[Length[dsi]]]}], 1]]}  
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

$susyRules = {};

Options[DeclareAlgebra] = {"MonitorProgress" -> True, "MaxDepth" -> 0};
DeclareAlgebra[OptionsPattern[]] := Module[{},
    If[!TrueQ[$algebraDeclared],
		DeclareAnnihilator["Q"]; DeclareAnnihilator["\!\(\*OverscriptBox[\(Q\), \(~\)]\)"];
		
		If[OptionValue["MonitorProgress"],
		    Do[
				monitorProgress[
					Do[
						If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> False];
						If[EvenQ[SpacetimeDimension[]], If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QBarTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> True]];,
						{op, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, 2 OptionValue["MaxDepth"] - 1}, {k, 0, 2 OptionValue["MaxDepth"] - 1 - j}]]]}
					],
					"Label" -> "Determining SUSY ansatzes ("<>$multipletName[i]<>")",
					"CurrentDisplayFunction" -> None
				],
			{i, $multipletIndices}],
			Do[
				If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> False];
				If[EvenQ[SpacetimeDimension[]], If[IntegerQ[ScalingDimension[op]], Commutator, Anticommutator][$QBarTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> True]];,
				{i, $multipletIndices},
				{op, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, 2 OptionValue["MaxDepth"] - 1}, {k, 0, 2 OptionValue["MaxDepth"] - 1 - j}]]]}
			]
		];
		
		Commutator[$QTensor, Tensor[{{"\[Delta]", ___}}]] = 0;
		Commutator[$QBarTensor, Tensor[{{"\[Delta]", ___}}]] = 0;
		Commutator[$QTensor, Tensor[{{\[Sigma]LowerTensor[_], ___}}]] = 0;
		Commutator[$QBarTensor, Tensor[{{\[Sigma]LowerTensor[_], ___}}]] = 0;
		Commutator[$QTensor, Tensor[{{"C", ___}}]] = 0;
		Commutator[$QBarTensor, Tensor[{{"C", ___}}]] = 0;
		Commutator[$QTensor, Tensor[{{chargeconj[], ___}}]] = 0;
		Commutator[$QBarTensor, Tensor[{{chargeconj[], ___}}]] = 0;
		Commutator[$QTensor, Tensor[{{"\[PartialD]", ___}}]] = 0;
		Commutator[$QBarTensor, Tensor[{{"\[PartialD]", ___}}]] = 0;
		Commutator[$QTensor, Tensor[{{SU2BreakingTensor[], ___}}]] = 0;
		Commutator[$QBarTensor, Tensor[{{SU2BreakingTensor[], ___}}]] = 0;
    ];
	
	$algebraDeclared = True;
];

quadraticZero[op_] := quadraticZero[op] = Which[
   SpacetimeDimension[] == 4, (* {Q, Qb} = 2P *)
   	NormalOrder[TensorProduct[$QTensor, $QBarTensor, Tensor[{op}]] + TensorProduct[$QBarTensor, $QTensor, Tensor[{op}]], "Vacuum" -> True] + 2 I TensorProduct[Kronecker[GlobalIndex[QGlobalRep[]]], Tensor[{{"\[PartialD]", Lowered[WeylSpinor[4]], Lowered[DottedWeylSpinor[4]]}}], Tensor[{op}]],
   SpacetimeDimension[] == 3, (* {Q, Q} = P *) (* hack to dodge SwapIn bug *)
    ((# + (Expand[#] /. a_ b_ /; ! FreeQ[b, Alternatives @@  TensorTools`Private`$TensorHeads] :> (a TensorPermute[b, 
     PermutationList[
      Cycles[{InversePermutation[
          TensorPermutation[
           b]][[Position[Indices[b], Lowered[DiracSpinor[3]]][[;; 2, 
            1]]]], InversePermutation[
          TensorPermutation[
           b]][[Position[Indices[b], 
            Raised[GlobalIndex[QGlobalRep[]]]][[;; 2, 1]]]]}], 
      Length[Indices[b]]]]))) & @ NormalOrder[TensorProduct[$QTensor, $QTensor, Tensor[{op}]], "Vacuum" -> True]) + 2 I TensorProduct[TwoPtGlobalInvariant[QGlobalRep[], QGlobalRep[]], Tensor[{{"\[PartialD]", Lowered[DiracSpinor[3]], Lowered[DiracSpinor[3]]}}], Tensor[{op}]]
];

Options[opGroup] = {"Conjugate" -> False};
opGroup[m_, i_, j_, OptionsPattern[]] := With[{bottom = First@MinimalBy[If[$multipletSC[m], Multiplet[m], Multiplet[m][[If[OptionValue["Conjugate"], 2, 1]]]], ScalingDimension]},
   Select[If[$multipletSC[m], Multiplet[m], Multiplet[m][[If[OptionValue["Conjugate"], 2, 1]]]], ScalingDimension[#] - ScalingDimension[bottom] == (i + j)/2 && Last[#] - Last[bottom] == i - j & ]
];

groupOf[f_Operator] := Module[{mult, conj, bottom, dimdiff, ydiff},
   mult = whichMultiplet[f];
   conj = (! $multipletSC[mult] && MemberQ[Multiplet[mult][[2]], f]);
   bottom = First@opGroup[mult, 0, 0, "Conjugate" -> conj];
   dimdiff = ScalingDimension[f] - ScalingDimension[bottom];
   ydiff = Last[f] - Last[bottom];
   {dimdiff + ydiff/2, dimdiff - ydiff/2, conj}
];
   
Options[linearEquations] = {"MaxDepth" -> 0};
linearEquations[i_, opt: OptionsPattern[]] := linearEquations[i, opt] = DeleteCases[Reduce /@ Thread[Flatten @ monitorProgress[
		Table[
		   ExpansionComponents@ExpandCorrelator@Correlator@NormalOrder[TensorProduct[QTensor["QBar" -> If[EvenQ[SpacetimeDimension[]], qbar, False]], Tensor[{op1, op2}]], "Vacuum" -> True],
		   {op1, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"] - 1}, {k, 0, OptionValue["MaxDepth"] - 1 - j}]]]}, 
		   {qbar, {False, True}}, 
		   {op2, With[{grp = groupOf[op1]}, opGroup[i, Sequence @@ If[SpacetimeDimension[] == 4, Reverse, Identity][grp[[;;2]] + If[qbar, {0, 1}, {1, 0}]], "Conjugate" -> ! grp[[3]]]]}
		],
		"Label" -> "Generating linear equations",
		"CurrentDisplayFunction" -> None
	] == 0], True];
	
Options[quadraticEquations] = {"MaxDepth" -> 0};
quadraticEquations[i_, opt: OptionsPattern[]] := quadraticEquations[i, opt] = DeleteDuplicates@DeleteCases[Simplify[Reduce[#], _SUSYCoefficient != 0] & /@ Thread[Flatten @ monitorProgress[
		Table[
		   ExpansionComponents@ExpandCorrelator@Correlator[TensorProduct[quadraticZero[op1], Tensor[{op2}]]],
		   {op1, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"] - 1}, {k, 0, OptionValue["MaxDepth"] - 1 - j}]]]}, 
		   {shift, {{-1,-1},{0,0},{1,1}}}, 
		   {op2, With[{grp = groupOf[op1]}, opGroup[i, Sequence @@ If[SpacetimeDimension[] == 4, Reverse, Identity][grp[[;;2]] + shift], "Conjugate" -> ! grp[[3]]]]}
		],
		"Label" -> "Generating quadratic equations",
		"CurrentDisplayFunction" -> None
	] == 0], True];

Options[SUSYRules] = {"EquationGroupSize" -> 10, "MaxDepth" -> 0};
SUSYRules::nosol = "No consistent SUSY transformations could be found for the multiplet '``'.";
SUSYRules[i_, opt: OptionsPattern[]] := SUSYRules[i] = SUSYRules[i, opt] = Module[{linears, linsol, quadratics, norms, normsol, vars, rvars, quadgroups, quadsol, partialsol},
	DeclareAlgebra["MaxDepth" -> OptionValue["MaxDepth"]];
	
	linears = linearEquations[i, "MaxDepth" -> OptionValue["MaxDepth"]];
	linsol = With[{tmp = Solve[linears]},
	   If[tmp === {}, Missing[],
	      First[tmp]
	   ]
	];
	If[MissingQ[linsol], Message[SUSYRules::nosol, $multipletName[i]]; Return[{}]];
	
	quadratics = quadraticEquations[i, "MaxDepth" -> OptionValue["MaxDepth"]];
	
	quadgroups = Partition[DeleteCases[DeleteDuplicates[Simplify[#, SUSYCoefficient[__] != 0] & /@ (quadratics /. linsol)], True], UpTo[OptionValue["EquationGroupSize"]]];
	
	vars = DeleteDuplicates@Cases[quadgroups, _SUSYCoefficient, All];
	
	Off[Solve::ifun];
	quadsol = Check[solveGroups[quadgroups, vars, "Assumptions" -> Thread[vars != 0]], Missing[], solveGroups::nosol];
	On[Solve::ifun];
	If[MissingQ[quadsol], Message[SUSYRules::nosol, $multipletName[i]]; Return[{}]];
	
	partialsol = Join[linsol /. quadsol, quadsol];
	
	rvars = DeleteDuplicates[Cases[partialsol[[;;, 2]], _SUSYCoefficient, All]];	
	If[EvenQ[SpacetimeDimension[]],
		norms = DeleteCases[Simplify[Cases[partialsol[[;; , 1]], 
		   s : SUSYCoefficient[a_, b_, "QBar" -> qb_] :> (Abs[s] == Abs[SUSYCoefficient[Conjugate[name2field[a]][[1]], b, "QBar" -> !qb]]), 
		   All] /. partialsol /. Thread[rvars -> Array[\[Alpha], Length[rvars]]], \[Alpha][_] > 0], 
		x_ /; ! FreeQ[x, SUSYCoefficient]];
		
		normsol = Last[Solve[norms]],
		normsol = {}
	];
	
	Thread[Join[partialsol[[;;, 1]], rvars] -> (Join[partialsol[[;;, 2]], rvars] /. Thread[rvars -> Array[\[Alpha], Length[rvars]]] /. normsol /. Thread[Array[\[Alpha], Length[rvars]] -> rvars])]
];

Options[susyTable] = {"Solved" -> True, "MaxDepth" -> 0};
susyTable[ops_, OptionsPattern[]] := With[{g = Grid[Table[{TensorProduct[$QTensor, ToTensor[op]], 
    QAnsatz[op, "QBar" -> False] /. If[TrueQ[OptionValue["Solved"]], If[OptionValue["MaxDepth"] == 0, SUSYRules[whichMultiplet[ops[[1]]]], SUSYRules[whichMultiplet[ops[[1]]], "MaxDepth" -> OptionValue["MaxDepth"]]], {}], 
    Sequence @@ If[EvenQ[SpacetimeDimension[]], {
       TensorProduct[$QBarTensor, ToTensor[Conjugate[op]]], 
       QAnsatz[Conjugate@op, "QBar" -> True] /.  If[TrueQ[OptionValue["Solved"]], If[OptionValue["MaxDepth"] == 0, SUSYRules[whichMultiplet[ops[[1]]]], SUSYRules[whichMultiplet[ops[[1]]], "MaxDepth" -> OptionValue["MaxDepth"]]], {}]
    }, {}
    ]}, {op, ops}], 
  Dividers -> All, Alignment -> Left]},
  	Column[{g // TraditionalForm,
 		Button[Style["Copy as TeX", 14], CopyToClipboard[ToString[TeXForm[g]]], ImageSize -> 200]
  	}, Alignment -> Center 
	]
  ];

Options[DisplaySUSYVariations] = {"Solved" -> True, "MaxDepth" -> 0};
DisplaySUSYVariations[opt:OptionsPattern[]] := (DeclareAlgebra[]; TabView[
   Table[$multipletName[i] -> susyTable[If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"]}, {k, 0, OptionValue["MaxDepth"] - j}]]],opt], {i, $multipletIndices}], 
   Alignment -> Center, ImageSize -> Automatic]);