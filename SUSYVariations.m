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

(* D = 2, 4 *)
CProducts[tensor_, {dj1_, dj2_}] := 
With[{products = 
     Table[TensorProduct[
        TensorPermute[tensor, p],
        TensorProduct @@ Table[WeylChargeConjugationMatrix[SpacetimeDimension[], "Raised" -> (Sign[dj1] == -1)], Abs[dj1]],
        TensorProduct @@ Table[WeylChargeConjugationMatrix[SpacetimeDimension[], "Raised" -> (Sign[dj2] == -1), "Dotted" -> True], Abs[dj2]]], {p, NontrivialPermutations[tensor]}]},
   With[{pairs = 
      Flatten[{
      	If[dj1 <= -1, 
       		Transpose[{Position[Indices[products[[1]]], Lowered[WeylSpinor[SpacetimeDimension[]]]][[;; -2 dj1, 1]], Position[Indices[products[[1]]], Raised[WeylSpinor[SpacetimeDimension[]]]][[;; -2 dj1, 1]]}], 
         	Nothing
         ], 
        If[dj2 <= -1, 
        	Transpose[{Position[Indices[products[[1]]], Lowered[DottedWeylSpinor[SpacetimeDimension[]]]][[;; -2 dj2, 1]], Position[Indices[products[[1]]], Raised[DottedWeylSpinor[SpacetimeDimension[]]]][[;; -2 dj2, 1]]}], 
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

(* D = 2, 4 *)
OperatorsWithQuantumNumbers[multiplet_, rep_, dim_, {j1_, j2_}, y_] :=
    DeleteCases[DeleteDuplicates[(SymmetryReduce /@ Flatten[{
        CProducts[Tensor[{#}], {j1, j2} - Spin[#]] & /@ 
         Select[multiplet, 
             dynkin[rep] == dynkin[GlobalRep[#]] && 
             y == #[[5]] && 
             ScalingDimension[#] == dim && 
             AllTrue[Spin[#] - {j1, j2}, IntegerQ] &
         ],
        CProducts[Tensor[{{"\[PartialD]", Lowered[WeylSpinor[SpacetimeDimension[]]], Lowered[If[SpacetimeDimension[] == 2, WeylSpinor, DottedWeylSpinor][SpacetimeDimension[]]]}, #}], {j1 - 1/2, j2 - 1/2} - Spin[#]] & /@ 
         Select[multiplet, 
             dynkin[rep] == dynkin[GlobalRep[#]] && 
             y == If[SpacetimeDimension[] == 2, 2, 0] + #[[5]] && 
             ScalingDimension[#] == dim - 1 && 
             AllTrue[Spin[#] - If[SpacetimeDimension[] == 2, {j1 - 1, j2}, {j1 - 1/2, j2 - 1/2}], IntegerQ] &
         ]
        }
    ]) /. a_ b_ /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] :> b], 0];

ToTensor[f_Operator] := Tensor[{{f[[1]], Raised[GlobalIndex[GlobalRep[f]]],
     Sequence @@ spinorIndicesOf[f]}}];
     
BuildTensor[{name_String, idxs___}] /; sixDOperatorQ[name] := sixDOperatorComponents[name, {idxs}];

(* The canonical component of a tensor with declared symmetry: the orbit of an index tuple
   under the symmetry group, represented by its smallest member with the matching sign.

   The sign matters.  A tuple fixed by a group element of sign -1 has a vanishing
   component, and the representative of an orbit has to be chosen by the INDEX TUPLE, not
   by sorting the resulting expressions -- sorting expressions happens to work for an
   all-plus symmetry, but for an antisymmetric tensor it yields nonsense (the diagonal
   comes out as -Component[d][1,1] rather than 0).  Nothing before d = 6 declared a
   signed symmetry through this path: the derivative is symmetric in d = 2, 3, carries two
   different index types in d = 4, and is antisymmetric only in d = 6, where the vector is
   Lambda^2 of the 4. *)
canonicalComponent[name_, idx_List, perms_] := Module[{n = Length[idx], orbit},
   orbit = Table[{idx[[PermutationList[p[[1]], n]]], p[[2]]}, {p, perms}];
   If[AnyTrue[orbit, #[[1]] === idx && #[[2]] == -1 &],
      0,
      With[{best = First@SortBy[orbit, First]}, best[[2]] (Component[name] @@ best[[1]])]
   ]
];

BuildTensor[{name_String, idxs___}] :=
   With[{perms = SymmetryPermutations[TensorSymmetries[name], "Minimal" -> False]},
    SparseArray[
     Flatten@Table[($i /@ Range@Length[{idxs}]) :>
       Evaluate[canonicalComponent[name, $i /@ Range@Length[{idxs}], perms]],
       Evaluate[
        Sequence @@
         Table[{$i[ii], IndexData[{idxs}[[ii, 1]]][[1]]}, {ii,
           Length[{idxs}]}]]],
     Table[IndexData[{idxs}[[ii, 1]]][[1]], {ii, Length[{idxs}]}]]
   ];
   
symmetryInconsistent[gen1_, gen2_] := Max[Length /@ Values[GroupBy[DeleteDuplicates@Join[SymmetryPermutations[gen1], SymmetryPermutations[gen2]], First]]] > 1;

(* [Q_A^i, O] in d = 6.  The ansatz index order is [Q's R index, O's R index, Q's spinor
   index, O's spinor indices], so O's block starts at slot 4 and is where O's Young
   symmetry has to be imposed.  Note the target is fixed by INDEX CONTENT, not by a target
   spin: Q's action is set-valued in d = 6 (rep (x) [1,0,0] is a sum of reps, e.g.
   [1,0,0] (x) [1,0,0] = [2,0,0] + [0,1,0]), so there is no single spin to aim at. *)
ansatzTensor6[rep_, rep2_, op_] := Contract[
   TensorProduct[ConjugateThreePtGlobalInvariant[{QGlobalRep[], rep}, rep2], op],
   {{1, 3 + Position[Indices[op], _Raised][[1, 1]]}}
];

possibleQActions6[f : Operator[_, rep_, dim_, spin_, y_]] := Module[
   {targetInds, n, terms, pairs},
   targetInds = Prepend[spinorIndicesOf[f], lowW6];
   n = Length[targetInds];
   (* [Q_A, O] inherits O's Young symmetry in O's slots, so each structure is projected
      there.  Permuting O's slots carries one member of a family to another, so the
      projection is taken by re-keying the assignment rather than by wrapping the tensor in
      a further TensorPermute (which TensorTools composes incorrectly -- see SixD.m).
      Transpose[T_asg, rho] === T_(asg[[InversePermutation[rho]]]). *)
   terms = youngSlotTerms6[partition6[spin], n];
   (* Each family member's components are evaluated once and specialised to numbers; the
      projected structures are then linear combinations of those vectors, so no projected
      sum ever has to be handed back to Components.  Without this an operator with four
      spinor indices (the stress tensor) puts 120 assignments times the symmetrizer's terms
      through a five-index Components evaluation apiece. *)
   pairs = Flatten[Table[
      Module[{asgs, tensors, vecs, slot},
         asgs = familyAssignments6[family];
         tensors = Table[ansatzTensor6[rep, rep2, familyLookup6[family, a]], {a, asgs}];
         vecs = Table[numericVector6[structureComponents6[t]], {t, tensors}];
         (* the member reached by permuting O's slots according to a symmetrizer term *)
         slot[asg_, tm_] := FirstPosition[asgs, asg[[InversePermutation[tm[[1]]]]]][[1]];
         Table[
            {Total[Function[tm, tm[[2]] tensors[[slot[asg, tm]]]] /@ terms],
             Total[Function[tm, tm[[2]] vecs[[slot[asg, tm]]]] /@ terms]},
            {asg, asgs}]
      ],
      {rep2, ReduceRepProduct[GlobalSymmetry[], {QGlobalRep[], rep}][[;; , 1]]},
      {family, operatorFamilies6[multipletOf[f], rep2, dim + 1/2, targetInds]}
   ], 2];
   independentByVectors6[pairs[[;; , 1]], pairs[[;; , 2]]]
];

Options[PossibleQActions] = {"QBar" -> False};
PossibleQActions::noqbar = "There is no \!\(\*OverscriptBox[\(Q\), \(_\)]\) in d = ``.";
PossibleQActions[f : Operator[_, rep_, dim_, spin_, y_], opt: OptionsPattern[]] := PossibleQActions[f, opt] = Which[
   TrueQ[OptionValue["QBar"]] && ! hasQBar[],
      Message[PossibleQActions::noqbar, SpacetimeDimension[]]; {},
   TrueQ[OptionValue["QBar"]],
      qToQBar @ PossibleQActions[Conjugate[f], "QBar" -> False],
   SpacetimeDimension[] == 6,
      possibleQActions6[f],
   True,
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
     ], Function[t, With[{si = Position[Indices[t], Lowered[If[ListQ[spin], WeylSpinor[SpacetimeDimension[]], DiracSpinor[3]]]][[2;;,1]], dsi = If[ListQ[spin], Position[Indices[t], Lowered[DottedWeylSpinor[SpacetimeDimension[]]]][[;;,1]], {}]},
      	Length[Cases[TensorSymmetries[t], {Cycles[{{x1_, x2_}}], -1} /; (MemberQ[si, x1] && MemberQ[si, x2]) || (MemberQ[dsi, x1] && MemberQ[dsi, x2])]] == 0  
     ]     
     ]]
   ) /. a_ b_ /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] :> b ]
];
            
Options[SUSYCoefficient] = {"QBar" -> False};
   
fieldPerm[t_] := With[{inds = 
     Position[
       Indices[t], (Lowered | Raised)[
        WeylSpinor[SpacetimeDimension[]] | DottedWeylSpinor[SpacetimeDimension[]]]][[If[
         Symbolic[t][[2, 1]] == "\[PartialD]", 3, 1] ;; If[Symbolic[t][[-1,1]] === chargeconj[], If[Symbolic[t][[-2,1]] === chargeconj[], -5, -3], -1], 1]], 
    f = name2field[If[Symbolic[t][[2, 1]] == "\[PartialD]", Symbolic[t][[3, 1]], 
         Symbolic[t][[2, 1]]]]}, 
	PermutationPower[Cycles[{inds}], 2 Spin[f][[1]]]
];
(* specific to QAnsatz expressions *)
qToQBar[expr_] := 
  Replace[expr, {SUSYCoefficient[name_, idx_, "QBar" -> False] :> 
     SUSYCoefficient[Conjugate[name2field[name]][[1]], idx, 
      "QBar" -> True], 
    Contract[
      t2 : Tensor[{{"C", Lowered[r1_], Raised[r2_], Raised[r3_]}, 
         op : {s_, __}, 
         Optional[c : {chargeconj[], __}, Nothing]}], {{1, 4}, 
       rest___}] :> 
     Module[{fp2 = PermutationList[fieldPerm[t2], Length[Indices[t2]]]},
       Contract[
       Tensor[{{"C", Raised[conjIndex[r3]], Lowered[r2], 
          Lowered[conjIndex[r1]]}, 
         ToTensor[Conjugate@name2field[s]][[1, 1]], 
         c /. {WeylSpinor -> DottedWeylSpinor, 
           DottedWeylSpinor -> WeylSpinor}}], {{3, 4}, 
        Sequence @@ ({rest} /. i_Integer :> fp2[[i]])}]], 
    Contract[
      t2 : Tensor[{{"C", Lowered[r1_], Raised[r2_], Raised[r3_]}, 
         deriv : {"\[PartialD]", __}, op : {s_, __}, 
         Optional[c : {chargeconj[], __}, Nothing]}], {{1, 6}, 
       rest___}] :> 
     Module[{fp2 = 
        PermutationList[
         PermutationProduct[Cycles[{{4, 5}}], fieldPerm[t2]], 
         Length[Indices[t2]]]}, 
      Contract[
       Tensor[{{"C", Raised[conjIndex[r3]], Lowered[r2], 
          Lowered[conjIndex[r1]]}, deriv, 
         ToTensor[Conjugate@name2field[s]][[1, 1]], 
         c /. {WeylSpinor -> DottedWeylSpinor, 
           DottedWeylSpinor -> WeylSpinor}}], {{3, 6}, 
        Sequence @@ ({rest} /. i_Integer :> fp2[[i]])}]], 
    TensorPermute[
      t : Contract[
        t2 : Tensor[{{"C", Lowered[r1_], Raised[r2_], Raised[r3_]}, 
           op : {s_, __}, 
           Optional[c : {chargeconj[], __}, Nothing]}], {{1, 4}, 
         rest___}], perm_] :> 
     Module[{fp = fieldPerm[t], 
       fp2 = PermutationList[fieldPerm[t2], Length[Indices[t2]]]}, 
      TensorPermute[
       Contract[
        Tensor[{{"C", Raised[conjIndex[r3]], Lowered[r2], 
           Lowered[conjIndex[r1]]}, 
          ToTensor[Conjugate@name2field[s]][[1, 1]], 
          c /. {WeylSpinor -> DottedWeylSpinor, 
            DottedWeylSpinor -> WeylSpinor}}], {{3, 4}, 
         Sequence @@ ({rest} /. i_Integer :> fp2[[i]])}], 
       PermutationList[
        PermutationProduct[fp, perm, InversePermutation[fp]], 
        Length[perm]]]], 
    TensorPermute[
      t : Contract[
        t2 : Tensor[{{"C", Lowered[r1_], Raised[r2_], Raised[r3_]}, 
           deriv : {"\[PartialD]", __}, op : {s_, __}, 
           Optional[c : {chargeconj[], __}, Nothing]}], {{1, 6}, 
         rest___}], perm_] :> 
     Module[{fp = fieldPerm[t], 
       fp2 = PermutationList[
         PermutationProduct[Cycles[{{4, 5}}], fieldPerm[t2]], 
         Length[Indices[t2]]]}, 
      TensorPermute[
       Contract[
        Tensor[{{"C", Raised[conjIndex[r3]], Lowered[r2], 
           Lowered[conjIndex[r1]]}, deriv, 
          ToTensor[Conjugate@name2field[s]][[1, 1]], 
          c /. {WeylSpinor -> DottedWeylSpinor, 
            DottedWeylSpinor -> WeylSpinor}}], {{3, 6}, 
         Sequence @@ ({rest} /. i_Integer :> fp2[[i]])}], 
       PermutationList[
        PermutationProduct[fp, Cycles[{{3, 4}}], perm, Cycles[{{3, 4}}],
          InversePermutation[fp]], Length[perm]]]]}, 
   If[Head[expr] === Plus, {2}, {1}]];
(*qToQBar[Contract[t_, pairs_]] := With[{fp = fieldPerm[t]},
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
};*)

fingerprint[Contract[t_, pairs_]] := Module[{indpos},
   indpos = Position[Symbolic[t], _Raised | _Lowered];
   Delete[Symbolic[t], indpos[[Flatten[pairs]]]]
];
fingerprint[TensorPermute[Contract[t_, pairs_], perm_]] := Module[{indpos},
   indpos = Position[Symbolic[t], _Raised | _Lowered];
   Delete[Symbolic[t], indpos[[Flatten[pairs]]]]
];

Options["QAnsatz"] = {"QBar" -> False, "Symmetrized" -> True};
QAnsatz[f_Operator, opt : OptionsPattern[]] :=
  QAnsatz[f, opt] = If[OptionValue["Symmetrized"],
   If[OptionValue["QBar"],
      qToQBar[QAnsatz[Conjugate[f], "QBar" -> False]],
   If[SpacetimeDimension[] == 6,
      (* possibleQActions6 already Young-projects each structure onto O's index symmetry and
         returns a linearly independent set, so there is nothing left to symmetrize.  The
         grouping below -- merging the coefficients of terms related by ANY permutation of
         O's index slots -- is only valid when that block is fully symmetric, which in
         d = 6 holds solely for single-row reps. *)
      QAnsatz[f, "QBar" -> False, "Symmetrized" -> False],
	   With[{unsym = QAnsatz[f, "QBar" -> OptionValue["QBar"], "Symmetrized" -> False]},
	      Which[unsym === 0, 0,
	         Head[unsym] =!= Plus, unsym, 
	         True, Module[{terms = Cases[unsym, coeff_SUSYCoefficient t_ :> {coeff, t}], groups, rules},
	          	  groups = Values[#[[;;, 1]] & /@ GroupBy[terms, With[{si = Position[Indices[#[[2]]], Lowered[If[ListQ[Spin[f]], WeylSpinor[SpacetimeDimension[]], DiracSpinor[3]]]][[2;;, 1]], dsi = If[ListQ[Spin[f]], Position[Indices[#[[2]]], Lowered[DottedWeylSpinor[SpacetimeDimension[]]]][[;;, 1]], {}]},
	          	  	{fingerprint[#[[2]]], First@Sort[Flatten[Table[TensorPermutation[#[[2]]] /. Thread[si -> si[[p]]] /. Thread[dsi -> dsi[[p2]]], {p, Permutations[Range[Length[si]]]}, {p2, Permutations[Range[Length[dsi]]]}], 1]]}  
	          	  ] &]];
	          	  rules = Flatten[Table[g[[i]] -> g[[1]], {g, groups}, {i, 2, Length[g]}]];
	          	  unsym /. rules
	         ]
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
						If[IntegerQ[ScalingDimension[op]], TensorCommutator, TensorAnticommutator][$QTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> False];
						If[hasQBar[], If[IntegerQ[ScalingDimension[op]], TensorCommutator, TensorAnticommutator][$QBarTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> True]];,
						{op, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, 2 OptionValue["MaxDepth"] - 1}, {k, 0, 2 OptionValue["MaxDepth"] - 1 - j}]]]}
					],
					"Label" -> "Determining SUSY ansatzes ("<>$multipletName[i]<>")",
					"CurrentDisplayFunction" -> None
				],
			{i, $multipletIndices}],
			Do[
				If[IntegerQ[ScalingDimension[op]], TensorCommutator, TensorAnticommutator][$QTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> False];
				If[hasQBar[], If[IntegerQ[ScalingDimension[op]], TensorCommutator, TensorAnticommutator][$QBarTensor, Tensor[{op}]] = QAnsatz[op, "QBar" -> True]];,
				{i, $multipletIndices},
				{op, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, 2 OptionValue["MaxDepth"] - 1}, {k, 0, 2 OptionValue["MaxDepth"] - 1 - j}]]]}
			]
		];
		
		TensorCommutator[$QTensor, Tensor[{{"\[Delta]", ___}}]] = 0;
		TensorCommutator[$QBarTensor, Tensor[{{"\[Delta]", ___}}]] = 0;
		TensorCommutator[$QTensor, Tensor[{{\[Sigma]LowerTensor[_], ___}}]] = 0;
		TensorCommutator[$QBarTensor, Tensor[{{\[Sigma]LowerTensor[_], ___}}]] = 0;
		TensorCommutator[$QTensor, Tensor[{{"C", ___}}]] = 0;
		TensorCommutator[$QBarTensor, Tensor[{{"C", ___}}]] = 0;
		TensorCommutator[$QTensor, Tensor[{{chargeconj[], ___}}]] = 0;
		TensorCommutator[$QBarTensor, Tensor[{{chargeconj[], ___}}]] = 0;
		TensorCommutator[$QTensor, Tensor[{{"\[PartialD]", ___}}]] = 0;
		TensorCommutator[$QBarTensor, Tensor[{{"\[PartialD]", ___}}]] = 0;
		TensorCommutator[$QTensor, Tensor[{{SU2BreakingTensor[], ___}}]] = 0;
		TensorCommutator[$QBarTensor, Tensor[{{SU2BreakingTensor[], ___}}]] = 0;
    ];
	
	$algebraDeclared = True;
];

quadraticZero[op_] := quadraticZero[op] = Which[
   SpacetimeDimension[] == 4, (* {Q, Qb} = (1/2)P *)
   	NormalOrder[TensorProduct[$QTensor, $QBarTensor, Tensor[{op}]] + TensorProduct[$QBarTensor, $QTensor, Tensor[{op}]], "Vacuum" -> True] + (I/2) TensorProduct[Kronecker[GlobalIndex[QGlobalRep[]]], Tensor[{{"\[PartialD]", Lowered[WeylSpinor[4]], Lowered[DottedWeylSpinor[4]]}}], Tensor[{op}]],
   SpacetimeDimension[] == 3, (* {Q, Q} = (1/2)P *)
    NormalOrder[TensorProduct[$QTensor, $QTensor, Tensor[{op}]] + TensorPermute[TensorProduct[$QTensor, $QTensor, Tensor[{op}]], Join[{3,4,1,2},4 + Range[Length[Indices[Tensor[{op}]]]]]],"Vacuum"->True] + (I/2) TensorProduct[TwoPtGlobalInvariant[QGlobalRep[], QGlobalRep[]], Tensor[{{"\[PartialD]", Lowered[DiracSpinor[3]], Lowered[DiracSpinor[3]]}}], Tensor[{op}]],
   SpacetimeDimension[] == 2, (* {Q, Q} = 2P *)
   	NormalOrder[TensorProduct[$QTensor, $QTensor, Tensor[{op}]] + TensorPermute[TensorProduct[$QTensor, $QTensor, Tensor[{op}]], Join[{3,4,1,2},4 + Range[Length[Indices[Tensor[{op}]]]]]],"Vacuum"->True] + (2I) TensorProduct[TwoPtGlobalInvariant[QGlobalRep[], QGlobalRep[]], Tensor[{{"\[PartialD]", Lowered[WeylSpinor[2]], Lowered[WeylSpinor[2]]}}], Tensor[{op}]],
   SpacetimeDimension[] == 6, (* {Q, Q} = (1/2)P *)
   (* 6D (N, 0) supersymmetry is chiral: a single Weyl supercharge Q_A^i, symplectic-
      Majorana with respect to the R-symmetry, so the anticommutator closes on Q alone,
      {Q_A^i, Q_B^j} ~ Omega^{ij} P_{AB}.  The R-symmetry factor is therefore the
      symplectic two-point invariant (as in d = 2, 3), not a Kronecker delta pairing Q
      with a Qbar (as in d = 4), and P_{AB} = sigma^mu_{AB} P_mu carries two undotted
      Weyl indices and is antisymmetric.  Q is bosonic in the index pair (A i), so the
      two Q's are symmetrized exactly as in the d = 2, 3 cases. *)
   	NormalOrder[TensorProduct[$QTensor, $QTensor, Tensor[{op}]] + TensorPermute[TensorProduct[$QTensor, $QTensor, Tensor[{op}]], Join[{3,4,1,2},4 + Range[Length[Indices[Tensor[{op}]]]]]],"Vacuum"->True] + (I/2) TensorProduct[TwoPtGlobalInvariant[QGlobalRep[], QGlobalRep[]], Tensor[{{"\[PartialD]", Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]]}}], Tensor[{op}]]
];

(* A multiplet is graded by how many Q's and Qbar's separate an operator from the
   superprimary.  Where there is a U(1) R-charge that grading is read off from the
   dimension and the R-charge together.  In d = 6 there is no U(1) R-symmetry (the
   R-symmetry is SU(2) for (1, 0) and USp(4) for (2, 0)), the fifth Operator slot is
   identically zero, and there is no Qbar -- so the level is 2 (Delta - Delta_bottom)
   Q's, with the Qbar count pinned to zero. *)
inQGroupQ[f_, bottom_, i_, j_] := If[hasU1RCharge[],
   ScalingDimension[f] - ScalingDimension[bottom] == (i + j)/2 && Last[f] - Last[bottom] == i - j,
   j == 0 && ScalingDimension[f] - ScalingDimension[bottom] == i/2
];

Options[opGroup] = {"Conjugate" -> False};
opGroup[m_, i_, j_, OptionsPattern[]] := With[{bottom = First@MinimalBy[If[$multipletSC[m], Multiplet[m], Multiplet[m][[If[OptionValue["Conjugate"], 2, 1]]]], ScalingDimension]},
   Select[If[$multipletSC[m], Multiplet[m], Multiplet[m][[If[OptionValue["Conjugate"], 2, 1]]]], inQGroupQ[#, bottom, i, j] & ]
];

groupOf[f_Operator] := Module[{mult, conj, bottom, dimdiff, ydiff},
   mult = whichMultiplet[f];
   conj = (! $multipletSC[mult] && MemberQ[Multiplet[mult][[2]], f]);
   bottom = First@opGroup[mult, 0, 0, "Conjugate" -> conj];
   dimdiff = ScalingDimension[f] - ScalingDimension[bottom];
   ydiff = Last[f] - Last[bottom];
   If[hasU1RCharge[],
      {dimdiff + ydiff/2, dimdiff - ydiff/2, conj},
      {2 dimdiff, 0, conj}
   ]
];

(* The level shift produced by the two supercharges in quadraticZero: {Q, Qbar} raises
   the Q and Qbar counts by one each, whereas the chiral {Q, Q} of d = 6 raises the Q
   count by two. *)
quadraticShifts[] := If[hasQBar[] || hasU1RCharge[],
   {{-1, -1}, {0, 0}, {1, 1}},
   {{-2, 0}, {0, 0}, {2, 0}}
];
   
Options[linearEquations] = {"MaxDepth" -> 0};
linearEquations[i_, opt: OptionsPattern[]] := linearEquations[i, opt] = DeleteCases[Reduce /@ Thread[Flatten @ monitorProgress[
		Table[
		   ExpansionComponents@ExpandCorrelator@Correlator@NormalOrder[TensorProduct[QTensor["QBar" -> If[hasQBar[], qbar, False]], Tensor[{op1, op2}]], "Vacuum" -> True],
		   {op1, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"] - 1}, {k, 0, OptionValue["MaxDepth"] - 1 - j}]]]}, 
		   {qbar, {False, True}}, 
		   {op2, With[{grp = groupOf[op1]}, opGroup[i, Sequence @@ If[SpacetimeDimension[] == 4, Reverse, Identity][grp[[;;2]] + If[qbar, {0, 1}, {1, 0}]], "Conjugate" -> ! grp[[3]]]]}
		],
		"Label" -> "Generating linear equations",
		"CurrentDisplayFunction" -> None
	] == 0], True];
	
Options[quadraticEquations] = {"MaxDepth" -> 0};
quadraticEquations[i_, opt: OptionsPattern[]] := quadraticEquations[i, opt] = DeleteDuplicates@DeleteCases[Simplify /@ Thread[Flatten @ monitorProgress[
		Table[
		   ExpansionComponents@ExpandCorrelator@Correlator[TensorProduct[quadraticZero[op1], Tensor[{op2}]]],
		   {op1, If[OptionValue["MaxDepth"] == 0, Flatten[Multiplet[i]], Flatten[Table[opGroup[i, j, k], {j, 0, OptionValue["MaxDepth"] - 1}, {k, 0, OptionValue["MaxDepth"] - 1 - j}]]]}, 
		   {shift, quadraticShifts[]},
		   {op2, With[{grp = groupOf[op1]}, opGroup[i, Sequence @@ If[SpacetimeDimension[] == 4, Reverse, Identity][grp[[;;2]] + shift], "Conjugate" -> ! grp[[3]]]]}
		],
		"Label" -> "Generating quadratic equations",
		"CurrentDisplayFunction" -> None
	] == 0], True];

Options[SUSYRules] = {"EquationGroupSize" -> 10, "MaxDepth" -> 0};
SUSYRules::nosol = "No consistent SUSY transformations could be found for the multiplet '``'.";
SUSYRules[i_, opt: OptionsPattern[]] := SUSYRules[i] = SUSYRules[i, opt] = Module[{linears, linsol, quadratics, norms, normsol, vars, rvars, rvars2, rsol, quadgroups, quadsol, partialsol},
	DeclareAlgebra["MaxDepth" -> OptionValue["MaxDepth"]];
	
	linears = linearEquations[i, "MaxDepth" -> OptionValue["MaxDepth"]];
	linsol = With[{tmp = Solve[linears]},
	   If[tmp === {}, Missing[],
	      First[tmp]
	   ]
	];
	If[MissingQ[linsol], Message[SUSYRules::nosol, $multipletName[i]]; Return[{}]];
	
	quadratics = quadraticEquations[i, "MaxDepth" -> OptionValue["MaxDepth"]];
	
	quadgroups = Partition[DeleteCases[DeleteDuplicates[Simplify /@ (quadratics /. linsol)], True], UpTo[OptionValue["EquationGroupSize"]]];
	
	vars = DeleteDuplicates@Cases[quadgroups, _SUSYCoefficient, All];
	
	Off[Solve::ifun];
	quadsol = Check[solveGroups[quadgroups, vars, "SortBy" -> (Count[#[[;;,2]], 0] &)], Missing[], solveGroups::nosol];
	On[Solve::ifun];
	If[MissingQ[quadsol], Message[SUSYRules::nosol, $multipletName[i]]; Return[{}]];
	
	partialsol = Join[linsol /. quadsol, quadsol];
	
	rvars = DeleteDuplicates[Cases[partialsol[[;;, 2]], _SUSYCoefficient, All]];	
	If[hasQBar[],
		norms = DeleteCases[Simplify[Cases[partialsol[[;; , 1]], 
		   s : SUSYCoefficient[a_, b_, "QBar" -> qb_] :> (Abs[s] == Abs[SUSYCoefficient[Conjugate[name2field[a]][[1]], b, "QBar" -> !qb]]), 
		   All] /. partialsol /. Thread[rvars -> Array[\[Alpha], Length[rvars]]], \[Alpha][_] > 0], 
		x_ /; ! FreeQ[x, SUSYCoefficient]];
		
		normsol = Last[Solve[norms]],
		normsol = {} 
	];
	rvars2 = Complement[rvars, normsol[[;;, 1]]];
	rsol = Thread[rvars2 -> 1]; (* fix remaining variables arbitrarily to 1 *)
	
	Thread[Join[partialsol[[;;, 1]], rvars] -> (Join[partialsol[[;;, 2]], rvars] /. Thread[rvars -> Array[\[Alpha], Length[rvars]]] /. normsol /. Thread[Array[\[Alpha], Length[rvars]] -> rvars /. rsol])]
];

Options[susyTable] = {"Solved" -> True, "MaxDepth" -> 0};
susyTable[ops_, OptionsPattern[]] := With[{g = Grid[Table[{TensorProduct[$QTensor, ToTensor[op]], 
    QAnsatz[op, "QBar" -> False] /. If[TrueQ[OptionValue["Solved"]], If[OptionValue["MaxDepth"] == 0, SUSYRules[whichMultiplet[ops[[1]]]], SUSYRules[whichMultiplet[ops[[1]]], "MaxDepth" -> OptionValue["MaxDepth"]]], {}], 
    Sequence @@ If[hasQBar[], {
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