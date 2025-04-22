(* Wolfram Language package *)

Clear[CartanSubalgebra];
CartanSubalgebra[U1] := {{{1}}, {1}};
CartanSubalgebra[grp_?IsSimpleGroupQ] := 
 Module[{diagonals, diagIdxs, weights, weights2, funds, funds2},
  diagIdxs = 
   Position[RepMatrices[grp, Adjoint[grp]], 
     mat_?DiagonalMatrixQ][[;; , 1]];
  diagonals = RepMatrices[grp, Adjoint[grp]][[diagIdxs]];
  weights = Thread[Diagonal /@ diagonals];
  weights2 = Flatten[Table @@@ Weights[grp, Adjoint[grp]], 1];
  funds = 
   weights[[SelectFirst[
      Tuples[Range@Length[diagonals[[1]]], Length[weights[[1]]]],
      Det[weights[[#]]] != 0 && 
        AllTrue[weights . Inverse[weights[[#]]], 
         Function[vec, AllTrue[vec, NonNegative] || 
           AllTrue[-vec, NonNegative]]] &
      ]]];
  funds2 = 
   weights2[[SelectFirst[
      Tuples[Range@Length[diagonals[[1]]], Length[weights2[[1]]]],
      Det[weights2[[#]]] != 0 && 
        AllTrue[weights2 . Inverse[weights2[[#]]], 
         Function[vec, AllTrue[vec, NonNegative] || 
           AllTrue[-vec, NonNegative]]] &
      ]]];
  {Transpose[funds2] . Inverse[Transpose[funds]] // Simplify, diagIdxs}
  ];
CartanSubalgebra[grp_] := Module[{cartans = CartanSubalgebra /@ grp},
  {BlockDiagonalMatrix[cartans[[;; , 1]]], 
   Join @@ Table[
     cartans[[i, 2]] + 
      Total[DimR[#, Adjoint[#]] & /@ grp[[;; i - 1]]], {i, 
      Length[cartans]}]}
  ]

Weights[U1, n_] := {{n, 1}};
  
collectIrreps[grp_, {}] = {};
collectIrreps[grp_, weights_] := 
  Module[{allweights, highest = Last@Sort@weights, perm, counts, pos, recur},
   allweights = Flatten[Table @@@ Weights[grp, highest], 1];
   counts = Counts[allweights];
   pos = 
    Position[
     withCounts[weights], {w_, n_} /; 
      KeyExistsQ[counts, w] && n <= counts[w]];
   perm = FindPermutation[weights[[pos[[;;,1]]]], allweights];
   pos = pos[[PermutationList[perm, Length[pos]]]];
   recur = collectIrreps[grp, Delete[weights, pos]];
   Prepend[
    Table[{r[[1]], 
      Position[
        Fold[Insert[#1, 0, #2] &, 
         Normal@SparseArray[Thread[(List /@ r[[2]]) -> 1], 
           Length[weights] - Length[pos]], pos[[;; , 1]]], 1][[;; , 
        1]]}, {r, recur}], {highest, pos[[;; , 1]]}]
  ];
  
  
embeddingIndex::notfound = "Out of subrepresentations ``, could not find instance no. `` of ``";
embeddingIndex[rep_, parent_] := embeddingIndex[RepWithMultiplicity[rep, 1], parent];
embeddingIndex[RepWithMultiplicity[rep_, multiplicity_], parent_] := embeddingIndex[RepWithMultiplicity[rep, multiplicity], parent] =
  Module[{cartan = CartanSubalgebra[GlobalSymmetry[]], decompWeights, collected, instances},
   decompWeights = Thread[Diagonal /@ (cartan[[1]] . (RepMatrices[GlobalSymmetry[], parent][[cartan[[2]]]]))] . Transpose[$embedding];
   collected = collectIrreps[DefectGlobalSymmetry[], decompWeights];
   instances = Cases[collected, {rep, pos_} | {{rep}, pos_} :> pos];
   If[Length[instances] >= multiplicity, instances[[multiplicity]], Message[embeddingIndex::notfound, collected, multiplicity, rep]]
  ];

allEmbeddings[grp_, subgrp_] /; IsSimpleGroupQ[grp] := 
  allEmbeddings[{grp}, subgrp];
allEmbeddings[grp_, subgrp_] /; IsSimpleGroupQ[subgrp] := 
  allEmbeddings[grp, {subgrp}];
allEmbeddings[grp_, subgrp_] := allEmbeddings[grp, subgrp] =
 With[{u1s = Position[subgrp, U1][[;; , 1]]},
  Table[
   {ReverseSort[emb[[2, 1]]],
     If[Length[u1s] > 0,
    If[Total[GroupRank /@ emb[[2, 1]]] - Count[emb[[2, 1]], U1] > 0,
     ArrayFlatten[{
        {IdentityMatrix[
          Total[GroupRank /@ emb[[2, 1]]] - Count[emb[[2, 1]], U1]], 
         0},
        {0, 
         Table[embeddingParameter[emb[[1]], i, j], {i, 
           Length[u1s]}, {j, Count[emb[[2, 1]], U1]}]}
        }] . emb[[2, 2]],
        
     Table[
       embeddingParameter[emb[[1]], i, j], {i, Length[u1s]}, {j, 
        Count[emb[[2, 1]], U1]}] . emb[[2, 2]]
     ],
     emb[[2,2,;;(Total[GroupRank /@ emb[[2, 1]]] - Count[emb[[2, 1]], U1])]]
     ]
    }
   , {emb, Thread[{Range@Length[#], #}] &@Embeddings[grp, subgrp]}]
  ]

branching[grp_, subgrp_, prj_] /; IsSimpleGroupQ[grp] := 
  branching[{grp}, subgrp, prj];
branching[grp_, subgrp_, prj_] := branching[grp, subgrp] =
  Table[{rep, DecomposeRep[grp, rep, subgrp, prj]}, {rep, 
    TakeList[#,GroupRank /@ grp] & /@ IdentityMatrix[Total[GroupRank /@ grp]]}];
displayBranching[grp_, subgrp_, prj_] /; IsSimpleGroupQ[grp] := 
  displayBranching[{grp}, subgrp, prj];
displayBranching[grp_, subgrp_, prj_] /; IsSimpleGroupQ[subgrp] := 
  displayBranching[grp, {subgrp}, prj];
displayBranching[grp_, subgrp_, prj_] /; !IsSimpleGroupQ[grp] && !IsSimpleGroupQ[subgrp] := displayBranching[grp, subgrp, prj] =
 Column[Prepend[RepName[grp, #[[1]]] -> 
     Table[RepName[subgrp, r], {r, #[[2]]}] & /@ 
   branching[grp, subgrp, prj], "Branching:"]]

embeddingSelector[grp_, subgrp_] /; IsSimpleGroupQ[grp] := 
  embeddingSelector[{grp}, subgrp];
embeddingSelector[grp_, subgrp_] /; IsSimpleGroupQ[subgrp] := 
  embeddingSelector[grp, {subgrp}];
embeddingSelector[grp_, subgrp_] /; !IsSimpleGroupQ[grp] && !IsSimpleGroupQ[subgrp] := (ClearAll[embeddingParameter]; 
   Column[{Style[
      ToString@
       StringForm["Choose embedding of `` into ``", CMtoName[subgrp], 
        CMtoName[grp]], 16],
     Spacer[20],
     Grid[Table[
       With[{rules = 
          Thread[Sort@
             DeleteDuplicates[
              Cases[emb[[2]], _embeddingParameter, All]] -> 
            Alphabet[][[;; 
               Length@DeleteDuplicates@
                 Cases[emb[[2]], _embeddingParameter, All]]]], 
         emb = emb},
        {
         MatrixForm[emb[[2]] /. rules],
         Spacer[20],
         If[!FreeQ[emb[[2]], embeddingParameter], "with", ""],
         Spacer[20],
         
         With[{vars = 
            Sort@DeleteDuplicates[
              Cases[emb[[2]], _embeddingParameter, All]]},
          DynamicModule[{params = vars /. _embeddingParameter -> Null},
           Column[Table[
             
             Row[{Alphabet[][[i]], " = ", 
               With[{i = i}, 
                InputField[
                 Dynamic[params[[i]], (If[IntegerQ[#], 
                    params[[i]] = #; vars = params]) &], Number, 
                 FieldSize -> 3, ImageSize -> 40]]}],
             {i, Length[params]}
             ]
            ]
           ]
          ],
         Spacer[20],
         Button["Select", ($embedding = emb[[2]]) &]
         }
        ]
       , {emb, allEmbeddings[grp, subgrp]}]
      ],
     Spacer[40],
     Style["Fundamental branching rules:", 14],
     Spacer[20],
     Dynamic[
      If[Head[$embedding] =!= Symbol && 
        AllTrue[Flatten[$embedding], IntegerQ], 
       displayBranching[grp, subgrp, $embedding], ""]]
     }]
   );

decomposeRepDefectFP[rep_] := Flatten[If[#[[2]] == 1, {{#[[1]]}}, Table[RepWithMultiplicity[#[[1]], i], {i, #[[2]]}]] & /@ Tally[Sort@DecomposeRep[$RSymmetry, rep, $DefectRSymmetry, $embedding]], 2];

branchingRules[] := With[{groups = GroupBy[Append[Flatten[Table[dynkin@*GlobalRep /@ Multiplet[idx], {idx, $multipletIndices}], 1], QGlobalRep[]], decomposeRepDefectFP]},
   Association[Flatten[Table[Thread[groups[k] -> Table[k, Length[groups[k]]]], {k, Keys[groups]}]]]
];

decomposeRepDefect[rep_] := branchingRules[][dynkin[rep]];
preimageReps[defectRep_] := Select[Keys[branchingRules[]], branchingRules[][#] == {defectRep} &];

convertRToDefect[a_ b_] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := a convertRToDefect[b];
convertRToDefect[a_] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := a;
convertRToDefect[a_ + b_] := convertRToDefect[a] + convertRToDefect[b];

invSelectionRule = {(
	Tensor[{___, {"C", Lowered[DefectGlobalIndex[i_, __]], Raised[DefectGlobalIndex[j_, __]], Raised[DefectGlobalIndex[k_, __]]}, ___}] | 
	Tensor[{___, {"C", Raised[DefectGlobalIndex[i_, __]], Lowered[DefectGlobalIndex[j_, __]], Lowered[DefectGlobalIndex[k_, __]]}, ___}]
  ) /; !MemberQ[ReduceRepProduct[DefectGlobalSymmetry[], {j, k} /. RepWithMultiplicity[r_, _] :> r][[;;,1]], i /. RepWithMultiplicity[r_, _] :> r] :> 0,
  (
	Tensor[{___, {SU2BreakingTensor[], Raised[DefectGlobalIndex[i_, __]], Raised[DefectGlobalIndex[j_, __]]}, ___}]
  ) /; !MemberQ[ReduceRepProduct[DefectGlobalSymmetry[], {i, j} /. RepWithMultiplicity[r_, _] :> r][[;;,1]], singRep[DefectGlobalSymmetry[]]] :> 0,
  (
	Tensor[{___, {SU2BreakingTensor[], Raised[DefectGlobalIndex[i_, __]], Lowered[DefectGlobalIndex[j_, __]]}, ___}]
  ) /; !MemberQ[ReduceRepProduct[DefectGlobalSymmetry[], {i, ConjugateIrrep[DefectGlobalSymmetry[], j]} /. RepWithMultiplicity[r_, _] :> r][[;;,1]], singRep[DefectGlobalSymmetry[]]] :> 0}; 
 

fixPermutation[t_Tensor] := t;
fixPermutation[TensorPermute[t_Tensor, perm_]] := Module[{symbolic, indexPos, wc, rearranged, wc2},
  symbolic = Symbolic[t];
  indexPos = Position[symbolic, _Raised | _Lowered];
  wc = withCounts[Indices[t]];
  rearranged = Tensor[ReplacePart[symbolic, Thread[indexPos -> (Part[symbolic, Sequence @@ #] & /@ (indexPos[[perm]]))]]];
  wc2 = withCounts[Indices[rearranged]];
  TensorPermute[rearranged, PermutationList[FindPermutation[wc[[perm]], wc2], Length[perm]]]
];

fixPermutation[Contract[t_, pairs_]] := Module[{newt},
  newt = fixPermutation[t];
  deltaperm = TensorPermutation[newt][[InversePermutation@TensorPermutation[t]]];
  Contract[newt, pairs /. n_Integer :> deltaperm[[n]]]
];

convertRToDefect[t_Tensor | t_TensorPermute] := Module[{perm, symbolic, rIndexPos},
   perm = TensorPermutation[t];
   symbolic = Symbolic[t];
   rIndexPos = SortBy[Position[symbolic, (Raised|Lowered)[_GlobalIndex]], {symbolic[[Sequence @@ #]], Total[(Length /@ symbolic[[;; #[[1]] - 1]]) - 1] + #[[2]] - 1} &];
   Table[
      fixPermutation@TensorPermute[
         Tensor[
            ReplacePart[
               symbolic, 
               Thread[
                  (Append[#, 1] & /@ rIndexPos) -> 
                  (toIndex @@@ Thread[{tup, symbolic[[Sequence @@ #,1,1]] & /@ rIndexPos}])
               ]
            ]
         ], 
      perm],
      {tup, Tuples[branchingRules[][symbolic[[Sequence @@ #,1,1]]] & /@ rIndexPos]}   
   ] /. invSelectionRule
];

convertRToDefect[Contract[t_, pairs_, OptionsPattern[]]] := Module[{perm, symbolic, rIndexPos, contracted, uncontracted},
   perm = TensorPermutation[t];
   symbolic = Symbolic[t];
   rIndexPos = Position[symbolic, (Raised|Lowered)[_GlobalIndex]];
   contracted = DeleteCases[pairs /. n_Integer :> SelectFirst[rIndexPos, Total[(Length /@ symbolic[[;; #[[1]] - 1]]) - 1] + #[[2]] - 1 == n &], {_?MissingQ, _?MissingQ}];
   uncontracted = SortBy[Complement[rIndexPos, Flatten[contracted, 1]], {symbolic[[Sequence @@ #]], Total[(Length /@ symbolic[[;; #[[1]] - 1]]) - 1] + #[[2]] - 1} &];
   Table[
      Sum[
      fixPermutation@Contract[
         TensorPermute[
            Tensor[
               ReplacePart[
                  symbolic, 
                  Thread[
                     (Append[#, 1] & /@ Join[uncontracted, contracted[[;;, 1]], contracted[[;;, 2]]]) -> 
                     (toIndex @@@ Thread[{Join[uTup, cTup, cTup], symbolic[[Sequence @@ #,1,1]] & /@ Join[uncontracted, contracted[[;;, 1]], contracted[[;;, 2]]]}])
                  ]
               ]
            ], 
         perm], 
      pairs],
      {cTup, Tuples[branchingRules[][symbolic[[Sequence @@ #,1,1]]] & /@ contracted[[;;,1]]]} 
      ],
      {uTup, Tuples[branchingRules[][symbolic[[Sequence @@ #,1,1]]] & /@ uncontracted]}  
   ] /. invSelectionRule  
];

Options[SU2Breaker] = {"Mixed" -> False};
SU2Breaker[OptionsPattern[]] := 
	If[OptionValue["Mixed"],
	   Tensor[{{SU2BreakingTensor[], Raised[GlobalIndex[QGlobalRep[]]], Lowered[GlobalIndex[QGlobalRep[]]]}}],
  	   Tensor[{{SU2BreakingTensor[], Raised[GlobalIndex[QGlobalRep[]]], Raised[GlobalIndex[QGlobalRep[]]]}}]
	];
BuildTensor[{SU2BreakingTensor[], Raised[DefectGlobalIndex[r1_, _]], Raised[DefectGlobalIndex[r2_, _]]}] := SparseArray[{{Boole[MemberQ[ReduceRepProduct[DefectGlobalSymmetry[], {r1, r2}][[;;,1]], singRep[DefectGlobalSymmetry[]]] ]}}];
BuildTensor[{SU2BreakingTensor[], Raised[DefectGlobalIndex[r1_, _]], Lowered[DefectGlobalIndex[r2_, _]]}] := SparseArray[{{Boole[MemberQ[ReduceRepProduct[DefectGlobalSymmetry[], {r1, ConjugateIrrep[DefectGlobalSymmetry[], r2]}][[;;,1]], singRep[DefectGlobalSymmetry[]]] ]}}];

(*transformer[rep_] := (branchingRules[]; transformer[rep]);*)

Options[TwoPtGlobalInvariant] = {"Conjugate" -> False};
TwoPtGlobalInvariant[rep1_, rep2_, OptionsPattern[]] := If[OptionValue["Conjugate"], ConjugateTwoPtGlobalInvariant[rep1, rep2], Tensor[{{"\[Delta]", Raised[toIndex[rep1]], Raised[toIndex[rep2]]}}]];
ConjugateTwoPtGlobalInvariant[rep1_, rep2_] := Tensor[{{"\[Delta]", Lowered[toIndex[rep1]], Lowered[toIndex[rep2]]}}];

Options[ThreePtGlobalInvariant] = {"Conjugate" -> False};
ThreePtGlobalInvariant[{rep1_, rep2_}, target_, OptionsPattern[]] := If[OptionValue["Conjugate"], ConjugateThreePtGlobalInvariant[{rep1, rep2}, target], Tensor[{{"C", Raised[toIndex[target]], Lowered[toIndex[rep1]], Lowered[toIndex[rep2]]}}]];
ConjugateThreePtGlobalInvariant[{rep1_, rep2_}, target_] := Tensor[{{"C", Lowered[toIndex[target]], Raised[toIndex[rep1]], Raised[toIndex[rep2]]}}];

Options[ThreePtGlobalInvariant] = {"Conjugate" -> False};
ThreePtGlobalInvariant[r1_, r2_, r3_, OptionsPattern[]] := If[OptionValue["Conjugate"], ConjugateThreePtGlobalInvariant[r1, r2, r3], Tensor[{{"C", Raised[toIndex[r1]], Raised[toIndex[r2]], Raised[toIndex[r3]]}}]];
ConjugateThreePtGlobalInvariant[r1_, r2_, r3_] := Tensor[{{"C", Lowered[toIndex[r1]], Lowered[toIndex[r2]], Lowered[toIndex[r3]]}}];
     
BuildTensor[arg : {"\[Delta]", Raised[i1_], Raised[i2_]}] := twopt[i1, i2];
BuildTensor[arg : {"\[Delta]", Lowered[i1_], Lowered[i2_]}] := SparseArray@Inverse[Components@Tensor[{{"\[Delta]", Raised[i2], Raised[i1]}}]];

$customInvariants = False;

twopt::undefined = "The two-point invariant for representations (``, ``) has not been defined.";
twopt::unable = "Cannot automatically construct two-point invariant for (`` from ``, `` from ``).";
twopt[GlobalIndex[r1_], GlobalIndex[r2_]] := Module[{reps = {dynkin[r1], dynkin[r2]}},
	TensorTranspose[twopt @@ Sort[reps], Ordering[reps]]
];
twopt[DefectGlobalIndex[r1_, p1_], DefectGlobalIndex[r2_, p2_]] := Module[{reps = {{r1, dynkin[p1]}, {r2, dynkin[p2]}}},
	TensorTranspose[twopt @@ Flatten[stableSortBy[reps, Last], 1], stableOrderingBy[reps, Last]]
];

twopt[r1_, r2_] := twopt[r1, r2] = If[$customInvariants,
   Message[twopt::undefined, r1, r2],
   If[!MemberQ[ReduceRepProduct[GlobalSymmetry[], {r1, r2}][[;;, 1]], singRep[GlobalSymmetry[]]],
      Message[twopt::undefined, r1, r2],
   	  IrrepInProduct[GlobalSymmetry[], {r1, r2}, singRep[GlobalSymmetry[]], TensorForm -> True][[1,1,;;,;;,1]]
   ]
];
twopt[r1_, p1_, r2_, p2_] := twopt[r1, p1, r2, p2] = If[$customInvariants,
   Message[twopt::undefined, {r1, p1}, {r2, p2}],
   Module[{singletProduct},
	  singletProduct = SelectFirst[ReduceRepProduct[GlobalSymmetry[], {p1, p2}][[;;, 1]], repDim[#] == 1 &];
      If[
         !MissingQ[singletProduct],
	      IrrepInProduct[GlobalSymmetry[], {p1, p2}, singletProduct, TensorForm -> True][[1,1,
	         embeddingIndex[r1, p1],
	         embeddingIndex[r2, p2],
	         1]],
      	IrrepInProduct[DefectGlobalSymmetry[], {r1, r2}, singRep[DefectGlobalSymmetry[]], TensorForm -> True][[1,1,;;,;;,1]]
      ]
   ]
];
 
threept::undefined = "The three-point invariant for representations (``, ``, ``) has not been defined.";
threept[GlobalIndex[r1_], GlobalIndex[r2_], GlobalIndex[r3_]] := Module[{reps = {dynkin[r1], dynkin[r2], dynkin[r3]}},
	TensorTranspose[threept @@ Sort[reps], Ordering[reps]]
];
threept[r1_, r2_, r3_] := threept[r1, r2, r3] = If[$customInvariants,
   Message[threept::undefined, r1, r2, r3],
   If[!MemberQ[ReduceRepProduct[GlobalSymmetry[], {r1, r2, r3}][[;;, 1]], singRep[GlobalSymmetry[]]],
      Message[threept::undefined, r1, r2, r3],
	   Module[{temp, other},
	   	temp = IrrepInProduct[GlobalSymmetry[], {r1, r2}, r3, ConjugateTargetRep -> True, TensorForm -> True][[1,1]];
	   	If[OrderedQ[{{r1, r2, r3}, Sort@{ConjugateIrrep[GlobalSymmetry[], r1], ConjugateIrrep[GlobalSymmetry[], r2], ConjugateIrrep[GlobalSymmetry[], r3]}}],
	   	   temp,
	   	   other = threept@@ Sort[{ConjugateIrrep[GlobalSymmetry[], r1], ConjugateIrrep[GlobalSymmetry[], r2], ConjugateIrrep[GlobalSymmetry[], r3]}];
	   	   (Norm[Flatten[other]]/Norm[Flatten[temp]]) temp
	   	]
	   ]
   ]
];

SetTwoPtGlobalInvariant[r1_, r2_, mat_] := Module[{reps = dynkin /@ ({r1, r2}), sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = Ordering[reps];
 	twopt[sorted[[1]], sorted[[2]]] = SparseArray@TensorTranspose[mat, order];
]

SetTwoPtDefectGlobalInvariant[{r1_, p1_}, {r2_, p2_}, mat_] := Module[{reps = {{dynkin[r1], dynkin[p1]}, {dynkin[r2], dynkin[p2]}}, sorted, order},
    $customInvariants = True;
 	sorted = stableSortBy[reps, Last];
 	order = stableOrderingBy[reps, Last];
 	twopt[sorted[[1,1]], sorted[[1,2]], sorted[[2,1]], sorted[[2,2]]] = SparseArray@TensorTranspose[mat, order];
]

SetThreePtGlobalInvariant[r1_, r2_, r3_, mat_] := Module[{reps = dynkin /@ {r1, r2, r3}, sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = InversePermutation@Ordering[reps];
 	threept[sorted[[1]], sorted[[2]], sorted[[3]]] = SparseArray@TensorTranspose[mat, order];
]

SetThreePtDefectGlobalInvariant[{r1_, p1_}, {r2_, p2_}, {r3_, p3_}, mat_] := Module[{reps = {{dynkin[r1], dynkin[p1]}, {dynkin[r2], dynkin[p2]}, {dynkin[r3], dynkin[p3]}}, sorted, order},
    $customInvariants = True;
 	sorted = stableSortBy[reps, Last];
 	order = InversePermutation@stableOrderingBy[reps, Last];
 	threept[sorted[[1,1]], sorted[[1,2]], sorted[[2,1]], sorted[[2,2]], sorted[[3,1]], sorted[[3,2]]] = SparseArray@TensorTranspose[mat, order];
]

BuildTensor[{"C", Raised[i1_], Raised[i2_], Raised[i3_]}] /; FreeQ[{i1,i2,i3}, DefectGlobalIndex] := threept[i1, i2, i3];
BuildTensor[{"C", Lowered[i1_], Raised[i2_], Raised[i3_]}] /; FreeQ[{i1,i2,i3}, DefectGlobalIndex] := 
    TensorTranspose[Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[conjIndex[i1]], Raised[i2], Raised[i3]}}], 
       Tensor[{{"\[Delta]", Lowered[conjIndex[i1]], Lowered[i1]}}]
    ], {{1, 4}}]], {2,3,1}];

BuildTensor[{"C", Raised[i1_], Lowered[i2_], Lowered[i3_]}] /; FreeQ[{i1,i2,i3}, DefectGlobalIndex] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Lowered[conjIndex[i1]], Raised[conjIndex[i2]], Raised[conjIndex[i3]]}}], 
       Tensor[{{"\[Delta]", Raised[conjIndex[i1]], Raised[i1]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i2]], Lowered[i2]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i3]], Lowered[i3]}}]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
BuildTensor[{"C", Lowered[i1_], Lowered[i2_], Lowered[i3_]}] /; FreeQ[{i1,i2,i3}, DefectGlobalIndex] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[conjIndex[i1]], Raised[conjIndex[i2]], Raised[conjIndex[i3]]}}], 
       Tensor[{{"\[Delta]", Lowered[conjIndex[i1]], Lowered[i1]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i2]], Lowered[i2]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i3]], Lowered[i3]}}]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
    
BuildTensor[{"C", rli1_, rli2_, rli3_}] /; AllTrue[{rli1,rli2,rli3}, MatchQ[#, Raised[_DefectGlobalIndex] | Lowered[_DefectGlobalIndex]] &] :=
	BuildTensor[{"C", Sequence @@ ({rli1,rli2,rli3} /. DefectGlobalIndex[_, parent_] :> GlobalIndex[parent])}][[
	   	embeddingIndex[rli1[[1,1]], rli1[[1,2]]],
		embeddingIndex[rli2[[1,1]], rli2[[1,2]]],
		embeddingIndex[rli3[[1,1]], rli3[[1,2]]]
	]]
      
buildExpression[LoopInvariant[edges_]] := 
 Module[{internals = SortBy[DeleteDuplicates@Cases[edges, _Internal, All], FirstCase[edges, {#, External[i_], _} :> i] &], pi, cs, deltas},
  pi = PositionIndex[internals];
  cs = Association@
    Thread[internals -> 
      Table[ThreePtGlobalInvariant[{FirstCase[edges, {_Internal, i, rep_} :> conjRep[rep]], 
         FirstCase[edges, {i, _Internal, rep_} :> rep]}, 
        FirstCase[edges, {i, _External, rep_} :> rep]], {i, 
        internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtGlobalInvariant[rep, conjRep[rep]]}];
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
      Table[ConjugateThreePtGlobalInvariant[
        Cases[edges, {i, _External, rep_} :> rep], 
        If[Cases[edges, {i, _Internal, _}] != {}, 
         FirstCase[edges, {i, _Internal, rep_} :> rep], 
         FirstCase[
          edges, {_Internal, i, rep_} :> 
           conjRep[rep]]]], {i, internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtGlobalInvariant[rep, conjRep[rep]]}];
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

$internalReps = Automatic;

allReps[] := If[$internalReps === Automatic, DeleteDuplicates[GlobalRep /@ Flatten[Multiplet /@ $multipletIndices]], $internalReps];
repTree[reps_] := repTree[reps] =
 Graph[Flatten[
   Table[{i, r}, {i, 0, Length[reps]}, {r, allReps[]}], 1], 
  Flatten[Table[
    DirectedEdge[{i, r}, {i + 1, rp}],
    {i, 0, 3}, {r, allReps[]},
    {rp, 
     Intersection[
      Select[ReduceRepProduct[
         appropriateGroup[reps[[1]]], {r, reps[[i + 1]]}], #[[2]] == 1 &][[;; , 1]], 
      allReps[]]}
    ]], VertexCoordinates -> 
   Flatten[Table[{i, (allReps[])[[j]]} -> {j, i}, {i, 0, 
      Length[reps]}, {j, Length[allReps[]]}], 1]]


Options[treeGraphs] = {"IrrepSet" -> Automatic};
Options[loopGraphs] = {"IrrepSet" -> Automatic};
loopGraphs[reps_, opt : OptionsPattern[]] := loopGraphs[reps, opt] = Flatten@Table[
    LoopInvariant[
     Join[Table[{Internal[i], External[i], reps[[i]]}, {i, 
        Length[reps]}], 
      Table[{Internal[perm[[i]]], Internal[perm[[Mod[i, 4] + 1]]], 
        path[[i + 1, 2]]}, {i, Length[reps]}]]],
    {perm, {{1, 2, 3, 4}, {1, 3, 4, 2}, {1, 4, 2, 3}}},
    {start, dynkin /@ allReps[]},
    {path, 
     FindPath[
      repTree[reps[[perm]]], {0, start}, {Length[reps], start}, {4}, 
      All]}];
treeGraphs[reps_] := treeGraphs[reps] = Flatten@Table[
   TreeInvariant[{{Internal[1], External[p[[1]]], 
      reps[[p[[1]]]]}, {Internal[1], External[p[[2]]], 
      reps[[p[[2]]]]}, {Internal[2], External[p[[3]]], 
      reps[[p[[3]]]]}, {Internal[2], External[p[[4]]], 
      reps[[p[[4]]]]}, {Internal[1], Internal[2], rep}}],
   {p, {{1, 2, 3, 4}, {1, 3, 4, 2}, {1, 4, 2, 3}}},
   {rep, 
    Intersection[allReps[], 
     Select[ReduceRepProduct[appropriateGroup[reps[[1]]], reps[[p[[;; 2]]]]], #[[2]] == 1 &][[;; , 1]], 
     conjRep /@ (Select[ReduceRepProduct[appropriateGroup[reps[[1]]], reps[[p[[3 ;;]]]]], #[[2]] == 1 &][[;; , 1]])]}
   ];

FourPtInvariantGraphs[reps_] /; Length[reps] == 4 := FourPtInvariantGraphs[reps, opt] = 
 With[{graphs = SortBy[Join[treeGraphs[reps], loopGraphs[reps]], {Max[Cases[#[[1]], {_Internal, _Internal, rep_} :> Times @@ repDim[rep]]], 
  Length[Cases[#[[1]], _Internal, All]]} &]},
    graphs[[IndependentSet[buildExpression /@ graphs, "MaxIndependent" -> numInvariants[reps], "Indices" -> True]]]
  ]
   
InvariantFourPts[reps_] /; Length[reps] == 4 := InvariantFourPts[reps] = With[{order = Ordering[dynkin /@ reps]},
   SparseArray@(TensorTranspose[CanonicallyOrderedComponents@buildExpression[#], order] & /@ FourPtInvariantGraphs[reps, opt])
];

  
FourPtGlobalInvariant[reps_, i_] := Tensor[{{GlobalInvariant[i], Sequence @@ Table[Raised[If[appropriateGroup[reps[[1]]] == GlobalSymmetry[], GlobalIndex, DefectGlobalIndex][r]], {r, reps}]}}];
BuildTensor[{GlobalInvariant[i_], Raised[GlobalIndex[r1_]], Raised[GlobalIndex[r2_]], Raised[GlobalIndex[r3_]], Raised[GlobalIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];
BuildTensor[{GlobalInvariant[i_], Raised[DefectGlobalIndex[r1_]], Raised[DefectGlobalIndex[r2_]], Raised[DefectGlobalIndex[r3_]], Raised[DefectGlobalIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];


appropriateGroup[rep_] := If[IntegerQ[rep] && MemberQ[{GlobalSymmetry[], DefectGlobalSymmetry[]}, U1],
   U1,
   If[Quiet@Check[fundRep[GlobalSymmetry[]] + rep; True, False], GlobalSymmetry[], DefectGlobalSymmetry[]]
];
repDim[RepWithMultiplicity[rep_, mult_]] := repDim[rep];
repDim[rep_] := Times @@ DimR[appropriateGroup[rep], rep];
repName[RepWithMultiplicity[rep_, mult_]] := Subscript[repName[rep], mult];
repName[rep_] := RepName[appropriateGroup[rep], rep];
dynkin[RepWithMultiplicity[rep_, mult_]] := dynkin[rep];
dynkin[rep_] := SimpleRepInputConversion[appropriateGroup[rep], rep];
conjRep[rep_] := ConjugateIrrep[appropriateGroup[rep], rep];
toIndex::needparent = "To create an index from the defect global symmetry irrep `` you must specify the full global symmetry irrep from which it branches."; 
toIndex[rep_] := If[appropriateGroup[rep] == GlobalSymmetry[], GlobalIndex[rep], Message[toIndex::needparent, repName[rep]]];
toIndex[rep_, parent_] := If[appropriateGroup[rep] == DefectGlobalSymmetry[], DefectGlobalIndex[rep, parent], GlobalIndex[rep]];
toIndex[RepWithMultiplicity[rep_, mult_], parent_] := DefectGlobalIndex[RepWithMultiplicity[rep, mult], parent];
conjIndex[GlobalIndex[rep_]] := GlobalIndex[conjRep[rep]];
conjIndex[DefectGlobalIndex[rep_, parent_]] := DefectGlobalIndex[conjRep[rep], conjRep[parent]];
toRep[GlobalIndex[rep_]] := rep;
toRep[DefectGlobalIndex[rep_, parent_]] := rep;

numInvariants[{idxs__GlobalIndex}] := numInvariants[{idxs}[[;;,1]]];
numInvariants[{d1_DefectGlobalIndex, d2_DefectGlobalIndex}] := If[!MemberQ[ReduceRepProduct[DefectGlobalSymmetry[], {d1[[1]], d2[[1]]} /. RepWithMultiplicity[r_, _] :> r][[;;,1]], singRep[DefectGlobalSymmetry[]]] || MatchQ[Normal[Flatten[twopt[d1,d2]]], {0...}], 0, 1]; 
numInvariants[reps_] /; FreeQ[reps, GlobalIndex | DefectGlobalIndex] := numInvariants[reps] =
   With[{grp = appropriateGroup[reps[[1]] /. RepWithMultiplicity[r_, _] :> r]},
  	 If[# == {}, 0, #[[1, 2]]] &@ Select[ReduceRepProduct[grp, reps /. RepWithMultiplicity[r_, _] :> r], #[[1]] == singRep[grp] &]
	];