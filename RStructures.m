(* Wolfram Language package *)

decomposeRepDefectFP::split = "When `1` is embedded into `2`, the representation `3` of `2` splits into several representations of `1`: `4`. This case has not yet been implemented.";
decomposeRepDefectFP[rep_] := With[{decomp = DecomposeRep[$RSymmetry, rep, $DefectRSymmetry, Embeddings[$RSymmetry, $DefectRSymmetry][[1,2]]]},
	If[Length[decomp] > 1, Message[decomposeRepDefectFP::manyembed, CMtoName[$DefectRSymmetry], CMtoName[$RSymmetry], RepName[$RSymmetry, rep], RepNameBatchMode[$DefectRSymmetry, defectFund]]; Return[]];
	decomp[[1]]   
];

branchingRules::overlap = "The representations `1` all branch to `2`, and are not related by conjugacy. This case has not yet been implemented.";
branchingRules[] := With[{groups = GroupBy[Flatten[Table[dynkin@*RRep /@ Multiplet[idx], {idx, $multipletIndices}], 1], decomposeRepDefectFP]},
   If[AnyTrue[Values[groups], Length[#] > 1 && repDim[#[[1]]] != 1 && (Length[#] > 2 || conjRep[#[[1]]] =!= dynkin[#[[2]]]) &],
      Message[branchingRules::overlap, 
         SelectFirst[Values[groups], Length[#] > 1 && repDim[#[[1]]] != 1 && (Length[#] > 2 || conjRep[#[[1]]] =!= dynkin[#[[2]]]) &], 
         decomposeRepDefectFP@First@SelectFirst[Values[groups], Length[#] > 1 && repDim[#[[1]]] != 1 && (Length[#] > 2 || conjRep[#[[1]]] =!= dynkin[#[[2]]]) &]
      ],
      Association[Flatten[Table[
         Which[
            DimR[DefectRSymmetry[], k] == 1,
            Thread[groups[k] -> Table[k, Length[groups[k]]]],
            Length[groups[k]] == 1, 
            groups[k][[1]] -> k, 
            True,
            (*transformer[k] = With[{
               zero1 = SelectFirst[groups[singRep[DefectRSymmetry[]]], numInvariants[{groups[k][[2]], groups[k][[2]], conjRep[#]}] > 0 &],
               zero2 = SelectFirst[groups[singRep[DefectRSymmetry[]]], numInvariants[{groups[k][[2]], groups[k][[1]], conjRep[#]}] > 0 &]
            },
            	Components[Tensor[{{"C", Lowered[RIndex[zero1]], Raised[RIndex[groups[k][[2]]]], Raised[RIndex[groups[k][[2]]]]}}]][[1]] . Components[Tensor[{{"C", Raised[RIndex[zero2]], Lowered[RIndex[groups[k][[2]]]], Lowered[RIndex[groups[k][[1]]]]}}]][[1]]
            ];*)
            Thread[ReverseSort[groups[k]] -> {k, AlternateRep[k]}]
         ],
      {k, Keys[groups]}]]]
   ]
];

decomposeRepDefect[rep_] := branchingRules[][dynkin[rep]];
preimageReps[defectRep_] := Select[Keys[branchingRules[]], branchingRules[][#] == defectRep &];

(*transformer[rep_] := (branchingRules[]; transformer[rep]);*)

TwoPtRInvariant[rep1_, rep2_] := Tensor[{{"\[Delta]", Raised[toIndex[rep1]], Raised[toIndex[rep2]]}}];
ConjugateTwoPtRInvariant[rep1_, rep2_] := Tensor[{{"\[Delta]", Lowered[toIndex[rep1]], Lowered[toIndex[rep2]]}}];

ThreePtRInvariant[{rep1_, rep2_}, target_] := Tensor[{{"C", Raised[toIndex[target]], Lowered[toIndex[rep1]], Lowered[toIndex[rep2]]}}];
ConjugateThreePtRInvariant[{rep1_, rep2_}, target_] := Tensor[{{"C", Lowered[toIndex[target]], Raised[toIndex[rep1]], Raised[toIndex[rep2]]}}];

ThreePtRInvariant[r1_, r2_, r3_] := Tensor[{{"C", Raised[toIndex[r1]], Raised[toIndex[r2]], Raised[toIndex[r3]]}}];
ThreePtRInvariant[r1_, r2_, r3_] := Tensor[{{"C", Lowered[toIndex[r1]], Lowered[toIndex[r2]], Lowered[toIndex[r3]]}}];
     
BuildTensor[arg : {"\[Delta]", Raised[i1_], Raised[i2_]}] := twopt[i1, i2];
BuildTensor[arg : {"\[Delta]", Lowered[i1_], Lowered[i2_]}] := SparseArray@Inverse[Components@Tensor[{{"\[Delta]", Raised[i2], Raised[i1]}}]];

$customInvariants = False;

createDefectTwoPt[r1_, r2_] := Components[Tensor[{{"C", Lowered[DefectRIndex[singRep[DefectRSymmetry[]]]], Raised[DefectRIndex[r1]], Raised[DefectRIndex[r2]]}}]][[1]];
createDefectTwoPt[a1: AlternateRep[r1_], a2: AlternateRep[r2_]] := Components[Contract[Tensor[{{"\[Delta]", Raised[toIndex[a1]], Raised[toIndex[r1]]}, {"\[Delta]", Lowered[toIndex[r1]], Lowered[toIndex[r2]]}, {"\[Delta]", Raised[toIndex[r2]], Raised[toIndex[a2]]}}],{{2,3},{4,5}}]];
     
twopt::undefined = "The two-point invariant for representations (``, ``) has not been defined.";
twopt[RIndex[r1_], RIndex[r2_]] := twopt[r1, r2];
twopt[i1_DefectRIndex, i2_DefectRIndex] := twopt @@ (toRep /@ {i1, i2});
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r1 =!= dynkin[r1] := twopt[dynkin[r1], r2];
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r2 =!= dynkin[r2] := twopt[r1, dynkin[r2]];
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && !OrderedQ[{r1, r2}] := Transpose[twopt[r2, r1]];
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && OrderedQ[{r1, r2}] := twopt[r1, r2] =
If[$customInvariants,
    Message[twopt::undefined, r1, r2],
	With[{grp = appropriateGroup[r1]},
	   If[grp == RSymmetry[],
	      IrrepInProduct[RSymmetry[], {r1, r2}, singRep[RSymmetry[]], TensorForm -> True][[1,1,;;,;;,1]],
	      With[{pair = SelectFirst[Tuples[preimageReps /@ {r1, r2}], numInvariants[#] > 0 &]},
	         If[MissingQ[pair],
	            createDefectTwoPt[r1, r2],
	         	twopt @@ pair
	         ]
	      ]
	   ]
   ]
];
 
threept::undefined = "The three-point invariant for representations (``, ``, ``) has not been defined.";
threept[RIndex[r1_], RIndex[r2_], RIndex[r3_]] := threept[r1, r2, r3];
threept[i1_DefectRIndex, i2_DefectRIndex, i3_DefectRIndex] := threept @@ (toRep /@ {i1, i2, i3});
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r1 =!= dynkin[r1] := threept[dynkin[r1], r2, r3];
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r2 =!= dynkin[r2] := threept[r1, dynkin[r2], r3];
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r3 =!= dynkin[r3] := threept[r1, r2, dynkin[r3]];
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && r3 === dynkin[r3] && !OrderedQ[{r1, r2, r3}] := 
   TensorTranspose[threept @@ Sort[{r1,r2,r3}], Ordering[{r1,r2,r3}]];
   
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && r3 === dynkin[r3] && OrderedQ[{r1, r2, r3}] := threept[r1, r2, r3] =
If[$customInvariants,
    Message[threept::undefined, r1, r2, r3],
      With[{grp = appropriateGroup[r1]},
		   If[grp == RSymmetry[],
		      IrrepInProduct[RSymmetry[], {r1, r2}, r3, ConjugateTargetRep -> True, TensorForm -> True][[1,1]],
		      With[{trip = SelectFirst[Tuples[preimageReps /@ {r1, r2, r3}], numInvariants[#] > 0 &]},
		         threept @@ trip
		      ]
		   ]
      ]
];

SetTwoPtRInvariant[r1_, r2_, mat_] := Module[{reps = dynkin /@ ({r1, r2}), sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = Ordering[reps];
 	twopt[sorted[[1]], sorted[[2]]] = SparseArray@TensorTranspose[mat, order];
]

SetThreePtRInvariant[r1_, r2_, r3_, mat_] := Module[{reps = dynkin /@ {r1, r2, r3}, sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = InversePermutation@Ordering[reps];
 	threept[sorted[[1]], sorted[[2]], sorted[[3]]] = SparseArray@TensorTranspose[mat, order];
]

BuildTensor[{"C", Raised[i1_], Raised[i2_], Raised[i3_]}] := threept[i1, i2, i3];
BuildTensor[{"C", Lowered[i1_], Raised[i2_], Raised[i3_]}] := 
    TensorTranspose[Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[conjIndex[i1]], Raised[i2], Raised[i3]}}], 
       Tensor[{{"\[Delta]", Lowered[conjIndex[i1]], Lowered[i1]}}]
    ], {{1, 4}}]], {2,3,1}];

BuildTensor[{"C", Raised[i1_], Lowered[i2_], Lowered[i3_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Lowered[conjIndex[i1]], Raised[conjIndex[i2]], Raised[conjIndex[i3]]}}], 
       Tensor[{{"\[Delta]", Raised[conjIndex[i1]], Raised[i1]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i2]], Lowered[i2]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i3]], Lowered[i3]}}]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
BuildTensor[{"C", Lowered[i1_], Lowered[i2_], Lowered[i3_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[conjIndex[i1]], Raised[conjIndex[i2]], Raised[conjIndex[i3]]}}], 
       Tensor[{{"\[Delta]", Lowered[conjIndex[i1]], Lowered[i1]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i2]], Lowered[i2]}}],
       Tensor[{{"\[Delta]", Lowered[conjIndex[i3]], Lowered[i3]}}]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
      
buildExpression[LoopInvariant[edges_]] := 
 Module[{internals = SortBy[DeleteDuplicates@Cases[edges, _Internal, All], FirstCase[edges, {#, External[i_], _} :> i] &], pi, cs, deltas},
  pi = PositionIndex[internals];
  cs = Association@
    Thread[internals -> 
      Table[ThreePtRInvariant[{FirstCase[edges, {_Internal, i, rep_} :> conjRep[rep]], 
         FirstCase[edges, {i, _Internal, rep_} :> rep]}, 
        FirstCase[edges, {i, _External, rep_} :> rep]], {i, 
        internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtRInvariant[rep, conjRep[rep]]}];
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
           conjRep[rep]]]], {i, internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtRInvariant[rep, conjRep[rep]]}];
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
         appropriateGroup[reps[[1]]], {r, reps[[i + 1]]}], #[[2]] == 1 &][[;; , 1]], 
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
    {start, dynkin /@ allReps[]},
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
     Select[ReduceRepProduct[appropriateGroup[reps[[1]]], reps[[p[[;; 2]]]]], #[[2]] == 1 &][[;; , 1]], 
     conjRep /@ (Select[ReduceRepProduct[appropriateGroup[reps[[1]]], reps[[p[[3 ;;]]]]], #[[2]] == 1 &][[;; , 1]])]}
   ];

appropriateGroup[AlternateRep[rep_]] := appropriateGroup[rep];
appropriateGroup[rep_] := If[Quiet@Check[fundRep[RSymmetry[]] + rep; True, False], RSymmetry[], DefectRSymmetry[]];
repDim[rep_] := Times @@ DimR[appropriateGroup[rep], rep];
repName[rep_] := RepName[appropriateGroup[rep], rep];
dynkin[rep_] := SimpleRepInputConversion[appropriateGroup[rep], rep];
dynkin[AlternateRep[rep_]] := AlternateRep[dynkin[rep]];
conjRep[rep_] := ConjugateIrrep[appropriateGroup[rep], rep];
toIndex[rep_] := If[appropriateGroup[rep] == RSymmetry[], RIndex[rep], If[MatchQ[rep, AlternateRep[_]], DefectRIndex[rep[[1]], "Alternate" -> True], DefectRIndex[rep]]];
conjIndex[RIndex[rep_]] := RIndex[conjRep[rep]];
conjIndex[DefectRIndex[rep_, opt: OptionsPattern[]]] := DefectRIndex[conjRep[rep], "Alternate" -> If[MemberQ[Values[branchingRules[]], AlternateRep[rep]], !OptionValue[DefectRIndex, {opt}, "Alternate"], False]];
(*conjIndex[DefectRIndex[rep_, opt: OptionsPattern[]]] := DefectRIndex[conjRep[rep], "Alternate" -> OptionValue[DefectRIndex, {opt}, "Alternate"]];*)
toRep[RIndex[rep_]] := rep;
toRep[DefectRIndex[rep_, opt:OptionsPattern[]]] := If[OptionValue[DefectRIndex, {opt}, "Alternate"], AlternateRep, Identity][rep];

numInvariants[reps_] := numInvariants[reps] =
   With[{grp = appropriateGroup[reps[[1]]]},
  	 If[# == {}, 0, #[[1, 2]]] &@ Select[ReduceRepProduct[grp, reps], #[[1]] == singRep[grp] &]
	];

InvariantFourPtGraphs[reps_] /; Length[reps] == 4 := InvariantFourPtGraphs[reps] = 
 With[{graphs = SortBy[Join[treeGraphs[reps], loopGraphs[reps]], {Max[Cases[#[[1]], {_Internal, _Internal, rep_} :> Times @@ repDim[rep]]], 
  Length[Cases[#[[1]], _Internal, All]]} &]},
    graphs[[IndependentSet[buildExpression /@ graphs, "MaxIndependent" -> numInvariants[reps], "Indices" -> True]]]
  ]
   
InvariantFourPts[reps_] /; Length[reps] == 4 := InvariantFourPts[reps] = With[{order = Ordering[dynkin /@ reps]},
   SparseArray@(TensorTranspose[CanonicallyOrderedComponents@buildExpression[#], order] & /@ InvariantFourPtGraphs[reps])
];

allReps[] := DeleteDuplicates[RRep /@ Flatten[Multiplet /@ $multipletIndices]];
  
FourPtRInvariant[reps_, i_] := Tensor[{{RInvariant[i], Sequence @@ Table[Raised[If[appropriateGroup[reps[[1]]] == RSymmetry[], RIndex, DefectRIndex][r]], {r, reps}]}}];
BuildTensor[{RInvariant[i_], Raised[RIndex[r1_]], Raised[RIndex[r2_]], Raised[RIndex[r3_]], Raised[RIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];
BuildTensor[{RInvariant[i_], Raised[DefectRIndex[r1_]], Raised[DefectRIndex[r2_]], Raised[DefectRIndex[r3_]], Raised[DefectRIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];