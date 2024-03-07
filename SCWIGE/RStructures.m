(* Wolfram Language package *)

decomposeRepDefect::split = "When `1` is embedded into `2`, the representation `3` of `2` splits into several representations of `1`: `4`. This case has not yet been implemented.";
decomposeRepDefect[rep_] := With[{decomp = DecomposeRep[$RSymmetry, rep, $DefectRSymmetry, Embeddings[$RSymmetry, $DefectRSymmetry][[1,2]]]},
	If[Length[decomp] > 1, Message[decomposeRepDefect::manyembed, CMtoName[$DefectRSymmetry], CMtoName[$RSymmetry], RepName[$RSymmetry, rep], RepNameBatchMode[$DefectRSymmetry, defectFund]]; Return[]];
	decomp[[1]]   
];

TwoPtRInvariant[rep1_, rep2_] :=
   If[appropriateGroup[rep1] == RSymmetry[],
   	Tensor[{{"\[Delta]", Raised[RIndex[rep1]], Raised[RIndex[rep2]]}}],
   	Tensor[{{"\[Delta]", Raised[DefectRIndex[rep1]], Raised[DefectRIndex[rep2]]}}]
   ];
ConjugateTwoPtRInvariant[rep1_, rep2_] :=
   If[appropriateGroup[rep1] == RSymmetry[],
   	Tensor[{{"\[Delta]", Lowered[RIndex[rep1]], Lowered[RIndex[rep2]]}}],
   	Tensor[{{"\[Delta]", Lowered[DefectRIndex[rep1]], Lowered[DefectRIndex[rep2]]}}]
   ];

ThreePtRInvariant[{rep1_, rep2_}, target_] :=
   If[appropriateGroup[rep1] == RSymmetry[],
	   Tensor[{{"C", Raised[RIndex[target]], Lowered[RIndex[rep1]], 
	      Lowered[RIndex[rep2]]}}],
	   Tensor[{{"C", Raised[DefectRIndex[target]], Lowered[DefectRIndex[rep1]], 
	      Lowered[DefectRIndex[rep2]]}}]
   ];
ConjugateThreePtRInvariant[{rep1_, rep2_}, target_] :=
   If[appropriateGroup[rep1] == RSymmetry[],
	   Tensor[{{"C", Lowered[RIndex[target]], Raised[RIndex[rep1]], 
	      Raised[RIndex[rep2]]}}],
	   Tensor[{{"C", Lowered[DefectRIndex[target]], Raised[DefectRIndex[rep1]], 
	      Raised[DefectRIndex[rep2]]}}]
   ];

ThreePtRInvariant[r1_, r2_, r3_] :=
	If[appropriateGroup[r1] == RSymmetry[],
   		Tensor[{{"C", Raised[RIndex[r1]], Raised[RIndex[r2]], Raised[RIndex[r3]]}}],
   		Tensor[{{"C", Raised[DefectRIndex[r1]], Raised[DefectRIndex[r2]], Raised[DefectRIndex[r3]]}}]
	];
ConjugateThreePtRInvariant[r1_, r2_, r3_] :=
	If[appropriateGroup[r1] == RSymmetry[],
		Tensor[{{"C", Lowered[RIndex[r1]], Lowered[RIndex[r2]], Lowered[RIndex[r3]]}}],
		Tensor[{{"C", Lowered[DefectRIndex[r1]], Lowered[DefectRIndex[r2]], Lowered[DefectRIndex[r3]]}}]
	];
     
BuildTensor[arg : {"\[Delta]", Raised@RIndex[r1_], Raised@RIndex[r2_]}] := twopt[r1, r2];
BuildTensor[arg : {"\[Delta]", Raised@DefectRIndex[r1_], Raised@DefectRIndex[r2_]}] := twopt[r1, r2];

BuildTensor[arg : {"\[Delta]", Lowered@RIndex[r1_], Lowered@RIndex[r2_]}] := SparseArray@Inverse[twopt[r2, r1]];
BuildTensor[arg : {"\[Delta]", Lowered@DefectRIndex[r1_], Lowered@DefectRIndex[r2_]}] := SparseArray@Inverse[twopt[r2, r1]];

$customInvariants = False;
     
twopt::undefined = "The two-point invariant for representations (``, ``) has not been defined.";
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 =!= dynkin[r1] := twopt[dynkin[r1], r2];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r2 =!= dynkin[r2] := twopt[r1, dynkin[r2]];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && !OrderedQ[{r1, r2}] := Transpose[twopt[r2, r1]];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && OrderedQ[{r1, r2}] := 
If[$customInvariants,
   Message[twopt::undefined, repName[r1], repName[r2]];,
   With[{grp = appropriateGroup[r1]},
   	SparseArray[IrrepInProduct[grp, {r1, r2}, singRep[grp], TensorForm -> True][[1, 1, ;; , ;; , 1]]]
   ]
];
 
threept::undefined = "The three-point invariant for representations (``, ``, ``) has not been defined.";
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 =!= dynkin[r1] :=
   threept[dynkin[r1], r2, r3];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r2 =!= dynkin[r2] :=
   threept[r1, dynkin[r2], r3];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r3 =!= dynkin[r3] :=
   threept[r1, r2, dynkin[r3]];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && r3 === dynkin[r3] && !OrderedQ[{r1, r2, r3}] := 
   TensorTranspose[threept @@ Sort[{r1,r2,r3}], Ordering[{r1,r2,r3}]];
   
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && r3 === dynkin[r3] && OrderedQ[{r1, r2, r3}] := 
If[$customInvariants,
   Message[threept::undefined, repName[r1], repName[r2], repName[r3]];,
   With[{grp = appropriateGroup[r1]},
   	SparseArray[IrrepInProduct[grp, {r1, r2}, r3, ConjugateTargetRep -> True, TensorForm -> True][[1, 1]]]
   ]
];

SetTwoPtRInvariant[r1_, r2_, mat_] := Module[{reps = dynkin /@ {r1, r2}, sorted, order},
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

BuildTensor[
   arg : {"C", Raised@RIndex[target_], Raised@RIndex[rep1_], 
     Raised@RIndex[rep2_]}] := threept[target, rep1, rep2];
BuildTensor[
   arg : {"C", Raised@DefectRIndex[target_], Raised@DefectRIndex[rep1_], 
     Raised@DefectRIndex[rep2_]}] := threept[target, rep1, rep2];

BuildTensor[
   arg : {"C", Lowered@RIndex[target_], Raised@RIndex[rep1_], 
     Raised@RIndex[rep2_]}] :=
    Components[ConjugateTwoPtRInvariant[target, conjRep[target]]] . threept[conjRep[target], rep1, rep2];
BuildTensor[
   arg : {"C", Lowered@DefectRIndex[target_], Raised@DefectRIndex[rep1_], 
     Raised@DefectRIndex[rep2_]}] :=
    Components[ConjugateTwoPtRInvariant[target, conjRep[target]]] . threept[conjRep[target], rep1, rep2];

BuildTensor[
   arg : {"C", Raised@RIndex[target_], Lowered@RIndex[rep1_], 
     Lowered@RIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Lowered[RIndex[conjRep[target]]], Raised[RIndex[conjRep[rep1]]], Raised[RIndex[conjRep[rep2]]]}}], 
       TwoPtRInvariant[conjRep[target], target],
       ConjugateTwoPtRInvariant[conjRep[rep1], rep1],
       ConjugateTwoPtRInvariant[conjRep[rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
BuildTensor[
   arg : {"C", Raised@DefectRIndex[target_], Lowered@DefectRIndex[rep1_], 
     Lowered@DefectRIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Lowered[RIndex[conjRep[target]]], Raised[RIndex[conjRep[rep1]]], Raised[RIndex[conjRep[rep2]]]}}], 
       TwoPtRInvariant[conjRep[target], target],
       ConjugateTwoPtRInvariant[conjRep[rep1], rep1],
       ConjugateTwoPtRInvariant[conjRep[rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
    
BuildTensor[
   arg : {"C", Lowered@RIndex[target_], Lowered@RIndex[rep1_], 
     Lowered@RIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[RIndex[conjRep[target]]], Raised[RIndex[conjRep[rep1]]], Raised[RIndex[conjRep[rep2]]]}}], 
       ConjugateTwoPtRInvariant[conjRep[target], target],
       ConjugateTwoPtRInvariant[conjRep[rep1], rep1],
       ConjugateTwoPtRInvariant[conjRep[rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
BuildTensor[
   arg : {"C", Lowered@DefectRIndex[target_], Lowered@DefectRIndex[rep1_], 
     Lowered@DefectRIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[RIndex[conjRep[target]]], Raised[RIndex[conjRep[rep1]]], Raised[RIndex[conjRep[rep2]]]}}], 
       ConjugateTwoPtRInvariant[conjRep[target], target],
       ConjugateTwoPtRInvariant[conjRep[rep1], rep1],
       ConjugateTwoPtRInvariant[conjRep[rep2], rep2]
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

appropriateGroup[rep_] := If[Quiet@Check[fundRep[RSymmetry[]] + rep; True, False], RSymmetry[], DefectRSymmetry[]];
repDim[rep_] := DimR[appropriateGroup[rep], rep];
repName[rep_] := RepName[appropriateGroup[rep], rep];
dynkin[rep_] := SimpleRepInputConversion[appropriateGroup[rep], rep];
conjRep[rep_] := ConjugateIrrep[appropriateGroup[rep], rep];

numInvariants[reps_] := 
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