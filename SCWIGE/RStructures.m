(* Wolfram Language package *)

TwoPtRInvariant[rep1_, rep2_] :=
   Tensor[{{"\[Delta]", Raised[RIndex[rep1]], Raised[RIndex[rep2]]}}];
ConjugateTwoPtRInvariant[rep1_, rep2_] :=
   Tensor[{{"\[Delta]", Lowered[RIndex[rep1]], Lowered[RIndex[rep2]]}}];

ThreePtRInvariant[{rep1_, rep2_}, target_] :=
   Tensor[{{"C", Raised[RIndex[target]], Lowered[RIndex[rep1]], 
      Lowered[RIndex[rep2]]}}];
ConjugateThreePtRInvariant[{rep1_, rep2_}, target_] :=
   Tensor[{{"C", Lowered[RIndex[target]], Raised[RIndex[rep1]], 
      Raised[RIndex[rep2]]}}];

ThreePtRInvariant[r1_, r2_, r3_] :=
   Tensor[{{"C", Raised[RIndex[r1]], Raised[RIndex[r2]], Raised[RIndex[r3]]}}];
ConjugateThreePtRInvariant[r1_, r2_, r3_] :=
   Tensor[{{"C", Lowered[RIndex[r1]], Lowered[RIndex[r2]], Lowered[RIndex[r3]]}}];
     
BuildTensor[arg : {"\[Delta]", Raised@RIndex[r1_], Raised@RIndex[r2_]}] := twopt[r1, r2];

BuildTensor[arg : {"\[Delta]", Lowered@RIndex[r1_], Lowered@RIndex[r2_]}] := SparseArray@Inverse[twopt[r2, r1]];

$customInvariants = False;
     
twopt::undefined = "The two-point invariant for representations (``, ``) has not been defined.";
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 =!= SimpleRepInputConversion[RSymmetry[], r1] := twopt[SimpleRepInputConversion[RSymmetry[], r1], r2];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r2 =!= SimpleRepInputConversion[RSymmetry[], r2] := twopt[r1, SimpleRepInputConversion[RSymmetry[], r2]];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && !OrderedQ[{r1, r2}] := Transpose[twopt[r2, r1]];
twopt[r1 : (_Integer | _List), r2 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && OrderedQ[{r1, r2}] := 
If[$customInvariants,
   Message[twopt::undefined, RepName[RSymmetry[], r1], RepName[RSymmetry[], r2]];,
   SparseArray[IrrepInProduct[$RSymmetry, {r1, r2}, singRep[$RSymmetry], TensorForm -> True][[1, 1, ;; , ;; , 1]]]
];
 
threept::undefined = "The three-point invariant for representations (``, ``, ``) has not been defined.";
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 =!= SimpleRepInputConversion[RSymmetry[], r1] :=
   threept[SimpleRepInputConversion[RSymmetry[], r1], r2, r3];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r2 =!= SimpleRepInputConversion[RSymmetry[], r2] :=
   threept[r1, SimpleRepInputConversion[RSymmetry[], r2], r3];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r3 =!= SimpleRepInputConversion[RSymmetry[], r3] :=
   threept[r1, r2, SimpleRepInputConversion[RSymmetry[], r3]];
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && r3 === SimpleRepInputConversion[RSymmetry[], r3] && !OrderedQ[{r1, r2, r3}] := 
   TensorTranspose[threept @@ Sort[{r1,r2,r3}], Ordering[{r1,r2,r3}]];
   
threept[r1 : (_Integer | _List), r2 : (_Integer | _List), r3 : (_Integer | _List)] /; r1 === SimpleRepInputConversion[RSymmetry[], r1] && r2 === SimpleRepInputConversion[RSymmetry[], r2] && r3 === SimpleRepInputConversion[RSymmetry[], r3] && OrderedQ[{r1, r2, r3}] := 
If[$customInvariants,
   Message[threept::undefined, RepName[RSymmetry[], r1], RepName[RSymmetry[], r2], RepName[RSymmetry[], r3]];,
   SparseArray[IrrepInProduct[$RSymmetry, {r1, r2}, r3, ConjugateTargetRep -> True, TensorForm -> True][[1, 1]]]
];

SetTwoPtRInvariant[r1_, r2_, mat_] := Module[{reps = SimpleRepInputConversion[RSymmetry[], #] & /@ {r1, r2}, sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = Ordering[reps];
 	twopt[sorted[[1]], sorted[[2]]] = SparseArray@TensorTranspose[mat, order];
]

SetThreePtRInvariant[r1_, r2_, r3_, mat_] := Module[{reps = SimpleRepInputConversion[RSymmetry[], #] & /@ {r1, r2, r3}, sorted, order},
    $customInvariants = True;
 	sorted = Sort[reps];
 	order = InversePermutation@Ordering[reps];
 	threept[sorted[[1]], sorted[[2]], sorted[[3]]] = SparseArray@TensorTranspose[mat, order];
]

BuildTensor[
   arg : {"C", Raised@RIndex[target_], Raised@RIndex[rep1_], 
     Raised@RIndex[rep2_]}] := threept[target, rep1, rep2];

BuildTensor[
   arg : {"C", Lowered@RIndex[target_], Raised@RIndex[rep1_], 
     Raised@RIndex[rep2_]}] :=
    Components[ConjugateTwoPtRInvariant[target, ConjugateIrrep[$RSymmetry, target]]] . threept[ConjugateIrrep[$RSymmetry, target], rep1, rep2];

BuildTensor[
   arg : {"C", Raised@RIndex[target_], Lowered@RIndex[rep1_], 
     Lowered@RIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Lowered[RIndex[ConjugateIrrep[$RSymmetry, target]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep1]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep2]]]}}], 
       TwoPtRInvariant[ConjugateIrrep[$RSymmetry, target], target],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep1], rep1],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
    
BuildTensor[
   arg : {"C", Lowered@RIndex[target_], Lowered@RIndex[rep1_], 
     Lowered@RIndex[rep2_]}] :=
    Components[Contract[TensorProduct[
       Tensor[{{"C", Raised[RIndex[ConjugateIrrep[$RSymmetry, target]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep1]]], Raised[RIndex[ConjugateIrrep[$RSymmetry, rep2]]]}}], 
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, target], target],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep1], rep1],
       ConjugateTwoPtRInvariant[ConjugateIrrep[$RSymmetry, rep2], rep2]
    ], {{1, 4}, {2, 6}, {3, 8}}]];
      
buildExpression[LoopInvariant[edges_]] := 
 Module[{internals = SortBy[DeleteDuplicates@Cases[edges, _Internal, All], FirstCase[edges, {#, External[i_], _} :> i] &], pi, cs, deltas},
  pi = PositionIndex[internals];
  cs = Association@
    Thread[internals -> 
      Table[ThreePtRInvariant[{FirstCase[edges, {_Internal, i, rep_} :> ConjugateIrrep[$RSymmetry, rep]], 
         FirstCase[edges, {i, _Internal, rep_} :> rep]}, 
        FirstCase[edges, {i, _External, rep_} :> rep]], {i, 
        internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtRInvariant[rep, ConjugateIrrep[$RSymmetry, rep]]}];
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
           ConjugateIrrep[$RSymmetry, rep]]]], {i, internals}]];
  deltas = 
   Cases[edges, {i_Internal, j_Internal, rep_} :> {i, j, 
      TwoPtRInvariant[rep, ConjugateIrrep[$RSymmetry, rep]]}];
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
         $RSymmetry, {r, reps[[i + 1]]}], #[[2]] == 1 &][[;; , 1]], 
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
    {start, 
     SimpleRepInputConversion[$RSymmetry, #] & /@ allReps[]},
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
     Select[ReduceRepProduct[$RSymmetry, reps[[p[[;; 2]]]]], #[[2]] == 1 &][[;; , 1]], 
     ConjugateIrrep[$RSymmetry, #] & /@ (Select[ReduceRepProduct[$RSymmetry, reps[[p[[3 ;;]]]]], #[[2]] == 1 &][[;; , 1]])]}
   ];
   
numInvariants[reps_] := If[# == {}, 0, #[[1, 2]]] &@ Select[ReduceRepProduct[RSymmetry[], reps], #[[1]] == singRep[RSymmetry[]] &];

InvariantFourPtGraphs[reps_] /; Length[reps] == 4 := InvariantFourPtGraphs[reps] = 
 With[{graphs = SortBy[Join[treeGraphs[reps], loopGraphs[reps]], {Max[Cases[#[[1]], {_Internal, _Internal, rep_} :> Times @@ DimR[RSymmetry[], rep]]], 
  Length[Cases[#[[1]], _Internal, All]]} &]},
    graphs[[IndependentSet[buildExpression /@ graphs, "MaxIndependent" -> numInvariants[reps], "Indices" -> True]]]
  ]
   
InvariantFourPts[reps_] /; Length[reps] == 4 := InvariantFourPts[reps] = With[{order = Ordering[SimpleRepInputConversion[$RSymmetry, #]& /@ reps]},
   SparseArray@(TensorTranspose[CanonicallyOrderedComponents@buildExpression[#], order] & /@ InvariantFourPtGraphs[reps])
];

allReps[] := DeleteDuplicates[RRep /@ Flatten[Multiplet /@ $multipletIndices]];
  
FourPtRInvariant[reps_, i_] := Tensor[{{RInvariant[i], Sequence @@ Table[Raised[RIndex[r]], {r, reps}]}}];
BuildTensor[{RInvariant[i_], Raised[RIndex[r1_]], Raised[RIndex[r2_]], Raised[RIndex[r3_]], Raised[RIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];