(* Wolfram Language package *)

decomposeRepDefectFP::split = "When `1` is embedded into `2`, the representation `3` of `2` splits into several representations of `1`: `4`. This case has not yet been implemented.";
decomposeRepDefectFP[rep_] := With[{decomp = DecomposeRep[$RSymmetry, rep, $DefectRSymmetry, Embeddings[$RSymmetry, $DefectRSymmetry][[1,2]]]},
	If[Length[decomp] > 1, Message[decomposeRepDefectFP::manyembed, CMtoName[$DefectRSymmetry], CMtoName[$RSymmetry], RepName[$RSymmetry, rep], RepNameBatchMode[$DefectRSymmetry, defectFund]]; Return[]];
	decomp[[1]]   
];

branchingRules::overlap = "The representations `1` all branch to `2`, and are not related by conjugacy. This case has not yet been implemented.";
branchingRules[] := With[{groups = GroupBy[Flatten[Table[dynkin@*GlobalRep /@ Multiplet[idx], {idx, $multipletIndices}], 1], decomposeRepDefectFP]},
   If[AnyTrue[Values[groups], Length[#] > 1 && repDim[#[[1]]] != 1 && (Length[#] > 2 || conjRep[#[[1]]] =!= dynkin[#[[2]]]) &],
      Message[branchingRules::overlap, 
         SelectFirst[Values[groups], Length[#] > 1 && repDim[#[[1]]] != 1 && (Length[#] > 2 || conjRep[#[[1]]] =!= dynkin[#[[2]]]) &], 
         decomposeRepDefectFP@First@SelectFirst[Values[groups], Length[#] > 1 && repDim[#[[1]]] != 1 && (Length[#] > 2 || conjRep[#[[1]]] =!= dynkin[#[[2]]]) &]
      ],
      Association[Flatten[Table[
         Which[
            DimR[DefectGlobalSymmetry[], k] == 1,
            Thread[groups[k] -> Table[k, Length[groups[k]]]],
            Length[groups[k]] == 1, 
            groups[k][[1]] -> k, 
            True,
            (*transformer[k] = With[{
               zero1 = SelectFirst[groups[singRep[DefectGlobalSymmetry[]]], numInvariants[{groups[k][[2]], groups[k][[2]], conjRep[#]}] > 0 &],
               zero2 = SelectFirst[groups[singRep[DefectGlobalSymmetry[]]], numInvariants[{groups[k][[2]], groups[k][[1]], conjRep[#]}] > 0 &]
            },
            	Components[Tensor[{{"C", Lowered[GlobalIndex[zero1]], Raised[GlobalIndex[groups[k][[2]]]], Raised[GlobalIndex[groups[k][[2]]]]}}]][[1]] . Components[Tensor[{{"C", Raised[GlobalIndex[zero2]], Lowered[GlobalIndex[groups[k][[2]]]], Lowered[GlobalIndex[groups[k][[1]]]]}}]][[1]]
            ];*)
            Thread[ReverseSort[groups[k]] -> {k, AlternateRep[k]}]
         ],
      {k, Keys[groups]}]]]
   ]
];

decomposeRepDefect[rep_] := branchingRules[][dynkin[rep]];
preimageReps[defectRep_] := Select[Keys[branchingRules[]], branchingRules[][#] == defectRep &];

(*transformer[rep_] := (branchingRules[]; transformer[rep]);*)

TwoPtGlobalInvariant[rep1_, rep2_] := Tensor[{{"\[Delta]", Raised[toIndex[rep1]], Raised[toIndex[rep2]]}}];
ConjugateTwoPtGlobalInvariant[rep1_, rep2_] := Tensor[{{"\[Delta]", Lowered[toIndex[rep1]], Lowered[toIndex[rep2]]}}];

ThreePtGlobalInvariant[{rep1_, rep2_}, target_] := Tensor[{{"C", Raised[toIndex[target]], Lowered[toIndex[rep1]], Lowered[toIndex[rep2]]}}];
ConjugateThreePtGlobalInvariant[{rep1_, rep2_}, target_] := Tensor[{{"C", Lowered[toIndex[target]], Raised[toIndex[rep1]], Raised[toIndex[rep2]]}}];

ThreePtGlobalInvariant[r1_, r2_, r3_] := Tensor[{{"C", Raised[toIndex[r1]], Raised[toIndex[r2]], Raised[toIndex[r3]]}}];
ThreePtGlobalInvariant[r1_, r2_, r3_] := Tensor[{{"C", Lowered[toIndex[r1]], Lowered[toIndex[r2]], Lowered[toIndex[r3]]}}];
     
BuildTensor[arg : {"\[Delta]", Raised[i1_], Raised[i2_]}] := twopt[i1, i2];
BuildTensor[arg : {"\[Delta]", Lowered[i1_], Lowered[i2_]}] := SparseArray@Inverse[Components@Tensor[{{"\[Delta]", Raised[i2], Raised[i1]}}]];

$customInvariants = False;

createDefectTwoPt[r1_, r2_] := Components[Tensor[{{"C", Lowered[DefectGlobalIndex[singRep[DefectGlobalSymmetry[]]]], Raised[DefectGlobalIndex[r1]], Raised[DefectGlobalIndex[r2]]}}]][[1]];
createDefectTwoPt[a1: AlternateRep[r1_], a2: AlternateRep[r2_]] := Components[Contract[Tensor[{{"\[Delta]", Raised[toIndex[a1]], Raised[toIndex[r1]]}, {"\[Delta]", Lowered[toIndex[r1]], Lowered[toIndex[r2]]}, {"\[Delta]", Raised[toIndex[r2]], Raised[toIndex[a2]]}}],{{2,3},{4,5}}]];
     
twopt::undefined = "The two-point invariant for representations (``, ``) has not been defined.";
twopt[GlobalIndex[r1_], GlobalIndex[r2_]] := twopt[r1, r2];
twopt[i1_DefectGlobalIndex, i2_DefectGlobalIndex] := twopt @@ (toRep /@ {i1, i2});
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r1 =!= dynkin[r1] := twopt[dynkin[r1], r2];
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r2 =!= dynkin[r2] := twopt[r1, dynkin[r2]];
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && !OrderedQ[{r1, r2}] := Transpose[twopt[r2, r1]];
twopt[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && OrderedQ[{r1, r2}] := twopt[r1, r2] =
If[$customInvariants,
    Message[twopt::undefined, r1, r2],
	With[{grp = appropriateGroup[r1]},
	   If[grp == GlobalSymmetry[],
	      IrrepInProduct[GlobalSymmetry[], {r1, r2}, singRep[GlobalSymmetry[]], TensorForm -> True][[1,1,;;,;;,1]],
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
threept[GlobalIndex[r1_], GlobalIndex[r2_], GlobalIndex[r3_]] := threept[r1, r2, r3];
threept[i1_DefectGlobalIndex, i2_DefectGlobalIndex, i3_DefectGlobalIndex] := threept @@ (toRep /@ {i1, i2, i3});
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r1 =!= dynkin[r1] := threept[dynkin[r1], r2, r3];
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r2 =!= dynkin[r2] := threept[r1, dynkin[r2], r3];
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r3 =!= dynkin[r3] := threept[r1, r2, dynkin[r3]];
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && r3 === dynkin[r3] && !OrderedQ[{r1, r2, r3}] := 
   TensorTranspose[threept @@ Sort[{r1,r2,r3}], Ordering[{r1,r2,r3}]];
   
threept[r1 : (_Integer | _List | _AlternateRep), r2 : (_Integer | _List | _AlternateRep), r3 : (_Integer | _List | _AlternateRep)] /; r1 === dynkin[r1] && r2 === dynkin[r2] && r3 === dynkin[r3] && OrderedQ[{r1, r2, r3}] := threept[r1, r2, r3] =
If[$customInvariants,
    Message[threept::undefined, r1, r2, r3],
      With[{grp = appropriateGroup[r1]},
		   If[grp == GlobalSymmetry[],
		      IrrepInProduct[GlobalSymmetry[], {r1, r2}, r3, ConjugateTargetRep -> True, TensorForm -> True][[1,1]],
		      With[{trip = SelectFirst[Tuples[preimageReps /@ {r1, r2, r3}], numInvariants[#] > 0 &]},
		         threept @@ trip
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

SetThreePtGlobalInvariant[r1_, r2_, r3_, mat_] := Module[{reps = dynkin /@ {r1, r2, r3}, sorted, order},
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
loopGraphs[reps_] := loopGraphs[reps] = Flatten@Table[
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

appropriateGroup[AlternateRep[rep_]] := appropriateGroup[rep];
appropriateGroup[rep_] := If[Quiet@Check[fundRep[GlobalSymmetry[]] + rep; True, False], GlobalSymmetry[], DefectGlobalSymmetry[]];
repDim[rep_] := Times @@ DimR[appropriateGroup[rep], rep];
repName[rep_] := RepName[appropriateGroup[rep], rep];
dynkin[rep_] := SimpleRepInputConversion[appropriateGroup[rep], rep];
dynkin[AlternateRep[rep_]] := AlternateRep[dynkin[rep]];
conjRep[rep_] := ConjugateIrrep[appropriateGroup[rep], rep];
toIndex[rep_] := If[appropriateGroup[rep] == GlobalSymmetry[], GlobalIndex[rep], If[MatchQ[rep, AlternateRep[_]], DefectGlobalIndex[rep[[1]], "Alternate" -> True], DefectGlobalIndex[rep]]];
conjIndex[GlobalIndex[rep_]] := GlobalIndex[conjRep[rep]];
conjIndex[DefectGlobalIndex[rep_, opt: OptionsPattern[]]] := DefectGlobalIndex[conjRep[rep], "Alternate" -> If[MemberQ[Values[branchingRules[]], AlternateRep[rep]], !OptionValue[DefectGlobalIndex, {opt}, "Alternate"], False]];
(*conjIndex[DefectGlobalIndex[rep_, opt: OptionsPattern[]]] := DefectGlobalIndex[conjRep[rep], "Alternate" -> OptionValue[DefectGlobalIndex, {opt}, "Alternate"]];*)
toRep[GlobalIndex[rep_]] := rep;
toRep[DefectGlobalIndex[rep_, opt:OptionsPattern[]]] := If[OptionValue[DefectGlobalIndex, {opt}, "Alternate"], AlternateRep, Identity][rep];

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

allReps[] := DeleteDuplicates[GlobalRep /@ Flatten[Multiplet /@ $multipletIndices]];
  
FourPtGlobalInvariant[reps_, i_] := Tensor[{{GlobalInvariant[i], Sequence @@ Table[Raised[If[appropriateGroup[reps[[1]]] == GlobalSymmetry[], GlobalIndex, DefectGlobalIndex][r]], {r, reps}]}}];
BuildTensor[{GlobalInvariant[i_], Raised[GlobalIndex[r1_]], Raised[GlobalIndex[r2_]], Raised[GlobalIndex[r3_]], Raised[GlobalIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];
BuildTensor[{GlobalInvariant[i_], Raised[DefectGlobalIndex[r1_]], Raised[DefectGlobalIndex[r2_]], Raised[DefectGlobalIndex[r3_]], Raised[DefectGlobalIndex[r4_]]}] := InvariantFourPts[{r1, r2, r3, r4}][[i]];