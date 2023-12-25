(* Wolfram Language package *)

RPart[Tensor[names_]] := 
  Tensor[Select[names, 
    MatchQ[#[[1]], RInvariant[_]] || 
      MemberQ[{"C", "\[Delta]"}, #[[1]]] &]];
NonRPart[Tensor[names_]] := 
  Tensor[Select[
    names, ! (MatchQ[#[[1]], RInvariant[_]] || 
        MemberQ[{"C", "\[Delta]"}, #[[1]]]) &]];

RPart[TensorPermute[t_, perm_]] := 
  With[{rInds = 
     Position[Indices[t], Raised[_RIndex] | Lowered[_RIndex]][[;; , 
       1]]}, TensorPermute[RPart[t], 
    Ordering[InversePermutation[perm][[rInds]]]]];
NonRPart[TensorPermute[t_, perm_]] := 
  With[{nonrInds = 
     Position[Indices[t], 
       Raised[Except[_RIndex]] | Lowered[Except[_RIndex]]][[;; , 1]]},
    TensorPermute[NonRPart[t], 
    Ordering[InversePermutation[perm][[nonrInds]]]]
   ];

RPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], 
       Raised[Except[_RIndex]] | Lowered[Except[_RIndex]]][[;; , 1]], 
    inner = RPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[pairs, 
         MatchQ[Indices[t][[#[[1]], 1]], _RIndex] &] /. 
        n_Integer :> 
         pi[[ip[[n]] - Length@Select[dels, # < ip[[n]] &]]])]
    ]
   ];
NonRPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], Raised[_RIndex] | Lowered[_RIndex]][[;; , 
       1]], inner = NonRPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[
         pairs, ! MatchQ[Indices[t][[#[[1]], 1]], _RIndex] &] /. 
        n_Integer :> 
         pi[[ip[[n]] - Length@Select[dels, # < ip[[n]] &]]])]
    ]
   ];

RPart[TensorProduct[a_, b__]] := 
  TensorProduct[RPart[a], RPart[TensorProduct[b]]];
NonRPart[TensorProduct[a_, b__]] := 
  TensorProduct[NonRPart[a], NonRPart[TensorProduct[b]]];

RPart[TensorDerivative[t_, _]] := RPart[t];
NonRPart[TensorDerivative[t_, i_]] := TensorDerivative[NonRPart[t], i];

RPart[a_ b_] /; FreeQ[a, Tensor] := RPart[b];
NonRPart[a_ b_] /; FreeQ[a, Tensor] := NonRPart[b];
   
ExpandCorrelator[Correlator[Tensor[names_]]] /; (AllTrue[names, MemberQ[Flatten[$multiplet /@ $multipletIndices][[;;, 1]], StringDrop[#[[1]], Count[Characters[#[[1]]], "\[PartialD]"]]] &] && Length[names] == 2) :=
With[{name2field = Association@Table[f[[1]] -> f, {f, Flatten[$multiplet /@ $multipletIndices]}]}, 
  With[{fields = 
     name2field[
        StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names, 
    derivs = 
     Flatten@Table[
       i, {i, 2}, {j, 
        Count[Characters[names[[i, 1]]], "\[PartialD]"]}]},
   If[ToExpression[Conjugate[fields[[1]]][[1]], InputForm, Hold] =!= 
     ToExpression[fields[[2, 1]], InputForm, Hold], 0,
    TensorProduct[
     TwoPtRInvariant[RRep[fields[[1]]], RRep[fields[[2]]]], 
     SpacetimeStructures[ScalingDimension /@ fields, Spin /@ fields, derivs, "\[PartialD]", {1, 2}][[1]]]
    ]
   ]
];

SpacetimeRelations[structs_] := 
  If[Length[structs] > 5 && 
    First@Cases[structs, s_SpacetimeStructure :> Length[s[[1]]], All] == 4, 
   relations[structs, 188, 6], SymbolicSpacetimeRelations[structs]];
   
Options[RRelations] = {"MonitorProgress" -> False};
RRelations[largebasis_, OptionsPattern[]] := Module[{relations, choice, tmp},
	If[OptionValue["MonitorProgress"], tmp = PrintTemporary["Calculating R-symmetry structure relations..."]];
	choice = 
	  With[{size = Length@Flatten@CanonicallyOrderedComponents[largebasis[[1]]]}, 
	   RandomSample[Range@size, Min[5000, size]]];
	relations = 
	  If[# === {}, {}, RowReduce@#] &@
	   NullSpace[
	    Transpose[
	     ArrayFlatten[
	       If[OptionValue["MonitorProgress"],
	       Monitor[Table[
	         Flatten[CanonicallyOrderedComponents[largebasis[[ri]]]][[
	          choice]], {ri, Length[largebasis]}], 
	        StringForm["``/``", ri, Length[largebasis]]],
	        Table[
	         Flatten[CanonicallyOrderedComponents[largebasis[[ri]]]][[
	          choice]], {ri, Length[largebasis]}]]]]];
	If[OptionValue["MonitorProgress"], NotebookDelete[tmp]];
	relations
];

Options[ExpansionComponents] = {"MonitorProgress" -> False};
ExpansionComponents[a_ b_, rest___, opt: OptionsPattern[]] /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] := Explicit[a] ExpansionComponents[b, rest, opt];
ExpansionComponents[a_, rest___, OptionsPattern[]] /; FreeQ[a, Tensor | TensorDerivative] := Explicit[a];
   
ExpansionComponents[t : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), rExpansion_, spExpansion_, OptionsPattern[]] := SparseArray@TensorProduct[
 expansion[RPart[t], ##] & @@ rExpansion, 
 expansion[NonRPart[t], ##] & @@ spExpansion
];

ExpansionComponents[Plus[a_, rest__], opt: OptionsPattern[]] := With[{allterms = List @@ Expand[Plus[a, rest]]},
   With[{largeRBasis = DeleteDuplicates[RPart /@ allterms], largeSTBasis = SortBy[DeleteDuplicates[NonRPart /@ allterms], First@Cases[#, s_SpacetimeStructure :> {Length[s[[3]]], s[[5]], -s[[6]]}, All] &]},
      With[{RRels = RRelations[largeRBasis, opt], STRels = SpacetimeRelations[largeSTBasis]},
      	Sum[ExpansionComponents[t /. u[perm_] :> (u^#1 v^#2 & @@ uvpowers[1, perm]) /. v[perm_] :> (u^#1 v^#2 & @@ uvpowers[2, perm]), {largeRBasis, RRels}, {largeSTBasis, STRels}], {t, List @@ Expand[Plus[a, rest]]}]
      ]
   ]
];
ExpansionComponents[t : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), OptionsPattern[]] := SparseArray[{{1,1} -> 1},{1,1}];
      
fieldOrder = OrderedQ[{{N@ScalingDimension[#1], Reverse@Spin[#1], Abs[Last[#1]], -Last[#1]}, {N@ScalingDimension[#2], Reverse@Spin[#2], Abs[Last[#2]], -Last[#2]}}] &;
      
crossingPermutationST[t_Tensor, order_] := 
  With[{ordered = SwapFactors[t, order]},
   InversePermutation@
    Ordering[
     TensorPermutation[ordered][[
      Select[Range@
        Length[Indices[ordered]], ! FreeQ[Indices[ordered][[#]], Spinor | DottedSpinor] &]]]]
   ];
crossingPermutationR[t_Tensor, order_] := 
  With[{ordered = SwapFactors[t, order]},
   InversePermutation@
    Ordering[
     TensorPermutation[ordered][[
      Select[Range@
        Length[Indices[ordered]], ! FreeQ[Indices[ordered][[#]], RIndex] &]]]]
   ];

ExpandCorrelator[0, OptionsPattern[]] = 0;

ExpandCorrelator[expr : Except[_Correlator], opt : OptionsPattern[]] /;
    Length[Cases[expr, _Correlator, All]] == 1 := 
  With[{swap = 
     ExpandCorrelator[First@Cases[expr, _Correlator, All], opt]},
   SwapIn[
    expr /. Correlator -> Identity, {Min[#], Max[#]} &@
     CorrelatedFields[expr], swap]
   ];
ExpandCorrelator[a_ + b_, opt : OptionsPattern[]] := 
  ExpandCorrelator[a, opt] + ExpandCorrelator[b, opt];
ExpandCorrelator[a_ b_, opt : OptionsPattern[]] /; FreeQ[a, Tensor] :=
   a ExpandCorrelator[b, opt];

ExpandCorrelator[Correlator[Tensor[names_]], 
    OptionsPattern[]] /; (AllTrue[names, 
      !MissingQ[name2field@StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] &] && 
     3 <= Length[names] <= 4) := 
  With[{fields = 
     name2field[
        StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names, 
    derivs = 
     Flatten@Table[
       i, {i, Length[names]}, {j, 
        Count[Characters[names[[i, 1]]], "\[PartialD]"]}]},
   With[{sfields = Sort[fields, fieldOrder], 
     order = Ordering[fields, All, fieldOrder]},
    Module[{numinvs = numInvariants[RRep /@ sfields], 
      numsts = numSTStructures[Spin /@ sfields],
      sign = 
       Signature@
        InversePermutation[order][[
         Select[Range@
           Length@fields, ! IntegerQ[ScalingDimension[fields[[#]]]] &]]], 
      STindperm = crossingPermutationST[Tensor[names], order], 
      Rindperm = crossingPermutationR[Tensor[names], order]},
     sign Sum[
       If[Length[names] == 3, \[Lambda][sfields,i,j], g[sfields, i, j][u[order], v[order]]] TensorProduct[
          TensorPermute[
           If[Length[names] == 4,
           	FourPtRInvariant[{##}, i] & @@ (RRep /@ sfields),
           	ThreePtRInvariant @@ (RRep /@ sfields)
           ], 
           Rindperm], 
          TensorPermute[
           SpacetimeStructures[ScalingDimension /@ sfields, Spin /@ sfields, 
             InversePermutation[order][[#]] & /@ derivs, 
             "\[PartialD]", order][[j]], STindperm]] + 
        If[derivs === {} || Length[names] == 3, 0,
         
         Derivative[1, 0][g[sfields, i, j]][u[order], 
            v[order]] TensorProduct[
            TensorPermute[
             FourPtRInvariant[{##}, i] & @@ (RRep /@ sfields), 
             Rindperm], 
            TensorPermute[
             SpacetimeStructures[ScalingDimension /@ sfields, 
               Spin /@ sfields, 
               InversePermutation[order][[#]] & /@ derivs, "u", 
               order][[j]], STindperm]] +
          
          Derivative[0, 1][g[sfields, i, j]][u[order], 
            v[order]] TensorProduct[
            TensorPermute[
             FourPtRInvariant[{##}, i] & @@ (RRep /@ sfields), 
             Rindperm], 
            TensorPermute[
             SpacetimeStructures[ScalingDimension /@ sfields, 
               Spin /@ sfields, 
               InversePermutation[order][[#]] & /@ derivs, "v", 
               order][[j]], STindperm]]
         ],
       {i, numinvs}, {j, numsts}
       ]
     ]
    ]
   ];