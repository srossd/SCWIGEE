(* Wolfram Language package *)

RPart[Tensor[names_]] := 
  Tensor[Select[names, 
    MatchQ[#[[1]], RInvariant[_]] || 
      MemberQ[{"C", "\[Delta]"}, #[[1]]] &]];
NonRPart[Tensor[names_]] := 
  Tensor[Select[
    names, ! (MatchQ[#[[1]], RInvariant[_]] || 
        MemberQ[{"C", "\[Delta]"}, #[[1]]]) &]];

RPart[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  With[{rInds = 
     Position[Indices[t], Raised[_RIndex] | Lowered[_RIndex]][[;; , 
       1]]}, TensorPermute[RPart[t], 
    Ordering[InversePermutation[perm][[rInds]]]]];
NonRPart[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  With[{nonrInds = 
     Position[Indices[t], 
       Raised[Except[_RIndex | _DefectRIndex]] | Lowered[Except[_RIndex | _DefectRIndex]]][[;; , 1]]},
    TensorPermute[NonRPart[t], 
    Ordering[InversePermutation[perm][[nonrInds]]]]
   ];

RPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], 
       Raised[Except[_RIndex | _DefectRIndex]] | Lowered[Except[_RIndex | _DefectRIndex | _DefectRIndex]]][[;; , 1]], 
    inner = RPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[pairs, 
         MatchQ[Indices[t][[#[[1]], 1]], _RIndex | _DefectRIndex] &] /. 
        n_Integer :> 
         pi[[ip[[n]] - Length@Select[dels, # < ip[[n]] &]]])]
    ]
   ];
NonRPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], Raised[_RIndex | _DefectRIndex] | Lowered[_RIndex | _DefectRIndex]][[;; , 
       1]], inner = NonRPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[
         pairs, ! MatchQ[Indices[t][[#[[1]], 1]], _RIndex | _DefectRIndex] &] /. 
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
   With[{largeRBasis = DeleteDuplicates[RPart /@ allterms], largeSTBasis = SortBy[DeleteDuplicates[NonRPart /@ allterms], First@Cases[#, s_SpacetimeStructure :> {Length[s[[3]]], s[[5]], -s[[7]]}, All] &]},
      With[{RRels = RRelations[largeRBasis, opt], STRels = SpacetimeRelations[largeSTBasis]},
      	Sum[ExpansionComponents[t /. u[perm_] :> If[Length[perm] == 4, u^#1 v^#2 & @@ uvpowers[1, perm], u] /. v[perm_] :> If[Length[perm] == 4, u^#1 v^#2 & @@ uvpowers[2, perm], v], {largeRBasis, RRels}, {largeSTBasis, STRels}], {t, List @@ Expand[Plus[a, rest]]}]
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

ExpandCorrelator[0] = 0;

ExpandCorrelator[expr : Except[_Correlator]] /;
    Length[Cases[expr, _Correlator, All]] == 1 := 
  With[{swap = 
     ExpandCorrelator[First@Cases[expr, _Correlator, All]]},
   SwapIn[
    expr /. Correlator[t_, OptionsPattern[]] :> t, {Min[#], Max[#]} &@
     CorrelatedFields[expr], swap]
   ];
ExpandCorrelator[a_ + b_] := 
  ExpandCorrelator[a] + ExpandCorrelator[b];
ExpandCorrelator[a_ b_] /; FreeQ[a, Tensor] :=
   a ExpandCorrelator[b];

ExpandCorrelator[Correlator[Tensor[names_], opt: OptionsPattern[]]] /; (AllTrue[names, 
      !MissingQ[name2field@StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] &] && 
     ((!OptionValue[Correlator, "Defect"] && 2 <= Length[names] <= 4) || (Length[names] <= 2))) := 
  With[{fields = 
     name2field[
        StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names, 
    derivs = 
     Flatten@Table[
       i, {i, Length[names]}, {j, 
        Count[Characters[names[[i, 1]]], "\[PartialD]"]}],
    q = If[OptionValue[Correlator, "Defect"], $qdefect, None],
    rrep = If[OptionValue[Correlator, "Defect"], DefectRRep, RRep]},
   With[{sfields = Sort[fields, fieldOrder], 
     order = Ordering[fields, All, fieldOrder]},
    Module[{numinvs = numInvariants[rrep /@ sfields], 
      STstructs = SpacetimeStructures[ScalingDimension /@ sfields, Spin /@ sfields, 
             InversePermutation[order][[#]] & /@ derivs, 
             "\[PartialD]", order, q],
      sign = 
       Signature@
        InversePermutation[order][[
         Select[Range@
           Length@fields, ! IntegerQ[ScalingDimension[fields[[#]]]] &]]], 
      STindperm = crossingPermutationST[Tensor[names], order], 
      Rindperm = crossingPermutationR[Tensor[names], order]},
     sign Sum[
       Switch[{Length[names], OptionValue[Correlator, "Defect"]},
          {2, False},
          I^(2 Abs[Subtract @@ Spin[fields[[1]]]]),
          {3, False}, 
          \[Lambda][sfields,i,j], 
          {4, False},
          g[sfields, i, j][u[order], v[order]],
          {1, True},
          a[sfields[[1]]],
          {2, True},
          g[sfields, i, j] @@ Table[c[order], {c, crossRatios[$qdefect]}]
       ] TensorProduct[
          TensorPermute[
           Switch[Length[names],
            4, FourPtRInvariant[{##}, i] & @@ (rrep /@ sfields),
           	3, ThreePtRInvariant @@ (rrep /@ sfields),
           	2, TwoPtRInvariant @@ (rrep /@ sfields),
           	1, 1
           ], 
           Rindperm], 
          TensorPermute[STstructs[[j]], STindperm]] + 
        If[derivs === {} || Length[names] < 2 || (!OptionValue[Correlator, "Defect"] && Length[names] < 4), 0,
            Sum[
               (((Derivative @@ Normal[SparseArray[{idx} -> 1, {Length[crossRatios[q]]}]])[
                  g[sfields, i, j]
               ]) @@ Table[c[order], {c, crossRatios[$qdefect]}]) TensorProduct[
            TensorPermute[
             Switch[Length[names],
	            4, FourPtRInvariant[{##}, i] & @@ (rrep /@ sfields),
	           	3, ThreePtRInvariant @@ (rrep /@ sfields),
	           	2, TwoPtRInvariant @@ (rrep /@ sfields),
	           	1, 1
	         ], 
             Rindperm], 
            TensorPermute[
             SpacetimeStructures[ScalingDimension /@ sfields, 
               Spin /@ sfields, 
               InversePermutation[order][[#]] & /@ derivs, ToString[crossRatios[q][[idx]]], 
               order][[j]], STindperm]]
        ,{idx, Length[crossRatios[q]]}]
        ],
       {i, numinvs}, {j, Length[STstructs]}
       ]
     ]
    ]
   ];