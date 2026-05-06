	(* Wolfram Language package *)

RPart[Tensor[names_]] := 
  Tensor[Select[names, 
    MatchQ[#[[1]], GlobalInvariant[_] | SU2BreakingTensor[]] || 
      MemberQ[{"C", "\[Delta]"}, #[[1]]] &]];
NonRPart[Tensor[names_]] := 
  Tensor[Select[
    names, ! (MatchQ[#[[1]], GlobalInvariant[_] | SU2BreakingTensor[]] || 
        MemberQ[{"C", "\[Delta]"}, #[[1]]]) &]];

RPart[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  With[{rInds = 
     Position[Indices[t], Raised[_GlobalIndex | _DefectGlobalIndex] | Lowered[_GlobalIndex | _DefectGlobalIndex]][[;; , 
       1]]}, TensorPermute[RPart[t], 
    Ordering[InversePermutation[perm][[rInds]]]]];
NonRPart[TensorPermute[t_, perm_, OptionsPattern[]]] := 
  With[{nonrInds = 
     Position[Indices[t], 
       Raised[Except[_GlobalIndex | _DefectGlobalIndex]] | Lowered[Except[_GlobalIndex | _DefectGlobalIndex]]][[;; , 1]]},
    TensorPermute[NonRPart[t], 
    Ordering[InversePermutation[perm][[nonrInds]]]]
   ];

RPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], 
       Raised[Except[_GlobalIndex | _DefectGlobalIndex]] | Lowered[Except[_GlobalIndex | _DefectGlobalIndex]]][[;; , 1]], 
    inner = RPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[pairs, 
         MatchQ[Indices[t][[#[[1]], 1]], _GlobalIndex | _DefectGlobalIndex] &] /. 
        n_Integer :> 
         pi[[ip[[n]] - Length@Select[dels, # < ip[[n]] &]]])]
    ]
   ];
NonRPart[Contract[t_, pairs_]] := 
  With[{dels = 
     Position[Indices[t], Raised[_GlobalIndex | _DefectGlobalIndex] | Lowered[_GlobalIndex | _DefectGlobalIndex]][[;; , 
       1]], inner = NonRPart[t]},
   With[{pi = TensorPermutation[inner], 
     ip = InversePermutation[TensorPermutation[t]]},
    Contract[inner, 
     Sort /@ (Select[
         pairs, ! MatchQ[Indices[t][[#[[1]], 1]], _GlobalIndex | _DefectGlobalIndex] &] /. 
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

RPart[a_ b_] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := RPart[b];
NonRPart[a_ b_] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := NonRPart[b];
   
Options[RRelations] = {"MonitorProgress" -> False, "SampleSize" -> 5000, "Rigorous" -> False};
RRelations[largebasis_, OptionsPattern[]] := RRelations[largebasis] = Module[{relations, comps, choice, choice2, size, nz},
  	size = Length@Flatten@CanonicallyOrderedComponents[largebasis[[1]]];
  	nz = DeleteDuplicates[Join @@ Table[ArrayRules[Flatten@CanonicallyOrderedComponents[s]][[;; -2, 1, 1]], {s, largebasis}]];
	choice = RandomSample[nz, Min[OptionValue["SampleSize"], Length[nz]]];
	choice2 = RandomSample[Range@size, If[OptionValue["Rigorous"], size, Min[2 OptionValue["SampleSize"], size]]]; 
	comps = ArrayFlatten[
	       If[OptionValue["MonitorProgress"],
	       monitorProgress[Table[
	         Flatten[CanonicallyOrderedComponents[largebasis[[ri]]]][[
	          Join[choice, choice2]]], {ri, Length[largebasis]}], "Label" -> "Global symmetry structure relations",
				"CurrentDisplayFunction" -> None
	        ],
	        Table[
	         Flatten[CanonicallyOrderedComponents[largebasis[[ri]]]][[
	          Join[choice, choice2]]], {ri, Length[largebasis]}]]];
	relations = If[# === {}, {}, RowReduce@Transpose@First@clearRadicals@Transpose@#] &@ NullSpace@Simplify[Transpose[comps][[;;Length[choice]]]];
	If[OptionValue["SampleSize"] >= size || relations === {} || Max[Chop@N@Flatten[relations . comps]] == 0, relations, RRelations[largebasis, "MonitorProgress" -> OptionValue["MonitorProgress"], "Rigorous" -> OptionValue["Rigorous"], "SampleSize" -> 2 OptionValue["SampleSize"]]]
];

uvReplace =  {
	u[dim_, perm_] :> If[Length[perm] == 4, u^#1 v^#2 & @@ ConformalStructures`Private`uvpowers[dim, 1, perm], u], 
	v[dim_, perm_] :> If[Length[perm] == 4, u^#1 v^#2 & @@ ConformalStructures`Private`uvpowers[dim, 2, perm], v],
	z[dim_, perm_] :> (z[dim, perm] /. ConformalStructures`Private`zcross),
	zb[dim_, perm_] :> (zb[dim, perm] /. ConformalStructures`Private`zcross)
};

Options[ExpansionComponents] = {"MonitorProgress" -> False};
ExpansionComponents[a_ b_, rest___, opt: OptionsPattern[]] /; FreeQ[a, Alternatives @@ (TensorTools`Private`$TensorHeads)] := Explicit[a /. uvReplace] ExpansionComponents[b, rest, opt];
ExpansionComponents[a_, rest___, OptionsPattern[]] /; FreeQ[a, Tensor | TensorDerivative] := Explicit[a /. uvReplace];
ExpansionComponents[xs_List, rest___, opt : OptionsPattern[]] := ExpansionComponents[#, rest, opt] & /@ xs;
   
ExpansionComponents[t : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), rExpansion_, spExpansion_, OptionsPattern[]] := SparseArray@TensorProduct[
 expansion[RPart[t], ##] & @@ rExpansion, 
 expansion[NonRPart[t], ##] & @@ spExpansion
];

ExpansionComponents[Plus[a_, rest__], opt: OptionsPattern[]] := With[{allterms = List @@ Expand[Plus[a, rest]]},
   With[{largeRBasis = DeleteDuplicates[RPart /@ allterms], largeSTBasis = SortBy[DeleteDuplicates[NonRPart /@ allterms], First@Cases[#, s_correlator :> {Length[s[[4]]], s[[5]], -s[[7]]}, All] &]},
      With[{RRels = RRelations[largeRBasis, opt], STRels = StructureRelations[largeSTBasis]},
      	Sum[ExpansionComponents[t /. uvReplace, {largeRBasis, RRels}, {largeSTBasis, STRels}], {t, List @@ Expand[Plus[a, rest]]}]
      ]
   ]
];
ExpansionComponents[t : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), OptionsPattern[]] := SparseArray[{{1,1} -> If[And @@ (Thread[Flatten[Normal@Components[t]] === 0]), 0, 1]},{1,1}];
      
fieldOrder = OrderedQ[{{N@ScalingDimension[#1], Abs[Last[#1]], -Last[#1], dynkin[#1[[2]]], First[#2]}, 
                       {N@ScalingDimension[#2], Abs[Last[#2]], -Last[#2], dynkin[#2[[2]]], First[#1]}}] &;
                             
crossingPermutationST[t_Tensor, order_] := 
  With[{ordered = SwapFactors[t, order]},
   InversePermutation@
    Ordering[
     TensorPermutation[ordered][[
      Select[Range@
        Length[Indices[ordered]], ! FreeQ[Indices[ordered][[#]], DiracSpinor | WeylSpinor | DottedWeylSpinor] &]]]]
   ];
crossingPermutationR[t_Tensor, order_] := 
  With[{ordered = SwapFactors[t, order]},
   InversePermutation@
    Ordering[
     TensorPermutation[ordered][[
      Select[Range@
        Length[Indices[ordered]], ! FreeQ[Indices[ordered][[#]], GlobalIndex | DefectGlobalIndex] &]]]]
   ];
   
RSTperm[0, __] := 0;
RSTperm[t1 : (_Tensor | _TensorPermute | _Contract | _Correlator | _TP), t2_Tensor] := Module[{order, inds, rinds, stinds, rperm, stperm},
	order = InversePermutation@Ordering[Join @@ Cases[t1, correlator[__, p_, _, _] :> p, All]];
	inds = Indices[t1];
	rinds = Position[inds, Raised[_GlobalIndex | _DefectGlobalIndex]][[;;,1]];
	stinds = Position[inds, Lowered[_DiracSpinor | _WeylSpinor | _DottedWeylSpinor]][[;;,1]];
	
	rperm = crossingPermutationR[t2, order];
	stperm = crossingPermutationST[t2, order];
	
	TensorPermute[TensorPermute[t1, InversePermutation@TensorPermutation[t1]], Range@Length[inds] /. Join[Thread[rinds -> rinds[[rperm]]], Thread[stinds -> stinds[[stperm]]]]]
];
RSTperm[a_ b_, t2_] /; ! FreeQ[b, Alternatives @@ TensorTools`Private`$TensorHeads] := a RSTperm[b, t2];
RSTperm[a_ + b_, t2_] := RSTperm[a, t2] + RSTperm[b, t2];

SetAttributes[ExpandCorrelator, Listable];
Options[ExpandCorrelator] = {"GFFT" -> False, "Points" -> Automatic};
ExpandCorrelator[x_?NumericQ, OptionsPattern[]] = x;

ExpandCorrelator[expr : Except[_Correlator], opt : OptionsPattern[]] /;
    Length[Cases[expr, _Correlator, All]] == 1 := 
  With[{swap = 
     ExpandCorrelator[First@Cases[expr, _Correlator, All], opt]},
   SwapIn[
    expr /. Correlator[t_, OptionsPattern[]] :> t, {Min[#], Max[#]} &@
     CorrelatedFields[expr], swap]
   ];
ExpandCorrelator[a_ + b_, opt: OptionsPattern[]] := 
  ExpandCorrelator[a, opt] + ExpandCorrelator[b, opt];
ExpandCorrelator[a_ b_, opt : OptionsPattern[]] /; FreeQ[a, Tensor] :=
   a ExpandCorrelator[b, opt];

ExpandCorrelator[Correlator[Tensor[names_], opt: OptionsPattern[]], opt2 : OptionsPattern[]] := 
  Module[{
     fields = name2field[StringDrop[#[[1]], Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names, 
     derivs = Flatten@Table[i, {i, Length[names]}, {j, Count[Characters[names[[i, 1]]], "\[PartialD]"]}],
     q = If[OptionValue[Correlator, "Defect"], $qdefect, None],
     rreps = FirstCase[#, Raised[ind : (_GlobalIndex | _DefectGlobalIndex)] :> ind] & /@ names,
     fieldsReplaced, sfields, order, pts,  numinvs, STs, numSTs, sign, STindperm, Rindperm, arrangements},
    	fieldsReplaced = ReplacePart[fields, Thread[Table[{i, 2}, {i, Length[fields]}] -> rreps[[;;, 1]]]];
   		sfields = Sort[fieldsReplaced, fieldOrder]; 
   		order = Ordering[fieldsReplaced, All, fieldOrder];
     	pts = If[OptionValue[ExpandCorrelator, "Points"] === Automatic, Range@Length[names], OptionValue[ExpandCorrelator, "Points"]][[order]];
    	numinvs = numInvariants[rreps];
    	STs = ConformalCorrelators[SpacetimeDimension[], ScalingDimension /@ sfields, Spin /@ sfields, {}, pts, "DefectCodimension" -> q];
      	numSTs = Length@STs;
      	sign = Signature@InversePermutation[order][[Select[Range@Length@fields, ! IntegerQ[ScalingDimension[fields[[#]]]] &]]];
      	STindperm = crossingPermutationST[Tensor[names], order]; 
      	Rindperm = crossingPermutationR[Tensor[names], order];
      	arrangements = Fold[Join[
		   Table[{term[[1]], Append[term[[2]], {"\[PartialD]", #2}]}, {term, #1}],
		   Flatten[Table[{term[[1]] + Table[Boole[icr == j], {j, Length@crossRatios[SpacetimeDimension[], q]}], Append[term[[2]], {ToString[crossRatios[SpacetimeDimension[], q][[icr]]], #2}]}, {icr, If[SpacetimeDimension[] == 2, 1, Length@crossRatios[SpacetimeDimension[], q]]}, {term, #1}], 1]
		   ] &, {{Table[0, Length[crossRatios[SpacetimeDimension[], q]]], {}}}, InversePermutation[order][[#]] & /@ derivs];
     	sign Sum[
       		Switch[{Length[names], OptionValue[Correlator, "Defect"]},
          		{2, False},
          		If[Total[derivArrangement[[1]]] != 0, 0, If[ListQ[Spin[fields[[1]]]], I^(2 Abs[Subtract @@ Spin[fields[[1]]]]), If[IntegerQ[Spin[fields[[1]]]], 1, I]]],
          		{3, False}, 
          		If[Total[derivArrangement[[1]]] != 0, 0, \[Lambda][sfields,i,j]], 
          		{4, False},
          		Derivative[Sequence @@ derivArrangement[[1]]][g[sfields, i, j]][Sequence @@ Table[c[SpacetimeDimension[], pts], {c, crossRatios[SpacetimeDimension[], q]}]],
          		{1, True},
          		If[Total[derivArrangement[[1]]] != 0, 0, a[sfields[[1]]]],
          		{2, True},
          		Derivative[Sequence @@ derivArrangement[[1]]][g[sfields, i, j]][Sequence @@ Table[c[SpacetimeDimension[], pts], {c, crossRatios[SpacetimeDimension[], q]}]]
       		] TensorProduct[
          TensorPermute[
           Switch[Length[names],
            4, FourPtGlobalInvariant[rreps[[order,1]], i],
           	3, Tensor[{{"C", Sequence @@ (Raised /@ rreps[[order]])}}],
           	2, Tensor[{{"\[Delta]", Sequence @@ (Raised /@ rreps[[order]])}}],
           	1, 1
           ], 
           Rindperm], 
          TensorPermute[ConformalCorrelators[SpacetimeDimension[], ScalingDimension /@ sfields, Spin /@ sfields, derivArrangement[[2]], pts, "DefectCodimension" -> q][[j]], STindperm]
       ],
       {i, numinvs}, {j, numSTs}, {derivArrangement, arrangements}
       ]
     ]  /; (AllTrue[names, 
      !MissingQ[name2field@StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] &] && 
     ((!OptionValue[Correlator, "Defect"] && 2 <= Length[names] <= 4) || (Length[names] <= 2)) && !OptionValue[ExpandCorrelator, "GFFT"]);
     

     
ExpandCorrelator[Correlator[Tensor[{}], OptionsPattern[]], OptionsPattern[]] := 1;
ExpandCorrelator[Correlator[Tensor[names_], opt: OptionsPattern[]], opt2 : OptionsPattern[]] := Module[
     	{operators, fakepts, realpts},
     	operators = name2field[StringDrop[#[[1]], Count[Characters[#[[1]]], "\[PartialD]"]]] & /@ names;
     	fakepts = Range@Length[names];
     	realpts = OptionValue[ExpandCorrelator, "Points"] /. Automatic -> Range@Length[names];
     	Sum[
     	 (-1)^(2 ScalingDimension[First[operators]] * (2 Total[ScalingDimension /@ operators[[2;;j-1]]]))*
     	 RSTperm[TensorProduct[
     	 	ExpandCorrelator[Correlator[Tensor[names[[{1,j}]]], opt], "Points" -> fakepts[[{1, j}]]],
     	 	ExpandCorrelator[Correlator[Tensor[Delete[names,{{1},{j}}]], opt], "GFFT" -> True, "Points" -> Delete[fakepts,{{1},{j}}]]	
     	 ], Tensor[names]]
     	,{j,Select[Range[2,Length[names]], realpts[[#]] =!= realpts[[1]] &]}] /. correlator[x__, p_, y_, z_] :> correlator[x, p /. Thread[fakepts -> realpts], y, z] /. Tensor[{___, {correlator[__, p_, _, _], ___}, ___}] /; Length[DeleteDuplicates[p]] != Length[p] -> 0
     ] /; (AllTrue[names, 
      !MissingQ[name2field@StringDrop[#[[1]], 
         Count[Characters[#[[1]]], "\[PartialD]"]]] &] && OptionValue[ExpandCorrelator, "GFFT"]);
         
GFFTRules[ops_] := Module[{zeros},
	zeros = Flatten@ExpansionComponents[ExpandCorrelator[Correlator[Tensor[ops]]] - ExpandCorrelator[Correlator[Tensor[ops]], "GFFT" -> True]];
	First@Solve[Thread[zeros == 0], DeleteDuplicates@Cases[Normal@zeros, g[__][__], All]] /. HoldPattern[h_[args__] -> val_] :> (h -> Function[{args}, val])
]
	