(* Wolfram Language package *)

Options[QTensor] = {"QBar" -> False};
QTensor[OptionsPattern[]] := If[TrueQ[OptionValue["QBar"]], $QBarTensor, $QTensor];

ScalingDimension[Field[name_, rep_, dim_, {j1_, j2_}, y_]] := dim;
Spin[Field[name_, rep_, dim_, {j1_, j2_}, y_]] := {j1, j2};
RRep[Field[name_, rep_, dim_, {j1_, j2_}, y_]] := rep;

whichMultiplet[f_Field] := whichMultiplet[f[[1]]];
whichMultiplet[name_] := SelectFirst[$multipletIndices, MemberQ[Flatten[Multiplet[#]][[;;,1]], name] &];

multipletOf[f_Field] := With[{mm = Multiplet[whichMultiplet[f]]},
   If[MatchQ[mm[[1]], _List], SelectFirst[mm, MemberQ[#, f] &], mm]
];

Field /: Conjugate[f : Field[name_, rep_, dim_, js_, y_]] := 
 With[{r = ConjugateIrrep[$RSymmetry, rep]},
    First@SortBy[Select[Flatten[Multiplet[whichMultiplet[f]]], SimpleRepInputConversion[$RSymmetry, #[[2]]] == r && #[[3]] == dim && #[[4]] == Reverse[js] && #[[5]] == -y &], # === f &] 
 ];
 
makeConjugate[f : Field[name_, rep_, dim_, js_, y_]] := Field[
   With[{nameExp = ToExpression[name, TraditionalForm]}, 
      If[FreeQ[nameExp, OverBar], 
         nameExp /. x_ :> ToString[OverBar[x], TraditionalForm],
         nameExp /. x_ :> ToString[x /. OverBar -> Identity, TraditionalForm]
      ]
   ],
   ConjugateIrrep[$RSymmetry, rep],
   dim, Reverse[js], -y];
 
name2field[name_] := SelectFirst[Flatten[Multiplet[whichMultiplet[name]]], #[[1]] == name &];

IndexData[Spinor] = Index[2, "Greek", 1];
IndexData[DottedSpinor] = Index[2, "Greek", 1, OverDot];
IndexData[SpaceTime] = Index[4, "Greek", 12];
IndexData[RIndex[rep_]] := 
  Index[Times @@ DimR[RSymmetry[], rep], "Latin", 9, Subscript[#, RepName[RSymmetry[], rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]] &];
RIndex[rep_Integer] /; RSymmetry[] =!= U1 := RIndex[SimpleRepInputConversion[RSymmetry[], rep]];

\[Epsilon]Lower = 
  Tensor[{{"\[Epsilon]", Lowered[Spinor], Lowered[Spinor]}}];
BuildTensor[{"\[Epsilon]", Lowered[Spinor], Lowered[Spinor]}] = 
  SparseArray@LeviCivitaTensor[2];

\[Epsilon]Upper = 
  Tensor[{{"\[Epsilon]", Raised[Spinor], Raised[Spinor]}}];
BuildTensor[{"\[Epsilon]", Raised[Spinor], Raised[Spinor]}] = 
  SparseArray@LeviCivitaTensor[2];

\[Epsilon]LowerDot = 
  Tensor[{{"\[Epsilon]", Lowered[DottedSpinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\[Epsilon]", Lowered[DottedSpinor], 
    Lowered[DottedSpinor]}] = SparseArray@LeviCivitaTensor[2];

\[Epsilon]UpperDot = 
  Tensor[{{"\[Epsilon]", Raised[DottedSpinor], Raised[DottedSpinor]}}];
BuildTensor[{"\[Epsilon]", Raised[DottedSpinor], 
    Raised[DottedSpinor]}] = SparseArray@LeviCivitaTensor[2];
    
DeclareTensorSymmetry["\[Epsilon]", {{Cycles[{{1,2}}], -1}}];

Unprotect[Tensor];
Tensor[{x___, f_Field, y___}] := Tensor[{x, ToTensor[f][[1, 1]], y}];
Protect[Tensor];

\[Eta]Upper = 
  Tensor[{{"\[Eta]", Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\[Eta]", Raised[SpaceTime], Raised[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[{-SignatureFactor[]^2, 1, 1, 1}]];

\[Eta]Lower = 
  Tensor[{{"\[Eta]", Lowered[SpaceTime], Lowered[SpaceTime]}}];
BuildTensor[{"\[Eta]", Lowered[SpaceTime], Lowered[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[{-SignatureFactor[]^2, 1, 1, 1}]];

\[Sigma]Lower = 
  Tensor[{{"\[Sigma]", Lowered[SpaceTime], Lowered[Spinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\[Sigma]", Lowered[SpaceTime], Lowered[Spinor], 
    Lowered[DottedSpinor]}] := 
  SparseArray[{-SignatureFactor[], 1, 1, 1} (PauliMatrix /@ Range[0, 3])];
\[Sigma]BarLower = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Lowered[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Lowered[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}] := 
  SparseArray[{SignatureFactor[], 1, 1, 1} (PauliMatrix /@ Range[0, 3])];

\[Sigma]Upper = 
  Tensor[{{"\[Sigma]", Raised[SpaceTime], Lowered[Spinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\[Sigma]", Raised[SpaceTime], Lowered[Spinor], 
    Lowered[DottedSpinor]}] := 
  Components@
   Contract[TensorProduct[\[Eta]Upper, \[Sigma]Lower], {{2, 3}}];

\[Sigma]BarUpper = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Raised[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Raised[SpaceTime], Raised[DottedSpinor], Raised[Spinor]}] := 
  Components@
   Contract[TensorProduct[\[Eta]Upper, \[Sigma]BarLower], {{2, 3}}];

\[Sigma]CommLower = 
  Tensor[{{"\[Sigma]", Lowered[SpaceTime], Lowered[SpaceTime], 
     Lowered[Spinor], Lowered[Spinor]}}];
BuildTensor[{"\[Sigma]", Lowered[SpaceTime], Lowered[SpaceTime], 
    Lowered[Spinor], Lowered[Spinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Sigma]Lower, \[Sigma]BarLower, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Sigma]Lower, \[Sigma]BarLower, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}], {3, 2, 1, 4}]], 
    InversePermutation@
     Ordering[{Lowered[SpaceTime], Lowered[SpaceTime], 
       Lowered[Spinor], Lowered[Spinor]}]];

\[Sigma]CommLowerDot = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Lowered[SpaceTime], Lowered[SpaceTime], Lowered[DottedSpinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Lowered[SpaceTime], Lowered[SpaceTime], Lowered[DottedSpinor], 
    Lowered[DottedSpinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarLower, \
\[Sigma]Lower], {{2, 4}, {5, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarLower, \
\[Sigma]Lower], {{2, 4}, {5, 7}}], {1, 3, 2, 4}]], 
    InversePermutation@
     Ordering[{Lowered[SpaceTime], Lowered[SpaceTime], 
       Lowered[DottedSpinor], Lowered[DottedSpinor]}]];

\[Sigma]CommUpper = 
  Tensor[{{"\[Sigma]", Raised[SpaceTime], Raised[SpaceTime], 
     Lowered[Spinor], Lowered[Spinor]}}];
BuildTensor[{"\[Sigma]", Raised[SpaceTime], Raised[SpaceTime], 
    Lowered[Spinor], Lowered[Spinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Sigma]Upper, \[Sigma]BarUpper, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Sigma]Upper, \[Sigma]BarUpper, \
\[Epsilon]Lower], {{3, 5}, {6, 7}}], {3, 2, 1, 4}]], 
    InversePermutation@
     Ordering[{Raised[SpaceTime], Raised[SpaceTime], Lowered[Spinor], 
       Lowered[Spinor]}]];

\[Sigma]CommUpperDot = 
  Tensor[{{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
     Raised[SpaceTime], Raised[SpaceTime], Lowered[DottedSpinor], 
     Lowered[DottedSpinor]}}];
BuildTensor[{"\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)", 
    Raised[SpaceTime], Raised[SpaceTime], Lowered[DottedSpinor], 
    Lowered[DottedSpinor]}] := -1/
   4 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarUpper, \
\[Sigma]Upper], {{2, 4}, {5, 7}}] - 
      TensorPermute[
       Contract[
        TensorProduct[\[Epsilon]LowerDot, \[Sigma]BarUpper, \
\[Sigma]Upper], {{2, 4}, {5, 7}}], {1, 3, 2, 4}]], 
    InversePermutation@
     Ordering[{Raised[SpaceTime], Raised[SpaceTime], 
       Lowered[DottedSpinor], Lowered[DottedSpinor]}]];
  
\[Epsilon]Spacetime = 
  Tensor[{{"\[Epsilon]", Lowered[SpaceTime], Lowered[SpaceTime], 
     Lowered[SpaceTime], Lowered[SpaceTime]}}];
\[Epsilon]SpacetimeUpper = 
  Tensor[{{"\[Epsilon]", Raised[SpaceTime], Raised[SpaceTime], 
     Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\[Epsilon]", Lowered[SpaceTime], Lowered[SpaceTime], 
    Lowered[SpaceTime], Lowered[SpaceTime]}] := -SignatureFactor[]^2 LeviCivitaTensor[4];
BuildTensor[{"\[Epsilon]", Raised[SpaceTime], Raised[SpaceTime], 
    Raised[SpaceTime], Raised[SpaceTime]}] := LeviCivitaTensor[4];

XX[i_] := Tensor[{{SpacetimePoint[i], Raised[SpaceTime]}}];
BuildTensor[{SpacetimePoint[i_], Raised[SpaceTime]}] := SparseArray@Table[x[i, k], {k, 4}];

XX[i_, j_] := Tensor[{{SpacetimeSeparation[i, j], Raised[SpaceTime]}}];
BuildTensor[{SpacetimeSeparation[i_, j_], Raised[SpaceTime]}] := SparseArray@Table[x[i, k] - x[j, k], {k, 4}];

SetAttributes[XXSquared, Orderless];
AppendTo[TensorTools`Private`explicitRules, 
  XXSquared[i_] :> 
   Components[
    Contract[
     TensorProduct[XX[i], \[Eta]Lower, XX[i]], {{1, 2}, {3, 4}}]]];
AppendTo[TensorTools`Private`explicitRules, 
  XXSquared[i_, j_] :> 
   Components[
    Contract[
     TensorProduct[XX[i, j], \[Eta]Lower, 
      XX[i, j]], {{1, 2}, {3, 4}}]]];
    
SpinorX[i_, j_] := Contract[TensorProduct[XX[i, j], \[Sigma]Lower], {{1, 2}}];

SpinorX[{i_, j_}, {k_, l_}, {m_, n_}] := 
  Contract[
   TensorProduct[\[Epsilon]Spacetime, XX[i, j], XX[k, l], 
    XX[m, n], \[Sigma]Upper], {{1, 5}, {2, 6}, {3, 7}, {4, 8}}];

AddTensorHead[TensorDerivative];
TensorDerivative[0, _] := 0;

Symbolic[
   TensorDerivative[
    a_. t : (_Tensor | _Contract | _TensorPermute | 
        TensorProduct[x_, y__] | _Correlator), i_]] := 
  Join[{{("\[PartialD]")^Row[{"(", i, ")"}], 
     Lowered[SpaceTime]}, {"("}, If[a =!= 1, {a}, Nothing]}, 
   Symbolic[t], {{")"}}];
Symbolic[TensorDerivative[a_, i_]] /; FreeQ[a, Tensor] := 
  Join[{{("\[PartialD]")^Row[{"(", i, ")"}], 
     Lowered[SpaceTime]}, {"("}, 
    If[a =!= 1, {a}, Nothing]}, {{")"}}];
Indices[TensorDerivative[t_, i_]] := 
  Prepend[Indices[t], Lowered[Spacetime]];
Permutation[TensorDerivative[t_, i_]] := Join[{1}, 1 + Permutation[t]];
DisplayTemplate[td_TensorDerivative] := 
  DisplayTemplate[Symbolic[td]];
Format[td_TensorDerivative, TraditionalForm] := 
  DisplayTemplate[td] /. 
   dn[_, a_, b_] | adn[_, a_, b_] :> DisplayName[a, b];
   
InactiveComponents[TensorDerivative[t_, i_]] := 
  With[{comps = Components[t]}, If[ArrayQ[comps],
    SparseArray[
     Flatten@Table[
       ArrayRules@comps /. 
        HoldPattern[a_ -> b_] :> Prepend[a, k] -> D[b, x[i, k]], {k, 
        4}], Prepend[Dimensions[comps], 4]],
    SparseArray[Table[D[comps, x[i, k]], {k, 4}]]
    ]
   ];

TensorDerivative[a_. Contract[t_, pairs_], i_] := 
  Contract[TensorDerivative[a t, i], pairs + 1];
TensorDerivative[a_. TensorPermute[t_, perm_, OptionsPattern[]], i_] :=
   TensorPermute[TensorDerivative[a t, i], Join[{1}, perm + 1]];
TensorSpinorDerivative[t_, i_] :=(*1/Sqrt[2]*)
  Contract[
   TensorProduct[\[Sigma]Upper, TensorDerivative[t, i]], {{1, 4}}];


AddTensorHead[Correlator];
Correlator[x_?NumericQ] := x;
Correlator[a_ b_] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := a Correlator[b];
Correlator[a_ + b_] := Correlator[a] + Correlator[b];

readyToCorrelate[names_, already_] := 
  With[{poss = 
     Flatten[If[Position[names, #] =!= {}, 
         Position[names, #][[;; , 1]], {}] & /@ already]}, 
   poss === Range@Length[poss]];
findSwap[{1, ys___}, n_] := findSwap[{ys} - 1, n + 1];
findSwap[{x_, ys___}, n_] := {n + x, n + 1};

Correlator[t_Tensor | t_TensorPermute | t_Contract] /; !readyToCorrelate[Symbolic[t], {"C"}] := 
  With[{swap = findSwap[Position[Symbolic[t], "C"][[;; , 1]], 0]}, 
   Correlator[SwapFactors[t, swap[[1]], swap[[2]]]]];
Correlator[t_Tensor | t_TensorPermute | t_Contract] /; readyToCorrelate[Symbolic[t], {"C"}] && !readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]"}] := 
  With[{swap = 
     findSwap[Position[Symbolic[t], "C" | "\[Epsilon]"][[;; , 1]], 
      0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]]]];
Correlator[t_Tensor | t_TensorPermute | t_Contract] /; readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]"}] && ! readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]", "\[Delta]"}] :=
   With[{swap = 
     findSwap[
      Position[Symbolic[t], "C" | "\[Epsilon]" | "\[Delta]"][[;; , 
        1]], 0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]]]];
Correlator[
   Tensor[{{s : "C" | "\[Epsilon]" | "\[Delta]", rest__}, y___}]] := 
  TensorProduct[Tensor[{{s, rest}}], Correlator[Tensor[{y}]]];
Correlator[
    Tensor[{x___, {"\[PartialD]", Lowered[Spinor], 
       Lowered[DottedSpinor]}, {f_, idxs___}, y___}]] /; 
   f =!= "\[PartialD]" := 
  Correlator[
   Tensor[{x, {"\[PartialD]" <> f, Lowered[Spinor], 
      Lowered[DottedSpinor], idxs}, y}]];
Correlator[Contract[t_, pairs_, opt : OptionsPattern[]]] /; 
   readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]", "\[Delta]"}] := 
  Contract[Correlator[t], pairs];
Correlator[TensorPermute[t_, perm_, OptionsPattern[]]] /; 
   readyToCorrelate[Symbolic[t], {"C", "\[Epsilon]", "\[Delta]"}] := 
  TensorPermute[Correlator[t], perm];
  
Symbolic[Correlator[t_]] := {{"\[LeftAngleBracket]"}, 
   Sequence @@ Symbolic[t], {"\[RightAngleBracket]"}};
DisplayTemplate[Correlator[t_]] := 
  Row[{"\[LeftAngleBracket]", DisplayTemplate[t], 
    "\[RightAngleBracket]"}];
Indices[Correlator[t_]] := Indices[t];
Permutation[Correlator[t_]] := Permutation[t];

CorrelatedFields[Correlator[t_]] := Range@Length[Symbolic[t]];
CorrelatedFields[t_Tensor] := {};
CorrelatedFields[TensorPermute[t_, _]] := CorrelatedFields[t];
CorrelatedFields[Contract[t_, _]] := CorrelatedFields[t];
CorrelatedFields[TensorProduct[t1_, t2_]] := 
  Join[CorrelatedFields[t1], 
   Length[Symbolic[t1]] + CorrelatedFields[t2]];
CorrelatedFields[TensorDerivative[t_, _]] := 1 + CorrelatedFields[t];
CorrelatedFields[a_ b_] /; FreeQ[a, Tensor] := CorrelatedFields[b];
    
AddExplicitRule[u[{ii_, jj_, kk_, ll_}] :> (XXSquared[ii, jj] XXSquared[kk, ll])/(
   XXSquared[ii, kk] XXSquared[jj, ll])];
AddExplicitRule[v[{ii_, jj_, kk_, ll_}] :> (XXSquared[ii, ll] XXSquared[jj, kk])/(
   XXSquared[ii, kk] XXSquared[jj, ll])];