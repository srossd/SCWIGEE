(* Wolfram Language package *)

Options[QTensor] = {"QBar" -> False, "Defect" -> False};
QTensor[OptionsPattern[]] := If[OptionValue["Defect"],
   defectSupercharge[$qdefect, Sequence @@ Switch[$qdefect, 1, {$q1angle}, 2, {$q2type, OptionValue["QBar"]}, 3, {$q3angle}]],
   If[OptionValue["QBar"],$QBarTensor, $QTensor]
];

ScalingDimension[Operator[name_, rep_, dim_, {j1_, j2_}, y_]] := dim;
Spin[Operator[name_, rep_, dim_, {j1_, j2_}, y_]] := {j1, j2};
GlobalRep[Operator[name_, rep_, dim_, {j1_, j2_}, y_]] := dynkin[rep];
DefectGlobalRep[Operator[name_, rep_, dim_, {j1_, j2_}, y_]] := If[appropriateGroup[rep] === DefectGlobalSymmetry[], dynkin[rep], decomposeRepDefect[rep]];

whichMultiplet[f_Operator] := whichMultiplet[f[[1]]];
whichMultiplet[name_] := SelectFirst[$multipletIndices, MemberQ[Flatten[Multiplet[#]][[;;,1]], name] &];

multipletOf[f_Operator] := With[{mm = Multiplet[whichMultiplet[f]]},
   If[MatchQ[mm[[1]], _List], SelectFirst[mm, MemberQ[#, f] &], mm]
];

Operator /: Conjugate[f : Operator[name_, rep_, dim_, js_, y_]] := 
 With[{r = ConjugateIrrep[GlobalSymmetry[], rep]},
    First@SortBy[Select[Flatten[Multiplet[whichMultiplet[f]]], SimpleRepInputConversion[$RSymmetry, #[[2]]] == r && #[[3]] == dim && #[[4]] == Reverse[js] && #[[5]] == -y &], # === f &] 
 ];
 
makeConjugate[f : Operator[name_, rep_, dim_, js_, y_]] := Operator[
   With[{nameExp = ToExpression[name, TraditionalForm]}, 
      If[FreeQ[nameExp, OverBar], 
         nameExp /. x_ :> ToString[OverBar[x], TraditionalForm],
         nameExp /. x_ :> ToString[x /. OverBar -> Identity, TraditionalForm]
      ]
   ],
   ConjugateIrrep[GlobalSymmetry[], rep],
   dim, Reverse[js], -y];
 
name2field[name_] := SelectFirst[Flatten[Multiplet[whichMultiplet[name]]], #[[1]] == name || #[[1]] == ToString[ToExpression[name], TraditionalForm] &];

IndexData[Spinor] = Index[2, "Greek", 1];
IndexData[DottedSpinor] = Index[2, "Greek", 1, OverDot];
IndexData[SpaceTime] = Index[4, "Greek", 12];
IndexData[GlobalIndex[rep_]] := 
  Index[repDim[rep], "Latin", 9, Subscript[#, repName[rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]] &];

IndexData[DefectGlobalIndex[defectRep_, parentRep_]] := 
  Index[repDim[defectRep], "Latin", 9, Subscript[Capitalize[#], Mouseover[repName[defectRep],repName[parentRep]]]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> ToString[ToExpression[s, TraditionalForm]] &];

Unprotect[Tensor];
Tensor[{x___, f_Operator, y___}] := Tensor[{x, ToTensor[f][[1, 1]], y}];
Tensor[names: {__String}] := Tensor[name2field[#] & /@ names];
Protect[Tensor];

\[Eta]Upper = 
  Tensor[{{"\[Eta]", Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\[Eta]", Raised[SpaceTime], Raised[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[{-SignatureFactor[]^2, 1, 1, 1}]];

\[Eta]Lower = 
  Tensor[{{"\[Eta]", Lowered[SpaceTime], Lowered[SpaceTime]}}];
BuildTensor[{"\[Eta]", Lowered[SpaceTime], Lowered[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[{-SignatureFactor[]^2, 1, 1, 1}]];

\[Eta]UpperDefect = 
  Tensor[{{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[DoubleVerticalBar]\)]\)", Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[DoubleVerticalBar]\)]\)", Raised[SpaceTime], Raised[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[PadLeft[Table[1, 4-$qdefect], 4]]];

\[Eta]LowerDefect = 
  Tensor[{{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[DoubleVerticalBar]\)]\)", Lowered[SpaceTime], Lowered[SpaceTime]}}];
BuildTensor[{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[DoubleVerticalBar]\)]\)", Lowered[SpaceTime], Lowered[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[PadLeft[Table[1, 4-$qdefect], 4]]];

\[Eta]UpperTransverse = 
  Tensor[{{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[UpTee]\)]\)", Raised[SpaceTime], Raised[SpaceTime]}}];
BuildTensor[{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[UpTee]\)]\)", Raised[SpaceTime], Raised[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[PadRight[{-SignatureFactor[]^2, 1, 1, 1}[[;;$qdefect]], 4]]];

\[Eta]LowerTransverse = 
  Tensor[{{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[UpTee]\)]\)", Lowered[SpaceTime], Lowered[SpaceTime]}}];
BuildTensor[{"\!\(\*SuperscriptBox[\(\[Eta]\), \(\[UpTee]\)]\)", Lowered[SpaceTime], Lowered[SpaceTime]}] := 
  SparseArray[DiagonalMatrix[PadRight[{-SignatureFactor[]^2, 1, 1, 1}[[;;$qdefect]], 4]]];

SetAttributes[\[Eta], Orderless];
\[Eta][attrs___] /; FreeQ[{attrs}, Lower | Upper] := \[Eta][attrs, Lower];
\[Eta][attrs___] /; FreeQ[{attrs}, Defect | Transverse | Full] := \[Eta][attrs, Full];
\[Eta][Upper, Full] = \[Eta]Upper;
\[Eta][Lower, Full] = \[Eta]Lower;
\[Eta][Upper, Defect] = \[Eta]UpperDefect;
\[Eta][Lower, Defect] = \[Eta]LowerDefect;
\[Eta][Upper, Transverse] = \[Eta]UpperTransverse;
\[Eta][Lower, Transverse] = \[Eta]LowerTransverse;
  
\[Sigma]LowerSingle[i_] :=  Tensor[{{\[Sigma]LowerTensor[i], Lowered[Spinor], Lowered[DottedSpinor]}}];
BuildTensor[{\[Sigma]LowerTensor[i_], Lowered[Spinor], Lowered[DottedSpinor]}] := SparseArray[{-SignatureFactor[], 1, 1, 1}[[i]] PauliMatrix[i - 1]];
\[Sigma]BarLowerSingle[i_] :=  Tensor[{{\[Sigma]BarLowerTensor[i], Raised[DottedSpinor], Raised[Spinor]}}];
BuildTensor[{\[Sigma]BarLowerTensor[i_], Raised[DottedSpinor], Raised[Spinor]}] := SparseArray[{SignatureFactor[], 1, 1, 1}[[i]] PauliMatrix[i - 1]];
\[Sigma]UpperSingle[i_] :=  Tensor[{{\[Sigma]UpperTensor[i], Lowered[Spinor], Lowered[DottedSpinor]}}];
BuildTensor[{\[Sigma]UpperTensor[i_], Lowered[Spinor], Lowered[DottedSpinor]}] := SparseArray[{SignatureFactor[]^3, 1, 1, 1}[[i]] PauliMatrix[i - 1]];
\[Sigma]BarUpperSingle[i_] :=  Tensor[{{\[Sigma]BarUpperTensor[i], Raised[DottedSpinor], Raised[Spinor]}}];
BuildTensor[{\[Sigma]BarUpperTensor[i_], Raised[DottedSpinor], Raised[Spinor]}] := SparseArray[{-SignatureFactor[]^3, 1, 1, 1}[[i]] PauliMatrix[i - 1]];

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
       
SetAttributes[\[Sigma], Orderless];
\[Sigma][attrs___] /; FreeQ[{attrs}, Lower | Upper] := \[Sigma][attrs, Lower];
\[Sigma][attrs___] /; FreeQ[{attrs}, Bar | NoBar] := \[Sigma][attrs, NoBar];
\[Sigma][attrs___] /; FreeQ[{attrs}, Commutator | Bare] := \[Sigma][attrs, Bare];
\[Sigma][attrs___] /; MemberQ[{attrs}, Bare] && FreeQ[{attrs}, Vector | 1 | 2 | 3 | 4] := \[Sigma][attrs, Vector];
\[Sigma][Lower, NoBar, Bare, Vector] = \[Sigma]Lower;
\[Sigma][Upper, NoBar, Bare, Vector] = \[Sigma]Upper;
\[Sigma][Lower, Bar, Bare, Vector] = \[Sigma]BarLower;
\[Sigma][Upper, Bar, Bare, Vector] = \[Sigma]BarUpper;
\[Sigma][Lower, NoBar, Commutator] = \[Sigma]CommLower;
\[Sigma][Upper, NoBar, Commutator] = \[Sigma]CommUpper;
\[Sigma][Lower, Bar, Commutator] = \[Sigma]CommLowerDot;
\[Sigma][Upper, Bar, Commutator] = \[Sigma]CommUpperDot;
\[Sigma][Lower, NoBar, Bare, n_Integer] = \[Sigma]LowerSingle[n];
\[Sigma][Upper, NoBar, Bare, n_Integer] = \[Sigma]UpperSingle[n];
\[Sigma][Lower, Bar, Bare, n_Integer] = \[Sigma]BarLowerSingle[n];
\[Sigma][Upper, Bar, Bare, n_Integer] = \[Sigma]BarUpperSingle[n];

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
    
\[Epsilon]Transverse[q_] := 
  Tensor[{{"\!\(\*SuperscriptBox[\(\[Epsilon]\), \(\[UpTee]\)]\)", Sequence @@ Table[Lowered[SpaceTime], q]}}];
BuildTensor[{"\!\(\*SuperscriptBox[\(\[Epsilon]\), \(\[UpTee]\)]\)", idxs : Lowered[SpaceTime]..}] /; Length[{idxs}] < 4 := 
  ArrayPad[LeviCivitaTensor[Length[{idxs}]], Table[{0, 4-Length[{idxs}]}, Length[{idxs}]]];
\[Epsilon]Defect[q_] := 
  Tensor[{{"\!\(\*SuperscriptBox[\(\[Epsilon]\), \(\[DoubleVerticalBar]\)]\)", Sequence @@ Table[Lowered[SpaceTime], 4 - q]}}];
BuildTensor[{"\!\(\*SuperscriptBox[\(\[Epsilon]\), \(\[DoubleVerticalBar]\)]\)", idxs : Lowered[SpaceTime]..}] /; Length[{idxs}] < 4 := 
  ArrayPad[LeviCivitaTensor[Length[{idxs}]], Table[{4-Length[{idxs}], 0}, Length[{idxs}]]];
  
SetAttributes[\[Epsilon], Orderless];
\[Epsilon][attrs___] /; FreeQ[{attrs}, Lower | Upper] := \[Epsilon][attrs, Lower];
\[Epsilon][attrs___] /; FreeQ[{attrs}, Spinor | SpaceTime] := \[Epsilon][attrs, Spinor];
\[Epsilon][attrs___] /; MemberQ[{attrs}, Spinor] && FreeQ[{attrs}, Dot | NoDot] := \[Epsilon][attrs, NoDot];
\[Epsilon][attrs___] /; MemberQ[{attrs}, SpaceTime] && FreeQ[{attrs}, Defect | Transverse | Full] := \[Epsilon][attrs, Full];
\[Epsilon][Lower, Spinor, NoDot] = \[Epsilon]Lower;
\[Epsilon][Upper, Spinor, NoDot] = \[Epsilon]Upper;
\[Epsilon][Lower, Spinor, Dot] = \[Epsilon]LowerDot;
\[Epsilon][Upper, Spinor, Dot] = \[Epsilon]UpperDot;
\[Epsilon][Lower, SpaceTime, Full] = \[Epsilon]Spacetime;
\[Epsilon][Upper, SpaceTime, Full] = \[Epsilon]SpacetimeUpper;
\[Epsilon][Lower, SpaceTime, Transverse] := \[Epsilon]Transverse[$qdefect];
\[Epsilon][Lower, SpaceTime, Defect] := \[Epsilon]Defect[$qdefect];

XX[i_] := Tensor[{{SpacetimePoint[i], Raised[SpaceTime]}}];
BuildTensor[{SpacetimePoint[i_], Raised[SpaceTime]}] := SparseArray@Table[x[i, k], {k, 4}];

XXDefect[i_] := Tensor[{{SpacetimePointDefect[i], Raised[SpaceTime]}}];
BuildTensor[{SpacetimePointDefect[i_], Raised[SpaceTime]}] := SparseArray[Table[{k} -> x[i, k], {k, If[$qdefect === None, {}, Range[$qdefect + 1, 4]]}], {4}];

XXTransverse[i_] := Tensor[{{SpacetimePointTransverse[i], Raised[SpaceTime]}}];
BuildTensor[{SpacetimePointTransverse[i_], Raised[SpaceTime]}] := SparseArray[Table[{k} -> x[i, k], {k, If[$qdefect === None, Range[4], Range[$qdefect]]}], {4}];

XX[i_, j_] := Tensor[{{SpacetimeSeparation[i, j], Raised[SpaceTime]}}];
BuildTensor[{SpacetimeSeparation[i_, j_], Raised[SpaceTime]}] := Components[XX[i]] - Components[XX[j]];

XXDefect[i_, j_] := Tensor[{{SpacetimeSeparationDefect[i, j], Raised[SpaceTime]}}];
BuildTensor[{SpacetimeSeparationDefect[i_, j_], Raised[SpaceTime]}] := Components[XXDefect[i]] - Components[XXDefect[j]];

XXTransverse[i_, j_] := Tensor[{{SpacetimeSeparationTransverse[i, j], Raised[SpaceTime]}}];
BuildTensor[{SpacetimeSeparationTransverse[i_, j_], Raised[SpaceTime]}] := Components[XXTransverse[i]] - Components[XXTransverse[j]];

SetAttributes[XXSquared, Orderless];
AddExplicitRule[
  XXSquared[i__] :> 
   Components[
    Contract[
     TensorProduct[XX[i], \[Eta]Lower, XX[i]], {{1, 2}, {3, 4}}]]];

SetAttributes[XXSquaredDefect, Orderless];
AppendTo[TensorTools`Private`explicitRules, 
  XXSquaredDefect[i__] :> 
   Components[
    Contract[
     TensorProduct[XXDefect[i], \[Eta]Lower, XXDefect[i]], {{1, 2}, {3, 4}}]]];

SetAttributes[XXSquaredTransverse, Orderless];
AddExplicitRule[
  XXSquaredTransverse[i__] :> 
   Components[
    Contract[
     TensorProduct[XXTransverse[i], \[Eta]Lower, XXTransverse[i]], {{1, 2}, {3, 4}}]]];

SetAttributes[XXDot, Orderless];
AddExplicitRule[
  XXDot[i_, j_] :> 
   Components[
    Contract[
     TensorProduct[XX[i], \[Eta]Lower, XX[j]], {{1, 2}, {3, 4}}]]];

SetAttributes[XXDotDefect, Orderless];
AddExplicitRule[
  XXDotDefect[i_, j_] :> 
   Components[
    Contract[
     TensorProduct[XXDefect[i], \[Eta]Lower, XXDefect[j]], {{1, 2}, {3, 4}}]]];

SetAttributes[XXDotTransverse, Orderless];
AddExplicitRule[
  XXDotTransverse[i_, j_] :> 
   Components[
    Contract[
     TensorProduct[XXTransverse[i], \[Eta]Lower, XXTransverse[j]], {{1, 2}, {3, 4}}]]];
    
SpinorX[i__] := Contract[TensorProduct[XX[i], \[Sigma]Lower], {{1, 2}}];
SpinorXDefect[i__] := Contract[TensorProduct[XXDefect[i], \[Sigma]Lower], {{1, 2}}];
SpinorXTransverse[i__] := Contract[TensorProduct[XXTransverse[i], \[Sigma]Lower], {{1, 2}}];

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
TensorTools`Private`DisplayTemplate[td_TensorDerivative] := 
  TensorTools`Private`DisplayTemplate[Symbolic[td]];
Format[td_TensorDerivative, TraditionalForm] := 
  TensorTools`Private`DisplayTemplate[td] /. 
   TensorTools`Private`dn[_, a_, b_] | TensorTools`Private`adn[_, a_, b_] :> DisplayName[a, b];
   
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


Options[Correlator] = {"Defect" -> False};
AddTensorHead[Correlator];
Correlator[expr_, opt : OptionsPattern[]] := Correlator[convertRToDefect[expr], opt] /; OptionValue[Correlator, "Defect"] && !FreeQ[expr, GlobalIndex]; 
Correlator[x_?NumericQ, OptionsPattern[]] := x;
Correlator[a_ b_, opt: OptionsPattern[]] /; FreeQ[a, Alternatives @@ TensorTools`Private`$TensorHeads] := a Correlator[b, opt];
Correlator[a_ + b_, opt: OptionsPattern[]] := Correlator[a, opt] + Correlator[b, opt];
Correlator[xs_List, opt: OptionsPattern[]] := Correlator[#, opt] & /@ xs;

readyToCorrelate[names_, already_] := 
  With[{poss = 
     Flatten[If[Position[names, #] =!= {}, 
         Position[names, #][[;; , 1]], {}] & /@ already]}, 
   poss === Range@Length[poss]];
findSwap[{1, ys___}, n_] := findSwap[{ys} - 1, n + 1];
findSwap[{x_, ys___}, n_] := {n + x, n + 1};

rPattern = "C" | SU2BreakingTensor[];
epsPattern = "\[Epsilon]" | "\!\(\*SuperscriptBox[\(\[Epsilon]\), \(\[DoubleVerticalBar]\)]\)";
sigmaPattern = \[Sigma]LowerTensor[_] | "\[Sigma]" | "\!\(\*OverscriptBox[\(\[Sigma]\), \(_\)]\)";

Correlator[t_Tensor | t_TensorPermute | t_Contract, opt: OptionsPattern[]] /; !readyToCorrelate[Symbolic[t], {rPattern}] := 
  With[{swap = findSwap[Position[Symbolic[t], rPattern][[;; , 1]], 0]}, 
   Correlator[SwapFactors[t, swap[[1]], swap[[2]]], opt]];
Correlator[t_Tensor | t_TensorPermute | t_Contract, opt: OptionsPattern[]] /; readyToCorrelate[Symbolic[t], {rPattern}] && !readyToCorrelate[Symbolic[t], {rPattern, epsPattern}] := 
  With[{swap = 
     findSwap[Position[Symbolic[t], rPattern | epsPattern][[;; , 1]], 
      0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]], opt]];
Correlator[t_Tensor | t_TensorPermute | t_Contract, opt: OptionsPattern[]] /; readyToCorrelate[Symbolic[t], {rPattern, epsPattern}] && ! readyToCorrelate[Symbolic[t], {rPattern, epsPattern, "\[Delta]"}] :=
   With[{swap = 
     findSwap[
      Position[Symbolic[t], rPattern | epsPattern | "\[Delta]"][[;; , 
        1]], 0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]], opt]];
Correlator[t_Tensor | t_TensorPermute | t_Contract, opt: OptionsPattern[]] /; readyToCorrelate[Symbolic[t], {rPattern, epsPattern, "\[Delta]"}] && ! readyToCorrelate[Symbolic[t], {rPattern, epsPattern, "\[Delta]", sigmaPattern}] :=
   With[{swap = 
     findSwap[
      Position[Symbolic[t], rPattern | epsPattern | "\[Delta]" | sigmaPattern][[;; , 
        1]], 0]}, Correlator[SwapFactors[t, swap[[1]], swap[[2]]], opt]];
Correlator[
   Tensor[{{s : rPattern | epsPattern | "\[Delta]" | sigmaPattern, rest__}, y___}], opt: OptionsPattern[]] := 
  TensorProduct[Tensor[{{s, rest}}], Correlator[Tensor[{y}], opt]];
Correlator[
    Tensor[{x___, {"\[PartialD]", Lowered[Spinor], 
       Lowered[DottedSpinor]}, {f_, idxs___}, y___}], opt: OptionsPattern[]] /; 
   f =!= "\[PartialD]" := 
  Correlator[
   Tensor[{x, {"\[PartialD]" <> f, Lowered[Spinor], 
      Lowered[DottedSpinor], idxs}, y}], opt];
Correlator[Contract[t_, pairs_, opt : OptionsPattern[]], opt2: OptionsPattern[]] /; 
   readyToCorrelate[Symbolic[t], {rPattern, epsPattern, "\[Delta]", sigmaPattern}] := 
  Contract[Correlator[t, opt2], pairs];
Correlator[TensorPermute[t_, perm_, OptionsPattern[]], opt2: OptionsPattern[]] /; 
   readyToCorrelate[Symbolic[t], {rPattern, epsPattern, "\[Delta]", sigmaPattern}] := 
  TensorPermute[Correlator[t, opt2], perm];
  
Symbolic[Correlator[t_, opt: OptionsPattern[]]] := {{"\[LeftAngleBracket]"}, 
   Sequence @@ Symbolic[t], {If[OptionValue[Correlator, "Defect"], "\!\(\*SubscriptBox[\(\[RightAngleBracket]\), \(\[ScriptCapitalD]\)]\)", "\[RightAngleBracket]"]}};
TensorTools`Private`DisplayTemplate[Correlator[t_, opt: OptionsPattern[]]] := 
  Row[{"\[LeftAngleBracket]", TensorTools`Private`DisplayTemplate[t], 
    If[OptionValue[Correlator, "Defect"], "\!\(\*SubscriptBox[\(\[RightAngleBracket]\), \(\[ScriptCapitalD]\)]\)", "\[RightAngleBracket]"]}];
Indices[Correlator[t_, opt: OptionsPattern[]]] := Indices[t];
Permutation[Correlator[t_, opt: OptionsPattern[]]] := Permutation[t];

CorrelatedFields[Correlator[t_, opt: OptionsPattern[]]] := Range@Length[Symbolic[t]];
CorrelatedFields[t_Tensor] := {};
CorrelatedFields[TensorPermute[t_, _]] := CorrelatedFields[t];
CorrelatedFields[Contract[t_, _]] := CorrelatedFields[t];
CorrelatedFields[TensorProduct[t1_, t2_]] := 
  Join[CorrelatedFields[t1], 
   Length[Symbolic[t1]] + CorrelatedFields[t2]];
CorrelatedFields[TensorDerivative[t_, _]] := 1 + CorrelatedFields[t];
CorrelatedFields[a_ b_] /; FreeQ[a, Tensor] := CorrelatedFields[b];