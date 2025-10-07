(* Wolfram Language package *)

Options[QTensor] = {"QBar" -> False, "Defect" -> False};
QTensor[OptionsPattern[]] := If[OptionValue["Defect"],
   defectSupercharge[$qdefect, Sequence @@ Switch[$qdefect, 1, {$q1angle}, 2, {$q2type, OptionValue["QBar"]}, 3, {$q3angle}]],
   If[OptionValue["QBar"],$QBarTensor, $QTensor]
];

ScalingDimension[Operator[name_, rep_, dim_, spin_, y_]] := dim;
Spin[Operator[name_, rep_, dim_, spin_, y_]] := spin;
GlobalRep[Operator[name_, rep_, dim_, spin_, y_]] := dynkin[rep];
DefectGlobalRep[Operator[name_, rep_, dim_, spin_, y_]] := If[appropriateGroup[rep] === DefectGlobalSymmetry[], dynkin[rep], decomposeRepDefect[rep]];

whichMultiplet[f_Operator] := whichMultiplet[f[[1]]];
whichMultiplet[name_] := SelectFirst[$multipletIndices, MemberQ[Flatten[Multiplet[#]][[;;,1]], name] || MemberQ[Flatten[Multiplet[#]][[;;,1]], ToString[ToExpression[name], TraditionalForm]] &];

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
   dim, If[ListQ[js], Reverse[js], js], -y];
 
name2field[name_] := SelectFirst[Flatten[Multiplet[whichMultiplet[name]]], #[[1]] == name || #[[1]] == ToString[ToExpression[name], TraditionalForm] &];

IndexData[GlobalIndex[rep_]] := 
  Index[repDim[rep], "Latin", 9, Subscript[#, repName[rep]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> 
  ToString[ToExpression[s, TraditionalForm]]] &];

IndexData[DefectGlobalIndex[defectRep_, parentRep_]] := 
  Index[repDim[defectRep], "Latin", 9, Subscript[Capitalize[#], Mouseover[repName[defectRep],repName[parentRep]]]/. s_?StringQ /; StringMatchQ[s, "\!" ~~ ___] :> ToString[ToExpression[s, TraditionalForm]] &];

Unprotect[Tensor];
Tensor[{x___, f_Operator, y___}] := Tensor[{x, ToTensor[f][[1, 1]], y}];
Tensor[names: {__String}] := Tensor[name2field[#] & /@ names];
Protect[Tensor];


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
epsPattern = chargeconj[] | _eps;
sigmaPattern = _sigma | "M" | _basis;

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
    Tensor[{x___, {"\[PartialD]", partialinds__}, {f_, idxs___}, y___}], opt: OptionsPattern[]] /; 
   f =!= "\[PartialD]" := 
  Correlator[
   Tensor[{x, {"\[PartialD]" <> f, partialinds, idxs}, y}], opt];
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

crossRatios = ConformalStructures`Private`crossRatios;