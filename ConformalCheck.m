(* Wolfram Language package *)

RotationSpinor = 
  Tensor[{{"M", Raised[SpaceTime], Lowered[SpaceTime], 
     Lowered[Spinor], Raised[Spinor]}}];
RotationDottedSpinor = 
  Tensor[{{"M", Raised[SpaceTime], Lowered[SpaceTime], 
     Lowered[DottedSpinor], Raised[DottedSpinor]}}];

BuildTensor[{"M", Raised[SpaceTime], Lowered[SpaceTime], 
    Lowered[Spinor], Raised[Spinor]}] := 
  1/2 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Eta]Upper, \[Sigma]Lower, \[Sigma]BarLower], \
{{2, 3}, {5, 7}}] - 
      Contract[
       TensorProduct[\[Eta]Upper, 
        TensorPermute[
         TensorProduct[\[Sigma]Lower, \[Sigma]BarLower], {4, 2, 3, 1, 
          5, 6}]], {{2, 3}, {5, 7}}]], 
     Ordering[{Raised[SpaceTime], Lowered[SpaceTime], Lowered[Spinor],
        Raised[Spinor]}]];

BuildTensor[{"M", Raised[SpaceTime], Lowered[SpaceTime], 
    Lowered[DottedSpinor], Raised[DottedSpinor]}] := - 1/
   2 TensorTranspose[
    CanonicallyOrderedComponents[
     Contract[
       TensorProduct[\[Eta]Upper, \[Sigma]BarLower, \[Sigma]Lower], \
{{2, 3}, {5, 7}}] - 
      Contract[
       TensorProduct[\[Eta]Upper, 
        TensorPermute[
         TensorProduct[\[Sigma]BarLower, \[Sigma]Lower], {4, 2, 3, 1, 
          5, 6}]], {{2, 3}, {5, 7}}]], 
     Ordering[{Raised[SpaceTime], Lowered[SpaceTime], 
       Lowered[DottedSpinor], Raised[DottedSpinor]}]];

ConformalCheck[t : Tensor[{{SpacetimeStructure[deltas_, spins_, {}, perm_, q_, i_], idxs___}}]] := ConformalCheck[deltas, spins, perm, t];

ConformalCheck[deltas_, spins_, perm_, t_] := 
  With[{xs = 
      Flatten[Table[{perm[[i]], 
         2 Total[Flatten@spins[[;; i - 1]]] + j}, {i, 
         Length[deltas]}, {j, 2 spins[[i, 1]]}], 1], 
     xsDot = Flatten[
       Table[{perm[[i]], 
         2 Total[Flatten@spins[[;; i - 1]]] + 2 spins[[i, 1]] + 
          j}, {i, Length[deltas]}, {j, 2 spins[[i, 2]]}], 1]},
    Sum[2 Contract[
         TensorProduct[XX[i], XX[i], 
          TensorDerivative[t, i]], {{2, 3}}] - 
       XXSquared[i] Contract[
         TensorProduct[\[Eta]Upper, 
          TensorDerivative[t, i]], {{2, 3}}], {i, Length[deltas]}] + 
     2 Sum[deltas[[InversePermutation[perm][[i]]]] TensorProduct[
         XX[i], t], {i, Length[deltas]}] +
     Sum[
      TensorPermute[
       Contract[
        TensorProduct[XX[xs[[idx, 1]]], RotationSpinor, 
         t], {{1, 3}, {5, 5 + xs[[idx, 2]]}}], 
       InversePermutation@
        PermutationList[Cycles[{Prepend[2 + xs[[;; idx - 1, 2]], 2]}],
          1 + 2 Total[Flatten[spins]]]], {idx, Length[xs]}] + 
     Sum[TensorPermute[
       Contract[
        TensorProduct[XX[xsDot[[idx, 1]]], RotationDottedSpinor, 
         t], {{1, 3}, {5, 5 + xsDot[[idx, 2]]}}], 
       InversePermutation@
        PermutationList[
         Cycles[{Prepend[2 + xsDot[[;; idx - 1, 2]], 2]}], 
         1 + 2 Total[Flatten[spins]]]], {idx, Length[xsDot]}]
    ];