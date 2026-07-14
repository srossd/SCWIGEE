#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Validation for d = 6 (SO(5,1) ~ SU*(4)) support in SCWIGEE.

   Runs standalone from the repository root:

       wolframscript -file Tests/SixDTests.wls

   It is also Get[] by Tests/RegressionTests.wls, which runs the whole suite in one
   process; RegressionTests sets $sixDIncluded, so the package load, the harness and
   the summary/Exit below are skipped and the two suites share one tally. *)

If[! TrueQ[$sixDIncluded],
   repoRoot = DirectoryName[DirectoryName[$InputFileName]];
   If[repoRoot === "", repoRoot = Directory[]];
   PrependTo[$Path, repoRoot];
   Quiet[Needs["SCWIGEE`"], {Get::noopen}];
   loaded = FindFile["SCWIGEE`"];
   Print["Loaded package from: ", loaded];
   forwardSlash[p_] := StringReplace[p, "\\" -> "/"];
   If[! StringContainsQ[forwardSlash[loaded], forwardSlash[repoRoot]],
      Print["ERROR: package loaded from ", loaded, ", not the repository checkout (", repoRoot, ")."];
      Exit[1]];
   $passed = 0; $failed = 0;
   assert[label_, cond_, info_: ""] := If[TrueQ[cond],
      $passed++; Print["  PASS  ", label],
      $failed++; Print["  FAIL  ", label, If[info === "", "", "  (" <> ToString[info, InputForm] <> ")"]]];
];

Begin["SCWIGEE`Private`"];
$consoleMode = True;

(* ------------------------------------------------------------------ *)
Print["\nd = 6: SU(4) representation layer"];
(* ------------------------------------------------------------------ *)

scal = {0, 0, 0};   (* 1   *)
spin = {1, 0, 0};   (* 4   *)
sdt  = {2, 0, 0};   (* 10  -- the self-dual tensor of the (1,0)/(2,0) tensor multiplet *)
vec  = {0, 1, 0};   (* 6   -- also the index structure of the derivative *)
stt  = {0, 2, 0};   (* 20' -- stress tensor: a mixed Young shape *)
abar = {0, 0, 1};   (* 4bar -- dotted *)

assert["Dynkin -> Young: irrep dimensions come out right",
   (su4IrrepDimension /@ {scal, spin, sdt, vec, stt, {1, 1, 0}, abar, {1, 0, 1}}) ===
      {1, 4, 10, 6, 20, 20, 4, 15},
   su4IrrepDimension /@ {scal, spin, sdt, vec, stt, {1, 1, 0}, abar, {1, 0, 1}}];

assert["box counts", (boxes6 /@ {scal, spin, sdt, vec, stt, abar}) === {0, 1, 2, 2, 4, 1},
   boxes6 /@ {scal, spin, sdt, vec, stt, abar}];
assert["partitions", (partition6 /@ {spin, sdt, vec, stt}) === {{1}, {2}, {1, 1}, {2, 2}},
   partition6 /@ {spin, sdt, vec, stt}];
assert["chirality: a >= c is undotted, a < c is dotted",
   (dottedQ6 /@ {scal, spin, sdt, vec, stt, abar}) === {False, False, False, False, False, True}];
(* A dotted rep is realized on the CONJUGATE diagram, so the 4bar is one dotted index
   rather than three undotted ones: a rep and its conjugate carry equal box counts. *)
assert["a dotted rep uses the conjugate diagram, so 4 and 4bar carry equally many indices",
   boxes6[abar] === boxes6[spin] && partition6[abar] === {1},
   {boxes6[abar], partition6[abar]}];
assert["a scalar has no row symmetry generators (empty partition)",
   rowSymmetryGenerators6[scal] === {}, rowSymmetryGenerators6[scal]];
assert["index heads", {indexHead6[spin], indexHead6[abar]} === {WeylSpinor, DottedWeylSpinor}];

(* the row symmetry is the part of the Young symmetry that is a permutation symmetry *)
assert["row generators: [2,0,0] (one row of 2) is symmetric in its block",
   rowSymmetryGenerators6[sdt] === {{Cycles[{{2, 3}}], 1}}, rowSymmetryGenerators6[sdt]];
assert["row generators: [0,1,0] (one column of 2) has no row symmetry",
   rowSymmetryGenerators6[vec] === {}, rowSymmetryGenerators6[vec]];
assert["row generators: [0,2,0] (shape {2,2}) is symmetric within each row, not across",
   rowSymmetryGenerators6[stt] === {{Cycles[{{2, 3}}], 1}, {Cycles[{{4, 5}}], 1}},
   rowSymmetryGenerators6[stt]];

(* The Young projector must cut the reducible index block down to the irrep: the rank of
   the projection acting on (C^4)^n is exactly the irrep dimension.  This is what makes
   a mixed shape like [0,2,0] (21 components with the row/column symmetry alone, but 20
   in the irrep) come out right. *)
projRank[rep_] := Module[{n = boxes6[rep], arr, vars},
   vars = Array[cc, ConstantArray[4, n]];
   arr = youngProjectTensor6[vars, rep];
   MatrixRank[Outer[D, Flatten[arr], Flatten[vars]]]];
assert["Young projection has rank = irrep dimension, for every shape incl. mixed [0,2,0]",
   (projRank /@ {spin, sdt, vec, stt}) === (su4IrrepDimension /@ {spin, sdt, vec, stt}),
   {projRank /@ {spin, sdt, vec, stt}, su4IrrepDimension /@ {spin, sdt, vec, stt}}];

(* ------------------------------------------------------------------ *)
Print["\nd = 6: SU(4) invariant tensors"];
(* ------------------------------------------------------------------ *)

SetSpacetimeDimension[6];
Mu = Normal@Components[RotationGenerators[6, "Weyl" -> True]];
Md = Normal@Components[RotationGenerators[6, "Weyl" -> True, "Dotted" -> True]];

assert["Weyl spinors have 4 components in d = 6", IndexData[WeylSpinor[6]][[1]] === 4];
assert["sigma^mu_{AB} is {Weyl, Weyl} and antisymmetric (the vector is Lambda^2 of the 4)",
   Indices[SigmaTensor[6]] === {Lowered[Spacetime[6]], Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]]} &&
      With[{s = Normal@Components[SigmaTensor[6]]}, AllTrue[Range[6], s[[#]] == -Transpose[s[[#]]] &]]];
assert["the Weyl charge conjugation matrix VANISHES in d = 6, so no 2-index epsilon exists",
   Normal@Components[WeylChargeConjugationMatrix[6]] === ConstantArray[0, {4, 4}]];
assert["the derivative d_{AB} is declared antisymmetric",
   TensorSymmetries["\[PartialD]"] === {{Cycles[{{1, 2}}], -1}}, TensorSymmetries["\[PartialD]"]];

(* eps_{ABCD} is invariant precisely because the generators are traceless *)
assert["eps_{ABCD} is SU(4) invariant (generators are traceless)",
   AllTrue[Flatten@Table[Tr[Mu[[m, n]]], {m, 6}, {n, 6}], # === 0 &]];
assert["eps_{ABCD} components are the rank-4 Levi-Civita",
   Normal@Components[SU4Epsilon[]] === Normal@LeviCivitaTensor[4]];
assert["eps is declared totally antisymmetric",
   TensorSymmetries[su4eps[]] === {Antisymmetric[{1, 2, 3, 4}]}];

(* K_{A Bdot} is the ONLY bridge between the two chiralities.  Re-derive it here from the
   generators so the hard-coded matrix in SixD.m cannot drift from the package's basis. *)
KL = Normal@Components[SU4Pairing[]];
KR = Normal@Components[SU4Pairing["Raised" -> True]];
assert["K_{A Bdot} is invariant: Mu^T.K + K.Md = 0",
   AllTrue[Flatten@Table[Transpose[Mu[[m, n]]] . KL + KL . Md[[m, n]], {m, 6}, {n, 6}], # === 0 &]];
assert["K^{A Bdot} is invariant: Mu.K + K.Md^T = 0",
   AllTrue[Flatten@Table[Mu[[m, n]] . KR + KR . Transpose[Md[[m, n]]], {m, 6}, {n, 6}], # === 0 &]];
assert["K^{A Cdot} K_{B Cdot} = delta^A_B", KR . Transpose[KL] === IdentityMatrix[4]];
assert["K is NOT the identity (it is antisymmetric, with K.K = -1)",
   KL =!= IdentityMatrix[4] && KL === -Transpose[KL] && KL . KL === -IdentityMatrix[4], KL];

(* ------------------------------------------------------------------ *)
Print["\nd = 6: supercharge and algebra"];
(* ------------------------------------------------------------------ *)

SetGlobalSymmetry[{SU2}];   (* (1,0): R-symmetry SU(2), Q in the doublet *)

assert["no Qbar in d = 6 (the supercharge is chiral)", ! TrueQ[hasQBar[]]];
assert["Qbar does exist in d = 4", SetSpacetimeDimension[4]; TrueQ[hasQBar[]]];
SetSpacetimeDimension[6]; SetGlobalSymmetry[{SU2}];
assert["no U(1) R-charge in d = 6", ! TrueQ[hasU1RCharge[]]];
assert["Q_A^i carries one undotted Weyl index and an R-symmetry index",
   Indices[$QTensor] === {Raised[GlobalIndex[QGlobalRep[]]], Lowered[WeylSpinor[6]]},
   Indices[$QTensor]];

(* The (1,0) tensor multiplet: phi [0,0,0], psi^i_A [1,0,0], H^+_{AB} [2,0,0], with psi
   in the SU(2)_R doublet and phi, H singlets.  There is no U(1) R-charge, so y = 0. *)
sing6 = singRep[GlobalSymmetry[]];
fund6 = fundRep[GlobalSymmetry[]];
phi6 = Operator["\[Phi]", sing6, 2, scal, 0];
psi6 = Operator["\[Psi]", fund6, 5/2, spin, 0];
h6   = Operator["H", sing6, 3, sdt, 0];
SetMultiplet[{phi6, psi6, h6}, "tensor", True, 1];

assert["ToTensor[phi] has no spinor indices", spinorIndicesOf[phi6] === {}];
assert["ToTensor[psi] has one undotted Weyl index",
   spinorIndicesOf[psi6] === {Lowered[WeylSpinor[6]]}, spinorIndicesOf[psi6]];
assert["ToTensor[H] has two undotted Weyl indices",
   spinorIndicesOf[h6] === {Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]]}, spinorIndicesOf[h6]];
assert["H's index block is declared symmetric (one row of two boxes)",
   TensorSymmetries["H"] === {{Cycles[{{2, 3}}], 1}}, TensorSymmetries["H"]];

(* SetSymmetries must declare the symmetry the PROJECTOR gives, not merely the row
   symmetry: a single-column rep has no row symmetry at all and yet is antisymmetric.
   Declaring {} for it would leave the vector with no symmetry whatsoever. *)
Block[{$multiplet = $multiplet, $multipletIndices = $multipletIndices},
   SetMultiplet[{Operator["V", sing6, 5, vec, 0]}, "v", True, 2];
   assert["a single-column rep [0,1,0] is declared ANTIsymmetric, not symmetryless",
      TensorSymmetries["V"] === {{Cycles[{{2, 3}}], -1}}, TensorSymmetries["V"]];
];

(* a dotted operator really does get dotted indices *)
chib = Operator["\[Chi]", fund6, 5/2, abar, 0];
assert["a dotted rep [0,0,1] yields one dotted Weyl index",
   Block[{$multiplet = <|1 -> {chib}|>}, spinorIndicesOf[chib]] === {Lowered[DottedWeylSpinor[6]]},
   Block[{$multiplet = <|1 -> {chib}|>}, spinorIndicesOf[chib]]];

(* the multiplet is graded by dimension alone, two Q's per unit of dimension *)
assert["multiplet graded by dimension: phi, psi, H sit at Q-levels 0, 1, 2",
   {groupOf[phi6], groupOf[psi6], groupOf[h6]} === {{0, 0, False}, {1, 0, False}, {2, 0, False}},
   {groupOf[phi6], groupOf[psi6], groupOf[h6]}];
assert["opGroup recovers each level", {opGroup[1, 0, 0], opGroup[1, 1, 0], opGroup[1, 2, 0]} === {{phi6}, {psi6}, {h6}},
   {opGroup[1, 0, 0], opGroup[1, 1, 0], opGroup[1, 2, 0]}];
assert["the chiral {Q,Q} shifts the level by two, not one",
   quadraticShifts[] === {{-2, 0}, {0, 0}, {2, 0}}, quadraticShifts[]];

(* ------------------------------------------------------------------ *)
Print["\nd = 6: wizard and expansion plumbing"];
(* ------------------------------------------------------------------ *)

(* A supercharge always steps diagonally on the lattice.  With a U(1) R-charge, Q and Qbar
   arrive from the two diagonals; in d = 6 there is a single supercharge, so only one. *)
assert["one supercharge means one diagonal move in d = 6",
   qLatticeMoves[] === {{-1, -1/2}}, qLatticeMoves[]];
assert["...and both diagonals in d = 4, where there is also a Qbar",
   Block[{$D = 4}, qLatticeMoves[]] === {{-1, -1/2}, {1, -1/2}}];

(* DisplayMultiplet lays operators out at {x, Delta}, and a supercharge always steps
   DIAGONALLY.  Where there is a U(1) R-charge, x is that charge.  Where there is none the
   Q-level -- 2 (Delta - Delta_bottom) -- plays the same role, so the multiplet is still a
   diagonal chain rather than collapsing onto a single vertical line at x = 0. *)
Module[{ops, grp},
   ops = {Operator["A", sing6, 4, scal, 0], Operator["B", fund6, 9/2, spin, 0],
          Operator["C", sing6, 5, sdt, 0], Operator["D", sing6, 5, vec, 0]};
   grp = multipletGroups[ops];
   assert["without a U(1) R-charge the lattice x is the Q-level, not 0",
      Sort[Keys[grp]] === {{0, 4}, {1, 9/2}, {2, 5}}, Sort[Keys[grp]]];
   assert["...so the multiplet is a diagonal chain, with every level joined",
      Length[multipletEdges[grp]] == 2, multipletEdges[grp]];
   assert["...and the diagram has a nonzero width", multipletImageWidth[grp] > 0];
];
assert["the edge rule is now the same in every dimension (a supercharge moves diagonally)",
   qAdjacentQ[{1, 3}, {2, 7/2}] && ! qAdjacentQ[{1, 3}, {1, 7/2}]];

(* Index symmetry is shown with brackets, taken from the same Young data SetSymmetries uses:
   (...) for a symmetrized block, [...] for an antisymmetrized one. *)
assert["a single row is symmetric: [2,0,0] -> one (..) block of two",
   operatorIndexBlocks[sdt] === {{2, 1}}, operatorIndexBlocks[sdt]];
assert["a single column is ANTIsymmetric: [0,1,0] -> one [..] block of two",
   operatorIndexBlocks[vec] === {{2, -1}}, operatorIndexBlocks[vec]];
assert["a mixed shape brackets each row: [1,1,0] -> (ab)c, [0,2,0] -> (ab)(cd)",
   {operatorIndexBlocks[{1, 1, 0}], operatorIndexBlocks[stt]} ===
      {{{2, 1}, {1, 1}}, {{2, 1}, {2, 1}}},
   {operatorIndexBlocks[{1, 1, 0}], operatorIndexBlocks[stt]}];
assert["the brackets agree with the symmetry actually declared",
   And @@ Table[
      With[{blocks = operatorIndexBlocks[r], gens = youngSymmetryGenerators6[r]},
         (* a two-index block is declared symmetric iff its bracket is round *)
         If[blocks === {{2, 1}}, gens === {{Cycles[{{2, 3}}], 1}},
            If[blocks === {{2, -1}}, gens === {{Cycles[{{2, 3}}], -1}}, True]]],
      {r, {sdt, vec}}]];
assert["d = 4 brackets the undotted and dotted blocks separately",
   Block[{$D = 4}, operatorIndexBlocks[{1, 1/2}]] === {{2, 1}, {1, 1}}];

(* the two-point phase is I^(chirality imbalance); in d = 6 that is the whole box count *)
assert["two-point phase: 1 for a scalar, I for a spinor",
   {twoPtPhase[scal], twoPtPhase[spin]} === {1, I}, {twoPtPhase[scal], twoPtPhase[spin]}];
assert["two-point phase in d = 4 is unchanged",
   Block[{$D = 4}, {twoPtPhase[{0, 0}], twoPtPhase[{1/2, 0}]}] === {1, I}];

(* ConformalStructures has no defect structures in d = 6 *)
assert["setting a defect codimension in d = 6 is refused with a message",
   Block[{$qdefect = None},
      Quiet[Check[SetDefectCodimension[2]; False, True, SetDefectCodimension::nodefect],
         SetDefectCodimension::nodefect] && $qdefect === None]];

(* a 6D operator prints with one chirality's worth of indices, not an undotted+dotted pair *)
assert["formatting gives an operator boxes6 indices of a single chirality",
   With[{ind = ToString[TraditionalForm[Operator["O", sing6, 3, stt, 0]]]},
      StringQ[ind] && ind =!= ""]];

(* ------------------------------------------------------------------ *)
Print["\nd = 6: the QAnsatz pipeline on the (1,0) tensor multiplet"];
(* ------------------------------------------------------------------ *)

(* The known variations are  [Q,phi] ~ psi,  [Q,psi] ~ H_{AB} + d_{AB} phi,
   [Q,H] ~ d_{A(B} psi_{C)}.  Note d_{BC} psi_A must die: it is antisymmetric in H's two
   slots, which H's Young projection symmetrizes. *)
namesIn[s_] := DeleteDuplicates@Cases[s, nm_String /; MemberQ[{"\[Phi]", "\[Psi]", "H", "\[PartialD]"}, nm], All];
builtFrom[op_] := Sort[namesIn /@ PossibleQActions[op]];

assert["[Q, phi] has exactly one structure, and it is psi",
   builtFrom[phi6] === {{"\[Psi]"}}, builtFrom[phi6]];
assert["[Q, psi] has exactly two structures: H and d phi",
   Sort[builtFrom[psi6]] === Sort[{{"H"}, {"\[PartialD]", "\[Phi]"}}], builtFrom[psi6]];
assert["[Q, H] has exactly one structure, d psi",
   builtFrom[h6] === {{"\[PartialD]", "\[Psi]"}}, builtFrom[h6]];

(* The surviving [Q,H] structure must carry H's symmetry.  In the ansatz index order
   [Q's R index, O's R index, Q's spinor index, O's spinor indices], H's block is slots 4,5. *)
qh = structureComponents6[First@PossibleQActions[h6]];
assert["[Q, H] is symmetric in H's two slots",
   AllTrue[Flatten[Expand[qh - Transpose[qh, {1, 2, 3, 5, 4}]]], PossibleZeroQ]];
assert["[Q, H] is not identically zero", ! AllTrue[Flatten[Expand[qh]], PossibleZeroQ]];

(* the antisymmetric derivative is what kills d_{BC} psi_A *)
assert["d_{AB} is antisymmetric in components (not merely declared so)",
   With[{cd = Normal@Components[Tensor[{{"\[PartialD]", lowW6, lowW6}}]]},
      Expand[cd + Transpose[cd]] === ConstantArray[0, {4, 4}] && Diagonal[cd] === {0, 0, 0, 0}]];

(* an operator's own components are Young-projected, not just symmetry-canonicalised *)
assert["H's components are symmetric in its two Weyl slots",
   With[{ch = Normal@Components[ToTensor[h6]]},
      Expand[ch - Transpose[ch, {1, 3, 2}]] === ConstantArray[0, Dimensions[ch]]]];

assert["QAnsatz assigns one coefficient per structure",
   Length[DeleteDuplicates@Cases[QAnsatz[psi6], _SUSYCoefficient, All]] == 2,
   Cases[QAnsatz[psi6], _SUSYCoefficient, All]];

(* {Q_A^i, Q_B^j} = Omega^{ij} P_{AB}: R-symmetry factor is the symplectic 2pt invariant,
   and P carries two undotted Weyl indices *)
qz = quadraticZero[phi6];
assert["quadraticZero[6] is nonzero and built on the {Q,Q} anticommutator",
   qz =!= 0 && Head[qz] === Plus && Head[qz] =!= Which];
assert["...its momentum term carries d_{AB} with two undotted Weyl indices",
   ! FreeQ[qz, {"\[PartialD]", Lowered[WeylSpinor[6]], Lowered[WeylSpinor[6]]}]];
(* The whole algebra is chiral: no dotted index occurs anywhere in it. *)
assert["...the algebra never mentions a dotted spinor", FreeQ[qz, DottedWeylSpinor]];
(* In d = 4 the R-symmetry factor is a Kronecker delta with one raised and one lowered
   index, pairing Q with Qbar.  In d = 6 it is the symplectic invariant Omega^{ij} with
   BOTH indices raised, pairing Q with Q. *)
assert["...and pairs the two Q's with a both-indices-raised symplectic R-symmetry invariant",
   ! FreeQ[qz, {"\[Delta]", Raised[_GlobalIndex], Raised[_GlobalIndex]}]];

End[];

If[! TrueQ[$sixDIncluded],
   Print["\n", $passed, " passed, ", $failed, " failed."];
   Exit[If[$failed > 0, 1, 0]]
];
