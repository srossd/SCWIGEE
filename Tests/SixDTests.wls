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
