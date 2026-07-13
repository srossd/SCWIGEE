#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Regression suite for SCWIGEE.

   Runs standalone from the repository root:

       wolframscript -file Tests/RegressionTests.wls

   The main suite below pins the d = 4 behaviour (N = 1 chiral multiplet, solved
   end to end) so that the d = 6 work cannot silently change it.  It then Get[]s
   Tests/SixDTests.wls, which shares the same $passed / $failed tally and harness. *)

repoRoot = DirectoryName[DirectoryName[$InputFileName]];
If[repoRoot === "", repoRoot = Directory[]];
PrependTo[$Path, repoRoot];

(* SCWIGEE's init.m Get[]s SCWIGEE`ConformalCheck`, which is not in the checkout;
   that Get failure is pre-existing and unrelated to the suite. *)
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

(* the package does everything in its private context *)
Begin["SCWIGEE`Private`"];
$consoleMode = True;

(* ------------------------------------------------------------------ *)
Print["\nd = 4: N = 1 chiral multiplet"];
(* ------------------------------------------------------------------ *)

(* Q carries U(1)_R charge +1, raises the dimension by 1/2 and j1 by 1/2.  A
   generic (non-free) scaling dimension is used: at the free value D[phi] = 1 the
   <Q psi Fbar> equation degenerates and the multiplet has no consistent solution. *)
SetSpacetimeDimension[4];
SetGlobalSymmetry[{U1}];
d4 = 3;
phi  = Operator["\[Phi]", {1}, d4, {0, 0}, 1];
psi  = Operator["\[Psi]", {2}, d4 + 1/2, {1/2, 0}, 2];
ff   = Operator["F", {3}, d4 + 1, {0, 0}, 3];
phib = Operator["\!\(\*OverscriptBox[\(\[Phi]\), \(_\)]\)", {-1}, d4, {0, 0}, -1];
psib = Operator["\!\(\*OverscriptBox[\(\[Psi]\), \(_\)]\)", {-2}, d4 + 1/2, {0, 1/2}, -2];
fb   = Operator["\!\(\*OverscriptBox[\(F\), \(_\)]\)", {-3}, d4 + 1, {0, 0}, -3];
SetMultiplet[{{phi, psi, ff}, {phib, psib, fb}}, "chiral", False, 1];

assert["Q tensor carries a Weyl index", Indices[$QTensor][[2]] === Lowered[WeylSpinor[4]]];
assert["Qbar tensor carries a dotted Weyl index", Indices[$QBarTensor][[2]] === Lowered[DottedWeylSpinor[4]]];
assert["hasQBar[] in d=4", TrueQ[hasQBar[]]];
assert["ToTensor[psi] has one undotted index",
   Indices[ToTensor[psi]] === {Raised[GlobalIndex[{2}]], Lowered[WeylSpinor[4]]},
   Indices[ToTensor[psi]]];
assert["Conjugate[phi] === phib", Conjugate[phi] === phib];
assert["groupOf grades the multiplet by (nQ, nQbar)",
   {groupOf[phi], groupOf[psi], groupOf[ff]} === {{0, 0, False}, {1, 0, False}, {2, 0, False}},
   {groupOf[phi], groupOf[psi], groupOf[ff]}];

(* [Q, phi] ~ psi ; [Q, psi] ~ eps_{ab} F ; [Qbar, phi] = 0 ; [Qbar, psi] ~ d phi *)
assert["[Q, phi] has one structure", Length[PossibleQActions[phi]] == 1];
assert["[Q, phi] is proportional to psi",
   ! FreeQ[PossibleQActions[phi], "\[Psi]"]];
assert["[Q, psi] is proportional to eps.F",
   Length[PossibleQActions[psi]] == 1 && ! FreeQ[PossibleQActions[psi], "F"] &&
      ! FreeQ[PossibleQActions[psi], chargeconj[]]];
assert["[Qbar, phi] = 0 (phi is chiral)", QAnsatz[phi, "QBar" -> True] === 0];
assert["[Qbar, psi] is proportional to d phi",
   ! FreeQ[QAnsatz[psi, "QBar" -> True], "\[PartialD]"] &&
      ! FreeQ[QAnsatz[psi, "QBar" -> True], "\[Phi]"]];

DeclareAlgebra["MonitorProgress" -> False];
lin4  = linearEquations[1];
quad4 = quadraticEquations[1];
(* counts pinned against the pristine pre-6D code *)
assert["4 linear equations", Length[lin4] == 4, Length[lin4]];
assert["7 quadratic equations", Length[quad4] == 7, Length[quad4]];

sol4 = SUSYRules[1];
assert["SUSYRules solves the multiplet", MatchQ[sol4, {__Rule}], sol4];
assert["the solution satisfies every linear equation",
   AllTrue[Simplify[lin4 /. sol4], TrueQ], Simplify[lin4 /. sol4]];
assert["the solution satisfies every quadratic equation",
   AllTrue[Simplify[quad4 /. sol4], TrueQ], Simplify[quad4 /. sol4]];
assert["no SUSY coefficient is left unfixed",
   FreeQ[sol4[[;; , 2]], _SUSYCoefficient], sol4];
assert["[Q, psi] is nonzero in the solution",
   Simplify[SUSYCoefficient["\[Psi]", 1, "QBar" -> False] /. sol4] =!= 0];

End[];

(* ------------------------------------------------------------------ *)
$sixDIncluded = True;
Get[FileNameJoin[{repoRoot, "Tests", "SixDTests.wls"}]];
(* ------------------------------------------------------------------ *)

Print["\n", $passed, " passed, ", $failed, " failed."];
Exit[If[$failed > 0, 1, 0]];
