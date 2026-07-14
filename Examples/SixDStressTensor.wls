#!/usr/bin/env wolframscript
(* ::Package:: *)

(* The 6d stress tensor multiplets, ready to run.

       wolframscript -file Examples/SixDStressTensor.wls           (* (2,0) *)
       wolframscript -file Examples/SixDStressTensor.wls 10        (* (1,0) *)

   Operator content is Cordova-Dumitrescu-Intriligator, arXiv:1612.00809:
     (2,0):  D_1[0,0,0]^(0,2)_4, eq (5.132)   -- the 128 + 128 multiplet
     (1,0):  Bbar_3[0,0,0]^(0)_4, eq (5.129) with j1 = j2 = 0

   CAUTION when writing your own scripts: do NOT name a variable `sigma`, `x`, `y`, `z`,
   `point`, `eps`, `basis` or `metric` inside SCWIGEE's context.  Those are exported
   ConformalStructures symbols -- assigning to `sigma` clobbers the head of SigmaTensor's
   tensors, which silently breaks TensorSpinorDerivative and hence every DERIVATIVE
   correlator, while leaving everything else working. *)

(* load SCWIGEE from this checkout rather than whatever is installed *)
repoRoot = DirectoryName[DirectoryName[$InputFileName]];
If[repoRoot =!= "", PrependTo[$Path, repoRoot]];
Quiet[Needs["SCWIGEE`"], {Get::noopen}];

Begin["SCWIGEE`Private`"];
$consoleMode = True;

theory = If[Length[$ScriptCommandLine] >= 2 && $ScriptCommandLine[[2]] === "10", "(1,0)", "(2,0)"];
Print["6d ", theory, " stress tensor multiplet\n"];

SetSpacetimeDimension[6];

If[theory === "(2,0)",

   (* ---------------------------------------------------------------- *)
   (* 6d (2,0): R-symmetry USp(4) = SP4.  Q is in the fundamental 4 = (1,0).   *)
   (* CDI eq (5.132):                                                          *)
   (*   l=0  [0,0,0]^(0,2)_4      14   superconformal primary                  *)
   (*   l=1  [1,0,0]^(1,1)_{9/2}  16                                           *)
   (*   l=2  [0,1,0]^(2,0)_5      10   SU(2)-> USp(4) R-symmetry current       *)
   (*        [2,0,0]^(0,1)_5       5   self-dual tensor                        *)
   (*   l=3  [1,1,0]^(1,0)_{11/2}  4   supersymmetry current                   *)
   (*   l=4  [0,2,0]^(0,0)_6       1   stress tensor                           *)
   (* ---------------------------------------------------------------- *)
   SetGlobalSymmetry[{SP4}];
   ops = {
      Operator["O",           {{0, 2}}, 4,    {0, 0, 0}, 0],
      Operator["\[Psi]",      {{1, 1}}, 9/2,  {1, 0, 0}, 0],
      Operator["J",           {{2, 0}}, 5,    {0, 1, 0}, 0],
      Operator["Z",           {{0, 1}}, 5,    {2, 0, 0}, 0],
      Operator["S",           {{1, 0}}, 11/2, {1, 1, 0}, 0],
      Operator["T",           {{0, 0}}, 6,    {0, 2, 0}, 0]
   };
   SetMultiplet[ops, "(2,0) stress tensor", True, 1];
   ,
   (* ---------------------------------------------------------------- *)
   (* 6d (1,0): R-symmetry SU(2).  Q is in the doublet.                        *)
   (* CDI eq (5.129) with j1 = j2 = 0:                                         *)
   (*   l=0  [0,0,0]^(0)_4        Sigma                                        *)
   (*   l=1  [1,0,0]^(1)_{9/2}    zeta                                         *)
   (*   l=2  [0,1,0]^(2)_5        SU(2)_R current                              *)
   (*        [2,0,0]^(0)_5        self-dual tensor                             *)
   (*   l=3  [1,1,0]^(1)_{11/2}   supersymmetry current                        *)
   (*   l=4  [0,2,0]^(0)_6        stress tensor                                *)
   (* ---------------------------------------------------------------- *)
   SetGlobalSymmetry[{SU2}];
   ops = {
      Operator["\[CapitalSigma]", {{0}}, 4,    {0, 0, 0}, 0],
      Operator["\[Zeta]",         {{1}}, 9/2,  {1, 0, 0}, 0],
      Operator["J",               {{2}}, 5,    {0, 1, 0}, 0],
      Operator["Z",               {{0}}, 5,    {2, 0, 0}, 0],
      Operator["S",               {{1}}, 11/2, {1, 1, 0}, 0],
      Operator["T",               {{0}}, 6,    {0, 2, 0}, 0]
   };
   SetMultiplet[ops, "(1,0) stress tensor", True, 1];
];

Print["operator content:"];
Do[Print["   ", op[[1]], "  \[CapitalDelta]=", ScalingDimension[op],
      "  R=", RepName[GlobalSymmetry[], op[[2]]],
      "  Lorentz=", Spin[op], " (Young shape ", partition6[Spin[op]], ")",
      "  Q-level ", groupOf[op][[1]]],
   {op, ops}];

Print["\nSUSY ansatz:"];
Do[Print["   [Q, ", op[[1]], "] -> ", Length[PossibleQActions[op]], " structure(s)"], {op, ops}];

(* The top of the multiplet is expensive: <Q S T> carries 1 + 3 + 4 = 8 free Weyl indices,
   and ConformalStructures' StructureRelations runs out of memory on that basis.  Solve to
   a finite depth instead -- "MaxDepth" -> n keeps operators up to Q-level n - 1. *)
Print["\nSolving to MaxDepth -> 3 (everything below the stress tensor)..."];
DeclareAlgebra["MonitorProgress" -> False, "MaxDepth" -> 3];
sol = SUSYRules[1, "MaxDepth" -> 3];
Print["\nSUSYRules:"];
Do[Print["   ", r], {r, sol}];

End[];
