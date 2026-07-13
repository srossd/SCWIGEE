(* Wolfram Language package *)

(* ------------------------------------------------------------------------- *)
(* d = 6 representation layer.                                                *)
(*                                                                            *)
(* In d = 6 the Lorentz group is SO(5,1) ~ SU*(4), so an operator's "spin" is *)
(* an SU(4) Dynkin label {a, b, c} rather than a number (d = 3) or a pair     *)
(* (d = 2, 4).  It carries a + 2b + 3c Weyl-spinor indices of a single        *)
(* chirality -- undotted (the 4) when a >= c, dotted (the 4bar) otherwise --   *)
(* Young-projected onto the diagram with rows {a+b+c, b+c, c}.                *)
(*                                                                            *)
(* The Dynkin -> Young pipeline is shared with ConformalStructures rather than *)
(* reimplemented, so the index ordering and projection conventions used to     *)
(* build SUSY ansatzes here cannot drift from the ones used to build the       *)
(* conformal structures the Ward identities are eventually expanded into.      *)
(* ------------------------------------------------------------------------- *)

dynkinToPartition    = ConformalStructures`Private`dynkinToPartition;
validDynkinQ         = ConformalStructures`Private`validDynkinQ;
opDottedQ6           = ConformalStructures`Private`opDottedQ6;
opPartition6         = ConformalStructures`Private`opPartition6;
opBoxes6             = ConformalStructures`Private`opBoxes6;
opIndexHead6         = ConformalStructures`Private`opIndexHead6;
youngSymmetrizerTerms = ConformalStructures`Private`youngSymmetrizerTerms;
su4IrrepDimension    = ConformalStructures`Private`su4IrrepDimension;

(* Convenience wrappers keyed on the rep alone. *)
dottedQ6[rep_]   := opDottedQ6[rep];
partition6[rep_] := opPartition6[rep, opDottedQ6[rep]];
boxes6[rep_]     := opBoxes6[rep, opDottedQ6[rep]];
indexHead6[rep_] := opIndexHead6[opDottedQ6[rep]];

(* Does the algebra have a second, opposite-chirality supercharge Qbar?

   d = 4 (0 mod 4): Q and Qbar are Weyl spinors of opposite chirality -- yes.
   d = 6 (2 mod 4): the supercharge is chiral.  6D (N, 0) supersymmetry has a
      single Weyl supercharge Q_A^i obeying a symplectic-Majorana condition; there
      is no independent Qbar, and sigma^mu_{AB} is {Weyl, Weyl} rather than
      {Weyl, DottedWeyl}, so {Q, Q} ~ P closes on Q alone.
   d = 2: also 2 mod 4, and SCWIGEE's 2D support is holomorphic-only by design
      (see SetSymmetries), so likewise no Qbar.
   d odd: the supercharge is Dirac; no Qbar. *)
hasQBar[] := Mod[SpacetimeDimension[], 4] == 0;

(* Is there a U(1) R-charge to grade multiplets by?  The 6D R-symmetries --
   SU(2) for (1,0), USp(4) for (2,0) -- have no U(1) factor, so the fifth
   Operator slot is identically 0 and multiplets are graded by dimension alone. *)
hasU1RCharge[] := SpacetimeDimension[] =!= 6;

(* The spinor indices carried by an operator, as a list of Lowered[...] heads. *)
spinorIndicesOf[f_Operator] := With[{spin = Spin[f], dim = SpacetimeDimension[]},
   Which[
      dim == 6,
         Table[Lowered[indexHead6[spin][6]], boxes6[spin]],
      ListQ[spin],
         Join[
            Table[Lowered[WeylSpinor[dim]], 2 spin[[1]]],
            Table[Lowered[DottedWeylSpinor[dim]], 2 spin[[2]]]
         ],
      True,
         Table[Lowered[DiracSpinor[dim]], 2 spin]
   ]
];

(* ------------------------------------------------------------------------- *)
(* Index symmetry of a 6D operator.                                           *)
(*                                                                            *)
(* The Young symmetrizer is (row symmetrizer).(column antisymmetrizer), so the *)
(* row symmetrizer acts last and the projected tensor is exactly symmetric     *)
(* within each row of its tableau.  That much is a signed permutation symmetry *)
(* and can be declared to TensorTools; the column antisymmetry and the Garnir  *)
(* relations are not, and are carried by youngProjectTensor6 below instead.    *)
(*                                                                            *)
(* Index 1 of an operator tensor is its R-symmetry index, so the spinor block  *)
(* starts at 2.                                                               *)
(* ------------------------------------------------------------------------- *)
rowSymmetryGenerators6[rep_] := With[{rows = partition6[rep]},
   If[rows === {}, {},   (* a scalar carries no spinor indices *)
      Join @@ MapThread[
         Function[{start, len}, Table[{Cycles[{{i, i + 1}}], 1}, {i, start, start + len - 2}]],
         {Prepend[Accumulate[Most[rows]], 0] + 2, rows}
      ]
   ]
];

(* Young-project the index block [first, first + n - 1] of an array onto the
   diagram of rep, using ConformalStructures' symmetrizer term list. *)
youngProjectTensor6[arr_, rep_, first_ : 1] := With[{lambda = partition6[rep], r = ArrayDepth[arr]},
   If[Total[lambda] <= 1, arr,
      Total[Function[term,
            term[[2]] TensorTranspose[arr,
               Join[Range[first - 1], first - 1 + term[[1]], Range[first + Total[lambda], r]]]
         ] /@ youngSymmetrizerTerms[lambda]]
   ]
];

(* ------------------------------------------------------------------------- *)
(* SU(4) invariant tensors.                                                   *)
(*                                                                            *)
(* In d = 6 the Weyl charge conjugation matrix vanishes identically: the 4 of  *)
(* SU(4) is complex, so unlike d = 2, 3, 4 there is no two-index epsilon with  *)
(* which to raise or lower a single spinor index.  The invariants are instead  *)
(* the four-index Levi-Civita eps_{ABCD} / eps^{ABCD} (which changes the index *)
(* count by four) and the 4-4bar pairing delta_{A Bdot}.  Both are introduced  *)
(* here; SetSpacetimeDimension declares their symmetry.                        *)
(* ------------------------------------------------------------------------- *)
SU4Epsilon[opt : OptionsPattern[]] := Tensor[{Prepend[
   Table[If[TrueQ[OptionValue[SU4Epsilon, "Raised"]], Raised, Lowered][
      If[TrueQ[OptionValue[SU4Epsilon, "Dotted"]], DottedWeylSpinor[6], WeylSpinor[6]]], 4],
   su4eps[]]}];
Options[SU4Epsilon] = {"Raised" -> False, "Dotted" -> False};

(* K_{A Bdot}: the singlet in 4 (x) 4bar -- the only invariant pairing an undotted
   with a dotted index, and the only bridge between the two chiralities.

   It is NOT the identity matrix.  Solving the invariance condition against
   RotationGenerators[6, "Weyl" -> True] and its dotted counterpart fixes it
   (uniquely up to scale) to the antisymmetric matrix below, which satisfies
   K.K = -1 and K.Transpose[K] = 1; the raised pairing K^{A Bdot} solves its own
   invariance condition with the same components, so that K^{A Cdot} K_{B Cdot} =
   delta^A_B.  See Tests/SixDTests.wls, which re-derives both from the generators. *)
SU4Pairing[opt : OptionsPattern[]] := Tensor[{{su4delta[],
   If[TrueQ[OptionValue[SU4Pairing, "Raised"]], Raised, Lowered][WeylSpinor[6]],
   If[TrueQ[OptionValue[SU4Pairing, "Raised"]], Raised, Lowered][DottedWeylSpinor[6]]}}];
Options[SU4Pairing] = {"Raised" -> False};

su4PairingMatrix = {{0, 0, 0, 1}, {0, 0, -1, 0}, {0, 1, 0, 0}, {-1, 0, 0, 0}};

BuildTensor[{su4eps[], Repeated[_Lowered | _Raised, {4}]}] :=
   SparseArray[Normal@LeviCivitaTensor[4]];
BuildTensor[{su4delta[], _, _}] := SparseArray[su4PairingMatrix];

(* su4delta pairs two different index types, so it has no permutation symmetry. *)
declareSixDSymmetries[] := DeclareTensorSymmetry[su4eps[], {Antisymmetric[Range[4]]}];
