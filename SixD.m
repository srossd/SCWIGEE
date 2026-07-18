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

(* The EXACT permutation symmetry of a Young-projected index block, read off the projector
   rather than assumed.  Row symmetry always holds, but it is not always the whole story: a
   single-column shape (the vector [0,1,0], say) has no row symmetry at all yet is totally
   antisymmetric, because its row symmetrizer is trivial and only the column
   antisymmetrizer acts.  So each transposition of the block is tested against the
   projector and kept when it acts with a definite sign.  Index 1 of an operator tensor is
   its R-symmetry index, hence the offset by one. *)
youngSymmetryGenerators6[rep_] := youngSymmetryGenerators6[rep] =
   Module[{lambda = partition6[rep], n, arr, swapped},
      n = Total[lambda];
      If[n <= 1, Return[{}]];
      arr = youngProjectTensor6[Array[$y, ConstantArray[4, n]], rep];
      Flatten[Table[
         swapped = TensorTranspose[arr, PermutationList[Cycles[{{i, j}}], n]];
         Which[
            Flatten[Expand[swapped - arr]] === ConstantArray[0, 4^n], {{Cycles[{{i + 1, j + 1}}], 1}},
            Flatten[Expand[swapped + arr]] === ConstantArray[0, 4^n], {{Cycles[{{i + 1, j + 1}}], -1}},
            True, {}
         ],
         {i, n - 1}, {j, i + 1, n}], 2]
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

(* ------------------------------------------------------------------------- *)
(* d = 6 SUSY ansatz structures.                                             *)
(*                                                                            *)
(* In d = 2, 3, 4 an ansatz structure is built by CProducts, which matches an  *)
(* operator's spinor index count to a target by tensoring with, or contracting *)
(* against, the two-index charge conjugation matrix.  That matrix vanishes in  *)
(* d = 6, so the whole mechanism has to be rebuilt on the invariants that do   *)
(* exist: eps_{ABCD}, which changes one chirality's index count by FOUR, and   *)
(* the pairing K, which adds or removes one index of EACH chirality.  A single *)
(* index can therefore never be raised or lowered on its own, and the shifts   *)
(* that are reachable, (+-4, 0), (0, +-4) and (+-1, +-1), preserve the SU(4)   *)
(* N-ality of the index block -- exactly as representation theory demands.     *)
(* ------------------------------------------------------------------------- *)
lowW6 := Lowered[WeylSpinor[6]];
lowD6 := Lowered[DottedWeylSpinor[6]];
raiW6 := Raised[WeylSpinor[6]];
raiD6 := Raised[DottedWeylSpinor[6]];

(* Which multisets of invariants carry an index block from (mW, mD) to (nW, nD)?  The
   counts are bounded: physical multiplets never need more than one epsilon or a couple of
   pairings, and an unbounded search would blow up the ansatz with redundant structures. *)
adjustSchemes6[mW_, mD_, nW_, nD_] := Select[
   Flatten[Table[{ea, ec, da, dc, ka, kc},
      {ea, 0, 1}, {ec, 0, 1}, {da, 0, 1}, {dc, 0, 1}, {ka, 0, 2}, {kc, 0, 2}], 5],
   Function[s,
      With[{ea = s[[1]], ec = s[[2]], da = s[[3]], dc = s[[4]], ka = s[[5]], kc = s[[6]]},
         nW == mW + 4 ea - 4 ec + ka - kc && nD == mD + 4 da - 4 dc + ka - kc &&
            4 ec + kc <= mW && 4 dc + kc <= mD
      ]
   ]
];

schemeFactors6[{ea_, ec_, da_, dc_, ka_, kc_}] := Join[
   Table[SU4Epsilon[], ea],
   Table[SU4Epsilon["Raised" -> True], ec],
   Table[SU4Epsilon["Dotted" -> True], da],
   Table[SU4Epsilon["Raised" -> True, "Dotted" -> True], dc],
   Table[SU4Pairing[], ka],
   Table[SU4Pairing["Raised" -> True], kc]
];

(* One "family" is a base tensor, a scheme, and a choice of which of base's indices are
   contracted against the invariants.  It is returned as an Association from the ASSIGNMENT
   -- which free index of the contracted product lands in which target slot -- to the
   resulting tensor, with every assignment enumerated.

   Keying by assignment is what lets the Young projection below be taken without ever
   nesting one TensorPermute inside another.  That matters: TensorTools composes
   TensorPermute[TensorPermute[x, q], p] to TensorPermute[x, q[[p]]], whereas its own
   single-level semantics (Components[TensorPermute[t, p]] === Transpose[Components[t], p])
   require p[[q]].  The two agree only when a permutation is an involution, so a nested
   permute silently produces the wrong tensor for, say, a 4-cycle.  Everything here
   therefore applies at most ONE TensorPermute, to an unpermuted base. *)
structureFamily6[base_, factors_, wSel_, dSel_, targetInds_] := Module[
   {prod, inds, nb, raiWf, raiDf, pairs, c, cinds, rpos, spos, asgs},
   prod = If[factors === {}, base, TensorProduct[base, Sequence @@ factors]];
   inds = Indices[prod];
   nb = Length[Indices[base]];
   raiWf = Select[Range[nb + 1, Length[inds]], inds[[#]] === raiW6 &];
   raiDf = Select[Range[nb + 1, Length[inds]], inds[[#]] === raiD6 &];
   pairs = Join[Transpose[{wSel, raiWf}], Transpose[{dSel, raiDf}]];
   c = If[pairs === {}, prod, Contract[prod, pairs]];
   cinds = Indices[c];
   (* every invariant's raised index is contracted, so the only raised index left is the
      operator's R-symmetry index.  It is left where it sits -- possibleQActions6 locates it
      by position and contracts it away -- and only the spinor slots are reordered. *)
   rpos = Position[cinds, _Raised][[1, 1]];
   spos = Complement[Range[Length[cinds]], {rpos}];
   If[Sort[cinds[[spos]]] =!= Sort[targetInds], Return[{}]];
   asgs = Select[Permutations[spos], cinds[[#]] === targetInds &];
   Table[
      {asg, TensorPermute[c,
         InversePermutation[ReplacePart[Range[Length[cinds]], Thread[spos -> asg]]]]},
      {asg, asgs}]
];

(* the tensor in a family whose free spinor indices sit in the order given by asg *)
familyLookup6[family_, asg_] := SelectFirst[family, First[#] === asg &][[2]];
familyAssignments6[family_] := First /@ family;

(* Every family of structures built from base whose free spinor indices are targetInds. *)
structureFamilies6[base_, targetInds_] := Module[{binds, lowWb, lowDb, mW, mD, nW, nD},
   binds = Indices[base];
   lowWb = Select[Range[Length[binds]], binds[[#]] === lowW6 &];
   lowDb = Select[Range[Length[binds]], binds[[#]] === lowD6 &];
   mW = Length[lowWb]; mD = Length[lowDb];
   nW = Count[targetInds, lowW6]; nD = Count[targetInds, lowD6];
   DeleteCases[Flatten[Table[
      With[{factors = schemeFactors6[scheme],
            nrw = 4 scheme[[2]] + scheme[[6]], nrd = 4 scheme[[4]] + scheme[[6]]},
         Table[structureFamily6[base, factors, wSel, dSel, targetInds],
            {wSel, Permutations[lowWb, {nrw}]}, {dSel, Permutations[lowDb, {nrd}]}]
      ],
      {scheme, adjustSchemes6[mW, mD, nW, nD]}
   ], 2], {}]
];

(* The d = 6 counterpart of OperatorsWithQuantumNumbers: instead of a target spin (which in
   d = 6 Q's action does not determine -- rep (x) [1,0,0] is a SUM of reps) it takes the
   target index content directly.  Operators at the target dimension contribute directly;
   operators one unit below contribute through a derivative, which carries two undotted
   Weyl indices.  There is no U(1) R-charge to select on. *)
operatorFamilies6[multiplet_, rep_, dim_, targetInds_] := Join[
   Flatten[structureFamilies6[ToTensor[#], targetInds] & /@
      Select[multiplet, dynkin[rep] == dynkin[GlobalRep[#]] && ScalingDimension[#] == dim &], 1],
   Flatten[structureFamilies6[Tensor[{{"\[PartialD]", lowW6, lowW6}, ToTensor[#][[1, 1]]}], targetInds] & /@
      Select[multiplet, dynkin[rep] == dynkin[GlobalRep[#]] && ScalingDimension[#] == dim - 1 &], 1]
];

(* The Young symmetrizer of shape lambda, as permutations of the n target spinor slots.
   Slot 1 is Q's own index and is left alone; O's block is slots 2 .. n. *)
youngSlotTerms6[lambda_, n_] := If[Total[lambda] <= 1,
   {{Range[n], 1}},
   {Join[{1}, 1 + #[[1]]], #[[2]]} & /@ youngSymmetrizerTerms[lambda]
];

(* Components[] does not evaluate on a sum of tensors, and a Young-projected structure is
   precisely such a sum, so the components are assembled term by term. *)
structureComponents6[a_Plus] := Total[structureComponents6 /@ (List @@ a)];
structureComponents6[c_ t_] /; FreeQ[c, Alternatives @@ TensorTools`Private`$TensorHeads] :=
   c structureComponents6[t];
structureComponents6[0] := 0;
structureComponents6[t_] := Normal[Components[t]];

(* Independence of structures is decided on components -- a 6D operator's components are
   Young-projected (see sixDOperatorComponents), so two structures that agree only by virtue
   of a column antisymmetry or a Garnir relation are correctly seen as dependent, which a
   purely symbolic comparison would miss.

   The components are polynomials in the abstract symbols Component[name][...], and ranking
   symbolic matrices is far too slow once an operator carries four indices.  Each symbol is
   therefore specialised to a large exact random integer, deterministically seeded from the
   symbol itself, so the ranks are exact integer computations.  A specialisation can only
   ever LOSE rank, and does so with negligible probability at this size; ConformalStructures
   reduces its own 6D structure bases at a single random frame for the same reason. *)
numComponent[c_] := numComponent[c] = BlockRandom[SeedRandom[Hash[c]]; RandomInteger[{-10^7, 10^7}]];
numericVector6[arr_] := Flatten[arr] /. (x : Component[_][___]) :> numComponent[x];

(* greedy maximal independent subset of structs, given a numeric vector for each *)
independentByVectors6[structs_, vecs_] := Module[{kept = {}, mat = {}, r = 0},
   Do[
      If[! AllTrue[vecs[[i]], # === 0 &] && MatrixRank[Append[mat, vecs[[i]]]] > r,
         mat = Append[mat, vecs[[i]]]; r = r + 1; kept = Append[kept, structs[[i]]]
      ],
      {i, Length[structs]}];
   kept
];

independentStructures6[structs_] :=
   independentByVectors6[structs, numericVector6[structureComponents6[#]] & /@ structs];

(* A 6D operator's spinor block is Young-projected, not merely symmetrized: for a mixed
   shape the declared permutation symmetry is only part of the story ([0,2,0] has 21
   components under its row/column symmetry but 20 in the irrep).  Its components are
   therefore built by applying the Young symmetrizer to a generic array. *)
sixDOperatorQ[name_String] := SpacetimeDimension[] === 6 &&
   $multipletIndices =!= {} &&
   MemberQ[Flatten[Multiplet /@ $multipletIndices][[;; , 1]], name];

sixDOperatorComponents[name_String, idxs_List] := With[
   {dims = Table[IndexData[idxs[[i, 1]]][[1]], {i, Length[idxs]}]},
   SparseArray[youngProjectTensor6[Array[Component[name], dims], Spin[name2field[name]], 2]]
];
