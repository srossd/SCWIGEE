(* Wolfram Language package *)


AddExplicitRule[u[{ii_, jj_, kk_, ll_}] :> (XXSquared[ii, jj] XXSquared[kk, ll])/(
   XXSquared[ii, kk] XXSquared[jj, ll])];
AddExplicitRule[v[{ii_, jj_, kk_, ll_}] :> (XXSquared[ii, ll] XXSquared[jj, kk])/(
   XXSquared[ii, kk] XXSquared[jj, ll])];
   
AddExplicitRule[u[{ii_, jj_}] :> XXSquared[ii, jj]/(4 Sqrt[XXSquaredTransverse[ii] XXSquaredTransverse[jj]])]; 
AddExplicitRule[v[{ii_, jj_}] :> XXDotTransverse[ii, jj]/(Sqrt[XXSquaredTransverse[ii] XXSquaredTransverse[jj]])]; 

crossRatios[None] = {u, v};
crossRatios[_Integer] = {u, v};
        
safeCrossRatios[_Integer] := safeCrossRatios[_Integer] = Module[{qs, vs},
	qs = Select[Rest@Most@FareySequence[70], FreeQ[Sqrt[2 # (Sqrt[1 + #^2] + #) + 1], Power[_, 1/2] | Power[_, -1/2]] &];
	vs = Select[Rest@Most@FareySequence[70], FreeQ[Sqrt[#^2 - 1], Power[_, 1/2] | Power[_, -1/2]] &];
	Rest@Flatten[Table[{(Sqrt[1 + q^2] - v)/2, v}, {q, qs}, {v, vs}], 1]
];
   
safeCrossRatios[None] = {{1, 36/25}, {1, 100/169}, {1, 256/289}, {1, 1600/841}, {1, 
    3136/2809}, {1, 4356/4225}, {1, 5184/7225}, {1, 6084/7921}, {1, 
    9216/5329}, {1, 14400/11881}, {9/16, 25/16}, {9/16, 1369/
    2704}, {16/9, 1369/1521}, {16/9, 8281/12321}, {25/16, 9/16}, {25/
    36, 25/36}, {36/25, 1}, {64/81, 7225/13689}, {81/64, 7225/
    10816}, {100/169, 1}, {169/100, 169/100}, {169/196, 225/
    196}, {169/225, 196/225}, {196/169, 225/169}, {196/225, 169/
    225}, {196/361, 10201/9025}, {225/169, 196/169}, {225/196, 169/
    196}, {256/289, 1}, {289/256, 289/256}, {289/441, 5776/
    11025}, {361/625, 4356/7225}, {400/441, 841/441}, {441/289, 5776/
    7225}, {625/361, 4624/9025}, {625/841, 1296/841}, {625/1156, 1521/
    1156}, {841/441, 400/441}, {841/1225, 2304/1225}, {841/1600, 841/
    1600}, {1296/841, 625/841}, {1369/1521, 16/9}, {1369/2704, 9/
    16}, {1521/1156, 625/1156}, {1521/1369, 2704/1369}, {1521/1681, 
    2500/1681}, {1521/2500, 1681/2500}, {1600/841, 1}, {1681/1521, 
    2500/1521}, {1681/2500, 1521/2500}, {1681/2601, 3364/2601}, {2304/
    1225, 841/1225}, {2304/3025, 5329/3025}, {2500/1521, 1681/
    1521}, {2500/1681, 1521/1681}, {2500/4761, 5329/4761}, {2601/2704,
     2809/2704}, {2601/2809, 2704/2809}, {2704/1369, 1521/
    1369}, {2704/2601, 2809/2601}, {2704/2809, 2601/2809}, {2704/5329,
     5625/5329}, {2809/2601, 2704/2601}, {2809/2704, 2601/
    2704}, {2809/3136, 2809/3136}, {3136/2809, 1}, {3136/3721, 5625/
    3721}, {3136/5625, 3721/5625}, {3249/4225, 4624/4225}, {3249/4624,
     4225/4624}, {3364/2601, 1681/2601}, {3600/5329, 8281/
    5329}, {3721/3136, 5625/3136}, {3721/5476, 7569/5476}, {3721/5625,
     3136/5625}, {3721/6400, 11881/6400}, {4225/3249, 4624/
    3249}, {4225/4356, 4225/4356}, {4225/4624, 3249/4624}, {4225/5184,
     9409/5184}, {4225/5776, 7569/5776}, {4225/7569, 5776/
    7569}, {4225/7569, 7744/7569}, {4225/7744, 7569/7744}, {4356/4225,
     1}, {4356/7225, 361/625}, {4489/7225, 13456/7225}, {4624/3249, 
    4225/3249}, {4624/4225, 3249/4225}, {4624/5625, 5929/5625}, {4624/
    5929, 5625/5929}, {4624/7569, 9025/7569}, {4624/9025, 625/
    361}, {4624/9025, 7569/9025}, {5184/7225, 1}, {5329/3025, 2304/
    3025}, {5329/4761, 2500/4761}, {5329/9216, 5329/9216}, {5625/3136,
     3721/3136}, {5625/3721, 3136/3721}, {5625/4624, 5929/
    4624}, {5625/5329, 2704/5329}, {5625/5929, 4624/5929}, {5625/7396,
     9409/7396}, {5625/9409, 7396/9409}, {5776/4225, 7569/
    4225}, {5776/7225, 441/289}, {5776/7569, 4225/7569}, {5776/11025, 
    289/441}, {5929/4624, 5625/4624}, {5929/5625, 4624/5625}, {6084/
    7921, 1}, {6084/9025, 9409/9025}, {6084/9409, 9025/9409}, {6084/
    10201, 13225/10201}, {7225/5184, 7225/5184}, {7225/5776, 11025/
    5776}, {7225/8649, 13456/8649}, {7225/10816, 81/64}, {7225/13456, 
    8649/13456}, {7225/13689, 64/81}, {7396/5625, 9409/5625}, {7396/
    9409, 5625/9409}, {7569/4225, 5776/4225}, {7569/4225, 7744/
    4225}, {7569/4624, 9025/4624}, {7569/5476, 3721/5476}, {7569/5776,
     4225/5776}, {7569/7744, 4225/7744}, {7569/9025, 4624/
    9025}, {7744/4225, 7569/4225}, {7744/7569, 4225/7569}, {7921/6084,
     7921/6084}, {7921/9801, 10000/9801}, {7921/10000, 9801/
    10000}, {8100/9409, 14161/9409}, {8100/14161, 9409/14161}, {8281/
    5329, 3600/5329}, {8281/12321, 16/9}, {8281/13225, 13456/
    13225}, {8281/13456, 13225/13456}, {8464/12321, 14161/
    12321}, {8464/14161, 12321/14161}, {8649/7225, 13456/7225}, {8649/
    13456, 7225/13456}, {9025/4624, 7569/4624}, {9025/6084, 9409/
    6084}, {9025/7569, 4624/7569}, {9025/9409, 6084/9409}, {9216/5329,
     1}, {9409/5184, 4225/5184}, {9409/5625, 7396/5625}, {9409/6084, 
    9025/6084}, {9409/7396, 5625/7396}, {9409/8100, 14161/
    8100}, {9409/9025, 6084/9025}, {9409/14161, 8100/14161}, {9801/
    7921, 10000/7921}, {9801/10000, 7921/10000}, {10000/7921, 9801/
    7921}, {10000/9801, 7921/9801}, {10201/9025, 196/361}, {10816/
    7225, 13689/7225}, {11025/5776, 7225/5776}, {11236/12769, 14161/
    12769}, {11236/14161, 12769/14161}, {11881/6400, 3721/
    6400}, {11881/14400, 11881/14400}, {12321/8464, 14161/
    8464}, {12321/14161, 8464/14161}, {12769/11236, 14161/
    11236}, {12769/14161, 11236/14161}, {13225/8281, 13456/
    8281}, {13225/10201, 6084/10201}, {13225/13456, 8281/
    13456}, {13456/7225, 4489/7225}, {13456/7225, 8649/7225}, {13456/
    8281, 13225/8281}, {13456/8649, 7225/8649}, {13456/13225, 8281/
    13225}, {13689/7225, 10816/7225}, {14161/8100, 9409/8100}, {14161/
    8464, 12321/8464}, {14161/9409, 8100/9409}, {14161/11236, 12769/
    11236}, {14161/12321, 8464/12321}, {14161/12769, 11236/
    12769}, {14400/11881, 1}};

uvpt = {x[1, i_] :> -Boole[i == 4], x[2, _] :> 0, 
   x[3, i_] :> Boole[i == 4], 
   x[4, i_] :> 
    Which[i == 3, -(
      Sqrt[-u^2 - (-1 + v)^2 + 2 u (1 + v)]/(-1 + 2 u + 2 v)), 
     i == 4, (-u + v)/(-1 + 2 u + 2 v), True, 0]};

uvptDefect = {x[1, 1] -> 1, x[1, 2] -> 0, x[1, 3] -> 0, x[1, 4] -> 0, 
 x[2, 1] -> v (2 u + v + Sqrt[-1 + 4 u^2 + 4 u v + v^2]), 
 x[2, 2] -> -Sqrt[-((-1 + v^2) (-1 + 8 u^2 + 2 v^2 + 
        2 v Sqrt[-1 + 4 u^2 + 4 u v + v^2] + 
        4 u (2 v + Sqrt[-1 + 4 u^2 + 4 u v + v^2])))], x[2, 3] -> 0, 
 x[2, 4] -> 0};

sct[x_, b_] := (x - b x . Components[\[Eta]Lower] . x)/(1 - 2 (b . Components[\[Eta]Lower] . x) + (b . Components[\[Eta]Lower] . b) (x . Components[\[Eta]Lower] . x));

sctuvpt[z_] := Simplify@Flatten@Table[x[i, j] -> sct[Table[x[i, kk] /. uvpt, {kk, 4}], {0, z, 0, 0}][[j]], {i, 4}, {j, 4}];

sctuvptDefect[z_] := Simplify@Flatten@Table[x[i, j] -> sct[Table[x[i, kk] /. uvptDefect, {kk, 4}], {0, 0, 0, z}][[j]], {i, 2}, {j, 4}];
      
genericPoint[q_, z_] := If[q === None, sctuvpt[z], sctuvptDefect[z]];

crossRatioAssumptions[q_] := If[q === None, And @@ (1/2 < # < 2 & /@ crossRatios[q]), And @@ (0 < # & /@ crossRatios[q])];

SymbolicSpacetimeRelations[largebasis_] := With[{q = First@Cases[largebasis, SpacetimeStructure[___, q_, _] :> q, All]},
   If[# === {}, {}, 
   FullSimplify[
    RowReduce[#, 
     ZeroTest -> (Function[expr, Simplify[expr, crossRatioAssumptions[q]] === 0])], 
    crossRatioAssumptions[q]]] &@
 FullSimplify[NullSpace[Flatten[Table[Transpose@ArrayFlatten[Flatten@*List@*CanonicallyOrderedComponents /@ largebasis] /. genericPoint[q, z], {z, 2, 5}], 1]], crossRatioAssumptions[q]]
];

SpacetimeRelations[structs_] := 
  If[First@Cases[structs, s_SpacetimeStructure :> s[[-2]], All] =!= None || (Length[structs] > 5 && First@Cases[structs, s_SpacetimeStructure :> Length[s[[1]]], All] == 4), 
   relations[structs, If[First@Cases[structs, s_SpacetimeStructure :> s[[-2]], All] =!= None, 100, 188], 6], SymbolicSpacetimeRelations[structs]];
   
relations[structs_, len_, deg_] := With[{q = First@Cases[structs, SpacetimeStructure[___, q_, _] :> q, All]},
 With[{data = spacetimePtData[structs, len]},
  With[{ind = data[[1]], 
    other = Complement[Range@Length[structs], data[[1]]]},
   ResourceFunction["MonitorProgress"][
    Table[SparseArray[
      Append[Table[{ind[[i]]} -> (-Simplify@
            fitRational[
             data[[2]] /. {{u_, v_}, b_} :> {u, v, b[[j, i]]}, 1, deg,
              "Prefactors" -> Which[q === None, {1, Sqrt[u], Sqrt[v], Sqrt[u v]}, q === 2, {1, Sqrt[1-v^2]}, True, {1}]]), {i, 
         Length[ind]}], {other[[j]]} -> 1], {Length[structs]}], {j, 
      Length[other]}], "Label" -> "Fitting rational functions",
		"CurrentDisplayFunction" -> None]
   ]
  ]
];
    
spacetimePtData[structs_, len_] := 
 spacetimePtData[structs, len] = With[{q = First@Cases[structs, SpacetimeStructure[___, q_, _] :> q, All]},
  With[{structComps = Flatten[Table[Transpose@ArrayFlatten[Flatten@*List@*CanonicallyOrderedComponents /@ structs] /. genericPoint[q, z], {z, 2, 5}], 1]}, 
   With[{idxs = Sort[Length[structs] + 1 - IndependentSet[Reverse@Transpose@structComps, "Rules" -> Thread[crossRatios[q] -> safeCrossRatios[q][[37]]], "Indices" -> True]]},
   {idxs, 
    ResourceFunction["MonitorProgress"][
     Table[Simplify@
       Quiet@Check[{safeCrossRatios[q][[ii]], 
          Table[LinearSolve[
            structComps[[;; , idxs]] /. Thread[crossRatios[q] -> safeCrossRatios[q][[ii]]], 
            structComps[[;; , reli]] /. Thread[crossRatios[q] -> safeCrossRatios[q][[ii]]]], {reli, 
            Complement[Range@Length[structs], idxs]}]}, Nothing], {ii, len}], 
     "Label" -> "Generating spacetime structure relation data", 
     "CurrentDisplayFunction" -> None]}
    ]
  ]
]

KinematicPrefactor[{\[CapitalDelta]1_, \[CapitalDelta]2_}, {{l1_, lb1_}, {l2_, lb2_}}, q_ : None] := If[q === None,
   1/XXSquared[1, 2]^(\[CapitalDelta]1 + l1 + lb1),
   1/(XXSquaredTransverse[1]^((\[CapitalDelta]1 + l1 + lb1)/2) XXSquaredTransverse[2]^((\[CapitalDelta]2 + l2 + lb2)/2))
];
KinematicPrefactor[deltas_, spins_, None] /; Length[deltas] == 3 := With[{kappas = deltas + Total /@ spins},
   XXSquared[1,2]^((kappas[[3]] - kappas[[1]] - kappas[[2]])/2) XXSquared[1,3]^((-kappas[[3]] - kappas[[1]] + kappas[[2]])/2) XXSquared[2,3]^((-kappas[[3]] + kappas[[1]] - kappas[[2]])/2)
];
KinematicPrefactor[deltas_, spins_, None] /; Length[deltas] == 4 := With[{kappas = deltas + Total /@ spins},
   (XXSquared[2, 4]/XXSquared[1, 4])^((kappas[[1]] - kappas[[2]])/2) (XXSquared[1, 4]/XXSquared[1, 3])^((kappas[[3]] - kappas[[4]])/2) XXSquared[1, 2]^(-(kappas[[1]] + kappas[[2]])/2) XXSquared[3, 4]^(-(kappas[[3]] + kappas[[4]])/2)
];

maxPts[q_] := 0;
allStructures[n_, q_] := {};

structIndices[1] := {};
structIndices[TensorProduct[t1_, rest__]] := Join @@ (structIndices /@ {t1, rest});

Structure[label_] := Structure[label, None];

AddSpacetimeStructure[symbol_, q_, npts_, indices_, expr_] := Module[{},
   MyInactive /: MakeBoxes[MyInactive[Structure[symbol, q]][idxs__], TraditionalForm] /; Length[{idxs}] == npts :=
   	SubsuperscriptBox[ToBoxes@symbol, 
   	   	RowBox[Riffle[MakeBoxes /@ {idxs}[[Select[Range[npts], ! MemberQ[(indices @@ Range[npts])[[;;,1]], #] &]]], ","]],
   	   	RowBox[Riffle[MakeBoxes /@ {idxs}[[Select[Range[npts],   MemberQ[(indices @@ Range[npts])[[;;,1]], #] &]]], ","]]
   	];
   	
   Structure[symbol, q][idxs__] /; Length[{idxs}] == npts := expr[idxs];
   structIndices[MyInactive[Structure[symbol, q]][idxs__]] /; Length[{idxs}] == npts := indices[idxs];
   
   validStruct[MyInactive[Structure[symbol, q]][idxs__]] /; Length[{idxs}] == npts := (
   	Length[DeleteDuplicates[{idxs}]] == npts && 
   	OrderedQ[{idxs}[[Select[Range[npts], ! MemberQ[(indices @@ Range[npts])[[;;,1]], #] &]]]]
   );
   
   spins[MyInactive[Structure[symbol, q]][idxs__], n_] /; Length[{idxs}] == npts :=
   	SparseArray[Normal[Counts[indices[idxs]]/2] /. {Spinor -> 1, DottedSpinor -> 2}, {n, 2}];
   	
   	
   	AppendTo[allStructures[npts, q], symbol];
   	maxPts[q] = Max[maxPts[q], npts];
];

(* No defect *)
AddSpacetimeStructure["I", None, 2, {{#2, Spinor}, {#1, DottedSpinor}} &, SpinorX[#1, #2] &];
AddSpacetimeStructure["I", None, 4, {{#2, Spinor}, {#1, DottedSpinor}} &, 
	1/(2 XXSquared[#3, #4]) ((XXSquared[#1, #3] SpinorX[#2, #4] - 
        XXSquared[#1, #4] SpinorX[#2, #3]) + (XXSquared[#2, #3] SpinorX[#1, #4] - XXSquared[#2, #4] SpinorX[#1, #3]) - 
      XXSquared[#1, #2] SpinorX[#3, #4] - XXSquared[#3, #4] SpinorX[#1, #2] - 
      (2 I / SignatureFactor[]) SpinorX[{#1, #3}, {#4, #2}, {#4, #3}]) &]
AddSpacetimeStructure["J", None, 3, {{#1, Spinor}, {#1, DottedSpinor}} &,
	(XXSquared[#2, #1] XXSquared[#3, #1])/XXSquared[#2, #3] (SpinorX[#2, #1]/XXSquared[#2, #1] - SpinorX[#3, #1]/XXSquared[#3, #1]) &];
AddSpacetimeStructure["K", None, 3, {{#1, Spinor}, {#2, Spinor}} &,
	1/2 Sqrt[XXSquared[#1, #2]]/ Sqrt[XXSquared[#1, #3] XXSquared[#2, #3]] (
		(XXSquared[#1, #3] + XXSquared[#2, #3] - XXSquared[#1, #2]) \[Epsilon]Lower - 
     	4 Contract[TensorProduct[XX[#1, #3], XX[#2, #3], \[Sigma]CommLower], {{1, 3}, {2, 4}}]
    ) &];
AddSpacetimeStructure["\!\(\*OverscriptBox[\(K\), \(_\)]\)", None, 3, {{#1, DottedSpinor}, {#2, DottedSpinor}} &,
	1/2 Sqrt[XXSquared[#1, #2]]/ Sqrt[XXSquared[#1, #3] XXSquared[#2, #3]] (
		(XXSquared[#1, #3] + XXSquared[#2, #3] - XXSquared[#1, #2]) \[Epsilon]LowerDot - 
     	4 Contract[TensorProduct[XX[#1, #3], XX[#2, #3], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}]
    ) &];
AddSpacetimeStructure["L", None, 4, {{#1, Spinor}, {#1, Spinor}} &,
   2/Sqrt[XXSquared[#2, #3] XXSquared[#3, #4] XXSquared[#4, 
      #2]] (XXSquared[#1, #2] Contract[
       TensorProduct[XX[#3, #4], 
        XX[#1, #4], \[Sigma]CommLower], {{1, 3}, {2, 4}}] + 
     XXSquared[#1, #3] Contract[
       TensorProduct[XX[#4, #2], 
        XX[#1, #2], \[Sigma]CommLower], {{1, 3}, {2, 4}}] + 
     XXSquared[#1, #4] Contract[
       TensorProduct[XX[#2, #3], 
        XX[#1, #3], \[Sigma]CommLower], {{1, 3}, {2, 4}}]) &
];
AddSpacetimeStructure["\!\(\*OverscriptBox[\(L\), \(_\)]\)", None, 4, {{#1, DottedSpinor}, {#1, DottedSpinor}} &,
   2/Sqrt[XXSquared[#2, #3] XXSquared[#3, #4] XXSquared[#4, 
      #2]] (XXSquared[#1, #2] Contract[
       TensorProduct[XX[#3, #4], 
        XX[#1, #4], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}] + 
     XXSquared[#1, #3] Contract[
       TensorProduct[XX[#4, #2], 
        XX[#1, #2], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}] + 
     XXSquared[#1, #4] Contract[
       TensorProduct[XX[#2, #3], 
        XX[#1, #3], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}]) &
];

(* q = 1 *)
AddSpacetimeStructure["I", 1, 2, {{#1, Spinor}, {#2, DottedSpinor}} &, SpinorX[#2, #1] &];
AddSpacetimeStructure["J", 1, 2, {{#1, Spinor}, {#1, DottedSpinor}} &, 1/XXSquaredTransverse[#2]^(1/2) (2 x[#1,1] Contract[TensorProduct[XX[#1, #2], \[Sigma]Lower], {{1, 2}}] - XXSquared[#1, #2] \[Sigma]LowerSingle[1]) &];
AddSpacetimeStructure["K", 1, 2, {{#1, Spinor}, {#2, Spinor}} &, -1/2 (Contract[TensorProduct[XX[#1, #2], \[Sigma]LowerSingle[1], \[Sigma]BarLower, \[Epsilon]Lower], {{1, 4}, {3, 5}, {6, 7}}] 
   - Contract[TensorProduct[XX[#1, #2], \[Sigma]Lower, \[Sigma]BarLowerSingle[1], \[Epsilon]Lower], {{1, 2}, {4, 5}, {6, 7}}]) + (x[#1, 1] + x[#2, 1]) \[Epsilon]Lower &];
AddSpacetimeStructure["\!\(\*OverscriptBox[\(K\), \(_\)]\)", 1, 2, {{#1, DottedSpinor}, {#2, DottedSpinor}} &, -1/2  (Contract[TensorProduct[XX[#1, #2], \[Sigma]BarLowerSingle[1], \[Sigma]Lower, \[Epsilon]LowerDot], {{1, 4}, {3, 5}, {2, 7}}] 
   - Contract[TensorProduct[XX[#1, #2], \[Sigma]BarLower, \[Sigma]LowerSingle[1], \[Epsilon]LowerDot], {{1, 2}, {4, 5}, {3, 7}}]) -  (x[#1, 1] + x[#2, 1])  \[Epsilon]LowerDot &];

(* q = 2 *)
AddSpacetimeStructure["I", 2, 2, {{#1, Spinor}, {#2, DottedSpinor}} &, SpinorX[#2, #1] &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(I\), \(Y\)]\))", 2, 2, {{#1, Spinor}, {#2, DottedSpinor}} &, Contract[TensorProduct[XX[#1] + XX[#2], \[Epsilon]Transverse[2], \[Sigma]Upper], {{1,2}, {3, 4}}] + 
 Contract[TensorProduct[XX[#1, #2], \[Epsilon]Defect[2], \[Sigma]Upper], {{1, 2}, {3, 4}}] &];
AddSpacetimeStructure["J", 2, 2, {{#1, Spinor}, {#1, DottedSpinor}} &, 1/XXDotTransverse[#1, #2] (2 XXSquaredTransverse[#1] Contract[TensorProduct[XXDefect[#1, #2], \[Sigma]Lower], {{1, 2}}] - 
 Contract[TensorProduct[XXTransverse[#1], \[Sigma]Lower], {{1, 2}}] (XXSquaredDefect[#1, #2] - XXSquaredTransverse[#1] + XXSquaredTransverse[#2])) &]
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(J\), \(Y\)]\))", 2, 1, {{#1, Spinor}, {#1, DottedSpinor}} &, Contract[TensorProduct[XX[#1], \[Epsilon]Transverse[2], \[Sigma]Upper], {{1, 2}, {3, 4}}] &];
AddSpacetimeStructure["K", 2, 2, {{#1, Spinor}, {#2, Spinor}} &, 1/Sqrt[XXSquaredTransverse[#1]] (XXSquaredTransverse[#1] \[Epsilon]Lower +
 2 Contract[TensorProduct[XXTransverse[#1], XXDefect[#1, #2], \[Sigma]CommLower], {{1, 3}, {2, 4}}] + 
 Contract[TensorProduct[XX[#1], XX[#2], \[Epsilon]Transverse[2], \[Epsilon]Defect[2], \[Sigma]CommUpper], {{1, 3}, {2, 4}, {5, 7}, {6, 8}}] + 
 XXDotTransverse[#1, #2] \[Epsilon]Lower) &];
AddSpacetimeStructure["(\!\(\*OverscriptBox[\(K\), \(_\)]\))", 2, 2, {{#1, DottedSpinor}, {#2, DottedSpinor}} &, 1/Sqrt[XXSquaredTransverse[#1]] (XXSquaredTransverse[#1] \[Epsilon]LowerDot +
 2 Contract[TensorProduct[XXTransverse[#1], XXDefect[#1, #2], \[Sigma]CommLowerDot], {{1, 3}, {2, 4}}] - 
 Contract[TensorProduct[XX[#1], XX[#2], \[Epsilon]Transverse[2], \[Epsilon]Defect[2], \[Sigma]CommUpperDot], {{1, 3}, {2, 4}, {5, 7}, {6, 8}}] +
 XXDotTransverse[#1, #2] \[Epsilon]LowerDot) &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(K\), \(Y\)]\))", 2, 2, {{#1, Spinor}, {#2, Spinor}} &, 1/Sqrt[XXSquaredTransverse[#1]] (2 Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Epsilon]Defect[2], \[Eta]Upper, \[Sigma]CommLower], {{1, 7}, {2, 3}, {4, 5}, {6, 8}}] - 
  Contract[TensorProduct[XX[#1], XX[#2], \[Epsilon]Transverse[2], \[Epsilon]Lower], {{1, 3}, {2, 4}}] + 
  (XXSquaredTransverse[#1] - XXDotTransverse[#1, #2]) Contract[TensorProduct[\[Epsilon]Transverse[2], \[Sigma]CommUpper], {{1, 3}, {2, 4}}]) &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(K\), \(_\)], \(Y\)]\))", 2, 2, {{#1, DottedSpinor}, {#2, DottedSpinor}} &, 1/Sqrt[XXSquaredTransverse[#1]] (-2 Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Epsilon]Defect[2], \[Eta]Upper, \[Sigma]CommLowerDot], {{1, 7}, {2, 3}, {4, 5}, {6, 8}}] - 
  Contract[TensorProduct[XX[#1], XX[#2], \[Epsilon]Transverse[2], \[Epsilon]LowerDot], {{1, 3}, {2, 4}}] + 
  (XXSquaredTransverse[#1] - XXDotTransverse[#1, #2]) Contract[TensorProduct[\[Epsilon]Transverse[2], \[Sigma]CommUpperDot], {{1, 3}, {2, 4}}]) &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(L\), \(Y\)]\))", 2, 2, {{#1, Spinor}, {#1, Spinor}} &, 1/Sqrt[XXSquaredTransverse[#2]] (Contract[TensorProduct[\[Epsilon]Defect[2], \[Sigma]CommUpper], {{1, 3}, {2,4}}] (XXSquaredTransverse[#1] - XXSquaredTransverse[#2] - XXSquaredDefect[#1, #2]) - 
 4  Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Epsilon]Defect[2], \[Eta]Upper, \[Sigma]CommLower], {{1, 7}, {2, 3}, {4, 5}, {6, 8}}]) &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(L\), \(_\)], \(Y\)]\))", 2, 2, {{#1, DottedSpinor}, {#1, DottedSpinor}} &, 1/Sqrt[XXSquaredTransverse[#2]] (Contract[TensorProduct[\[Epsilon]Defect[2], \[Sigma]CommUpperDot], {{1, 3}, {2,4}}] (XXSquaredTransverse[#1] - XXSquaredTransverse[#2] - XXSquaredDefect[#1, #2]) - 
 4  Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Epsilon]Defect[2], \[Eta]Upper, \[Sigma]CommLowerDot], {{1, 7}, {2, 3}, {4, 5}, {6, 8}}]) &];


(* q = 3 *)
AddSpacetimeStructure["J", 3, 2, {{#1, Spinor}, {#1, DottedSpinor}} &,
   1/XXDotTransverse[#1, #2] ((2 XXSquaredTransverse[#1] - 2 XXDotTransverse[#1, #2] -  XXSquared[#1, #2]) SpinorXTransverse[#1] + 2 XXSquaredTransverse[#1] SpinorXDefect[#1, #2]) &]
AddSpacetimeStructure["\!\(\*OverscriptBox[\(J\), \(~\)]\)", 3, 2, {{#1, Spinor}, {#1, DottedSpinor}} &,
   2(SpinorXTransverse[#1] - XXSquaredTransverse[#1]/XXDotTransverse[#1, #2] SpinorXTransverse[#2]) &];
AddSpacetimeStructure["I", 3, 2, {{#1, Spinor}, {#2, DottedSpinor}} &, SpinorX[#2, #1] &];
AddSpacetimeStructure["\!\(\*OverscriptBox[\(I\), \(~\)]\)", 3, 2, {{#1, Spinor}, {#2, DottedSpinor}} &, 
	1/XXDotTransverse[#1, #2] (XXSquaredTransverse[#1] SpinorXTransverse[#2] - XXSquaredTransverse[#2] SpinorXTransverse[#1] + XXDotTransverse[#1, #2] SpinorXDefect[#1, #2]
		+ Contract[TensorProduct[XXTransverse[1], XXTransverse[2], XXDefect[1, 2], \[Epsilon]Spacetime, \[Sigma]Upper], {{1, 4}, {2, 5}, {3, 6}, {7, 8}}]) &];
AddSpacetimeStructure["K", 3, 2, {{#1, Spinor}, {#2, Spinor}} &,
   1/Sqrt[XXSquaredTransverse[#1]] (2 Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Sigma]CommLower], {{1,3},{2,4}}] + (XXDotTransverse[#1, #1] + XXDotTransverse[#1, #2])\[Epsilon]Lower) &];
AddSpacetimeStructure["\!\(\*OverscriptBox[\(K\), \(_\)]\)", 3, 2, {{#1, DottedSpinor}, {#2, DottedSpinor}} &,
   1/Sqrt[XXSquaredTransverse[#1]] (2 Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Sigma]CommLowerDot], {{1,3},{2,4}}] + (XXDotTransverse[#1, #1] + XXDotTransverse[#1, #2])\[Epsilon]LowerDot) &];
AddSpacetimeStructure["L", 3, 2, {{#1, Spinor}, {#1, Spinor}} &,
   2/(Sqrt[XXSquaredTransverse[#2]] XXDotTransverse[#1, #2]) (2 XXSquared[#1, #2] Contract[TensorProduct[XXTransverse[#2], XXTransverse[#1], \[Sigma]CommLower], {{1,3},{2,4}}]
   	- 4 XXSquaredTransverse[#1] Contract[TensorProduct[XXTransverse[#2], XX[#1, #2], \[Sigma]CommLower], {{1,3},{2,4}}]
   	+ 4 XXDotTransverse[#1, #2] Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Sigma]CommLower], {{1,3},{2,4}}]) &]
AddSpacetimeStructure["\!\(\*OverscriptBox[\(L\), \(_\)]\)", 3, 2, {{#1, DottedSpinor}, {#1, DottedSpinor}} &,
   2/(Sqrt[XXSquaredTransverse[#2]] XXDotTransverse[#1, #2]) (2 XXSquared[#1, #2] Contract[TensorProduct[XXTransverse[#2], XXTransverse[#1], \[Sigma]CommLowerDot], {{1,3},{2,4}}]
   	- 4 XXSquaredTransverse[#1] Contract[TensorProduct[XXTransverse[#2], XX[#1, #2], \[Sigma]CommLowerDot], {{1,3},{2,4}}]
   	+ 4 XXDotTransverse[#1, #2] Contract[TensorProduct[XXTransverse[#1], XX[#1, #2], \[Sigma]CommLowerDot], {{1,3},{2,4}}]) &]
   
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(I\), \(Y\)]\))", 3, 2, {{#1, Spinor}, {#2, DottedSpinor}} &,
   ((1/Sqrt[XXSquaredTransverse[#1]])(Contract[TensorProduct[XX[#1], XX[#2], \[Epsilon]Transverse[3], \[Sigma]Upper], {{1, 3}, {2, 4}, {5,6}}]
   + (x[#2, 4] - x[#1, 4]) SpinorXTransverse[#1]
   + (XXSquaredTransverse[#1] - XXDotTransverse[#1, #2]) \[Sigma]LowerSingle[4]))&]
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(I\), \(~\)], \(Y\)]\))", 3, 2, {{#1, Spinor}, {#2, DottedSpinor}} &,
   ((1/Sqrt[XXSquaredTransverse[#2]])(Contract[TensorProduct[XX[#1], XX[#2], \[Epsilon]Transverse[3], \[Sigma]Upper], {{1, 3}, {2, 4}, {5,6}}]
   - (x[#2, 4] - x[#1, 4]) SpinorXTransverse[#2]
   + (XXSquaredTransverse[#2] - XXDotTransverse[#1, #2]) \[Sigma]LowerSingle[4]))&]
    
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(J\), \(Y\)]\))", 3, 2, {{#1, Spinor}, {#1, DottedSpinor}} &,
   (2 (x[#1, 4] - x[#2, 4])/Sqrt[XXSquaredTransverse[#2]] SpinorXTransverse[#1] 
   + (XXSquaredTransverse[#2] - XXSquaredTransverse[#1] + XXSquaredDefect[#1, #2])/(Sqrt[XXSquaredTransverse[#2]]) \[Sigma]LowerSingle[4])&]
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(J\), \(~\)], \(Y\)]\))", 3, 2, {{#1, Spinor}, {#1, DottedSpinor}} &,
   1/Sqrt[XXSquaredTransverse[#2]] Contract[TensorProduct[XXTransverse[#1], XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]Upper], {{1, 3}, {2, 4}, {5, 6}}] &]
   
AddSpacetimeStructure["(\!\(\*SubscriptBox[\(K\), \(Y\)]\))", 3, 2, {{#1, Spinor}, {#2, Spinor}} &,
   Contract[TensorProduct[XXTransverse[#1] + XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]CommUpper], {{1, 2}, {3, 5}, {4, 6}}] + (x[#1, 4] - x[#2, 4]) \[Epsilon]Lower &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(K\), \(_\)], \(Y\)]\))", 3, 2, {{#1, DottedSpinor}, {#2, DottedSpinor}} &,
   Contract[TensorProduct[XXTransverse[#1] + XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]CommUpperDot], {{1, 2}, {3, 5}, {4, 6}}] - (x[#1, 4] - x[#2, 4]) \[Epsilon]LowerDot &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(K\), \(~\)], \(Y\)]\))", 3, 2, {{#1, Spinor}, {#2, Spinor}} &,
   ((1/XXDotTransverse[#1, #2])(- XXDotTransverse[#1, #2] Contract[TensorProduct[XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]CommUpper], {{1, 2}, {3, 5}, {4, 6}}] 
    - XXDotTransverse[#1, #2] Contract[TensorProduct[XXTransverse[#1], \[Epsilon]Transverse[3], \[Sigma]CommUpper], {{1, 2}, {3, 5}, {4, 6}}]
    - XXDotTransverse[#1, #2] (x[#1, 4] - x[#2, 4]) \[Epsilon]Lower
    -2 Contract[TensorProduct[XXTransverse[#1], XXTransverse[#2], XX[#1, #2], \[Epsilon]Transverse[3], \[Eta]Lower, \[Sigma]CommUpper], {{1, 4}, {2, 5}, {3, 7}, {6, 9}, {8, 10}}])) &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[OverscriptBox[\(K\), \(_\)], \(~\)], \(Y\)]\))", 3, 2, {{#1, DottedSpinor}, {#2, DottedSpinor}} &,
   ((1/XXDotTransverse[#1, #2])(- XXDotTransverse[#1, #2] Contract[TensorProduct[XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]CommUpperDot], {{1, 2}, {3, 5}, {4, 6}}] 
    - XXDotTransverse[#1, #2] Contract[TensorProduct[XXTransverse[#1], \[Epsilon]Transverse[3], \[Sigma]CommUpperDot], {{1, 2}, {3, 5}, {4, 6}}]
    + XXDotTransverse[#1, #2] (x[#1, 4] - x[#2, 4]) \[Epsilon]LowerDot
    -2 Contract[TensorProduct[XXTransverse[#1], XXTransverse[#2], XX[#1, #2], \[Epsilon]Transverse[3], \[Eta]Lower, \[Sigma]CommUpperDot], {{1, 4}, {2, 5}, {3, 7}, {6, 9}, {8, 10}}])) &];

AddSpacetimeStructure["(\!\(\*SubscriptBox[\(L\), \(Y\)]\))", 3, 1, {{#1, Spinor}, {#1, Spinor}} &,
   Contract[TensorProduct[XX[#1], \[Epsilon]Transverse[3], \[Sigma]CommUpper], {{1, 2}, {3, 5}, {4, 6}}] &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(L\), \(_\)], \(Y\)]\))", 3, 1, {{#1, DottedSpinor}, {#1, DottedSpinor}} &,
   Contract[TensorProduct[XX[#1], \[Epsilon]Transverse[3], \[Sigma]CommUpperDot], {{1, 2}, {3, 5}, {4, 6}}] &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[\(L\), \(~\)], \(Y\)]\))", 3, 2, {{#1, Spinor}, {#1, Spinor}} &,
   ( (1/XXSquaredTransverse[#2])(-(2 XXDot[#1, #2] - XXSquared[#1] - XXSquared[#2]) Contract[TensorProduct[XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]CommUpper], {{1, 2}, {3, 5}, {4, 6}}] 
   + 2 XXSquaredTransverse[#2] Contract[TensorProduct[XXTransverse[#1], \[Epsilon]Transverse[3], \[Sigma]CommUpper], {{1,2}, {3, 5}, {4, 6}}]
   -4 Contract[TensorProduct[XXTransverse[#1], XXTransverse[#2], XX[#1, #2], \[Epsilon]Transverse[3], \[Eta]Lower, \[Sigma]CommUpper], {{1, 4}, {2, 5}, {3, 7}, {6, 9}, {8, 10}}])) &];
AddSpacetimeStructure["(\!\(\*SubscriptBox[OverscriptBox[OverscriptBox[\(L\), \(_\)], \(~\)], \(Y\)]\))", 3, 2, {{#1, DottedSpinor}, {#1, DottedSpinor}} &,
   ( (1/XXSquaredTransverse[#2])(-(2 XXDot[#1, #2] - XXSquared[#1] - XXSquared[#2]) Contract[TensorProduct[XXTransverse[#2], \[Epsilon]Transverse[3], \[Sigma]CommUpperDot], {{1, 2}, {3, 5}, {4, 6}}] 
   + 2 XXSquaredTransverse[#2] Contract[TensorProduct[XXTransverse[#1], \[Epsilon]Transverse[3], \[Sigma]CommUpperDot], {{1,2}, {3, 5}, {4, 6}}]
   -4 Contract[TensorProduct[XXTransverse[1], XXTransverse[2], XX[1, 2], \[Epsilon]Transverse[3], \[Eta]Lower, \[Sigma]CommUpperDot], {{1, 4}, {2, 5}, {3, 7}, {6, 9}, {8, 10}}])) &];

numSTStructures[ls_, q_] /; Length[ls] == 2 := Infinity; 

numSTStructures[ls_, None] := 
 Which[Length[ls] == 2, Boole[ls[[2]] == Reverse[ls[[1]]]], Length[ls] == 3, 
  With[{js = 
     Tuples[Range[Abs[#[[1]] - #[[2]]], #[[1]] + #[[2]]] & /@ ls]},
   Count[
    js, {x_?NumericQ, y_, z_} /; 
     Total[Sort[{x, y, z}][[;; 2]]] >= Max[{x, y, z}] && 
      IntegerQ[x + y + z]]
   ], Length[ls] == 4, 
  Length@Select[Tuples[Range[-#, #] & /@ Flatten[ls]], 
    Total[#[[{1, 3, 5, 7}]]] == Total[#[[{2, 4, 6, 8}]]] &]];
 
structTypes[n_, q_ : None] := Select[Flatten@Table[
   MyInactive[Structure[struct, q]] @@@ Tuples[Range[n], m]
 , {m, Min[n, maxPts[q]]}
 , {struct, allStructures[m, q]}
], validStruct];

structBasis[n_, q_ : None] := Normal[spins[#, n]] -> # & /@ structTypes[n, q];

maxMultiplier[basisElem_, target_] := 
  Floor@Min[Part[target, Sequence @@ #[[1]]]/#[[2]] & /@ 
    Select[ArrayRules[basisElem], TrueQ[#[[2]] > 0] &]];
sumsToProducts[{}, target_] := If[Total@Flatten[target] === 0, {1}, {}];
sumsToProducts[basis_, target_] := 
  Flatten@Table[
    Thread[TensorProduct[TensorProduct[Sequence @@ Table[basis[[1,2]], n]], sumsToProducts[Rest[basis], target - n basis[[1, 1]]]]], {n,
     maxMultiplier[basis[[1, 1]], target], 0, -1}];

constructStructure[expr_, perm_] := With[{inds = structIndices[expr]},
	With[{unsym = TensorTranspose[CanonicallyOrderedComponents[expr /. MyInactive -> Identity], Ordering[inds[[;; , 2]]]], syms = Select[Permutations[Range@Length[inds]], inds[[#]] === inds &]},
	   If[syms == {}, unsym,
		(1/Length[syms]) TensorTranspose[Sum[TensorTranspose[unsym, p], {p, syms}], perm]
	   ]
	]
]

SpacetimeStructureExpressions::num = "Only 2-, 3-, and 4-point structures are supported.";
Options[SpacetimeStructureExpressions] = {"Overcomplete" -> False, "MonitorProgress" -> False};
SpacetimeStructureExpressions[ls_, opt : OptionsPattern[]] := SpacetimeStructureExpressions[ls, $qdefect, opt];
SpacetimeStructureExpressions[ls_, q_, opt : OptionsPattern[]] := SpacetimeStructureExpressions[ls, q, opt] = If[!(2 <= Length[ls] <= 4),
   Message[SpacetimeStructureExpressions::num],
   If[TrueQ[OptionValue["Overcomplete"]],
   	With[{s2p = sumsToProducts[structBasis[Length[ls], q], ls], cinds = Flatten[Table[{i, If[j == 1, Spinor, DottedSpinor]}, {i, Length[ls]}, {j, 2}, {k, 2 ls[[i, j]]}], 2]},
        Select[{#, FindPermutation[If[Total[Flatten[ls]] == 0, {}, structIndices[#]], cinds]}& /@ s2p, True || EvenQ@Length[Cases[#, s_String /; StringContainsQ[s, "*"] || s == "\!\(\*OverscriptBox[\(I\), \(~\)]\)", All, Heads -> True]] &]
   	],
   	With[{structs = SpacetimeStructureExpressions[ls, q, "Overcomplete" -> True]},
   	   If[Length[structs] <= 1, structs,
	   	With[{comps = constructStructure @@@ structs},
	   		structs[[IndependentSet[Transpose@Flatten[Table[Transpose@ArrayFlatten[Flatten /@ comps] /. genericPoint[q, z], {z, 2, 5}], 1], "MonitorProgress" -> OptionValue["MonitorProgress"], "Rules" -> Thread[crossRatios[q] -> Last[safeCrossRatios[q]]], "MaxIndependent" -> numSTStructures[ls, q], "Indices" -> True]]]
	   	]
   	   ]
   	]
   ]
];

SpacetimeStructures[dims_, ls_, derivs_, derivtype_, perm_] := SpacetimeStructures[dims, ls, derivs, derivtype, perm, $qdefect];
SpacetimeStructures[dims_, ls_, derivs_, derivtype_, perm_, q_] := Table[Tensor[{{SpacetimeStructure[dims, ls, derivs, derivtype, perm, q, i],
   Sequence @@ Flatten[Table[
      {Table[{Lowered[Spinor], Lowered[DottedSpinor]}, Count[derivs, j]], Table[Lowered[Spinor], 2 ls[[j, 1]]], Table[Lowered[DottedSpinor], 2 ls[[j, 2]]]},
      {j, Length[dims]}]]}}], {i, Length[SpacetimeStructureExpressions[ls, q]]}];
      
BuildTensor[{SpacetimeStructure[dims_, ls_, {}, derivtype_, perm_, q_, i_], idxs___}] := With[{comps = Explicit[KinematicPrefactor[dims, ls, q]] (constructStructure @@ SpacetimeStructureExpressions[ls, q][[i]])},
 	If[Length[{idxs}] == 0, 
 	   comps /. x[ii_, j_] :> x[perm[[ii]], j],
 	   SparseArray[ArrayRules[comps] /. x[ii_, j_] :> x[perm[[ii]], j]]
 	]    
];

BuildTensor[{SpacetimeStructure[dims_, ls_, 
      derivs_, derivtype_, perm_, q_, i_], idxs___}] /; derivs =!= {} := 
  With[{bd = 
     SpacetimeStructures[dims, ls, {}, "\[PartialD]", perm, q][[i]]},
   With[{dsis = 
      Table[2 Length[derivs] + 
        Join @@ (Range[#1 + 1, #2] & @@@ 
           Partition[Accumulate[2 Flatten[ls[[;; ii]]]], 2]), {ii, 
        Length[dims] - 1}],
     sis = 
      Table[2 Length[derivs] + 
        Join @@ (Range[#1 + 1, #2] & @@@ 
           Partition[Accumulate[Prepend[2 Flatten[ls[[;; ii]]], 0]], 
            2]), {ii, Length[dims] - 1}], 
     pd = perm[[#]] & /@ derivs},
    TensorTranspose[
     CanonicallyOrderedComponents@Fold[TensorPermute, Switch[derivtype,
        "\[PartialD]", 
        Fold[TensorSpinorDerivative, bd, ReverseSort[pd]],
        "u", 
        TensorProduct[
         Fold[TensorSpinorDerivative, u[perm], ReverseSort[pd]], bd],
        "v", 
        TensorProduct[
         Fold[TensorSpinorDerivative, v[perm], ReverseSort[pd]], bd]
        ], 
       Table[PermutationList[
         InversePermutation@
          PermutationPower[
           Cycles[{Join[
              Range[2 Count[derivs, x_ /; x <= (Length[dims] - ii)] + 
                1, 2 Count[derivs, 
                 x_ /; x <= (Length[dims] + 1 - ii)], 2], 
              sis[[Length[dims] - ii]]], 
             Join[Range[
               2 Count[derivs, x_ /; x <= (Length[dims] - ii)] + 2, 
               2 Count[derivs, x_ /; x <= (Length[dims] + 1 - ii)], 
               2], dsis[[Length[dims] - ii]]]}], 
           Count[derivs, Length[dims] + 1 - ii]], 
         Length@Indices[bd] + 2 Length[pd]], {ii, Length[dims] - 1}]],
      Ordering@{idxs}]
    ]
   ];