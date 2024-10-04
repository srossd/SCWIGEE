(* Wolfram Language package *)
    
zeroVecQ[vec_] := MatchQ[vec,{0..}] || MatchQ[Simplify[ArrayRules[vec][[;;,2]]],{0..}];
    
(* fix this to prevent false positive *)
indQ[basis_, vec_] := Length[basis] == 0 || (! zeroVecQ[vec] && 
   If[Length[basis[[1]]] < 10  Length[basis],
    Quiet@Check[LinearSolve[Transpose[basis], vec]; False, True],
    Module[{nzidxs, chosen, sol, sbz},
     nzidxs = Select[ArrayRules[vec][[;; , 1, 1]], IntegerQ];
     chosen = 
      RandomSample[nzidxs, Min[Length[nzidxs], 10  Length[basis]]];
      sol = Quiet@LinearSolve[Transpose[basis][[chosen]], vec[[chosen]]];
      If[Head[sol] === LinearSolve,
         True,
	     sbz = sol . basis - vec;
	     !zeroVecQ[sbz] (* could be that a different solution would have worked *)
      ]
     ]
    ]);

Options[IndependentSet] = {"Rules" -> {}, "MonitorProgress" -> False, "MaxIndependent" -> 0, "Indices" -> False};
IndependentSet[{}] := {};
IndependentSet[tensors_, OptionsPattern[]] := If[!ArrayQ[tensors[[1]]] && Indices[tensors[[1]]] == {},
   If[TrueQ[OptionValue["Indices"]], {1}, tensors[[{1}]]],
   With[{indices = Module[{runningComps = SparseArray[{}, {Length[tensors], If[!ArrayQ[tensors[[1]]], Times @@ (First@*IndexData@*First /@ Indices[tensors[[1]]]), Length[Flatten[tensors[[1]]]]]}]},
	If[TrueQ[OptionValue["MonitorProgress"]] || ArrayQ[OptionValue["MonitorProgress"]],
		ResourceFunction["MonitorProgress"][
			Fold[
			   If[OptionValue["MaxIndependent"] == 0 || Length[#1] < OptionValue["MaxIndependent"],
				    With[{comp = Flatten@Normal@CanonicallyOrderedComponents[tensors[[#2]]] /. OptionValue["Rules"]},
						If[indQ[runningComps[[;;Length[#1]]], comp], 
							runningComps[[Length[#1] + 1]] = comp; Append[#1, #2],
							#1
						]
				    ],
				    #1
				] &, 
				{}, 
				Range@Length[tensors]
			], Sequence @@ If[ArrayQ[OptionValue["MonitorProgress"]], OptionValue["MonitorProgress"], {}],
			"CurrentDisplayFunction" -> None 	
		],
		Fold[
		    If[OptionValue["MaxIndependent"] == 0 || Length[#1] < OptionValue["MaxIndependent"],
			    With[{comp = Flatten@Normal@CanonicallyOrderedComponents[tensors[[#2]]] /. OptionValue["Rules"]},
					If[indQ[runningComps[[;;Length[#1]]], comp], 
						runningComps[[Length[#1] + 1]] = comp; Append[#1, #2],
						#1
					]
			    ],
			    #1
			] &, 
			{}, 
			Range@Length[tensors]
		]
	]
]}, If[TrueQ[OptionValue["Indices"]], indices, tensors[[indices]]]
]];


   
Options[fitRational] = {"Prefactors" -> {1 &}};
fitRational[data_, deg_, opt : OptionsPattern[]] := 
  Module[{numParams = Dimensions[data][[2]] - 1, params, tups, 
    ansatzes, vars, rat, mats, found, ans},
   params = (ToExpression["\\[Formal" <> # <> "]"] & /@ 
       RotateLeft[Capitalize@Alphabet[], 20])[[;; numParams]];
   tups = 
    Select[Tuples[Range[0, deg], numParams], Total[#] <= deg &];
   If[AllTrue[data[[;; , -1]], # === 0 &], 0 &,
    ansatzes = 
     Table[f @@ params, {f, OptionValue[
       "Prefactors"]}]  (Sum[(\[Beta]up @@ tup)  Times @@ 
           Thread[params^tup], {tup, tups}]/
        Sum[(\[Beta]down @@ tup)  Times @@ Thread[params^tup], {tup, 
          tups}]);
    vars = 
     Flatten[Table[{\[Beta]up @@ tup, \[Beta]down @@ tup}, {tup, 
        tups}]];
    rat = 
     FirstCase[data[[;; , -1]], 
      x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
         Cases[x, Power[y_, 1/2 | -1/2] :> Sqrt[y], All])]; 
    mats = Table[(Denominator[ansatz]  Simplify[Last[pt]/rat] - 
          Numerator[ansatz] /. Thread[params -> Most[pt]]) /. 
       Thread[vars -> IdentityMatrix[Length[vars]]], {ansatz, 
       ansatzes}, {pt, data}];
    found = False;
    ans = 0;
    
    Do[If[MatrixRank[N@mats[[i]]] != Min[Dimensions[mats[[i]]]], 
      With[{null = NullSpace[mats[[i]]]}, 
       If[null =!= {}, 
        ans = rat  ansatzes[[i]] /. Thread[vars -> null[[1]]];
        If[ans =!= 0, found = True;
         Break[]];]]], {i, Length[mats]}];
    If[found, 
     With[{params2 = params, ans2 = Simplify[ans]}, 
      Function[Evaluate@params2, ans2]
     ], 
     None
    ]
    ]
   ];

expansion[structure_, basis_, relations_] := 
  With[{idx = Position[basis, structure, 1][[1, 1]], 
    pivots = 
     Association@
      KeyValueMap[Rule @@ Reverse[{#1, #2}] &, 
       Sort[#][[1, 1, 2]] & /@ 
        KeySelect[GroupBy[ArrayRules[relations], #[[1, 1]] &], 
         IntegerQ]]},
   If[Length[pivots] == Length[basis], 0,
   If[KeyExistsQ[pivots, 
     idx], -relations[[pivots[idx], 
      Complement[Range@Length[basis], Keys@pivots]]], 
    Table[Boole[i == idx], {i, Length[basis]}][[
     Complement[Range@Length[basis], Keys@pivots]]]]
   ]
  ];

solveGroups::nosol = "No solution could be found.";
solveGroups[grps_, vars_, rules_, assum_] := 
 ResourceFunction["MonitorProgress"][Fold[
   With[{sol = Quiet[Assuming[assum, Simplify@With[{tmp = Solve[#2 /. #1, vars]},
      If[tmp === {}, Missing[], First[tmp]]
   ]]]},
     If[MissingQ[sol], Message[solveGroups::nosol]; Return[{}]];
     Sort@Select[Join[Simplify[#1 /. sol, assum], sol], ! SameQ @@ # &]
     ] &,
   {}, grps
   ], "Label" -> "Solving equations", 
  "CurrentDisplayFunction" -> None
  ]
  
withCounts[xs_] := Last@FoldList[
    Function[{list, x}, 
     Append[list, {x, Count[list[[;; , 1]], x] + 1}]
    ], 
    {}, 
    xs
  ];
  
(* SortBy without breaking ties by canonical order *)
stableSortBy[xs_, f_] := FixedPoint[Replace[#, {a___, x_, y_, b___} /; ! OrderedQ[{f[x], f[y]}] :> {a, y, x, b}] &, xs];
stableOrderingBy[xs_, f_] := PermutationList[FindPermutation[stableSortBy[xs, f], xs], Length[xs]];