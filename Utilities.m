(* Wolfram Language package *)
    
indQ[basis_, vec_] := !MatchQ[vec, {0..}] && Quiet@Check[LinearSolve[Transpose[basis], vec]; False, True];

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


   
Options[fitRational] = {"Prefactors" -> {1}};
fitRational[data_, step_, maxDeg_, opt : OptionsPattern[]] := 
  fitRational[data, 0, step, maxDeg, opt];
fitRational[data_, deg_, step_, maxDeg_, opt : OptionsPattern[]] := 
  If[AllTrue[data[[;; , 3]], # === 0 &], 0, 
   With[{ansatzes = 
      OptionValue[
        "Prefactors"] (Sum[\[Beta]up[k, l] u^(k step) v^(l step), {k, 
           0, deg/step}, {l, 0, deg/step - k}]/
         Sum[\[Beta]down[k, l] u^(k step) v^(l step), {k, 0, 
           deg/step}, {l, 0, deg/step - k}]), 
     vars = Flatten[
       Table[{\[Beta]up[k, l], \[Beta]down[k, l]}, {k, 0, 
         deg/step}, {l, 0, deg/step - k}]], 
     rat = FirstCase[data[[;; , 3]], 
       x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
          Cases[x, Power[y_, 1/2 | -1/2] :> Sqrt[y], All])]},
    Module[{mats = 
       Table[(Denominator[ansatz] Simplify[pt[[3]]/rat] - 
            Numerator[ansatz] /. {u -> pt[[1]], v -> pt[[2]]}) /. 
         Thread[vars -> IdentityMatrix[Length[vars]]], {ansatz, 
         ansatzes}, {pt, data}], found = False, ans = 0},
     Do[
      If[MatrixRank[N@mats[[i]]] != Min[Dimensions[mats[[i]]]],
       With[{null = NullSpace[mats[[i]]]},
        If[null =!= {},
         found = True;
         ans = rat ansatzes[[i]] /. Thread[vars -> null[[1]]];
         Break[];
         ]
        ]
       ],
      {i, Length[mats]}
      ];
     If[found, ans,
      If[deg < maxDeg, 
       fitRational[data, deg + step, step, maxDeg, opt], Print["!"]; 
       Null]
      ]
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
   If[KeyExistsQ[pivots, 
     idx], -relations[[pivots[idx], 
      Complement[Range@Length[basis], Keys@pivots]]], 
    Table[Boole[i == idx], {i, Length[basis]}][[
     Complement[Range@Length[basis], Keys@pivots]]]]
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