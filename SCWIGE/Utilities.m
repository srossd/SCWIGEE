(* Wolfram Language package *)

timeString[secs_?NumericQ] /; !IntegerQ[secs] := timeString[Round[secs]];
timeString[secs_Integer] := If[secs <= 60, ToString[secs] <> "s", ToString[Floor[secs/60]] <> "m" <> ToString[Mod[secs, 60]] <> "s"];
timeString[str_String] := str;

progressString[done_, total_] := StringJoin @@ Flatten[{"[", Table["=", done], Table[" ", total - done], "]"}];

Options[monitorProgress] = {"Resolution" -> Automatic, "Label" -> None, "CurrentDisplayFunction" -> Full};
SetAttributes[monitorProgress, HoldFirst];
If[$consoleMode,
   monitorProgress[(h: Do | Table)[elem_, vars__], OptionsPattern[]] := Module[{start, elapsed, current, total, remaining, totaltime, steptime, resolution, ans},
       total = Length@Flatten[Table[Null, vars]];
       current = 1;
       start = AbsoluteTime[];
       resolution = OptionValue["Resolution"] /. Automatic -> 20;
       ans = h[
          elapsed = If[current > 1, AbsoluteTime[] - start, "NA"];
          steptime = If[current > 1, elapsed/(current - 1), "NA"];
          totaltime = If[current > 1, steptime total, "NA"];
          remaining = If[current > 1, totaltime - elapsed, "NA"];
           
          Run[If[$OperatingSystem == "Windows", "cls", "clear"]];
          If[OptionValue["Label"] =!= None, Print[OptionValue["Label"]]];
          Print["Current item: ",current];
          Print["Progress: ", current - 1, "/", total];  
          Print["Time elapsed: ", timeString[elapsed]];
          Print["Time per step: ", timeString[steptime]];
          Print["Est. time remaining: ", timeString[remaining]];
          Print["Est. total time: ", timeString[totaltime]];	
          Print[progressString[Floor[(current - 1)*resolution/total], resolution]];
          current += 1;
          
          elem,
          vars
       ];
       Run[If[$OperatingSystem == "Windows", "cls", "clear"]];
       ans
   ];
   monitorProgress[Fold[f_, x_, list_], OptionsPattern[]] := Module[{start, elapsed, current, total, remaining, totaltime, steptime, resolution, ans},
       total = Length[list];
       current = 1;
       start = AbsoluteTime[];
       resolution = OptionValue["Resolution"] /. Automatic -> 20;
       ans = Fold[Function[{y, z},
	          elapsed = If[current > 1, AbsoluteTime[] - start, "NA"];
	          steptime = If[current > 1, elapsed/(current - 1), "NA"];
	          totaltime = If[current > 1, steptime total, "NA"];
	          remaining = If[current > 1, totaltime - elapsed, "NA"];
	           
	          Run[If[$OperatingSystem == "Windows", "cls", "clear"]];
	          If[OptionValue["Label"] =!= None, Print[OptionValue["Label"]]];
	          Print["Current item: ",current];
	          Print["Progress: ", current - 1, "/", total];  
	          Print["Time elapsed: ", timeString[elapsed]];
	          Print["Time per step: ", timeString[steptime]];
	          Print["Est. time remaining: ", timeString[remaining]];
	          Print["Est. total time: ", timeString[totaltime]];	
	          Print[progressString[Floor[(current - 1)*resolution/total], resolution]];
	          current += 1;
	          
	          f[y, z]
       	  ],
          x, list
       ];
       Run[If[$OperatingSystem == "Windows", "cls", "clear"]];
       ans
   ];
   ,
   monitorProgress[expr_, opt : OptionsPattern[]] := ResourceFunction["MonitorProgress"][expr, opt]
];
    
zeroVecQ[vec_] := MatchQ[vec,{0..}] || MatchQ[Simplify[ArrayRules[vec][[;;,2]]],{0..}];
    
(* fix this to prevent false positive *)
indQ[basis_, vec_] := Length[basis] == 0 || (! zeroVecQ[vec] && 
   If[Length[basis[[1]]] < Max[2000, 20  Length[basis]],
    Quiet@Check[LinearSolve[Transpose[basis], vec]; False, True],
    Module[{nzidxs, chosen, sol, sbz},
     nzidxs = Select[ArrayRules[vec][[;; , 1, 1]], IntegerQ];
     chosen = 
      RandomSample[nzidxs, Min[Length[nzidxs], 20  Length[basis]]];
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
		monitorProgress[
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
  Module[{numParams = Dimensions[data][[2]] - 1, params, monomials, 
    numMonomials, rat, mats, found, ans}, 
   params = (ToExpression["\\[Formal" <> # <> "]"] & /@ 
       RotateLeft[Capitalize@Alphabet[], 20])[[;; numParams]];
   monomials = 
    Times @@@ (params^# & /@ 
       Select[Tuples[Range[0, deg], numParams], Total[#] <= deg &]);
   If[AllTrue[data[[;; , -1]], # === 0 &],
    0 &,
    numMonomials = 
     Table[monomials /. Thread[params -> Most[pt]], {pt, data}];
    rat = 
     FirstCase[data[[;; , -1]], 
      x_ /; x =!= 0 :> (If[# == {}, 1, #[[1]]] &@
         Cases[x, Power[y_, 1/2 | -1/2] :> Sqrt[y], All])];
    mats = 
     Table[ArrayFlatten[{{(pf @@@ (Most /@ 
              data)) numMonomials, (data[[;; , -1]] /
            rat) numMonomials}}], {pf, OptionValue["Prefactors"]}];
    found = False;
    ans = 0;
    Do[
     If[
      MatrixRank[N@mats[[i]]] != Min[Dimensions[mats[[i]]]],
      With[{null = NullSpace[mats[[i]]]},
       If[null =!= {},
        ans = -rat  (OptionValue["Prefactors"][[i]] @@ 
            params) null[[1, ;; Length[monomials]]] . monomials/
          null[[1, Length[monomials] + 1 ;;]] . monomials;
        If[ans =!= 0,
         found = True;
         Break[]
         ];
        ]
       ]
      ],
     {i, Length[mats]}
     ];
    If[found, 
     With[{params2 = params, ans2 = Check[Simplify[ans], Print[ans]; ans]}, 
      Function[Evaluate@params2, ans2]], None]
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

Options[solveGroups] = {"Assumptions" -> {}, 
   "Transformation" -> Simplify, "TempRules" -> {}};
solveGroups::nosol = "No solution could be found.";
solveGroups[grps_, vars_, OptionsPattern[]] := 
 DeleteCases[
 monitorProgress[
  Fold[If[Length[#1] == 
       Length[vars] || (Length[#1] > 0 && Last[#1] === None), #1, 
     With[{sol = 
        Quiet[Assuming[OptionValue["Assumptions"], 
          With[{tmp = 
             Solve[#2 /. #1 /. OptionValue["TempRules"], 
               vars /. OptionValue["TempRules"]] /. (Reverse /@ 
                OptionValue["TempRules"])}, 
           If[tmp === {}, Missing[], 
            OptionValue["Transformation"][First[tmp], 
             OptionValue["Assumptions"]]]]]]}, 
      If[MissingQ[sol], Message[solveGroups::nosol]; 
       Append[#1, None],
       Sort@
        DeleteDuplicatesBy[
         Table[If[FreeQ[rule, Alternatives @@ Keys[sol]], rule, 
           OptionValue["Transformation"][
            rule /. HoldPattern[a_ -> b_] :> (a -> (b /. sol)), 
            OptionValue["Assumptions"]]], {rule, Join[#1, sol]}], 
         First]]]] &, {}, grps], "Label" -> "Solving equations", 
  "CurrentDisplayFunction" -> None], None]
  
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

unrollRows[mat_, subset_, numRows_] := SparseArray[ArrayRules[mat] /. {a_Integer, b_Integer} :> {subset[[a]], b}, {numRows, Length[mat[[1]]]}];