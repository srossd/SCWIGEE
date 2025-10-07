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

Options[solveGroups] = {"Assumptions" -> {}, "Transformation" -> Simplify, "TempRules" -> {}, "RemoveRedundant" -> False};
solveGroups::nosol = "No solution could be found.";
solveGroups[grps_, vars_, OptionsPattern[]] := 
 DeleteCases[
 monitorProgress[
  Fold[If[Length[#1] == Length[vars] || #1 === {None}, 
        #1, 
        Module[{teqs, tvars, bb, mm, inds, sol, tmp},
          teqs = OptionValue["Transformation"][#2 /. #1] /. OptionValue["TempRules"];
          tvars = vars /. OptionValue["TempRules"];
          tmp = Quiet@Solve[teqs, tvars] /. (Reverse /@ OptionValue["TempRules"]);
          If[tmp === {} && OptionValue["RemoveRedundant"],
            {bb, mm} = CoefficientArrays[teqs, tvars];
            inds = If[OptionValue["RemoveRedundant"], IndependentSet[mm, "Indices" -> True], Range@Length[mm]];
            tmp = Quiet@Solve[teqs[[inds]], tvars] /. (Reverse /@ OptionValue["TempRules"]);
          ];
          sol = Assuming[OptionValue["Assumptions"], If[tmp === {}, Missing[], OptionValue["Transformation"][First[tmp], OptionValue["Assumptions"]]]];
          If[MissingQ[sol], 
            Message[solveGroups::nosol]; {None},
            Sort@DeleteDuplicatesBy[
              Table[
                If[FreeQ[rule, Alternatives @@ Keys[sol]], 
                  rule, 
                  OptionValue["Transformation"][rule /. HoldPattern[a_ -> b_] :> (a -> (b /. sol)), OptionValue["Assumptions"]]
                ], 
              {rule, Join[#1, sol]}
              ], 
            First]
          ]
        ]
      ] &, {}, grps], "Label" -> "Solving equations", 
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