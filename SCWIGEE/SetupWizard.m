(* Wolfram Language package *)

SetGlobalSymmetry[group_] := (
	$RSymmetry = group;
	SetQGlobalRep[fundRep[$RSymmetry]];
	$QTensor = Tensor[{{"Q", Raised[GlobalIndex[QGlobalRep[]]], Lowered[Spinor]}}];
	$QBarTensor = Tensor[{{"\!\(\*OverscriptBox[\(Q\), \(~\)]\)", Lowered[GlobalIndex[QGlobalRep[]]], Lowered[DottedSpinor]}}];
	
	$DefectRSymmetry = Null;
	$embedding = Null;
);

SetQGlobalRep[rep_] := If[FailureQ[Enclose[ConfirmQuiet[RepName[GlobalSymmetry[], rep]]]], 
   MessageDialog[StringForm["`` is not a valid R-symmetry representation.", rep]],
   $QGlobalRep = SimpleRepInputConversion[GlobalSymmetry[], rep];
   $QTensor = Tensor[{{"Q", Raised[GlobalIndex[QGlobalRep[]]], Lowered[Spinor]}}];
   $QBarTensor = Tensor[{{"\!\(\*OverscriptBox[\(Q\), \(~\)]\)", Lowered[GlobalIndex[QGlobalRep[]]], Lowered[DottedSpinor]}}];
];

SetDefectGlobalSymmetry::noembed = "There is no embedding of `1` into `2`; cannot proceed.";
SetDefectGlobalSymmetry[group_] := Module[{embeddings},
	embeddings = Embeddings[$RSymmetry, group];
	If[embeddings == {}, 
	   Message[SetDefectGlobalSymmetry::noembed, CMtoName[group], CMtoName[$RSymmetry]],
	
		$DefectRSymmetry = group;
		If[$RSymmetry === {SU2, U1} && $qdefect === 2, Which[group === SU2, $q2type = {4, 0}, group === U1, $q2type = {2,2}]];
		
		$embedding = allEmbeddings[$RSymmetry, $DefectRSymmetry][[1,2]] /. _embeddingParameter -> 1;
	]
];

SetDefectGlobalSymmetry[group_, embedding_] := ($DefectRSymmetry = group; $embedding = embedding;);

SetSymmetries[ops_] := Do[
	DeclareTensorSymmetry[op[[1]], Join[
		Table[{Cycles[{{i, i+1}}], 1}, {i, 2, 2 + 2 Spin[op][[1]] - 2}],
		Table[{Cycles[{{i, i+1}}], 1}, {i, 2 + 2 Spin[op][[1]], 2 + 2 Spin[op][[1]] + 2 Spin[op][[2]] - 2}]
	]],
	{op, ops}];

$multipletIndices = {};
SetMultiplet[ops_, name_, sc_, i_ : 1] := (
	SetSymmetries[Flatten[ops]]; 
	$multipletIndices = Union[$multipletIndices, {i}]; 
	$multiplet[i] = ops;
	$multipletName[i] = name;
	$multipletSC[i] = sc;
);

GlobalSymmetry[] := $RSymmetry;
QGlobalRep[] := $QGlobalRep;
DefectGlobalSymmetry[] := $DefectRSymmetry;
Multiplet[i_] := $multiplet[i];

$signatureFactor = 1;
SignatureFactor[] := $signatureFactor;

SetSignature["Lorentzian"] := ($signatureFactor = 1;);
SetSignature["Euclidean"] := ($signatureFactor = I;);

SetSignature::badsig = "The signature `` is not recognized; use \"Lorentzian\" or \"Euclidean\".";
SetSignature[sig_] := Message[SetSignature::badsig, sig];

SetDefectCodimension::invalid = "The codimension `` needs to be an integer between 1 and 3 inclusive, or None.";
SetDefectCodimension[q_] := If[MemberQ[{None,1,2,3},q], ($qdefect = q;), Message[SetDefectCodimension::invalid, q]];

SetDefectCodimension[3, \[CurlyPhi]_ : 0] := ($qdefect = 3; $q3angle = \[CurlyPhi];);
SetDefectCodimension[2, type_] := ($qdefect = 2; $q2type = type;);
SetDefectCodimension[1, \[CurlyPhi]_ : 0] := ($qdefect = 3; $q1angle = \[CurlyPhi];);

extraPos[mult_] := 
  With[{pos = DeleteDuplicates[List @@@ mult[[;; , {5, 3}]]]}, 
   Complement[
    Join[pos, # + {1, 1/2} & /@ pos, # + {-1, 1/2} & /@ pos], pos]];
fundRep[grp_] := 
  If[IsSimpleGroupQ[grp], 
   If[grp == {}, 1, PadRight[{1}, Length[grp]]], fundRep /@ grp];
singRep[grp_] := 
  If[IsSimpleGroupQ[grp], 
   If[grp == {}, 0, PadRight[{}, Length[grp]]], singRep /@ grp];
   
$RSymmetry = Null;
$QGlobalRep = Null;
$DefectRSymmetry = Null;
$embedding = Null;
$editing = True;
$viewingMultiplet = 1;
$multiplet = <||>;
$multipletName = <||>;
$multipletSC = <||>;


$qdefect = None;
$q3angle = 0;
$q2type = {2,2};
$q1angle = 0;

plusIcon = 
  Graphics[{Darker@Orange, Disk[], Thickness[.15], White, 
    Line[{{-.5, 0}, {.5, 0}}], Line[{{0, -.5}, {0, .5}}]}];
   
tikzLine[{{x1_, y1_}, {x2_, y2_}}] := StringForm["\\draw (``, ``) -- (``, ``);", x1, y1, x2, y2];
tikzNode[exprs_, style_, {x1_, y1_}] := StringForm["\\node[``] at (``, ``) {$``$};", style, x1, y1, StringRiffle[exprs, " \\quad "]];
tikzMultiplet[i_] := With[{grp = GroupBy[Flatten[$multiplet[i]], List @@ (#[[{5, 3}]]) &]},
   StringRiffle[Flatten[{
   "\\begin{tikzpicture}[box/.style = {draw,black,thick,rectangle,fill=orange!20,inner sep=.1cm},scale=1.5]",
   tikzLine /@ Map[{-2, -2} # &, Select[Subsets[Keys[grp], {2}],Abs[Subtract @@ #] == {1, 1/2} &], {2}],
   Table[tikzNode[ToString@*TeXForm /@ vals, "box", {-2 vals[[1,5]], -2 vals[[1,3]]}], {vals, Values[grp]}],
   Table[tikzNode[{"\\Delta = "<>ToString[TeXForm[dim]]}, "", {Min[-Keys[grp][[;;,1]]] - 1, -2 dim}], {dim, Keys[grp][[;;,2]] // DeleteDuplicates}],
   "\\end{tikzpicture}"
}], "\n"]];

Options[DisplayMultiplet] := {"EditMode" -> False};
DisplayMultiplet[i_, OptionsPattern[]] := 
  With[{grp = 
     If[OptionValue["EditMode"] && Length[Flatten[$multiplet[i]]] <= 30, 
       Merge[{#, AssociationMap[Function[x, {}], extraPos@Flatten[$multiplet[i]]]}, 
         Flatten@*Join] &, # &]@
      GroupBy[Flatten[$multiplet[i]], List @@ (#[[{5, 3}]]) &]},
   If[(! OptionValue["EditMode"]) || Length[Flatten[$multiplet[i]]] > 30,
    Graphics[{
      Line /@ Map[{-1, -2} # &, 
  Select[Subsets[Keys[grp], {2}], 
   Abs[Subtract @@ #] == {1, 1/2} &], {2}], 
      Text[Style[
          Framed[Row[If[Length[#] > 1 && Length[Flatten[$multiplet[i]]] > 30, {Style[Length[#], Italic]}, TraditionalForm /@ #], Spacer[2]], 
           Background -> LightOrange], 20, 
          FontFamily -> "CMU Serif"], {- #[[1, 5]], -2 #[[1, 3]]}] & /@ 
       Values[grp],
       If[!$multipletSC[i], {Thick, Dashed, Line[{{0, -2 Min[ScalingDimension /@ Flatten[$multiplet[i]]] + .5}, {0, -2 Max[ScalingDimension /@ Flatten[$multiplet[i]]] - .5}}]}, Nothing],
       Table[Text[Style["\[CapitalDelta] = "<>ToString[dim,TraditionalForm], 14, FontFamily -> "CMU Serif"], {Min[-Keys[grp][[;;,1]]] - 1, -2 dim}], {dim, Keys[grp][[;;,2]] // DeleteDuplicates}]
      }, 
     ImageSize -> (75 (Max[Keys[grp][[;; , 1]]] - 
          Min[Keys[grp][[;; , 1]]]))],
    If[Length[grp] == 0,
     Graphics[{
       Text[
        Style[Framed[
          Button[plusIcon, 
           With[{res = 
              opDialog[{}, "New Operator", 
               Operator[Null, 1, Null, {0, 0}, If[$multipletSC[i], 0, Null]]]},
               If[FailureQ[Enclose[ConfirmQuiet[RepName[GlobalSymmetry[], GlobalRep[res]]]]], MessageDialog[StringForm["`` is not a valid R-symmetry representation.", GlobalRep[res]]],
            If[MatchQ[res, _Operator], $multiplet[i] = If[$multipletSC[i], {res}, ReverseSortBy[{{res}, {makeConjugate[res]}}, #[[1,-1]]&]]]]],
           Method -> "Queued", ImageSize -> Small, 
           Appearance -> "Frameless", Enabled -> ($RSymmetry =!= Null)]], 20, 
         FontFamily -> "CMU Serif"], {0, 0}]}],
     Graphics[{ 
       Table[Text[Style["\[CapitalDelta] = "<>ToString[dim,TraditionalForm], 14, FontFamily -> "CMU Serif"], {Min[-.75 Keys[grp][[;;,1]]] - 1, -2 dim}], {dim, Keys[grp][[;;,2]] // DeleteDuplicates}],
       Line /@ (Select[Subsets[Keys[grp], {2}], 
           Abs[Subtract @@ #] == {1, 1/2} &] . {{-0.75, 0}, {0, -2}}), 
       Text[Style[Framed[Grid[{Append[
               Function[x, Button[
                   Style[TraditionalForm[x], 18], 
                   With[{res = 
                    opDialog[
                    DeleteDuplicates[
                    Flatten[Table[
                    ReduceRepProduct[$RSymmetry, {If[diff[[1]] == 1, 
                    ConjugateIrrep[$RSymmetry, QGlobalRep[]], 
                    QGlobalRep[]], GlobalRep[f]}][[;; , 
                    1]], {diff, {{-1, -1/2}, {1, -1/2}}}, {f, 
                    If[KeyExistsQ[grp, #[[1]] + diff], 
                    grp[#[[1]] + diff], {}]}], 2]], "Edit Operator", 
                    x]}, If[MatchQ[res, _Operator] && StringQ[res[[1]]], 
                    $multiplet[i] = $multiplet[i] /. {x -> res, makeConjugate[x] -> makeConjugate[res]}]],
                   Method -> "Queued", Appearance -> "Frameless"
                   ]] /@ #[[2]],
               
               Button[plusIcon, 
                With[{res = 
                   opDialog[
                    DeleteDuplicates[
                    Flatten[Table[
                    ReduceRepProduct[$RSymmetry, {If[diff[[1]] == 1, 
                    ConjugateIrrep[$RSymmetry, QGlobalRep[]], 
                    QGlobalRep[]], GlobalRep[f]}][[;; , 
                    1]], {diff, {{-1, -1/2}, {1, -1/2}}}, {f, 
                    If[KeyExistsQ[grp, #[[1]] + diff], 
                    grp[#[[1]] + diff], {}]}], 2]], "New Operator", 
                    Operator[Null, 1, #[[1, 2]], {0, 0}, #[[1, 1]]]]}, 
                 If[MatchQ[res, _Operator] && StringQ[res[[1]]], 
                    If[$multipletSC[i], 
                       AppendTo[$multiplet[i], res];
                       If[res[[-1]] != 0, AppendTo[$multiplet[i], makeConjugate[res]]], 
                       $multiplet[i] = {
                          Append[$multiplet[i][[1]], If[res[[-1]] >= 0, res, makeConjugate[res]]],
                       	  Append[$multiplet[i][[2]], If[res[[-1]] >= 0, makeConjugate[res], res]]
                       };
                    ]
                 ]
                ],
                Method -> "Queued", ImageSize -> Small, 
                Appearance -> "Frameless"]
               ]}, Alignment -> {Center, Center}, Spacings -> .5], 
            Background -> LightOrange], 20, 
           FontFamily -> 
            "CMU Serif"], {-0.75 #[[1, 1]], -2 #[[1, 2]]}] & /@ 
        Normal[grp]}, 
      ImageSize -> (75 (Max[Keys[grp][[;; , 1]]] - 
           Min[Keys[grp][[;; , 1]]]))]
     ]
    ]
   ];
   
opDialog[reps_, label_, 
   init_ : Operator[Null, 1, 2, {0, 0}, 0]] := ($nfName = 
    init[[1]]; $nfRep = init[[2]]; $nfL = 2 init[[4, 1]]; $nfLb = 
    2 init[[4, 2]]; $nfDim = init[[3]]; $nfRCharge = If[init[[5]] === Null, Null, init[[5]]];
   DialogInput[
    DialogNotebook[{Framed@
       Panel[Grid[{{"Operator name:", 
           InputField[Dynamic[$nfName]]}, {"Global symmetry rep:", 
           If[Length[reps] == 0, InputField[Dynamic[$nfRep]], 
            RadioButtonBar[Dynamic[$nfRep], 
             AssociationMap[RepName[$RSymmetry, #] &, 
              reps]]]}, {"Spinor indices:", 
           InputField[Dynamic[$nfL], 
            Number]}, {"Dotted spinor indices:", 
           InputField[Dynamic[$nfLb], Number]}, 
          If[init[[3]] === Null, {"Scaling dimension:", 
            InputField[Dynamic[$nfDim], Number]}, Nothing],
          If[init[[5]] === Null, {"R-charge:", 
            InputField[Dynamic[$nfRCharge], Number]}, Nothing]}], 
        Style[label, 20], Top, Background -> LightBlue], 
      DefaultButton[
       DialogReturn[
        Operator[ReleaseHold[
          ToString[#, TraditionalForm] & /@ 
           Hold[$nfName]], SimpleRepInputConversion[GlobalSymmetry[], $nfRep], $nfDim, {$nfL, $nfLb}/2, 
         $nfRCharge]]]}]]);
         
multipletDialog[] := ($multName = Null; $multSC = True;
   DialogInput[
    DialogNotebook[{Framed@
       Panel[Grid[{{"Multiplet name:", 
           InputField[Dynamic[$multName]]}, {"Self-conjugate:", 
           Checkbox[Dynamic[$multSC]]}}], 
        Style["Add Multiplet", 20], Top, Background -> LightBlue], 
      DefaultButton[DialogReturn[{ToString[$multName], $multSC}]]}]]);
      
conventionButton[eq_] := With[{s = ToString@TeXForm[eq]},
   Button[ClickToCopy[
  Style[eq, {"Output", "TraditionalForm"}], Null], 
 CopyToClipboard@s, 
 Appearance -> "Frameless"]
]
      
conventions[1] := {\[Eta]Lower == MatrixForm[Components[\[Eta]Lower]], \[Epsilon]Lower ==
   MatrixForm@Components[\[Epsilon]Lower], \[Epsilon]LowerDot == 
  MatrixForm@Components[\[Epsilon]LowerDot], \[Sigma]Lower == 
  MatrixForm /@ Components[\[Sigma]Lower], \[Sigma]BarLower == 
  MatrixForm /@ Components[\[Sigma]BarLower], 
 Subscript["\[Epsilon]", Row[{0, 1, 2, 3}]] == 
  Components[\[Epsilon]Spacetime][[1, 2, 3, 4]]} /. (x_ == y_) :> conventionButton[x == y];
  
conventions[2] := Flatten[Table[
   MyInactive[Structure[struct, $qdefect]] @@ Take[{HoldForm[Global`i], HoldForm[Global`j], HoldForm[Global`k], HoldForm[Global`l]}, m] == 
   Structure[struct, $qdefect] @@ Take[{HoldForm[Global`i], HoldForm[Global`j], HoldForm[Global`k], HoldForm[Global`l]}, m],  
   {m, maxPts[$qdefect]}, {struct, allStructures[m, $qdefect]}]] /. (x_ == y_) :> conventionButton[x == y];
  
conventionsPanel[] := Row[{Column[Prepend[conventions[1], Style["General conventions", Bold, 16]], Spacings -> 2]//TraditionalForm, 
   Spacer[20], 
   Column[{Style["Spacetime structure building blocks", Bold, 16], Pane[Column[conventions[2]] //TraditionalForm, ImageSize -> {700, 500}, Scrollbars -> True]}, Spacings -> 1]
}, Alignment -> Top];

ExportResults[file_] := NotebookSave[resultsNotebook[], file];

resultsNotebook[] := CreateDocument[{
   Cell[TextData[{ButtonBox["SCWIGEE", BaseStyle->"Hyperlink", ButtonData->{URL["https://github.com/srossd/SCWIGEE"], None}, ButtonNote->"https://github.com/srossd/SCWIGEE"], " Results"}], "Title"],
   CellGroup[{
      TextCell["Conventions", "Chapter"],
   	  ExpressionCell[conventionsPanel[]]
   }, Closed],
   CellGroup[{
     TextCell["Operators", "Chapter"],
     TextCell["SUSY Multiplets", "Section"],
     TabView[
  	      Table[$multipletName[i] -> Column[{Framed@DisplayMultiplet[i, "EditMode" -> False]}, Alignment -> Center], {i, $multipletIndices}], 
       Dynamic[$viewingMultiplet], 
       Alignment -> Center, 
       ImageSize -> Automatic
     ],
     TextCell["SUSY Variations", "Section"],
   	 ExpressionCell[DisplaySUSYVariations[]],
   	 TextCell["Global Symmetry Invariants", "Section"],
   	 ExpressionCell[TableForm[
   	    Table[{TraditionalForm[Tensor[{c}]], Components[Tensor[{c}]]}, {c, 
   	       DeleteDuplicates@Cases[
			  Flatten@
			   Table[QAnsatz[op, "QBar" -> qb], 
			      {mult, $multipletIndices}, 
			      {op, Multiplet[mult]}, 
			      {qb, {False, True}}
			   ],
			  {"C", _, _, _},
			  All
		   ]
  	  }]
   	 ], "Output"]
   }, Closed],
   Cell["Correlators", "Chapter"],
   Cell[TextData[{
 "For  each  correlator, we  expand  into  invariant  tensors  of  the  global symmetry  and  into  conformally  invariant  spacetime  structures. The function g[",
 StyleBox["ops",
  FontSlant->"Italic"],
 ", ",
 StyleBox["i",
  FontSlant->"Italic"],
 ", ",
 StyleBox["j",
  FontSlant->"Italic"],
 "]  is  the  coefficient  of  the  ",
 StyleBox["i",
  FontSlant->"Italic"],
 "th  global  symmetry  invariant  times  the  ",
 StyleBox["j",
  FontSlant->"Italic"],
 "th spacetime structure."
}], "Text"],
   Sequence @@ Flatten[Table[
      {
      	Cell[BoxData[RowBox[{"\[LeftAngleBracket]",(StringJoin @@ (ToString[ToExpression[#], StandardForm] & /@ grp[[1,;;,1]])), "\[RightAngleBracket]"}]], "Section"],
      	CellGroup[{
      	   TextCell["Global Symmetry Invariants", "Subsection"],
      	   If[Length[grp[[1]]] == 4,
      	      CellGroup[{
	      	   	  TextCell["Contractions of three-point invariants: ", "Subsubsection"],
	      	      ExpressionCell[Module[{rrep = If[$qdefect =!= None, DefectGlobalRep, GlobalRep]},
	      	       Format[FourPtInvariantGraphs[rrep /@ grp[[1]]], TraditionalForm]
	      	      ], "Output"]
      	   	  }, Closed],
      	   	  Nothing
      	   ],
      	   CellGroup[{
      	   	  TextCell["Explicit components: ", "Subsubsection"],
      	      ExpressionCell[Module[{rreps = If[SCWIGEE`Private`$qdefect =!= None, DefectGlobalIndex[DefectGlobalRep[#], GlobalRep[SCWIGEE`Private`name2field[#[[1]]]]] & /@ grp[[1]], GlobalIndex@*GlobalRep /@ grp[[1]]]},
      	       Components /@ Switch[Length[grp[[1]]],
      	          	4, Table[Tensor[{{GlobalInvariant[i], Sequence @@ (Raised /@ rreps)}}], {i, numInvariants[rreps[[;;,1]]]}],
		           	3, {Tensor[{{"C", Sequence @@ (Raised /@ rreps)}}]},
		           	2, {Tensor[{{"\[Delta]", Sequence @@ (Raised /@ rreps)}}]},
		           	1, {1}
		      ]
      	      ], "Output"]
      	   }, Closed]
      	},Closed],
      	CellGroup[{
      	   TextCell["Spacetime Structures", "Subsection"],
      	   CellGroup[{
      	      TextCell["In terms of building block structures: ", "Subsubsection"],
      	   	  ExpressionCell[TraditionalForm[KinematicPrefactor[ScalingDimension /@ grp[[1]], Spin /@ grp[[1]], $qdefect] SpacetimeStructureExpressions[Spin /@ grp[[1]], $qdefect][[;;,1]]], "Output"]
      	   }, Closed],
      	   CellGroup[{
      	   	  TextCell["Explicit components: ", "Subsubsection"],
      	   	  TextCell["The spinor indices appear in the same order in which they appear in the correlator. X[i, j] denotes the jth coordinate of the ith operator position.", "Text"],
      	      ExpressionCell[If[ArrayQ[#], SparseArray[#], #] & /@ (Normal@*Components /@ SpacetimeStructures[ScalingDimension /@ grp[[1]], Spin /@ grp[[1]], {}, Range[Length[grp[[1]]]], $qdefect] /. SCWIGEE`x -> Global`X), "Output"]
      	   }, Closed]
      	},Closed],
      	Cell["Coefficients", "Subsection"],
      	Cell[BoxData[
			RowBox[{
			   	RowBox[{"coefficients", "[", "\"" <> (ToString[ToExpression[#], StandardForm] & /@ grp[[1,;;,1]]) <> "\"", "]"}],
			   	"=", 
      			ToBoxes[grp[[2]] 
      			   /. f: HoldPattern[Function[args_, _]] :> (f @@ args) 
      			   /. g[ops_, i_, j_] :> If[$qdefect === None || Max[Length /@ Values[branchingRules[]]] == 1,
      			      g[ToString[ToExpression[#, InputForm], StandardForm] & /@ ops[[;;, 1]], i,j],
      			      g[{ToString[ToExpression[#[[1]], InputForm], StandardForm], #[[2]]} & /@ ops, i,j]
      				 ]
      			   /. \[Lambda][ops_, i_, j_] :> \[Lambda][ToString[ToExpression[#, InputForm], StandardForm] & /@ ops[[;;, 1]], i,j]]
      		}]
      	], "Output"]
      }
      ,{grp, Normal@GroupBy[Normal[First /@ SolvedCorrelators[]], First@Cases[#, g[fields_, __] :> fields, All, Heads -> True] &]}]]
}];

preservedSusyPanel[q_] := Which[
   q === None,
   Nothing,
   
   q === 3,
   {
      {
	     Row[{
	        Column[{
	           Style["Preserved supercharge: ", 16],
	           Style[ToString[StringForm["``", defectSupercharge[3, "\[CurlyPhi]"]], TraditionalForm], 12]
	        }],
	        Style[" with \[CurlyPhi] = ", 16]
	     }],
	     Dynamic[InputField[Dynamic[$q3angle, {Automatic, (If[NumericQ[#], Set[$q3angle, #]]) &}], Enabled -> $editing, FieldSize -> {20, 1}]]
   	  }
   },
   
   q === 2,
   {
      {
	     Style["Supersymmetry type: ", 16], 
         Dynamic[RadioButtonBar[Dynamic[$q2type, {Automatic, (If[# == {4,0}, SetDefectGlobalSymmetry[SU2], SetDefectGlobalSymmetry[U1]]; Set[$q2type, #]) &}], 
            {
      			{4,0} -> Style["\[ScriptCapitalN] = (4,0)", 16], 
      			{2,2} -> Style["\[ScriptCapitalN] = (2,2)", 16]
         	}, 
         	Enabled -> $editing
         	]
         ]
      },
      If[$q2type === {4,0},
	      {
	         Column[{
	           Style["Preserved supercharge: ", 16],
	           Style[ToString[StringForm["``", defectSupercharge[2, {4, 0}, False]], TraditionalForm], 12]
	         }],
	     	 ""
	      },
	      {
	         Column[{
	           Style["Preserved supercharge: ", 16],
	           Style[ToString[StringForm["``", defectSupercharge[2, {2, 2}, False]], TraditionalForm], 12]
	         }],
	     	 ""
	      }
      ]
   },
   
   q === 1,
   {
     {
	     Row[{
	        Column[{
	           Style["Preserved supercharge: ", 16],
	           Style[ToString[StringForm["``", defectSupercharge[1, "\[CurlyPhi]"]], TraditionalForm], 12]
	        }],
	        Style[ToString[StringForm[" with \[CurlyPhi] = ", SU2BreakingTensor[$q1angle]], TraditionalForm], 16]
	     }],
	     Dynamic[InputField[Dynamic[$q1angle, {Automatic, (If[NumericQ[#], Set[$q1angle, #]]) &}], Enabled -> $editing, FieldSize -> {20, 1}, ImageSize -> 50]]
	 } 
   }   
]

wizardPanel[] := Panel[Dynamic[Grid[
   {
      {"", 
       "", 
       Style["Setup Wizard", 20], 
       Row[{Style["Editing: ", 16], 
       Spacer[5], 
       Checkbox[Dynamic[$editing]]}]
      }, 
      {"", 
       "", 
       Grid[
          {
             {
                Style["Signature: ", 14], 
      			Dynamic[RadioButtonBar[Dynamic[$signatureFactor], {1 -> Style["Lorentzian", 12], I -> Style["Euclidean", 12]}, Enabled -> $editing]]
      		 },
      		 {
      		    Style["Defect: ", 14], 
      		    Dynamic[RadioButtonBar[Dynamic[$qdefect], {
      		       None -> Style["None", 12], 
      		       1 -> Tooltip[Style["\!\(\*FormBox[\(q\), TraditionalForm]\) = 1", 12], "Defect codimension 1"], 
           		   2 -> Tooltip[Style["\!\(\*FormBox[\(q\), TraditionalForm]\) = 2", 12], "Defect codimension 2"], 
         		   3 -> Tooltip[Style["\!\(\*FormBox[\(q\), TraditionalForm]\) = 3", 12], "Defect codimension 3"]}, 
         		  Enabled -> $editing]
         		]
         	 }
         }, 
         Alignment -> Left
       ], 
      Dynamic[PopupWindow[Button["View Conventions"], conventionsPanel[], WindowSize -> {1100, 800}, WindowFloating -> False, WindowTitle -> "Conventions"]]
     }, 
     {
      "", 
      "", 
      Dynamic[
         Grid[
            {
               {
                  Tooltip[Style["Global symmetry: ", 16], "The R-symmetry, along with any additional symmetries. For instance, for an \[ScriptCapitalN] = 2 theory with SU(2) flavor symmetry, enter {SU2, SU2, U1}"], 
      			  Dynamic[InputField[Dynamic[$RSymmetry, {Automatic, SetGlobalSymmetry[#1] &}], Enabled -> ($multipletIndices === {} && $editing), FieldSize -> {20, 1}, ImageSize -> 50]],
      			  Style[If[$RSymmetry =!= Null && Head[$RSymmetry] =!= Symbol, CMtoName[$RSymmetry], ""], 14]
      		   },
        	   {
        	      Tooltip[Style["Supercharge representation: ",16], "The global symmetry representation of Q. For instance, for an \[ScriptCapitalN] = 2 theory with SU(2) flavor symmetry, and thus SU(2)_F \[Times] SU(2)_R \[Times] U(1)_R global symmetry, enter {1, 2, 1}."],
        	      Dynamic[InputField[Dynamic[$QGlobalRep, {Automatic, If[#1 =!= Null, SetQGlobalRep[#1]] &}], Enabled -> ($multipletIndices === {} && $editing), FieldSize -> {20, 1}, ImageSize -> 50]],
      			  Style[If[$RSymmetry =!= Null && $QGlobalRep =!= Null && Head[$RSymmetry] =!= Symbol && Head[$QGlobalRep] =!= Symbol, RepName[$RSymmetry, $QGlobalRep], ""], 14]
        	   },
        	   If[$qdefect =!= None, 
        	      {
        	         Style["Defect global symmetry: ",16],
        	         Dynamic[InputField[Dynamic[$DefectRSymmetry, {Automatic, SetDefectGlobalSymmetry[#1] &}], Enabled -> $editing, FieldSize -> {20, 1}, ImageSize -> 50]],
      			  	 If[$DefectRSymmetry =!= Null && Head[$DefectRSymmetry] =!= Symbol,
	      			   Button[Style[CMtoName[$DefectRSymmetry], 14], DialogInput[{embeddingSelector[$RSymmetry, $DefectRSymmetry], DefaultButton[]}], Method -> "Queued"],
      			  	   ""
      			  	 ]
        	      },
        	   	  Nothing
        	   ],
        	   If[$DefectRSymmetry =!= Null && $RSymmetry =!= Null && $embedding =!= Null && Head[$RSymmetry] =!= Symbol && Head[$DefectRSymmetry] =!= Null && Head[$embedding] =!= Null,
        	      {
        	         "",
        	         displayBranching[$RSymmetry, $DefectRSymmetry, $embedding],
        	         SpanFromLeft
        	      },
        	      Nothing  
        	   ]
        	}, 
        	Alignment -> Right
        ]
     ], 
     Dynamic[
        If[
           Head[$QTensor] =!= Tensor, 
           "", 
           Row[{Spacer[20], Framed@Graphics[{AbsoluteThickness[1], Arrowheads[.1], Arrow[{{0, 0}, {-1,-.75}}], Text[Style[TraditionalForm[$QTensor], 14], {-.35,-.6}]}, ImageSize -> 75]}]
        ]
     ], 
     ""
    }, 
	If[$qdefect =!= None && GlobalSymmetry[] =!= Null && DefectGlobalSymmetry[] =!= Null && (SubsetQ[GlobalSymmetry[], {SU2, U1}] && Sort[DeleteElements[GlobalSymmetry[], 1 -> {SU2}]] === Sort[DefectGlobalSymmetry[]] && $QGlobalRep =!= Null),
	   {"", "", Grid[preservedSusyPanel[$qdefect]], ""},
	   Nothing
	],
    {
     "", 
     "", 
     Dynamic[
        If[Length[$multipletIndices] == 0, 
           Style["Set the global symmetry, and then add a multiplet.", 11, Italic], 
      	   TabView[
      	      Table[
      	         $multipletName[i] -> 
         			Column[{
         			   Framed@DisplayMultiplet[i, "EditMode" -> $editing], 
           			   Button[Style["Copy as TeX", 14], CopyToClipboard[tikzMultiplet[$viewingMultiplet]], ImageSize -> 200],
           			   Button[Style["Write Code to Cell", 14], 
                   		CellPrint[
                   		   Cell[
                   		      BoxData[
 								RowBox[{"SetMultiplet", "[", 
  									RowBox[{ToBoxes[Multiplet[$viewingMultiplet]], ",", "\""<>ToString[$multipletName[$viewingMultiplet]]<>"\"", ",", $multipletSC[$viewingMultiplet], ",", $viewingMultiplet}], 
  								"]"}]
  							  ], 
  							"Input"
  						   ]
  						], 
                       ImageSize -> 200]
                     }, 
          	         Alignment -> Center
          	       ], 
          	 {i, $multipletIndices}], 
          Dynamic[$viewingMultiplet], 
          Alignment -> Center, 
          ImageSize -> Automatic
          ]
        ]
     ], 
     Button["Add Multiplet", 
     	With[{res = multipletDialog[], new = If[Length[$multipletIndices] == 0, 0, Max[$multipletIndices]] + 1}, 
           If[Length[res] === 2, 
              AppendTo[$multipletIndices, new];
       		  $multiplet[new] = If[res[[2]], {}, {{}, {}}];
       		  $multipletName[new] = res[[1]];
       		  $multipletSC[new] = res[[2]];
       	   ]
       	], 
       	Method -> "Queued", 
     	ImageSize -> {Automatic, 30}, 
     	Enabled -> Dynamic[$RSymmetry =!= Null]
     ]
    },
    If[Length[SolvedCorrelators[]] == 0, 
       Nothing, 
          {
             "", 
             "", 
      		 Style[ToString@StringForm["Progress: `` correlators computed", Length[GroupBy[Normal[First /@ SolvedCorrelators[]], #[[1,1]] &]]], 16],
      		 Style[Button["Write Results Notebook", resultsNotebook[]], DynamicEvaluationTimeout -> Infinity]
      	  }
     ]
    }, 
    Alignment -> Center, 
  	Spacings -> {Automatic, 2}, 
  	Dividers -> {True, All}]], 
    Background -> Lighter[Gray, 0.9]
];

If[!$consoleMode,
	wizardCell = CellPrint[ExpressionCell[SCWIGEE`Private`wizardPanel[], TextAlignment -> Center]]
];