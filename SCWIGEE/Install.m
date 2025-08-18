InstallSCWIGEE::version="Warning: The package structure of `1` is only supported by Mathematica versions \[GreaterEqual] `2`. You are using Mathematica `3`.";

InstallGroupMath[]:= Module[{
        pkgDir= FileNameJoin[{$UserBaseDirectory, "Applications", "GroupMath"}],
        pkgLink= "https://renatofonseca.net/groupmath/ProgramVersions/GroupMath-1.1.2.zip",
        pkgName= "GroupMath",
        proceed = True,
        questionOverwrite, tmpFile, unzipDir, zipDir},

	(* Messages *)
	questionOverwrite= pkgName<> " is already installed. Do you want to replace the content of "<> pkgDir<> " with a newly downloaded version?";

	(* Check if GroupMath has already been installed *)
	If[
		DirectoryQ[pkgDir],
		If[
			ChoiceDialog[questionOverwrite, {"Yes"-> True, "No"-> False},
                WindowFloating-> True,
                WindowTitle-> pkgName<> " installation detected"],
			Quiet@ DeleteDirectory[pkgDir, DeleteContents-> True],
			proceed = False
		];
	];

	If[proceed,
		(* Download GroupMath *)
		Print["Downloading "<> pkgName<> " from ", pkgLink<> "."];
	
		tmpFile= Quiet@ URLSave[pkgLink];
	
		If[tmpFile=== $Failed,
			Print["Failed to download "<> pkgName<> ".\nInstallation aborted!"];
			Abort[]
		];
	
		(* Unzip GroupMath file *)
		Print["Extracting "<> pkgName<> " zip file."];
	
		unzipDir= tmpFile<>".dir";
		ExtractArchive[tmpFile, unzipDir];
	
		(* Move files to the Mathematica packages folder *)
		Print["Copying "<> pkgName<> " to "<> pkgDir<> "."];
	
		zipDir= FileNames["GroupMath.m", unzipDir, Infinity];
		CopyDirectory[DirectoryName[zipDir[[1]], 1], pkgDir];
	
		(* Delete the extracted archive *)
		Quiet@ DeleteDirectory[unzipDir, DeleteContents-> True];
	];
	Print["Installation complete!"];
];


InstallSCWIGEE[]:= Module[{
        pkgDir= FileNameJoin[{$UserBaseDirectory, "Applications", "SCWIGEE"}],
        pkgLink= "https://github.com/srossd/SCWIGEE/archive/main.zip",
        pkgName= "SCWIGEE",
        minVersion= 9.0,
        proceed = True,
        questionOverwrite, tmpFile, unzipDir, zipDir},

	
	Print[Style["Checking TensorTools dependency...",Bold]];
	Get["https://raw.githubusercontent.com/srossd/TensorTools/main/Install.m"];
	Print[Style["TensorTools dependency resolved", Bold]];

	Block[{$checkedTensorTools = True},
	Print[Style["Checking ConformalStructures dependency...",Bold]];
	Get["https://raw.githubusercontent.com/srossd/ConformalStructures/main/Install.m"];
	Print[Style["ConformalStructures dependency resolved", Bold]];
	];
	
	Print[Style["Checking GroupMath dependency...",Bold]];
	InstallGroupMath[];
	Print[Style["GroupMath dependency resolved",Bold]];

	(* Messages *)
	questionOverwrite= pkgName<> " is already installed. Do you want to replace the content of "<> pkgDir<> " with a newly downloaded version?";

	(* Check Mathematica version *)
	If[$VersionNumber< minVersion,
		Message[InstallSCWIGEE::version, pkgName, ToString@ minVersion, $VersionNumber];
	];

	(* Check if SCWIGEE has already been installed *)
	If[
		DirectoryQ[pkgDir],
		If[
			ChoiceDialog[questionOverwrite, {"Yes"-> True, "No"-> False},
                WindowFloating-> True,
                WindowTitle-> pkgName<> " installation detected"],
			Quiet@ DeleteDirectory[pkgDir, DeleteContents-> True],
			proceed = False
		];
	];

	If[proceed,
		(* Download SCWIGEE *)
		Print["Downloading "<> pkgName<> " from ", pkgLink<> "."];
	
		tmpFile= Quiet@ URLSave[pkgLink];
	
		If[tmpFile=== $Failed,
			Print["Failed to download "<> pkgName<> ".\nInstallation aborted!"];
			Abort[]
		];
	
		(* Unzip SCWIGEE file *)
		Print["Extracting "<> pkgName<> " zip file."];
	
		unzipDir= tmpFile<>".dir";
		ExtractArchive[tmpFile, unzipDir];
	
		(* Move files to the Mathematica packages folder *)
		Print["Copying "<> pkgName<> " to "<> pkgDir<> "."];
	
		zipDir= FileNames["Utilities.m", unzipDir, Infinity];
		CopyDirectory[DirectoryName[zipDir[[1]], 1], pkgDir];
	
		(* Delete the extracted archive *)
		Quiet@ DeleteDirectory[unzipDir, DeleteContents-> True];
		
		Quiet[ResourceFunction["MonitorProgress"]];
	];
	Print["Installation complete!"];
];

InstallSCWIGEE[];