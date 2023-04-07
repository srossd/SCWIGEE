InstallSCWIGE::version="Warning: The package structure of `1` is only supported by Mathematica versions \[GreaterEqual] `2`. You are using Mathematica `3`.";


InstallSCWIGE[]:= Module[{
        pkgDir= FileNameJoin[{$UserBaseDirectory, "Applications", "SCWIGE"}],
        pkgLink= "https://github.com/srossd/SCWIGE/archive/main.zip",
        pkgName= "SCWIGE",
        minVersion= 9.0,
        questionOverwrite, tmpFile, unzipDir, zipDir},

	
	Print[Style["Checking TensorTools dependency...",Bold]];
	Get["https://raw.githubusercontent.com/srossd/TensorTools/main/Install.m"];
	Print[Style["Dependency resolved", Bold]];

	(* Messages *)
	questionOverwrite= pkgName<> " is already installed. Do you want to replace the content of "<> pkgDir<> " with a newly downloaded version?";

	(* Check Mathematica version *)
	If[$VersionNumber< minVersion,
		Message[InstallSCWIGE::version, pkgName, ToString@ minVersion, $VersionNumber];
	];

	(* Check if SCWIGE has already been installed *)
	If[
		DirectoryQ[pkgDir],
		If[
			ChoiceDialog[questionOverwrite, {"Yes"-> True, "No"-> False},
                WindowFloating-> True,
                WindowTitle-> pkgName<> " installation detected"],
			Quiet@ DeleteDirectory[pkgDir, DeleteContents-> True],
			Abort[]
		];
	];

	(* Download SCWIGE *)
	Print["Downloading "<> pkgName<> " from ", pkgLink<> "."];

	tmpFile= Quiet@ URLSave[pkgLink];

	If[tmpFile=== $Failed,
		Print["Failed to download "<> pkgName<> ".\nInstallation aborted!"];
		Abort[]
	];

	(* Unzip SCWIGE file *)
	Print["Extracting "<> pkgName<> " zip file."];

	unzipDir= tmpFile<>".dir";
	ExtractArchive[tmpFile, unzipDir];

	(* Move files to the Mathematica packages folder *)
	Print["Copying "<> pkgName<> " to "<> pkgDir<> "."];

	zipDir= FileNames["SCWIGE.m", unzipDir, Infinity];
	CopyDirectory[DirectoryName[zipDir[[1]], 1], pkgDir];

	(* Delete the extracted archive *)
	Quiet@ DeleteDirectory[unzipDir, DeleteContents-> True];
	Print["Installation complete!"];
];

InstallSCWIGE[];