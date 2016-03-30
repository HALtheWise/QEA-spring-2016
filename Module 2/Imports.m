(* ::Package:: *)

(* ::Section:: *)
(*Importing faces*)


SetDirectory[NotebookDirectory[]];
imageDirectory="P:\\+Courses\\QEA Spring 2016\\Module 2\\database\\database";
imageFiles=FileNames["*.png",imageDirectory];


images=Import[FileNameJoin[{ParentDirectory[],"data","classdata.mat"}]][[1]];


me = images[[82]];


(* ::Subsection::Closed:: *)
(*Names*)


names=ImportString["Ariana
Bryan 
Charlie
Chloe
Chris
Danny
David
Dhash
Diego
Emily 
Eric
Gwen
Harper
Isaac 
Izzy 
Jared
John Geddes
John
Jonah
Kaitlyn 
Kevin
Lauren
Joseph 
Lydia
Margo
Mark
Mary 
Max Wei
Mica
Nathan
Paige
Rebecca
Regina 
Ruby
Sam
Sarah
Sean
Siddhartan 
Min
Sung
Taylor
Uma
Willem","Table"][[All,1]];


namesTable[]:=namesTable[]=Grid[Transpose[{names,Image/@images[[Range[1,Length@images,8]]],Range[1,Length@images,8]}],Frame->All]


(* ::Section:: *)
(*Correlation*)


Clear@correlation
correlation[a_]:=Module[{o,m,s,b,c},
o=ConstantArray[{1},Length@a];
m=o.{Mean[a]};
s=o.{StandardDeviation[a]};
b=(a-m)/s;
c=(1/(Length@a-1))*Transpose[b].b
]
