// Install and use FsCheck from a .fsx script file        Michael R. Hansen 08-09-2022
//
// You must refer to the FsCheck package before you can use it

// The following directive refers to FsCheck and should install the package 
// if it is not already installed

#r "nuget: FsCheck";;


// Note that you can always install FsCheck from: https://www.nuget.org/packages?q=FsCheck
// and refer using a full path to the assembly: FsCheck.dll
// as follows:                               -- revise path properly
// #r @"C:\Users\mire\.nuget\packages\fscheck\2.16.3\lib\net452\FsCheck.dll"
// 
// or 
// #I @"C:\Users\mire\.nuget\packages\fscheck\2.16.3\lib\net452"
// #r @"FsCheck.dll"


open FsCheck;;

let rec sumA xs a =  
     match xs with 
     | [] -> a
     | x::tail -> sumA tail (x+a);;

let prop1 xs = List.sum xs = sumA xs 0;;

let t1 = FsCheck.Check.Quick prop1;;   

let t2 = Check.Quick prop1;; 

let t3 = Check.Verbose prop1;;
