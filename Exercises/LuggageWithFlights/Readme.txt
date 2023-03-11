// Michael R. Hansen 27-10-2022

The three-project solution is formed using the following 
dotnet CLI commands:

dotnet new sln -o LuggageWithFlights

in folder LuggageWithFlights:

dotnet new classlib -lang F# -o src\Library
dotnet new xunit -lang F# -o src\FunctionalTests
dotnet new xunit -lang F# -o src\StructuralTests

in the two folders FunctionalTests and StructuralTests:
dotnet add reference ..\Library\Library.fsproj

To run the tests you issue the command: 
      dotnet test
in the folders FunctionalTests and StructuralTests
