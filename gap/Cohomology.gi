#################################
##
## Attributes
##
#################################


#################################
##
## Methods for truncation
##
#################################


# this reads a file of integers into a list of lists of integers supported in homalg
# every sublist corresponds to one line of the file
InstallMethod( ReadIntegerListsFromFile,
               " for existing files of integers ",
               [ IsDirectory, IsString ],
function( directory, name )

local l,f,i,a,r;

f:=InputTextFile( Filename( directory, name ) );
a:=[];
while not IsEndOfStream(f) do
   l:=ReadLine(f);
   if l<>fail then
     l:=Chomp(l); # remove trailing CR/LF
     r:=[];
     for i in SplitString(l," ,") do # separate by SPACE or ,
       if Length(i)>0 then
         Add(r,Int(i));
       fi;
     od;
     Add(a,r);
   fi;
 od;
 CloseStream(f);    # <== New line
return a;

end );


# This here is part of the interface to normaliz (faster computation than with polymake)
# It computes the exponents of the monoms in the degree part of the cox ring and saves them to a separate file
# Once the exponents are computed once, we read the content of the respective file to avoid the computation
# Unfortunately that saves about no time so far... :(

# The Normaliz folder is located right below the ToricVarieties folder, its path is hard coded - so this folder cannot be moved

InstallMethod( Exponents,
               "for a toric variety and a list describing a degree",
               [ IsToricVariety, IsList ],
function( variety, degree )

local s, d, file, rays, divisor, otf, otf2, R, A, i, j, n, myList, myList2, exponents;

# check if the variety has a name
if not HasNameOfVariety( variety ) then

   Error( "Variety needs to have a name, so the computed output can be saved!" );

fi;

# get the directory in which Normaliz can be found
# this is where we want to store the .exp files/ read from these files
# need a more proper treatment, maybe set the package path on startup
d := Concatenation( PackageInfo( "ToricVarieties")[1].InstallationPath, "/Normaliz/" );

# create the name of the file
s := Concatenation( NameOfVariety( variety ) , String( degree ) );
RemoveCharacters( s, " " );
file := Filename( Directory( d ), Concatenation( s, ".in" ) );

# check if the file already exists - if it does no computation is needed, otherwise perform the computation just once
# and save the result for future use
myList2 := [];
if not IsExistingFile( file ) then

   # unfortunately the file does not yet exist, so we need to create it and apply normaliz to it

   # write the polytope to a file Test.in
   divisor := DivisorOfGivenClass( variety, degree );

   # extract the "coordinate" of this divisor in the Weil group
   A := UnderlyingListOfRingElements( UnderlyingGroupElement( divisor ) );
   # IS THIS ALWAYS CORRELATED WITH THE ORDER IN WHICH THE RAY GENERATORS ARE LISTED IN R BELOW

   # and the ray generators
   R := RayGenerators( FanOfVariety( variety ) );

   # write input file for normaliz
   otf := OutputTextFile( file, false );
   SetPrintFormattingStatus( otf, false );
   AppendTo( otf, Length( R ) );
   AppendTo( otf, "\n" );
   AppendTo( otf, Length( R[1] ) + 1 );
   AppendTo( otf, "\n" );
   for i in [1..Length( R ) ] do
	for j in [1..Length( R[i] )] do
		AppendTo( otf, R[i][j] );
		AppendTo( otf, " " );
	od;
	AppendTo( otf, A[i] );
	AppendTo( otf, "\n" );
   od;
   AppendTo( otf, "inequalities \n" );
   AppendTo( otf, "1 \n" );
   AppendTo( otf, Length( R[1] ) + 1 );
   AppendTo( otf, "\n" );
   for i in [1..Length( R[1] ) ] do
	AppendTo( otf, "0 ");
   od;
   AppendTo( otf, "1 \n" );
   AppendTo( otf, "grading" );
   CloseStream( otf );

   # and let normaliz operate on this file
   Exec( Concatenation( d, "normaliz -1 -a ", d, s ) );

   # suppose normaliz can't operate on the above input file, then the polytope probably contains no lattice points at all
   # unfortunately, instead of returning "nothing" it raises an error
   # so we need to check if such a situation occured, and if it did we just use the slower version "LatticePoints" that uses
   # the polymake interface - for no lattice points to be returned I hope however that it will still be fast enough

   # this is the output file that normaliz should have produced
   file := Filename( Directory( d ), Concatenation( s, ".ht1" ) );

   # check if this file exists
   if IsExistingFile( file ) then
      
      # normaliz did operate sucessfully, so just read from its output file
      myList := ReadIntegerListsFromFile( Directory( d ), Concatenation( s, ".ht1" ) );

      # and slightly manipulate it by first removing the first two lists...
      Remove( myList, 1 );
      Remove( myList, 1 );

      # ...and then remove from all other lists the very last entry, which is redundant output from Normaliz
      for i in [1..Length( myList )] do
          Remove( myList[i], Length( myList[i] ) );
      od;

   else

      # normaliz failed and thus gave us lovely output
      # anyways, we now use our slow fallback method
      myList := LatticePoints( PolytopeOfDivisor( DivisorOfGivenClass( variety, degree ) ) );

   fi;

   # now the content of myList is suitable for use within homalg
   # we use it to compute the exponents of the monomials and save those exponents in the corresponding .exp file for simple handeling
   
   # collect the necessary information
   rays := RayGenerators( FanOfVariety( variety ) );
   divisor := UnderlyingListOfRingElements( UnderlyingGroupElement( DivisorOfGivenClass( variety, degree ) ) );    
   n := Length( rays );
   
   # compute the exponents
   myList2 := [];
   for i in myList do

       exponents := List( [1..n], j -> divisor[j] + Sum( List( [1..Length(i)], m -> rays[j][m] * i[m]) ) );

       Add( myList2, exponents );

   od;
   
   # now write myList2 to the .exp file so that we can easily read this file with gap
   file := Filename( Directory( d ), Concatenation( s, ".exp" ) );

   # start the writing process
   otf2 := OutputTextFile( file, false );
   SetPrintFormattingStatus( otf2, false );
   for i in myList2 do
       for j in i do
           AppendTo( otf2, String( j ) );
           AppendTo( otf2, " " );
       od;
       AppendTo( otf2, "\n" );
   od;

   # close the stream
   CloseStream( otf2 );
   
else

   # the computation has been done before
   # so just read from the produced output file
   myList2 := ReadIntegerListsFromFile( Directory( d ), Concatenation( s, ".exp" ) );

fi;

# and return the exponents
return myList2;

end );


# this method computes the Laurent monomials of the lattice points and thereby returns the monoms of given degree within the Coxring
InstallMethod( MonomsOfCoxRingOfDegreeByNormaliz,
               "for a smooth and compact toric variety and a list describing a degree in its class group",
               [ IsToricVariety, IsList ],
function( variety, degree )

    local cox_ring, ring, exponents, i,j, mons, mon;
    
    if not IsComplete( variety ) then

       Error( "Method only applicable to compact varieties" );

    elif not IsSmooth( variety) then
   
       Error( "Method only applicable to smooth varieties" );

    elif not HasCoxRing( variety ) then
        
        Error( "specify cox ring first\n" );

    elif not Length( degree ) = Rank( ClassGroup( variety ) ) then

        Error( "Length of degree does not match the rank of the class group" );
        
    fi;
    
    # collect the necessary information
    cox_ring := CoxRing( variety );
    ring := ListOfVariablesOfCoxRing( variety );
    exponents := Exponents( variety, degree );
    
    # initialise the list of monoms
    mons := [ ];
    
    # turn the lattice points into monoms of the cox_ring
    for i in exponents do
        
        mon := List( [ 1 .. Length( ring ) ], j -> JoinStringsWithSeparator( [ ring[ j ], String( i[j] ) ], "^" ) );
        mon := JoinStringsWithSeparator( mon, "*" );
        
        Add( mons, HomalgRingElement( mon, cox_ring ) );
        
    od;
    
    # and return this result
    return mons;
    
end );


# Compute basis of the degree X part of the Coxring of a toric variety (smooth and compact will be assumed)
# probably not needed
InstallMethod( DegreeXPart,
               " for toric varieties, a list specifying a degree ",
               [ IsToricVariety, IsList ],
function( variety, degree )

if not Length( degree ) = Rank( ClassGroup( variety ) ) then

   Error( "Length of degree does not match the rank of the class group." );

elif not HasCoxRing( variety ) then

   Error( "Specify Coxring first!" );

elif not HasNameOfVariety( variety ) then

   Error( "The variety needs a name, so the produced output can be saved." );

fi;

# return the output
return MonomsOfCoxRingOfDegreeByNormaliz( variety, degree );

end );


# a small helper function
InstallMethod( replacer,
               "to help me.",
               [ IsInt, IsInt, IsRingElement ],
function( int, index, mon )

   if not int = index then
      return 0;
   else
      return mon;
   fi;
end );


# represent the degreeX part of a line bundle as "matrices" of length 'length' and the corresponding monoms at position 'index'
InstallMethod( DegreeXPartVects,
               " for toric varieties, a list specifying a degree, a positie integer",
               [ IsToricVariety, IsList, IsPosInt, IsPosInt ],

function( variety, charges, index, length )

local gens, gens2;

if not Length( charges ) = Rank( ClassGroup( variety ) ) then

   Error( "Length of charges does not match the rank of the class group." );

elif not HasCoxRing( variety ) then

   Error( "Specify Coxring first!" );

else

      	# compute Q-Basis of its global sections
      	gens := MonomsOfCoxRingOfDegreeByNormaliz( variety, charges );

        # now represent these as matrices of length 'length' which contain zero but at position 'index' where we place the monoms
        gens2 := List( [1.. Length( gens )], x -> HomalgMatrix( [ List( [1..length], y -> replacer( y, index, gens[x] ) ) ], CoxRing( variety ) ) );

      	return gens2;

fi;

end);

# represent the degreeX part of a line bundle as lists of length 'length' and the corresponding monoms at position 'index'
InstallMethod( DegreeXPartVectsII,
               " for toric varieties",
               [ IsToricVariety, IsList, IsPosInt, IsPosInt ],
function( variety, charges, index, length )

local gens, gens2;

if not Length( charges ) = Rank( ClassGroup( variety ) ) then

   Error( "Length of charges does not match the rank of the class group." );

elif not HasCoxRing( variety ) then

   Error( "Specify Coxring first!" );

else

      	# compute Q-Basis of its global sections
      	gens := MonomsOfCoxRingOfDegreeByNormaliz( variety, charges );

        # now represent these as lists of length 'length' which contain zero but at position 'index' where we place the monoms
        # that form a Q-basis of the corresponding degreeXpart
        gens2 := List( [1.. Length( gens )], x -> List( [1..length], y -> replacer( y, index, gens[x] ) ) );

      	return gens2;

fi;

end);


# compute degree X part of a free graded S-module
InstallMethod( DegreeXPartOfFreeModule,
               " for toric varieties, a free graded S-module, a list specifying a degree ",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ],
function( variety, module, degree )

local list, i, gens, help;

      if not IsFree( module ) then
          
         Error( "This module is not free." );

      elif not Length( degree ) = Rank( ClassGroup( variety ) ) then

            Error( "Length of degree does not match rank of class group of the variety." );

      elif not HasCoxRing( variety ) then

            Error( "Specify Coxring first!" );

      fi;

      # compute the degree layers of S that need to be computed
      list := DegreesOfGenerators( module );
      list := List( [1..Rank( module)], x -> degree - UnderlyingListOfRingElements( list[x] ) );

      # concatenate the lists of basis elements to have basis of the degreeX part of the free module M
      gens := DegreeXPartVects( variety, list[1], 1, Rank(module) );
             
      # check if gens is not the zero vector space
      # for if that is the case, then the module M has no degreeX part, so we should return the empty list
      if not gens = [] then

         for i in [2.. Length( list )] do

                 # check if DegreeXPartVects( variety, list[i], i, Rank( module ) ) is not the zero vector space
                 # for if that is the case, then the module has no degreeX part, so we should return the empty list
                 help := DegreeXPartVects( variety, list[i], i, Rank(module) );
                 if help = [] then
                    return [];
                    break;
                 else
                    gens := Concatenation( gens, help );
                 fi;

         od;

      fi;

      # return the resulting list
      return gens;

end);


# Compute degree X part of a graded free S-module as Q-vector space
InstallMethod( DegreeXPartOfFreeModuleAsVectorSpace,
               " for toric varieties, a free graded S-module, a list specifying a degree ",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ],
function( variety, module, degree )

local list, i, gens, help, dim, Q;

      if not IsFree( module ) then
          
         Error( "This module is not free." );

      elif not Length( degree ) = Rank( ClassGroup( variety ) ) then

            Error( "Length of degree does not match rank of class group of the variety." );

      elif not HasCoxRing( variety ) then

            Error( "Specify Coxring first!" );

      else

             # compute the underlying coefficient ring
             Q := UnderlyingNonGradedRing( CoefficientsRing( HomalgRing( module ) ) );

             # compute the degree layers of S that need to be computed
             list := DegreesOfGenerators( module );
             list := List( [1..Rank( module)], x -> degree - UnderlyingListOfRingElements( list[x] ) );

             # concatenate the lists of basis elements to have basis of the degreeX part of the free module M
             gens := DegreeXPart( variety, list[1] );
             
             # check if gens is not the zero vector space
             # for if that is the case, then the module M has no degreeX part, so we should return the empty list
             if gens = [] then
                dim := 0;
             else
                dim := Length( gens );

             for i in [2.. Length( list )] do

                 # check if DegreeXPartVects( variety, list[i], i, Rank( module ) ) is not the zero vector space
                 # for if that is the case, then the module has no degreeX part, so we should return the empty list
                 help := DegreeXPart( variety, list[i] );
                 if help = [] then
                    dim := 0;
                    break;
                 else
                    dim := dim + Length( help );
                 fi;

             od;

             fi;

             # return the dimension of this Q-vector space
             return RowSpace( Q, dim );

      fi;

end);


# compute DegreeXPart of a free graded S-module as a matrix in whose rows we write the generators of the degree X part
InstallMethod( DegreeXPartOfFreeModuleAsMatrix,
               " for toric varieties, a free graded S-module, a list specifying a degree",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ],
function( variety, module, degree )

local gens, list, help, i, matrix;

      if not IsFree( module ) then
          
         Error( "This module is not free." );

      elif not Length( degree ) = Rank( ClassGroup( variety ) ) then

            Error( "Length of degree does not match rank of class group of the variety." );

      elif not HasCoxRing( variety ) then

            Error( "Specify Coxring first!" );

      else

             # compute the degree layers of S that need to be computed
             list := DegreesOfGenerators( module );
             list := List( [1..Rank( module)], x -> degree - UnderlyingListOfRingElements( list[x] ) );

             # concatenate the lists of basis elements to have basis of the degreeX part of the free module M
             gens := DegreeXPartVectsII( variety, list[1], 1, Rank(module) );
             
             # check if gens is not the zero vector space
             # for if that is the case, then the module M has no degreeX part, so we should return the empty list
             if gens = [] then
                return [];
             else

             for i in [2.. Length( list )] do

                 # check if DegreeXPartVects( variety, list[i], i, Rank( module ) ) is not the zero vector space
                 # for if that is the case, then the module has no degreeX part, so we should return the empty list
                 help := DegreeXPartVectsII( variety, list[i], i, Rank(module) );
                 if help = [] then
                    return [];
                    break;
                 else
                    gens := Concatenation( gens, help );
                 fi;

                 # if the degree X layer is non-trivial, we should now have a neat list of its generators saved in gens
                 # we turn it into a nice matrix
                 matrix := HomalgMatrix( gens, HomalgRing( module ) );

                 # then hand this matrix back
                 return matrix;

             od;

             fi;

      fi;

end);


# a method that unifies a list of rows-matrices into a single matrix
InstallMethod( UnionOfRows,
               "for list of row matrices",
               [ IsList ],
function( ListOfMatrices )

local resmat,i;

      resmat := ListOfMatrices[1];
      for i in [2..Length( ListOfMatrices ) ] do

          resmat := UnionOfRows( resmat, ListOfMatrices[i] );

      od;

return resmat;

end);


# compute the degree X part of a f.p. graded S-module
InstallMethod( DegreeXPartOfFPModule,
               " for toric varieties, a f.p. graded S-module, a list specifying a degree",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ],
function( variety, module, degree )

local h, M1, M2, gens1, gens2, rows, matrix, map, Q;

      # reduce the work amount by choosing a smaller presentation
      ByASmallerPresentation( module );

      # check if this is a right-module, if so turn it into a left-module
      if not IsHomalgLeftObjectOrMorphismOfLeftObjects( module ) then
         module := GradedHom( HomalgRing( module) * 1, module );
      fi;

      # determine the underlying ring
      Q := UnderlyingNonGradedRing( CoefficientsRing( HomalgRing( module ) ) );

      # check for simple cases, to reduce the work amount
      if IsZero( module ) then

         return RowSpace( Q, 0 );

      elif IsFree( module ) then

         return DegreeXPartOfFreeModuleAsVectorSpace( variety, module, degree );

      else

          # the module is not zero nor free, so extract the presentation morphism h
          h := ByASmallerPresentation( PresentationMorphism( module ) );

          # source and range of h give the source and range free modules
          M1 := Source( h );
          M2 := Range( h );

          # compute Q-basis of the degree 'charges' layers of M1 and M2
          gens1 := DegreeXPartOfFreeModule( variety, M1, degree );
          gens2 := DegreeXPartOfFreeModuleAsMatrix( variety, M2, degree );
          
          # check for degenerate cases
          if gens1 = [] and gens2 = [] then

             # gens1 and gens2 is the zero vector space, so cokernel is isomorphic to the zero vector space too
             return RowSpace( Q, 0 );

          elif gens1 = [] and not gens2 = [] then

             # gens1 is the zero vector space, so cokernel is isomorphic to gens2
             return RowSpace( Q, NrRows( gens2 ) );

          elif gens2 = [] then

             # gens2 is the zero vector space, so the cokernel is the zero vector space as well
             return RowSpace( Q, 0 );

          else

	       # no degenerate case, so do the full computation

	       # compute the images of gens1 in the range of h expressed in terms of gens2 (that is what I use gens2 as matrix for)
               # this needs "applyMorphismToElement"
               # try to find work around
               #rows := List([1..Length(gens1)], x -> RightDivide( MatrixOfMap( UnderlyingMorphism( MyApplyMorphismToElement( h, gens1[x] ) ) ), gens2 ) );
               rows := List([1..Length(gens1)], x -> RightDivide( gens1[x] * MatrixOfMap( h ), gens2 ) );

               #-> rows=list of matrices s.t. each of these matrices is a row of the rep. matrix of the degreeXpart of the morphism h
               # we need to "compose" these matrices, to form the final matrix
               matrix := UnionOfRows( rows );

               # now turn matrix into a matrix over Q
               matrix := Q * matrix;

               # use matrix to define a map of finite dimensional Q-vector spaces
               map := HomalgMap( matrix, RowSpace( Q, NrRows( matrix ) ), RowSpace( Q, NrColumns( matrix ) ) );

               # the cokernel of map is (isomorphic to) the degree X part of module, so return this
               return ByASmallerPresentation( Cokernel( map ) );

          fi;

      fi;

end );


#################################
##
## Methods for B-transform
##
#################################



# STEP 6:
# STEP 6:
# Compute H^0 from the B-transform

# compute H^0 from the B-transform
InstallMethod( H0FromBTransform, 
               " for toric varieties, a f.p. graded S-module, a non-negative integer",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsInt ],
function( variety, module, index )

local B, BPower, GH, zero;

# check if the input is valid
if index < 0 then
   Error( "Index must be a non-negative integer." );
fi;

# generate the input
B := IrrelevantIdeal( variety );
BPower := GradedLeftSubmodule( List( EntriesOfHomalgMatrix( MatrixOfSubobjectGenerators( B ) ), a -> a^(index) ) );	
GH := ByASmallerPresentation( GradedHom( BPower, module ) );
zero := List( [1..Rank( ClassGroup( variety ) )], x -> 0 );

# compute the degree 0 part of the GradedHom under consideration
return DegreeXPartOfFPModule( variety, GH, zero );

end);

# compute H^0 from the B-transform
InstallMethod( H0FromBTransformInInterval, 
               " for toric varieties, a f.p. graded S-module, a non-negative integer",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsInt, IsInt ],
function( variety, module, min, max )

local results;

# check if the input is valid
if min < 0 then
   Error( "min must not be negative." );
elif not min <= max then
   Error( "max must not be smaller than min" );
fi;

# compute the results
results := List( [min..max], x -> H0FromBTransform( variety, module, x ) );

# compute the degree 0 part of the GradedHom under consideration
return results;

end);



#######################################
##
## Methods to apply theorem by G. Smith
##
#######################################



InstallMethod( VToH, 
               " for list of lists of integers.",
               [ IsList ],
function( ListOfVertices )

local d, file, otf, itf, i, j, output, N, l, r;

# extract the normaliz directory
d := Concatenation( PackageInfo( "ToricVarieties")[1].InstallationPath, "/Normaliz/" );

# fix filename
file := Filename( Directory( d ), "VToH.in" );

# write input file for normaliz (overwrite existing file)
otf := OutputTextFile( file, false );
SetPrintFormattingStatus( otf, false );

# write information about how many vertices of which length will be written
AppendTo( otf, Length( ListOfVertices ) );
AppendTo( otf, "\n" );
AppendTo( otf, Length( ListOfVertices[1] ) );
AppendTo( otf, "\n" );

# now write the vertices to the input file
for i in [1..Length( ListOfVertices ) ] do
    for j in [1..Length( ListOfVertices[i] )] do
	AppendTo( otf, ListOfVertices[i][j] );
	AppendTo( otf, " " );
    od;
    AppendTo( otf, "\n" );
od;

AppendTo( otf, "integral_closure \n" );
CloseStream( otf );

# finally let normaliz operate on this input file
Exec( Concatenation( d, "normaliz -s -a ", d, "VToH" ) );
#Exec( "/home/martin/WorkWithGAP/MyMethods/Normaliz/normaliz -s -a /home/martin/WorkWithGAP/MyMethods/Normaliz/VToH" );

# in the file .cst the constraints defining the cone are written, so we can read the contraints from there
file := Filename( Directory( d ), "VToH.cst" );
itf := InputTextFile( file );
N := Int( Chomp( ReadLine( itf ) ) );
ReadLine( itf );
output:=[];
for j in [1..N] do

   # read a line
   l := ReadLine( itf );

   # and turn it into a list of integers
   if l<>fail then
      l:=Chomp(l); # remove trailing CR/LF
      r:=[];
      for i in SplitString(l," ,") do # separate by SPACE or ,
          if Length(i)>0 then
             Add(r,Int(i));
          fi;
      od;
      Add(output,r);
   fi;

od;
CloseStream( itf );

# return the constraints
return output;

end );


# compute the cone C = K^sat = K mentioned in Greg Smith
# this requires that the variety be complete, smooth and projective
InstallMethod( GSCone,
                " for toric varieties.",
                [ IsToricVariety ],
function( variety )

#local B, i, j, k, M, monoms, help, deg, cones, conesHConstraints, file, otf, itf, gens, N, l, r;
local deg, rayList, conesVList, help, i, j, conesHList, file, otf, itf, gens, N, l, r;

if not IsSmooth( variety ) then

   Error( "Variety must be smooth for this method to work." );

elif not IsComplete( variety ) then

   Error( "Variety must be complete for this method to work." );

elif not IsProjective( variety ) then

   Error( "Variety must be projective for this method to work." );

fi;

# obtain degrees of the generators of the Coxring
deg := WeightsOfIndeterminates( CoxRing( variety ) );
deg := List( [1..Length(deg)], x -> UnderlyingListOfRingElements( deg[x] ) );

# figure out which rays contribute to the maximal cones in the fan
rayList := RaysInMaximalCones( FanOfVariety( variety ) );

# use raylist to produce list of cones to be intersected - each cone is V-presented first
conesVList := [];
for i in [1..Length(rayList)] do
    help := [];
    for j in [1..Length(rayList[i])] do
        if rayList[i][j] = 0 then
           # the generator j in deg should be added to help
           Add( help, deg[j] );
        fi;
    od;
    Add( conesVList, help );
od;

# remove duplicates in coneVList
conesVList := DuplicateFreeList( conesVList );

# compute the H-presentation for the cones given by the V-presentation in the above list
conesHList := [];
for i in [1..Length(conesVList)] do

    Append( conesHList, VToH( conesVList[i] ) );

od;

# return the list of hyperplane constraints
return conesHList;

end);


# extract the weights a_ij for a f.p. Z^n-graded S-module
InstallMethod( MultiGradedBetti, 
               " for f.p. graded S-modules.",
               [ IsGradedModuleOrGradedSubmoduleRep ],
function( F )

local r, mList, i, j, a, help;

# compute minimal free resolution/resolution
#r := FiniteFreeResolution( F );
r := Resolution( F );
mList := MorphismsOfComplex( r );

# investigate these morphisms to compute the degrees a
a := [];
for i in [1.. Length( mList )] do
    Add( a, DegreesOfGenerators( Range( mList[i] ) ) );
od;

# if the last object in the resolution is non-zero, then its degrees of generators need to be added
if not IsZero( mList[ Length( mList )] ) then
   Add( a, DegreesOfGenerators( Source( mList[i] ) ) );
fi;

# represent the degrees a as lists of ring elements
for i in [1.. Length( a )] do
    help := [];
    for j in [1.. Length( a[i] )] do
        Add( help, UnderlyingListOfRingElements( a[i][j] ) ); 
    od;
    a[i] := help;
od;

# return the weights
return a;

end );


# check if a point satisfies hyperplane constraints for a cone, thereby determining if the point lies in the cone
InstallMethod( Contained, 
               " for a cone given by H-constraints, a list specifying a point ",
               [ IsList, IsList ],
function( cone, point )

local i, res, help;

# initialise a value for res
res := true;

# check if the point satisfies the hyperplane constraints or not
for i in [1..Length(cone)] do

    # compute constraint
    help := Sum( List( [1..Length(cone[i])], x -> cone[i][x] * point[x]) );

    # if non-negative, the point satisfies this constraint
    if help < 0 then
       res := false;
       break;
    fi;

od;

# return the result
return res;

end );


# this methods checks if the conditions in the theorem by Greg Smith are satisfied
InstallMethod( Checker,
               "for a toric variety, an integer, and a module",
               [ IsToricVariety, IsInt, IsGradedModuleOrGradedSubmoduleRep ],
function( variety, e, module )

local B, BPower,aB, aB01, aM, d, Deltaa, i, j, C, deg, div, result;

# initialise result
result := true;

# we first compute the e-th Frobenius power of the irrelevant ideal
B := IrrelevantIdeal( variety );
BPower := GradedLeftSubmodule( List( EntriesOfHomalgMatrix( MatrixOfSubobjectGenerators( B ) ), x -> x^(e) ) );	

# check for ampleness
deg := UnderlyingListOfRingElements( DegreesOfGenerators( B )[1] );
div := DivisorOfGivenClass( variety, deg );

if IsAmple( div ) then
   # divisor is ample, we can thus proceed

   # compute the respective degree that is needed to compare
   aB := MultiGradedBetti( BPower );
   aB01 := aB[1][1];

   # compute the respective degrees of the module
   aM := MultiGradedBetti( module );

   # determine range in which we need to check
   d := Minimum( Dimension( variety ), Length( aM ) -1 );

   # now compute a list of vectors that need to lie in the GS cone
   Deltaa := [];
   for i in [0..d] do
       for j in [1..Length( aM[i+1] )] do
           Add( Deltaa, aB01 - aM[i+1][j] );
       od;
   od;

   # compute the GScone via its hyperplane criterion
   C := GSCone( variety );

   # now check if the points in Deltaa satisfy these constraints
   # if at least one does not, then return false
   for i in [1..Length(Deltaa)] do

       if not Contained( C, Deltaa[i] ) then
          result := false;
       fi;

   od;

else

   result := false;

fi;

# return the result
return result;

end );


# compute H0 by applying the theorem from Greg Smith
InstallMethod( H0ByGS, 
               " for a toric variety, a f.p. graded S-module ",
               [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep ],
function( variety, module )

local e, B, BPower, zero, GH;

# first determine the integer e that we need to perform the computation of the cohomology
e := 0;
while not Checker( variety, e, module ) do
      e := e + 1;
od;

# now compute the appropriate Frobenius power of the irrelevant ideal
B := IrrelevantIdeal( variety );
BPower := GradedLeftSubmodule( List( EntriesOfHomalgMatrix( MatrixOfSubobjectGenerators( B ) ), x -> x^(e) ) );	

# compute the GradedHom
GH := GradedHom( BPower, module );

# truncate the degree 0 part
zero := List( [1..Rank( ClassGroup( variety ) )], x -> 0 );
return [e, DegreeXPartOfFPModule( variety, GH, zero ) ];

end );
