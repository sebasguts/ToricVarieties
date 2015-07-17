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

DeclareOperation( "ReadIntegerListsFromFile",
               [ IsDirectory, IsString ] );

DeclareOperation( "Exponents",
               [ IsToricVariety, IsList ] );

DeclareOperation( "MonomsOfCoxRingOfDegreeByNormaliz",
               [ IsToricVariety, IsList ] );

DeclareOperation( "DegreeXPart",
                 [ IsToricVariety, IsList ] );

DeclareOperation( "replacer",
               [ IsInt, IsInt, IsRingElement ] );

DeclareOperation( "DegreeXPartVects",
                 [ IsToricVariety, IsList, IsPosInt, IsPosInt ] );

DeclareOperation( "DegreeXPartVectsII",
                 [ IsToricVariety, IsList, IsPosInt, IsPosInt ] );

DeclareOperation( "DegreeXPartOfFreeModule",
                 [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ] );

DeclareOperation( "DegreeXPartOfFreeModuleAsVectorSpace",
                 [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ] );

DeclareOperation( "DegreeXPartOfFreeModuleAsMatrix",
                 [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ] );

DeclareOperation( "DegreeXPartOfFPModule",
                 [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsList ] );

DeclareOperation( "UnionOfRows",
                 [ IsList ] );


#################################
##
## Methods for B-transform
##
#################################

DeclareOperation( "H0FromBTransform",
                 [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsInt ] );

DeclareOperation( "H0FromBTransformInInterval",
                 [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep, IsInt, IsInt ] );


#######################################
##
## Methods to apply theorem by G. Smith
##
#######################################

DeclareOperation( "VToH", 
                  [ IsList ] );

DeclareOperation( "GSCone",
                  [ IsToricVariety ] );

DeclareOperation( "MultiGradedBetti", 
                  [ IsGradedModuleOrGradedSubmoduleRep ] );

DeclareOperation( "Contained",
                  [ IsList, IsList ] );

DeclareOperation( "Checker",
                  [ IsToricVariety, IsInt, IsGradedModuleOrGradedSubmoduleRep ] );

DeclareOperation( "H0ByGS",
                  [ IsToricVariety, IsGradedModuleOrGradedSubmoduleRep ] );

