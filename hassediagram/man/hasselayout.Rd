\name{hasselayout}

\alias{hasselayout}

\title{Hasse diagram of the Layout Structure}

\description{Returns a Hasse diagram of the Layout Structure of an experimental design}

\usage{
  hasselayout(datadesign, randomfacsid=NULL, pdf="N", example="example", 
              outdir=NULL, produceBWPlot = "N", larger.fontlabelmultiplier = 1, 
              middle.fontlabelmultiplier = 1, smaller.fontlabelmultiplier = 1, 
              check.shared.df = "Y", maxlevels.df = "Y", showLS = "Y", 
              showpartialLS = "Y", showdfLS = "Y")
}

\arguments{
  \item{datadesign}{A data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables/factors in the experimental design. The dataframe should only include the variables/factors/columns that the user wants to evaluate on the Hasse diagram.}
  \item{randomfacsid}{An optional vector specifying whether the factors are fixed (entry = 0) or random (entry = 1). The default choice is NULL and the function automatically sets all entries to 0. The length of the vector should be equal to the number of variables/factors in the design, i.e., the length of the vector should be equal to the number of columns of the argument datadesign.}
  \item{pdf}{logical. If "Y" then a pdf file containing the Hasse Diagram of the layout structure is generated. The default is "N", i.e., a pdf file is not generated.}
  \item{example}{Filename for the pdf of the Hasse diagram. The default is set to "example".}
  \item{outdir}{Location of the pdf output file if pdf="Y". The default is set to NULL and in this case if pdf="Y" then, the pdf containing the Hasse diagram will be outputed in the set working directory of the user's R session.}
  \item{produceBWPlot}{logical. If "Y" then the Hasse diagram will be generated in black and white format. The default is set to "N", i.e., a coloured version of the plot is generated.}
  \item{larger.fontlabelmultiplier}{The large font multiplier is the multiplier for the font used to define terms on the Hasse diagram where there are four or less terms at that level in the diagram. The default is 1.}
  \item{middle.fontlabelmultiplier}{The medium font multiplier is the multiplier for the font used to define terms on the Hasse diagram involving a factor that is equivalent to an interaction term. The default is 1.}
  \item{smaller.fontlabelmultiplier}{The medium font multiplier is the multiplier for the font used to define terms on the Hasse diagram involving a factor that is equivalent to an interaction term. The default is 1.}
  \item{check.shared.df}{logical. If "N" then a check for shared degrees of freedom is not performed. The default is "Y".}
  \item{maxlevels.df}{logical. If "N" then the potential number of maximum levels of an interaction is removed from the structural object label on the Hasse diagram of the layout structure. The default is "Y".}
  \item{showLS}{logical. If "N" then generation of the diagram is suppressed. The default is "Y".}
  \item{showpartialLS}{logical. If "N" then the partial crossing between structural objects are not illustrated (using dotted connecting lines) on the Hase diagram of the layout structure. The default is "Y".}
  \item{showdfLS}{logical. If "N" then the structural object label is not displayed on the Hasse diagram of the layout structure. The default is "Y".}
}

\details{
The hasselayout function generates Hasse diagram of the layout structure of the experimental design, as described in Bate and Chatfield (2016). The diagram consists of a set of structural objects, corresponding to the 
factors and generalised factors, and the relationships between the objects (either crossed, nested, partially crossed or equivalent), as defined by the structure of the experimental design.

The function requires a dataframe containing the variables corresponding to the experimental factors only (i.e., no response variables). 

In the dataset the levels of the variables/factors should be uniquely identified and have a physical meaning, otherwise the function will not correctly identify the nesting/crossing of the variables/factors.

Where present, partially crossed factors are illustrated on the diagram with dotted line connecting them. This feature can be excluded using the showpartialLS option.

The user has the ability to include the number of possible levels of the generalised factor (as well as the actual number present in the design), along with the "skeleton ANOVA" degrees of freedom of the structural object label on the diagram.

The function evaluates the design in order to identify if there are any shared degrees of freedom across the design. It is not recommended to perform this procedure for large designs, due to the potential high computational cost. This can be controlled using the check.shared.df = "N" option. 
}

\author{Marion Chatfield, Simon Bate and Damianos Michaelides}

\references{
Bate, S.T. and Chatfield, M.J. (2016), Identifying the structure of the experimental design. Journal of Quality Technology, 48, 343-364.

Bate, S.T. and Chatfield, M.J. (2016), Using the structure of the experimental design and the randomization to construct a mixed model. Journal of Quality Technology, 48, 365-387.

Brien, C.J. and Payne, R.W. (1999), Tiers, structure formulae and the analysis of complicated experiments. The Statistician, 48, 41-52.

McIntyre, G.A. (1955), Design and analysis of two phase experiments. Biometrics, 11, 324-34.

Williams, E.R., Matheson, A.C. and Harwood, C.E. (2002), Experimental design and analysis for tree improvement. 2nd edition. CSIRO, Melbourne, Australia.
}

\examples{ 
## Examples using the package build-in data
\link[hassediagram]{concrete}, \link[hassediagram]{dental}, \link[hassediagram]{human}, \link[hassediagram]{analytical}

## A fractional factorial design for investigating asphalt concrete production
hasselayout(datadesign=concrete, larger.fontlabelmultiplier=1.6, smaller.fontlabelmultiplier=1.3)

## A crossover design for a dental study
hasselayout(datadesign=dental, randomfacsid = c(0,1,0,0,0), larger.fontlabelmultiplier = 1.6)

## A block design for an experiment in human-computer interaction
hasselayout(datadesign=human, randomfacsid = c(1,1,0,0,0,1), larger.fontlabelmultiplier=1.4)

## A cross-nested design for an analytical method investigation
hasselayout(datadesign=analytical, randomfacsid = c(0,0,1,1,1,0,0,0), showpartialLS="N",
            check.shared.df="N", larger.fontlabelmultiplier=1, smaller.fontlabelmultiplier=1.6)


## Examples using data from the dae package

library(dae)

## Data for a balanced incomplete block experiment (Joshi (1987))

data(BIBDWheat.dat)
# remove the response from the dataset
BIBDWheat <- BIBDWheat.dat[, -4]
hasselayout(datadesign=BIBDWheat, example = "BIBDWheat")


## Data for an un-replicated 2^4 factorial experiment to investigate a chemical process

data(Fac4Proc.dat)
# remove the response from the dataset
Fac4Proc <- Fac4Proc.dat[, -6]
hasselayout(datadesign=Fac4Proc, example = "Fac4Proc", showpartialLS="N")


## Data for an experiment with rows and columns from Williams (2002)

data(Casuarina.dat)
# remove the response from the dataset
Casuarina <- Casuarina.dat[, -7]
# create unique factor level labels
Casuarina$Row <- paste(Casuarina$Reps, Casuarina$Rows)
Casuarina$Col <- paste(Casuarina$Reps, Casuarina$Columns)
Casuarina <- Casuarina[, -c(2,3)]
hasselayout(datadesign=Casuarina, example="Casuarina", check.shared.df="N", showpartialLS="N")


## Data for the three-phase sensory evaluation experiment in Brien, C.J. and Payne, R.W. (1999)

data(SPLGrass.dat)
# remove the response from the dataset
SPLGrass <- SPLGrass.dat[, -8]
# create unique factor level labels
SPLGrass$Mainunit <- paste(SPLGrass$Rows, SPLGrass$Columns)
SPLGrass$SCols <- paste(SPLGrass$Mainunit, SPLGrass$SubColumns)
SPLGrass$SRows <- paste(SPLGrass$Mainunit, SPLGrass$SubRows)
SPLGrass <- SPLGrass[, -c(3,4)]
hasselayout(datadesign=SPLGrass, example="SPLGrass", check.shared.df="N", showpartialLS="N")
}



