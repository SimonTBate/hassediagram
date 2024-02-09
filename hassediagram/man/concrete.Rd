\name{concrete}

\alias{concrete}

\title{A fractional factorial design for investigating asphalt concrete production}

\description{This is fractional factorial design given in Anderson and McLean (1974) pp.256 from an experiment to investigate the effect of 5 variables/factors on the quality of asphalt concrete production.}

\usage{
  concrete
}

\format{
A half fraction factorial design of 16 runs on 5 variables/factors. The 5 variables/factors, each at two levels, included in the design are:
  \describe{
    \item{Aggregate gradation (AG)}{Categoric factor with levels being fine and coarse.}
    \item{Compaction temperature (CoT)}{Numeric factor with low 250°F and high 300°F.}
    \item{Asphalt content (AC)}{Numeric factor with low 5\% and high 7\%.}
    \item{Curing condition (CC)}{Categoric factor with levels wrapped and unwrapped.}
    \item{Curing temperature (CuT)}{Numeric factor with low 45°F and high 72°F.}
  }
}

\source{
Anderson, V.L. and McLean, R.A. (1974). Design of Experiments. Marcel Dekker Inc.: New York.
}


\examples{ 
data("concrete")
"concrete"
}



