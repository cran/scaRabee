%Copyright (c) 2009-2022 Sebastien Bihorel
%All rights reserved.
%
%This file is part of scaRabee.
%
%    scaRabee is free software: you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation, either version 3 of the License, or
%    (at your option) any later version.
%
%    scaRabee is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with scaRabee.  If not, see <http://www.gnu.org/licenses/>.
%

\name{get.secondary}
\alias{get.secondary}
\title{
  Computation of Secondary Parameter Estimates
}
\description{
  \code{get.secondary} is a secondary function called during estimation runs by 
  \code{fitmle.cov}. It computes estimates of secondary parameters and related
  statistics (covariance, coefficient of variations, and confidence intervals).
  \code{get.secondary} is typically not called directly by users.
}
\usage{
get.secondary(subproblem = NULL,
              x = NULL)
}
\arguments{
  \item{subproblem}{A list containing the following levels:\describe{
    \item{data}{A list containing as many levels as there are treatment levels 
      for the subject (or population) being evaluated, plus the \code{trts} 
      level listing all treatments for this subject (or population), and the 
      \code{id} level giving the identification number of the subject (or set to
      1 if the analysis was run at the level of the population.
      
      Each treatment-specific level is a list containing the following levels: 
      \describe{
        \item{cov}{mij x 3 data.frame containing the times of observations of the
          dependent variables (extracted from the TIME variable), the indicators
          of the type of dependent variables (extracted from the CMT variable),
          and the actual dependent variable observations (extracted from the 
          DV variable) for this particular treatment.}
        \item{cov}{mij x c data.frame containing the times of observations of 
          the dependent variables (extracted from the TIME variable) and all the
          covariates identified for this particular treatment.} 
        \item{bolus}{bij x 4 data.frame providing the instantaneous inputs 
          for a treatment and individual.}
        \item{infusion}{fij x (4+c) data.frame providing the zero-order inputs for
          a treatment and individual.}
        \item{trt}{the particular treatment identifier.}}
    }
    \item{method}{A character string, indicating the scale of the analysis. Should
       be 'population' or 'subject'.}
    \item{init}{A data.frame of parameter data with the following columns:
      'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
    \item{debugmode}{Logical indicator of debugging mode.}
    \item{modfun}{Model function.}
    }
  }
  \item{x}{The vector of \emph{p} parameter estimates.}
}
\value{
  Return a list of with the following elements:\describe{
    \item{init}{The vector of \emph{s} secondary parameter estimates derived
      from initial structural model parameter estimates.}
    \item{estimates}{The vector of \emph{s} secondary parameter estimates
      derived from final structural model parameter estimates.}
    \item{names}{The vector of \emph{s} secondary parameter names.}
    \item{pder}{The \emph{p} x \emph{s} matrix of partial derivatives for the
      secondary parameters.}
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{fitmle.cov}}
}
\keyword{ method }
