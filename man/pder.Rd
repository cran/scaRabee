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

\name{pder}
\alias{pder}
\title{
 Compute Matrix of Partial Derivatives 
}
\description{
  \code{pder} is a secondary function called by \code{get.cov.matrix}. It 
  computes the matrix of partial derivatives for the model predictions and the
  residual variability. \code{pder} is typically not called directly by users.
}
\usage{
  pder(subproblem = NULL,
       x = NULL)
}
\arguments{
  \item{subproblem}{A list containing the following levels:\describe{
    \item{code}{A list of R code extracted from the model file. Depending on 
      content of the model file, the levels of this list could be: template,
      derived, lags, ode, dde, output, variance, and/or secondary.}
    \item{method}{A character string, indicating the scale of the analysis. Should
      be 'population' or 'subject'.}
    \item{init}{A data.frame of parameter data with the following columns:
      'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
    \item{debugmode}{Logical indicator of debugging mode.}
    \item{modfun}{Model function.}
    \item{data}{A list containing the following levels:\describe{
      \item{xdata}{1 x m matrix of time of observations of the dependent
        variables.}
      \item{data}{m x 3 data.frame containing the times of observations of the
        dependent variables (extracted from the TIME variable), the indicators 
        of the type of dependent variables (extracted from the CMT variable), 
        and the actual dependent variable observations (extracted from the DV 
        variable).}
      }
    }
    \item{bolus}{bij x 4 data.frame providing the instantaneous inputs for a
      treatment and individual.}
    \item{infusion}{fij x (4+c) data.frame providing the zero-order inputs for a
      treatment and individual.}
    \item{cov}{mij x c data.frame containing the times of observations of the 
      dependent variables (extracted from the TIME variable) and all the
      covariates identified for this particular treatment.}
    }
  }
  \item{x}{The vector of \emph{p} final parameter estimates.}
}
\value{
  Return a list containing the \emph{p} x \emph{p} matrices of partial
  derivatives for model predictions (\code{mpder}) and residual variability
  (\code{wpder}).
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{get.cov.matrix}}
}
\keyword{ method }
