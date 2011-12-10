%Copyright (c) 2009-2011 Sebastien Bihorel
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

\name{problem.eval}
\alias{problem.eval}
\title{
  Evaluation of structural and residual variability models
}
\description{
  \code{problem.eval} is a secondary function called during estimation runs. It 
  evaluates the structural model and the residual variability model at given
  point estimates and at given values of the time variable. \code{problem.eval} 
  is typically not called directly by users.
}
\usage{
  problem.eval(subproblem = NULL,
               x = NULL,
               grid = FALSE,
               check = FALSE)
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
  \item{x}{A vector of numerical estimates of numerical parameters.}
  \item{grid}{A logical variable, indicating whether the analysis is a direct
    grid search or not.}
  \item{check}{An indicator whether checks should be performed to validate 
    function inputs.}
}
\value{
  Return a list of two elements: \describe{
    \item{f}{A vector of model evaluations at all requested time points (all
      states values are concatenated into a single vector).}
    \item{weight}{A vector of residual variability related to the model
      evaluations.}
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}

\seealso{
  \code{\link[scaRabee]{fitmle}}
}
\keyword{ method }
