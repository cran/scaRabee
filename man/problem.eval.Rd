%Copyright (c) 2009, 2010 Sebastien Bihorel
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
  point estimates and at given values of the independent variable (typically
  time).
}
\usage{
  problem.eval(subproblem = NULL,
               x = NULL)
}
\arguments{
  \item{subproblem}{A list containing the following levels:\describe{
     \item{data}{A list containing the following levels:\describe{
                  \item{xdata}{1 x m matrix of independent variable.}
                  \item{ydata}{n x m matrix of observations from model states.}
                  \item{ids}{Data.frame of indices for data subsetting (output
                    from \code{find.id}).}
                }}
     \item{dosing}{A list containing the following levels:\describe{
                     \item{history}{d x 4 data.frame of dosing history.}
                     \item{ids}{data.frame of indices for dosing subsetting
                       (output from \code{find.id}).}
                  }}
     \item{cov}{A list containing the following levels:\describe{
                  \item{data}{c x t data.frame of covariate history.}
                  \item{ids}{Data.frame of indices for cov subsetting (output
                    from \code{find.id}).}
               }}
     \item{states}{Indices of the states to be output by the model.}
     \item{init}{A data.frame of parameter data with the following columns:
       'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
     \item{debugmode}{Logical indicator of debugging mode.}
     \item{modfun}{Model function.}
     \item{varfun}{Variance function; if empty \code{weighting.additive} is
       used.}
     \item{secfun}{Secondary parameter function.}
     }
  }
  \item{x}{A vector of numerical estimates of numerical parameters.}
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
