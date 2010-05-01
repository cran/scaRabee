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

\name{compute.secondary}
\alias{compute.secondary}
\title{
  Computes secondary parameter values
}
\description{
  \code{compute.secondary} is a secondary function called during estimations
  runs. It evaluates the function defined as the \code{subproblem$sec}
  argument at the initial and the final estimates of the model parameters.
}
\usage{
  compute.secondary(subproblem = NULL,
                    x = NULL)
}
\arguments{
  \item{subproblem}{A list typically provided by \code{get.secondary} containing
    the following levels:\describe{
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
  \item{x}{The vector of \emph{p} final parameter estimates.}
}
\value{
  Return a list of with the following elements:\describe{
    \item{init}{The vector of \emph{s} secondary parameter estimates derived
      from initial structural model parameter estimates.}
    \item{estimates}{The vector of \emph{s} secondary parameter estimates
      derived from final structural model parameter estimates.}
    \item{names}{The vector of \emph{s} secondary parameter names.}
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}

\seealso{
  \code{\link[scaRabee]{scarabee.analysis}},
  \code{\link[scaRabee]{weighting.additive}},
  \code{\link[scaRabee]{fitmle}},
  \code{\link[scaRabee]{get.secondary}}
}

\keyword{ method }
