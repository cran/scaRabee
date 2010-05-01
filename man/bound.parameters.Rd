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

\name{bound.parameters}
\alias{bound.parameters}
\title{
  Forces parameter estimates between defined boundaries.
}
\description{
  \code{bound.parameters} is a utility function called during estimation runs. It forces the parameter
  estimates to remain within the boundaries defined in the .csv file of initial estimates.
}
\usage{
  bound.parameters(x = NULL,
                   lb = NULL,
                   ub = NULL)
}

\arguments{
  \item{x}{A vector of \emph{p} parameter estimates.}
  \item{lb}{A vector of \emph{p} lower boundaries.}
  \item{ub}{A vector of \emph{p} upper boundaries.}
}

\value{
  Returns a vector of \emph{p} values. The ith element of the returned vector is: \itemize{
    \item x[i]  if lb[i] < x[i] < ub[i]
    \item lb[i] if x[i] <= lb[i]
    \item ub[i] if ub[i] <= x[i]
  }
}

\author{
 Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}

\examples{

bound.parameters(seq(1:5), lb=rep(3,5), ub=rep(4,5))

# The following call should return an error message
bound.parameters(1, lb=rep(3,5), ub=rep(4,5))

}
\keyword{ method }
