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

\name{fitmle.cov}
\alias{fitmle.cov}
\title{
  Computation of the Covariance Matrix
}
\description{
  \code{fitmle.cov} is a secondary function called during estimation runs. It 
  performs multiple tasks after completion of the model optimization by
  \code{fitmle}:

  1- It computes the matrix of covariance (as described by D'Argenio and 
  Schumitzky) by calling \code{get.cov.matrix} and derives some related
  statistics: correlation matrix, coefficient of variation of parameter
  estimates, confidence intervals and Akaike Information criterion,

  2- It estimates secondary parameters and computes the coefficient of variation
  of those estimates, as well as the confidence intervals.

}
\usage{
  fitmle.cov(problem = NULL,
             Fit = NULL)
}
\arguments{
    \item{problem}{A list containing the following levels:\describe{
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
  \item{Fit}{A list of containing the following levels:\describe{
    \item{estimations}{The vector of final parameter estimates.}
    \item{fval}{The minimal value of the objective function.}
    }
  }
}
\value{
  Return a list containing the following elements:\describe{
    \item{estimations}{The vector of final parameter estimates.}
    \item{fval}{The minimal value of the objective function.}
    \item{cov}{The matrix of covariance for the parameter estimates.}
    \item{orderedestimations}{A data.frame with the same structure as
      \code{problem$init} but only containing the sorted estimated estimates.
      The sorting is performed by \code{order.param.list}.}
    \item{cor}{The upper triangle of the correlation matrix for the parameter
      estimates.}
    \item{cv}{The coefficients of variations for the parameter estimates.}
    \item{delta}{The intervals used for the computation of confidence
      intervals.}
    \item{ci}{The confidence interval for the parameter estimates.}
    \item{AIC}{The Akaike Information Criterion.}
    \item{sec}{A list of data related to the secondary parameters, containing
      the following elements:\describe{
        \item{estimates}{A vector of secondary parameter estimates.}
        \item{cov}{The matrix of covariance for the secondary parameter
          estimates.}
        \item{cv}{The coefficients of variations for the secondary parameter
          estimates.}
        \item{ci}{The confidence interval for the secondary parameter
          estimates.}
        }
      }
    }
}
\references{
  D.Z. D'Argenio and A. Schumitzky. ADAPT II User's Guide: Pharmacokinetic/
  Pharmacodynamic Systems Analysis Software. Biomedical Simulations Resource,
  Los Angeles, 1997.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})

  Pawel Wiczling
}
\seealso{
  \code{\link[scaRabee]{fitmle}}, \code{\link[scaRabee]{order.param.list}}
}
\keyword{ method }
