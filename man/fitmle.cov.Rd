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
  
  \code{fitmle.cov} is typically not called directly by users.
  
}
\usage{
  fitmle.cov(problem = NULL,
             Fit = NULL)
}
\arguments{
  \item{problem}{A list containing the following levels:\describe{
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
    \item{ci}{The confidence interval for the parameter estimates.}
    \item{AIC}{The Akaike Information Criterion.}
    \item{sec}{A list of data related to the secondary parameters, containing
      the following elements:\describe{
        \item{estimates}{The vector of secondary parameter estimates calculated
          using the initial estimates of the primary model parameters.}
        \item{estimates}{The vector of secondary parameter estimates calculated
          using the final estimates of the primary model parameters.}
        \item{names}{The vector of names of the secondary parameter estimates.}
        \item{pder}{The matrix of partial derivatives for the secondary
          parameter estimates.}
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
  \code{\link[scaRabee]{fitmle}}, \code{\link[scaRabee]{order.parms.list}}
}
\keyword{ method }
