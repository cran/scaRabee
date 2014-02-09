%Copyright (c) 2009-2014 Sebastien Bihorel
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

\name{dde.model}
\alias{dde.model}
\title{
  Delay Differential Equations
}
\description{
  \code{dde.model} is the system evaluation function called when a $DDE block is
  detected in the model file, indicating that the model is defined by delay 
  differential equations. \code{dde.model} is typically not called directly by 
  users
}
\usage{
  dde.model(parms = NULL,
            derparms = NULL,
            code = NULL,
            bolus = NULL,
            infusion = NULL,
            xdata = NULL,
            covdata = NULL,
            issim = 0,
            check = FALSE,
            options = list(method='lsoda'))
}
\arguments{
  \item{parms}{A vector of primary parameters.}
  \item{derparms}{A list of derived parameters, specified in the $DERIVED block
    of code.}
  \item{code}{A list of R code extracted from the model file. Depending on 
      content of the model file, the levels of this list could be: template,
      derived, lags, ode, dde, output, variance, and/or secondary.}
  \item{bolus}{A data.frame providing the instantaneous inputs  entering the 
    system of delay differential equations for the treatment and individual 
    being evaluated.}
  \item{infusion}{A data.frame providing the zero-order inputs entering the 
    system of delay differential equations for the treatment and individual 
    being evaluated.}
  \item{xdata}{A vector of times at which the system is being evaluated.}
  \item{covdata}{A data.frame of covariate data for the treatment and individual 
    being evaluated.}
  \item{issim}{An indicator for simulation or estimation runs.}
  \item{check}{An indicator whether checks should be performed to validate 
    function inputs.}
  \item{options}{A list of delay differential equation solver options. Currently
    not modifiable by users.}
}
\details{
  \code{dde.model} evaluates the model for each treatment of each individual 
  contained in the dataset using, among other, the dedicated utility functions:
  \code{dde.syst}, and \code{dde.lags}. The actual evaluation of the system is 
  performed by \code{dede} from the \pkg{deSolve} package.
  
  \code{dde.model} also make use of utility functions which it shares with the 
  other system evaluation functions \code{explicit.model}, and \code{ode.model},
  such as \code{create.intervals}, \code{derived.parms}, \code{init.cond}, 
  \code{input.scaling}, \code{make.dosing}, \code{init.update}, and 
  \code{de.output}.
}
\value{
  Returns a matrix of system predictions.
}
\seealso{
  \code{\link[deSolve]{dede}},
  \code{\link[scaRabee]{dde.syst}}, \code{\link[scaRabee]{dde.lags}}, 
  \code{\link[scaRabee]{explicit.model}}, \code{\link[scaRabee]{ode.model}},
  \code{\link[scaRabee]{init.cond}}, \code{\link[scaRabee]{input.scaling}},
  \code{\link[scaRabee]{make.dosing}}, \code{\link[scaRabee]{init.update}},
  \code{\link[scaRabee]{de.output}}
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }