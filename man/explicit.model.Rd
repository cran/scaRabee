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

\name{explicit.model}
\alias{explicit.model}
\title{
  Explicit Equations
}
\description{
  \code{explicit.model} is the system evaluation function called when neither
  $ODE or $DDE blocks are detected in the model file, indicating that the model
  is defined by explicit equations. \code{explicit.model} is typically not 
  called directly by users.
}
\usage{
  explicit.model(parms = NULL,
                 derparms = NULL,
                 code = NULL,
                 bolus = NULL,
                 infusion = NULL,
                 xdata = NULL,
                 covdata = NULL,
                 issim = 0,
                 check = FALSE)
}
\arguments{
  \item{parms}{A vector of primary parameters.}
  \item{derparms}{A list of derived parameters, specified in the $DERIVED block
    of code. (NULL for \code{explicit.model}).}
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
}
\details{
  \code{explicit.model} evaluates the model for each treatment of each 
  individual contained in the dataset, based upon the code specified in the 
  $OUTPUT block in the model file.
}
\value{
  Returns a matrix of system predictions.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }