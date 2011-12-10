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

\name{dde.utils}
\alias{dde.syst}
\alias{dde.lags}
\title{
  Utility Functions for Delay Differential Equation Systems
}
\description{
  This is a collection of utility functions called by \code{dde.model} when 
  a model defined by delay differential equations is evaluated. None of these 
  functions is typically called directly by users.
}
\usage{
  dde.syst(t = NULL,
           y = NULL,
           dde.parms = NULL)
  dde.lags(parms = NULL,
           derparms = NULL,
           codelags = NULL,
           check = FALSE)
}
\arguments{
  \item{t}{A scalar or a vector of numerical time values.}
  \item{y}{A vector of system state values.}
  \item{dde.parms}{A list of parameters with the following levels:
    \describe{
      \item{parms}{See below}
      \item{derparms}{See below}
      \item{lags}{A vector of delay parameters, typically returned by 
        \code{dde.lags}.}
      \item{codedde}{The content of the R code specified within the $DDE block 
        in the model file.}
      \item{dosing}{See below}
      \item{xdata}{A vector of times at which the system is being evaluated.}
      \item{covdata}{A matrix of covariate data extracted from the dataset.}
      \item{scale}{A vector of system scale, typically returned by 
        \code{input.scaling}}
      \item{times}{A vector of times need by \code{dde.switch} to determine
        the system switches}
      \item{signal}{A vector of signals need by \code{dde.switch} to determine
        the system switches}
      \item{ic}{A vector of initial conditions, typically returned by 
        \code{init}}
      \item{check}{An indicator whether checks should be performed to validate 
        function inputs}
    }
  }
  \item{parms}{A vector of primary parameters.}
  \item{derparms}{A list of derived parameters, specified in the $DERIVED block
    of code.}
  \item{codelags}{The content of the R code specified within the $LAGS block in
    the model file.}
  \item{check}{An indicator whether checks should be performed to validate 
    function inputs.}
}
\details{
  \code{dde.syst} is the function which actually evaluates the system of delay
  differential equations specified in the $DDE block.
  
  \code{dde.lags} is the function which evaluates the code specified in the $LAG 
  block and defines the delays at which the system needs to be computed.
  
}
\seealso{
  \code{\link[scaRabee]{dde.model}}, \code{\link[scaRabee]{make.dosing}}
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }