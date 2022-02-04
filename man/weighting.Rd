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

\name{weighting}
\alias{weighting}
\title{
  Residual Variability
}
\description{
  \code{weighting} is a secondary function called during estimation run to
  evaluate the model(s) of residual variability specified by the code provided
  in the $VARIANCE block. \code{weighting} is typically not called directly by
  users.
}
\usage{
  weighting(parms = NULL,
            derparms = NULL,
            codevar=NULL,
            y=NULL,
            xdata=NULL,
            check=FALSE)
}
\arguments{
  \item{parms}{A vector of primary parameters.}
  \item{derparms}{A list of derived parameters, specified in the $DERIVED block
    of code.}
  \item{codevar}{The content of the R code specified within the $VARIANCE block 
    in the model file.}
  \item{y}{The matrix of structural model predictions.}
  \item{xdata}{A vector of times at which the system is being evaluated.}
  \item{check}{An indicator whether checks should be performed to validate 
     function inputs}
}
\value{
  Return a matrix of numeric values of the same dimension as \code{f}.}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
