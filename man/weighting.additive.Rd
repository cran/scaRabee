
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

\name{weighting.additive}
\alias{weighting.additive}
\title{
  Default Residual Variability Model
}
\description{
  \code{weighting additive} is a secondary function called during estimation run
  for which no residual variability model was provided
}
\usage{
  weighting.additive(f = NULL)
}
\arguments{
  \item{f}{matrix of structural model predictions.}
}
\value{
  Return a matrix of numeric values of the same dimension as \code{f}.}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
