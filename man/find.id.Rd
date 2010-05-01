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

\name{find.id}
\alias{find.id}
\title{  
  Scan for Integers in a Sorted Vector and Return Table of Min and Max Indices
}
\description{
  \code{find.id} is a secondary function, which main purpose is to split the 
  problem into sub-problems. It scans a vector of sorted integers (i.e. the
  \code{DoseID} variable from the data, dosing and covariate input files) and
  returns a table indicating between indices a particular integer is used.
}
\usage{
  find.id(x)
}
\arguments{
  \item{x}{A vector of sorted integers starting at 1 and increasing to n.}
}
\value{
  Returns a data.frame with the following columns: \describe{
    \item{ID}{the unique integers of \code{x}.}
    \item{starting}{first indice of \code{x} where the corresponding integer is
      used.}
    \item{ending}{last indice of \code{x} where the corresponding integer is
      used.}
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\examples{
  x <- rep(1:3,each=5)
  x
  find.id(x)
}
\keyword{ method }
