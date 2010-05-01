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

\name{get.layout}
\alias{get.layout}
\title{
  Layout for Lattice Functions
}
\description{
  \code{get.layout} is a utility function called by \code{estimation.plot} and 
  \code{simulation.plot}. It provides a layout for \pkg{lattice} functions based
  upon a user-defined number of plots per page.
}
\usage{
  get.layout(nplot = NULL)
}
\arguments{
  \item{nplot}{A integer scalar defining the number of plots per page.}
}
\value{
  Return a vector of two integers (nx,ny), where nx is the number of rows and
  ny the number of columns for the \pkg{lattice} layout.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{estimation.plot}},
  \code{\link[scaRabee]{simulation.plot}}
}
\examples{
  get.layout(1)
  get.layout(7)
  \dontrun{get.layout(1:5)}
  \dontrun{get.layout(NA)}
}
\keyword{ method }
