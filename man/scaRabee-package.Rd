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

\name{scaRabee-package}
\alias{scaRabee-package}
\alias{scaRabee}
\docType{package}
\title{
  scaRabee Toolkit
}
\description{
  Framework for Pharmacokinetic-Pharmacodynamic Model Simulation and Optimization
}
\details{
\tabular{ll}{
Package: \tab scaRabee\cr
Type: \tab Package\cr
Version: \tab 1.0-2\cr
Date: \tab 2010-05-25\cr
License: \tab GPL-v3 \cr
LazyLoad: \tab yes
}
\pkg{scaRabee} is a toolkit for modeling and simulation primarily intended for
the field
of pharmacometrics. This package is a R port of Scarabee, a Matlab-based piece of
software developed as a fairly simple application for the simulation and
optimization of pharmacokinetic and/or pharmacodynamic models specified
with explicit solutions, ordinary or delayed differential equations.

The method of optimization used in scaRabee is based upon the Nelder-Mead simplex
algorithm, as implemented by the \code{fminsearch} function from the
\pkg{neldermead} package.

Please, refer to the vignette to learn how to run analyses with \pkg{scaRabee} and
read more about the methods used in \pkg{scaRabee}.

\pkg{scaRabee} is  available on the Comprehensive R Archive Network and also at:
\url{http://code.google.com/p/pmlab/}

}

\author{
Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}

\keyword{package}
\seealso{
  \code{\link[neldermead]{neldermead}}
}

