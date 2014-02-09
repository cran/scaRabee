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

\name{scarabee.check.reserved}
\alias{scarabee.check.reserved}
\title{
  Check for Reserved Variable Names
}
\description{
  \code{scarabee.check.reserved} is a secondary function called at each 
  \pkg{scaRabee} run. It determined whether user-defined parameter names use 
  reserved names and if some user-defined parameters are related to the dosing
  history. \code{scarabee.check.reserved} is typically not called directly by
  users. \code{scarabee.check.reserved} is typically not called directly by
  users.
}
\usage{
  scarabee.check.reserved(names = NULL, covnames = NULL)
}
\arguments{
  \item{names}{A vector of parameter names, typically extracted from the file
    of parameter definition.}
  \item{covnames}{A vector of covariate names, typically extracted from the 
    data file.}
}
\details{
  If one or more user-defined parameters are found to use reserved names, the 
  run is stopped and the user is ask to update the name(s) of this(ese) 
  parameter(s).
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }