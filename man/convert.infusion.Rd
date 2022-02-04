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

\name{convert.infusion}
\alias{convert.infusion}
\title{Process Infusion Information}
\description{
  \code{convert.infusion} is a secondary function, which main purpose is to
  transform infusion information provided using NONMEM standards into an object
  that can be used by scaRabee model functions. \code{convert.infusion} is 
  typically not called directly by users.
}
\usage{
  convert.infusion(infusion.data = NULL)
}
\arguments{
  \item{infusion.data}{A data.frame with the following variables: trt, time, 
    cmt, amt, and rate.}
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\value{
  Return a data.frame with the following variables: trt, time, state,  bolus, 
  and infusion.
}
\seealso{
  \code{\link[scaRabee]{scarabee.read.data}}
}
\keyword{ method }
