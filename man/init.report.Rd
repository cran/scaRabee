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

\name{init.report}
\alias{init.report}
\title{
  Initiliaze Iteration Log and Report Files
}
\description{
  \code{init.report} is a secondary function called during estimation runs. It
  creates the log file and the report file in the results & backup directory.
  The log file stores the values of the objective function and the parameter
  estimates at each iteration of the estimation process. The report file reports
  information about the estimation run, including the final estimates and some
  related statistics.
}
\usage{
  init.report(param = NULL,
              files = NULL)
}
\arguments{
  \item{param}{A data.frame containing the values of fixed and variable 
    parameter estimates. Expected to contain the 'names', 'type', 'value',
    'isfix', 'lb', and 'ub' columns.}
  \item{files}{A list of input used for the analysis. The following elements are
    expected and none of them could be null: \describe{
      \item{data}{A .csv file located in the working directory, which contains
        the observations of the dependent variable(s) to be modeled. The
        expected format of this file is described in details in
        \code{vignette('scaRabee',package='scaRabee')}.}
      \item{param}{A .csv file located in the working directory, which contains
        the initial guess(es) for the model parameter(s) to be optimized or used
        for model simulation. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{dose}{A .csv file located in the working directory, which contains
        the dosing information. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{cov}{A .csv file located in the working directory, which contains
        the values of one or more covariates that may or may or be used within
        the model. The expected format of this file is described in details in
        \code{vignette('scaRabee',package='scaRabee')}.}
      \item{model}{A .R file located in the 'model.definition'
        sub-directory in the working directory, which defines the model. Models
        specified with explicit, ordinary or delayed differential equations
        should be preferentially defined using the provided templates. More
        details about the expected structure of this file is provided in
        \code{vignette('scaRabee',package='scaRabee')}, in case the user would
        want to develop her/his own template.}
      \item{var}{A .R file located in the 'model.definition' sub-directory
        in the working directory, which defines the model of residual
        variability. More details about the expected content of this file is
        provided in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{sec}{A .R file located in the 'model.definition' sub-directory
        in the working directory, which defines the method of computation of
        secondary parameters (derived from the fixed or estimated model
        parameters). More details about the expected content of this file is
        provided in \code{vignette('scaRabee',package='scaRabee')}.}
    }
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\keyword{ method }
