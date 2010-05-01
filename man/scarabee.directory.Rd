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

\name{scarabee.directory}
\alias{scarabee.directory}
\title{
  Creation of the Run Directory
}
\description{
  \code{scarabee.directory} is a secondary function called at each 
  \pkg{scaRabee} run. It creates a directory to store the results of the run and
  a sub-directory to backup all files used for the run. This directory is
  referred to as the 'run directory' in all \pkg{scaRabee} documentation and
  help.
}
\usage{
  scarabee.directory(curwd = getwd(),
                     files = NULL,
                     runtype = NULL,
                     analysis = NULL)
}
\arguments{
  \item{curwd}{The current working directory.}
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
  \item{runtype}{A character string, indicating the type of analysis. Should be
    'simulation' or 'estimation'.}
  \item{analysis}{A character string, defining the 'title' of the run that will
    be used to name the output files of the analysis. If
    \code{scarabee.analysis} is called within \code{script.R}, a file created
    from the provided template , \code{analysis} will be 'script'.}
}
\value{
  When \code{scarabee.directory} is called, a new folder is created in the 
  working directory. The name of the new folder is a combination of the string
  contained in \code{analysis}, an abbreviation of the type of run ('est'
  for estimation or 'sim' for simulation) and an incremental integer, e.g.
  test.est.01. This directory will contain the text and graph outputs of the
  run.

  Additionally, a sub-directory called \code{run.config.files} is created into
  the new folder and all the files defining the run (\code{covariates.csv},
  \code{data.csv}, \code{dosing.csv}, \code{initials}, \code{model.R},
  \code{secondary.R}, \code{weighting.R}, and the master R script) are stored.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{scarabee.analysis}}
}
\keyword{ method }
