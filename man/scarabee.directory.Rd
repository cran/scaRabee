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
  help. \code{scarabee.directory} is typically not called directly by users.
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
        the dosing information, the observations of the dependent variable(s)
        to be modeled, and possibly covariate information. The expected format 
        of this file is described in details in \code{vignette('scaRabee',
        package='scaRabee')}.}
      \item{param}{A .csv file located in the working directory, which contains
        the initial guess(es) for the model parameter(s) to be optimized or used
        for model simulation. The expected format of this file is described in
        details in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{model}{A text file located in the working directory, which defines 
        the model. Models specified with explicit, ordinary or delay 
        differential equations are expected to respect a certain syntax and 
        organization detailed in \code{vignette('scaRabee',package='scaRabee')}.}
      \item{iter}{A .csv file reporting the values of the objective function
        and estimates of model parameters at each iteration.  (Not used for 
        simulation runs).}
      \item{report}{A text file reporting for each individual in the dataset the
        final parameter estimates for structural model parameters, residual 
        variability and secondary parameters as well as the related statistics 
        (coefficients of variation, confidence intervals, covariance and 
        correlation matrix). (Not used for simulation runs).}
      \item{pred}{A .csv file reporting the predictions and calculated residuals
        for each individual in the dataset. (Not used for simulation runs).}
      \item{est}{A .csv file reporting the final parameter estimates for each
        individual in the dataset. (Not used for simulation runs).}
      \item{sim}{A .csv file reporting the simulated model predictions for each 
        individual in the dataset. (Not used for estimation runs).}
    }
  }
  \item{runtype}{A character string, indicating the type of analysis. Should be
    'simulation', 'estimation', or 'gridsearch'.}
  \item{analysis}{A character string directly following the $ANALYSIS tag in the
    model file.}
}
\value{
  When \code{scarabee.directory} is called, a new folder is created in the 
  working directory. The name of the new folder is a combination of the string
  directly following the $ANALYSIS tag in the model file, an abbreviation of the
  type of run ('est' for estimation, 'sim' for simulation, or 'grid' for grid 
  search) and an incremental integer, e.g. 'test.est.01'. This directory 
  contains the text and graph outputs of the run.

  Additionally, a sub-directory called \code{run.config.files} is created into
  the new folder and all the files defining the run, i.e. the dataset, the file 
  of initial model parameters, the model file and the master R script), are
  stored.
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{scarabee.analysis}}
}
\keyword{ method }
