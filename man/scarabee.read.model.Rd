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

\name{scarabee.read.model}
\alias{scarabee.read.model}
\title{
  Read scaRabee Model File
}
\description{
  \code{scarabee.read.model} is a secondary function called at each 
  \pkg{scaRabee} run. It reads and processes the data contained in the specified
  model file. This function performs numerous checks to ensure that the model 
  file contains the expected information. \code{scarabee.read.model} is 
  typically not be called directly by users.
}
\usage{
  scarabee.read.model(files = NULL, 
                      runtype = NULL)
}
\arguments{
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
  \item{runtype}{A single character string, indicating the type of run; either
    'estimation', 'simulation', or 'gridsearch'.}
}
\value{
  Return a list with various levels, depending on the content of the model file
  and the type of run:\describe{
    \item{name}{The character string which follows the $ANALYSIS tag.}
    \item{template}{A character string, indicating which scaRabee function needs
      to be called to evaluate the model. This level is set to 'ode.model' if
      the $ODE tag is detected, to 'dde.model' if the $DDE tag is detected, and 
      to 'explicit.model' otherwise.}
    \item{derived}{A character string containing the R code provided within the
      $DERIVED block. (Only for models defined with $ODE or $DDE).}
    \item{ic}{A character string containing the R code provided within the $IC 
      block. (Only for models defined with $ODE or $DDE).}
    \item{scale}{A character string containing the R code provided within the
      $SCALE block. (Only for models defined with $ODE or $DDE).}
    \item{de}{A character string containing the R code provided within the
      $ODE or $DDE block. (Only for models defined with $ODE or $DDE).}
    \item{lags}{A character string containing the R code provided within the
      $LAGS block. (Only for models defined with $DDE).}
    \item{output}{A character string containing the R code provided within the
      $OUTPUT block.}
    \item{var}{A character string containing the R code provided within the
      $VARIANCE block.}
    \item{sec}{A character string containing the R code provided within the
      $SECONDARY block.}
  }
}
\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}
\seealso{
  \code{\link[scaRabee]{scarabee.analysis}}
}
\keyword{ method }
