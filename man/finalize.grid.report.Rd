%Copyright (c) 2009-2011 Sebastien Bihorel
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

\name{finalize.grid.report}
\alias{finalize.grid.report}
\title{
  Finalize Direct Grid Search Report
}
\description{
  \code{finalize.grid.report} is a secondary function called at the end of the 
  direct grid search step of the direct grid search runs (this step is actually
  by a simulation step). It outputs to the report file the grid search summary 
  table produced by \code{scarabee.gridsearch}. \code{finalize.grid.report} is 
  typically not called directly by users.
}
\usage{
  finalize.grid.report(problem = NULL,
                      fgrid = NULL,
                      files = NULL)
}
\arguments{
  \item{problem}{A list containing the following levels:\describe{
    \item{data}{A list which content depends on the scope of the analysis. If 
      the analysis was run at the level of the subject, \code{data} contains as 
      many levels as the number of subjects in the dataset, plus the \code{ids}
      level containing the vector of identification numbers of all subjects 
      included in the analysis population. If the analysis was run at the level 
      of the population, \code{data} contains only one level of data and 
      \code{ids} is set to 1.
      
      Each subject-specific level contains as many levels as there are treatment
      levels for this subject, plus the \code{trts} level listing all treatments
      for this subject, and the \code{id} level giving the identification number
      of the subject. 
      
      Each treatment-specific levels is a list containing the following levels: 
      \describe{
        \item{cov}{mij x 3 data.frame containing the times of observations of the
          dependent variables (extracted from the TIME variable), the indicators
          of the type of dependent variables (extracted from the CMT variable),
          and the actual dependent variable observations (extracted from the 
          DV variable) for this particular treatment and this particular 
          subject.}
        \item{cov}{mij x c data.frame containing the times of observations of 
          the dependent variables (extracted from the TIME variable) and all the
          covariates identified for this particular treatment and this 
          particular subject.} 
        \item{bolus}{bij x 4 data.frame providing the instantaneous inputs 
          for a treatment and individual.}
        \item{infusion}{fij x (4+c) data.frame providing the zero-order inputs for
          a treatment and individual.}
        \item{trt}{the particular treatment identifier.}}
      }
    \item{method}{A character string, indicating the scale of the analysis. Should
       be 'population' or 'subject'.}
    \item{init}{A data.frame of parameter data with the following columns:
      'names', 'type', 'value', 'isfix', 'lb', and 'ub'.}
    \item{debugmode}{Logical indicator of debugging mode.}
    \item{modfun}{Model function.}
    \item{hbsize}{An integer scalar providing the size of the history buffer for
      the solver of delay differential equations. This variable is ignored if
      the model file does not contain a $DDE tag.}
    }
  }
  \item{fgrid}{A data.frame with pe+2 columns. The last 2 columns report the
  value and the feasibility of the objective function at each specific vector of 
  parameter estimates which is documented in the first pe columns. This 
  data.frame must be ordered by feasibility and increasing value of the objective
  function.}
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
        and estimates of model parameters at each iteration. (Not used for
        direct grid search runs).}
      \item{report}{A text file reporting the summary tables of ordered 
        objective function values for the various tested vectors of model 
        parameters.}
      \item{pred}{A .csv file reporting the predictions and calculated residuals
        for each individual in the dataset. (Not used for direct grid search 
        runs).}
      \item{est}{A .csv file reporting the final parameter estimates for each
        individual in the dataset. (Not used for direct grid search runs).}
      \item{sim}{A .csv file reporting the simulated model predictions for each 
        individual in the dataset. (Not used for direct grid search runs).}
    }
  }
}

\author{
  Sebastien Bihorel (\email{sb.pmlab@gmail.com})
}

\value{
  Does not return any value.
}

\seealso{
  \code{\link[scaRabee]{scarabee.gridsearch}},
}

\keyword{ method }
