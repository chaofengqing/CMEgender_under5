# National, regional, and global sex ratios of infant, child, and under-5 mortality and identification of countries with outlying ratios: a systematic assessment

This repository presents country-specific mortality sex ratios for infants, children aged 1–4 years, and children under the age of 5 years (under 5s) for all countries from 1990 to 2012 using the Bayesian hierarchical model. 

**Please refer to the [published paper](https://www.sciencedirect.com/science/article/pii/S2214109X14702803?via%3Dihub) regarding to this repository for details on the research background/context, model implementation, data sources, and policy implications**:

* Main paper: Leontine Alkema, Fengqing Chao, Danzhen You, Jon Pedersen, Cheryl C. Sawyer (2014). National, regional, and global sex ratios of infant, child, and under-5 mortality and identification of countries with outlying ratios: a systematic assessment. The Lancet Global Health, 2(9) e521–e530.
* Technical appendix of the main paper: Leontine Alkema, Fengqing Chao, Danzhen You, Jon Pedersen, Cheryl C. Sawyer (2014). Supplementary appendix: National, regional, and global sex ratios of infant, child, and under-5 mortality and identification of countries with outlying ratios: a systematic assessment.

## Repository Structure
The repository contains two parts: **code** and **data**. 

* Run the master R code files to get all the results. Specifically,
    - /code/main48_1,2,3,4.R: run this to get the Markov chain Monte Carlo (MCMC) posterior samples of the Bayesian hierarchical model parameters; and
    - /code/main48_output.R: after finish running the master code above, run this master code to get all the related output files and plots.
* Input data for the model: 
    - /data/input/: data files related to country information, covariates, parameter priors;
    - /data/qtotal_trajectories/U5MRandIMRtrajectories.L_20130813.rda: data files related to trajectories;due to the size of the file, if you need this file please contact to the corresponding author.
    - /data/interim/databset2013_fullyclean_2014-05-23.csv: database of Child Mortality Rate(CMR), Infant Mortality Rate(IMR) and Under 5 Mortality Rate(U5MR) from all countries.
        - This database is based on millions birth records from censuses and nationally representative surveys from all over the world.
        - Refer to the [supplementary appendix](https://www.thelancet.com/cms/10.1016/S2214-109X%2814%2970280-3/attachment/f8888b19-ce87-4f68-8eba-8ccb90976b8f/mmc1.pdf) for details on data source and preprocessing.


## Research Context

This project presents a national estimation and projection of country-specific mortality sex ratios from 1990 to 2012, based on a Bayesian hierarchical time series mixture model. The estimation of mortality by sex and identification of countries with outlying levels is challenging because of issues with data availability and quality, and because sex ratios might vary naturally based on differences in mortality levels and associated cause of death distributions. 

<!-- Our results highlight the distinct spatial dynamics of sex selection, showing elevated SRBs in the northern regions such as the Red River Delta, while SRBs remain close to natural levels in the southern provinces. We identify the onset, stabilization, and projected turnaround phases of SRB transitions, linking these temporal patterns to demographic, cultural, and policy factors. In doing so, we contribute the first set of regional-level SRB projections for Vietnam and provide an open-source framework for reproducible, fine-grained demographic modeling in contexts of gender bias.

We release the full model specification, data harmonization pipeline, and scripts for model estimation and projection. This work is intended to inform both researchers and policymakers on the local dynamics of sex imbalances at birth, and to support evidence-based interventions aimed at reducing gender bias and its demographic consequences.
 --> 

## Methodology

The study estimates country-specific mortality sex ratios for infants, children aged 1–4 years, and children under the age of 5 years (under 5s) for all countries , The full model write ups are in the [supplementary appendix](https://www.thelancet.com/cms/10.1016/S2214-109X%2814%2970280-3/attachment/f8888b19-ce87-4f68-8eba-8ccb90976b8f/mmc1.pdf).

We used the JAGS (Just Another Gibbs Sampler) to do Bayesian inference. The relevant R code to call JAGS and to get the MCMC samples are (they are called automatically in /code/main.R):
* /code/jags_setupMCMC.R: prior settings, MCMC settings
* /code/jags_writeJAGSmodel.R: the Bayesian hierarchical model to run in JAGS
* /code/jags_getMCMC.R: get the posterior samples using MCMC algorithm via JAGS

The model convergence is checked by the code:
* /code/jags_ConvergenceCheck.R: this code is called in /code/main_output.R
