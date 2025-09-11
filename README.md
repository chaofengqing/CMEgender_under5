# 

This repository presents the first subnational study of Sex Ratio at Birth (SRB) trends and projections in Vietnam from 1980 to 2050. We adopt a **Bayesian hierarchical time-series mixture model** to estimate and forecast SRB across six macro-regions of Vietnam. The model leverages an extensive database compiled from all available censuses and nationally representative surveys. 

**Please refer to the [published paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0253721) regarding to this repository for details on the research background/context, model implementation, data sources, and policy implications**:

* Main paper: Chao, Fengqing, Christophe Z. Guilmoto, and Hernando Ombao. "Sex ratio at birth in Vietnam among six subnational regions during 1980–2050, estimation and probabilistic projection using a Bayesian hierarchical time series model with 2.9 million birth records". _PLoS ONE_ 16, no. 7 (2021): e0253721.
* Technical appendix of the main paper: Chao, Fengqing, Christophe Z. Guilmoto, and Hernando Ombao. 2021. "S1 Appendix Sex ratio at birth in Vietnam among six subnational regions during 1990–2050, estimation and probabilistic projection using a Bayesian hierarchical time series model". _figshare_. doi:[10.6084/m9.figshare.14152979](https://doi.org/10.6084/m9.figshare.14152979).

## Repository Structure
The repository contains two parts: **code** and **data**. 

* Run the master R code files to get all the results. Specifically,
    - /code/main.R: run this to get the Markov chain Monte Carlo (MCMC) posterior samples of the Bayesian hierarchical model parameters; and
    - /code/main-output.R: after finish running the master code above, run this master code to get all the related output files and plots.
* Input data for the model:
    - /data/input/: data files related to country information, covariates, parameter priors;
    - /data/interim/database_for_modeling_2021-02-10.csv: database of regional-level SRB observations from Vietnam.
        - 526 observations with reference years ranging from 1972 to 2019.
        - This database is based on over 2.9 million birth records from censuses and nationally representative surveys in Vietnam.
        - Refer to the [published paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0253721) for details on data source and preprocessing.



## Research Context

This project presents a subnational estimation and projection of Vietnam’s Sex Ratio at Birth (SRB) from 1980 to 2050, based on a Bayesian hierarchical time series mixture model. While previous studies have examined SRB at the national level, our approach addresses the geographic heterogeneity of sex imbalances in Vietnam—a country where regional variation is both significant and policy-relevant. The model draws on a comprehensive compilation of survey and census data, spanning four decades, and statistically reconciles inconsistencies across data sources through a robust probabilistic framework.

<!-- Our results highlight the distinct spatial dynamics of sex selection, showing elevated SRBs in the northern regions such as the Red River Delta, while SRBs remain close to natural levels in the southern provinces. We identify the onset, stabilization, and projected turnaround phases of SRB transitions, linking these temporal patterns to demographic, cultural, and policy factors. In doing so, we contribute the first set of regional-level SRB projections for Vietnam and provide an open-source framework for reproducible, fine-grained demographic modeling in contexts of gender bias.

We release the full model specification, data harmonization pipeline, and scripts for model estimation and projection. This work is intended to inform both researchers and policymakers on the local dynamics of sex imbalances at birth, and to support evidence-based interventions aimed at reducing gender bias and its demographic consequences.
 --> 

## Methodology

The study estimates Vietnam's SRBs by subnational region from 1980 to 2019 and projects them until 2050 using a Bayesian hierarchical time series mixture model. This model incorporates uncertainties from observations and natural year-by-year fluctuations. The full model write ups are in the [published paper](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0253721).

We used the JAGS (Just Another Gibbs Sampler) to do Bayesian inference. The relevant R code to call JAGS and to get the MCMC samples are (they are called automatically in /code/main.R):
* /code/jags_setupMCMC.R: prior settings, MCMC settings
* /code/jags_writeJAGSmodel.R: the Bayesian hierarchical model to run in JAGS
* /code/jags_getMCMC.R: get the posterior samples using MCMC algorithm via JAGS

The model convergence is checked by the code:
* /code/jags_ConvergenceCheck.R: this code is called in /code/main_output.R
