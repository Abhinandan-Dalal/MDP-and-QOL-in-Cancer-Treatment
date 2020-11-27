# MDP and QOL in Cancer Treatment
## Introduction
Markov Decision Processes and Dynamic Treatment Regimes have grown increasingly popular in the treatment of diseases, including cancer. However, cancer treatment often impacts quality of life drastically, and people often fail to take treatments that are sustainable, affordable and can be adhered to. In this paper, we emphasize the usage of ambient factors like profession, radioactive exposure, food habits on the treatment choice, keeping in mind not that the aim is not just to relieve the patient of his disease, but rather to maximize his overall physical, social and mental well being. We delineate a general framework which can directly incorporate a net benefit function from a physician as well as patient's utility, and can incorporate the varying probabilities of exposure and survival of patients of varying medical profiles. We also show by simulations that the optimal choice of actions often is sensitive to extraneous factors, like the financial status of a person (as a proxy for the affordability of treatment), and that these actions should be welcome keeping in mind the overall quality of life.

In our work, we formulate a Markov decision process to mode the transition of stages between different stages of cancer as a Markov process with associated rewards depending upon the actions performed in form of treatment modalities as well as the patient's personalized covariate profile that incorporates health related and socio-economic stature. We test our model on simuated data with covariate data on a cohort of Myeoid Lekemia patients. Data consists of age, blood pressure, income level, the indicator of radioactive exposure, hormonal level as covariates under consideration. For estimating the state transition probabilites Proportional Odds Cumulative Logit model has been used. With the estimated transition probabilities we search for the optimal treatment policies and further study their susceptibility towards a covariate's changes.

## Authors
* **Navonil Deb** - Master of Statistics, Final year, Indian Statistical Institute, Kolkata, India
* **Abhinandan Dalal** - Master of Statistics, Final year, Indian Statistical Institute, Kolkata, India
* **Gopal Krishna Basak** - Thereotical Statistics and Mathematics Unit, Indian Statistical Institute, Kolkata, India

## Installed Packages
* `mvtnorm`
* `nnet`
* `actuar`
