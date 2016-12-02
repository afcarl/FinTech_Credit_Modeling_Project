# FinTech_Credit_Modeling_Project
Final project for INTA-GB.2320

As a group of two, we developed two credit models that predict the probability of default for each bank given their financial data. Preprocessing includes filling in na's with mean values, eliminating collinearity (i.e. total asset and average asset), feature selection based on both domain and statistical insights, and normalization by mapping values of each feature to percentiles. The linear regression models are pretty straightforward, one baseline model containing 9 variables based on domain insights only, the other model containing 41 variables based on both domain and statistical insights. Calibration was done by referencing the results from the valiation sample. The more complicated model, which we call the "sweet model," was able to achieve a ROC-AUC above 0.98 for our testing sample, while the baseline model at 0.93.

Training & Validation:

```bash
sweet.model.R
```

Testing:

```bash
create.PDs.R
```