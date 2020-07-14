# Estimating Gini coefficients using CACI Ltd Paycheck data with skewed tails
## Calum Webb, the University of Sheffield
## 14th July 2020

This code was used by Webb et al. to estimate Gini coefficients for Local Education Authorities in England and Wales, using CACI Ltd Paycheck data. Because of licensing restrictions, we are not permitted to share the data used, but have made the code use available here.

The script works by taking income summary statistics (in 26 income bands up to £200k+) and simulating a distribution of incoem on the basis of these summary statistics. The head and tail of the distribution is skewed on the basis of the mean income (also provided by CACI). This usually corrects for the fact that there are a few cases in the £200k+ category with very large incomes and failing to account for these may underestimate income inequality in some areas. Finally, this simulated distribution is used to produce a Lorenz curve and calculate a Gini coefficient.  
