# cNORMj - Continuous Norming for Jamovi

## Overview
cNORMj provides conventional and regression-based continuous norming methods for 
psychometric test development, biometrics and references scores in medical 
research in Jamovi. It is based on the 
[cNORM package for R](https://cran.r-project.org/package=cNORM) 
(Lenhard, Lenhard & Gary, 2018).

The continuous norming method produces norm scores by modeling the functional relationship between:
- Raw scores
- Norm scores
- Grouping variables (e.g., age, grade)

This approach:
- Reduces necessary sample size
- Closes gaps between norm groups
- Decreases error variance in norm data
- Smooths sampling errors

## Modules

### Single Group
For norming within a single group (e.g., all participants of the same age/grade).

### Continuous Norming
For creating continuous norms across multiple groups or ages. It includes
distribution free modelling (Taylor polynomials) and parametric modelling with
the beta binomial (e. g. 1PL IRT scales), Conway-Maxwell-Poisson (purely 
speeded tests, count information) and SinH ArcsinH (Shash; most flexible
parametric models).

## Key References
- Lenhard, A., Lenhard, W., Sebastian, S. & Segerer, R. (2018). A continuous solution to the norming problem. Assessment, 25, 112-125. doi:10.1177/1073191116656437
- Lenhard, A., Lenhard, W. & Gary, S. (2019). Continuous norming of psychometric tests: A simulation study of parametric and semi-parametric approaches. PLoS One, 14(9). doi:10.1371/journal.pone.0222279
- Lenhard, W. & Lenhard, A. (2020). Improvement of Norm Score Quality via Regression-Based Continuous Norming. Educational and Psychological Measurement, Online First, 1-33. doi:10.1177/0013164420928457
- Gary, S., Lenhard, W. & Lenhard, A. (2021). Modelling Norm Scores with the cNORM Package in R. Psych, 3(3), 501-521. https://doi.org/10.3390/psych3030033
- Gary, S., Lenhard, W., Lenhard, A., & Herzberg, D. (2023). A tutorial on automatic post-stratification and weighting in conventional and regression-based norming of psychometric tests. Behavior Research Methods. https://doi.org/10.3758/s13428-023-02207-0
- Gary, S., Lenhard, A., Lenhard, W., & Herzberg, D. S. (2023). Reducing the Bias of Norm Scores in Non-Representative Samples: Weighting as an Adjunct to Continuous Norming Methods. Assessment, 30(8), 2491–2509. https://doi.org/10.1177/10731911231153832
- Laurenz, R., & Lenhard, W. (2025). Comparison of cNORM and LMS methods for estimating reference percentile curves from biometric data. Scientific Reports. https://doi.org/10.1038/s41598-025-29580-4

## Further Information
More details and tutorials are available via [Psychometrica](https://www.psychometrica.de/cNorm_jamovi_en.html).