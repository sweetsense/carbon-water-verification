# Verification requirements in carbon-financed water projects

Data and analysis code for the study *"Tying payment to proof: verification requirements in
carbon-financed water projects improve drinking water safety"* (Ecklu, Nagel, MacDonald, Thomas).

This repository reproduces every statistic and figure in the paper from de-identified
microbial water quality data collected across seven climate-financed drinking water
projects in six countries (Kenya, Rwanda, the Democratic Republic of Congo, Madagascar,
Burundi, and Tanzania), reported under the Gold Standard carbon-crediting process.

## Data

The `data/` directory contains **de-identified** water quality records. Each file has been
reduced to only the variables used in the analysis (deployment/round, E. coli result, and,
for the institutional model, an anonymous school grouping identifier). All personal or
directly identifying fields present in the source monitoring databases — respondent names,
enumerator names, household codes, and GPS coordinates — have been removed. The underlying
data were collected by the implementing partners for Gold Standard monitoring and reporting
and are managed in the mWater platform.

| File | Project | Country | Test type |
|------|---------|---------|-----------|
| `LifeStraw_WQ.csv` | LifeStraw (schools) | Kenya | MPN risk category |
| `AmaziMeza_WQ.csv` | Amazi Meza (schools) | Rwanda | MPN risk category |
| `Asili_WQ.csv` | Asili (piped/kiosks) | DRC | MPN risk category |
| `Burundi_WQ.csv` | Amazi Water | Burundi | MPN risk category |
| `Helvetas_WQ.csv` | Ranovola/Helvetas | Madagascar | MPN risk category |
| `MWA_baseline.csv` | DRIP-FUNDI (MWA) | Kenya | presence/absence |
| `DRIP_first_monitor.csv` | DRIP-FUNDI (MWA), first monitoring | Kenya | presence/absence |
| `MWA_monitor.csv` | DRIP-FUNDI (MWA), current monitoring | Kenya | MPN risk category |
| `WaterMission_baseline.csv` | Water Mission | Tanzania | fecal coliform CFU |

## Code

| Script | Purpose |
|--------|---------|
| `R/prep.R`  | Reads `data/`, partitions baseline vs. monitoring rounds, applies the documented cleaning/exclusion steps, and writes `prepped.rds`. |
| `R/model.R` | Fits the Bayesian hierarchical multinomial logistic regression (`brms`) for the institutional projects and reports odds ratios; summarises Asili, the DRIP-FUNDI trajectory, and maintenance records. |
| `R/figs.R`  | Regenerates all figures into `figures/`. |

### Reproduce

```r
# from the repository root
install.packages(c("tidyverse", "janitor", "brms", "tidybayes", "scales"))
source("R/prep.R")    # -> prepped.rds
source("R/model.R")   # -> model.rds, model_out.rds, console odds ratios
source("R/figs.R")    # -> figures/*.pdf
```

The institutional model uses a fixed seed (`20260529`) for reproducibility.

## Key results

- 4,110 baseline microbial water quality tests; 74.4% positive for E. coli, 42.2% of
  graded samples in the WHO "very high risk" category.
- Institutional filter projects (LifeStraw + Amazi Meza): odds of "very high risk"
  post-intervention reduced ~99% (OR 0.010, 95% CrI 0.004–0.022).
- DRIP-FUNDI (Kenya): contamination across three successive verification rounds of
  51% → 64% → 7%, illustrating that the verification requirement, not the initial
  infrastructure, drove safe water delivery.

## License

Released under the MIT License (see `LICENSE`).
