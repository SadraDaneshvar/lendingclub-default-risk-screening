<span style="font-family:Times New Roman; font-size:14pt;">
<h1 align="center"><b>Risk at First Sight: Default-Risk Prediction & Screening at Loan Origination in P2P Consumer Lending</b></h2>
</span>

Peer-to-peer (P2P) lending platforms must decide, *at origination*, which applicants to accept and how to price risk.  
This project studies a practical question:

**How predictable is loan default risk using only information available at origination, and what does that imply for screening policies?**

Using LendingClub loan-level data, I build a clean “origination-only” modeling pipeline, compare several classifiers, and translate prediction quality into **screening performance** (e.g., how many future defaults you can catch by reviewing only the riskiest applications). As an econometric add-on, I also run a **Double / Debiased Machine Learning (DML)** extension to study how contract terms relate to default after flexible adjustment for borrower characteristics.

## What’s in This Repository

- **`Risk_at_First_Sight.pdf`** — the full research-paper report (methods, results, figures, tables).
- **`Risk_at_First_Sight.R`** — a single, fully reproducible R script that:
  
  - prepares the dataset,
  - trains/evaluates models,
  - generates plots + LaTeX-ready tables,
  - runs the DML extension.

## Data

The analysis uses the **LendingClub “accepted loans” dataset**. Due to file size/licensing, the dataset is **not included** in the repo, but can be accessed from [LendingClub dataset on Kaggle](https://www.kaggle.com/datasets/wordsforthewise/lending-club/data). 

## Methodology (Project Phases)

### Phase 1. Data Construction (Origination-Only, No Leakage)

A major goal is to keep the prediction problem realistic:

- keep only features available at origination,
- avoid post-origination variables (payments, recoveries, etc.),
- construct a cohort to reduce right-censoring issues,
- split into train/test cleanly (with a fixed seed for reproducibility).

### Phase 2. Predictive Modeling + Screening Evaluation

Models compared (all trained on the same origination-only feature set):

- **Logistic regression**
- **Elastic net (penalized logit)**
- **CART**
- **Bagging** (two versions: custom + `ipred`)
- **Random forest** (`ranger`)

Evaluation focuses on both:

- **overall predictive metrics** (AUC, Brier score, accuracy, error), and
- **screening metrics** that matter operationally (ROC/PR curves, cumulative gains / lift curves).

### Phase 3. DML Extension (Contract Terms & Default)

As a causal-style add-on, the project estimates how default risk differs across:

- **60-month vs 36-month terms**, and
- **high interest rate (≥ 75th percentile) vs lower rates**,

using a **partially linear model** with **cross-fitted elastic-net** nuisance functions (DML / orthogonalized “residual-on-residual” regression).  
This is mainly used to show *why naive differences can be misleading* under strong selection.

## How to Run

### 1) Put the dataset in place

```

data/accepted_2007_to_2018Q4.csv

````

### 2) Run the script

From the repo root:

```bash
Rscript ML_Final_Assignment_756486.R
````

or open in RStudio and:

```r
source("ML_Final_Assignment_756486.R")
```

### 3) Outputs

The script generates:

* publication-style **plots** (ROC/PR curves, gains/lift, risk distributions, diagnostics),
* LaTeX-ready **tables**, including model metrics and the DML comparison table.

> If you prefer all outputs to go into an `output/` folder, you can trivially redirect the `ggsave()` / table exports to `output/` paths.

## Notes on Reproducibility

* The pipeline uses fixed random seeds for consistent train/test splits and model tuning.
* The entire project is designed around an **origination-only information set** to avoid leakage and keep the screening interpretation meaningful.

## License

[MIT LICENSE](LICENSE)
