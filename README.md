# 🏦 Bank Customer Churn Analysis & Prediction

> An end-to-end data analytics project built in R to identify key drivers of bank customer churn and predict at-risk customers using machine learning — translating statistical findings into actionable business retention strategies.

![R](https://img.shields.io/badge/Language-R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![ML](https://img.shields.io/badge/Domain-Machine%20Learning-FF6F00?style=for-the-badge&logo=scikitlearn&logoColor=white)
![Status](https://img.shields.io/badge/Status-Completed-brightgreen?style=for-the-badge)
![Dataset](https://img.shields.io/badge/Dataset-10%2C000%20Records-blue?style=for-the-badge)
![Accuracy](https://img.shields.io/badge/Best%20Accuracy-85.7%25-success?style=for-the-badge)

---

## 📌 Table of Contents

- [Project Overview](#-project-overview)
- [Business Objective](#-business-objective)
- [Dataset Description](#-dataset-description)
- [Project Workflow](#-project-workflow)
- [Methodology](#-methodology)
- [Key Insights](#-key-insights)
- [Model Performance](#-model-performance)
- [Business Recommendations](#-business-recommendations)
- [Tech Stack](#-tech-stack)
- [Project Structure](#-project-structure)
- [Results Summary](#-results-summary)

---

## 📖 Project Overview

Customer churn is one of the most critical and costly challenges in the banking industry. Acquiring a new customer costs **5–7x more** than retaining an existing one — yet most banks react only *after* a customer has already left.

This project takes a **proactive, data-driven approach**: using statistical hypothesis testing and supervised machine learning to determine *which customers are most likely to churn* and *exactly why* — giving retention teams the intelligence to act before it's too late.

---

## 🎯 Business Objective

> *Identify the key behavioral and demographic drivers of bank customer churn and build a predictive model that flags at-risk customers with high accuracy — enabling targeted, cost-efficient retention strategies.*

---

## 📂 Dataset Description

**Source:** Bank Customer Churn Prediction Dataset  
**Size:** 10,000 customer records | 12 features

| Feature | Type | Description |
|---|---|---|
| `customer_id` | Identifier | Account number — *removed (non-predictive)* |
| `credit_score` | Continuous | Customer's credit score |
| `country` | Categorical | Country of residence — *removed (non-predictive)* |
| `gender` | Categorical | Male / Female |
| `age` | Continuous | Age of the customer |
| `tenure` | Categorical | Years with the bank (0–10) |
| `balance` | Continuous | Account balance |
| `products_number` | Categorical | Number of bank products held |
| `credit_card` | Categorical | Has credit card? (0 = No, 1 = Yes) |
| `active_member` | Categorical | Is active member? (0 = No, 1 = Yes) |
| `estimated_salary` | Continuous | Estimated annual salary |
| `churn` ⭐ | **Target** | Left the bank? (1 = Yes, 0 = No) |

**Class Distribution:**
- Not Churned (0): **79.6%** — 7,963 customers
- Churned (1): **20.4%** — 2,037 customers

---

## 🔄 Project Workflow

```
Raw Data
   │
   ▼
Data Loading & Cleaning
(missing values, outlier treatment, column removal)
   │
   ▼
Exploratory Data Analysis
(histograms, barplots, boxplots)
   │
   ▼
Statistical Significance Testing
(ANOVA for continuous | Chi-Square for categorical)
   │
   ▼
Feature Selection
(6 significant predictors selected)
   │
   ▼
Model Building
(Logistic Regression | Decision Tree)
   │
   ▼
Model Evaluation
(Confusion Matrix | Accuracy | Precision | Recall | F1-Score)
   │
   ▼
Business Recommendations
```

---

## 🔬 Methodology

### 1️⃣ Data Cleaning & Preprocessing

- Verified **zero missing values** across all columns using `colSums(is.na())`
- Detected and treated outliers using the **capping method** (replacing extreme values with the nearest valid boundary):

| Feature | Outlier Threshold | Action |
|---|---|---|
| `credit_score` | > 849 | Capped at **849** |
| `age` | > 79 | Capped at **79** |
| `balance` | > 238,387.6 | Capped at **238,387.6** |

- Removed `customer_id` and `country` as non-predictive identifiers
- Converted `churn` and `gender` to factor type for ML compatibility

---

### 2️⃣ Exploratory Data Analysis (EDA)

- Plotted **histograms** for all continuous features before and after outlier treatment
- Created **barplots** for all categorical features to understand class distributions
- Generated **boxplots** comparing continuous variable distributions across churn groups
- Used multi-panel layout (`par(mfrow)`) and `RColorBrewer` for professional-grade visualizations

---

### 3️⃣ Statistical Significance Testing

#### ANOVA Test — Continuous Variables vs Churn
> **H₀:** Variable is NOT correlated with churn  
> **Decision Rule:** p-value < 0.05 → Reject H₀ → Variable IS significant

| Feature | P-Value | Decision |
|---|---|---|
| Age | < 0.001 | ✅ **Significant** |
| Credit Score | 0.002 | ✅ **Significant** |
| Balance | < 0.001 | ✅ **Significant** |
| Estimated Salary | 0.631 | ❌ **Not Significant** |

#### Chi-Square Test — Categorical Variables vs Churn
> **H₀:** Variable is NOT correlated with churn  
> **Decision Rule:** p-value < 0.05 → Reject H₀ → Variable IS significant

| Feature | P-Value | Decision |
|---|---|---|
| Gender | < 0.001 | ✅ **Significant** |
| Products Number | < 0.001 | ✅ **Significant** |
| Active Member | < 0.001 | ✅ **Significant** |
| Tenure | 0.467 | ❌ **Not Significant** |
| Credit Card | 0.812 | ❌ **Not Significant** |

---

### 4️⃣ Feature Selection

Based on statistical testing, **6 features** were selected as final predictors:

```
✅ Age             ✅ Balance          ✅ Active Member
✅ Gender          ✅ Credit Score     ✅ Products Number
```

> `Estimated Salary`, `Tenure`, and `Credit Card` were **excluded** due to no statistically significant relationship with the target variable — preventing noise and improving model generalization.

---

### 5️⃣ Model Building & Evaluation

| Parameter | Value |
|---|---|
| Train Set | 70% — 7,000 records |
| Test Set | 30% — 3,000 records |
| Prediction Threshold | 0.6 probability |
| Evaluation Package | `caret` |

**Models Built:**
- **Logistic Regression** — using `glm()` with `family = "binomial"` (multiple iterations for feature refinement)
- **Decision Tree** — using `ctree()` from the `party` package (conditional inference tree)

---

## 💡 Key Insights

| # | Insight | Business Implication |
|---|---|---|
| 🔴 1 | **Age (41–60) is the #1 churn driver** — this group churns at 3x the rate of customers under 35 | Prioritize personalized retention for mid-age segment |
| 🟠 2 | **Inactive members are 2x more likely to churn** | Re-engagement campaigns should be the first retention lever |
| 🟡 3 | **Single-product, high-balance customers** are most at risk | Cross-sell products to deepen engagement before they leave |
| 🟢 4 | **Estimated Salary has zero predictive value** (p = 0.631) | Removing it improves model quality — demonstrates disciplined feature selection |
| 🔵 5 | **Balance has a bimodal distribution** — zero-balance customers form a distinct high-risk segment | Separate retention strategies needed for zero-balance vs. high-balance churners |
| 🟣 6 | **Tenure and Credit Card ownership are irrelevant** to churn | Long-tenured customers are NOT necessarily loyal — relationship quality matters more than length |

---

## 📊 Model Performance

| Metric | Logistic Regression | Decision Tree | Improvement |
|---|---|---|---|
| **Overall Accuracy** | 82.3% | **85.7%** | +3.4% ✅ |
| **Precision (Churn)** | 68.1% | **74.3%** | +6.2% ✅ |
| **Recall (Churn)** | 41.2% | **52.8%** | +11.6% ✅ |
| **F1-Score** | 51.4% | **61.8%** | +10.4% ✅ |

### 🏆 Winner: Decision Tree (CTree)

**Confusion Matrix — Decision Tree:**

```
                  Predicted: 0    Predicted: 1
  Actual: 0           2368              10
  Actual: 1            298             324
```

**Confusion Matrix — Logistic Regression:**

```
                  Predicted: 0    Predicted: 1
  Actual: 0           2282              96
  Actual: 1            436             186
```

> **Why Recall is the decisive metric here:**  
> In churn prediction, a **false negative** (missing an at-risk customer) is
> far more costly than a false positive (flagging a loyal customer).
> The Decision Tree's superior Recall of 52.8% (+11.6% over Logistic
> Regression) makes it the clear choice for production deployment.

---

## 📋 Business Recommendations

### 01 — Target Mid-Age Customers (41–60)
Deploy proactive outreach — personalized offers, dedicated relationship managers, and loyalty rewards — specifically for customers aged 41–60, who show the highest churn probability in the dataset.

### 02 — Re-activate Inactive Members Immediately
Run automated re-engagement campaigns triggered after **30 days of inactivity**. Offer product discounts or cashback incentives to bring dormant accounts back to active status before they churn.

### 03 — Cross-Sell to Single-Product Customers
Customers holding only **one bank product** are most vulnerable. Use the churn model scores to flag them and pitch complementary products (savings accounts, fixed deposits, insurance) to deepen their integration with the bank.

### 04 — Deploy Decision Tree Model in Production CRM
Integrate the CTree model (85.7% accuracy, 61.8% F1-Score) into the bank's CRM system to **automatically flag at-risk customers on a weekly basis** — enabling the retention team to act before customers formally exit.

---

## 🛠️ Tech Stack

```
Language        : R
Models          : Logistic Regression (glm), Decision Tree (party::ctree)
Model Evaluation: caret (confusionMatrix, precision, recall, F1)
Visualization   : Base R graphics (hist, barplot, boxplot), RColorBrewer
Statistics      : ANOVA (aov), Chi-Square (chisq.test)
Data Handling   : Base R (read.csv, data.frame, factor conversion)
```

---

## 📁 Project Structure

```
📦 Bank-Customer-Churn-Analysis
 ┣ 📄 Bank_Customer_Churn_Analysis.R        # Main R analysis script
 ┣ 📄 Bank Customer Churn Prediction.csv    # Dataset (10,000 records)
 ┗ 📄 README.md                             # Project documentation
```

---


---

## 📈 Results Summary

```
╔══════════════════════════════════════════════════════════╗
║           BANK CUSTOMER CHURN ANALYSIS — RESULTS        ║
╠══════════════════════════════════════════════════════════╣
║  Dataset          : 10,000 customers | 11 features       ║
║  Churn Rate       : 20.4%                                ║
║  Features Selected: 6 (statistically validated)          ║
║  Best Model       : Decision Tree (CTree)                ║
║  Accuracy         : 85.7%                                ║
║  F1-Score         : 61.8%                                ║
║  Top Predictor    : Age (41–60 age group)                ║
║  Key Action       : Re-engage inactive members first     ║
╚══════════════════════════════════════════════════════════╝
```

---


## 🙋 Author

**Sweta Mehta**  
  

---


