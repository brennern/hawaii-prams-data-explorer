# Hawaii PRAMS Data Explorer: An Interactive R Shiny App
This repository contains the code used to build the "Hawaii PRAMS Data Explorer", an R {[shiny](https://shiny.posit.co/)} app. The data utilized in this app come from the Hawaii pregnancy risk data from the Pregnancy Risk Assessment Monitoring Systems (PRAMS) survey dataset. Through this app, we explore and analyze a wide range of variables across decades of PRAMS data, including Phase 4 through Phase 8 (2000 - 2023).

## The PRAMS Data
The PRAMS program collects health, behavior, and maternal experience data from preconception into the early infancy of the child. Through random selection via birth certificantes of newly born infants, approximately 200 new mothers are mailed the PRAMS survey each month across the Hawaiian islands. The overall goal of the PRAMS program is to positively influence maternal actions throughout the pre-pregnancy and post-pregnancy experience for the reduction of infant morbidity and mortality. Our analysis aims to explore the Hawaii PRAMS data and analyze trends in specific variables over the course of the past two decades.

## App Features
- **About Page**: Landing page includes usage instructions and a GitHub link for full transparency.
- **Trend Analysis Tab**: Visualize changes in maternal health indicators over time, stratified by race, education, and SES.
- **Logistic Regression Tab**: Explore survey-weighted multivariate regression models with adjustable inputs and downloadable results.
- **Download Center**: Export plots in PNG, PDF, or HTML format.
- **Interactive Help**: Tooltips and question-mark icons provide guidance for users throughout the app.

## How to Run This App Locally
1. Clone this repository:
```bash
git clone https://github.com/brennern/hawaii-prams-data-explorer.git
```
2. Open the project in RStudio.
3. Run `shiny::runApp()` from the root directory.

## Our Paper
The methods, results, and public health implications of this app are described in our manuscript:
> **Maternal Health Trends and Disparities in Hawai‘i: Visualizing PRAMS Data Through an Interactive Shiny Application**  
> Submitted to the *Hawaiʻi Journal of Health & Social Welfare*, 2025.  
> [Link to paper (once available)]

## Data Acknowledgment
This project uses data from the [PRAMS program](https://health.hawaii.gov/fhsd/home/hawaii-pregnancy-risk-assessment-monitoring-system-prams/), conducted by the Center for Disease Control and Prevention. To access PRAMS data, researchers must acquire a Secure Access Management Services (SAMS) account and accept a data sharing agreement on the PRAMS portal.








