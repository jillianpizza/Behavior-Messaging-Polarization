# Comparing Campaign Rhetoric and Legislative Behavior Through Issue-Specific Polarization in the U.S. House of Representatives, 2021–2025

This repository contains the data and code used in the paper *"Comparing Campaign Rhetoric and Legislative Behavior Through Issue-Specific Polarization in the U.S. House of Representatives, 2021–2025."*

## Overview

This project examines the relationship between campaign rhetoric and legislative behavior among members of the 117th and 118th U.S. House of Representatives. Campaign text is analyzed using the Wordscores text-scaling method, while legislative behavior is analyzed using a Bayesian Issue-Specific Item Response Theory (IssueIRT) model. The analysis compares polarization patterns across policy domains and evaluates the extent to which campaign messaging aligns with subsequent voting behavior.

## Data

### Roll-Call Voting Data

* `HouseOnly118.csv` – Roll-call votes from the 118th House of Representatives obtained from Voteview.
* `H117_rollcalls_FULL.csv` – Roll-call votes from the 117th House of Representatives obtained from Voteview.

Both datasets include an additional `issue_code` variable containing Congressional Research Service (CRS) policy classifications used to assign votes to issue areas.

### Party Platform Data

* `PartyPlatformText.xlsx` – Issue-specific sections of the 2020 Democratic and Republican Party platforms. Platform text was divided into policy domains corresponding to the CampaignView major policy categories.

### Additional Data

All other datasets used in this project are publicly available through their original sources.

## Code

### Text Analysis

* `WordscoresMessaging.R` – Performs preprocessing, Wordscores estimation, and visualization of campaign rhetoric and party platform text.

### Roll-Call Analysis

* `IssueIRTHPC.R` – Estimates issue-specific ideal points using a Bayesian IssueIRT model and generates the associated visualizations.

