# Biotelemetry of Amphibians and Reptiles
This repository accompanies the review titled **"How to track herps? A review of electronic telemetry techniques for monitoring Amphibians and Reptiles and future perspectives"**, 
which brings together literature relating to the tracking and movement ecology of herpetofauna. The study spans the years 1970 to 2023, covering key methodologies and technologies.

## Overview
Biotelemetry methods used to study the movement of amphibians and reptiles include:
* Radio transmitters
* Harmonic direction finders
* GPS devices

These methods have greatly improved our understanding of small-scale behaviors, such as habitat selection and movement patterns.

#### Methodology
To compile a comprehensive review, a systematic search was conducted using specific keywords (e.g., "tracking amphibian", "radio telemetry reptile", "GPS tracking reptile", etc.) across the first 10 pages of Google Scholar. A detailed table was created to extract relevant data from each study, including parameters such as species, transmitter type, attachment methods, monitoring duration, and effects on animals (e.g., injury and mortality rates).

This systematic approach allowed for the organization of key information from a broad range of studies, providing a comprehensive overview of the current state of biotelemetry research in herpetology.

## Repository Structure
The repository is organized into the following folders:
1. **Data**: contains the CSV file with the comprehensive table generated from the systematic review. This table includes key parameters from the reviewed studies, such as species, type of transmitter used, attachment methods, and other relevant data as described in the methods section of the review.

2. **Figure**: contains all the figures used in the paper, including visual representations of the key findings, graphs, and charts used to illustrate trends in the biotelemetry methods and geographic distribution of studies.

3. **Code**: includes two R scripts:
  - Analysis.R: this script performs the data analysis based on the data in the Data folder, helping to extract insights on trends in tracking methods, geographic representation, and other variables.
  - Figure.R: this script is used to recreate the figures included in the Figure folder, allowing for reproducibility of the visual elements used in the paper.