---
title: "healthiar: An R package to quantify and monetize health impacts attributable to (environmental) exposure"
tags:
  - Health impact quantification
  - Burden of disease
  - Health risk assessment 
  - Attributable health impacts
  - Monetization
  - Environmental exposures
  - Air pollution
  - Noise
authors:
  - name: Alberto Castro
    orcid: 0000-0002-4665-3299
    equal-contrib: true
    affiliation: "1, 2" 
  - name: Axel Luyten
    orcid: 0000-0002-7005-5889
    equal-contrib: true
    affiliation: "1, 2" 
  - name: Arno Pauwels 
    orcid: 0000-0001-7519-8080
    affiliation: 3
  - name: Liliana Vázquez Fernández 
    orcid: 0000-0003-3778-9415
    affiliation: 4 
  - name: Gianni Ardielli
    orcid: 0009-0000-3408-2217
    affiliation: "1, 2" 
  - name: Iracy Pimenta 
    orcid: 0000-0003-0032-1536
    affiliation: 5
  - name: Susanne Breitner
    orcid: 0000-0002-0956-6911
    affiliation: 6
  - name: Carl Baravelli
    orcid: 0000-0001-7772-5315
    affiliation: 4
  - name: Vanessa Gorasso
    orcid: 0000-0001-6884-9316
    affiliation: 3
  - name: Maria Lepnurm
    orcid: 0009-0009-4372-6227
    affiliation: 7
  - name: Andreia Novais 
    orcid: 0009-0007-7775-108X
    affiliation: 5
  - name: María José Rueda-López 
    orcid: 0000-0002-2443-1038
    affiliation: 8
  - name: Pham Minh Nhat 
    orcid: 0000-0002-5972-1668 
    affiliation: 4
  - name: Ana Barbosa 
    orcid: 0000-0002-9623-9002
    affiliation: 5
  - name: João Vasco Santos
    orcid: 0000-0003-4696-1002
    affiliation: 5
  - name: Anette Kocbach Bølling
    orcid: 0000-0003-4209-7448
    affiliation: 4
    
  
affiliations:
 - name: Swiss Tropical and Public Health Institute, Switzerland
   index: 1
 - name: University of Basel, Switzerland
   index: 2
 - name: Sciensano, Belgium
   index: 3
 - name: Norwegian Institute of Public Health, Norway
   index: 4
 - name: University of Porto, Portugal
   index: 5
 - name: LMU Munich, Germany
   index: 6
 - name: Tervise Arengu Instituut, Estonia
   index: 7
 - name: Centre Scientifique Et Technique Du Batiment, France
   index: 8
   
date: 19 February 2026
bibliography: paper.bib

---

# Summary
Health impacts attributable to exposures, such as ambient air pollution or noise, 
can be quantified (and monetized). However, such assessments can be time-consuming
in terms of programming and often lack transparency in methodology. 
The open-source R package `healthiar` contributes to solving these challenges by 
providing functions to quantify (and monetize) attributable health impacts. 
It offers multiple calculation pathways and options, including iteration across 
multiple geographical units and stratification by subgroups. 
Epidemiologists and public health professionals 
can benefit from using `healthiar` for health impact quantifications. 


# Statement of need
Environmental exposures, e.g., ambient air pollution, are major contributors to 
disease and mortality worldwide [@GBD2023]. Epidemiologists and public health researchers or 
practitioners quantify the (negative or positive) health impacts that are 
attributable to (no) changes in exposure to inform policy making. 

The general methodology for quantifying health impacts has been documented 
in the literature [@Lehtomaki2025]. However, studies/reports of specific assessments are not 
always transparent or complete about the specific methods used and the assumptions made, 
hindering reproducibility and plausibility checks. 
Additionally, the calculation can become tedious and error-prone, 
also when programming, e.g., in R [@RCoreTeam2023]. 

The open-source R package `healthiar` quantifies (and monetizes) 
health impacts attributable to exposure. `healthiar` contributes to the 
comparability and transparency of results by using established methodology 
approved by international experts and making it publicly available. 
The package is available on the Comprehensive R Archive Network (CRAN), 
with a development version accessible via a public 
[GitHub repository](https://github.com/SwissTPH/healthiar)
and provides online documentation 
(see [package website](https://swisstph.github.io/healthiar/)). 

`healthiar` enables multiple calculation pathways and options. 
Health impacts can be quantified using a) relative or absolute risk, 
b) single or age-specific baseline health data, 
c) fixed-shape (e.g. linear, log-linear…) or user-defined exposure-response functions, 
d) population-weighted mean or categorical exposure. 
Additionally, `healthiar` offers the option to specify disability weight, cutoff 
and confidence interval, stratify by socio-economic group, iterate assessments 
across geographical units, compare scenarios, summarize uncertainty 
using Monte Carlo simulation, monetize health impacts or consider social 
inequalities in exposures. While `healthiar` focuses on ambient air pollution 
and noise exposure, it can potentially be used for other exposures 
(e.g., green spaces or chemicals). 


# State of the field 
R packages similar to `healthiar` are `rhap` [@Sampedro2025] and `ithimr` [@Abbas2023]. 
`rhap` quantifies health impacts attributable to household air pollution based on 
predefined scenarios, while `healthiar` focuses on ambient air pollution (not indoor), 
allowing users to enter any exposure scenario. 

`ithimr` is the R package of the ITHIM (Integrated Transport and Health Impact Modelling) tool. 
It quantifies health impacts attributable to air pollution, traffic safety and 
physical activity resulting from changes in walking and cycling mobility. 
Thus, the main user input data of `ithimr` is mobility-related, 
whereas `healthiar` requires (changes in) exposure. 

The World Health Organization (WHO) tool HEAT (Health-Economic Assessment Tool 
for walking and cycling) [@Gotschi2020] also quantifies health benefits of walking and cycling, 
but using an online R-Shiny web application. 
The offline WHO tool AirQ+ [@Amini2024] focuses on ambient air pollution, but not noise. 
None of these WHO-tools have an R package counterpart, 
which hinders integration into R scripts or tools.

# Software design 
The functions included in `healthiar` enable a modular workflow \autoref{fig:workflow}. 
Functions for additional analyses, such as modification of existing scenarios, 
scenario comparisons, or multi-exposure assessments, use `healthiar` 
outputs as input data. 
In other cases, for instance, for monetization and social inequalities, 
the health impact can be entered either using `healthiar` outputs 
or directly from user input. Furthermore, smaller helper functions covering preparatory steps or 
single calculation steps are available. 

We refrained from providing default or background input data 
(e.g., exposure data, exposure-response functions, or baseline health data), 
as they a) would be incomplete given the many different exposures and health 
outcomes and b) would become quickly outdated, and updates would be time-consuming. 
Nevertheless, the R package includes example datasets that allow users to explore 
the functions.

Input data for `healthiar` functions are entered as separate arguments using 
vectors instead of within a data set/frame. This has two objectives: 
a) to facilitate simple assessment without creating a table, and 
b) to explicitly identify input data, avoiding mismatches.   


The use of tidy data [@Wickham2014] is supported and encouraged by `healthiar`. 
Thus, each row in the user data set corresponds to a unique stratification level 
(geographical unit, social group…), and each column to a function argument. 

Methodological decisions have been taken together with experts from the European 
project BEST-COST [@EC_Burden_2025]. Currently, 
more than 350 internal tests check the correct behavior of `healthiar`, including 
the correctness of its results.

# Research Impact Statement
Still as a non-public prototype, `healthiar` was firstly presented at
the scientific conference Urban Transitions in November 2024 [@Castro2024]. 
In September-October 2025, shortly after the public release on GitHub, 
two international online workshops introduced `healthiar` to around 110 participants. 
`healthiar` was published in [CRAN](https://cran.r-project.org/web/packages/healthiar/) 
on 11 November 2025 and, eight months later, had been downloaded over 
2,000 times [@Csardi2026].  

Within the European research project BEST-COST (where `healthiar` was developed), 
the package was adopted to quantify health impacts attributable to air pollution and noise
in case studies (publications in preparation). External 
adoption of `healthiar` (beyond BEST-COST) further demonstrates its significance 
as a research tool:
- The European research project UBD-Policy [@EC_Urban_2025] also adopted `healthiar`
for case studies on air pollution, e.g. in Barcelona [@Cussotto2025]. 
- Other researchers deployed `healthiar` to quantify 
health impacts attributable to noise 
in Europe [@Engelmann2026] and Germany [walsch2026].

Additionally, new international research projects and existing exposure assessment tools
have expressed interest in adopting `healthiar` to quantify health impacts.

# AI usage disclosure
We only used artificial intelligence (AI) in a few cases for code optimization 
and debugging, rather than extensively. 
The whole code was internally reviewed by the two main developers. 

# Acknowledgements
`healthiar` has been developed under the framework of European project BEST-COST, 
which has been funded by the European Union’s Horizon Europe program 
under Grant Agreement No.101095408. 
We also appreciate the feedback of all test users.

# Author contributions
AC and AL are the main authors of `healthiar` and this manuscript. 
AC, as creator, led code development and AL was code co-developer.

The rest of the JOSS manuscript authors are contributors to `healthiar`
(methodological advice and systematic testing) and 
provided feedback on the manuscript. 
AP, CB and VG are also co-authors of helper functions.

# Figures
![Workflow of healthiar functions in June 2026. An up-to-date version is available on the [healthiar website](https://swisstph.github.io/healthiar/).](cheatsheet_healthiar_1st_page_june_2026.png){#fig:workflow}

