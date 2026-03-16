## Accompanying material for: _"The scientific impact of gender in behaviour, ecology, and evolution: authorship and citation performance"_

**Authors:** [Marina Papadopoulou](www.mpapadopoulou.com) & Andrea Afruni

---
The repository includes the code to reproduce the data collection, analysis, and figures of the manuscript.

### Code

Data collection is performed in Python, analysis and plotting are performed in R (v.4.4).

1. _collect_data.py_: given a list of journals in the data repository, it downloads .json files for the metadata of each retrieved paper from 2000-2020. The user needs to retrieve a personal API key from the Elsevier Developer Portal to download data, more info here: https://dev.elsevier.com/ 

2. _prepare_data.R_: goes through the downloaded json files, extracts the fields of interest, and cleans the collected data, saving everything into the *all_data.csv* file.

3. _identify_beh_journals.R_: goes through all abstracts in the *all_data.csv* file and calculates the similartity of words in the abstracts of _Animal Behaviour_ with other journals.

4. _stat_analysis.R_: performs the statistical analysis presented in the paper.
   
5. _figs.R_: reproduces all the figures (main and supplementary) included in the paper.

### Data

The data files include:
1. *journals_list.csv*: the list of journals for which to download data.
2. *all_data.csv*: for plotting, exported by the _prepare_data.R_ file
3. *data_for_stats.csv*: for statistical analysis and plotted.
4. *baseline_gbi_per_year.csv*: the GBI of all authors published in a year of our dataset.

### Citation:

Papadopoulou M. and Afruni A. (2026) The scientific impact of gender in behaviour, ecology, and evolution: authorship and citation performance. _Animal Behaviour_.

### Funding:

A pilot version of this study was supported by an Equal Opportunities Initiative Fund of the European Society of Evolutionary Biology (ESEB) awarded to M.P. (project BiasWatchEvol).

### Contact: 
- Dr.Marina Papadopoulou: m.papadopoulou.rug[at]gmail.com
