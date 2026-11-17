# Does Prompt Language Shape AI Interpretations of Political Controversies in Turkey?

**Ezgi Ecem Yılmaz | Columbia University | Fall 2026**

## Overview

This randomized experiment tests whether Turkish versus English prompts affect GPT-6 Astra's normative evaluations of 30 political controversies in Turkey (2008–2025). Each event received one independent prompt in each language, yielding 60 responses. Human coding is primary, with GPT-6.1 Sol used for reliability assessment.

**Main result:** Turkish responses were modestly less critical (ATE = 0.200, SE = 0.074, 95% CI [0.048, 0.352], two-sided randomization p ≈ 0.030). Six event pairs differed in valence.

## Files

- `essay.pdf`: Final report.
- `essay/`: LaTeX source and four figures.
- `PAP.Rmd`: Pre-analysis plan.
- `01_collect.R`, `02_ai_code.R`, `03_analyze.R`: Collection, coding, and analysis scripts.
- `main/`, `pilot/`, `ai_coding/`: Archived API outputs.
- `analysis_results/`: Results and figures.
- CSV files and `coding_rubric.md`: Event lists, assignment, and coded data.

## Replication

Open `LLM_Language_Practicum.Rproj` in RStudio and run:

`source("03_analyze.R")`

`analyze_main("human_coding_final.csv")`

No API calls are required to reproduce the analysis.

**Preregistration:** https://osf.io/he7u4/
