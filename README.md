# Practicum: Does Prompt Language Shape AI Interpretations of Political Controversies in Turkey?

**Experimental Research Practicum | Columbia University | Fall 2026**

Ezgi Ecem Yılmaz

## Overview

This randomized experiment examines whether Turkish versus English prompts influence a large language model's normative evaluations of politically controversial state actions in Turkey.

The study covers 30 purposively selected controversies from 2008 to 2025. Each event was submitted independently in Turkish and English to GPT-6 Astra (high reasoning effort), yielding 60 responses. Language assignment to request order was randomized within event pairs.

Responses were evaluated using a standardized coding rubric. Human coding serves as the primary measure, with independent coding by GPT-6.1 Sol as a supplementary reliability check.

## Main Findings

- **Human-coded average effect (Turkish − English):** +0.200 on a three-point valence scale (SE = 0.074; 95% CI: [0.048, 0.352]).
- **Two-sided randomization inference:** p ≈ 0.030, using 10,000 within-event permutations.
- **AI-coded average effect:** +0.100 (p ≈ 0.250).
- **Intercoder agreement:** 57/60 responses (95%; Cohen's κ = 0.848).

Turkish responses received less critical evaluations in six event pairs, with identical scores in the remaining 24. The findings suggest differences in evaluative framing rather than a uniform reversal of political positions.

## Preregistration

The pre-analysis plan was registered on OSF before the collection of main experimental outcomes.

**OSF:** https://osf.io/he7u4/

The repository includes the RMarkdown pre-analysis plan (`PAP.Rmd`), experimental materials, response data, coding files, and analysis scripts.

## Repository Contents

**Study design and data collection**
- `PAP.Rmd`: Pre-analysis plan and prespecified analysis procedures.
- `main_events.csv`: The 30 experimental controversies and bilingual event descriptions.
- `pilot_events.csv`: Four excluded pilot controversies.
- `main_assignment.csv`: Realized randomized assignment schedule.
- `01_collect.R`: Prompt construction, randomization, API data collection, and response logging.
- `main/` and `pilot/`: Archived main-study and pilot API responses.

**Outcome coding**
- `coding_rubric.md`: Outcome definitions and coding criteria.
- `human_coding_final.csv`: Primary human-coded data.
- `02_ai_code.R`: Independent supplementary AI-coding procedure.
- `ai_coding.csv`: Consolidated AI-coded outcomes.
- `ai_coding/`: Archived AI-coding outputs and prompts.
- `rubric_used.md` and `coder_settings.txt`: Coding documentation.

**Analysis and results**
- `03_analyze.R`: Data validation, paired estimation, randomization inference, coder agreement, and visualizations.
- `analysis_results/`: Generated estimates, comparison tables, and figures.

## Reproducing the Analysis

Open `LLM_Language_Practicum.Rproj` in RStudio and set the project directory as the working directory.

Run:

1. `source("03_analyze.R")`
2. `analyze_main("human_coding_final.csv")`

The analysis reads the archived human and AI coding files and writes output tables and figures to `analysis_results/`.

The original API collection and AI-coding scripts are also provided for reproducibility. Re-executing those scripts requires API access, appropriate credentials configured through R's `keyring` package, and the response storage path specified in `01_collect.R`. Reproducing the reported estimates does not require new API calls.

## Scope

The experiment identifies language-related differences under the specified model, prompt templates, and collection conditions. The purposively selected controversies are not representative of all political events, and the design cannot independently identify differences in model training data as the underlying mechanism.
