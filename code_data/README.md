# ECON221A - INFORMALITY - PROJECT 

## Purpose 
Build a California-specific **synthetic sales tax base** at the **county** and
**state aggregate** level by combining observed taxable activity, county income-consumption proxies,
and tax rates. Then the observed tax base or implied revenue is compared to collected
revenue to quantify the extent of missing tax revenue. This missing tax revenue is consistent with 
noncompliance in consumption tax, or more broadly. informality. 

---

## Repository structure

├── .git/
├── .gitignore
├── .RData
├── .Rhistory
├── .Rproj.user/
├── code_data/
├── reading_presentations/
├── research_paper/
└── research_presentations/


## Descriptions of folders 

### research_presentations
Copies of each presentation related to informality research 

### research_paper
Final paper copy with date and attached version. 

### code_data
├── code_data/
  ├── 00-citations-raw.R
  ├── 00-startup.R
  ├── 01-prelim-data-analysis.R
  ├── 02-graphics-only.R
  ├── data-dicts/
  ├── econ221_fall2025.Rproj
  ├── inputs/
  ├── output-figs/
  ├── output-tables/
  ├── raw-copies/
  ├── README.md
  └── z-archive/
 
    
- `00-startup.R`: setup (paths, packages, globals)
- `00-citations-raw.R`: citation/bib utilities where I've just been listing the sources of relevance
- `01-prelim-data-analysis.R`: main data construction + synthetic tax base build (core pipeline)
- `02-graphics-only.R`: generates figures from constructed datasets for output - key results
- `inputs/`: inputs used by scripts, directly fed into 01-prelim-... (not raw originals)
- `raw-copies/`: raw source files copied in or downloaded from API (do not edit)
- `output-figs/`: exported figures for paper/slides
- `output-tables/`: exported tables (made in R) for paper/slides
- `data-dicts/`: variable dictionaries / metadata notes
- `z-archive/`: old versions, scratch, deprecated code


## How to run

Requires R (4.x) and RStudio; packages are loaded in `00-startup.R` and the scripts.

1. Open `econ221_fall2025.Rproj` in RStudio. This sets the working directory to the project root.
2. Run `01-prelim-data-analysis.R`. It sources `00-startup.R`, loads `inputs/`, and builds the California synthetic tax base at county and state level, writing `00_column_dictionary.csv`, `00_year_ranges.csv` and the merged county tax base `cty-level-estimated-taxbase.csv` to `output-tables/`. It expects to run from `code_data/`; if the working directory is the project root, it changes into `code_data/` automatically (and stops with an error if that folder is missing).
3. Run `02-graphics-only.R`. It sources `00-startup.R` and `01-prelim-data-analysis.R` itself (so step 2 need not be run separately) and then draws the figures.
