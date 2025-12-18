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
- Requires R, RStudio 
- Open the project by launching `econ221_fall2025.Rproj` in RStudio 
- Run `01-prelim-data-analysis.R` first. This script automatically sources `00-startup.R` 
and executes the full data construction pipeline, including loading inputs and building
the California synthetic tax base at both the county and aggregate level. 
The only required change is to update the local file path pointing to the inputs directory.
- Modify the following line in `01-prelim-data-analysis.R` to match your local setup:

`owd <- getwd()`
# if current folder is not `'code_data',` move into it
`if (basename(owd) != "code_data")` `{`
  `target <- file.path(owd, "code_data")`
  `if (!dir.exists(target)) stop("Missing 'code_data' inside: ", owd)`
  `setwd(target)`
`}`
    
- Replace `code_data` with your local folder if needed. 
- Open `02-graphics-only.R`.
	•	This script reads the outputs created in Step 2 and generates all final visualizations
	•	Figures are saved to output-figs/ and are ready for use in the paper and presentations 

