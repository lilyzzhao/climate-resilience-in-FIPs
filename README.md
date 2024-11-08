# climate-resilience-in-FIPs


### If you want to build all data from scratch follow step 0 and step .5 otherwise skip and go to step 1:

1. **Step 0 (initial processing)**
   - Run `Step_0_processing.R` to process raw data and create base datasets.
   - **Input files:**
     - `data/meta_data/component_meta_data.csv`
     - `data/meta_data/attribute_meta.csv`
     - `data/meta_data/indicator_meta.csv`
     - `data/raw_data/final_raw_components.csv`
   - **Output files:**
     - `data/processed_data/df_tier0.csv` (Attribute-Component-Indicator level)
     - `data/processed_data/tier1_data.csv` (Attribute-Indicator level; used for Figure 1)
     - `data/processed_data/tier1_nozero.csv` (Attribute-Indicator level with zeros removed)
     - `data/processed_data/Tier_2_combined_only.csv` (Assessment level with overall coverage scores using ERA+SRA indicators; generally useful)
     - `data/processed_data/Tier_2_all.csv` (Assessment level; used for Figure 3)

2. **Step 0.5 (further processing)**
   - Run `Step_0.5_processing.R` to generate data for Figure 2.
   - **Output file:**
     - `data/processed_data/Figure_2_data.csv`

3. **Step 1 (pre-analysis setup)**
   - Run `Step_1_load_before_analysis.R` to load themes, packages, and prepare data for analysis.

### Start with pre-processed data
If you prefer to skip initial data processing, start from **Step 1**:
- Use `Step_1_load_before_analysis.R` to set up the environment, load packages, and apply the theme.

### Run analysis scripts
After running Step 1, run any script in the `analysis` folder in any order as needed.
