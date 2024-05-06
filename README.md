# Act 135 Analysis + Mapping App

**To update:**
- Replace [Act 135 data](https://github.com/lizard12995/Act-135/blob/main/Act_135_Philly/raw/Act-135-Cases_2024.02.07.xlsx) (Act-135-Cases_2024.02.07.xlsx) in /raw with updated data
  - Name the new file "Act-135-Cases_[DATE].xlsx"
- Create a folder within Act_135_Philly folder called "raw2"
- Download OPA data as CSV from [here](https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/) and add to /raw2 folder
  - Make sure it's called "opa_properties_public.csv"
- Un-comment the code in lines 19 - 148 in global.R and re-run with new data
- Re-comment the code!
- Deploy the shiny app with rsconnect
- Be sure to NOT include raw2 when you push your code to GitHub
