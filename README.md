# Act 135 Analysis + Mapping App

**To update:**

- First, get updated docket sheet from Jonathan Pyle. Then, do some manual manipulation to single out one respondent. Some general rules to follow:
  - TBD
- Replace [Act 135 data](https://github.com/lizard12995/Act-135/blob/main/Act_135_Philly/raw/Act-135-Cases_2024.02.07.xlsx) (Act-135-Cases_2024.02.07.xlsx) or simply add new file in /raw with updated data
  - Name the new file "Act-135-Cases_[DATE].xlsx"
- Create a folder within Act_135_Philly folder called "raw2"
- Download OPA data as CSV from [here](https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/) and add to /raw2 folder
  - Make sure it's called "opa_properties_public.csv"
- Un-comment the code in lines 19 - 148 in global.R and re-run with new data
- Re-comment the code!
- Open the server.R file
- Deploy the shiny app with rsconnect using the blue button! Be sure to deploy to pclapps. Hit "publish" to deploy the app.
- Be sure to NOT include raw2 when you push your code to GitHub
