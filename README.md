# Act 135 Analysis + Mapping App

**To update:**

- First, get updated docket sheet from Jonathan Pyle. Then, do some manual manipulation to single out one respondent in the SING_RESP column. We need to isolate just one of sometimes MANY respondents, many of whom are corporate entitities. This is so that we can do the name analysis. Some general rules to follow:
  - You will need to figure out how to single out the new data that needs to be manually manipulated. The docket for some reason is not listed in date order. You could write code to do this or just continue doing it manually!
  - If there's an estate, the person whose estate it is should be the SING_RESP.
  - Otherwise, just use the first person listed!
  - If no person listed, leave SING_RESP empty. 
- Replace [Act 135 data](https://github.com/lizard12995/Act-135/blob/main/Act_135_Philly/raw/Act-135-Cases_2024.02.07.xlsx) (Act-135-Cases_2024.02.07.xlsx) or simply add new file in /raw with updated data
  - Name the new file "Act-135-Cases_[DATE].xlsx" and be sure to update this in your code when reading in files.
- Create a folder within Act_135_Philly folder called "raw2"
- Download OPA data as CSV from [here](https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/) and add to /raw2 folder
  - Make sure it's called "opa_properties_public.csv"
- Un-comment the code in lines 19 - 148 in global.R and re-run with new data
- Re-comment the code!
- Open the server.R file
- Deploy the shiny app with rsconnect using the blue button! Be sure to deploy to pclapps. Hit "publish" to deploy the app.
- Be sure to NOT include raw2 when you push your code to GitHub
