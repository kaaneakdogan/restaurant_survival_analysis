INTRODUCTION

This is a research project on the probability of survival of restaurants. I used Yelp Public Dataset to analyze survival probabilities of restaurants based on the number of reviews posted in 1.11 km coordinate grids. I found that 7 to 15 reviews per restaurant in a grid results in the best survival probability. I hypothesize that this is due to the fact that higher competition does not necessarily counter higher demand the same way medium-level competition counters medium-level demand. When there are a lot of businesses with a lot of people, demand is most likely not distributed evenly, causing some businesses to fail while others flourish. Whereas in regions where medium-level competition meets medium-level demand, businesses have a more evenly-distributed chance of grabbing a customer's attention. And this leads to better survival probabilities overall. 

VISUALS

Go to: https://github.com/kaaneakdogan/restaurant_survival_analysis/blob/main/conclusion/Visual%20of%20Survival%20Chance%20vs%20Review%20Density%20Quantile.jpeg

SIGNIFICANCE OF THIS PROJECT

Read: https://github.com/kaaneakdogan/restaurant_survival_analysis/blob/main/Why%20This%20Matters.md

RESULTS:

For more detailed analysis of results, read: https://github.com/kaaneakdogan/restaurant_survival_analysis/blob/main/conclusion/results

HOW TO REPRODUCE

The data is sourced from the following website:
https://business.yelp.com/data/resources/open-dataset/

After getting the TAR file from the website, extract the files yelp_academic_dataset_business.json and yelp_academic_dataset_review.json

Once you have those files you can run your code. Please make sure that you have the required packages listed in the beginning of the code.

To run the code, first run the python code after running 
all_business <- business |>
  select(business_id, name, categories, latitude, longitude, stars, review_count)

After running the python code, you can proceed with the rest. 

As a note, some functions in MASS and dplyr conflict, not allowing for certain functions to be executed once the glm_prediction script
is ran in Rstudio. To run some functions in all_quant_analysis after running glm_prediction, disable the MASS package. 
