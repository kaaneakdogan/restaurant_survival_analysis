# restaurant_survival_analysis
I used Yelp Public Dataset to analyze survival probabilities of restaurants based on the number of reviews posted in 1-mile coordinate grids. I found that 7 to 15 reviews per restaurant in a grid results in the best survival probability.

HOW TO REPRODUCE

The data is sourced from the following website:
https://business.yelp.com/data/resources/open-dataset/

After getting the TAR file from the website, extract the files yelp_academic_dataset_business.json and yelp_academic_dataset_review.json

Once you have those files you can run your code. Please make sure that you have the required packages listed in the beginning of the code.

To run the code, first run the python code after running 
all_business <- business |>
  select(business_id, name, categories, latitude, longitude, stars, review_count)

After running the python code, you can proceed with the rest. 
