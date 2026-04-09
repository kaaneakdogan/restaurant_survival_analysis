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

I made two logistic regression models, one suggesting a linear relationship, while the other suggesting a non-liner relationship. The non-linear model is a better model, suggesting that aggregated per-restaurant reviews are the best at the third quantile. 

I conclude that this is because high-review density is associated with high restaurant density. When there is high restaurant density in a grid, this shows higher competition. Higher competition, even though it is countered by high demand, is not equivalent to medium competition with medium demand or low competition with low demand. When people have limited attention spans, competition can not be countered with demand past a certain point. 

Further research is necessary to find why review per business variable is non-linear. 
