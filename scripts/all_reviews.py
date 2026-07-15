import json
import csv

# -------------------------
# Step 1: Load business IDs
# -------------------------

business_ids = set()

with open("data_raw/yelp_academic_dataset_business.json", "r") as f:
    for line in f:
        b = json.loads(line)
        business_ids.add(b["business_id"])

print(f"Total businesses: {len(business_ids)}")

# -------------------------
# Step 2: Stream reviews to CSV
# -------------------------

input_reviews = "data_raw/yelp_academic_dataset_review.json"
output_csv = "all_reviews.csv"

with open(input_reviews, "r") as infile, open(output_csv, "w", newline="", encoding="utf-8") as outfile:
    writer = None
    count = 0

    for line in infile:
        review = json.loads(line)

        if review["business_id"] in business_ids:
            if writer is None:
                writer = csv.DictWriter(outfile, fieldnames=review.keys())
                writer.writeheader()

            writer.writerow(review)
            count += 1

        if count % 500_000 == 0 and count > 0:
            print(f"Wrote {count:,} reviews...")

print(f"Finished. Total reviews written: {count:,}")
