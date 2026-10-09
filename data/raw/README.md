# Local input exports

Download the respondent-level category exports from the [NECTAR Taste of the Industry 2025 / Palate Insights dashboard](https://www.nectar.org/sensory-research/2025-taste-of-the-industry) and save them here with these exact names:

```text
Bacon.csv
Bratwurst.csv
Breaded_Chicken_Filets.csv
Breakfast_Sausages.csv
Burgers.csv
Chicken_Nuggets.csv
Deli_Ham.csv
Deli_Turkey.csv
Hot_Dogs.csv
Meatballs.csv
Pulled_Pork.csv
Steak.csv
Unbreaded_Chicken_Filets.csv
Unbreaded_Chicken_StripsandChunks.csv
```

Keep the original header metadata and response labels. The script parses `[QUESTION_TYPE: ...]`, `[MODALITY: ...]`, `[RELATED_PRODUCT: ...]`, and product-code tags. Do not replace the respondent-level export with a summary dashboard download.

Raw exports are deliberately excluded from Git. Input hashes for the article's snapshot are recorded in `docs/input_manifest.csv`.
