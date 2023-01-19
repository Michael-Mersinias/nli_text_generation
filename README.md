# nli_text_generation
Keep it Neutral: Using Natural Language Inference to Improve Generation

# Requirements:
- Python 3.8.10
- Pandas 1.3.5
- Numpy 1.21.6
- Transformers 4.25.1
- Torch 1.13.1+cu116

# Experimental Setup:
- Infantructure: Google Colab Pro+ (GPU: A100-SXM4-40GB)
- Computational Cost: 35 compute units
- Required Time: 3 hours

# Datasets
- Scarecrow_Initial_Dataset.csv: The original SCARECROW dataset, publicly available for research use at https://yao-dou.github.io/scarecrow.
- Experiments.csv: The GPT-J generations (both vanilla and per NLI strategy) used for experimental evaluation in section 3 of the paper.
- Holistic_Ratings_Annotation_Results.csv: Average holistic ratings (scale: 1-5) as assigned by human annotators.
- Error_Types_Annotation_Results.csv: Error Types (tags: Off-Prompt, Self-Contradiction, Incoherent, Redundant) as assigned by human annotators.
- scarecrow_table1_tidy.csv: The subset of the SCARECROW dataset, where model equals GPT-3 and temperature equals 1.

# Evaluation Code
- Requires the Scarecrow_Initial_Dataset.csv dataset as input.
- Data preprocessing functions: split_into_sentences, calculate_text_error_types, calculate_gen_text_nli_tag.
- NLI classification function: nli_final_comparison.
- Text generation functions: gen_text_from_vanilla_gptj, gen_text_from_nli_gptj.
- Results in reproducing Table 1, Table 2 and Figure 2 of Section 2, as well as the dataset scarecrow_table1_tidy.csv. It is also used for the GPT-J generations (both vanilla and per NLI strategy) present in Experiments.csv.

# Analysis Scripts
- table1.R: Results in reproducing Figure 3 of Section 2. Requires the scarecrow_table1_tidy.csv dataset as input.
- annotation_analysis.R: Results in reproducing Figure 1 and Figure 4 of Section 3. Requires the error_types_annotation_results.csv and holistic_ratings_annotation_results.csv datasets as input.
