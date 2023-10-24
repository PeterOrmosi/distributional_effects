# distributional_effects

This repo contains the code used for the paper "Does increasing concentration hit poorer areas more? A study of retail petroleum markets"

Download data into a local folder from https://drive.google.com/drive/folders/1k9DzgtAqEoePaOrIl7PdUgvgwfL10Vht?usp=sharing


1_building_df.R 

Loading data, creating weekly petrol prices, linking them to local characteristics, deseasoning time series. It creates a table of petrol station level weekly prices in a long table format. It also adds the number of rivals within 1, 2, 5 miles (note that in the code the variables are named as 'km', but these are milages).

2_adding_min_prices_same_brand_traffic.R 

Adding area minimum prices, adding information on the number of same brand other stations in the area, adding traffic data.

3_selecting_treatment_and_control.R

Creating treatment and control groups. It generates the treatment and control groups, using nearest neighbour mathching. Running the loop takes a while. It looks at each treatment petrol station (finds stations that had a drop or increase in the number of rivals within 1, 2, 5 miles - by default this is looking at rivals within 1 miles), and decides whether there was another exit/entry within +/-26 weeks. It discards those exits/entries that had another exit/entry within this interval. It than choses 5 similar firms. The number of neigbours can be changed in the Matchit command line.

4_raw_data_graphs.R 

Create raw data time series plots for treatment and control group. 

5_summary_tables.R

Generate descriptive tables and figures in Sections 3 and 4, and Appendix B

6_regression_did.R

Generate regression table: this runs a simple unit fixed effects difference-in-differences model using R's plm package (Section 5.2 in paper)

7_cf_exit.R
7_cf_entry.R

Main causal forest results (Section 5.3 in paper)

8_simulation.R

Simulation example from Appendix


The traffic_data folder shows how the traffic data was created from the original publicly available source file.
