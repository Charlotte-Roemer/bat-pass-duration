Authors: Sarah Chayrigues, Yves Bas & Charlotte Roemer.  
These scripts were developed using the **[Tadarida](https://github.com/YvesBas/Tadarida-C)**  workflow.

# Calculate bat pass duration 
1) Run a [species classifier](https://github.com/YvesBas/Tadarida-C) to identify bat species in your sounds (you need to develop your own classifier or use another source)
2) Run 2_Calculate_bat_pass_duration_and_standard_deviation.R

# Associate bat species ID and feeding buzzes
1) Run a [species classifier](https://github.com/YvesBas/Tadarida-C) to identify bat species in your sounds (you need to develop your own classifier or use another source)
2) Run a [sonotypes classifier](https://github.com/YvesBas/Tadarida-C/tree/master/Sonotypes) to find bat feeding buzzes in your sounds (you can download the classifier on the link provided in the Readme)
3) Run 0_Associate_species_ID_with_buzz.R
4) Optional: run 1_Calculate_nb_buzz_per_night.R
5) Optional: run 3_Calculate_nb_bat_passes_per_night.R

# Study correlations between bat feeding buzzes and bat pass duration
1) Run all scripts above
2) Run 4_Create_final_dataset.R
3) Run 5_Model_Pippip.R


