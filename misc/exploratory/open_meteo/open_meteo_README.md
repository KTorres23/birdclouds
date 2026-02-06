

To run the test_openmeteo.R, you need two things:

1. You need to download the bird data from Teams (Located in the `misc_files` folder, named `Birdweather.Cardinals.csv`) and put it in this open_meteo directory
2. You need to specify which dataset you want to work with:

```{r}
#################### SPECIFY WHICH SIZE OF DATASET YOU WANT TO RUN

# work with 20 stations
#test_birds <- birds[1:20, ] ######### RUN LINES 41 AND CODE UNDER LINE 112

# work with 5 stations
#test_birds <- birds[1:5, ]  ######### RUN LINES 41 AND CODE UNDER LINE 162
```

The sample output in [weather_batches](./weather_batches/) works with 20 stations, and ran code from Lines 41-109 and 162-255.

If you want to run a quick API call for a couple stations and keep the data in R (no exporting to CSVs), use the dataset with 5 stations  and run Lines 41-159