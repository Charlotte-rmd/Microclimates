------------------------------------->> Microclimate modelling on R <<-------------------------------------------

# 0 - Intro

Here I develop the different steps required to model microclimate in Wytham Woods using Ilya McLean's R package : microclimF.
I would advice you have a look at the R guide provided here : https://rpubs.com/ilyamaclean/1248406, which is the one I followed. 



# 1 - Gathering the inputs - 

  ## A. Meteorological data

The following parameters are required to run the model : 

Air temperature at 10 m high, 
Atmospheric pressure (kPa), 
Total precipitation (mm), 
Relative humidity (%),
Shortwave radiation (W/m2), 
Diffuse radiation (W/m2), 
Total downward radiation (W/m2) radiation, 
Wind speed (m/s) and 
Wind direction (degrees


## See Era5 code 

Several possibilities, whether taking :
 - Radcliff Observatory data on air temperature and precipitation (daily)
 - ERA5 analysis dataset by the European Centre of Medium-Range Weather Forecasts (ECMWF) 
   You can find all the information regarding this dataset following this link : https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview
  the advantages of this datases are : 1) Hourly resolution, 2) Easy download, 3) Dowload directly from R using the mcera5 package (Klinges et al., 2022).

I choose the ERA5 dataset as it appered more suitbale because you can directly obtain all the information in the right format.
If you choose to download it from online, be carefull about the units of the parameters. 


   ## B. Environmental parameters


1) DTM, Canopy height = > LiDAR Imagery : https://environment.data.gov.uk/dataset/13787b9a-26a4-4775-8523-806d13af58fc
   ### See Veg code 


3) Soil-types + vegetation types => UAV-derived RGB-imagery (Georgios Voulgaris, Ella F. Cole, Sam J. Crofts, and Ben C. Sheldon)
   ### See Soil code 

#### * 1rst bias  *
3) Soil reflectance : approximation based on bibliography such as this article : https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0270629

4) To compute the other vegetation parameters I used the function vegfromhab() from the microclima package ?
   This function estimates leaf transmittance, reflectance, mean diameter, angle distribution, max stomala conductance.

   You must provide the Plant Area Index (PAI) for this function. The safest way to do so is using LiDAR Point Program Cloud data, but there are no data on the Wytahm 31km x 31km cell.
   Therefore, I estimated PAI as you can see in : 
   ### See Veg code

   I also estimate canopy clumpiness using the function clumpestimate, but I had to actually modify the function to make it work, as
   ### See Veg code


# 1 - Running the model - 

 Few things to be careful of : 

 - Check that all the inputs have the same extent and resolution
 - Check time is in UTC in the weather data


  ## A. Run the point model 

  No real difficulty, not supposed to take more than 2 min.
  

  ## B. Run the whole model

  





















   
