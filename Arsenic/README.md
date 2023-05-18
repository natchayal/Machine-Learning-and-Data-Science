# Abstract
The State of California is one of the hotspots most affected by the natural presence of 
Arsenic in groundwater. One of the problems with studying Arsenic is the high cost of accurate
measurement. This study aims to develop a model that would reliably predict the Arsenic content 
of a water sample based on other more easily determined characteristics of the water system or of
the water itself. A dataset of water samples from the Water Quality Portal from the National Water 
Quality Monitoring Council website (https://www.waterqualitydata.us), was downloaded as a .csv 
file. On this interactive website, basic data filtering was completed to only select the physical and 
chemical water quality data for wells in the state of California. In the dataset provided, the 
concentrations of several common ions had been quantified, including Arsenic. Several regression 
models were trained on the data and it was found that Potassium (K) is the parameter most closely 
associated with Arsenic presence in this dataset and a Random Forest Model using K concentration
and a few other water quality parameters as input variables performed exceptionally well.
However, this study needs to be repeated with a wider range of samples to determine if the results 
and model could be validated and generalized
