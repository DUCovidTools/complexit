## Summary

This model provides short-term forecasts for each of the authority districts in the North East and North Yorkshire regions. These forecasts are usually seven to ten days. The forecasts come from comparing the daily cummulative incidence trends in the North East and North Yorkshire regions to the trends throughout the provinces in Italy, where the outbreak and its management are roughly two weeks ahead of the UK. The trends in the North East and North Yorkshire authority districts are also compared to the authority districts in the rest of England, which, where applicable, are also reported.

The process of matching authority districts in the North East and North Yorkshire with Italian provinces is done using a combination of classification techniques known as machine learning. Machine learning is a method of data analysis that automates analytical model building. It is a branch of artificial intelligence based on the idea that systems can learn from data in order to help public health and healthcare experts identify nonobvious patterns and trends, including infectious disease outbreaks.

The data used for running our classification methods come from [Public Health England](https://coronavirus.data.gov.uk/) and [Public Health Italy](http://opendatadpc.maps.arcgis.com/apps/opsdashboard/index.html#/b0c68bce2cce478eaac82fe38d4138b1). The starting date we use for classifying trends is the 9th of March, as it provides the most comprehensive dataset. Given that Public Health England recommends caution on any given week from using the most recent cumulative incidence rates, we model from the 9th of March to the current date of download, minus the three most recent days. Because the Italian provinces are roughly two weeks ahead of the outbreak in the UK, they offer a short-term forecast for their matching authority districts in the North East and North Yorkshire region. 
 



