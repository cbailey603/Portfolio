---
title: "Sales Analysis and Visualization Group Project"
layout: post
post-image: /assets/images/team_1/top10_cities.png
output: pdf_document
tags:
- Tableau
- R
- Visualization
description: "This is my final group project for my software tools course from graduate school. The goal of this project was to visually display and analyze Iowa alcohol sales, within a context of a professional environment. More specifically, our group was to generate Tableau dashboards that clearly demonstrated and highlighted aspects of the Iowa alcohol market, and provide recommendations to the management of a hypothetical company entering that market."
---

# Background
<p>
This was group project aimed at demonstrating sales data analysis, visualization skills, and teamwork via a GitHub repository. The software utilized was primarily R and Tableau. The following report is a collaboration between myself and my two team members.
</p>

# Introduction 
The analysis within this report was requested by our Marketing and Sales department in the context of helping them decide how we as a liquor distributor should enter the Iowa market. The following report begins by discussing general demographic and sales data related to the Iowa market. The the report focuses in on per capita sales across geographies within Iowa (counties, cities and zip codes). Specifically, there are 3 key questions the report aims to bring clarity to.

1. What is the distribution of per capita sales across geographies? 
2. What are the ranks of top 10 geographies for per capita consumption across liquor categories?
3. Are there any outliers that have high per capita sales in on ly one specific liquor category?

These questions will be answered within the Data Analysis portion of the report. The Conclusion section will serve as a summary of our analysis. The Policy Recommendation portion will highlight our recommendations for ways to efficiently enter the Iowa market. Lastly, data analysis was completed using R and visualizations were completed in Tableau. We believe R is a more robust data analysis platform and Tableau offers great visualization capabilities. This report will not include any code snippets. For those interested in a code review please refer to our R files in the repo.

# General Demographic and Sales Data Analysis

## Demographic and Income Analysis

The following visualization points out that the Iowa market is predominately WHITE aounting to 90.5% of the population. We also analyzed the distribution of median income across counties in Iowa. We see that the median income of all counties in Iowa hovers around $31,000 with a standard distribution with few outliers. This indicates that income on a county level is evenly disbursed.

![Race/Ethnicity and Income Analysis](/assets/images/team_1/ACS_visualization.png)

## Sales and Consumption Analysis

The data analyzed in the below charts contains sales data from 2012 to 2016, measuring both dollar and volume sales, and demographic data for all regions of Iowa. The data is also segmented into three layers: zipcode level, county level, and city level. The focus of the analysis in this report is on per capita consumption and sales, so the first step of our analysis is to merge the demographic data with the sales data. This is done in R as opposed to Tableau, as R allows greater control of how the datasets are merged and the creation of the needed per capita variables. 
Demographic Analysis:

![General Sales analysis of County Level Across Categories ](/assets/images/team_1/General_County.png)

![General Sales analysis of City Level Across Categories ](/assets/images/team_1/General_City.png)

Across City and County level, the sales data projected similar pattern as Whisky, Vodka and Rum dominated the most part of the sales, followed by Misc, Tequila, Brandy, Schnapps, Gin, etc. The rankings acroos the top 10 counties with most sales in dollars and most sales in volume stayed constant, with Polk, Linn and Scott county taking the first three across dollar sale and volume sale.

City sales saw identical ranking in popularity of product categories. However, there is change of ranking between sale in dollar and sale in volume. Cities including Waterloo, Iowa city and West Des Moines swtiched places, the reason of this swtich in places could due to the fact that Vodka's retail prices are lower but sold in greater volume, hence causing the change in ranking, besides this, the pattern of the city level analysis stayed constant with the county level.

# Per Capita Sales/Volume Data Analysis

This section of our report will breakdown per capita sales and consumption across geographies. 

## Per Capita Sales Distribution

We see in the following charts how per capita sales are distributed across counties, cities and zip codes. Per capita sales is defined as total sales per geography / total popluation. We see within the visualizations that per capita sales begins to skew towards smaller amounts as we become more granular with the geographies. For instance, with city distribution of per capita sales we see a high amount of cities with less than $50 of per capita sales per year. The reason being is that as our locations become more granular we have less sales dollars and less population. This level of granularity is helpful to identify outliers where greater sales potential lies. As we see in the zip codes graph that are a few zip codes with very high per capita sales that could represent a potentially lucrative entry market. More to come on high per capita geographies.

![Per Capita Sales Distribution Across Geographies](/assets/images/team_1/percapita_sales_distribution.png)

## Per Capita Consumption

### County Level

![Top 10 Counties per capita consumption](/assets/images/team_1/top10_percap_count.png)

As we can see from the graph, the most popular alcohol category remains to be Whisky, Vodka and Rum. The county with the highest per capita consumption across all category combined, is Dickinson County, which surprisingly, is not on the Top 10 counties by total sales in both dollar and volume. This could mean that the county is generally small in population, but has a very large amount of alcohol sales in volume comparing to is population, which gives us a very interesting county, and a potential marketing experiment target.

### City level

![Top 10 Cities per capita consumption](/assets/images/team_1/top10_percap_city.png)

Similar to the county level, the most popular alcohol is still Whisky, Vodka and Rum. Additionally, the city of Bevington, which is not on the Top 10 cities by total sales and volume, is the highest on this ranking. Upon research, as 2010 census shows, this city only has 63 people and 28 households, which explains its exponentially high per capita comsumption, and the city of Wesley shares a similar situation, hence it would be more reasonable to conduct the marketing activities at larger cities like Waterloo or Iowa City.

### Zipcode Level

![Top 10 Zipcodes per capita consumption](/assets/images/team_1/Top10_percap_zip.png)

The top 3 zipcodes are within Bevington, Cedar Rapids and Des Moines repectively. Since we have already ruled out the feasibility of conducting experiment at Bevington, we can firstly plan to begin the experiment at the most popular alcohol shopping destinations within the corresponding zipcodes of the City of Cedar Rapids and Des Moines.

### Analysis of Per Capita Consumption

The most popular liquor category remained constant throughout the county, city and zipcode level, the geographies with most per capita consumption also aligns with the general sales analysis data, supporting the assumption of the popularity of Whisky, Vodka and Rum. Out of all the geographies, Dickinson County and Bevington city stood out, as they have the highest per capita consumption in their geographical category respectively, while not being in the top 10 of overall sale in dollars and volume. This could mean that the drinking culture in these two places is more profound comparing to other counties and cities in the state of Iowa.

## Per Capita Sales

To determine and visualize the top geographies by per capita dollar sales, we use Tableau instead of R for a few reasons. Firstly, Tableau enables us to visually depict the per capita sales data on a map of Iowa by employing the longitude and latitude measures, which are unique to the program. Secondly, we can combine multiple visualizations into a dashboard that can be collectively filtered, which, as discussed later in the report, is helpful when examining the geographic trends for specific alcohol categories. Lastly, and more generally, Tableau has intuitive and powerful formatting tools, which allow our visualizations to be aesthetically pleasing. 

In Tableau, for all three levels of geography, the per capita dollar sales are mapped onto a map of Iowa, with their size and color gradient, indicating how large sales per capita are in that region.  This map is combined in a dashboard with a top ten list bar chart, with the visual color breakdown highlighting the makeup of different alcohol categories in that region. Finally, a filter is added to the dashboard to examine the per capita dollar sales of specific alcohol categories and the corresponding top ten regions. The results for all three levels of geography can be seen below 

### Top 10 Cities:

![Top 10 Cities per capita](/assets/images/team_1/top10_cities.png)

### Top 10 Counties:

![Top 10 Counties per capita](/assets/images/team_1/top10_counties.png)

### Top 10 Zipcodes:

![Top 10 Zipcodes per capita](/assets/images/team_1/top10_zipcodes.png)

### Analysis of Per Capita Sales:

The intuitive choice regarding where to enter the alcohol market in Iowa may be to choose areas with the highest population density or total sales, given that these areas have the largest potential customer base. However, as seen from the analysis above, the areas with the highest total dollar sales do not necessarily correlate with the highest per capita sales of alcohol. For example, Des Moines, Cedar Rapids, and Davenport are the three largest cities in Iowa and have the highest total dollar sales. However, they are not in the top ten cities for alcohol sales per capita. High per capita sales indicate a substantial demand for alcohol in a region. This fact makes the regions highlighted in the visualizations above potentially good regions to enter the Iowa market. Furthermore, the analysis can be drilled down to the alcohol category level, and regions with the highest per capita sales in a particular category can be identified and marketed to, specifically for that alcohol type. 

### A Notable Outlier

When examining the top cities for every alcohol category, a particular city stands out for sales of distilled spirits. In Mount Vernon, there is a much higher sale of distilled spirits per capita than in any other city in the state, and by some margin. This indicates a high demand for distilled spirits in Mount Vernon, making the region a potential target for marketing our distilled spirit brand. This is demonstrated in the screenshot below: 

![Mount Vernon - An outlier for Distilled Spirits](/assets/images/team_1/mount_vernon.png)

## Conclusion

When entering a new market, it is strategically important to understand that market comprehensively. To achieve this goal, this report merged demographic and sales data across geographic levels and determined regions with a history of substantial demand for our products. The analysis and visualization created empower management and other decision-makers with the knowledge necessary to allocate resources efficiently and confidently. 

The focus of this report is per capita sales and consumption throughout the levels of geography. The distribution of per capita sales is not uniform at the zipcode or city level and is heavily right-skewed, with a few outliers having high per capita sales. At the county level, the distribution level is more uniform but is still skewed slightly right. The top geographies by per capita consumption and sales are listed below:

#### Top 10 Geographies by per capita sales ($)

|Rank|Cities       |Counties     |Zipcodes
|----|-------------|-------------|--------
|1   |Wesley       |Dickinson    |52401   
|2   |Bevington    |Polk         |50033   
|3   |Mount Vernon |Cerro Gordo  |50314   
|4   |Spirit Lake  |Black Hawk   |51101   
|5   |Bancroft     |Johnson      |52166   
|6   |Arnold's Park|Scott        |50320   
|7   |Milford      |Carroll      |51331   
|8   |Floyd        |Linn         |52807   
|9   |Fort Atkinson|Kossuth      |50311   
|10  |Swisher      |Pottawattamie|50483   

<p>
</p>

#### Top 10 Geographies by per capita volume

|Rank|Cities       |Counties     |Zipcodes|
|----|-------------|-------------|--------|
|1   |Bevington    |Dickinson    |50033   |
|2   |Wesley       |Cerro Gordo  |52401   |
|3   |Mount Vernon |Polk         |50314   |
|4   |Spirit Lake  |Carroll      |52166   |
|5   |Bancroft     |Black Hawk   |51101   |
|6   |Milford      |Kossuth      |51331   |
|7   |Fort Atkinson|Linn         |50320   |
|8   |Arnold's Park|Scott        |50311   |
|9   |Floyd        |Johnson      |50483   |
|10  |Holy Cross   |Clay         |52807   |

<p>
</p>

While the analysis in this report is significant, it is limited to three years and does not enable a large-scale historical analysis of the trends within the region. Expended datasets that include more years could reveal further insights about the region as a whole and about particular areas within Iowa. Additionally, a breakdown of the sales by date, as opposed to total annual sales, would enable an analysis of seasonal dynamics, improving our marketing and resource allocation throughout the year. 

## Policy Recommendation

The recommendation of this report is to begin entering the Iowa market in areas with high per capita consumption in sales and then, depending on the success within these regions, expand into areas with higher population density and total sales overall. This tiered approach will better ensure a low-risk but profitable breakthrough into the market. This is because, in regions with smaller total sales but high per capita sales, we do not need to invest as much capital and supply when entering the market to meet the demand. However, because the per capita sales are high, we can expect a return on that investment, resulting in a profitable enterprise. Then, once our relationships with our customers and our brand recognition are established, we can expand into the high total sales markets. This way, we reduce our potential risk and provide a roadmap to increase our market share. 

# Data and Files

[All data and files can be found on my Github](https://github.com/cbailey603/Team_1)
