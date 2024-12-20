# STAT451-Final
### Research Question:
> **How do various socioeconomic and demographic factors affect crime rates across the United States?**

The United States has a relatively high crime rate compared to other developed countries such as China, Japan, Singapore, etc. The US has a rate of around 381 incidents per 100,000 people in 2022. According to the general public, most Americans do not feel safe in America.

There are many reasons and factors that contribute to the high crime rate, and we can’t simply pin down one factor. Therefore, we aim to find the relationship between health, economics, social factors, and crime rate. We will be exploring multiple datasets that contain information of these intertwined factors, such as crime rate by county, violent crime rate by state, demographic descriptions and crime, unemployment rate, health and opportunity metrics, and other social factors. We intend to highlight trends and associations between these areas of interest, mainly their relationship with crime rate, but also may include analysis between these factors to better understand the multifaceted impact on one another.

We have identified 6 datasets from various sources including Kaggle and the government websites that contain information about one or more of these factors for further analysis and visualization. These datasets are available as csvs, which will alow us to easily import them into R for analysis. As our question is fairly open ended and allows room for exploration into the data, our question will able to be answered from multiple perspectives and angles using these datasets. Some have redundant information, like crimedata and crime-rates-by-us-state, but they also contain unique info that can add depth to our analysis, such as income, population, or interesting methods of identifying places in need of extra opportunity. Our intention with adding many datasets is it allows us the ability to explore the data, identify interesting associations that tell a story, and visualize this to convey a message. To answer our question, it will consist of trial and error, exploring which factors show influence in crime, as well as influence on one another. 


## Murder Rate Bar Graph
Represents the bar graph of murder rates by states in descending order. Murder rate here is defined as the number of murders per 100,000 population of a state.
This gives us a spatial understanding of where, specifically in which states, the most violent crime is occuring. We can see that Georgia, Mississipi and Florida 
are the highest in the list. While Maine, New Hampshire and North Dakota are among the lowest at less than 2.5 murders per 100,000.

## Rape Rate Bar Graph
Represents the bar graph of rape rates by states in descending order. Rape rate here is defined as the number of rapes per 100,000 population of a state.
This gives us a spatial understanding of where, specifically in which states, the most rapes are occuring, giving us insights into women safety. 
We can see that Nevada, Alaska and California are the highest in the list, with Nevada having a staggering 46 rapes per 100,000. While Rhode Island, Maine, North Dakota are among the lowest at less than 8 rapes per 100,000.

Comparing the murder and rape bar graphs, we notice that rapes are much more common than murders (by a factor of 2 to 3).

## Assault Rate Bar Graph
Represents the bar graph of assault rates by states in descending order. Assault rate here is defined as the number of assaults per 100,000 population of a state.
This gives us a spatial understanding of where, specifically in which states, the most assaults not amounting to murder are occuring. Assaults are far more common than both rape and murder.
Also, North Carolina surprisingly leads the assault bar graph, despite not featuring in the top 3 for either rape or murder.

## Medicare Breakdown by Race for Each State Histogram 
Represents the histogram of people who are enrolled in Medicare by state, distinguishing the proportions of each state by race (white, black, hispanic, and other). The histograms are located by total enrollments in descending order. This plot helps highlight how many people (with their demographics) are enrolled in a federal health insurance plan, which can give insights into health equity, as well as economic status due to the need for people at a lower economic status to pursue government sponsored programs. In this you can see that states with higher populations (CA, FL, TX, NY) have the most enrollements, which makes sense, but it also helps us understand which people in states are enrolling in these programs. We hope to try map this to crime rate in order to test the association that people needing federal health insurance (lower class) see higher incidents of crime. 

## Number of violent crimes vs. unemployment rate Scatterplot
From the visualization, we can see that there is a loose correlation between average unemployment rate and average number of violent crimes across the country. Specifically, states that has relatively lower unemployment rate is expect to have lower number of violent crimes that includes murder, rape, robbery and assault We also observe an outlier that has extremely high average number of violent crimes, which could worth further investigation. However, the connection between unemployment rate and number of violent crimes is not clear by just observing the pattern from the plot. For example, there are some states that have high unemployment rate but low average number of violent crimes.
