### UBC STAT 545B - Assignment B3 & B4
##### Author: Joel Campbell

#### Description

This github repository is for STAT 545B assignment b3 and b4.

This assignment consists of the creation of a shiny app that provides a quick and convenient method for the exploration of a UBC curated fungi dataset. This includes the ability to filter and sort the data, and an auto-updating count of the total number of results based on the filters chosen.

The shiny app can be found at the following link: https://joelkcamp.shinyapps.io/UBCFungi/

#### Release 1.1

For assignment b4 I have added a multitude of features, including:
- a separate tab for data visualization
- two visualization methods (word cloud and bar chart)
- the capability to download the visualizations as png files (note that the word cloud visualization function is not current working on the published app, only remotely)
- an updated theme using a .css file
- a small image in the title row

This updated shiny app can be found at the following link: https://joelkcamp.shinyapps.io/UBCFungi2/


#### Data

The data used for this assignment was downloaded from the Consortium of Pacific Northwest Herbaria (https://www.pnwherbaria.org). The exact data set is loaded into the shiny app through a helper function using the following link: https://www.pnwherbaria.org/data/getdataset.php?File=UBC_Fungi_Native.zip
