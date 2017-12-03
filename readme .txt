The google drive link is https://drive.google.com/open?id=0B2CPJvg5zun_VUJ6VTBDRUZUOTg

1. all related R scripts and a file readme.txt specifying the function of each R script

a. Library Used:
XML = To extract the xml attributes from the pages. 
rvest = We have used rvest library to read and prase the HTML pages. 
stringr = To find and replace the particular characters to get desired result.
Rselenium = To extract the price history and tax history as we were not able to extract same using the Xpath of elements.

b. Script Description :
1. As zillow.com only shows 520 records at a time we had to apply price filters accordingly to get exact upto 500 houses for each iteration.
2. After applying the price filter we get upto 520 houses and we used that URL to crawl all of the pages.
3. The URL for the Zillow home page after applying the filter has to be put in baseUrl manually each time before running the code.
4. The number of pages can be different for all filters, thus the number of pages should be updated manually to get 
   all individual page URl . Update this number in the for loop.
The data thus recovered has to be written to a txt file. 
5. To extract specified 22 fields, we have used individual url of pages(url for each house) and Xpath to get different information from the different nodes.
6. We have created multiple nodes (facts,others,neighbour,school,pricenode) to extract information for particular house.
7. Then these nodes are passed into the related function which returns the vector of that specified fied.
8. As the nodes will return entire tag we have used grep function to get the relative information.
9. Finally, we have created data frame of these fields.

c. Instructions to run the code
1. Run all libraries used to avoid any errors.
1. Run and compile all objects used for the code.
2. Compile all the functions used.
4. Then run the for loop give by title get all data
 


 
