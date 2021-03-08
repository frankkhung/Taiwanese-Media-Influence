# Do people watch the news for the news? Or for their confirmation bias?
In this analysis, we use Taiwanese Communication Survey (TCS) to analyze the political behavior in Taiwan. The data can be downloaded through the following procedures

## TCS data
This data can be found at the following link: https://srda.sinica.edu.tw/datasearch_detail.php?id=1291 . The process of downloading the file is as follows:
1. You would have to register for the account on the website of Survey Research Data Archive and register as a student/researcher to obtain the file
2. After registering the file, you would have to apply for downloading.
3. After you applied for downloading, you can download the file from "My List"
4. Due to the reason that the file you download would be a raw data, you would have to run the file name called "data_cleaning.R" under the folder "scripts".
5. After running the file you would produce data files: "green.csv", "blue.csv", "neutral.csv", "media.csv", and "media_3.csv" in the "Input" file directory.

To reproduce the analysis from the report, open up __report.rmd__. This file contains the codes and the report content. 

# File Introduction
- __scripts/data_clearning.R__: the file for the user to clean the data and perform data manipulation
- __scripts/model.R__: the raw file for the codes that showing up in the __report.rmd__
- __report.pdf__: pdf format of the report
- __report.rmd__: like above mentioned, it is the file for reproducing the entire report. 
