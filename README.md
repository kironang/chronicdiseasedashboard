# McLennan County Dashboard
This repository provides an organized system for visualizing common public health datasets. Local health districts can use the code in this repository to create online dashboards for public access. Although the current state of the repository showcases methods for McLennan County, the manual below contains instructions for simple transitions to other counties in the United States.

Version 0.2.0, 2025-July-21:
```
├── LICENSE
├── README.md
├── dashboard
|     ├── app.r
├── data
|     ├── DiabetesAtlas_CountyData (1).csv
|     ├── DiabetesAtlas_CountyData (2).csv
|     ├── DiabetesAtlas_CountyData (3).csv
|     ├── DiabetesAtlas_CountyData (4).csv
|     ├── DiabetesAtlas_CountyData (5).csv
|     ├── DiabetesAtlas_CountyData (6).csv
|     ├── DiabetesAtlas_CountyData (7).csv
|     ├── DiabetesAtlas_CountyData (8).csv
|     ├── DiabetesAtlas_CountyData (9).csv
|     ├── DiabetesAtlas_CountyData.csv
|     ├── data.csv
|     ├── data.r
|     ├── indicators.csv
```

Kiron created this public health dashboard for a Summer 2025 internship with the Waco-McLennan Public Health District. It relies on the R programming language and the Shiny application framework. It also features the GNU Affero General Public License v3.0.

Kiron tested the code in this repository with version 4.5.1 of the R programming language. Additionally, Kiron compiled the data from several different online sources:
- https://gis.cdc.gov/grasp/diabetes/diabetesatlas-surveillance.html
- https://chronicdata.cdc.gov/Heart-Disease-Stroke-Prevention/Rates-and-Trends-in-Heart-Disease-and-Stroke-Morta/7b9s-s8ck
- https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/dv4u-3x3q
- https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/pqpp-u99h
- https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/duw2-7jbt
- https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/h3ej-a9ec
- https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb
- https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation
- https://www.countyhealthrankings.org/health-data/methodology-and-sources/data-documentation/national-data-documentation-2010-2023

This repository represents the code corresponding to the version listed above. View this repository online at https://github.com/kironang. Kiron dedicated the rest of this document to instructions and guidance. Please contact Kiron at kiron_ang1@baylor.edu if you have any questions.

# User Manual
Good morning! Thank you for choosing this repository as a solution for your dashboard needs. My name is Kiron, and I'll be providing you with some guidance on how to maximize your county dashboard experience. First, I'll address any questions about testing the dashboard on your own computer, and then, I'll focus on what to do if you want to deploy the dashboard to an online website.

I've added images where appropriate to improve your experience. Please let me know if anything is outdated, incorrect, or unhelpful by contacting me at kiron_ang1@baylor.edu. Finally, please keep in mind that I firmly support open source information; I encourage you to do the same by publishing your code and data publicly, or at least by mentioning this repository.

One last note: This manual assumes that you have basic familiarity with R and RStudio. Additionally, I wrote this manual with my colleagues in mind, and all of them use Windows. Again, if anything is unclear, please send me an email and I'll be happy to help. With introductions out of the way, we will now begin!

## How can I run the dashboard on my computer?
Please read all of these instructions before deployment! In this section, not only do I provide instructions for local testing, but also I explain the data preparation needed to incorporate additional datasets.

First, open this website with your favorite web browser: https://github.com/kironang/mclennancountydashboard. Click on ``<> Code``, then click on ``Download ZIP``. See the image below for more information.

![Screenshot of https://github.com/kironang/mclennancountydashboard showing users where the download button is](image-8.png)

Open the folder where the ZIP file went and extract all its contents. A new directory called ``mclennancountydashboard-main`` should appear. 

![Screenshot of Windows File Explorer showing users that another directory is created after extracting the contents of the ZIP file](image-1.png)

At this point, please open RStudio and navigate into the folder until you see the README.md file.

![Screenshot of RStudio interface showing the repository's files](image-2.png)

At this point, you can already run the app, especially if you're just interested in McLennan County; just click into the ``dashboard`` folder and then open the ``app.r`` file. Wait a few seconds; if a yellow message appears and asks you to install some libraries, click ``Install``. Once everything is installed, click on the ``Run app`` button.

![Screenshot of RStudio interface showing users where the run button is](image-3.png)

Once the app starts to run, you should immediately see the dashboard appear in a separate window.

![Screenshot of the dashboard running locally on my computer](image-4.png)

Congratulations! You've successfully tested out the dashboard on your own computer! If you want to change the indicator names, units, or categories, open and edit the indicators.csv file in Excel (or any other application that can open and edit CSV files easily). 

![Screenshot of Excel interface showing users the structure of the indicators.csv file](image-5.png)

This magical data structure made it super easy for me to merge different datasets together. Each row represents a specific indicator present across the four datasets, and each row must belong to one of the following categories:

- Population Health
- Access to Care
- Childcare and Education
- Demographics
- No Category
- Mortality
- Diseases and Lifestyle Conditions

The first three columns — ``indicator``, ``unit``, and ``source`` — give information on the original measurements that were present in the datasets provided. All the other columns can be modified; the ``data.r`` script will use this information to modify and transform the data accordingly. The ``new_indicator`` 

If you're in a different county, delete the following files:
- data.csv
- indicators.csv
- All the ``DiabetesAtlas`` files

All the ``DiabetesAtlas`` files were downloaded from this CDC website after specifically selecting data for McLennan County: https://gis.cdc.gov/grasp/diabetes/diabetesatlas-surveillance.html. If you would like to incorporate this dataset, please specify your county on the website and download the CSV files you want to include. Then, move the CSV files back into the ``data`` folder. If you don't need this diabetes data, then just go to the ``data.r`` file and change ``use_diabetes_atlas <- TRUE`` to ``use_diabetes_atlas <- FALSE``.

Since you probably have ``data.r`` open already at this point, please change ``county <- "McLennan"`` and ``state_abbreviation <- "TX"`` as needed. Now, your ``data`` folder should be completely empty, except for a ``data.r`` file. In RStudio, click ``Source`` to run all the code in the ``data.r`` file. An indicators.csv file will be created for you. Edit the indicators.csv file as described above, and then source ``data.r`` again.

## How can I deploy the dashboard to a public-facing website?

This is the fun part! Sign up for an account at https://shinyapps.io; you should see a screen like the image below. Do NOT pay any money. You can do this for free.

![Screenshot of the shinyapps.io website interface where users can see their name, token, and secret](image-6.png)

Take note of the name, token, and secret. To deploy the dashboard, you need to edit the ``deploy.r`` found in the same directory as this ``README.md`` file. Swap out the strings for your information and then source the script in RStudio. Again, if you see a yellow warning asking you to install the necessary library, please click ``Install``.

It might take a few minutes to deploy the app; this is completely normal. At some point, though, you should see a message like the one below to inform you of the completed deployment:

![Screenshot of the RStudio console interface demonstrating that the app has been successfully deployed to https://kiron.shinyapps.io/dashboard](image-7.png)

Amazing work! You've finished deploying the dashboard online! Please bear in mind that there are some usage limits to the shinyapps.io platform, at least if you're using the free plan. I still recommend using only the free plan, because for most use cases, the free plan is sufficient.

## How do I add more datasets to the dashboard?

Now, it's time to address the most important question! Let's say that you want to add a new dataset; let's pretend that this dataset is named ``Dataset A``. I will be very blunt and say that, unless you feel comfortable with the R programming language, the easiest method for adding a new dataset is to convert ``Dataset A`` to a CSV file like ``dataset_a.csv``, format it so it looks similar to the data in ``dashboard/data.csv``.



## Does the "description" column do anything? What about the "subcategory" column?

The "description" column is currently useless. In the future, I plan to add a section for paragraphs under the graph, and that's where the text in the description column would go. The "subcategory" column is not necessary to fill out, but you can use it to add another dropdown for further organization.

[add more here about the subcategory column]




