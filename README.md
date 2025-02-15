# GamesPred

## Overview
GamesPred was developed to predict how a game released on Steam will be successful. This includes games not yet announced. The average number of concurrent players in 2 months after the release is used to measure a game's success. This has been calculated from steamcharts.com for roughly 4,200 games which are used to make predictions about new games. Free-to-play and Early Access titles were left out of the study and it is therefore recommended not to use this tool on them. In addition, only games from a developer/publisher with at least 2 games in the database can be reliably predicted. The application notifies you immediately if this condition is not satisfied.

## Installation
The project was tested on Windows 10 and Xubuntu 16.04.

Requirements:

Essential for predictions:  
R: https://cran.r-project.org/mirrors.html

Required for re-evaluation:  
Java: https://java.com/en/download/

Recommended:  
RStudio: https://www.rstudio.com/products/rstudio/download/

Download the project from GitHub to a desired location  

## Usage

### Predictions

#### Via RStudio
Using RStudio makes the whole process a lot more user-friendly and is therefore recommended.

* Install [RStudio](https://www.rstudio.com/products/rstudio/download/) if you have not yet done so.
* Open "app.R" in RStudio.
* Use "getwd()" in the console to check whether the working directory is set to the "GamesPred-master" project root folder.
* If not, go to Session -> Set Working Directory -> To Source File Location.
* Click "Run App" at the top of the code panel. Click "Yes" when asked to install shiny.
* RStudio will now install required packages. If the app refuses to launch, restart RStudio and try again as there may be a temporary problem with the CRAN repository.

#### Via Terminal
cd to the download location

    cd <path_to_download>/GamesPred-master

Note: There may be a problem with the "slam" package on unix-based systems, hence you may want to install it manually before proceeding:

    sudo apt-get install r-cran-slam

Install required packages:

Unix-based systems:

    sudo R -e "source('./Init.R'); Init()"

On Windows, you need to specify the R bin location. E.g.:*

    "C:\Program Files\R\R-3.3.2\bin\R.exe" -e "source('./Init.R'); Init()"

You can run the prediction app via

    R -e "shiny::runApp('./', launch.browser=TRUE)"

*Note: By default, R installs packages to "C:/Program Files/R/R-version/library" and will require administrator rights.

### Download/Evaluation:

Either run R in terminal or open RStudio with working directory set to the "GamesPred-master" project root folder

    source("./Evaluation.main.R")
    Evaluate()

#### "Evaluate" function description

Download and process data, create train-validation-test splits and perform evaluation

Usage:  
Evaluate(update = FALSE, split = c(0.60, 0.20, 0.20), mode = "reg", intervals = c(20), seed = 61, top.terms = 50)

Arguments:  
update - whether data should be updated/downloaded  (the whole process may take up to 2 days if no data is present due to API's traffic limitations)
split - how dataset should be split into train, validation, and test set  
mode - "reg" for regression or "class" for classification  
intervals - players are split into classes which are then used for classification and are also balanced  
seed - random generator seed  
top.terms - how many top terms (based on information gain) from a description's document-term matrix should be selected; all if top.terms = 0
pca.ncomp - if > 0, top.terms will be tranformed to this number of principal components

## Details

The following information was downloaded for each game on Steam (almost 10,000 games as of September 1st, 2016):

* Name  
* Developer  
* Publisher  
* Age Requirements  
* Release Date  
* Price  
* Short Description  
* Description  
* Platforms  
* Game/Steam Features  
* HW Requirements  
* Languages  
* Genres  
* Thumbnail  
* Screenshots
* User Tags  
* DRM Notice  
* Concurrent Players (steamcharts.com)  

Each game has its own folder with data and images. These are further processed into a single data table with new and adjusted attributes (e.g. each game has a record about its developer's/publisher's previous games, and languages are separated into multiple attributes). Only applicable records (not free-to-play, not Early Access, no crucial value missing) are allowed for further processing (about 4,200 games). The dataset is then split into training, validation, and test set, and further processing is performed on these splits (such as transforming descriptions into a document-term matrix).

Some statistics about the data can be found at: https://is.muni.cz/www/trnenym/GamesPred/statistics.pdf

The best results were achieved using SVM on continuous class attribute:

Correlation coefficient: 0.82  
Normalized Root Mean Squared Error: 58 %
