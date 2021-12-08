# Reptiles of the Australian Capital Territory

This repository includes data extraction, data cleaning, data exploration and visualisation of species of the class "Reptilia" located in the Australian Capital Territory (ACT). The data was downloaded from [Atlas of Living Australia](https://www.ala.org.au/).


## Structure of the repository

### Data extraction, cleaning and exploration

The `data_exploration.R` code contains all the steps required to obtain a clean dataset of the reptiles located in ACT. The required libraries are:

```r
library(tidyverse)
library(lubridate)
library(janitor)
library(httr)
library(jsonlite)
library(inspectdf)
```

The final data is in an RData format and contains the following variables:

- `decimalLatitude`: decimal latitude of the species recorded.
- `decimalLongitude`: decimal longitude of the species recorded.
- `eventDate`: date of observation or specimen collection date.
- `month`: month of the observation or specimen collection date.
- `scientificName`: scientific name of the species recorded.
- `class`: class of the species recorded. In this case, class = "Reptilia".
- `dataResourceName`: data resource that supplies the record.
- `basisOfRecord`: nature of the record (e.g. specimen, human observation, fossil, etc).
- `state`: Australian States and Territories. In this case, state = "Australian Capital Territory".
- `forest2013`: forests of Australia 2013.

### Visualisation

The visualisation is in a Shiny app format. The app requires the following libraries:

```r
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(plotly)
library(htmltools)
```

The app was built in the `app.R` file and it requires `reptiles.RData`, `style.css` and the `www` folder. 

- `app.R`: contains the ui and server for the Shiny app.
- `reptiles.RData`: dataset of reptiles in the ACT, previously generated with `data_exploration.R`.
- `style.css`: CSS format for the application.
- `www`: contains the principal media utilised by the app. In this case, GitHub logo.

#### Running the app

**Option 1**

Install the dependencies required for the visualisation. Run the following in R studio:

```r
shiny::runGitHub("ala_reptiles", "fmaron")
```

**Option 2**

1. Clone the GitHub repo
2. Open `app.R` in RStudio
3. Click on "Run App" at the top right corner of the script pane


## Limitations

- The visualisations were generated with species that had a valid scientific name.
- The number of occurrences versus type of forest plot does not consider forest change over time. The type of forest is for 2013.
- The number of occurrences versus month does not display dates with NA values.

