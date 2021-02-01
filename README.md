# Threddit - Wardrobe performance

### Project
"Threddit" is my personal project attempting to build a proper understanding of the real cost and eventually environmental impact of my use of clothes. The data set is a continuous daily log of my use of each piece of garment since 1 January 2018. In addition to showing the real cost per wear and frequency of use for each garment, this enables studying the correlation between price and quality (as measured by durability), as well as actual vs imagined use.

In order to be able to do the computing and visualisaion I wanted, I built this solution in R.

### Motivation
Visibility into item-level cost efficiency has been eye-opening. On a personal level, my main goal is not to drive down cost though, but to make better consumption choices especially from a sustainability perspective. In a broader perspective, I believe taking and enabling a data-driven approach to consumption and sustainability might be valuable at scale.

I publish my continously updated wardrobe performance on [hoverfalt.github.io](https://hoverfalt.github.io/).

For context and background, please see my blog post [Why I’ve tracked every single piece of clothing I’ve worn for three years](https://www.reaktor.com/blog/why-ive-tracked-every-single-piece-of-clothing-ive-worn-for-three-years/). 

### Tech and data pipeline
Thie project is built entirely in R. It uses the Google Sheets API to read use data, and the Dropbox API to host plots and animations for publishing.

The data collection UI and data storage is in Google Sheets. Data processing and computing is done locally, wich plots and animations being stored in Dropbox. The final results are published using R Markdown Websties on GitHub Pages, with the static content hosted on Dropbox.

### Contribute
I'm not a professional developer. I built this to be a tool for myself.

Due to increasing interest, I'm thinking about how to best go about making something like this available for broader good. If you have ideas or would like to contibute, please feel free to reach out :)  

### License
This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). CC BY, Olof Hoverfält 2018-2021