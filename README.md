# memo-keywords-across-time

Generate datasets with statistically over-represented words from a series of time periods, and visualize them in word clouds in a Shiny app.

## Requirements

- Packages "tm" and "RWeka".

```R
install.packages(c("tm", "RWeka"))
```

- A set of novels placed in the directory "corpustxts".

```
├── corpustxts
│   ├── 1870_AT_DenVanvittigesBoernEllerSkurkenPaaNoerrebro.txt
│   ├── 1870_AndersenHC_LykkePeer.txt
│   ├── 1870_Anonym_KraemmersvendeOgSyjomfruerEllerPrincipalernesPengeskuffer.txt
│   ...
```

- The directory "rdata".


## Generate data

Data for the Shiny app are generated using "generate_keyness_data.R":

- Set working directory to the project directory.
- Make sure the directories "corpustxts" and "rdata" exist, the former filled with novel txts.
- Read all functions into R (i.e. run all code)
- Run make_data_chunks()

E.g.:

```R
setwd("/Users/phb514/mygit/memo-keywords-across-time")
install.packages(c("tm", "RWeka"))
# Run all code (e.g. opt+cmd+R)
make_data_chunks()
```


## Run Shiny app

- Copy rdata/signifdata.Rda to shinyapp/memo_keyness_over_time/rdata/signifdata.Rda
- Run `library("shiny")`
- Run `runApp('shinyapp/memo_keyness_over_time')` (or open app.R in RStudio and press "Run App").


## Upload app to shinyapps.io

Google how to activate your free shinyapps.io account. Once you're set, run

```R
rsconnect::deployApp('shinyapp/memo_keyness_over_time')
```

The app is now live at https://philip-diderichsen.shinyapps.io/memo_keyness_over_time

