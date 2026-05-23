# Place the raw CSV here

The Quarto report expects the file at:

    Take-home_Ex/Take-home_Ex01/data/insurance_raw.csv

The original Mendeley file uses semicolons; the code auto-detects the delimiter, so either format works.

After placing the file, run:

    Rscript Take-home_Ex/Take-home_Ex01/setup_packages.R   # one-time
    quarto render                                          # produce the site
