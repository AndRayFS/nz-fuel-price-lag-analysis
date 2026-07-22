# How Oil Price Shocks Reach New Zealand Fuel Prices

An investigation into how long it takes for a global oil price shock to reach
the pump in New Zealand — triggered by the 2026 Middle East crisis, and
checked against three earlier price shocks to see whether the pattern holds.

## Purpose

Compares the lag between crude oil price movements and NZ retail fuel prices
across six periods: two calm stretches and four price shocks spanning
New Zealand's transition from domestic oil refining (Marsden Point, closed
March 2022) to fully imported refined fuel.

## Data

[MBIE Weekly Fuel Price Monitoring](https://www.mbie.govt.nz/building-and-energy/energy-and-natural-resources/energy-statistics-and-modelling/energy-statistics/weekly-fuel-price-monitoring)
— public weekly series, 2004–2026: Dubai crude benchmark price (USD and NZD),
NZD/USD exchange rate, and retail petrol/diesel prices for New Zealand.

## Tools

R — `dplyr`, `ggplot2`, `tidyr`. Cross-correlation (custom implementation,
see note in script), linear regression.

## Method

For each of the six periods, correlation between crude price and retail
price is computed at every lag from 0 up to a data-driven maximum (not just
the best-fitting lag — the full profile is shown, so a sharp single peak can
be told apart from a flat or noisy one). Petrol and diesel are analysed
separately throughout, since they don't always share the same best-fit lag.

## Key findings

- During major oil shocks, NZ pump prices consistently reacted after about
  two weeks (r = 0.92 for the 2026 crisis, r = 0.84 for the comparable 2025
  US–China tariff shock)
- Outside major shocks, that relationship weakens sharply or turns negative
  — the two-week lag is a shock-specific effect, not a constant background
  relationship
- Importer margins on both petrol and diesel briefly fell to zero or
  negative during the sharpest week of the 2026 crisis, consistent with
  independent reporting ([Stuff, 26 Mar 2026](https://www.stuff.co.nz/politics/360955662/very-slim-margins-fuel-prices-are-rising-not-much-they-could-be))
  that diesel margins were close to zero in the first week of March
- The lag appears to have lengthened after Marsden Point refinery closed in
  2022 and New Zealand switched to fully importing refined fuel from Asia —
  2020 and 2022 show almost no lag; 2025 and 2026 (both post-closure) both
  independently land on ~2 weeks

## Repository contents

| File | Description |
|---|---|
| `fuel_analysis_v2.R` | Full analysis script |
| `weekly-table.csv` | Source data (MBIE) |
| `charts/prices_*.png` | Crude vs retail (petrol + diesel), one per period |
| `charts/scatter_*.png` | Crude vs retail at each fuel's own best-fit lag |
| `charts/margin_06_iranus_2026.png` | Importer margin, petrol vs diesel, 2026 crisis |

## Caveats

This is a casual analysis, not a controlled study. It does not isolate the
individual effects of the exchange rate, freight costs, or refining margins
from the crude price effect — overlaying NZD/USD specifically would be a
natural next step. Some periods (notably 2022) straddle a structural change
(the refinery closure) partway through the window, making them a messier
comparison point than the others.

## How to run

```r
install.packages(c("dplyr", "ggplot2", "tidyr"))
```
Then open `fuel_analysis_v2.R` in RStudio and run. `weekly-table.csv` must
be in the same working directory. Charts are written to `./charts/`.
