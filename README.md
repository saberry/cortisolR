## Cortisol Analyses

Run 'initialInspectionCortisol' first.

```{r, eval = FALSE}
load("data/cortValues.RData")

cortDat = initialInspectionCortisol(cortvalues, "STAND", 0, "CPM")

cortDat = backEstimateCortisol(corTest, "STAND")
```
