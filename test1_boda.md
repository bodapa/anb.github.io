------------------------------------------------------------------------

### Blog Post 1: Title

A few basic libraries are loaded initially:

    require(knitr)
    require(markdown)
    library(ggplot2)
    library(ISLR)
    library(pander)

Some text such as this goes in here...

    data("Hitters")
    ggplot(Hitters, aes(factor(Years), Salary)) + geom_boxplot()

    ## Warning: Removed 59 rows containing non-finite values (stat_boxplot).

![](test1_boda_files/figure-markdown_strict/unnamed-chunk-2-1.png)<!-- -->

    model1 <- lm(Salary ~ Years, data = Hitters)
    pander(model1)

<table>
<caption>Fitting linear model: Salary ~ Years</caption>
<colgroup>
<col width="25%" />
<col width="15%" />
<col width="18%" />
<col width="13%" />
<col width="13%" />
</colgroup>
<thead>
<tr class="header">
<th align="center"> </th>
<th align="center">Estimate</th>
<th align="center">Std. Error</th>
<th align="center">t value</th>
<th align="center">Pr(&gt;|t|)</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center"><strong>Years</strong></td>
<td align="center">37.71</td>
<td align="center">5.337</td>
<td align="center">7.065</td>
<td align="center">1.463e-11</td>
</tr>
<tr class="even">
<td align="center"><strong>(Intercept)</strong></td>
<td align="center">260.2</td>
<td align="center">46.64</td>
<td align="center">5.58</td>
<td align="center">6.012e-08</td>
</tr>
</tbody>
</table>
