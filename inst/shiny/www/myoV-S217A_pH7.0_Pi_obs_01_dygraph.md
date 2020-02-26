

```
## # A tibble: 1 x 2
##   `Number of Events` `Signal (V1/V2)`
##                <int>            <dbl>
## 1                 70             2.73
```

This is an interactive plot of the running variance and running mean.
The algorithm for event detection is a Hidden Markov Model and these are the data the model receives as input. The model is fitted with the EM algorithm and the
gray shaded regions are the binding events identified through state sequence decoding via the Viterbi alogorithm.



```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```




Below is an interactive plot of the raw data (model does not use this data).
The overlay is the Hidden Markov Model 'state' prediction multipled by several conversion factors.
These convert the x-axis (time) from 'windows' to 'data points' to 'seconds'.
The HMM state, baseline mean, and measured step size are used to scale the model overlay to each step and subsequent baseline level.


```
## Error in loadNamespace(name): there is no package called 'webshot'
```



Plots of the running mean vs. running variance.
This provides insight into how the model divided data into either the baseline or event populations.

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

