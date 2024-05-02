## detectPVC - detect premature ventricular complexes with Polar H10

[![R-CMD-check](https://github.com/kbroman/detectPVC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kbroman/detectPVC/actions/workflows/R-CMD-check.yaml)

R package to detect premature ventricular complexes (PVCs) in data from a [Polar
H10](https://www.polar.com/us-en/sensors/h10-heart-rate-sensor) chest-strap heart rate sensor.

We have used the [ECGLogger app](https://www.ecglogger.com/) on an iPhone
to extract the Polar H10 data as a CSV file (or really a series of CSV
files, in one hour blocks), and the [rsleep](https://rsleep.org/)
package to detect R peaks in the ECG signal.


---

### Installation

You can install the detectPVC package from
[GitHub](https://github.com/kbroman/detectPVC).

You first need to install the
[remotes](https://remotes.r-lib.org) package.

```{r}
install.packages("remotes")
```

Then use `remotes::install_github()`:

```{r}
library(remotes)
install_github("kbroman/detectPVC")
```

---

### Usage

The library comes with a ~40 sec sample data set, `h10`.

```{r}
library(detectPVC)
data(h10)
```

Convert the included times (which are time stamps from a Polar H10, in
1e-9 seconds) to a standard date-time values.

```{r}
h10$datetime <- convert_timestamp(h10$time)
```


Use `detect_peaks()` to detect R peaks in the ECG trace.

```{r}
peaks <- detect_peaks(h10$ecg)
```

Plot the data, and add points above the peaks. The function
`plot_ecg()` is a base-graphics-based plotting function to mimic a
traditional ECG trace.

```{r}
plot_ecg(h10$datetime, h10$ecg)
points(h10$datetime[peaks], rep(1, length(peaks)), pch=16, col="slateblue")
```

Use `calc_peak_stats()` to calculate some statistics about each peak.

```{r}
peak_stats <- calc_peak_stats(peaks, h10$ecg)
```

The statistics `pmin` and `leftRR` seem particularly good for
identifying the PVCs.

```{r}
par(las=1, mar=c(4.1, 4.1, 0.6, 0.6))
plot(peak_stats$pmin, peak_stats$leftRR, xlab="peak min", ylab="left RR")
```

Label the inferred PVCs with pink dots, and the others with green
dots.

```{r}
plot_ecg(h10$datetime, h10$ecg)
pvc <- (peak_stats$pmin > -0.45)
points(h10$datetime[peaks], rep(1, length(peaks)), pch=16, col=c("green3", "violetred")[pvc+1])
```

In this 40 second window, there are 5 PVCs in 40 total beats.

---

### License

[detectPVC](https://github.com/kbroman/detectPVC) is released under the
[MIT license](LICENSE.md).
