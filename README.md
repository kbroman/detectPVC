## detectPVC - detect premature ventricular complexes with Polar H10

[![R-CMD-check](https://github.com/kbroman/detectPVC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kbroman/detectPVC/actions/workflows/R-CMD-check.yaml)

R package to detect premature ventricular complexes (PVCs) in data from a [Polar
H10](https://www.polar.com/us-en/sensors/h10-heart-rate-sensor) chest-strap heart rate sensor.

I'm using the [ECGLogger app](https://www.ecglogger.com/) on an iPhone
to extract the Polar H10 data as a CSV file (or really a series of CSV
files, in one hour blocks), and the [rsleep](https://rsleep.org/)
package to detect R peaks in the ECG signal.


---

### Installation

You can install the detectPVC package from
[GitHub](https://github.com/kbroman/detectPVC).

You first need to install the
[remotes](https://remotes.r-lib.org) package.

    install.packages("remotes")

Then use `remotes::install_github()` to install R/GNapi.

    library(remotes)
    install_github("kbroman/detectPVC")

---

### Usage

The library comes with a ~40 sec sample data set, `h10`.

    library(detectPVC)
    data(h10)

We can add a column with the time in seconds from the first
measurement.

    h10$time_sec <- (h10$time - h10$time[1])/1e9

We use `detect_rpeaks_sliding()` to detect R peaks in the ECG trace.

    peaks <- detect_rpeaks_sliding(h10$ecg)

Plot the data, and add vertical lines at the peaks

    plot(h10$time_sec, h10$ecg, type="l")
    abline(v=h10$time_sec[peaks], lty=2, col="red")

Use `get_rpeaks_stats()` to calculate some statistics about each peak.

    peak_stats <- get_rpeaks_stats(peaks, h10$ecg)

The statistics `pmin` and `leftRR` seem particularly good for
identifying the PVCs.

    plot(peak_stats$pmin, peak_stats$leftRR)

We can then label the 5 PVCs with pink dots, and the others with green
dots.

    plot(h10$time_sec, h10$ecg, type="l")
    pvc <- (peak_stats$pmin > -0.45)
    points(h10$time_sec[peaks[pvc]], peak_stats$pmax[pvc], pch=16, col="violetred")
    points(h10$time_sec[peaks[!pvc]], peak_stats$pmax[!pvc], pch=16, col="green3")

In this 40 second window, there are 5 PVCs in 40 total beats.

---

### License

[detectPVC](https://github.com/kbroman/detectPVC) is released under the
[MIT license](LICENSE.md).
