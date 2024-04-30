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

Explanation to come.

---

### License

[detectPVC](https://github.com/kbroman/detectPVC) is released under the
[MIT license](LICENSE.md).
