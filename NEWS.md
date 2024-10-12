## detectPVC 0.3.2 (2024-10-12)

- Revised find_peaks() to avoid extra called peaks by throwing one out
  when two peaks are so close that heart rate would be > 220 bpm.
  It now needs the times of the measurements, among the inputs, so
  this is a breaking change.


## detectPVC 0.2.5 (2024-10-03)

- read_multcsv can now read gzipped CSV files

- Added summary_states() to summarize the output from plot_states()


## detectPVC 0.2.1 (2024-09-28)

- Added function plot_states() for classifying PVC state as
  normal/bigeminy/trigeminy/omitted and making a plot


## detectPVC 0.1.16 (2024-05-26)

- A new package, to detect premature ventricular complexes (PVCs) in
  data from a Polar H10 chest-strap heart rate sensor.
