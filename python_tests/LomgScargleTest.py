import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from astropy.timeseries import LombScargle
from datetime import datetime

# Load your data
jd, mag = np.loadtxt("TIC_229887190-Sector77.dat", unpack=True, comments="#")
f_min =  0.0002  # minimum frequency (1/day)
f_max = 10.0002  # maximum frequency (1/day)
f_step = 0.0002  # frequency step
frequency = np.arange(f_min, f_max, f_step)

t_start = datetime.now()
lombScargle = LombScargle(jd, mag, fit_mean=True, nterms=8)
power = lombScargle.power(frequency)
calc_time = datetime.now() - t_start
print("calc_time=", calc_time.total_seconds())

plt.figure(figsize=(8,4))
plt.plot(frequency, power)
plt.xlabel('Frequency (1/day)')
plt.ylabel('Power')
plt.title('Lomb-Scargle Periodogram (custom frequency grid)')
plt.grid(True)
plt.show()

# Peak frequency
best_freq = frequency[np.argmax(power)]
print(f"Best frequency: {best_freq:.4f} 1/day")

data = pd.DataFrame({
    "frequency": frequency,  # remove units for saving
    "power": power
})

# Save to CSV
data.to_csv("periodogram.tsv", sep="\t", index=False)
