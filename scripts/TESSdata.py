# -*- coding: utf-8 -*-
"""
Created on Tue May 13 16:23:58 2025

@author: Maksym Pyatnytskyy, mpyat2@gmail.com
"""

import lightkurve as lk
import numpy as np
import matplotlib.pyplot as plt
import tkinter as tk
from tkinter import filedialog
import sys
import os

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    REVERSE = '\033[7m'

starName = None
search_result = None
current_nn = None
current_time = None
current_obs = None
current_obs_err = None
current_to_mag = None
current_lc_info = None

UNKNOWN_MAG = -99
DEFAULT_MAG = 0

def printError(msg):
    print(bcolors.FAIL + msg + bcolors.ENDC)

def printHeader(msg, starInfo):
    print(bcolors.HEADER + msg  + bcolors.ENDC + bcolors.REVERSE + starInfo + bcolors.ENDC)

def printMenu(msg):
    print(bcolors.OKCYAN + msg + bcolors.ENDC)

def runCommand():
    cmd = input("Enter a command: ").strip()
    if cmd == "":
        return
    try:
        os.system(cmd)
    except Exception as e:
        printError(f"Error: {e}. Press ENTER to continue:")
        input("")
        return

def chDir():
    dir_name = input("Enter a directory name: ").strip()
    if dir_name == "":
        return
    try:
        os.chdir(dir_name)
    except Exception as e:
        printError(f"Error: {e}. Press ENTER to continue:")
        input("")
        return

def getStarName():
    starNameLocal = input("Enter a name or coordinates: ").strip()
    if not (starNameLocal is None or starNameLocal == ""):
        print("Querying a list of light curves...")
        global starName        
        global search_result
        global current_time
        global current_obs
        global current_obs_err
        global current_nn
        global current_to_mag
        starName = None
        search_result = None
        current_time = None
        current_obs = None
        current_obs_err = None
        current_nn = None
        current_to_mag = None
        plt.close('all')
        try:
            get_available_data_list(starNameLocal)
            if search_result is None or len(search_result) < 1:
                search_result = None
                printError("No data found for the star. Press ENTER to continue:")
                input("")
            else:
                starName = starNameLocal
        except Exception as e:
            search_result = None
            starName = None
            printError(f"Error: {e}. Press ENTER to continue:")
            input("")
            return

def normStrR(s, n):
  return s.rjust(n, " ")

def normStrL(s, n):
  return s.ljust(n, " ")

def printSearchResult():
    global search_result
    if search_result is None:
        return
    if len(search_result) < 1:
        printError("No data found for the star. Press ENTER to continue:")
        input("")
        return
    #print(search_result)
    print()
    print(bcolors.OKGREEN + bcolors.REVERSE + bcolors.UNDERLINE + normStrR("#", 4) + bcolors.ENDC + bcolors.OKGREEN + bcolors.UNDERLINE,
          normStrL("mission", 16),
          normStrR("year", 4),
          normStrL("author", 12),
          normStrR("exptime", 10),
          normStrR("target_name", 12),
          normStrR("distance", 12))
    for i in range(len(search_result)):
        print(bcolors.REVERSE + bcolors.UNDERLINE + normStrR(str(i), 4) + bcolors.ENDC + bcolors.OKGREEN,
              normStrL(search_result.mission[i], 16),
              normStrR(str(search_result.year[i]), 4),
              normStrL(search_result.author[i], 12),
              normStrR(str(search_result.exptime[i]), 10),
              normStrR(search_result.target_name[i], 12),
              normStrR(str(search_result.distance[i]), 12)
        )
    print(bcolors.ENDC)

# returns a sorted set of unique integers
def enterListOfNumbers(prompt, minN, maxN):
    ns = input(prompt).strip()
    if ns == "":
        return None
    ns = ns.split()
    try:
        nn = list(map(int, ns))
    except ValueError as e:
        printError(f"Error: {e}. Press ENTER to continue:")
        input("")
        return None
    nn = sorted(set(nn))
    for n in nn:
        if n < minN or n > maxN:
            printError(f"Error: numbers must be between {minN} and {maxN}. Press ENTER to continue:")
            input("")
            return None
    return nn

def downloadLC(to_mag):
    global search_result
    global current_time
    global current_obs
    global current_obs_err
    global current_nn
    global current_to_mag
    global current_lc_info
    if search_result is None:
        return
    if len(search_result) < 1:
        printError("No data found for the star. Press ENTER to continue:")
        input("")
        return
    nn = enterListOfNumbers("Enter LC numbers: ", 0, len(search_result) - 1)
    if nn is None or len(nn) < 1:
        return
    current_time = None
    current_obs = None
    current_obs_err = None
    current_nn = None
    current_to_mag = None
    current_lc_info = None
    plt.close('all')
    print("Loading light curve(s)...")
    tess_mags = [UNKNOWN_MAG]
    try:
        current_time, current_obs, current_obs_err, tess_mags = get_lc(search_result, nn, to_mag)
    except Exception as e:
        current_time = None
        current_obs = None
        current_obs_err = None
        current_nn = None
        current_to_mag = None
        current_lc_info = None
        printError(f"Error: {e}. Press ENTER to continue:")
        input("")
        return
    current_nn = nn
    current_to_mag = to_mag
    tess_mags_unique = set(tess_mags)
    if len(tess_mags_unique) == 1:
        current_lc_info = "TESS magnitude: " + str(list(tess_mags_unique)[0])
    else:
        current_lc_info = "WARNING! The selected light curves have different TESSMAG values:\n" + str(tess_mags)

def chooseFileToSaveAs():
    root = tk.Tk()
    root.withdraw()
    return filedialog.asksaveasfilename(title="Select a file")

def saveLC():
    global starName
    global current_time
    global current_obs
    global current_obs_err
    global current_nn
    global current_to_mag
    global current_lc_info
    if current_nn is None:
        return
    fname = input("Enter a file name, or type '?' to open the chooser: ").strip()
    if fname == "":
        return
    if fname == "?":
        fname = chooseFileToSaveAs()
    if fname == "":
        return
    try:
        with open(fname, "w") as f:
            # The following directives are for the VStar FlexText plug-in
            f.write('## Created with TESSdata.py\n')
            f.write('## VStar Flexible Text Format 1.2\n')
            f.write('\n')
            f.write("#DELIM=space\n")
            f.write("#FIELDS=time,mag,magerr\n")
            f.write("#DATE=BJD\n")
            f.write("#DEFINESERIES=TESS data,TESS data,#AA00AA\n")
            f.write("#FILTER=TESS data\n")            
            f.write("#NAME=" + starName + "\n")                        
            f.write("#TITLEX=BJD_TDB\n")
            if current_to_mag:
                f.write("#TITLEY=Magnitude\n")
                f.write("##Time Mag MagErr\n")
            else:
                f.write("#TITLEY=Flux\n")
                f.write("##Time Flux FluxErr\n")
            # Sort: sectors may go in an arbitrary order
            # Get sorting indices based on current_time
            sort_idx = np.argsort(current_time)
            # Apply the sorting to all arrays
            current_time_sorted = current_time[sort_idx]
            current_obs_sorted = current_obs[sort_idx]
            current_obs_err_sorted = current_obs_err[sort_idx]            
            for t, o, e in zip(current_time_sorted, current_obs_sorted, current_obs_err_sorted):
                f.write(f"{t} {o} {e}\n")
    except Exception as e:
        printError(f"Error: {e}. Press ENTER to continue:")
        input("")
        return
    
def plotLC():
    if current_nn is None:
        return
    fig = plt.figure(0)
    fig.clear()
    global starName
    global current_time
    global current_obs
    global current_obs_err
    global current_to_mag
    plt.plot(current_time, current_obs, 'r.')
    plt.title(starName)
    plt.xlabel('BJD_TDB')    
    if current_to_mag:
        plt.ylim(max(current_obs), min(current_obs))
        plt.ylabel('Magnitude', fontsize=15)
    else:
        plt.ylabel('Flux', fontsize=15)
    plt.show()

def get_available_data_list(id):
    global search_result
    search_result = lk.search_lightcurve(id, mission="TESS")
    
def get_lc(search_result, nn, to_mag):
    tess_mags = []
    time = np.array([], dtype=np.float64)
    obs = np.array([], dtype=np.float64)
    obs_err = np.array([], dtype=np.float64)
    for n in nn:
        lc = search_result[n].download()
        btjd_ref = lc.meta.get("BJDREFI", 0) + lc.meta.get("BJDREFF", 0)        
        # use only 'quality' values
        lc = lc[lc.quality == 0]
        # There may be cases when the flux = nan even if quality = 0.
        # Filter them off
        mask = ~np.isnan(lc.flux)
        lc = lc[mask]
        t = lc.time.value + btjd_ref
        o = lc.flux.value
        e = lc.flux_err.value
        # replace nan in uncertainties with 0 for compatibility with VStar
        e = np.nan_to_num(e, nan=0.0)
        tess_mag = lc.meta.get("TESSMAG", UNKNOWN_MAG)
        if to_mag:
            # There can be cases when the flux < 0: an exception will be raised for log10().
            median_flux = np.median(o)
            e = 1.086 * e / o
            o = -2.5  * np.log10(o)
            if tess_mag != UNKNOWN_MAG:
                median_mag = -2.5 * np.log10(median_flux)
                o = o - median_mag + tess_mag
        tess_mags.append(tess_mag)
        time = np.append(time, t)
        obs = np.append(obs, o)
        obs_err = np.append(obs_err, e)
    return time, obs, obs_err, tess_mags

def main():
    global starName
    global current_time
    global current_obs
    global current_obs_err
    global current_nn
    global current_to_mag
    global current_lc_info

    np.seterr(invalid="raise")
    
    while True:
        print()
        print("=" * 79)
        print("Currend Dir  :", os.getcwd())
        print("=" * 79)
        print()
        if not (starName is None or starName == ""):
            printHeader("Selected Star: ", starName)
            if not (current_nn is None):
                print("LCs:", str(current_nn))
                if not current_lc_info is None:
                    print(current_lc_info)
                if current_to_mag is None:
                    s = "<unknown>"
                elif current_to_mag:
                    s = "Magnitudes"
                else:
                    s = "Fluxes"
                print("Brightness in", s)
        else:    
            printHeader("Selected star: ", "No star selected")
        print()
        printMenu("1  Specify a star")
        if not (search_result is None):
            printMenu("2  Print a table of available LCs")
            printMenu("3  Select and download light curve(s): fluxes")
            printMenu("4  Select and download light curve(s): magnitudes")
            if not (current_nn is None):
                printMenu("5  Save the selected light curve")
                printMenu("6  Plot the selected light curve")
        printMenu("0  Exit")
        printMenu("c  Change directory")
        printMenu("r  Run external command")
        c = input("> ").strip().upper()
        match c:
            case "?":
                print()
                print("TESS Light Curve Downloader")
            case "R":
                runCommand()
            case "C":
                chDir()
            case "Q":
                return
            case "0":
                return
            case "1":
                getStarName()
            case "2":
                printSearchResult()
            case "3":
                downloadLC(False)
            case "4":
                downloadLC(True)
            case "5":
                saveLC()
            case "6":
                plotLC()

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        printError(f"Fatal Error: {e}. Press ENTER to continue:")
        input("")
    finally:
        print("End")
        sys.exit()
