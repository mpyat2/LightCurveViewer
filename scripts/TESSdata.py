# -*- coding: utf-8 -*-
"""
Created on Tue May 13 16:23:58 2025

@author: Maksym Pyatnytskyy, mpyat2@gmail.com
"""

import lightkurve as lk
import numpy as np
import sys
import matplotlib.pyplot as plt
import tkinter as tk
from tkinter import filedialog

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
current_n = None
current_time = None
current_obs = None
current_to_mag = None
current_lc_info = None

UNKNOWN_MAG = -99
DEFAULT_MAG = 0

def getStarName():
    global starName
    starName = input("Enter a name or coordinates: ").strip()
    if not (starName is None or starName == ""):
        print("Querying a list of light curves...")
        global search_result
        global current_time
        global current_obs
        global current_n
        global current_to_mag
        search_result = None
        current_time = None
        current_obs = None
        current_n = None
        current_to_mag = None
        plt.close('all')
        try:
            get_available_data_list(starName)
            if search_result is None or len(search_result) < 1:
                printError("No data found for the star. Press ENTER to continue:")
                input("")
        except Exception as e:
            search_result = None
            printError(f"Error: {e}. Press ENTER to continue:")
            input("")
            return

def printSearchResult():
    global search_result
    if search_result is None or len(search_result) < 1:
        printError("No data found for the star. Press ENTER to continue:")
        input("")
        return
    print(bcolors.OKGREEN)
    print(search_result)
    print(bcolors.ENDC)

def downloadLC(to_mag):
    global search_result
    global current_time
    global current_obs
    global current_n
    global current_to_mag
    global current_lc_info
    if search_result is None or len(search_result) < 1:
        printError("No data found for the star. Press ENTER to continue:")
        input("")
        return
    try:
        n = int(input("Enter an LC number (-1 to cancel): ").strip())
        if n == -1:
            return
        if n < 0 or n >= len(search_result):
            printError("Invalid LC number. Press ENTER to continue:")
            input("")
            return
        current_time = None
        current_obs = None
        current_n = None
        current_to_mag = None
        current_lc_info = None
        plt.close('all')
        print("Loading light curve...")
        tess_mag = UNKNOWN_MAG
        try:
            current_time, current_obs, tess_mag = get_lc(search_result, n, to_mag)
        except Exception as e:
            current_time = None
            current_obs = None
            current_n = None
            current_to_mag = None
            current_lc_info = None
            printError(f"Error: {e}. Press ENTER to continue:")
            input("")
            return
        current_n = n
        current_to_mag = to_mag
        current_lc_info = "TESS Magnitude = " + str(tess_mag)
    except ValueError:
        printError("Invalid input! Must be a non-negative integer or -1. Press ENTER to continue:")
        input("")
        return

def chooseFileToSaveAs():
    root = tk.Tk()
    root.withdraw()
    return filedialog.asksaveasfilename(title="Select a file")

def saveLC():
    global starName
    global current_time
    global current_obs
    global current_n
    global current_to_mag
    global current_lc_info
    fname = input("Enter a file name, or type '?' to open the chooser: ").strip()
    if fname == "":
        return
    if fname == "?":
        fname = chooseFileToSaveAs()
    if fname == "":
        return
    try:
        with open(fname, "w") as f:
            f.write("#NAME=" + starName + "\n")
            if current_to_mag:
                f.write("#Time Mag\n")
            else:
                f.write("#Time Flux\n")
            for t, m in zip(current_time, current_obs):
                f.write(f"{t} {m}\n")
    except Exception as e:
        printError(f"Error: {e}. Press ENTER to continue:")
        input("")
        return
    
def plotLC():
    fig = plt.figure(0)
    fig.clear()
    global starName
    global current_time
    global current_obs
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
    
def get_lc(search_result, n, to_mag):
    lc = search_result[n].download()
    btjd_ref = lc.meta.get("BJDREFI", 0) + lc.meta.get("BJDREFF", 0)        
    lc = lc[lc.quality == 0]
    time = lc.time.value + btjd_ref
    obs = lc.flux.value
    tess_magnitude = lc.meta.get("TESSMAG", UNKNOWN_MAG)
    if to_mag:
        obs = -2.5 * np.log10(obs)
        if tess_magnitude != UNKNOWN_MAG:
            median_mag = np.median(obs)
            #print(median_mag)
            obs = obs - median_mag + tess_magnitude
    return time, obs, tess_magnitude

def printError(msg):
    print(bcolors.FAIL + msg + bcolors.ENDC)
    
def printHeader(msg, starInfo):
    print(bcolors.HEADER + msg  + bcolors.ENDC + bcolors.REVERSE + starInfo + bcolors.ENDC)    
    
def printMenu(msg):
    print(bcolors.OKCYAN + msg + bcolors.ENDC)    

def main():
    global starName
    global current_time
    global current_obs
    global current_n
    global current_to_mag
    global current_lc_info

    np.seterr(invalid="raise")
    
    while True:
        print()
        print("=" * 79)
        if not (starName is None or starName == ""):
            s = starName
            if not (current_n is None):
                s += "; LC #" + str(current_n)
                if current_to_mag is None:
                    s += "; <unknown state>"
                elif current_to_mag:
                    s += "; Magnitudes"
                else:
                    s += "; Fluxes"
                if not current_lc_info is None:
                    s += "; " + current_lc_info
        else:    
            s = "No star selected"
        printHeader("Selected star: ", s)
        print()
        printMenu("1. Specify a star")
        if not (search_result is None):
            printMenu("2. Print a table of available LCs")
            printMenu("3. Select and download a light curve: fluxes")
            printMenu("4. Select and download a light curve: magnitudes")
            if not (current_n is None):
                printMenu("5. Save the selected light curve")
                printMenu("6. Plot the selected light curve")
        printMenu("0. Exit")
        c = input("> ").strip()
        match c:
            case "?":
                print()
                print("TESS Light Curve Downloader")
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
    finally:
        #print("Cleaning up before exit...")
        sys.exit()
 
