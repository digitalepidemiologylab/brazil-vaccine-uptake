# Script information ------------
#' Aim: ML to predict vaccine uptake with Twitter sentiment analysis
#' Author: Laura Espinosa
#' Date created: 21 August 2022
#' Date updated: 22 August 2022

# 1 - Packages ----------------------
reticulate::import('pandas')
import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns

from sklearn.feature_selection import SelectKBest
from sklearn.feature_selection import f_classif
from sklearn.model_selection import train_test_split

# Ignore warnings
import warnings
warnings.simplefilter(action = "ignore")




py -m pandas --version
