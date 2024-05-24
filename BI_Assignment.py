import pandas as pd
import numpy as np
import seaborn as sb
from matplotlib import pyplot as plt

import os

from factor_analyzer import FactorAnalyzer
from sklearn.cluster import AgglomerativeClustering
from sklearn.cluster import KMeans
import scipy.cluster.hierarchy as sch

import scipy.stats as stats