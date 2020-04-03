import pandas as pd
import ssl

ssl._create_default_https_context = ssl._create_unverified_context
dataset =
pd.read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data',
 names=['sepal_length', 'sepal_width','petal_length', 'petal_width', 'species'])

