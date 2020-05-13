import pandas as pd

import ssl

ssl._create_default_https_context = ssl._create_unverified_context
dataset =pd.read_csv('https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data',
   names=['sepal_length', 'sepal_width','petal_length', 'petal_width', 'species'])
dataset['species'] = pd.Categorical(dataset['species']).codes
dataset = dataset.sample(frac = 1, random_state = 1234)
 

