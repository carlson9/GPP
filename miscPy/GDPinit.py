import numpy as np, pandas as pd
from matplotlib import pyplot as plt
import os
os.chdir('GPP/')

import pyreadr
d = pyreadr.read_r('GPP/data/GDPdata.rdata')
d = d['GDPdata']

d['gdp'] = (d.gdp - d.gdp.mean())/d.gdp.std()

toComp = d.loc[(d['country'] == 'West Germany') & (d['year'] > 1989), 'gdp']

XY = d[['gdp', 'invest', 'school', 'ind', 'country', 'year']].loc[(d['country'] != 'West Germany') | (d['year'] <= 1989),]

y = XY['gdp']

from sklearn.preprocessing import LabelBinarizer
label_binarizer = LabelBinarizer()
label_binarizer_output = label_binarizer.fit_transform(XY.country)

X = XY[['invest', 'school', 'ind', 'year']]
X = (X - X.mean())/X.std()

dummy = pd.DataFrame(label_binarizer_output).reset_index()
X = X.reset_index()
X = pd.concat([X, dummy], axis = 1)
X = X.drop('rownames', axis = 1)
X = X.drop('index', axis = 1)
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import ConstantKernel, RBF, DotProduct

gpr = GaussianProcessRegressor(kernel=DotProduct(), random_state = 1, alpha = 1E-6)

gpr.fit(X, y)

# Obtain optimized kernel parameters
sigma_0 = gpr.kernel_.sigma_0

#predict West Germany X

rbf = ConstantKernel(1.0) * RBF(length_scale=1.0)
gprX0 = GaussianProcessRegressor(kernel=rbf, alpha = 1)

X_WG_train0 = X.drop(['school', 'ind'], axis = 1)
X_WG_train1 = X.drop(['invest', 'ind'], axis = 1)
X_WG_train2 = X.drop(['invest', 'school'], axis = 1)

y0 = X_WG_train0.invest
X0 = X_WG_train0.drop('invest', axis = 1)
gprX0.fit(X0, y0)
dummyWG = np.zeros((X0.year.unique()[d.year.unique() > 1989].shape[0], 17))
#dummy.loc[XY.reset_index().country == 'West Germany',]
#column 16
dummyWG[:,16] = 1.
X_WG_test0 = pd.concat([pd.Series(X0.year.unique()[d.year.unique() > 1989]).reset_index(), pd.DataFrame(dummyWG).reset_index()], axis = 1)
X_WG_test0 = X_WG_test0.drop('index', axis = 1)

X0_preds = gprX0.predict(X_WG_test0)

#TODO: use CV for alpha and length-scale

#TODO: repeat for other X's

#TODO: make predictions (with uncertainty) on GDP for WG based on estimated X's

#TODO: put everything in a function to assess CP for imaginary drops, use this to find appropriate alpha for original gpr()

#TODO: generalize



