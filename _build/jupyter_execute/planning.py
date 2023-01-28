#!/usr/bin/env python
# coding: utf-8

# # Answering scientific questions using data
# 
# ## Key texts 

# In[1]:


import pandas as pd

texts = pd.DataFrame(
    [
        ['Statistical Rethinking', 'Complete all exercises'],
        ['Modern Approach', 'Recreate examples, in both R and python'],
        ['B and E', 'No hard objective, is for when I feel like doing math']
    ],
    columns=['Resource', 'Objective']
).set_index('Resource')

texts.style


# ## Key skills
# 
# - First Jupyter Book
# - Use R and Python in book using VSC

# ## Where to start?
# 
# Reading is good. Apply notes to simulated datasets.
# 
# Also tracking, when in doubt. Come up with ways to track. 
