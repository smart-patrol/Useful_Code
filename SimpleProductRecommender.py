# utf-8
# python 3.6

## Simple Recommender
## Code will take transactions by order id and product - then return the most similar product pairings
from sklearn.preprocessing import Normalizer
from sklearn.decomposition import TruncatedSVD
from sklearn.neighbors import NearestNeighbors
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.pipeline import Pipeline
from time import time

# initilize parameters for run
n_components = 5 # SVD components
n_neighbors = 10 # the # of like products to return
random_state = 123 # random seed

print("Readin Data")
# using Kaggle Instacart data available here: https://www.kaggle.com/c/instacart-market-basket-analysis
t0 = time()
df = pd.read_csv("order_products__prior.csv",nrows=100000) 
df['product_id'] = df['product_id'].astype(str)
print(t0 - time())

print("Map data so that all products are by the order")
t0 = time()
df = df.groupby(['order_id'])['product_id'].apply(lambda x : ' '.join(x))
print(t0 - time())

## construct a pipeline that does the following:
# 1) convert to binary matirx
# 2) reduce dimension of the matrix
# 3) Normalize again to prevent high values from swaying results
# 4) perform nearest neigbor to find most similar

t0 = time()
vectorizer = CountVectorizer(max_df=0.95, min_df=5, binary=True, max_features=1000)
svd = TruncatedSVD(n_components=n_components, random_state=random_state)
normalizer = Normalizer()
nn = NearestNeighbors(n_neighbors=n_neighbors, n_jobs=-1, metric='cosine',algorithm='brute')
pipeline = Pipeline([("dummy", vectorizer) , ("svd", svd), ("normalize", normalizer)])

print("Fitting Pipeline of Data Transform")
t0 = time()
X = pipeline.fit_transform(df.values)
print(t0 - time())

print("Fitting Nearest Neighbors")
t0 = time()
nn.fit(X)
# get distances and most similar indexes -  
distances, idxs = nn.kneighbors(X, n_neighbors=n_neighbors)
valid_idxs = np.where(distances != 0, idxs, np.NaN)
print(t0 - time())

# pair each similar product vector back as list of all similar products by the order
print("Collect Like Products by Order ID")
t0 = time()
paired_products = []
rows = valid_idxs.shape[0]

for i in range(rows):
    similar_items = df[df.index.isin(valid_idxs[i,:][~np.isnan(valid_idxs[i,:])])]
    paired_products.append(similar_items.values.tolist())

print(t0 - time())

print("Map Back to the Order ID for Recommendation DF")
t0 = time()
# names for products #s when converted to DF
col_names = []
for j in range(10):
    col_names.append("similar_num" + str(j+1))

paired_products = pd.DataFrame(paired_products, columns=col_names, index=df.index)

# keep only the top 5 and then concat back to grouped df
paired_products = paired_products.iloc[:,0:5 ]
paired_products = pd.concat([df, paired_products], 1)
print(t0 - time())
print("Complete")
