library(styloWorkshop)
db.connect()

texts = get.texts()

# all Abacus texts
texts %>%
  filter(source %in% 'abacus') %>%
  mutate(year = substr(date, 1, 4)) %>%
  write.corpus('year', 'title', 'ca_mds_corpus')

par(mfrow = c(2, 2))# to see many graphs at time

# 1) CA on 200 most frequent bigrams with Delta metric and ward linking
features1 = load.corpus.and.parse(
  corpus.dir = 'ca_mds_corpus',
  language = 'German',
  features = 'w',
  ngram.size = 1,
  preserve.case = FALSE
)
freqTab1 = count.freqs(features1, relative = TRUE)
freqTab1 = freqTab1[, 1:200]

results1 = stylo(
  gui = FALSE,
  frequencies = freqTab1,
  mfw.min = ncol(freqTab1), mfw.max = ncol(freqTab1),
  analysis.type = 'CA',
  distance.measure = 'delta', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
  linkage = 'ward' # method of linking texts into tree ward/nj/single/complete/average/mcquitty/median/centroid
)

# 2) CA on 200 most frequent bigrams,Euclidean metric and nj linking
results2 = stylo(
  gui = FALSE,
  frequencies = freqTab1,
  mfw.min = ncol(freqTab1), mfw.max = ncol(freqTab1),
  analysis.type = 'CA',
  distance.measure = 'euclidean', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
  linkage = 'nj' # method of linking texts into tree ward/nj/single/complete/average/mcquitty/median/centroid
)

# 3) CA on 200 most frequent words, single metric and complete linking
features3 = load.corpus.and.parse(
  corpus.dir = 'ca_mds_corpus',
  language = 'German',
  features = 'w',
  ngram.size = 1,
  preserve.case = FALSE
)
freqTab3 = count.freqs(features3, relative = TRUE)
freqTab3 = freqTab3[, 1:200]

results3 = stylo(
  gui = FALSE,
  frequencies = freqTab3,
  mfw.min = ncol(freqTab3), mfw.max = ncol(freqTab3),
  analysis.type = 'CA',
  distance.measure = 'cosine', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
  linkage = 'single' # method of linking texts into tree ward/nj/single/complete/average/mcquitty/median/centroid
)

# 4) MDS on 200 most frequent bigrams, Delta metric
results4 = stylo(
  gui = FALSE,
  frequencies = freqTab1,
  mfw.min = ncol(freqTab1), mfw.max = ncol(freqTab1),
  analysis.type = 'MDS',
  distance.measure = 'delta', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
  text.id.on.graphs = 'both',
  label.offset = 4 # adjust if labels intersect points on the plot
)

# Questions:
# - Can we draw any common conclusions from all graphs?
# - Can we assess which method is better?
# - Do we have any measure of certainty for these results?
