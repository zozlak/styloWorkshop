library(styloWorkshop)
db.connect('stylometry', '127.0.0.1', 'zozlak', NULL)

texts = get.texts() %>%
  mutate(
    year = substr(date, 1, 4),
    month = substr(date, 6, 7)
  )

# 2% sample of AMC from February, April, June and December grouped by year and month
# each group limited to ~200k characters
texts  %>%
  filter(source %in% 'amc', month %in% c('02', '04', '07', '12')) %>%
  write.corpus('docsrc', c('year', 'month'), 'corpus', 0.02, 200*10^3)

# word bigrams
features = load.corpus.and.parse(
  corpus.dir = 'corpus',
  language = 'German',
  features = 'w',
  ngram.size = 2,
  preserve.case = FALSE
)
features$all_all = unlist(features) # trick to add a fake text combining all features

# 200 most frequent features
freqTab = count.freqs(features, relative = TRUE)
freqTab = freqTab[, 1:1000] # save memory
freqTabAdj = freqTab[, 1:200] # take 200 most frequent

# compute Delta distance
# we will use MDS for that because we have to perform some kind of analysis
#   but in fact we care only about distances between each text and the "all" one
results = stylo(
  gui = FALSE,
  frequencies = freqTabAdj,
  mfw.min = ncol(freqTabAdj), mfw.max = ncol(freqTabAdj),
  analysis.type = 'MDS',
  distance.measure = 'delta', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
  text.id.on.graphs = 'both',
  label.offset = 4 # adjust if labels intersect points on the plot
)
distances = results$distance.table['all_all', ]
distances = distances[distances > 0] # skip distance to itself

# generate some contextual data
types = c(
  FALTER = 'magazine', HEUTE = 'newspaper', KRONE = 'newspaper', MWVOLL = 'tv', PRESSE = 'newspaper', 
  PROFIL = 'magazine', SPORTZTG = 'magazine', STANDARD = 'newspaper', WOMAN = 'magazine'
)
lmData = data.frame(
  distance = distances,
  year = as.numeric(sub('^[^0-9]+([0-9]+) .*$', '\\1', names(distances))) - 1994,
  month = sub('^.* ([0-9]+)$', '\\1', names(distances)),
  type = types[sub('^([A-Z]+)_.*$', '\\1', names(distances))]
)

# compute a linear model
model = lm(distance ~ year + month + type, data = lmData)
summary(model)
