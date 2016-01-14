library(styloWorkshop)
db.connect()

texts = get.texts()

# 10% sample of three magazines with different target groups
texts %>%
  filter(docsrc %in% c('SPORTZTG', 'PROFIL', 'WOMAN')) %>%
  mutate(year = substr(date, 1, 4)) %>%
  write.corpus('docsrc', 'year', 'corpus', 0.1)

par(mfrow = c(1, 2)) # to see 2 plots next to each other

# 1st variant - single words
features1 = load.corpus.and.parse(
  corpus.dir = 'corpus',
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
  analysis.type = 'PCV',
  pca.visual.flavour = 'classic', # classic/loadings/technical/symbols
  text.id.on.graphs = 'both',
  label.offset = 4 # adjust if labels intersect points on the plot
)

# 2nd variant - bigrams
features2 = load.corpus.and.parse(
  corpus.dir = 'corpus',
  language = 'German',
  features = 'w',
  ngram.size = 2,
  preserve.case = FALSE
)
freqTab2 = count.freqs(features2, relative = TRUE)
freqTab2 = freqTab2[, 1:200]
results2 = stylo(
  gui = FALSE,
  frequencies = freqTab2,
  mfw.min = ncol(freqTab2), mfw.max = ncol(freqTab2),
  analysis.type = 'PCV',
  pca.visual.flavour = 'classic', # classic/loadings/technical/symbols
  text.id.on.graphs = 'both',
  label.offset = 4 # adjust if labels intersect points on the plot
)

# Questions:
# - Can we give a name to the 1st or 2nd component?
# - Are this results similar or differnt?
