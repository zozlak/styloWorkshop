library(styloWorkshop)
db.connect()

texts = get.texts() %>%
  mutate(
    year = substr(date, 1, 4),
    month = substr(date, 6, 7)
  )

# newspaper and 20th century novels corpora
# grouped by author->year and year->author
# each combination of year and author limite to ~500k characters
newspapers = texts  %>%
  filter(docsrc %in% c('KRONE', 'STANDARD')) %>%
  mutate(author = docsrc)
novels = texts %>%
  filter(source == 'Harper Lee')
bind_rows(newspapers, novels) %>%
  write.corpus('author', 'year', 'author_year_learning', 0.67, limit = 0.5*10^6) %>%
  write.corpus('author', 'year', 'author_year_test', limit = 0.5*10^6)
bind_rows(newspapers, novels) %>%
  write.corpus('year', 'author', 'year_author_learning', 0.67, limit = 0.5*10^6) %>%
  write.corpus('year', 'author', 'year_author_test', limit = 0.5*10^6)

# 100 most frequent bigrams
featuresAYlearn = load.corpus.and.parse(
  corpus.dir = 'author_year_learning',
  language = 'German',
  features = 'w',
  ngram.size = 2,
  preserve.case = FALSE
)
freqTabAYlearn = count.freqs(featuresAYlearn, relative = TRUE)
freqTabAYlearn = freqTabAYlearn[, 1:100]

featuresAYtest = load.corpus.and.parse(
  corpus.dir = 'author_year_test',
  language = 'German',
  features = 'w',
  ngram.size = 2,
  preserve.case = FALSE
)
freqTabAYtest = count.freqs(featuresAYtest, relative = TRUE)
freqTabAYtest = freqTabAYtest[, 1:100]

featuresYAlearn = load.corpus.and.parse(
  corpus.dir = 'year_author_learning',
  language = 'German',
  features = 'w',
  ngram.size = 2,
  preserve.case = FALSE
)
freqTabYAlearn = count.freqs(featuresYAlearn, relative = TRUE)
freqTabYAlearn = freqTabYAlearn[, 1:100]

featuresYAtest = load.corpus.and.parse(
  corpus.dir = 'year_author_test',
  language = 'German',
  features = 'w',
  ngram.size = 2,
  preserve.case = FALSE
)
freqTabYAtest = count.freqs(featuresYAtest, relative = TRUE)
freqTabYAtest = freqTabYAtest[, 1:100]

# 1) Author attribution using Delta classifier, Delta metric
resultsAYdelta = classify(
  gui = FALSE,
  training.frequencies = freqTabAYlearn,
  test.frequencies = freqTabAYtest,
  mfw.min = ncol(freqTabAYlearn),
  mfw.max = ncol(freqTabAYlearn),
  classification.method = 'delta',
  distance.measure = 'delta', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
)
resultsLongAYdelta = paste(readLines('final_results.txt'), collapse = '\n')

# 2) Author attribution using kNN classifier, 4 neighbors taken into account
resultsAYknn = classify(
  gui = FALSE,
  training.frequencies = freqTabAYlearn,
  test.frequencies = freqTabAYtest,
  mfw.min = ncol(freqTabAYlearn),
  mfw.max = ncol(freqTabAYlearn),
  classification.method = 'kNN',
  k.value = 4 # number of neighbors taken into account
)
resultsLongAYknn = paste(readLines('final_results.txt'), collapse = '\n')

# 3) Year attribution using Delta classifier, Delta metric
resultsYAdelta = classify(
  gui = FALSE,
  training.frequencies = freqTabYAlearn,
  test.frequencies = freqTabAYtest,
  mfw.min = ncol(freqTabYAlearn),
  mfw.max = ncol(freqTabYAlearn),
  classification.method = 'delta',
  distance.measure = 'delta', # distance metric delta/argamon/eder/simple/manhattan/canberra/euclidean/cosine
)
resultsLongYAdelta = paste(readLines('final_results.txt'), collapse = '\n')

# 4) Year attribution using kNN classifier, 4 neighbors taken into account
resultsAYknn = classify(
  gui = FALSE,
  training.frequencies = freqTabYAlearn,
  test.frequencies = freqTabYAtest,
  mfw.min = ncol(freqTabYAlearn),
  mfw.max = ncol(freqTabYAlearn),
  classification.method = 'kNN',
  k.value = 4 # number of neighbors taken into account
)
resultsLongYAknn = paste(readLines('final_results.txt'), collapse = '\n')

# Compare results
cat(resultsLongAYdelta)
cat(resultsLongAYknn)
cat(resultsLongYAdelta)
cat(resultsLongYAknn)

# Questions
# - are the results consistent?
# - are they in line with our intuitions about which classification should be more difficult?
# - do we have any idea why one method misclassified languages (sic!)?
# - can we make any story useful in our reaserch out of these results?
