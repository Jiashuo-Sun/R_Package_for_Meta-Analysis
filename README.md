# R Package for Meta Analysis

This is an R package for meta analysis based on paper [Meta-analysis methods for combining multiple expression profiles](https://www.ncbi.nlm.nih.gov/pubmed/24359104).

* "metaGroup8_1.0.tar.gz" is the completed r package and ready to install in any R environment.

* "metaGroup8.R" is the original source code for all r functions.

* "metaGroup8" folder contains all r functions and their description (.Rd files).

* "Package User Guide.pdf" is also provided for this package.

* "Project Notes.md" is the notes I took to build an own R package.

### Background and Meta-Analysis

As high-throughput genomic technologies become accurate and affordable, an increasing number of data sets have been accumulated in the public domain and genomic information integration and meta-analysis have become routine in biomedical research. Microarray meta-analysis is where multiple microarray studies with relevant biological hypotheses are combined in order to improve candidate marker detection.

Microarray meta-analysis methods can be categorized into three types: combine *p*-values, combine effect sizes and combine ranks. In this package, we focused on combining *p*-values methods.

### 4 Methods of Combining *p*-value

1. Fisher

The Fisher’s method sums up the log-transformed *p*-values obtained from individual studies. 

2. Stouffer

Stouffer’s method sums the inverse normal transformed *p*-values.

3. Minimum *p*-value (minP)

The minP method takes the
minimum *p*-value among the K studies as the test statistic. It follows a beta distribution with degrees of freedom *α = 1* and *β = k* under the null hypothesis.

4. Maximum *p*-value (maxP)

The maxP method takes maximum *p*-value as the test statistic. It follows a beta distribution with degrees of freedom *α = K* and *β = 1* under the null hypothesis.

![pooled p value](https://github.com/Jiashuo-Sun/R_Package_for_Meta-Analysis/blob/master/demo_picture/pooledP.png)

### Future Work

1. More methods of combining p-value. 
2. Implement functions of combining effect size.
3. Implement functions of combining rank statistics. 