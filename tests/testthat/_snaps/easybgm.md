# easybgm works for bgms

    Code
      summary(res_bgms)
    Output
      
       BAYESIAN ANALYSIS OF NETWORKS 
       Model type: ordinal 
       Number of nodes: 5 
       Fitting Package: bgms 
      --- 
       EDGE SPECIFIC OVERVIEW 
                Relation Estimate Posterior Incl. Prob. Inclusion BF     Category
        intrusion-dreams    0.514                 1.000          Inf     included
         intrusion-flash    0.566                 1.000          Inf     included
            dreams-flash    0.291                 0.913       10.521     included
         intrusion-upset    0.034                 0.197        0.245 inconclusive
            dreams-upset    0.094                 0.438        0.780 inconclusive
             flash-upset    0.124                 0.525        1.104 inconclusive
       intrusion-physior    0.099                 0.450        0.819 inconclusive
          dreams-physior    0.002                 0.062        0.066     excluded
           flash-physior    0.107                 0.455        0.834 inconclusive
           upset-physior    0.797                 1.000          Inf     included
      
       Bayes Factors larger than 10 were considered sufficient evidence for the categorization. 
       --- 
       AGGREGATED EDGE OVERVIEW 
       Number of included edges: 4 
       Number of inconclusive edges: 5 
       Number of excluded edges: 1 
       Number of possible edges: 10 
       
       --- 
       STRUCTURE OVERVIEW 
       Number of visited structures: 101 
       Number of possible structures: 1024 
       Posterior probability of most likely structure: 0.1091 
      ---

# easybgm works for bdgraph

    Code
      summary(res_bdgraph)
    Output
      
       BAYESIAN ANALYSIS OF NETWORKS 
       Model type: ggm 
       Number of nodes: 5 
       Fitting Package: bdgraph 
      --- 
       EDGE SPECIFIC OVERVIEW 
                Relation Estimate Posterior Incl. Prob. Inclusion BF Category
        intrusion-dreams    0.447                     1          Inf included
         intrusion-flash    0.283                     1          Inf included
            dreams-flash    0.317                     1          Inf included
         intrusion-upset    0.158                     1          Inf included
            dreams-upset    0.199                     1          Inf included
             flash-upset    0.239                     1          Inf included
       intrusion-physior    0.205                     1          Inf included
          dreams-physior   -0.176                     1          Inf included
           flash-physior    0.164                     1          Inf included
           upset-physior    0.598                     1          Inf included
      
       Bayes Factors larger than 10 were considered sufficient evidence for the categorization. 
       --- 
       AGGREGATED EDGE OVERVIEW 
       Number of included edges: 10 
       Number of inconclusive edges: 0 
       Number of excluded edges: 0 
       Number of possible edges: 10 
       
       --- 
       STRUCTURE OVERVIEW 
       Number of visited structures: 62 
       Number of possible structures: 1024 
       Posterior probability of most likely structure: 1 
      ---

