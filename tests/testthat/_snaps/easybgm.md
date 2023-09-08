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
      
       Bayes Factors larger than 10 were considered sufficient evidence for the classification 
       --- 
       AGGREGATED EDGE OVERVIEW 
       Number of edges with sufficient evidence for inclusion: 4 
       Number of edges with insufficient evidence: 5 
       Number of edges with sufficient evidence for exclusion: 1 
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
                Relation Estimate Posterior Incl. Prob. Inclusion BF     Category
        intrusion-dreams    0.436                  1.00          Inf     included
         intrusion-flash    0.444                  1.00          Inf     included
            dreams-flash    0.197                  0.71        2.448 inconclusive
         intrusion-upset    0.123                  0.45        0.818 inconclusive
            dreams-upset    0.165                  0.73        2.704 inconclusive
             flash-upset    0.185                  0.68        2.125 inconclusive
       intrusion-physior    0.172                  0.68        2.125 inconclusive
          dreams-physior    0.003                  0.04        0.042     excluded
           flash-physior    0.125                  0.48        0.923 inconclusive
           upset-physior    0.592                  1.00          Inf     included
      
       Bayes Factors larger than 10 were considered sufficient evidence for the classification 
       --- 
       AGGREGATED EDGE OVERVIEW 
       Number of edges with sufficient evidence for inclusion: 3 
       Number of edges with insufficient evidence: 6 
       Number of edges with sufficient evidence for exclusion: 1 
       Number of possible edges: 10 
       
       --- 
       STRUCTURE OVERVIEW 
       Number of visited structures: 29 
       Number of possible structures: 1024 
       Posterior probability of most likely structure: 0.2386 
      ---

