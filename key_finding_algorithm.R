#Key-finding algorithm
key_finding = 
  function(melody){
    
    #Standardized key-profiles
    major_profile   = c(3.66, 2.29, 2.88, 6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39)
    n_minor_profile = c(5.08,	3.03,	3.73,	4.23,	3.64,	3.85,	3.13,	5.29,	4.43,	3.95,	5.26,	3.99)
    h_minor_profile = c(4.62, 2.63, 3.74, 4.23, 3.63, 3.81, 4.15, 5.21, 4.77, 3.95, 3.79, 5.3)
    m_minor_profile = c(4.75, 3.26, 3.76, 4.46, 3.49, 4.09, 3.67, 5.08, 4.14, 4.43, 4.51, 4.91)
    
    profile_vectors = list(c(major_profile), c(n_minor_profile), c(h_minor_profile), c(m_minor_profile))

    profile_names = c("major", "natural minor", "harmonic minor", "melodic minor")

    #Shifts key-profiles to cover all 12 tonalities
    shifter <- function(prof, n = 0) {
      if (n == 0) prof else c(tail(prof, -n), head(prof, n))
    }
    
    result = c()
    
    for(k in 1:length(profile_vectors)){
     
      r = c()
      p_val = c()
      nomes = c('a', 'a#', 'b', 'c', 'c#', 'd', 'd#', 'e', 'f', 'f#', 'g', 'g#')
      
      for(i in 0:11){

        cor_result = cor.test(shifter(profile_vectors[[k]], n = i), melody)
        r = c(r, cor_result[['estimate']])
        p_val = c(p_val, cor_result[["p.value"]])
        
      }
      
      result = c(result, data.frame(
                                  cor = r, 
                                  p_val = p_val, 
                                  key = nomes, 
                                  profiles = profile_names[k])
                 )

  }
    #Create data frame with results
    result = dplyr::bind_cols(result)
    return(result[order(-result$cor),])
  }

tonal = c(2, 0, 1, 2, 0, 0, 0, 0, 1, 0, 1, 0)
atonal = c(1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0)

t = key_finding(tonal)
at = key_finding(atonal)