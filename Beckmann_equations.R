
# FUNCTION DEFINITIONS
# CALCULATION FOR  *** FIXED *** SPAN OF CONTROL

# Source: BECKMANN, Martin J. (1983)
# "Some principles of efficient organizational design"
# International Journal of Systems Science
# https://doi.org/10.1080/00207728308926502


#### Number of persons of rank r
persons_of_rank <- function(s, R, r) {
  persons = s^(R-r)
  return (persons)
}

#### Number of positions of rank r or higher
positions_of_rank <- function(s, R, r) {
  positions = (s^(R-r+1)-1)/(s-1)
  return (positions)
}

#### Number of operatives (persons in first-line i.e. r=0)
operatives <- function(s, R) {
  Q=s^R
  return(Q)
}


#### Total size of an organization (based on constant span of control and hierarchical levels)
size_of_Organization <- function(s, R) {
  N = (s^(R+1) - 1) / (s - 1)
  return(N)
}

#### Sum of managers (level r>0) : number of supervisors including top-level leadership
size_of_Administration <- function (s, R) {
  A = (s^R-1)/(s-1)
  return(A)
}

#### Managers to first-line employees ratio
Administrators_per_operative <- function (s, R) {
  AtoQ = (1/(s-1))-(s^(-R)/(s-1))
  return (AtoQ)
  
  # The limit 1/(s-1) of administration per worker is rapidly approached when s>=3
}


#### Estimation of fixed span of control based on size of organization (number of operatives) and hierarchical levels
fixed_span <- function (Q, R) {
  s <- Q^(1/R)
  return(s)
}

# Organizational multiplier
org_factor <- function(s) {
  return((1-1/s)^-1)
}

# Matrix creation for sizes based on fixed span of control
sizes_table <- function(s = s, R = R) {
  ## Create an empty matrix to store the results
  result_matrix <- matrix(nrow = s, ncol = R)
  
  ## Assign row and column names to the matrix
  rownames(result_matrix) <- paste("s =", 2:(s+1))
  colnames(result_matrix) <- paste("r =", 1:R)
  
  ## Fill the matrix with the size of the organization values
  for (ss in 2:(s+1)) {
    for (RR in 1:R) {
      size <- size_of_Organization(ss, RR)
      if (size <= 22222) {
        result_matrix[ss-1, RR] <- size
      }
    }
  }

  ## Print the result matrix
  cat("\nTotal size of Organization for fixed span s from 2 to", s+1, "and total levels R =", R,"\n\n")
  return(result_matrix)
}



