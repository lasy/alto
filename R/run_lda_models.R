run_lda_models = function(
  data,
  varying,
  values,
  seed
){

  # 1. CHECKS
  # check data (format and values)
  # check varying arguments
  varying = match.arg(varying, c("K","alpha","environment"))
  # check argument values
  if(varying == "K"){
    # values should be positive integer (allow 1?)
  }
  if(varying == "alpha"){
    # values should be positive doubles
  }
  if(varying == "environment"){
    # values should be a vector (of integer/characters/does not matter?) with a length equal to the number of rows in data
  }


  # 2. RUNNING MODELS




}
