# R version of the GNRND4 program
# copyright 2016, January  Roger M. Palay
# Saline, MI 48176

# this program mimics the TI-83/84 program
# and the Javascript program  by the same name in that it generates
# a file of values generated via two or three KEY values that are
# supplied by the user.  In this case, however, the output will go both
# to a specific file and also to vectors in the workspace.
# The user is prompted for the name of the
# output file.

# Note that the program is designed to mimic the originals and therefore there
# is less attention paid to ultimate efficiency and more to keeping the
# versions aligned.

gn_maxint <- 8388607
gn_urands <- 47;

gn_get_urand <- function()
{ gn_urands <<- gn_urands * 4111;
if( gn_urands > gn_maxint )
{ gn_urands <<- gn_urands - floor( gn_urands/gn_maxint ) * gn_maxint
}
gn_urand <<- gn_urands/ gn_maxint
return( gn_urand)
}

gnrnd4 <- function(key1=123450801, key2=1300050, key3=0,
                   filebase="L", trace_level=9)
{
  temp <- floor( key1/100 )
  style_num <- key1 - temp*100
  key1 <- temp
  temp <- floor( key1/100 )
  samp_size <- key1 - temp*100 + 1
  key1 <- temp
  temp <- floor( key1/100000 )
  seed <- key1 - temp*100000
  key1 <- temp
  temp <- floor( key1/10 )
  num_dig <- key1 - temp*10
  key1 <- temp
  if ( num_dig > 4 )
  { num_dig <- num_dig - 5
  alt_sign <- -1
  }
  else { alt_sign <- 1 }
  
  L1 <<- numeric(samp_size)
  if( any (style_num == c(6, 10)) )
  { L2 <<- numeric(samp_size) }
  
  if( trace_level > 5 )
  { cat( c("style=", style_num, "  size=", samp_size,
           "   seed=", seed, "  num digits=", num_dig,
           "  alt_sign=", alt_sign, "\n") )
    
  }
  
  gn_urands <<- seed
  
  if (style_num < 4 )
  { ## styles 1 through 3 use the same decoding for key 2
    temp <- floor( key2/100000 )
    low = key2 - temp*100000
    key2 <- temp
    temp <- floor( key2/1000000 )
    range = key2 - temp*100000
    key2 <- temp
    
    low <- alt_sign*low/ (10^num_dig)
    range <- range/(10^num_dig)
    fcode <- c("%3d,  %8.",num_dig,"f")
    
    
    
    for (i in 1:samp_size)
    {
      uval <- gn_get_urand()
      if (style_num == 2 )
      { uval <- uval*uval }
      if ( style_num == 3 )
      { uval <- sqrt( uval) }
      j <- low + range* uval
      k <- round( j, num_dig)
      L1[ i ] <<- k
    }
    
  }
  else if (style_num == 4 | style_num == 10 )
  {  # we need to get the mean and standard deviation
    # from the second key, and if this is a style=10
    # then we need to get the pair_spred from that key also.
    temp <- floor(key2/100000)
    this_mean <- key2 - temp*100000
    key2 <- temp
    temp <- floor(key2/100000)
    this_sd <- key2 - temp*100000
    key2 <- temp
    if( style_num == 10 )
    { temp <- floor( key2 / 100)
    pair_spread <- key2 - temp*100
    key2 <- temp
    
    e <- (pair_spread+1)/25
    f <- 0.5 + e/9
    }
    this_mean <- alt_sign*this_mean/ (10^num_dig)
    this_sd <- this_sd/(10^num_dig)
    
    for ( i in 1:samp_size )
    {
      uval <- ((gn_get_urand()+gn_get_urand()+gn_get_urand())/3-0.5)*6
      # uval is approximately N(0,1)
      j <- uval*this_sd + this_mean
      L1[ i ] <<- round(j, num_dig)
      if( style_num == 10)
      { L2[i] <<- round(L1[ i ] + (f - gn_get_urand())*e*this_sd, num_dig)
      }
    }
  }
  else if( style_num == 5 )
  {  # bi-modal   that we create by creating two separate, 
    # but probably overlapping, normal distributions
    temp <- floor(key2/100000)
    this_mean_1 <- (key2 - temp*100000)*alt_sign / (10^num_dig)
    key2 <- temp
    temp <- floor(key2/100000)
    this_sd_1 <- (key2 - temp*100000 ) / (10^num_dig)
    key2 <- temp
    if( key3 < 0)
    { key3 <- -key3
    mean_2_sign <- -1
    }
    else
    { mean_2_sign <- 1
    }
    temp <- floor(key3/100000)
    this_mean_2 <- (key3 - temp*100000) * mean_2_sign / (10^num_dig)
    key3 <- temp
    temp <- floor(key3/100000)
    this_sd_2 <- (key3 - temp*100000) / (10^num_dig)
    key3 <- temp
    
    for ( i in 1:samp_size )
    {
      uval <- ((gn_get_urand()+gn_get_urand()+gn_get_urand())/3-0.5)*6
      # uval is approximately N(0,1)
      if( gn_get_urand() < 0.5 )
      {
        j <- uval*this_sd_1 + this_mean_1
      }
      else
      {
        j <- uval*this_sd_2 + this_mean_2
      }
      L1[ i ] <<- round(j, num_dig)
    }
    
    
  }
  else if( style_num == 6 )
  {  # this will be a quasi-linear relation between the values in L1 and those
    # in L2. The relation is described as Dy = Mx + B
    # Pull values from key 2
    temp <- floor( key2/100 )
    yD = key2 - temp*100
    key2 <- temp
    temp <- floor( key2/100 )
    M <- key2 - temp*100
    key2 <- temp
    temp <- floor( key2/1000 )
    B <- key2 - temp*1000
    key2 <- temp
    temp <- floor (key2/10 )
    Msign <- key2 - temp*10
    key2 <- temp
    temp <- floor (key2/10 )
    Bsign <- key2 - temp*10
    key2 <- temp
    temp <- floor (key2/10 )
    err <- key2 - temp*10
    err <- (err+1)*(err+2)/200
    key2 <- temp
    
    
    if (Msign > 5 ) { M <- -M }
    if (Bsign > 5 ) { B <- -B }
    
    # then pull values from key3
    temp <- floor(key3/100000)
    xlow <- (key3 - temp*100000) * alt_sign / (10^num_dig)
    key3 <- temp
    temp <- floor(key3/100000)
    xrange <- (key3 - temp*100000) / (10^num_dig)
    key3 <- temp
    
    # get the range of y-values
    gn_left_y <- M*xlow/yD + B/yD
    gn_right_y <- M*(xlow+xrange)/yD + B/yD
    
    gn_y_range <- abs( gn_left_y - gn_right_y )
    
    # now generate the values
    
    for (i in 1:samp_size )
    {
      j <- xlow +gn_get_urand()*xrange
      x <- round( j, num_dig )
      y <- M*x/yD + B/yD
      
      # now introduce our error factor
      y <- y + (gn_get_urand()-0.5)*2*err*gn_y_range
      
      y <- round(y,num_dig)
      L1[ i ] <<- x
      L2[ i ] <<- y
    }
  }
  else if (style_num == 7 )
  { # set up a discrete probability distribution
    # we will need to get the number of discrete values 
    # from the second key, along with the goal
    # relative frequency of those values.  We will
    # then create a helper list from which we can select our
    # values.
    temp <- floor(key2/10)
    num_row <- key2 - temp*10
    key2 <- temp
    
    goal_freq <- numeric(num_row)
    total <- 0
    for (i in 1:num_row)
    { temp <- floor(key2/10)
    j <- key2 - temp*10
    total <- total + j
    goal_freq[i] <- j
    key2 <- temp
    }
    cat( goal_freq, "\n")
    
    freq_help <- numeric(total)
    k <- 0
    for (i in 1:num_row )
    { j <- goal_freq[ i ]
    for( m in 1:j )
    { k <- k + 1
    freq_help[k] <- i
    }
    }
    cat( freq_help, "\n" )
    for (i in 1:samp_size )
    { j <- floor( gn_get_urand()*total) + 1
    L1[ i ] <<- freq_help[ j ]
    }
    
  }
  else if( style_num == 8 ) 
  { #  we will build a table, a matrix, with a  specified
    # number of rows and columns where the row and column
    # frequencies approach specified values
    
    # first get the number of rows and columns from key2
    temp <- floor( key2/10 )
    nr <- key2 - temp*10
    key2 <- temp
    temp <- floor( key2/10 )
    nc <- key2 - temp*10
    key2 <- temp
    
    #then get the relative weight of each row
    
    row_freq <- numeric( nr )
    row_need <- 0
    for (i in 1:nr )
    { temp <- floor( key2 / 10 )
    j <- key2 - temp*10
    row_freq[ i ] <- j
    row_need <- row_need + j
    key2 <- temp
    
    }
    
    #then get the relative weight of each column
    
    col_freq <- numeric( nr )
    col_need <- 0
    for (i in 1:nc )
    { temp <- floor( key2 / 10 )
    j <- key2 - temp*10
    col_freq[ i ] <- j
    col_need <- col_need + j
    key2 <- temp
    
    }
    # now build the helper distributions for the rows and columns
    row_help <- numeric( row_need )
    k <- 0
    for (i in 1:nr )
    { for (j in 1:row_freq[i] )
    { k <- k+1
    row_help[ k ] = i
    }
    }
    col_help <- numeric( col_need )
    k <- 0
    for (i in 1:nc )
    { for (j in 1:col_freq[i] )
    { k <- k+1
    col_help[ k ] = i
    }
    }
    
    # now we can generate the matrix
    
    matrix_A <<- matrix(0, nrow=nr, ncol=nc)
    num_gen <- nr*nc*samp_size
    for( i in 1:num_gen )
    { j <- floor( gn_get_urand()*row_need )+1
    k <- floor( gn_get_urand()*col_need ) + 1
    jj <- row_help[ j ]
    kk <- col_help[ k ]
    matrix_A[jj,kk] <<- matrix_A[jj,kk]+1
    }
    
  }
  
  else if (style_num == 9 )
  { # a distribution set to conform to quartiles
    # adjust samp_size to be =4n-1 for some n
    n <- floor( (samp_size+1)/4 )
    lcl_size <- 4*n - 1
    L1 <<- numeric( lcl_size )
    
    # Because we will generate values in order, we create
    # a random pattern to store them
    lcl_L2 <- 1:lcl_size
    # shuffle them
    for (i in 1:lcl_size)
    { j <- floor(gn_get_urand()*lcl_size)+1;
    if(i!=j)
    { k  <-  lcl_L2[i]
    lcl_L2[i] <- lcl_L2[j]
    lcl_L2[j] <- k
    }
    }
    
    # before we go any further we need to get the xlow, xrange,
    # and position of the quartiles in that range, all from key2
    temp <- floor( key2 / 1000)
    xlow <- (key2 - temp*1000)*alt_sign/(10 ^ num_dig )
    key2 <- temp
    temp <- floor( key2 / 1000)
    xrange <- (key2 - temp*1000)/(10 ^ num_dig )
    key2 <- temp
    temp <- floor( key2 / 100)
    r3 <- key2 - temp*100
    key2 <- temp
    temp <- floor( key2 / 100)
    r2 <- key2 - temp*100
    key2 <- temp
    temp <- floor( key2 / 100)
    r1 <- key2 - temp*100
    key2 <- temp
    r4 = 100 - (r1+r2+r3)
    gn_pnts <- numeric(7)
    gn_pnts[1] <- xlow
    gn_pnts[7] <- xlow + xrange
    gn_pnts[3] <- round( xlow+ r1*xrange/100+0.00001, num_dig )
    gn_pnts[4] <- round( xlow+(r1+r2)*xrange/100+0.00001, num_dig)
    gn_pnts[5] <- round( xlow+(r1+r2+r3)*xrange/100+0.00001, num_dig)
    iqr <- 1.5*(gn_pnts[5]-gn_pnts[3])
    gn_pnts[2] <- round( gn_pnts[3]-iqr+0.0000000001, num_dig)
    if( gn_pnts[2] < gn_pnts[1] )
    { gn_pnts[2] <- gn_pnts[1] }
    gn_pnts[6] <- round( gn_pnts[5]+iqr+0.0000000001, num_dig)
    if( gn_pnts[6] > gn_pnts[7] )
    { gn_pnts[6] <- gn_pnts[7] }
    L1[ lcl_L2[1] ] <<-  round( gn_pnts[1] + (gn_pnts[2]-gn_pnts[1])*gn_get_urand(), num_dig )
    L1[ lcl_L2[2] ] <<- gn_pnts[3]
    L1[ lcl_L2[3] ] <<- gn_pnts[4]
    L1[ lcl_L2[4] ] <<- gn_pnts[5]
    L1[ lcl_L2[5] ] <<-  round( gn_pnts[6] + (gn_pnts[7]-gn_pnts[6])*gn_get_urand(), num_dig )
    j <- 6
    
    k <- gn_pnts[3] - gn_pnts[2]
    m <- gn_pnts[6] - gn_pnts[5]
    
    for (i in 1:(n-2) )
    { L1[ lcl_L2[ j ] ] <<- round( gn_pnts[2]+k*gn_get_urand(), num_dig)
    j <- j + 1
    L1[ lcl_L2[ j ] ] <<- round( gn_pnts[5]+m*gn_get_urand(), num_dig)
    j <- j + 1
    }
    
    k <- gn_pnts[4] - gn_pnts[3]
    m <- gn_pnts[5] - gn_pnts[4]
    
    for (i in 1:(n-1) )
    { L1[ lcl_L2[ j ] ] <<- round( gn_pnts[3]+k*gn_get_urand(), num_dig)
    j <- j + 1
    L1[ lcl_L2[ j ] ] <<- round( gn_pnts[4]+m*gn_get_urand(), num_dig)
    j <- j + 1
    }
    
  }
  else
  {
    cat("Not a recognized style at this point.\n" )
  }
  
  "DONE "
  
}