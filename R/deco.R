deco <- function()
{
  total <- 20
  # create progress bar
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  for(i in 1:total){
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
}