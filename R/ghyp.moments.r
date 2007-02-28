"ghyp.moments" <- function(object)
{
  test.class.ghyp(object,case="ghypbase")
  return(list(e.ghyp = object@expected.value, var.ghyp=object@variance,
              e.gig = Egig(object@lambda,object@chi,object@psi,func="x"),
              var.gig = Egig(object@lambda,object@chi,object@psi,func="var")))
}