#<--------------------------------   HYP   ------------------------------------>
"hyp" <- function(chi=0.5, psi=2, mu=0, sigma=1, gamma=0, alpha.bar=NULL, data=NULL){
  ghyp(lambda=(length(mu)+1)/2, chi=chi, psi=psi, mu=mu, sigma=sigma, 
       gamma=gamma, alpha.bar=alpha.bar, data=data)
}
#<--------------------------------   NIG   ------------------------------------>
"NIG" <- function(chi=2, psi=2, mu=0, sigma=1, gamma=0, alpha.bar=NULL, data=NULL){
  ghyp(lambda=-0.5, chi=chi, psi=psi, mu=mu, sigma=sigma, 
       gamma=gamma, alpha.bar=alpha.bar, data=data)
}
#<--------------------------------   t   ------------------------------------>
"student.t" <- function(nu=5, mu=0, sigma=1, gamma=0, data=NULL){
  ghyp(lambda= -nu/2, psi=0, mu=mu, sigma=sigma, 
       gamma=gamma, alpha.bar=0, data=data)
}
#<--------------------------------   VG   ------------------------------------>
"VG" <- function(lambda=1, mu=0, sigma=1, gamma=0, data=NULL){
  ghyp(lambda=lambda, chi=0, psi=2 * lambda, mu=mu, sigma=sigma, 
       gamma=gamma, alpha.bar=NULL, data=data)
}
