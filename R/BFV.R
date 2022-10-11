#' @name GenPolyMod
#' @title Generate Polynomial Modulo
#' @param n the order
#' @return polynomial of the form x^^n + 1
#' @export
GenPolyMod <- function(n)
  polynomial( coef=c(1, rep(0, n-1), 1 ) )


#' @name GenPubKey0
#' @title Generate part 0 of the Public Key
#' @param a a
#' @param s s
#' @param e e
#' @param pm pm
#' @param q q
#' @return polynomial
#' @export
GenPubKey0 <- function(a, s, e, pm, q) {
  temp = -(a*s + e) # e should be generated in here, not fed into it
  temp = temp %% pm
  temp = HEtools::CoefMod(temp, q)
  return(temp)
}

#' @name GenPubKey1
#' @title Generate part 1 of the Public Key
#' @param a a
#' @return polynomial
#' @export
GenPubKey1 <- function(a)
  return(a)

#' @name GenPubKey
#' @title Generate the Public Key
#' @param a a
#' @param n n
#' @param e e
#' @param pm pm
#' @return list with the two polynomials that form the public key
#' @export
GenPubKey <- function(a, n, e, pm) {
  temp     = list()
  temp$pk0 = GenPubKey0(a, n, e, pm)
  temp$pk1 = GenPubKey1(a          )
  return(temp)
}

#' @name EncryptPoly0
#' @title Encrypt Polynomial Message Part 0
#' @param m message
#' @param pk0 public key part 0
#' @param u u
#' @param e1 e1
#' @param p p
#' @param pm pm
#' @param q q
#' @return polynomial which contains the message in ciphertext
#' @export
EncryptPoly0 <- function(m, pk0, u, e1, p, pm, q) {
  temp = pk0 * u + e1 + floor(q/p) * m
  temp = temp %% pm
  temp = HEtools::CoefMod(temp, q)
  return(temp)
}

#' @name EncryptPoly1
#' @title Encrypt Polynomial Message Part 1
#' @param pk1 public key part 1
#' @param u u
#' @param e2 e2
#' @param pm pm
#' @param q q
#' @return polynomial which contains the message in ciphertext
#' @export
EncryptPoly1 <- function(pk1, u, e2, pm, q) {
  temp = pk1 * u + e2
  temp = temp %% pm
  temp = HEtools::CoefMod(temp, q)
  return(temp)
}

