#LCA for files with assigned cmat 
#results (MWA, SE, vi) from these LCAs were manually entered into results.cmat for each file

library(SAGA2)

#armbruster files
dat <- read.csv("eFL.ME.high.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("eFL.ME.low.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("eFL.wFL.high.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("eFL.wFL.low.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("ON.ME.high.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("ON.ME.low.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("wFL.ON.high.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("wFL.ON.low.csv")
cmat <- as.matrix(read.csv("cmat.armbruster.csv"))
res <- LCA(dat, Cmatrix = cmat)


#etterson files
#cmat ACE
dat <- read.csv("seed.fruit.A.csv")
cmat <- as.matrix(read.csv("cmat.etterson.ACE.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seed.fruit.C.csv")
cmat <- as.matrix(read.csv("cmat.etterson.ACE.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seed.fruit.E.csv")
cmat <- as.matrix(read.csv("cmat.etterson.ACE.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seedwt.A.csv")
cmat <- as.matrix(read.csv("cmat.etterson.ACE.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seedwt.C.csv")
cmat <- as.matrix(read.csv("cmat.etterson.ACE.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seedwt.E.csv")
cmat <- as.matrix(read.csv("cmat.etterson.ACE.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

#cmat BD
dat <- read.csv("seed.fruit.B.csv")
cmat <- as.matrix(read.csv("cmat.etterson.BD.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seed.fruit.D.csv")
cmat <- as.matrix(read.csv("cmat.etterson.BD.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seedwt.B.csv")
cmat <- as.matrix(read.csv("cmat.etterson.BD.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seedwt.D.csv")
cmat <- as.matrix(read.csv("cmat.etterson.BD.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

#cmat F
dat <- read.csv("seed.fruit.F.csv")
cmat <- as.matrix(read.csv("cmat.etterson.F.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)

dat <- read.csv("seedwt.F.csv")
cmat <- as.matrix(read.csv("cmat.etterson.F.csv"))
res <- LCA(dat, SCS = "NSC", Cmatrix = cmat)


#fox files
dat <- read.csv("eggdisp.BF.csv")
cmat <- as.matrix(read.csv("cmat.fox.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("eggdisp.cowpeaA.csv")
cmat <- as.matrix(read.csv("cmat.fox.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("eggdisp.cowpeaB.csv")
cmat <- as.matrix(read.csv("cmat.fox.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("eggdisp.cowpeaC.csv")
cmat <- as.matrix(read.csv("cmat.fox.csv"))
res <- LCA(dat, Cmatrix = cmat)


#mcclelland files
dat <- read.csv("salmonL.1-16-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.10-21-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.10-29-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.2-1-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.4-15-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.6-28-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.6-5-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonL.7-24-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.1-16-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.10-21-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.10-29-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.2-1-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.2-26-1.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.4-15-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.6-28-3.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.6-5-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)

dat <- read.csv("salmonW.7-24-2.csv")
cmat <- as.matrix(read.csv("cmat.mcclelland.csv"))
res <- LCA(dat, Cmatrix = cmat)


#miller files
#cmat sperm
dat <- read.csv("dros.sperm.csv")
cmat <- as.matrix(read.csv("cmatrix.sperm.csv"))
res <- LCA(dat, Cmatrix = cmat)

#cmat sr
dat <- read.csv("dros.sr.csv")
cmat <- as.matrix(read.csv("cmatrix.sr.csv"))
res <- LCA(dat, Cmatrix = cmat)
