# The purpose of this script is to have matching objects from both sigexUI
#	and original sigex for testing purposes.


# ---- model as SigexModel ----
model = SigexModel(N) %>%
	addComponent(SigexModelComponentARMA(p = 1,
										  q = 0,
										  epithet = "process",
										  delta = c(1,-1))) %>%
	addComponent(SigexModelComponentVARMA(p = 0,
										  q = 0,
										  epithet = "irregular",
										  delta = 1)) %>%
	setRegComponent(data.ts, d = 0)

model

# ---- model from old sigex ----
mdl = NULL
mdl = sigex.add(mdl = mdl,
				vrank = seq(1,N),
				class = "arma",
				order = c(1, 0),
				bounds = NULL,
				name = "process",
				delta = c(1,-1) )
mdl = sigex.add(mdl = mdl,
				vrank = seq(1,N),
				class = "arma",
				order = c(0, 0),
				bounds = NULL,
				name = "irregular",
				delta = 1 )
mdl = sigex.meaninit(mdl = mdl, data.ts = data.ts, d = 0)
