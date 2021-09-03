---
title: Notes on sigexUI Development
author: Andrew
output:
  html_document:
    number_sections: true
---

# Introduction
These are transient notes while working on the interface.

Some book chapters on S3, S4, R6 programming are here:
<https://adv-r.hadley.nz/oo.html>

A guide to S4 classes and methods:
<https://bioconductor.org/help/course-materials/2017/Zurich/S4-classes-and-methods.html>

# What we have so far

1. Classes for building parameters whose structure is appropriate for the
   desired type of model.
	a. Use the `%>%` operator to compose multiple components.
	a. User can provide GCDs or general matrices (which are transformed into GCDs).

1. Classes for building a model.
	a. Not sure if this means SigexModel and SigexModelComponent classes, an
	   AddSigexComponent method, etc.
	a. SigexModelComponents might be almost redundant with SigexParams. Make
	   sure to avoid too unnecessary redundancy. Also, maybe we don't need some
	   of the model metadata now that its in the parameters.
	a. Use the `%>%` operator to compose multiple components.
	a. Print a model and see a summary of all the added components.
	a. Add regression component to model.

1. Converting `ar` outputs to SigexParam objects

# Some things to consider next

1. For the MLE and MOM functions, there should either be a default initial
  parameter like "zero", or at least an easy way to create one that doesn't
  require too much thinking.

# Some things to consider later

1. Some potential suggestions for sigex
	a. The MLE function takes a long time. The vast majority of time seems to be
	   spent in `mvar.midcast`. I wonder if the performance can be improved,
	   e.g. by writing it in C++, but this looks like it would be an involved
	   effort.

	a. For the MLE function, it would be useful to let the user pass a control
	   object for the optimizer. This would let users set the trace level, for
	   example, so that the `debug` flag (which seems to print the likelihood
	   value each time it's computed) may not be necessary.

	a. Some functions appear to take arguments that should not be necessary. For
	   example, `sigex.psi2par` takes a dataset, but data should not be needed
	   to transform the parameters to another representation.

	a. Many places in sigex return lists that would benefit from labels. An
	   example of this is `getGCD`. The first element of the return value could
	   be labeled `L` and the second element could be labeled `D` or `D_vec`.
	   This would help with readability and maintainability of the code.

	a. The `meaninit` function seems like it could be more general. For example,
	   the user could prepare the covariate themself (maybe with some helper
	   functions from us) and pass them into the model.

	a. I noticed some places where sigex catches errors and returns error codes.
	   It might be better if it just let the errors be thrown so that they propogate
	   up to someone else who will handle them.

1. Make sure S4 is being used properly within a package
	a. Exported correctly?
	a. Roxygen?
