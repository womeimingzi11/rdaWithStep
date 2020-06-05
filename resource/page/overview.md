## What is rdaWithStep？
A shiny app to perform RDA with variable selection and create awesome figures and tables.

## Wha is RDA with step selection?

Briefly, the Monte Carlo permutation tests followed by backward, forward or bothward selection were used to determine which variable was contained in each variable set.

As [vegan::ordistep](https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/ordistep) described:

> The basic functions for model choice in constrained ordination are add1.cca and drop1.cca. With these functions, ordination models can be chosen with standard R function step which bases the term choice on AIC. AIC-like statistics for ordination are provided by functions deviance.cca and extractAIC.cca (with similar functions for rda). Actually, constrained ordination methods do not have AIC, and therefore the step may not be trusted. This function provides an alternative using permutation P-values.

> Function ordistep defines the model, scope of models considered, and direction of the procedure similarly as step. The function alternates with drop and add steps and stops when the model was not changed during one step. The - and + signs in the summary table indicate which stage is performed. It is often sensible to have Pout > Pin in stepwise models to avoid cyclic adds and drops of single terms

## Focus on species or sample site?

In general, there are two main scopes of RDA:

  1. determine the relationships of species and environment variables only;
  2. except determine the relationships of species and environment variables, the simple sites were also considered.
  
In this case, adding sample sites in the figure is not in my plan yet.

However, you are welcomed to commit any feature about this and even any other features in my [repo on GitHub](https://github.com/womeimingzi11/rdaWithStep).

You are also welcomed to visit my [Blog (in Chinese)](https://womeimingzi11.github.io) or contact me by [mail](mailto://chenhan28@gmail.com).