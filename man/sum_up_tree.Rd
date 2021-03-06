% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_funs.r
\name{sum_up_tree}
\alias{sum_up_tree}
\title{Sum Property Up Tree}
\usage{
sum_up_tree(node, property)
}
\arguments{
\item{node}{data.tree node, for which a specified quantity is aggregated}

\item{property}{name of a property to be aggregated}
}
\value{
a data.tree with <property>_agg as a property
}
\description{
Recursive function to be applied to a tree node,
will *sum* a specified quantity from all child nodes, and grandchildren etc
}
\details{
Does this by calling itself on the $children of a node.
Once it reaches a leaf, there are no children.

Works as a side effect, output does not need to be assigned
}
