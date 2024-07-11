# Incrementally rendering forests

The rendering algorithm of forester should be an incremental computation: After
making an edit to one or more trees, we should only rerender those trees
affected by the changes. The tricky bit is determining which trees require
re-evaluation and/or re-rendering.

## Change propagation

Some scenarios

- We change the title of $T$. $T$ needs to be re-evaluated. After reevaluation
is finished, we need to rerender all trees whose queries return a tree that is
changed by re-evaluation, and those trees adjacent to $S$ in the link graph
*and* those trees with paths from $S$ in the transclusion graph. We can reuse
the graphs for some of these computations, but we need a `Addr_set.t
Addr_map.t` such that `lookup addr m` returns the set of trees with queries
that match `addr`. This map also needs to be updated appropriately.

- We change the structure of a query in $T$. $T$ needs to be re-rendered, and
so do all trees with queries that match $T$. No re-evaluation needs to take
place.

- ...

## Designing the algorithm

Let us say that $T$ user-queries $S$ when the body of $T$ contains a query
expression that matches $S$. There is then a general user-query relation
(graph) on the set of trees. This is significant because the relational graphs
are constructed at eval time, while user-written queries are evaluated at
render time. So even though they both use the query mechanism, they can not be
treated uniformly. Changes to user-queries only require rerendering.

Should we use a library such as `current_incr` or make a custom solution?
`current_incr` is based on "Adaptive Functional Programming" by Acar, Blelloch
and Harper. I've not dug too deep into the paper yet, but it seems that the
core idea is to use a dynamic dependence graph and a change propagation
algorithm. In our situation, the dependence graph should be a union of the link
graph, the transitive closure of the transclusion graph, the opposite of the
transclusion graph (explained below) and the graph of the general "$T$
user-queries $S$" relation. The change propagation is then also relatively
straightforward.

## How will `forester build` change?

One possibility is to use the "development server" model. Forester can keep
these graphs in memory and react to changes. One issue here is that there does
not seem to be a reliable cross-platform file watcher. We can use dream to make
a generic endpoint for notifying forester of changes. Formally, we can think of
it as a Mealy machine or something.

Another possibility is to cache this information in the build directory, so
that successive calls to `forester build` first check the cache, then rerender
only what is necessary. If we take this approach, the rendered xml should also
be rendered to the build directory and the copied, because otherwise we would
need to check the integrity of the output directory.

Forester should probably be able to do both. The dev server approach seems
easier to implement. For the other approach, we need to find a method for
efficiently storing the data structures on disk.

# Fast queries for the hypermedia server

In order to avoid rerunning queries, I created a triemap indexed by `addr
Query.t`. Lookup procedes by matching on the strucure of the query.

It may be used as a cache for storing query results. For queries such as
`Isect` and `Union`, we can compute the result by looking up its parts, only
running the query when it is not present in the triemap.
