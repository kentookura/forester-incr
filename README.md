# Incrementally rendering forests

The rendering algorithm of forester should be an incremental computation: After
making an edit to one or more trees, we should only rerender those trees
affected by the changes. For the sake of simplicity, let us start with
determining the set S of trees affected by a single tree T.

It is easy to determine some subsets of S. Certainly

{tree with path to/from S in transclusion graph} ∪ {pred/succ of S in link graph} ⊆ S

However, we must also rerender those trees whose queries match S. How can this
be done efficiently?

We could use `Graph.Imperative.Digraph.ConcreteLabled (Addr) (Query)` and add
an edge whenever `run_query` returns an `Addr_set.t`. 

# Fast queries for the hypermedia server

In order to avoid rerunning queries, I created a triemap indexed by `addr
Query.t`. Lookup procedes by matching on the strucure of the query.

It may be used as a cache for storing query results. For queries such as
`Isect` and `Union`, we can compute the result by looking up its parts, only
running the query when it is not present in the triemap.
