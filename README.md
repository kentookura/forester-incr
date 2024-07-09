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
an edge whenever `run_query` returns an `Addr_set.t`. This repo is not the
place for this experiment.

# Fast queries for the hypermedia server

In order to avoid rerunning queries, I created a triemap indexed by `addr
Query.t`. It can efficiently look up stored values by matching on the strucure
of the query. It might be useful when rendering query results, or when in a
dynamic environment in which a user can provide queries. For queries such as
`Isect` and `Union`, we can construct the search result by looking up its
parts, only querying when we need to and compute the result.
