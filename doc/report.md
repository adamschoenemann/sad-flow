---
title: Flow Report
author: Adam Schønemann
---

Results
=======

Our implementation successfully computes a flow of 163 on the input
file, confirming the analysis of the American enemy.

We have analysed the possibilities of descreasing the capacities near
Minsk. Our analysis is summaries in the following table:

| case | 4W-48 | 4W-49 | Effect on Flow       |
| :--: | :---: | :---: | :------------------: |
| 1    | 30    | 30    | no change (original) |
| 2    | 30    | 20    | no change            |
| 3    | 20    | 30    | no change            |
| 4    | 20    | 20    | no change            |

In case 4 we have a new bottleneck, namely
10-25, 11-25, 11-24, 12-23, 18-22, 20-23, 20-21, 27-26, 28-30

The comrade from Minsk is advised to reduce the capacity, since it does not
affect the maximum throughput of backdoored iPhones we can ship to the capitalists!.

Implementation details
======================
We use Schroeder et al's [^shroeder] algorithm for finding maximum flows of
undirected graphs. The algorithm creates two residual edges for each undirected
edge - one in each direction.
Initially, each edge in the residual graph has a capacity equal to their corresponding
edge in the actual graph.
If you push $x$ flow in one direction through an edge, the residual edge
that goes in the other direction has its capacity incremented by $x$, while the
residual edge that goes in the same direction has its capacity decremented by $x$.

Augmented paths are found using a standard breadth-first search.

Running time
------------
Let $n$ be the number of nodes in the graph and $m$ be the number of edges.
Let $C = \sum_{e \text{ out of } s}c_e$. Since each iteration of the algorithm
finds an augmented path that augments the value of the flow with at least $1$,
the algorithm cannot run more iterations than $C$. The residual graph $G_f$ has
at most $2m$ edges. The edges are stored in adjacency lists, so looking up the
edges of nodes is a constant time operations. Augmenting paths are found using
breadth-first search which is $O(m + n) = O(m)$ since $m ≥ n/2$. The capacities
and flows are stored in hashmaps, so queries are constant time. `augment` takes
$O(n)$ time as the path has at most $n-1$ edges. The Residual graph $G_f$ is just a
wrapper over the graph $G$ that recomputes capacities (constant time) based on
the original capacities and the flow (all $O(1)$).
All in all, since there are $C$ iterations and each step takes $O(m)$ time, the
total running time is $O(mC)$.

Implementation
--------------


[^shroeder]: [Schroeder et al, 2004](http://www.inf.ufpr.br/pos/techreport/RT_DINF003_2004.pdf)