# InterSystems IRIS Demo: Deploy a Sharded Cluster

Sharded Cluster

This page introduces you to the InterSystems IRIS® data platform sharding feature and its use in a sharded cluster to horizontally scale InterSystems IRIS for data volume.

## 1 How Can Sharding Help You?

We are all managing more data than ever before and being asked to do more with it — and the response times demanded are growing ever shorter. Each business-specific w orkload presents different challenges to the data platform on which it operates — and as workloads grow, those challenges become even more acute.

InterSystems IRIS includes a comprehensive set of capabilities to scale your applications, which can be applied alone or in combination, depending on the nature of your workload and the specific performance challenges it f aces. One of these, sharding, partitions both data and its associated cache across a number of servers, providing fle xible, inexpensive performance scaling for queries and data ingestion while maximizing infrastructure value through highly efficient resource utilization. An InterSystems IRIS sharded cluster can provide significant performance benefits for a wide v
especially for those with workloads that include one or more of the following:

ariety of applications, but

- High-volume or high-speed data ingestion, or a combination.

- Relatively large data sets, queries that return large amounts of data, or both.

- Complex queries that do large amounts of data processing, such as those that scan a lot of data on disk or involve significant compute w ork.

Each of these factors on its own influences the potential g ain from sharding, but the benefit may be enhanced where the y combine. For example, a combination of all three factors — large amounts of data ingested quickly, large data sets, and complex queries that retrieve and process a lot of data — makes many of today’s analytic workloads very good candidates for sharding.

Note that these characteristics all have to do with data; the primary function of InterSystems IRIS sharding is to scale for
data volume. But a sharded cluster can also include features that scale for user volume, when workloads involving some or all of these data-related factors also experience a very high query volume from large numbers of users. And sharding can be combined with vertical scaling as well. With InterSystems IRIS, you can create just the right overall scaling solution for your workload’s performance challenges.

## 2 How Does Sharding Work?

The heart of the sharded architecture is the partitioning of data and its associated cache across a number of systems. A sharded cluster partitions large database tables horizontally — that is, by row — across multiple InterSystems IRIS instances, called data nodes, while allowing applications to access these tables through any one of those instances. Each data node’s
share of the cluster’s sharded data is called a shard. This architecture provides three advantages:

- Parallel processing More Sharded Cluster Options Queries are run in parallel on the data nodes, with the results combined, and returned to the application as full query results, significantly enhancing e xecution speed in many cases.

Partitioned caching

Each data node has its own dedicated cache, rather than a single instance’s cache serving the entire data set, which greatly reduces the risk of overflo wing the cache and forcing performance-degrading disk reads.

Parallel loading

- Data can be loaded onto the data nodes in parallel, reducing cache and disk contention between the ingestion workload and the query workload and improving the performance of both.

- A federated software component called the sharding manager keeps track of which data is on which data nodes and directs queries accordingly. Nonsharded data is stored on the first data node configured, called data node 1 (which also stores code and metadata). From the perspective of the application SQL, the distinction between sharded and nonsharded tables is totally transparent.

Figure 1: A Basic Sharded Cluster

## 3 More Sharded Cluster Options

Additional options for a sharded cluster include the following:

- You can add data nodes at any time and rebalance existing sharded data across the expanded set of data nodes. Rebalancing cannot coincide with queries and updates, and so can take place only when the sharded cluster is offline and no other sharded operations are possible. (See Add Nodes and Rebalance Data.) sharddataserversshardmasterdataservermasternamespace...tableapplicationconnections...shardnamespace...datashardshardnamespace...datashardshardnamespace...datashardshardnamespace...datashardshardingmanager Learn More About Sharding

- To add high availability for the data on the cluster, you can deploy data nodes as mirrored failover pairs. (See Mirror for High Availability.)

- For advanced use cases in which extremely low query latencies are required, potentially at odds with a constant influx of data, compute nodes can be added to provide a transparent caching layer for servicing queries. When a cluster includes compute nodes, read-only queries are automatically executed in parallel on the compute nodes, rather than
on the data nodes; all write operations (insert, update, delete, and DDL operations) continue to be executed on the data
nodes. This division of labor separates the query and data ingestion workloads while maintaining the advantages of parallel processing and distributed caching, improving the performance of both. (See Deploy Compute Nodes.)

## 4 Learn More About Sharding

To learn more about sharding, see

- Introduction to Sharding (online course)

- Sharding Basics (online course)

- Deploying InterSystems IRIS in Containers and the Cloud (learning path)

- Scalability Guide
