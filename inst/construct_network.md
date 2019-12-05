### Constructing the network

The network can be constructed in different ways, depending on the definition of what consitutes a connection between facilities. In this framework, two facilities are connected if they have shared a same subject (i.e. a subject was admitted in both facilities), conditioning on some constraints. The different networks that can be constructed from a same database, differ in the constraints used (i.e. the parameters).

* The first type of constraint is the time span between two admissions. Two facilities will be connected in the network if they have shared a same subject within a defined time span. This parameters is the **window threshold** , expressed in days. Formally, the facilities will be connected if the time difference between the admission date of a stay N and the discharge date of a stay N-1 is *less than or equal to* the window threshold. A window threshold of zero days corresponds to a direct transfer.

* The second type of constraint is whether to consider **all stays** of a subject within the window threshold, or only **successive** stays. If considering only successive stays, two facilities will be connected if they have shared a subject within the window threshold AND if the subject was not admitted in another facility in between. If considering all stays, two facilities will be connected if the window threshold constraint is met, no matter how many facilities the subject visited in between.

* Additional variables can be used as a third constraint to identify connections between facilities. They are called flag variables. See the *vignette* for more details on how to use flag variables. The network can be constructed conditioning only on the dates variables, only on the flag variables, or on both.

A connection will be recorded between two facilities for each of the pair of stays meeting the constraints. Thus, the core data structure of the network is a table, in which each row corresponds to a single connection: it is called the *edgelist* of the network, and it is said to be in *long* format. The edgelist can be *aggregated*, each row thus representing a unique pair of connected facilities, with an additional variable counting the number of individual connections between these facilities.

Two final parameters are needed to construct the network:

* **remove loops.** Two stays of a subject that meet the above constraints will constitute a connection, whether the two facilities are different or not. This can result in facilities being connected to themselves, which are called loops. Loops can be removed from the network by switching the corresponding button.

* **transfer cutoff.** If the aggregated number of connections between two facilities is below this cutoff, the connections will be removed from the network.
