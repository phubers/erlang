# README for package Erlang

The "Erlang" package contains functions related to (call center) staffing and the Erlang C formula. It can be used, for example, to calculate required staffing levels, based on forecasted volume and average handling time for day or time interval.

The package revolves around the Erlang C function, which calculates the change of calls having to wait before an agents becomes available to serve them. Based on this function, the package can calculate:

- what service level will be achieved, given arrival rate of the calls, the average handle time of calls and the number of agents
- the average waiting time (or average speed of answer) of calls
- the number of agents needed to achieve a certain service goal (either service level or ASA)

## Erlang B formula
The Erlang B formula is used to calculate the chance of blocked calls (busy tone). This can be used to determine how many trunks are needed to handle call traffic. It is also used in the Erlang C formula.

## Erlang C formula
The Erlang C formula is used to calculate the chance of queued calls (waiting to be answered). This can be used to determine how many operators/agents are needed to answer all arriving calls. 

Note that the Erlang C formula does not consider call abandonment: it assumes that all callers will remain in the queue until they are being served. In reality, some callers will abandon when the waiting time exceeds their patience. This has the effect of improving the waiting time for subsequent callers in the queue. Abandonment will therefore improve the service level for remaining callers. The net effect of this is, that the Erlang C formula will essentially overestimate the numbers of operators/agents that are needed.

Techniques do exist to mitigate this problem somewhat, but these have not been implemented in the package yet. 

