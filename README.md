README for package Erlang
========================================================

The "Erlang" package contains functions related to (call center) staffing and the Erlang C formula. It can be used, for example, to create a list of required staffing levels, based on forecasted volume and average handling time for a number of days or intervals.

The package revolves around the Erlang C function, which calculates the change of calls having to wait before an agents becomes available to serve them. Based on this function, the package can calculate
- what service level will be achieved, given arrival rate of the calls, the average handle time of calls and the number of agents
- the average waiting time (or average speed of answer) of calls
- the number of agents needed to achieve a certain service goal (either service level or ASA)

