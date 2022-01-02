/** upsh_version(-Vers).
    upsh_version(-Vers,-Date).

Version and release date for upsh package.

==
?- upsh_version( V, D).
V = 2:5:0,
D = date(2019, 12, 10).
==

*/
% upsh_version(2:1:0).
% upsh_version(2:1:0,date(2017,6,3)).
% upsh_version(2:1:1,date(2018,1,3)).
% upsh_version(2:2:0,date(2018,4,29)).
% upsh_version(2:3:0,date(2018,5,2)).
% upsh_version(2:4:0,date(2018,12,10)).
upsh_version(2:5:0). % this is read by upsh so keep first and as is...!
upsh_version(2:5:0,date(2019,12,23)).
