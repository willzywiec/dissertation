> 
> library(gRain)
> 
> grain.bn <- as.grain(bn) # Bayesian network conditional probability tables
> 
> summary(grain.bn)
Independence network: Compiled: TRUE Propagated: FALSE 
 Nodes : Named chr [1:8] 'op' 'ctrl' 'mass' 'form' 'mod' 'rad' 'ref' 'thk'
 - attr(*, 'names')= chr [1:8] 'op' 'ctrl' 'mass' 'form' ...
 Number of cliques:                 6 
 Maximal clique size:               3 
 Maximal state space in cliques: 168042 
> 
> grain.bn$rip
cliques
  1 : op ctrl mass 
  2 : op ctrl form 
  3 : op ctrl mod 
  4 : op ctrl rad 
  5 : op ctrl ref 
  6 : op ctrl thk 
separators
  1 :  
  2 : op ctrl 
  3 : op ctrl 
  4 : op ctrl 
  5 : op ctrl 
  6 : op ctrl 
parents
  1 : 0 
  2 : 1 
  3 : 2 
  4 : 3 
  5 : 4 
  6 : 5 
> 
> finding <- setFinding(grain.bn, nodes = 'mass', states = '500')
> 
> pFinding(finding)
[1] 2.611831e-08
> 