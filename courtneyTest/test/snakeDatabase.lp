% database of rattlesnakes
% colors: yellow, red, black, brown, silver
% patterns: spotted, striped,
% headshape: big_head, triangular
% rattle: rattle_yes, rattle_no
% length: large, medium, small
% large if 2m, medium if 1-2m, small if 0-1m


#compute 3 { red(X), big_head(X) }.

red(copperhead).
yellow(copperhead).
striped(copperhead).
big_head(copperhead).

poisonous(X):- big_head(X).
