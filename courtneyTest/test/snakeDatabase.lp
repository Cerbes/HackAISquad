% database of snakes
% colors: yellow, red, black, brown, white, orange, green, beige, grey, blue
% patterns: spotted, striped, gradient, no_pattern
% if striped: horizontal_, vertical_
% if spotted: large_, small_
% headshape: big_head, small_head
% rattle: rattle_yes, rattle_no
% length: large, medium, small
% large if 2m, medium if 1-2m, small if 0-1m
% when identified, user will be notified if matched snake is
% venomous or not

#compute 3 { red(X), yellow(X), vertical_striped(X), big_head(X), rattle_no(X), medium(X) }.

% Copperhead(venomous)
red(copperhead).
yellow(copperhead).
vertical_striped(copperhead).
big_head(copperhead).
rattle_no(copperhead).
medium(copperhead).
venomous(copperhead).

% Cottonmouth(venomous)
brown(cottonmouth).
yellow(cottonmouth).
black(cottonmouth).
no_pattern(cottonmouth).
big_head(cottonmouth).
rattle_no(cottonmouth).
medium(cottonmouth).
venomous(cottonmouth).

% Glossy Snake(non-venomous)
yellow(glossy_snake).
brown(glossy_snake).
small_spotted(glossy_snake).
small_head(glossy_snake).
rattle_no(glossy_snake).
medium(glossy_snake).

% Trans-Pecos Rat Snake(non-venomous)
yellow(trans_pecos_rat_snake).
black(trans_pecos_rat_snake).
horizontal_striped(trans_pecos_rat_snake).
small_spotted(trans_pecos_rat_snake).
small_head(trans_pecos_rat_snake).
rattle_no(trans_pecos_rat_snake).
medium(trans_pecos_rat_snake).

% Western Worm Snake(non-venomous)
black(western_worm_snake).
red(western_worm_snake).
horizontal_striped(western_worm_snake).
small_head(western_worm_snake).
rattle_no(western_worm_snake).
small(western_worm_snake).

% Scarlet Snake(non-venomous)
black(scarlet_snake).
red(scarlet_snake).
white(scarlet_snake).
large_spotted(scarlet_snake).
small_head(scarlet_snake).
rattle_no(scarlet_snake).
small(scarlet_snake).

% Racer(non-venomous)
black(racer).
no_pattern(racer).
small_head(racer).
rattle_no(racer).
medium(racer).

% Regal Black-striped Snake(mildly venomous)
black(regal_black_striped_snake).
yellow(regal_black_striped_snake).
brown(regal_black_striped_snake).
horizontal_striped(regal_black_striped_snake).
small_head(regal_black_striped_snake).
rattle_no(regal_black_striped_snake).
small(regal_black_striped_snake).
mild_venomous(regal_black_striped_snake).

% Western Diamondback Rattlesnake(venomous) ************* pg 2
white(western_diamondback_rattlesnake).
beige(western_diamondback_rattlesnake).
brown(western_diamondback_rattlesnake).
large_spotted(western_diamondback_rattlesnake).
big_head(western_diamondback_rattlesnake).
rattle_yes(western_diamondback_rattlesnake).
large(western_diamondback_rattlesnake).
venomous(western_diamondback_rattlesnake).

% Timber Rattlesnake(venomous)
black(timber_rattlesnake).
white(timber_rattlesnake).
brown(timber_rattlesnake).
vertical_striped(timber_rattlesnake).
small_spotted(timber_rattlesnake).
big_head(timber_rattlesnake).
rattle_yes(timber_rattlesnake).
medium(timber_rattlesnake).
venomous(timber_rattlesnake).

% Rock Rattlesnake(venomous)
beige(rock_rattlesnake).
black(rock_rattlesnake).
white(rock_rattlesnake).
vertical_striped(rock_rattlesnake).
big_head(rock_rattlesnake).
rattle_yes(rock_rattlesnake).
small(rock_rattlesnake).
venomous(rock_rattlesnake).

% Black-tailed Rattlesnake(venomous)
beige(black_tailed_rattlesnake).
brown(black_tailed_rattlesnake).
black(black_tailed_rattlesnake).
vertical_striped(black_tailed_rattlesnake).
small_spotted(black_tailed_rattlesnake).
big_head(black_tailed_rattlesnake).
rattle_yes(black_tailed_rattlesnake).
medium(black_tailed_rattlesnake).
venomous(black_tailed_rattlesnake).

% Mohave Rattlesnake(venomous)
white(mohave_rattlesnake).
black(mohave_rattlesnake).
green(mohave_rattlesnake).
large_spotted(mohave_rattlesnake).
big_head(mohave_rattlesnake).
rattle_yes(mohave_rattlesnake).
medium(mohave_rattlesnake).
venomous(mohave_rattlesnake).

% Western Rattlesnake(venomous)
brown(western_rattlesnake).
beige(western_rattlesnake).
black(western_rattlesnake).
white(western_rattlesnake).
large_spotted(western_rattlesnake).
big_head(western_rattlesnake).
rattle_yes(western_rattlesnake).
medium(western_rattlesnake).
venomous(western_rattlesnake).

% Ring-necked Snake(non-venomous)
orange(ring_necked_snake).
yellow(ring_necked_snake).
brown(ring_necked_snake).
no_pattern(ring_necked_snake).
small_head(ring_necked_snake).
rattle_no(ring_necked_snake).
small(ring_necked_snake).

% Indigo Snake(non-venomous)
black(indigo_snake).
no_pattern(indigo_snake).
small_head(indigo_snake).
rattle_no(indigo_snake).
large(indigo_snake).

% Central American Speckled Racer(non-venomous)
black(central_american_speckled_racer).
red(central_american_speckled_racer).
orange(central_american_speckled_racer).
green(central_american_speckled_racer).
yellow(central_american_speckled_racer).
small_spotted(central_american_speckled_racer).
small_head(central_american_speckled_racer).
rattle_no(central_american_speckled_racer).
medium(central_american_speckled_racer).

% Baird's Rat Snake(non-venomous)
grey(baird_rat_snake).
red(baird_rat_snake).
black(baird_rat_snake).
horizontal_striped(baird_rat_snake).
small_head(baird_rat_snake).
rattle_no(baird_rat_snake).
medium(baird_rat_snake).

% Great Plains Rat Snake(non-venomous)
orange(great_plains_rat_snake).
beige(great_plains_rat_snake).
black(great_plains_rat_snake).
large_spotted(great_plains_rat_snake).
small_head(great_plains_rat_snake).
rattle_no(great_plains_rat_snake).
medium(great_plains_rat_snake).

% Rat Snake(non-venomous)
yellow(rat_snake).
brown(rat_snake).
black(rat_snake).
beige(rat_snake).
large_spotted(rat_snake).
horizontal_striped(rat_snake).
small_head(rat_snake).
rattle_no(rat_snake).
large(rat_snake).

% Mud Snake(non-venomous)
black(mud_snake).
red(mud_snake).
large_spotted(mud_snake).
small_head(mud_snake).
rattle_no(mud_snake).
large(mud_snake).

% Chihuahuan Hook-nosed Snake(mildly venomous)
beige(chihuahuan_hook_nosed_snake).
brown(chihuahuan_hook_nosed_snake).
black(chihuahuan_hook_nosed_snake).
vertical_striped(chihuahuan_hook_nosed_snake).
small_head(chihuahuan_hook_nosed_snake).
rattle_no(chihuahuan_hook_nosed_snake).
small(chihuahuan_hook_nosed_snake).
mild_venomous(chihuahuan_hook_nosed_snake).

% Hog-nosed Snake(non-venomous)
black(hog_nosed_snake).
grey(hog_nosed_snake).
vertical_striped(hog_nosed_snake).
small_head(hog_nosed_snake).
rattle_no(hog_nosed_snake).
medium(hog_nosed_snake).

% Eastern Hog Nosed Snake
black(eastern_hog_nosed_snake).
yellow(eastern_hog_nosed_snake).
large_spotted(eastern_hog_nosed_snake).
big_head(eastern_hog_nosed_snake).
rattle_no(eastern_hog_nosed_snake).
medium(eastern_hog_nosed_snake).

% Night Snake(non-venomous)
brown(night_snake).
beige(night_snake).
black(night_snake).
large_spotted(night_snake).
small_spotted(night_snake).
big_head(night_snake).
rattle_no(night_snake).
small(night_snake).

% Gray-banded Kingsnake (non-venomous)
yellow(gray_banded_kingsnake).
black(gray_banded_kingsnake).
orange(gray_banded_kingsnake).
grey(gray_banded_kingsnake).
vertical_striped(gray_banded_kingsnake).
small_head(gray_banded_kingsnake).
