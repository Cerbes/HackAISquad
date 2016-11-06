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

#include 'query.lp'.

% Copperhead(venomous)
orange(copperhead).
beige(copperhead).
not_black(copperhead).
not_brown(copperhead).
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
not_black(glossy_snake).
small_spotted(glossy_snake).
small_head(glossy_snake).
rattle_no(glossy_snake).
medium(glossy_snake).

% Trans-Pecos Rat Snake(non-venomous)
yellow(trans_pecos_rat_snake).
black(trans_pecos_rat_snake).
not_brown(trans_pecos_rat_snake).
horizontal_striped(trans_pecos_rat_snake).
small_spotted(trans_pecos_rat_snake).
small_head(trans_pecos_rat_snake).
rattle_no(trans_pecos_rat_snake).
medium(trans_pecos_rat_snake).

% Western Worm Snake(non-venomous)
black(western_worm_snake).
red(western_worm_snake).
not_brown(western_worm_snake).
horizontal_striped(western_worm_snake).
small_head(western_worm_snake).
rattle_no(western_worm_snake).
small(western_worm_snake).

% Scarlet Snake(non-venomous)
black(scarlet_snake).
red(scarlet_snake).
white(scarlet_snake).
not_brown(scarlet_snake).
large_spotted(scarlet_snake).
small_head(scarlet_snake).
rattle_no(scarlet_snake).
small(scarlet_snake).

% Racer(non-venomous)
black(racer).
not_brown(racer).
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
venomous(regal_black_striped_snake).

% Western Diamondback Rattlesnake(venomous)
white(western_diamondback_rattlesnake).
beige(western_diamondback_rattlesnake).
brown(western_diamondback_rattlesnake).
not_black(western_diamond_rattlesnake).
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
not_brown(rock_rattlesnake).
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
not_brown(mohave_rattlesnake).
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
not_black(ring_necked_snake).
vertical_striped(ring_necked_snake).
small_head(ring_necked_snake).
rattle_no(ring_necked_snake).
small(ring_necked_snake).

% Indigo Snake(non-venomous)
black(indigo_snake).
not_brown(indigo_snake).
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
not_brown(central_american_speckled_racer).
small_spotted(central_american_speckled_racer).
small_head(central_american_speckled_racer).
rattle_no(central_american_speckled_racer).
medium(central_american_speckled_racer).

% Baird's Rat Snake(non-venomous)
grey(baird_rat_snake).
red(baird_rat_snake).
black(baird_rat_snake).
not_brown(baird_rat_snake).
horizontal_striped(baird_rat_snake).
small_head(baird_rat_snake).
rattle_no(baird_rat_snake).
medium(baird_rat_snake).

% Great Plains Rat Snake(non-venomous)
orange(great_plains_rat_snake).
beige(great_plains_rat_snake).
black(great_plains_rat_snake).
not_brown(great_plains_rat_snake).
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
not_brown(mud_snake).
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
venomous(chihuahuan_hook_nosed_snake).

% Hog-nosed Snake(non-venomous)
black(hog_nosed_snake).
grey(hog_nosed_snake).
not_brown(hog_nosed_snake).
vertical_striped(hog_nosed_snake).
small_head(hog_nosed_snake).
rattle_no(hog_nosed_snake).
medium(hog_nosed_snake).

% Eastern Hog Nosed Snake
black(eastern_hog_nosed_snake).
yellow(eastern_hog_nosed_snake).
not_brown(eastern_hog_nosed_snake).
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
not_brown(gray_banded_kingsnake).
vertical_striped(gray_banded_kingsnake).
small_head(gray_banded_kingsnake).
rattle_no(gray_banded_kingsnake).
medium(gray_banded_kingsnake).

% Yellow-bellied Kingsnake(non-venomous)
beige(yellow_bellied_kingsnake).
brown(yellow_bellied_kingsnake).
black(yellow_bellied_kingsnake).
large_spotted(yellow_bellied_kingsnake).
big_head(yellow_bellied_kingsnake).
rattle_no(yellow_bellied_kingsnake).
medium(yellow_bellied_kingsnake).

% Common kingsnake (non-venomous)
white(common_kingsnake).
black(common_kingsnake).
not_brown(common_kingsnake).
vertical_stripes(common_kingsnake).
small_head(common_kingsnake).
rattle_no(common_kingsnake).
large(common_kingsnake).

% Milksnake (non-venomous)
red(milksnake).
black(milksnake).
beige(milksnake).
not_brown(milksnake).
large_spotted(milksnake).
small_head(milksnake).
rattle_no(milksnake).
medium(milksnake).

% Northern Cat-eyed Snake(mildly venomous)
orange(northern_cat_eyed_snake).
brown(northern_cat_eyed_snake).
white(northern_cat_eyed_snake).
not_black(northern_cat_eyed_snake).
large_spotted(northern_cat_eyed_snake).
big_head(northern_cat_eyed_snake).
rattle_no(northern_cat_eyed_snake).
small(northern_cat_eyed_snake).
venomous(northern_cat_eyed_snake).

% Plains Slender Blind Snake(non-venomous)
red(plains_slender_blind_snake).
grey(plains_slender_blind_snake).
not_black(plains_slender_blind_snake).
not_brown(plains_slender_blind_snake).
no_pattern(plains_slender_blind_snake).
small_head(plains_slender_blind_snake).
rattle_no(plains_slender_blind_snake).
small(plains_slender_blind_snake).

%Western Slender Blind Snake(poisonous)
brown(western_slender_blind_snake).
not_black(western_slender_blind_snake).
small(western_slender_blind_snake).
small_head(western_slender_blind_snake).
rattle_no(western_slender_blind_snake).
no_pattern(western_slender_blind_snake).
venomous(western_slender_blind_snake).

%Smooth Green Snake(non-venomous)
not_black(smooth_green_snake).
green(smooth_green_snake).
small(smooth_green_snake).
no_pattern(smooth_green_snake).
small_head(smooth_green_snake).
rattle_no(smooth_green_snake).

%Coachwhip(non-venomous)
not_brown(coachwhip).
black(coachwhip).
white(coachwhip).
red(coachwhip).
gradient(coachwhip).
small_head(coachwhip).
large(coachwhip).
rattle_no(coachwhip).

%Schott's Whipsnake(non-venomous)
black(schott_whipsnake).
yellow(schott_whipsnake).
not_brown(schott_whipsnake).
horizontal_striped(schott_whipsnake).
medium(schott_whipsnake).
small_head(schott_whipsnake).
rattle_no(schott_whipsnake).

%North American Coral(extremely venomous)
black(north_american_coral_snake).
yellow(north_american_coral_snake).
not_brown(north_american_coral_snake).
red(north_american_coral_snake).
vertical_striped(north_american_coral_snake).
big_head(north_american_coral_snake).
rattle_no(north_american_coral_snake).
medium(north_american_coral_snake).
venomous(north_american_coral_snake).

%saltmarsh snake (non-venomous).
orange(saltmarsh_snake).
not_black(saltmarsh_snake).
not_brown(saltmarsh_snake).
horizontal_striped(saltmarsh_snake).
small_head(saltmarsh_snake).
rattle_no(saltmarsh_snake).
small(saltmarsh_snake).

%Mississippi Green Water Snake (non-venomous).
black(mississippi_green_water_snake).
beige(mississippi_green_water_snake).
not_brown(mississippi_green_water_snake).
small_spotted(mississippi_green_water_snake).
small_head(mississippi_green_water_snake).
rattle_no(mississippi_green_water_snake).
medium(mississippi_green_water_snake).

%Plain-bellied Water Snake (no-venomous).
black(plain_bellied_water_snake).
not_brown(plain_bellied_water_snake).
no_pattern(plain_bellied_water_snake).
small_head(plain_bellied_water_snake).
rattle_no(plain_bellied_water_snake).
medium(plain_bellied_water_snake).

%Southern Water Snake(non-venomous)
black(southern_water_snake).
not_brown(southern_water_snake).
beige(southern_water_snake).
vertical_striped(southern_water_snake).
small_head(southern_water_snake).
rattle_no(southern_water_snake).
medium(southern_water_snake).

%Brazos River Water Snake(non-venomous)
brown(brazos_river_water_snake).
beige(brazos_river_water_snake).
not_black(brazos_river_water_snake).
large_spotted(brazos_river_water_snake).
small_head(brazos_river_water_snake).
small(brazos_river_water_snake).
rattle_no(brazos_river_water_snake).

%Concho Water Snake (non_venomous).
brown(concho_water_snake).
beige(concho_water_snake).
not_black(concho_water_snake).
large_spotted(concho_water_snake).
small_head(concho_water_snake).
rattle_no(concho_water_snake).
small(concho_water_snake).

%Diamondback Water Snake (non_venomous).
brown(diamondback_water_snake).
beige(diamondback_water_snake).
not_black(diamondback_water_snake).
vertical_striped(diamondback_water_snake).
small_head(diamondback_water_snake).
medium(diamondback_water_snake).
rattle_no(diamondback_water_snake).

%Rough Green Snake (non-venomous).
green(rough_green_snake).
not_black(rough_green_snake).
not_brown(rough_green_snake).
small_head(rough_green_snake).
no_pattern(rough_green_snake).
medium(rough_green_snake).
rattle_no(rough_green_snake).

%Gopher Snake(non-venomous).
brown(gopher_snake).
beige(gopher_snake).
not_black(gopher_snake).
small_spotted(gopher_snake).
small_head(gopher_snake).
rattle_no(gopher_snake).
large(gopher_snake).

%Graham Crayfish Snake (non Venomous)
brown(graham_crayfish_snake).
yellow(graham_crayfish_snake).
not_black(graham_crayfish_snake).
horizontal_striped(graham_crayfish_snake).
small_head(graham_crayfish_snake).
rattle_no(graham_crayfish_snake).
medium(graham_crayfish_snake).

%Glossy Crayfish Snake(no-venomous).
orange(glossy_crayfish_snake).
brown(glossy_crayfish_snake).
black(glossy_crayfish_snake).
horizontal_striped(glossy_crayfish_snake).
small_head(glossy_crayfish_snake).
rattle_no(glossy_crayfish_snake).
small(glossy_crayfish_snake).

%Long-nosed Snake (no_venomous).
red(long_nosed_snake).
black(long_nosed_snake).
not_brown(long_nosed_snake).
vertical_striped(long_nosed_snake).
small_head(long_nosed_snake).
rattle_no(long_nosed_snake).
medium(long_nosed_snake).

%Big Bend Patch-nosed Snake (no_venomous).
white(big_bend_patch_nose_snake).
brown(big_bend_patch_nose_snake).
not_black(big_bend_patch_nose_snake).
horizontal_striped(big_bend_patch_nose_snake).
small_head(big_bend_patch_nose_snake).
rattle_no(big_bend_patch_nose_snake).
medium(big_bend_patch_nose_snake).

%Mountain Patch-nosed Snake (no-venomous).
orange(mountain_patch_nosed_snake).
black(mountain_patch_nosed_snake).
not_brown(mountain_patch_nosed_snake).
horizontal_striped(mountain_patch_nosed_snake).
small_head(mountain_patch_nosed_snake).
rattle_no(mountain_patch_nosed_snake).
medium(mountain_patch_nosed_snake).

%Massasauga (extremely venomous).
brown(massasauga).
beige(massasauga).
not_black(massasauga).
large_spotted(massasauga).
rattle_yes(massasauga).
big_head(massasauga).
venomous(massasauga).

%Pygmy Rattlesnake(extremely venomous).
white(pygmy_rattlesnake).
black(pygmy_rattlesnake).
not_brown(pygmy_rattlesnake).
large_spotted(pygmy_rattlesnake).
big_head(pygmy_rattlesnake).
rattle_yes(pygmy_rattlesnake).
small(pygmy_rattlesnake).
venomous(pygmy_rattlesnake).

%Ground Snake (no-venomous).
grey(ground_snake).
orange(ground_snake).
not_black(ground_snake).
not_brown(ground_snake).
horizontal_striped(ground_snake).
small_head(ground_snake).
rattle_no(ground_snake).
small(ground_snake).

%Dekay's Brown Snake (no-venomous).
brown(dekay_brown_snake).
black(dekay_brown_snake).
horizontal_striped(dekay_brown_snake).
small_head(dekay_brown_snake).
rattle_no(dekay_brown_snake).
small(dekay_brown_snake).

%Red-Bellied Snake
red(red_bellied_snake).
grey(red_bellied_snake).
black(red_bellied_snake).
not_brown(red_bellied_snake).
horizontal_striped(red_bellied_snake).
small_head(red_bellied_snake).
rattle_no(red_bellied_snake).
small(red_bellied_snake).

%Mexican Black headed Snake (no-venomous).
black(mexican_black_headed_snake).
beige(mexican_black_headed_snake).
not_brown(mexican_black_headed_snake).
horizontal_striped(mexican_black_headed_snake).
small_head(mexican_black_headed_snake).
rattle_no(mexican_black_headed_snake).
small(mexican_black_headed_snake).

%Flat_headed_Snake (midly_venomous)
beige(flat_headed_snake).
no_black(flat_headed_snake).
no_brown(flat_headed_snake).
no_pattern(flat_headed_snake).
small_head(flat_headed_snake).
rattle_no(flat_headed_snake).
small(flat_headed_snake).
venomous(flat_headed_snake).

%Southwestern Black-headed Snake (midly_venomous)
black(southwestern_black_headed_snake).
not_brown(southwestern_black_headed_snake).
beige(southwestern_black_headed_snake).
small_head(southwestern_black_headed_snake).
no_pattern(southwestern_black_headed_snake).
rattle_no(souther_water_snake).
small(southwestern_black_headed_snake).
venomous(southwestern_black_headed_snake).

%Plain Black-headed Snake (midly_venomous)
black(plain_black_headed_snake).
not_brown(plain_bellied_water_snake).
beige(plain_black_headed_snake).
small_head(plain_bellied_water_snake).
no_pattern(plain_black_headed_snake).
rattle_no(plain_black_headed_snake).
small(plain_bellied_water_snake).
venomous(plain_bellied_water_snake).

%Black-necked Garter Snake (no_venomous)
black(black_necked_garter_snake).
not_brown(black_necked_garter_snake).
white(black_necked_garter_snake).
small_head(black_necked_garter_snake).
horizontal_striped(black_necked_garter_snake).
rattle_no(black_necked_garter_snake).
medium(black_necked_garter_snake).

%Checkered Garter Snake (no_venomous)
black(checkered_garter_snake).
not_brown(checkered_garter_snake).
grey(checkered_garter_snake).
small_spotted(checkered_garter_snake).
small_head(checkered_garter_snake).
rattle_no(checkered_garter_snake).
medium(checkered_garter_snake).

%Western Ribbon Snake (no-venomous).
black(western_ribbon_snake).
not_brown(western_ribbon_snake).
yellow(western_ribbon_snake).
white(western_ribbon_snake).
horizontal_striped(western_ribbon_snake).
small_head(western_ribbon_snake).
rattle_no(western_ribbon_snake).
medium(western_ribbon_snake).

%Plain Garter Snake (no_venomous).
black(plain_garter_snake).
not_brown(plain_garter_snake).
yellow(plain_garter_snake).
horizontal_striped(plain_garter_snake).
small_head(plain_garter_snake).
rattle_no(plain_garter_snake).
medium(plain_garter_snake).

%Common Garter Snake (midly_venomous).
blue(common_garter_snake).
red(common_garter_snake).
not_black(common_garter_snake).
not_brown(common_garter_snake).
horizontal_striped(common_garter_snake).
small_head(common_garter_snake).
rattle_no(common_garter_snake).
medium(common_garter_snake).
venomous(common_garter_snake).

%Western Lyre Snake
grey(western_lyre_snake).
brown(western_lyre_snake).
not_black(western_lyre_snake).
big_head(western_lyre_snake).
vertical_striped(western_lyre_snake).
medium(western_lyre_snake).
venomous(western_lyre_snake).
rattle_no(western_lyre_snake).

%Lined Snake (no-venomous)
grey(lined_snake).
black(lined_snake).
not_brown(lined_snake).
white(lined_snake).
horizontal_striped(lined_snake).
small_head(lined_snake).
small(lined_snake).
rattle_no(lined_snake).

%Rough Earth Snake (no-venomous).
brown(rough_earth_snake).
not_black(rough_earth_snake).
small_head(rough_earth_snake).
no_pattern(rough_earth_snake).
rattle_no(rough_earth_snake).
small(rough_earth_snake).

%Smooth Earth Snake (no-venomous).
brown(smooth_earth_snake).
not_black(smooth_earth_snake).
small_head(smooth_earth_snake).
no_pattern(smooth_earth_snake).
rattle_no(smooth_earth_snake).
small(smooth_earth_snake).
