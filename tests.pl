:- use_module(clearance_base).
:- use_module(clearance_api).
:- use_module(test_framework).

test_all :- 
   "Create user" should_evaluate insert_user_with_clearance(director, topsecret, Director) to director,

   "Director user should be able to create another user on lower level" should_evaluate create_user_as_user(user, official, Director, OfficialUser) to user,
   "Director user should be able to create another user on his level" should_evaluate create_user_as_user(coDirector, topsecret, Director, _) to coDirector,

   "1 should equal to 2" should_evaluate (1==2),

   "Lower than topsecret user should be able to create a user on a lower level" should_evaluate create_user_as_user(user, unclassified, OfficialUser, _) to user,
   "Lower than topsecret user should not be able to create a user on his or a higher level" should_not_evaluate (
        % Maybe mit mapList alle
        create_user_as_user(_, restricted, OfficialUser, _);
        create_user_as_user(_, topsecret, OfficialUser, _)
   ),
   "User should be able to update clearance of another user of a lower level" should_evaluate (
       % create_user_as_user(promotedUser, official, Director, PromotedUser),
        update_user_clearance_as_user(OfficialUser, secret, Director)
   ),
   "Director should be able to update clearance of a user to his level" should_evaluate (
        create_user_as_user(promotedUser, official, Director, PromotedUser),
        update_user_clearance_as_user(PromotedUser, topsecret, Director)
   ),
   "User should not be able to update clearance of another to his or higher a level" should_not_evaluate (
        create_user_as_user(promotedUser, unclassified, OfficialUser, PromotedUser),
        % Maybe mit mapList
        \+ (update_user_clearance_as_user(PromotedUser, official, OfficialUser); update_user_clearance_as_user(PromotedUser, secret, OfficialUser))
   ).
   % "Lower than topsecret user should not be able to create a user on his level" should_not_evaluate create_user_as_user(_, restricted, RestrictedUser, _),
   % "Lower than topsecret user should not be able to create a user on his level" should_not_evaluate create_user_as_user(_, restricted, RestrictedUser, _),

