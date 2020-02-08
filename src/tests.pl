:- use_module(clearance_base).
:- use_module(clearance_api).
:- use_module(test_framework).

test_clearance_base :-
     "Clearance hierarchies should be able to be checked" should_evaluate (
          higher(official,unclassified),
          higher_or_equal(unclassified, unclassified),
          higher_or_equal(topsecret, official)
     ),
     "Users should be insertable with a given clearance" should_evaluate (
          insert_user_with_clearance(userTopSecret, topsecret, _),
          insert_user_with_clearance(userSecret, secret, _),
          insert_user_with_clearance(userUnclassified, unclassified, _)

     ),
     "Documents should be insertable with a given clearance" should_evaluate insert_document_with_clearance(docSecret, secret, _) to docSecret,
     "Users should have access rights to documents of their level or lower" should_evaluate (
          has_document_rights(userSecret,secret),
          has_document_rights(userSecret,official),
          \+ has_document_rights(userSecret,topsecret)
     ),
     "Non topsecret Users should have modification rights to users of lower levels" should_evaluate (
          has_user_rights(userSecret,official),
          \+ has_user_rights(userSecret,secret)
     ),
     "Topsecret Users should have modification rights to users of their level or lower" should_evaluate (
          has_user_rights(userTopSecret,secret),
          has_user_rights(userTopSecret,topsecret)
     ),
     "Special permission logic should work" should_evaluate (
          insert_document_with_clearance(docTopsecret, topsecret, _),
          insert_special_permission(userSecret, docTopsecret),
          insert_special_permission(unclassified, docTopsecret),
          specialPermission(userSecret, docTopsecret)
     ),
     "Users should be removable & get relations removed throughoutly" should_evaluate (
          remove_user_and_relations(userSecret),
          \+ (userClearance(userSecret, secret); specialPermission(userSecret, docTopsecret))
     ),
     "Documents should be removable & get relations removed throughoutly" should_evaluate (
          remove_document_and_relations(docTopsecret),
          \+ (documentClearance(docTopsecret, topsecret), specialPermission(_, docTopsecret))
     ).

test_clearance_api :- 
     "Create user" should_evaluate insert_user_with_clearance(director, topsecret, Director) to director,
     % USER
     "Director user should be able to create another user on lower level" should_evaluate create_user_as_user(userOfficial, official, Director, OfficialUser) to userOfficial,
     "Director user should be able to create another user on his level" should_evaluate create_user_as_user(coDirector, topsecret, Director, CoDirector) to coDirector,

     "Lower than topsecret user should be able to create a user on a lower level" should_evaluate create_user_as_user(userUnclassified, unclassified, OfficialUser, UnclassifiedUser) to userUnclassified,
     "Lower than topsecret user should not be able to create a user on his or a higher level" should_not_evaluate (
          % Maybe mit mapList alle
          create_user_as_user(_, restricted, OfficialUser, _);
          create_user_as_user(_, topsecret, OfficialUser, _)
     ),
     "User should be able to update clearance of another user of a lower level" should_evaluate (
          create_user_as_user(promotedUser, official, Director, PromotedUser),
          update_user_clearance_as_user(OfficialUser, confidential, Director)
     ),
     "Director should be able to update clearance of a user to his level" should_evaluate (
          create_user_as_user(promotedUser, official, Director, PromotedUser),
          update_user_clearance_as_user(PromotedUser, topsecret, Director)
     ),
     "User should not be able to update clearance of another to his or higher a level" should_not_evaluate (
          create_user_as_user(promotedUser, unclassified, OfficialUser, PromotedUser),
          \+ (update_user_clearance_as_user(PromotedUser, official, OfficialUser); update_user_clearance_as_user(PromotedUser, secret, OfficialUser))
     ),
     "Director should be able to remove a user on his level" should_evaluate (
          remove_user_as_user(CoDirector, Director)
     ),
     "User should only be able to remove a user on a lower level" should_evaluate (
          remove_user_as_user(UnclassifiedUser, OfficialUser),
          \+ (remove_user_as_user(Director, OfficialUser))
     ),
     % DOCUMENTS
     "User should be able to create Documents on his or lower level" should_evaluate (
          create_document_as_user(docOfficial, official, OfficialUser, DocumentOfficial),
          create_document_as_user(docRestricted, restricted, OfficialUser, DocumentRestricted),
          create_document_as_user(docTopsecret, topsecret, Director, DocumentTopsecret),
          \+ (create_document_as_user(docFail, secret, OfficialUser, _))
     ),
     "User should be able to read documents up to his level" should_evaluate (
          get_document(DocumentRestricted, OfficialUser, _),
          get_document(DocumentOfficial, OfficialUser, _)
     ),
     "User should be able to update clearance of Documents up to his level" should_evaluate (
          update_document_clearance_as_user(DocumentRestricted, official, OfficialUser),
          \+ (update_document_clearance_as_user(DocumentRestricted, secret, OfficialUser))
     ),
     "User should be able to read & remove Documents up to his level" should_evaluate (
          remove_document_as_user(DocumentRestricted, OfficialUser),
          remove_document_as_user(DocumentOfficial, OfficialUser),
          \+ (remove_document_as_user(DocumentTopsecret, OfficialUser))
     ),
     % Special Permissions
     "Special permissions should be grantable and work" should_evaluate (
          grant_special_permission_as_user(OfficialUser, DocumentTopsecret, Director),
          get_document(DocumentTopsecret, OfficialUser, _),
          \+ (update_document_clearance_as_user(DocumentTopsecret, unclassified, OfficialUser); remove_document_as_user(DocumentTopsecret, OfficialUser))
     ).

   % "Lower than topsecret user should not be able to create a user on his level" should_not_evaluate create_user_as_user(_, restricted, RestrictedUser, _),
   % "Lower than topsecret user should not be able to create a user on his level" should_not_evaluate create_user_as_user(_, restricted, RestrictedUser, _),

:- style_check(-singleton).
test_flex :- 
     "1 should equal to 2" should_evaluate (1==2),
     "Term variables of create_user_as_user(User, Clearance, AccessUser, R) should match [User, Clearance, AccessUser, R]" should_evaluate term_variables(create_user_as_user(User, Clearance, AccessUser, R),_) to [User, Clearance, AccessUser, R],
     "Term variables of create_user_as_user(User, Clearance, AccessUser, R) should match [User, Clearance, AccessUser, R] even if named differently in call" should_evaluate term_variables(create_user_as_user(U, C, A, _),_) to [User, Clearance, AccessUser, R],
     "Term variables of term_variables(Term, L) should match [Term, L]" should_evaluate term_variables(term_variables(_,_),_) to [Term, L].

test_test_framework :-
     "should_equal should work for atoms, numerics, strings and terms" should_evaluate (
          should_equal(1, 1),
          \+ should_equal(1, 2),
          should_equal(test, test),
          should_equal("test", "test"),
          should_equal((1==1), (1==1))
     ),
     "should_equal should work for lists in any order" should_evaluate (
          should_equal([1,2,3], [3,2,1]),
          should_equal([1,2,3], [1,2,3]),
          \+ should_equal([1,1,2,3], [1,2,3])
     ),
     "last_element_of_list should return the last element of the list" should_evaluate last_element_of_list([1,2,3], _) to 3.

