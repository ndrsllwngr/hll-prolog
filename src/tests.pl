:- use_module(clearance_base).
:- use_module(clearance_api).
:- use_module(test_framework).
:- use_module(database_util).

test_clearance_base :-
     "Clearance hierarchies should be able to be checked" should_evaluate (
          get_all_higher_clearances(unclassified, R),
          R should_equal [official, restricted, confidential, secret, topsecret],
          maplist(higher, R, [unclassified, unclassified, unclassified, unclassified, unclassified]),
          maplist(next_higher_level, R, [unclassified, official, restricted, confidential, secret])
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
          insert_user_with_clearance(userSP1, official, _),
          insert_user_with_clearance(userSP2, official, _),

          insert_document_with_clearance(docSP1, topsecret, _),
          insert_document_with_clearance(docSP2, topsecret, _),

          insert_special_permission(userSP1, docSP1),
          insert_special_permission(userSP1, docSP2),
          insert_special_permission(userSP2, docSP1),

          specialPermission(docSP1, userSP1),
          specialPermission(docSP2, userSP1),
          specialPermission(docSP1, userSP2)
     ),
     "Users should be removable & get relations removed throughoutly" should_evaluate (
          remove_user_and_relations(userSP1),
          \+ ( userClearance(userSP1, secret);
               specialPermission(docSP1, userSP1);
               specialPermission(docSP2, userSP1)),
          specialPermission(docSP1, userSP2)
     ),
     "Documents should be removable & get relations removed throughoutly" should_evaluate (
          insert_user_with_clearance(userSP1, official, _),
          insert_special_permission(userSP1, docSP1),
          remove_document_and_relations(docSP1),
          \+ ( documentClearance(docSP1, topsecret);
               specialPermission(docSP1, userSP1);
               specialPermission(docSP1, userSP2))
     ),
     % Clean Database
     clean_database.

test_clearance_api :- 
     "Create user Director" should_evaluate insert_user_with_clearance(director, topsecret, Director) to director,
     % User
     "Director user should be able to create another user on lower level" should_evaluate create_user_as_user(userRestricted, restricted, Director, RestrictedUser) to userRestricted,
     "Director user should be able to create another user on his level" should_evaluate create_user_as_user(coDirector, topsecret, Director, CoDirector) to coDirector,

     "Lower than topsecret user should be able to create a user on a lower level" should_evaluate create_user_as_user(userUnclassified, unclassified, RestrictedUser, UnclassifiedUser) to userUnclassified,
     "Lower than topsecret user should not be able to create a user on his or a higher level" should_not_evaluate (
          create_user_as_user(_, restricted, RestrictedUser, _);
          create_user_as_user(_, topsecret, RestrictedUser, _)
     ),
     "Director should be able to update clearance of a user to his level" should_evaluate (
          create_user_as_user(userPromoted1, official, Director, PromotedUser1),
          update_user_clearance_as_user(PromotedUser1, topsecret, Director)
     ),
     "Director should be able to remove a user on his level" should_evaluate (
          remove_user_as_user(CoDirector, Director),
          remove_user_as_user(PromotedUser1, Director)
     ),
     "User should be able to update clearance of another user to lower levels than his" should_evaluate (
          create_user_as_user(userPromoted2, unclassified, RestrictedUser, PromotedUser2),
          update_user_clearance_as_user(PromotedUser2, official, RestrictedUser),
          \+ ( update_user_clearance_as_user(PromotedUser2, secret, RestrictedUser);
               update_user_clearance_as_user(RestrictedUser, secret, RestrictedUser);
               update_user_clearance_as_user(Director, unclassified, RestrictedUser))
     ),
     "User should only be able to remove a user on a lower level" should_evaluate (
          remove_user_as_user(UnclassifiedUser, RestrictedUser),
          \+ remove_user_as_user(Director, RestrictedUser)
     ),
     % Documents
     "User should be able to create Documents on his or lower level" should_evaluate (
          create_document_as_user(docOfficial, official, RestrictedUser, DocumentOfficial),
          create_document_as_user(docRestricted, restricted, RestrictedUser, DocumentRestricted),
          create_document_as_user(docTopsecret, topsecret, Director, DocumentTopsecret),
          \+ create_document_as_user(docFail, secret, RestrictedUser, _)
     ),
     "User should be able to read documents up to his level" should_evaluate (
          get_document(DocumentRestricted, RestrictedUser, _),
          get_document(DocumentOfficial, RestrictedUser, _)
     ),
     "User should be able to update clearance of Documents up to his level" should_evaluate (
          update_document_clearance_as_user(DocumentOfficial, restricted, RestrictedUser),
          \+ update_document_clearance_as_user(DocumentRestricted, secret, RestrictedUser)
     ),
     "User should be able to remove Documents up to his level" should_evaluate (
          remove_document_as_user(DocumentRestricted, RestrictedUser),
          remove_document_as_user(DocumentOfficial, RestrictedUser),
          \+ remove_document_as_user(DocumentTopsecret, RestrictedUser)
     ),
     % Special Permissions
     "Special permissions should be grantable and work" should_evaluate (
          grant_special_permission_as_user(RestrictedUser, DocumentTopsecret, Director),
          get_document(DocumentTopsecret, RestrictedUser, _),
          \+ (update_document_clearance_as_user(DocumentTopsecret, unclassified, RestrictedUser); remove_document_as_user(DocumentTopsecret, RestrictedUser)),
          retract_special_permission_as_user(RestrictedUser, DocumentTopsecret, Director),
          \+ get_document(DocumentTopsecret, RestrictedUser, _)
     ).
     % Clean Database
     %clean_database.

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

:- style_check(-singleton).
test_examples :- 
     "1 should equal to 2" should_evaluate (1==2),
     "Term variables of create_user_as_user(User, Clearance, AccessUser, R) should match [User, Clearance, AccessUser, R]" should_evaluate term_variables(create_user_as_user(User, Clearance, AccessUser, R),_) to [User, Clearance, AccessUser, R],
     "Term variables of create_user_as_user(User, Clearance, AccessUser, R) should match [User, Clearance, AccessUser, R] even if named differently in call" should_evaluate term_variables(create_user_as_user(U, C, A, _),_) to [User, Clearance, AccessUser, R],
     "Term variables of term_variables(Term, L) should match [Term, L]" should_evaluate term_variables(term_variables(_,_),_) to [Term, L].

test_all :-
     write("\ntest_clearance_base:\n---\n"),
     test_clearance_base,
     write("\n\ntest_clearance_api:\n---\n"),
     test_clearance_api,
     write("\n\ntest_test_framework:\n---\n"),
     test_test_framework,
     write("\n\ntest_examples\n---\n"),
     test_examples.
