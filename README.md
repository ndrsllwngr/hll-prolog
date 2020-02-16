# hll-prolog
High level languages: Prolog - Group project (2019/2020)

## Description

**Topic:** Testing Interaction Scenarios - Classified Information System (CIS)

- Different clearance levels, which get assigned to documents and users (`topsecret, secret, confidential, restricted, official, unclassified`)
- Based on their clearance level users can perform different actions and access documents

## Requirements

```bash
$ swipl --version
SWI-Prolog version 8.0.3 for x86_64-darwin
```

## Usage

### Start REST API
```bash
$ swipl -s src/main.pl
```

### Run unit/integration tests
```bash
$ swipl -s src/tests.pl
?- test_all.
```

### Usable functions `clearance_api.pl`

- Retrieve `Document` as `AccessUser`, returns document only if `AccessUser` has access rights
    ```prolog
    get_document(+Document, +AccessUser, -R)
    ``` 
- Retrieve all documents accessible by `AccessUser`
    ```prolog
    get_documents_accesible_by_user(+AccessUser, -R)
    ```
- Retrieve all Users managable by `AccessUser`
    ```prolog
    get_users_managable_by_user(+AccessUser, -R)
    ```
- Create a `User` with a given `Clearance` as `AccessUser` and return user if successfull.
    ```prolog
    create_user_as_user(+User, +Clearance, +AccessUser, -R)
    ``` 
- Create a `Document` with a given `Clearance` as `AccessUser` and return document if successfull
    ```prolog
    create_document_as_user(+Document, +Clearance, +AccessUser, -R)
    ```
- Remove a `User` as `AccessUser`
    ```prolog
    remove_user_as_user(+User, +AccessUser)
    ``` 
- Remove `Document` as `AccessUser`
    ```prolog
    remove_document_as_user(+Document, +AccessUser)
    ``` 
- Set `NewClearance` as `User`'s clearance as `AccessUser`
    ```prolog
    update_user_clearance_as_user(+User, +NewClearance, +AccessUser)
    ``` 
- Set `NewClearance` as `Document`s clearance as `AccessUser`
    ```prolog
    update_document_clearance_as_user(+Document, +NewClearance, +AccessUser)
    ``` 
- Grant `User` a special permission to view `Document` as `AccessUser`
    ```prolog
    grant_special_permission_as_user(+User, +Document, +AccessUser)
    ``` 
- Retract special permission of `User` to `Document` as `AccessUser`
    ```prolog
    retract_special_permission_as_user(+User, +Document, +AccessUser)
    ``` 

### REST API

#### Requests examples

Start Postman and load `rest_api.postman_collection.json`.

#### GET requests

```jsonld
GET http://localhost:5004/health
```
```jsonld
GET http://localhost:5004/document?document=nsa_files&access_user=director
```
```jsonld
GET http://localhost:5004/document/get_accessible?access_user=director
```
```jsonld
GET http://localhost:5004/user/get_managable?access_user=director
```

#### POST requests
Use `Content-Type: application/json`

```jsonld
POST http://localhost:5004/user/create_as_user
{
    "user" : "snowden",
    "clearance" : "restricted",
    "access_user" : "director"
}
```

```jsonld
POST http://localhost:5004/user/update_clearance_as_user
{
    "user" : "snowden",
    "clearance" : "secret",
    "access_user" : "director"
}
```

```jsonld
POST http://localhost:5004/user/remove_as_user
{
    "user" : "snowden",
    "access_user" : "director"
}
```

```jsonld
POST http://localhost:5004/document/create_as_user
{
    "document" : "nsa_files",
    "clearance" : "secret",
    "access_user" : "snowden"
}
```

```jsonld
POST http://localhost:5004/document/update_clearance_as_user
{
    "document" : "nsa_files",
    "clearance" : "secret",
    "access_user" : "snowden"
}
```

```jsonld
POST http://localhost:5004/document/remove_as_user
{
    "document" : "nsa_files",
    "access_user" : "snowden"
}
```

```jsonld
POST http://localhost:5004/document/grant_special_permission_as_user
{ 
    "user" : "press",
    "document" : "nsa_files",
    "access_user" : "snowden"
}
```

```jsonld
POST http://localhost:5004/document/retract_special_permission_as_user
{
    "user" : "press", 
    "document" : "nsa_files",
    "access_user" : "snowden"
}
```

## Test framework `test_framework.pl`

We wrote a test framework in order to test the correctness of our system. It provides operators to test prolog terms using the following syntax:

```prolog
"1 should equal to 1" should_evaluate (1==1).
```
```prolog
"1 should not equal to 2" should_not_evaluate (1==2).
```
```prolog
"Term variables of term_variables(Term, L) should match [Term, L]" should_evaluate term_variables(term_variables(_,_),_) to [Term, L].
```
```prolog
"should_equal should work for atoms, numerics, strings and terms" should_evaluate (
    should_equal(1, 1),
    \+ should_equal(1, 2),
    should_equal(test, test),
    should_equal("test", "test"),
    should_equal((1==1), (1==1))
).
  ```
```prolog   
"should_equal should work for lists in any order" should_evaluate (
    should_equal([1,2,3], [3,2,1]),
    should_equal([1,2,3], [1,2,3]),
    \+ should_equal([1,1,2,3], [1,2,3])
).

```

## Libraries

- http/thread_httpd
- http/http_dispatch
- http/http_error
- http/json_convert
- http/http_client
- http/html_write
- http/http_parameters
- http/http_json
- lists

## Contributors (Group ATA)

- Andreas Ellwanger
- Timo Erdelt
- Andreas Griesbeck

### Individual contributions

Due too the small group size of 3 it is impossible for us to properly distinguish what of our project has been done by whom. We all worked on all parts of our application, especially since we mostly did “pair-programming” (with two or often all three of us working together). So all of us were equally involved in all parts of our application.
We would be happy to answer questions about our development process, aswell as our individual/colletive contributions at the examination.
––