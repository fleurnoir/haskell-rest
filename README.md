# Demo REST API in Haaskell

The application is REST API representing CRUD operations of a rudimetary "Book store". The Book object has the following format:
```JSON
{
    "id": "number",
    "year": "number",
    "author": "string",
    "title": "string"
}
```

## Used stack: 
- Web framework: happstack-server
- SQL storage: persistent

## Endpoints
- `GET /book` (roles: "User") - returns a list of all books in the store.
- `GET /book/{id}` (roles: "User") - returns the book with the specified id.
- `POST /book` (roles: "Admin") - creates a new book. Payload should contain a JSON Book object.
- `PUT /book` (roles: "Admin") - updates an existing book. Payload should contain a JSON Book object with existing Book id.
- `DELETE /book/{id}` (roles: "Admin") - deletes the Book with the specified id.

## Authorization
The service implements [Basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication). Users are currently hardcoded in [User.hs](https://github.com/fleurnoir/haskell-rest/blob/main/src/BookStore/Repository/User.hs)

## Build and run
- `stack build`
- `stack exec haskell-rest-exe`

## Run functional tests
There is a [Postman test collection](https://github.com/fleurnoir/haskell-rest/blob/main/Books.postman_collection.json) that can be loaded to Posman and run against local application.