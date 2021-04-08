# Beam Crud

## Concepts

There are two main concepts in this package, a _Set_ and a _View_. A _Set_ is a
a collection of database records, for example,

- All books
- Books about Haskell
- No books at all

Basically a _Set_ is just a database table together with some filters, i.e. the
`FROM books WHERE title LIKE '%Haskell%'` part. Your access control rules specify
nothing more than just which users have access to which sets. A _Set_ is a
database table with access control rules enforced.

A _View_ is a what the _user_ specifies they want to see from the _Set_. Basically
just what they specify in the request parameters. This includes pagination,
extra filtering (other than the filters you already applied for access control) and
ordering.

This library is about the _View_ part. We will convert the query parameters to
functions which, given an _Set_ will reorder them, apply some extra filtering,
do some pagination and finally return a combined query. Now you only have to
write the _Set_ part, where the business logic is.

In particular, if there are no access control rules, this means that you get a
_fully functioning_ CRUD API by just specifying the database table.
