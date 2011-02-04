semver 0.1.0
============

Synopsis
--------

    SELECT '1.2.1'::semver;
     semver    
    ────────
     1.2.1

    SELECT '1.2.0'::semver > '1.2.0b1'::semver;
     ?column?
    ──────────
     t
    
Description
-----------

This library contains a single PostgreSQL extension, a samantic version data
type called `semver`. It's an implementation of the version number format
specified by the [Samantic Versioning Specification](http://semver.org/).
Currently it's implemented as a data domain, but may be converted to a native
type implemented in C in the future. Either way, functionality should remain
the same.

The two important points describing the structure of a semantic version and
how two versions compare are these, from [the spec](http://semver.org/):

> 1. A normal version number MUST take the form X.Y.Z where X, Y, and
> Z are integers. X is the major version, Y is the minor version, and
> Z is the patch version. Each element MUST increase numerically. For
> instance: 1.9.0 < 1.10.0 < 1.11.0.
>
> 2. A special version number MAY be denoted by appending an arbitrary
> string immediately following the patch version. The string MUST
> be comprised of only alphanumerics plus dash [0-9A-Za-z-] and
> MUST begin with an alpha character [A-Za-z]. Special versions
> satisfy but have a lower precedence than the associated normal
> version. Precedence SHOULD be determined by lexicographic ASCII sort
> order. For instance: 1.0.0beta1 < 1.0.0beta2 < 1.0.0.

Usage
-----

Use like any other data type. Here's an example in a table:

    CREATE TABLE extensions (
        name        TEXT,
        version     SEMVER,
        description TEXT,
        PRIMARY KEY (name, version)
    );

The type can be in indexed using btree:

    CREATE INDEX idx_extension_version ON extensions(version);

And some sample usage:

    INSERT INTO extensions
    VALUES ('pgtap', '0.35.0',   'PostgreSQL unit testing'),
           ('pgtap', '0.35.0b1', 'PostgreSQL unit testing.'),
           ('pair',  '0.1.0',    'Key/value pair data type'),
           ('PostGIS', '1.5.0',  'Gelocation data types');

    SELECT * FROM extensions WHERE VERSION = '1.5.0';
      name   │ version │      description      
    ─────────┼─────────┼───────────────────────
     PostGIS │ 1.5.0   │ Gelocation data types

    SELECT * FROM extensions WHERE VERSION < '0.35.0';
     name  │ version  │       description        
    ───────┼──────────┼──────────────────────────
     pgtap │ 0.35.0b1 │ PostgreSQL unit testing.
     pair  │ 0.1.0    │ Key/value pair data type
    
Note that "0.35.0b1" is less than "0.35.0", as required by the specification.

Currently, `semver` is implemented as a data domain. As such, use of `semver`
in an `ORDER BY` expression only works using the `<` operator for ascending
order, and `>` for descending order:

    SELECT version FROM extensions ORDER BY version USING <;
     version  
    ──────────
     0.1.0
     0.35.0b1
     0.35.0
     1.5.0

    SELECT version FROM extensions ORDER BY version USING >;
     version  
    ──────────
     1.5.0
     0.35.0
     0.35.0b1
     0.1.0

Operators and Functions
-----------------------

### Operators ###

 Operator |             Description                   | Example                            | Result
----------|-------------------------------------------|------------------------------------|--------
 `=`      | Are semvers equivalent                    | '1.2.0'semver = '1.2.00'::semver   | `t`
 `<>`     | Are semvers different                     | '1.2.0'semver <> '1.2.00'::semver  | `f`
 `<`      | Is semver less than right semver          | '3.4.0b1'semver < '3.4.0'::semver  | `t`
 `<=`     | Is semver less than or equal to semver    | '3.4.0b1'semver <= '3.4.0'::semver | `t`
 `>`      | Is semver greater than right semver       | '3.4.0b1'semver > '3.4.0'::semver  | `f`
 `>=`     | Is semver greater than or equal to semver | '3.4.0b1'semver >= '3.4.0'::semver | `f`
     
### Aggregate Functions ###

The examples assume the values inserted into the `extensions` table in the above examples.

   Function    | Return Type |        Description        |               Example                  | Result
---------------|-------------|---------------------------|----------------------------------------|--------
 `MIN(semver)` |  `semver`   | Return the lowest semver  | `SELECT MIN(version) FROM extensions;` | `0.1.0`
 `MAX(semver)` |  `semver`   | Return the highest semver | `SELECT MAX(version) FROM extensions;` | `1.5.0`

Support
-------

This library is stored in an open [GitHub
repository](http://github.com/theory/pg-semver). Feel free to fork and
contribute! Please file bug reports via [GitHub
Issues](http://github.com/theory/pg-semver/issues/).

Author
------

[David E. Wheeler](http://www.justatheory.com/)

Copyright and License
---------------------

Copyright (c) 2010-2011 David E. Wheeler.

This module is free software; you can redistribute it and/or modify it under
the [PostgreSQL License](http://www.opensource.org/licenses/postgresql).

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose, without fee, and without a written agreement is
hereby granted, provided that the above copyright notice and this paragraph
and the following two paragraphs appear in all copies.

In no event shall David E. Wheeler be liable to any party for direct,
indirect, special, incidental, or consequential damages, including lost
profits, arising out of the use of this software and its documentation, even
if David E. Wheeler has been advised of the possibility of such damage.

David E. Wheeler specifically disclaims any warranties, including, but not
limited to, the implied warranties of merchantability and fitness for a
particular purpose. The software provided hereunder is on an "as is" basis,
and David E. Wheeler has no obligations to provide maintenance, support,
updates, enhancements, or modifications.
