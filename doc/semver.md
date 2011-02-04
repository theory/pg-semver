semver 0.2.0
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

The type can be in indexed using btree or hash indexes:

    CREATE INDEX idx_extension_version ON extensions(version);
    CREATE INDEX hdx_extension_version ON extensions USING hash (version);

Hash indexes aren't worth much, but the functionality is there to support hash
aggregates in query optimizations.

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
Use `ORDER BY` to get more of a feel for semantic version ordering rules:

    SELECT version FROM extensions ORDER BY version;
     version  
    ──────────
     0.1.0
     0.35.0b1
     0.35.0
     1.5.0

    SELECT version FROM extensions ORDER BY version DESC;
     version  
    ──────────
     1.5.0
     0.35.0
     0.35.0b1
     0.1.0

Interface
---------

### Operators ###

 Operator |             Description                   | Example                            | Result
----------|-------------------------------------------|------------------------------------|--------
 `=`      | Are semvers equivalent                    | '1.2.0'semver = '1.2.00'::semver   | `t`
 `<>`     | Are semvers different                     | '1.2.0'semver <> '1.2.00'::semver  | `f`
 `<`      | Is semver less than right semver          | '3.4.0b1'semver < '3.4.0'::semver  | `t`
 `<=`     | Is semver less than or equal to semver    | '3.4.0b1'semver <= '3.4.0'::semver | `t`
 `>`      | Is semver greater than right semver       | '3.4.0b1'semver > '3.4.0'::semver  | `f`
 `>=`     | Is semver greater than or equal to semver | '3.4.0b1'semver >= '3.4.0'::semver | `f`

### Functions ###

         Function           |          Description            |          Example                | Result
----------------------------|---------------------------------|---------------------------------|----------
 `to_semver(text)`          | Parse semver from text          | `to_semver('1.02')`             | `1.2.0`
 `semver(text)`             | Cast text to semver             | `semver('1.2.1')`               | `1.2.1`
 `semver(numeric)`          | Cast numeric to semver          | `semver(1.2)`                   | `1.2.0`
 `semver(real)`             | Cast real to semver             | `semver(12.0::real)`            | `12.0.0`
 `semver(double precision)` | Cast double precision to semver | `semver(9.2::double precision)` | `9.2.0`
 `semver(integer)`          | Cast integer to semver          | `semver(42::integer)`           | `42.0.0`
 `semver(bigint)`           | Cast bigint to semver           | `semver(19::bigint)`            | `19.0.0`
 `semver(smallint)`         | Cast smallint to semver         | `semver(2::smallint)`           | `2.0.0`
 `text(semver)`             | Cast semver to text             | `text('1.2.54'::semver)`        | `1.2.54`

The difference between `semver(text)` and `to_semver(text)` is that the former
requires a valid semver format, while the latter is a bit more permissive,
doing its best to convert other version number formats to semantic versions.
Numeric casts simply extract an integer from the decimal portion, so that
`1.20` and `1.02` would both be parsed as `1.2.0`.

### Aggregate Functions ###

The examples assume the values inserted into the `extensions` table in the above examples.

   Function    | Return Type |        Description        |               Example                  | Result
---------------|-------------|---------------------------|----------------------------------------|--------
 `MIN(semver)` |  `semver`   | Return the lowest semver  | `SELECT MIN(version) FROM extensions;` | `0.1.0`
 `MAX(semver)` |  `semver`   | Return the highest semver | `SELECT MAX(version) FROM extensions;` | `1.5.0`

### Casts ###

      From        |  To    |          Example                | Result
---------------------------|---------------------------------|---------
 text             | semver | `'1.2.1'::semver`               | `1.2.1`
 numeric          | semver | `1.2::semver`                   | `1.2.0`
 real             | semver | `12.0::real::semver`            | `12.0.0`
 double precision | semver | `9.2::double precision::semver` | `9.2.0`
 integer          | semver | `42::integer::semver`           | `42.0.0`
 bigint           | semver | `19::bigint::semver`            | `19.0.0`
 smallint         | semver | `2::smallint::semver`           | `2.0.0`
 semver           | text   | `'1.2.54'::semver::text`        | `1.2.54`

Note that numeric casts simply extract an integer from the decimal portion, so that
`1.20` and `1.02` would both be parsed as `1.2.0`.

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
