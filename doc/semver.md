semver 0.1.0
============

Synopsis
--------

    % SELECT '1.2.1'::semver;
     semver    
    --------
     1.2.1

     % SELECT '1.2.0'::semver > '1.2.0b1'::semver;
      ?column?
     ----------
      t
    
Description
-----------
This library contains a single PostgreSQL extension, a samantic version data
type called `semver`.

Usage
-----

TBD.

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
