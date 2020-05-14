\set ECHO none
-- This file defines pgTAP Core, a portable collection of assertion
-- functions for TAP-based unit testing on PostgreSQL 8.3 or higher. It is
-- distributed under the revised FreeBSD license. The home page for the pgTAP
-- project is:

--
-- https://pgtap.org/
--

\pset format unaligned
\pset tuples_only true
\pset pager

-- Revert all changes on failure.
\set ON_ERROR_ROLLBACK 1
\set ON_ERROR_STOP true

CREATE OR REPLACE FUNCTION pg_version()
RETURNS text AS 'SELECT current_setting(''server_version'')'
LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION pg_version_num()
RETURNS integer AS $$
    SELECT s.a[1]::int * 10000
           + COALESCE(substring(s.a[2] FROM '[[:digit:]]+')::int, 0) * 100
           + COALESCE(substring(s.a[3] FROM '[[:digit:]]+')::int, 0)
      FROM (
          SELECT string_to_array(current_setting('server_version'), '.') AS a
      ) AS s;
$$ LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION pgtap_version()
RETURNS NUMERIC AS 'SELECT 0.25;'
LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION plan( integer )
RETURNS TEXT AS $$
DECLARE
    rcount INTEGER;
BEGIN
    BEGIN
        EXECUTE '
            CREATE TEMP SEQUENCE __tcache___id_seq;
            CREATE TEMP TABLE __tcache__ (
                id    INTEGER NOT NULL DEFAULT nextval(''__tcache___id_seq''),
                label TEXT    NOT NULL,
                value INTEGER NOT NULL,
                note  TEXT    NOT NULL DEFAULT ''''
            );
            CREATE UNIQUE INDEX __tcache___key ON __tcache__(id);
            GRANT ALL ON TABLE __tcache__ TO PUBLIC;
            GRANT ALL ON TABLE __tcache___id_seq TO PUBLIC;

            CREATE TEMP SEQUENCE __tresults___numb_seq;
            CREATE TEMP TABLE __tresults__ (
                numb   INTEGER NOT NULL DEFAULT nextval(''__tresults___numb_seq''),
                ok     BOOLEAN NOT NULL DEFAULT TRUE,
                aok    BOOLEAN NOT NULL DEFAULT TRUE,
                descr  TEXT    NOT NULL DEFAULT '''',
                type   TEXT    NOT NULL DEFAULT '''',
                reason TEXT    NOT NULL DEFAULT ''''
            );
            CREATE UNIQUE INDEX __tresults___key ON __tresults__(numb);
            GRANT ALL ON TABLE __tresults__ TO PUBLIC;
            GRANT ALL ON TABLE __tresults___numb_seq TO PUBLIC;
        ';

    EXCEPTION WHEN duplicate_table THEN
        -- Raise an exception if there's already a plan.
        EXECUTE 'SELECT TRUE FROM __tcache__ WHERE label = ''plan''';
      GET DIAGNOSTICS rcount = ROW_COUNT;
        IF rcount > 0 THEN
           RAISE EXCEPTION 'You tried to plan twice!';
        END IF;
    END;

    -- Save the plan and return.
    PERFORM _set('plan', $1 );
    RETURN '1..' || $1;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION no_plan()
RETURNS SETOF boolean AS $$
BEGIN
    PERFORM plan(0);
    RETURN;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _get ( text )
RETURNS integer AS $$
DECLARE
    ret integer;
BEGIN
    EXECUTE 'SELECT value FROM __tcache__ WHERE label = ' || quote_literal($1) || ' LIMIT 1' INTO ret;
    RETURN ret;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _get_latest ( text )
RETURNS integer[] AS $$
DECLARE
    ret integer[];
BEGIN
    EXECUTE 'SELECT ARRAY[ id, value] FROM __tcache__ WHERE label = ' ||
    quote_literal($1) || ' AND id = (SELECT MAX(id) FROM __tcache__ WHERE label = ' ||
    quote_literal($1) || ') LIMIT 1' INTO ret;
    RETURN ret;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _get_latest ( text, integer )
RETURNS integer AS $$
DECLARE
    ret integer;
BEGIN
    EXECUTE 'SELECT MAX(id) FROM __tcache__ WHERE label = ' ||
    quote_literal($1) || ' AND value = ' || $2 INTO ret;
    RETURN ret;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _get_note ( text )
RETURNS text AS $$
DECLARE
    ret text;
BEGIN
    EXECUTE 'SELECT note FROM __tcache__ WHERE label = ' || quote_literal($1) || ' LIMIT 1' INTO ret;
    RETURN ret;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _get_note ( integer )
RETURNS text AS $$
DECLARE
    ret text;
BEGIN
    EXECUTE 'SELECT note FROM __tcache__ WHERE id = ' || $1 || ' LIMIT 1' INTO ret;
    RETURN ret;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _set ( text, integer, text )
RETURNS integer AS $$
DECLARE
    rcount integer;
BEGIN
    EXECUTE 'UPDATE __tcache__ SET value = ' || $2
        || CASE WHEN $3 IS NULL THEN '' ELSE ', note = ' || quote_literal($3) END
        || ' WHERE label = ' || quote_literal($1);
    GET DIAGNOSTICS rcount = ROW_COUNT;
    IF rcount = 0 THEN
       RETURN _add( $1, $2, $3 );
    END IF;
    RETURN $2;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _set ( text, integer )
RETURNS integer AS $$
    SELECT _set($1, $2, '')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _set ( integer, integer )
RETURNS integer AS $$
BEGIN
    EXECUTE 'UPDATE __tcache__ SET value = ' || $2
        || ' WHERE id = ' || $1;
    RETURN $2;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _add ( text, integer, text )
RETURNS integer AS $$
BEGIN
    EXECUTE 'INSERT INTO __tcache__ (label, value, note) values (' ||
    quote_literal($1) || ', ' || $2 || ', ' || quote_literal(COALESCE($3, '')) || ')';
    RETURN $2;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _add ( text, integer )
RETURNS integer AS $$
    SELECT _add($1, $2, '')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION add_result ( bool, bool, text, text, text )
RETURNS integer AS $$
BEGIN
    EXECUTE 'INSERT INTO __tresults__ ( ok, aok, descr, type, reason )
    VALUES( ' || $1 || ', '
              || $2 || ', '
              || quote_literal(COALESCE($3, '')) || ', '
              || quote_literal($4) || ', '
              || quote_literal($5) || ' )';
    RETURN currval('__tresults___numb_seq');
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION num_failed ()
RETURNS INTEGER AS $$
DECLARE
    ret integer;
BEGIN
    EXECUTE 'SELECT COUNT(*)::INTEGER FROM __tresults__ WHERE ok = FALSE' INTO ret;
    RETURN ret;
END;
$$ LANGUAGE plpgsql strict;

CREATE OR REPLACE FUNCTION _finish ( INTEGER, INTEGER, INTEGER)
RETURNS SETOF TEXT AS $$
DECLARE
    curr_test ALIAS FOR $1;
    exp_tests INTEGER := $2;
    num_faild ALIAS FOR $3;
    plural    CHAR;
BEGIN
    plural    := CASE exp_tests WHEN 1 THEN '' ELSE 's' END;

    IF curr_test IS NULL THEN
        RAISE EXCEPTION '# No tests run!';
    END IF;

    IF exp_tests = 0 OR exp_tests IS NULL THEN
         -- No plan. Output one now.
        exp_tests = curr_test;
        RETURN NEXT '1..' || exp_tests;
    END IF;

    IF curr_test <> exp_tests THEN
        RETURN NEXT diag(
            'Looks like you planned ' || exp_tests || ' test' ||
            plural || ' but ran ' || curr_test
        );
    ELSIF num_faild > 0 THEN
        RETURN NEXT diag(
            'Looks like you failed ' || num_faild || ' test' ||
            CASE num_faild WHEN 1 THEN '' ELSE 's' END
            || ' of ' || exp_tests
        );
    ELSE

    END IF;
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION finish ()
RETURNS SETOF TEXT AS $$
    SELECT * FROM _finish(
        _get('curr_test'),
        _get('plan'),
        num_failed()
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION diag ( msg text )
RETURNS TEXT AS $$
    SELECT '# ' || replace(
       replace(
            replace( $1, E'\r\n', E'\n# ' ),
            E'\n',
            E'\n# '
        ),
        E'\r',
        E'\n# '
    );
$$ LANGUAGE sql strict;

CREATE OR REPLACE FUNCTION diag ( msg anyelement )
RETURNS TEXT AS $$
    SELECT diag($1::text);
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION diag( VARIADIC text[] )
RETURNS TEXT AS $$
    SELECT diag(array_to_string($1, ''));
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION diag( VARIADIC anyarray )
RETURNS TEXT AS $$
    SELECT diag(array_to_string($1, ''));
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION ok ( boolean, text )
RETURNS TEXT AS $$
DECLARE
   aok      ALIAS FOR $1;
   descr    text := $2;
   test_num INTEGER;
   todo_why TEXT;
   ok       BOOL;
BEGIN
   todo_why := _todo();
   ok       := CASE
       WHEN aok = TRUE THEN aok
       WHEN todo_why IS NULL THEN COALESCE(aok, false)
       ELSE TRUE
    END;
    IF _get('plan') IS NULL THEN
        RAISE EXCEPTION 'You tried to run a test without a plan! Gotta have a plan';
    END IF;

    test_num := add_result(
        ok,
        COALESCE(aok, false),
        descr,
        CASE WHEN todo_why IS NULL THEN '' ELSE 'todo' END,
        COALESCE(todo_why, '')
    );

    RETURN (CASE aok WHEN TRUE THEN '' ELSE 'not ' END)
           || 'ok ' || _set( 'curr_test', test_num )
           || CASE descr WHEN '' THEN '' ELSE COALESCE( ' - ' || substr(diag( descr ), 3), '' ) END
           || COALESCE( ' ' || diag( 'TODO ' || todo_why ), '')
           || CASE aok WHEN TRUE THEN '' ELSE E'\n' ||
                diag('Failed ' ||
                CASE WHEN todo_why IS NULL THEN '' ELSE '(TODO) ' END ||
                'test ' || test_num ||
                CASE descr WHEN '' THEN '' ELSE COALESCE(': "' || descr || '"', '') END ) ||
                CASE WHEN aok IS NULL THEN E'\n' || diag('    (test result was NULL)') ELSE '' END
           END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ok ( boolean )
RETURNS TEXT AS $$
    SELECT ok( $1, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION is (anyelement, anyelement, text)
RETURNS TEXT AS $$
DECLARE
    result BOOLEAN;
    output TEXT;
BEGIN
    -- Would prefer $1 IS NOT DISTINCT FROM, but that's not supported by 8.1.
    result := NOT $1 IS DISTINCT FROM $2;
    output := ok( result, $3 );
    RETURN output || CASE result WHEN TRUE THEN '' ELSE E'\n' || diag(
           '        have: ' || CASE WHEN $1 IS NULL THEN 'NULL' ELSE $1::text END ||
        E'\n        want: ' || CASE WHEN $2 IS NULL THEN 'NULL' ELSE $2::text END
    ) END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION is (anyelement, anyelement)
RETURNS TEXT AS $$
    SELECT is( $1, $2, NULL);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION isnt (anyelement, anyelement, text)
RETURNS TEXT AS $$
DECLARE
    result BOOLEAN;
    output TEXT;
BEGIN
    result := $1 IS DISTINCT FROM $2;
    output := ok( result, $3 );
    RETURN output || CASE result WHEN TRUE THEN '' ELSE E'\n' || diag(
           '        have: ' || COALESCE( $1::text, 'NULL' ) ||
        E'\n        want: anything else'
    ) END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION isnt (anyelement, anyelement)
RETURNS TEXT AS $$
    SELECT isnt( $1, $2, NULL);
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _alike ( BOOLEAN, ANYELEMENT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    result ALIAS FOR $1;
    got    ALIAS FOR $2;
    rx     ALIAS FOR $3;
    descr  ALIAS FOR $4;
    output TEXT;
BEGIN
    output := ok( result, descr );
    RETURN output || CASE result WHEN TRUE THEN '' ELSE E'\n' || diag(
           '                  ' || COALESCE( quote_literal(got), 'NULL' ) ||
       E'\n   doesn''t match: ' || COALESCE( quote_literal(rx), 'NULL' )
    ) END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION matches ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~ $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION matches ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~ $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION imatches ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~* $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION imatches ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~* $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION alike ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~~ $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION alike ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~~ $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION ialike ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~~* $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION ialike ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _alike( $1 ~~* $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _unalike ( BOOLEAN, ANYELEMENT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    result ALIAS FOR $1;
    got    ALIAS FOR $2;
    rx     ALIAS FOR $3;
    descr  ALIAS FOR $4;
    output TEXT;
BEGIN
    output := ok( result, descr );
    RETURN output || CASE result WHEN TRUE THEN '' ELSE E'\n' || diag(
           '                  ' || COALESCE( quote_literal(got), 'NULL' ) ||
        E'\n         matches: ' || COALESCE( quote_literal(rx), 'NULL' )
    ) END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION doesnt_match ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~ $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION doesnt_match ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~ $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION doesnt_imatch ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~* $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION doesnt_imatch ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~* $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION unalike ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~~ $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION unalike ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~~ $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION unialike ( anyelement, text, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~~* $2, $1, $2, $3 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION unialike ( anyelement, text )
RETURNS TEXT AS $$
    SELECT _unalike( $1 !~~* $2, $1, $2, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION cmp_ok (anyelement, text, anyelement, text)
RETURNS TEXT AS $$
DECLARE
    have   ALIAS FOR $1;
    op     ALIAS FOR $2;
    want   ALIAS FOR $3;
    descr  ALIAS FOR $4;
    result BOOLEAN;
    output TEXT;
BEGIN
    EXECUTE 'SELECT ' ||
            COALESCE(quote_literal( have ), 'NULL') || '::' || pg_typeof(have) || ' '
            || op || ' ' ||
            COALESCE(quote_literal( want ), 'NULL') || '::' || pg_typeof(want)
       INTO result;
    output := ok( COALESCE(result, FALSE), descr );
    RETURN output || CASE result WHEN TRUE THEN '' ELSE E'\n' || diag(
           '    ' || COALESCE( quote_literal(have), 'NULL' ) ||
           E'\n        ' || op ||
           E'\n    ' || COALESCE( quote_literal(want), 'NULL' )
    ) END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION cmp_ok (anyelement, text, anyelement)
RETURNS TEXT AS $$
    SELECT cmp_ok( $1, $2, $3, NULL );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION pass ( text )
RETURNS TEXT AS $$
    SELECT ok( TRUE, $1 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION pass ()
RETURNS TEXT AS $$
    SELECT ok( TRUE, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION fail ( text )
RETURNS TEXT AS $$
    SELECT ok( FALSE, $1 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION fail ()
RETURNS TEXT AS $$
    SELECT ok( FALSE, NULL );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION todo ( why text, how_many int )
RETURNS SETOF BOOLEAN AS $$
BEGIN
    PERFORM _add('todo', COALESCE(how_many, 1), COALESCE(why, ''));
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION todo ( how_many int, why text )
RETURNS SETOF BOOLEAN AS $$
BEGIN
    PERFORM _add('todo', COALESCE(how_many, 1), COALESCE(why, ''));
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION todo ( why text )
RETURNS SETOF BOOLEAN AS $$
BEGIN
    PERFORM _add('todo', 1, COALESCE(why, ''));
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION todo ( how_many int )
RETURNS SETOF BOOLEAN AS $$
BEGIN
    PERFORM _add('todo', COALESCE(how_many, 1), '');
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION todo_start (text)
RETURNS SETOF BOOLEAN AS $$
BEGIN
    PERFORM _add('todo', -1, COALESCE($1, ''));
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION todo_start ()
RETURNS SETOF BOOLEAN AS $$
BEGIN
    PERFORM _add('todo', -1, '');
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION in_todo ()
RETURNS BOOLEAN AS $$
DECLARE
    todos integer;
BEGIN
    todos := _get('todo');
    RETURN CASE WHEN todos IS NULL THEN FALSE ELSE TRUE END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION todo_end ()
RETURNS SETOF BOOLEAN AS $$
DECLARE
    id integer;
BEGIN
    id := _get_latest( 'todo', -1 );
    IF id IS NULL THEN
        RAISE EXCEPTION 'todo_end() called without todo_start()';
    END IF;
    EXECUTE 'DELETE FROM __tcache__ WHERE id = ' || id;
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _todo()
RETURNS TEXT AS $$
DECLARE
    todos INT[];
    note text;
BEGIN
    -- Get the latest id and value, because todo() might have been called
    -- again before the todos ran out for the first call to todo(). This
    -- allows them to nest.
    todos := _get_latest('todo');
    IF todos IS NULL THEN
        -- No todos.
        RETURN NULL;
    END IF;
    IF todos[2] = 0 THEN
        -- Todos depleted. Clean up.
        EXECUTE 'DELETE FROM __tcache__ WHERE id = ' || todos[1];
        RETURN NULL;
    END IF;
    -- Decrement the count of counted todos and return the reason.
    IF todos[2] <> -1 THEN
        PERFORM _set(todos[1], todos[2] - 1);
    END IF;
    note := _get_note(todos[1]);

    IF todos[2] = 1 THEN
        -- This was the last todo, so delete the record.
        EXECUTE 'DELETE FROM __tcache__ WHERE id = ' || todos[1];
    END IF;

    RETURN note;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION skip ( why text, how_many int )
RETURNS TEXT AS $$
DECLARE
    output TEXT[];
BEGIN
    output := '{}';
    FOR i IN 1..how_many LOOP
        output = array_append(output, ok( TRUE, 'SKIP: ' || COALESCE( why, '') ) );
    END LOOP;
    RETURN array_to_string(output, E'\n');
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION skip ( text )
RETURNS TEXT AS $$
    SELECT ok( TRUE, 'SKIP: ' || $1 );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION skip( int, text )
RETURNS TEXT AS 'SELECT skip($2, $1)'
LANGUAGE sql;

CREATE OR REPLACE FUNCTION skip( int )
RETURNS TEXT AS 'SELECT skip(NULL, $1)'
LANGUAGE sql;

CREATE OR REPLACE FUNCTION _query( TEXT )
RETURNS TEXT AS $$
    SELECT CASE
        WHEN $1 LIKE '"%' OR $1 !~ '[[:space:]]' THEN 'EXECUTE ' || $1
        ELSE $1
    END;
$$ LANGUAGE SQL;

-- throws_ok ( sql, errcode, errmsg, description )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT, CHAR(5), TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    query     TEXT := _query($1);
    errcode   ALIAS FOR $2;
    errmsg    ALIAS FOR $3;
    desctext  ALIAS FOR $4;
    descr     TEXT;
BEGIN
    descr := COALESCE(
          desctext,
          'threw ' || errcode || ': ' || errmsg,
          'threw ' || errcode,
          'threw ' || errmsg,
          'threw an exception'
    );
    EXECUTE query;
    RETURN ok( FALSE, descr ) || E'\n' || diag(
           '      caught: no exception' ||
        E'\n      wanted: ' || COALESCE( errcode, 'an exception' )
    );
EXCEPTION WHEN OTHERS THEN
    IF (errcode IS NULL OR SQLSTATE = errcode)
        AND ( errmsg IS NULL OR SQLERRM = errmsg)
    THEN
        -- The expected errcode and/or message was thrown.
        RETURN ok( TRUE, descr );
    ELSE
        -- This was not the expected errcode or errmsg.
        RETURN ok( FALSE, descr ) || E'\n' || diag(
               '      caught: ' || SQLSTATE || ': ' || SQLERRM ||
            E'\n      wanted: ' || COALESCE( errcode, 'an exception' ) ||
            COALESCE( ': ' || errmsg, '')
        );
    END IF;
END;
$$ LANGUAGE plpgsql;

-- throws_ok ( sql, errcode, errmsg )
-- throws_ok ( sql, errmsg, description )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    IF octet_length($2) = 5 THEN
        RETURN throws_ok( $1, $2::char(5), $3, NULL );
    ELSE
        RETURN throws_ok( $1, NULL, $2, $3 );
    END IF;
END;
$$ LANGUAGE plpgsql;

-- throws_ok ( query, errcode )
-- throws_ok ( query, errmsg )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    IF octet_length($2) = 5 THEN
        RETURN throws_ok( $1, $2::char(5), NULL, NULL );
    ELSE
        RETURN throws_ok( $1, NULL, $2, NULL );
    END IF;
END;
$$ LANGUAGE plpgsql;

-- throws_ok ( sql )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT )
RETURNS TEXT AS $$
    SELECT throws_ok( $1, NULL, NULL, NULL );
$$ LANGUAGE SQL;

-- Magically cast integer error codes.
-- throws_ok ( sql, errcode, errmsg, description )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT, int4, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT throws_ok( $1, $2::char(5), $3, $4 );
$$ LANGUAGE SQL;

-- throws_ok ( sql, errcode, errmsg )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT, int4, TEXT )
RETURNS TEXT AS $$
    SELECT throws_ok( $1, $2::char(5), $3, NULL );
$$ LANGUAGE SQL;

-- throws_ok ( sql, errcode )
CREATE OR REPLACE FUNCTION throws_ok ( TEXT, int4 )
RETURNS TEXT AS $$
    SELECT throws_ok( $1, $2::char(5), NULL, NULL );
$$ LANGUAGE SQL;

-- lives_ok( sql, description )
CREATE OR REPLACE FUNCTION lives_ok ( TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    code  TEXT := _query($1);
    descr ALIAS FOR $2;
BEGIN
    EXECUTE code;
    RETURN ok( TRUE, descr );
EXCEPTION WHEN OTHERS THEN
    -- There should have been no exception.
    RETURN ok( FALSE, descr ) || E'\n' || diag(
           '        died: ' || SQLSTATE || ': ' || SQLERRM
    );
END;
$$ LANGUAGE plpgsql;

-- lives_ok( sql )
CREATE OR REPLACE FUNCTION lives_ok ( TEXT )
RETURNS TEXT AS $$
    SELECT lives_ok( $1, NULL );
$$ LANGUAGE SQL;

-- performs_ok ( sql, milliseconds, description )
CREATE OR REPLACE FUNCTION performs_ok ( TEXT, NUMERIC, TEXT )
RETURNS TEXT AS $$
DECLARE
    query     TEXT := _query($1);
    max_time  ALIAS FOR $2;
    descr     ALIAS FOR $3;
    starts_at TEXT;
    act_time  NUMERIC;
BEGIN
    starts_at := timeofday();
    EXECUTE query;
    act_time := extract( millisecond from timeofday()::timestamptz - starts_at::timestamptz);
    IF act_time < max_time THEN RETURN ok(TRUE, descr); END IF;
    RETURN ok( FALSE, descr ) || E'\n' || diag(
           '      runtime: ' || act_time || ' ms' ||
        E'\n      exceeds: ' || max_time || ' ms'
    );
END;
$$ LANGUAGE plpgsql;

-- performs_ok ( sql, milliseconds )
CREATE OR REPLACE FUNCTION performs_ok ( TEXT, NUMERIC )
RETURNS TEXT AS $$
    SELECT performs_ok(
        $1, $2, 'Should run in less than ' || $2 || ' ms'
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION display_type ( OID, INTEGER )
RETURNS TEXT AS $$
    SELECT COALESCE(substring(
        pg_catalog.format_type($1, $2),
        '(("(?!")([^"]|"")+"|[^.]+)([(][^)]+[)])?)$'
    ), '')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION display_type ( NAME, OID, INTEGER )
RETURNS TEXT AS $$
    SELECT CASE WHEN $1 IS NULL THEN '' ELSE quote_ident($1) || '.' END
        || display_type($2, $3)
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _ident_array_to_string( name[], text )
RETURNS text AS $$
    SELECT array_to_string(ARRAY(
        SELECT quote_ident($1[i])
          FROM generate_series(1, array_upper($1, 1)) s(i)
         ORDER BY i
    ), $2);
$$ LANGUAGE SQL immutable;

-- Borrowed from newsysviews.
CREATE OR REPLACE VIEW tap_funky
 AS SELECT p.oid         AS oid,
           n.nspname     AS schema,
           p.proname     AS name,
           array_to_string(p.proargtypes::regtype[], ',') AS args,
           CASE p.proretset WHEN TRUE THEN 'setof ' ELSE '' END
             || p.prorettype::regtype AS returns,
           p.prolang     AS langoid,
           p.proisstrict AS is_strict,
--         p.proisagg    AS is_agg,
           p.prosecdef   AS is_definer,
           p.proretset   AS returns_set,
           p.provolatile::char AS volatility,
           pg_catalog.pg_function_is_visible(p.oid) AS is_visible
      FROM pg_catalog.pg_proc p
      JOIN pg_catalog.pg_namespace n ON p.pronamespace = n.oid
;

CREATE OR REPLACE FUNCTION _got_func ( NAME, NAME, NAME[] )
RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT TRUE
          FROM tap_funky
         WHERE schema = $1
           AND name   = $2
           AND args   = array_to_string($3, ',')
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _got_func ( NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS( SELECT TRUE FROM tap_funky WHERE schema = $1 AND name = $2 );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _got_func ( NAME, NAME[] )
RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT TRUE
          FROM tap_funky
         WHERE name = $1
           AND args = array_to_string($2, ',')
           AND is_visible
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _got_func ( NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS( SELECT TRUE FROM tap_funky WHERE name = $1 AND is_visible);
$$ LANGUAGE SQL;

-- has_function( schema, function, args[], description )
CREATE OR REPLACE FUNCTION has_function ( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT ok( _got_func($1, $2, $3), $4 );
$$ LANGUAGE SQL;

-- has_function( schema, function, args[] )
CREATE OR REPLACE FUNCTION has_function( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        _got_func($1, $2, $3),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should exist'
    );
$$ LANGUAGE sql;

-- has_function( schema, function, description )
CREATE OR REPLACE FUNCTION has_function ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _got_func($1, $2), $3 );
$$ LANGUAGE SQL;

-- has_function( schema, function )
CREATE OR REPLACE FUNCTION has_function( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
        _got_func($1, $2),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '() should exist'
    );
$$ LANGUAGE sql;

-- has_function( function, args[], description )
CREATE OR REPLACE FUNCTION has_function ( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT ok( _got_func($1, $2), $3 );
$$ LANGUAGE SQL;

-- has_function( function, args[] )
CREATE OR REPLACE FUNCTION has_function( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        _got_func($1, $2),
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should exist'
    );
$$ LANGUAGE sql;

-- has_function( function, description )
CREATE OR REPLACE FUNCTION has_function( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _got_func($1), $2 );
$$ LANGUAGE sql;

-- has_function( function )
CREATE OR REPLACE FUNCTION has_function( NAME )
RETURNS TEXT AS $$
    SELECT ok( _got_func($1), 'Function ' || quote_ident($1) || '() should exist' );
$$ LANGUAGE sql;

-- hasnt_function( schema, function, args[], description )
CREATE OR REPLACE FUNCTION hasnt_function ( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _got_func($1, $2, $3), $4 );
$$ LANGUAGE SQL;

-- hasnt_function( schema, function, args[] )
CREATE OR REPLACE FUNCTION hasnt_function( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        NOT _got_func($1, $2, $3),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should not exist'
    );
$$ LANGUAGE sql;

-- hasnt_function( schema, function, description )
CREATE OR REPLACE FUNCTION hasnt_function ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _got_func($1, $2), $3 );
$$ LANGUAGE SQL;

-- hasnt_function( schema, function )
CREATE OR REPLACE FUNCTION hasnt_function( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
        NOT _got_func($1, $2),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '() should not exist'
    );
$$ LANGUAGE sql;

-- hasnt_function( function, args[], description )
CREATE OR REPLACE FUNCTION hasnt_function ( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _got_func($1, $2), $3 );
$$ LANGUAGE SQL;

-- hasnt_function( function, args[] )
CREATE OR REPLACE FUNCTION hasnt_function( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        NOT _got_func($1, $2),
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should not exist'
    );
$$ LANGUAGE sql;

-- hasnt_function( function, description )
CREATE OR REPLACE FUNCTION hasnt_function( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _got_func($1), $2 );
$$ LANGUAGE sql;

-- hasnt_function( function )
CREATE OR REPLACE FUNCTION hasnt_function( NAME )
RETURNS TEXT AS $$
    SELECT ok( NOT _got_func($1), 'Function ' || quote_ident($1) || '() should not exist' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _pg_sv_type_array( OID[] )
RETURNS NAME[] AS $$
    SELECT ARRAY(
        SELECT t.typname
          FROM pg_catalog.pg_type t
          JOIN generate_series(1, array_upper($1, 1)) s(i) ON t.oid = $1[i]
         ORDER BY i
    )
$$ LANGUAGE SQL stable;

-- can( schema, functions[], description )
CREATE OR REPLACE FUNCTION can ( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
DECLARE
    missing text[];
BEGIN
    SELECT ARRAY(
        SELECT quote_ident($2[i])
          FROM generate_series(1, array_upper($2, 1)) s(i)
          LEFT JOIN tap_funky ON name = $2[i] AND schema = $1
         WHERE oid IS NULL
         GROUP BY $2[i], s.i
         ORDER BY MIN(s.i)
    ) INTO missing;
    IF missing[1] IS NULL THEN
        RETURN ok( true, $3 );
    END IF;
    RETURN ok( false, $3 ) || E'\n' || diag(
        '    ' || quote_ident($1) || '.' ||
        array_to_string( missing, E'() missing\n    ' || quote_ident($1) || '.') ||
        '() missing'
    );
END;
$$ LANGUAGE plpgsql;

-- can( schema, functions[] )
CREATE OR REPLACE FUNCTION can ( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT can( $1, $2, 'Schema ' || quote_ident($1) || ' can' );
$$ LANGUAGE sql;

-- can( functions[], description )
CREATE OR REPLACE FUNCTION can ( NAME[], TEXT )
RETURNS TEXT AS $$
DECLARE
    missing text[];
BEGIN
    SELECT ARRAY(
        SELECT quote_ident($1[i])
          FROM generate_series(1, array_upper($1, 1)) s(i)
          LEFT JOIN pg_catalog.pg_proc p
            ON $1[i] = p.proname
           AND pg_catalog.pg_function_is_visible(p.oid)
         WHERE p.oid IS NULL
         ORDER BY s.i
    ) INTO missing;
    IF missing[1] IS NULL THEN
        RETURN ok( true, $2 );
    END IF;
    RETURN ok( false, $2 ) || E'\n' || diag(
        '    ' ||
        array_to_string( missing, E'() missing\n    ') ||
        '() missing'
    );
END;
$$ LANGUAGE plpgsql;

-- can( functions[] )
CREATE OR REPLACE FUNCTION can ( NAME[] )
RETURNS TEXT AS $$
    SELECT can( $1, 'Schema ' || _ident_array_to_string(current_schemas(true), ' or ') || ' can' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _has_type( NAME, NAME, CHAR[] )
RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT true
          FROM pg_catalog.pg_type t
          JOIN pg_catalog.pg_namespace n ON t.typnamespace = n.oid
         WHERE t.typisdefined
           AND n.nspname = $1
           AND t.typname = $2
           AND t.typtype = ANY( COALESCE($3, ARRAY['b', 'c', 'd', 'p', 'e']) )
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _has_type( NAME, CHAR[] )
RETURNS BOOLEAN AS $$
    SELECT EXISTS(
        SELECT true
          FROM pg_catalog.pg_type t
         WHERE t.typisdefined
           AND pg_catalog.pg_type_is_visible(t.oid)
           AND t.typname = $1
           AND t.typtype = ANY( COALESCE($2, ARRAY['b', 'c', 'd', 'p', 'e']) )
    );
$$ LANGUAGE sql;

-- has_type( schema, type, description )
CREATE OR REPLACE FUNCTION has_type( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, $2, NULL ), $3 );
$$ LANGUAGE sql;

-- has_type( schema, type )
CREATE OR REPLACE FUNCTION has_type( NAME, NAME )
RETURNS TEXT AS $$
    SELECT has_type( $1, $2, 'Type ' || quote_ident($1) || '.' || quote_ident($2) || ' should exist' );
$$ LANGUAGE sql;

-- has_type( type, description )
CREATE OR REPLACE FUNCTION has_type( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, NULL ), $2 );
$$ LANGUAGE sql;

-- has_type( type )
CREATE OR REPLACE FUNCTION has_type( NAME )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, NULL ), ('Type ' || quote_ident($1) || ' should exist')::text );
$$ LANGUAGE sql;

-- hasnt_type( schema, type, description )
CREATE OR REPLACE FUNCTION hasnt_type( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, $2, NULL ), $3 );
$$ LANGUAGE sql;

-- hasnt_type( schema, type )
CREATE OR REPLACE FUNCTION hasnt_type( NAME, NAME )
RETURNS TEXT AS $$
    SELECT hasnt_type( $1, $2, 'Type ' || quote_ident($1) || '.' || quote_ident($2) || ' should not exist' );
$$ LANGUAGE sql;

-- hasnt_type( type, description )
CREATE OR REPLACE FUNCTION hasnt_type( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, NULL ), $2 );
$$ LANGUAGE sql;

-- hasnt_type( type )
CREATE OR REPLACE FUNCTION hasnt_type( NAME )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, NULL ), ('Type ' || quote_ident($1) || ' should not exist')::text );
$$ LANGUAGE sql;

-- has_domain( schema, domain, description )
CREATE OR REPLACE FUNCTION has_domain( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, $2, ARRAY['d'] ), $3 );
$$ LANGUAGE sql;

-- has_domain( schema, domain )
CREATE OR REPLACE FUNCTION has_domain( NAME, NAME )
RETURNS TEXT AS $$
    SELECT has_domain( $1, $2, 'Domain ' || quote_ident($1) || '.' || quote_ident($2) || ' should exist' );
$$ LANGUAGE sql;

-- has_domain( domain, description )
CREATE OR REPLACE FUNCTION has_domain( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, ARRAY['d'] ), $2 );
$$ LANGUAGE sql;

-- has_domain( domain )
CREATE OR REPLACE FUNCTION has_domain( NAME )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, ARRAY['d'] ), ('Domain ' || quote_ident($1) || ' should exist')::text );
$$ LANGUAGE sql;

-- hasnt_domain( schema, domain, description )
CREATE OR REPLACE FUNCTION hasnt_domain( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, $2, ARRAY['d'] ), $3 );
$$ LANGUAGE sql;

-- hasnt_domain( schema, domain )
CREATE OR REPLACE FUNCTION hasnt_domain( NAME, NAME )
RETURNS TEXT AS $$
    SELECT hasnt_domain( $1, $2, 'Domain ' || quote_ident($1) || '.' || quote_ident($2) || ' should not exist' );
$$ LANGUAGE sql;

-- hasnt_domain( domain, description )
CREATE OR REPLACE FUNCTION hasnt_domain( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, ARRAY['d'] ), $2 );
$$ LANGUAGE sql;

-- hasnt_domain( domain )
CREATE OR REPLACE FUNCTION hasnt_domain( NAME )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, ARRAY['d'] ), ('Domain ' || quote_ident($1) || ' should not exist')::text );
$$ LANGUAGE sql;

-- has_enum( schema, enum, description )
CREATE OR REPLACE FUNCTION has_enum( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, $2, ARRAY['e'] ), $3 );
$$ LANGUAGE sql;

-- has_enum( schema, enum )
CREATE OR REPLACE FUNCTION has_enum( NAME, NAME )
RETURNS TEXT AS $$
    SELECT has_enum( $1, $2, 'Enum ' || quote_ident($1) || '.' || quote_ident($2) || ' should exist' );
$$ LANGUAGE sql;

-- has_enum( enum, description )
CREATE OR REPLACE FUNCTION has_enum( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, ARRAY['e'] ), $2 );
$$ LANGUAGE sql;

-- has_enum( enum )
CREATE OR REPLACE FUNCTION has_enum( NAME )
RETURNS TEXT AS $$
    SELECT ok( _has_type( $1, ARRAY['e'] ), ('Enum ' || quote_ident($1) || ' should exist')::text );
$$ LANGUAGE sql;

-- hasnt_enum( schema, enum, description )
CREATE OR REPLACE FUNCTION hasnt_enum( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, $2, ARRAY['e'] ), $3 );
$$ LANGUAGE sql;

-- hasnt_enum( schema, enum )
CREATE OR REPLACE FUNCTION hasnt_enum( NAME, NAME )
RETURNS TEXT AS $$
    SELECT hasnt_enum( $1, $2, 'Enum ' || quote_ident($1) || '.' || quote_ident($2) || ' should not exist' );
$$ LANGUAGE sql;

-- hasnt_enum( enum, description )
CREATE OR REPLACE FUNCTION hasnt_enum( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, ARRAY['e'] ), $2 );
$$ LANGUAGE sql;

-- hasnt_enum( enum )
CREATE OR REPLACE FUNCTION hasnt_enum( NAME )
RETURNS TEXT AS $$
    SELECT ok( NOT _has_type( $1, ARRAY['e'] ), ('Enum ' || quote_ident($1) || ' should not exist')::text );
$$ LANGUAGE sql;

-- enum_has_labels( schema, enum, labels, description )
CREATE OR REPLACE FUNCTION enum_has_labels( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT is(
        ARRAY(
            SELECT e.enumlabel
              FROM pg_catalog.pg_type t
              JOIN pg_catalog.pg_enum e      ON t.oid = e.enumtypid
              JOIN pg_catalog.pg_namespace n ON t.typnamespace = n.oid
              WHERE t.typisdefined
               AND n.nspname = $1
               AND t.typname = $2
               AND t.typtype = 'e'
             ORDER BY e.oid
        ),
        $3,
        $4
    );
$$ LANGUAGE sql;

-- enum_has_labels( schema, enum, labels )
CREATE OR REPLACE FUNCTION enum_has_labels( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT enum_has_labels(
        $1, $2, $3,
        'Enum ' || quote_ident($1) || '.' || quote_ident($2) || ' should have labels (' || array_to_string( $3, ', ' ) || ')'
    );
$$ LANGUAGE sql;

-- enum_has_labels( enum, labels, description )
CREATE OR REPLACE FUNCTION enum_has_labels( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT is(
        ARRAY(
            SELECT e.enumlabel
              FROM pg_catalog.pg_type t
              JOIN pg_catalog.pg_enum e ON t.oid = e.enumtypid
              WHERE t.typisdefined
               AND pg_catalog.pg_type_is_visible(t.oid)
               AND t.typname = $1
               AND t.typtype = 'e'
             ORDER BY e.oid
        ),
        $2,
        $3
    );
$$ LANGUAGE sql;

-- enum_has_labels( enum, labels )
CREATE OR REPLACE FUNCTION enum_has_labels( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT enum_has_labels(
        $1, $2,
        'Enum ' || quote_ident($1) || ' should have labels (' || array_to_string( $2, ', ' ) || ')'
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _cmp_types(oid, name)
RETURNS BOOLEAN AS $$
DECLARE
    dtype TEXT := display_type($1, NULL);
BEGIN
    RETURN dtype = _quote_ident_like($2, dtype);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _cast_exists ( NAME, NAME, NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
       SELECT TRUE
         FROM pg_catalog.pg_cast c
         JOIN pg_catalog.pg_proc p ON c.castfunc = p.oid
         JOIN pg_catalog.pg_namespace n ON p.pronamespace = n.oid
        WHERE _cmp_types(castsource, $1)
          AND _cmp_types(casttarget, $2)
          AND n.nspname   = $3
          AND p.proname   = $4
   );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _cast_exists ( NAME, NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
       SELECT TRUE
         FROM pg_catalog.pg_cast c
         JOIN pg_catalog.pg_proc p ON c.castfunc = p.oid
        WHERE _cmp_types(castsource, $1)
          AND _cmp_types(casttarget, $2)
          AND p.proname   = $3
   );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _cast_exists ( NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
       SELECT TRUE
         FROM pg_catalog.pg_cast c
        WHERE _cmp_types(castsource, $1)
          AND _cmp_types(casttarget, $2)
   );
$$ LANGUAGE SQL;

-- has_cast( source_type, target_type, schema, function, description )
CREATE OR REPLACE FUNCTION has_cast ( NAME, NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
   SELECT ok( _cast_exists( $1, $2, $3, $4 ), $5 );
$$ LANGUAGE SQL;

-- has_cast( source_type, target_type, schema, function )
CREATE OR REPLACE FUNCTION has_cast ( NAME, NAME, NAME, NAME )
RETURNS TEXT AS $$
   SELECT ok(
       _cast_exists( $1, $2, $3, $4 ),
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') WITH FUNCTION ' || quote_ident($3)
        || '.' || quote_ident($4) || '() should exist'
    );
$$ LANGUAGE SQL;

-- has_cast( source_type, target_type, function, description )
CREATE OR REPLACE FUNCTION has_cast ( NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
   SELECT ok( _cast_exists( $1, $2, $3 ), $4 );
$$ LANGUAGE SQL;

-- has_cast( source_type, target_type, function )
CREATE OR REPLACE FUNCTION has_cast ( NAME, NAME, NAME )
RETURNS TEXT AS $$
   SELECT ok(
        _cast_exists( $1, $2, $3 ),
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') WITH FUNCTION ' || quote_ident($3) || '() should exist'
    );
$$ LANGUAGE SQL;

-- has_cast( source_type, target_type, description )
CREATE OR REPLACE FUNCTION has_cast ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _cast_exists( $1, $2 ), $3 );
$$ LANGUAGE SQL;

-- has_cast( source_type, target_type )
CREATE OR REPLACE FUNCTION has_cast ( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
        _cast_exists( $1, $2 ),
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') should exist'
    );
$$ LANGUAGE SQL;

-- hasnt_cast( source_type, target_type, schema, function, description )
CREATE OR REPLACE FUNCTION hasnt_cast ( NAME, NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
   SELECT ok( NOT _cast_exists( $1, $2, $3, $4 ), $5 );
$$ LANGUAGE SQL;

-- hasnt_cast( source_type, target_type, schema, function )
CREATE OR REPLACE FUNCTION hasnt_cast ( NAME, NAME, NAME, NAME )
RETURNS TEXT AS $$
   SELECT ok(
       NOT _cast_exists( $1, $2, $3, $4 ),
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') WITH FUNCTION ' || quote_ident($3)
        || '.' || quote_ident($4) || '() should not exist'
    );
$$ LANGUAGE SQL;

-- hasnt_cast( source_type, target_type, function, description )
CREATE OR REPLACE FUNCTION hasnt_cast ( NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
   SELECT ok( NOT _cast_exists( $1, $2, $3 ), $4 );
$$ LANGUAGE SQL;

-- hasnt_cast( source_type, target_type, function )
CREATE OR REPLACE FUNCTION hasnt_cast ( NAME, NAME, NAME )
RETURNS TEXT AS $$
   SELECT ok(
        NOT _cast_exists( $1, $2, $3 ),
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') WITH FUNCTION ' || quote_ident($3) || '() should not exist'
    );
$$ LANGUAGE SQL;

-- hasnt_cast( source_type, target_type, description )
CREATE OR REPLACE FUNCTION hasnt_cast ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _cast_exists( $1, $2 ), $3 );
$$ LANGUAGE SQL;

-- hasnt_cast( source_type, target_type )
CREATE OR REPLACE FUNCTION hasnt_cast ( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
        NOT _cast_exists( $1, $2 ),
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') should not exist'
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _expand_context( char )
RETURNS text AS $$
   SELECT CASE $1
          WHEN 'i' THEN 'implicit'
          WHEN 'a' THEN 'assignment'
          WHEN 'e' THEN 'explicit'
          ELSE          'unknown' END
$$ LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION _get_context( NAME, NAME )
RETURNS "char" AS $$
   SELECT c.castcontext
     FROM pg_catalog.pg_cast c
    WHERE _cmp_types(castsource, $1)
      AND _cmp_types(casttarget, $2)
$$ LANGUAGE SQL;

-- cast_context_is( source_type, target_type, context, description )
CREATE OR REPLACE FUNCTION cast_context_is( NAME, NAME, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    want char = substring(LOWER($3) FROM 1 FOR 1);
    have char := _get_context($1, $2);
BEGIN
    IF have IS NOT NULL THEN
        RETURN is( _expand_context(have), _expand_context(want), $4 );
    END IF;

    RETURN ok( false, $4 ) || E'\n' || diag(
       '    Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
      || ') does not exist'
    );
END;
$$ LANGUAGE plpgsql;

-- cast_context_is( source_type, target_type, context )
CREATE OR REPLACE FUNCTION cast_context_is( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT cast_context_is(
        $1, $2, $3,
        'Cast (' || quote_ident($1) || ' AS ' || quote_ident($2)
        || ') context should be ' || _expand_context(substring(LOWER($3) FROM 1 FOR 1))
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _op_exists ( NAME, NAME, NAME, NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
       SELECT TRUE
         FROM pg_catalog.pg_operator o
         JOIN pg_catalog.pg_namespace n ON o.oprnamespace = n.oid
        WHERE n.nspname = $2
          AND o.oprname = $3
          AND CASE o.oprkind WHEN 'l' THEN $1 IS NULL
              ELSE _cmp_types(o.oprleft, $1) END
          AND CASE o.oprkind WHEN 'r' THEN $4 IS NULL
              ELSE _cmp_types(o.oprright, $4) END
          AND _cmp_types(o.oprresult, $5)
   );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _op_exists ( NAME, NAME, NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
       SELECT TRUE
         FROM pg_catalog.pg_operator o
        WHERE pg_catalog.pg_operator_is_visible(o.oid)
          AND o.oprname = $2
          AND CASE o.oprkind WHEN 'l' THEN $1 IS NULL
              ELSE _cmp_types(o.oprleft, $1) END
          AND CASE o.oprkind WHEN 'r' THEN $3 IS NULL
              ELSE _cmp_types(o.oprright, $3) END
          AND _cmp_types(o.oprresult, $4)
   );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _op_exists ( NAME, NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
       SELECT TRUE
         FROM pg_catalog.pg_operator o
        WHERE pg_catalog.pg_operator_is_visible(o.oid)
          AND o.oprname = $2
          AND CASE o.oprkind WHEN 'l' THEN $1 IS NULL
              ELSE _cmp_types(o.oprleft, $1) END
          AND CASE o.oprkind WHEN 'r' THEN $3 IS NULL
              ELSE _cmp_types(o.oprright, $3) END
   );
$$ LANGUAGE SQL;

-- has_operator( left_type, schema, name, right_type, return_type, description )
CREATE OR REPLACE FUNCTION has_operator ( NAME, NAME, NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists($1, $2, $3, $4, $5 ), $6 );
$$ LANGUAGE SQL;

-- has_operator( left_type, schema, name, right_type, return_type )
CREATE OR REPLACE FUNCTION has_operator ( NAME, NAME, NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists($1, $2, $3, $4, $5 ),
        'Operator ' || quote_ident($2) || '.' || $3 || '(' || $1 || ',' || $4
        || ') RETURNS ' || $5 || ' should exist'
    );
$$ LANGUAGE SQL;

-- has_operator( left_type, name, right_type, return_type, description )
CREATE OR REPLACE FUNCTION has_operator ( NAME, NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists($1, $2, $3, $4 ), $5 );
$$ LANGUAGE SQL;

-- has_operator( left_type, name, right_type, return_type )
CREATE OR REPLACE FUNCTION has_operator ( NAME, NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists($1, $2, $3, $4 ),
        'Operator ' ||  $2 || '(' || $1 || ',' || $3
        || ') RETURNS ' || $4 || ' should exist'
    );
$$ LANGUAGE SQL;

-- has_operator( left_type, name, right_type, description )
CREATE OR REPLACE FUNCTION has_operator ( NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists($1, $2, $3 ), $4 );
$$ LANGUAGE SQL;

-- has_operator( left_type, name, right_type )
CREATE OR REPLACE FUNCTION has_operator ( NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists($1, $2, $3 ),
        'Operator ' ||  $2 || '(' || $1 || ',' || $3
        || ') should exist'
    );
$$ LANGUAGE SQL;

-- has_leftop( schema, name, right_type, return_type, description )
CREATE OR REPLACE FUNCTION has_leftop ( NAME, NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists(NULL, $1, $2, $3, $4), $5 );
$$ LANGUAGE SQL;

-- has_leftop( schema, name, right_type, return_type )
CREATE OR REPLACE FUNCTION has_leftop ( NAME, NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists(NULL, $1, $2, $3, $4 ),
        'Left operator ' || quote_ident($1) || '.' || $2 || '(NONE,'
        || $3 || ') RETURNS ' || $4 || ' should exist'
    );
$$ LANGUAGE SQL;

-- has_leftop( name, right_type, return_type, description )
CREATE OR REPLACE FUNCTION has_leftop ( NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists(NULL, $1, $2, $3), $4 );
$$ LANGUAGE SQL;

-- has_leftop( name, right_type, return_type )
CREATE OR REPLACE FUNCTION has_leftop ( NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists(NULL, $1, $2, $3 ),
        'Left operator ' || $1 || '(NONE,' || $2 || ') RETURNS ' || $3 || ' should exist'
    );
$$ LANGUAGE SQL;

-- has_leftop( name, right_type, description )
CREATE OR REPLACE FUNCTION has_leftop ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists(NULL, $1, $2), $3 );
$$ LANGUAGE SQL;

-- has_leftop( name, right_type )
CREATE OR REPLACE FUNCTION has_leftop ( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists(NULL, $1, $2 ),
        'Left operator ' || $1 || '(NONE,' || $2 || ') should exist'
    );
$$ LANGUAGE SQL;

-- has_rightop( left_type, schema, name, return_type, description )
CREATE OR REPLACE FUNCTION has_rightop ( NAME, NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists( $1, $2, $3, NULL, $4), $5 );
$$ LANGUAGE SQL;

-- has_rightop( left_type, schema, name, return_type )
CREATE OR REPLACE FUNCTION has_rightop ( NAME, NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists($1, $2, $3, NULL, $4 ),
        'Right operator ' || quote_ident($2) || '.' || $3 || '('
        || $1 || ',NONE) RETURNS ' || $4 || ' should exist'
    );
$$ LANGUAGE SQL;

-- has_rightop( left_type, name, return_type, description )
CREATE OR REPLACE FUNCTION has_rightop ( NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists( $1, $2, NULL, $3), $4 );
$$ LANGUAGE SQL;

-- has_rightop( left_type, name, return_type )
CREATE OR REPLACE FUNCTION has_rightop ( NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists($1, $2, NULL, $3 ),
        'Right operator ' || $2 || '('
        || $1 || ',NONE) RETURNS ' || $3 || ' should exist'
    );
$$ LANGUAGE SQL;

-- has_rightop( left_type, name, description )
CREATE OR REPLACE FUNCTION has_rightop ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _op_exists( $1, $2, NULL), $3 );
$$ LANGUAGE SQL;

-- has_rightop( left_type, name )
CREATE OR REPLACE FUNCTION has_rightop ( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
         _op_exists($1, $2, NULL ),
        'Right operator ' || $2 || '(' || $1 || ',NONE) should exist'
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _is_trusted( NAME )
RETURNS BOOLEAN AS $$
    SELECT lanpltrusted FROM pg_catalog.pg_language WHERE lanname = $1;
$$ LANGUAGE SQL;

-- has_language( language, description)
CREATE OR REPLACE FUNCTION has_language( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _is_trusted($1) IS NOT NULL, $2 );
$$ LANGUAGE SQL;

-- has_language( language )
CREATE OR REPLACE FUNCTION has_language( NAME )
RETURNS TEXT AS $$
    SELECT ok( _is_trusted($1) IS NOT NULL, 'Procedural language ' || quote_ident($1) || ' should exist' );
$$ LANGUAGE SQL;

-- hasnt_language( language, description)
CREATE OR REPLACE FUNCTION hasnt_language( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _is_trusted($1) IS NULL, $2 );
$$ LANGUAGE SQL;

-- hasnt_language( language )
CREATE OR REPLACE FUNCTION hasnt_language( NAME )
RETURNS TEXT AS $$
    SELECT ok( _is_trusted($1) IS NULL, 'Procedural language ' || quote_ident($1) || ' should not exist' );
$$ LANGUAGE SQL;

-- language_is_trusted( language, description )
CREATE OR REPLACE FUNCTION language_is_trusted( NAME, TEXT )
RETURNS TEXT AS $$
DECLARE
    is_trusted boolean := _is_trusted($1);
BEGIN
    IF is_trusted IS NULL THEN
        RETURN fail( $2 ) || E'\n' || diag( '    Procedural language ' || quote_ident($1) || ' does not exist') ;
    END IF;
    RETURN ok( is_trusted, $2 );
END;
$$ LANGUAGE plpgsql;

-- language_is_trusted( language )
CREATE OR REPLACE FUNCTION language_is_trusted( NAME )
RETURNS TEXT AS $$
    SELECT language_is_trusted($1, 'Procedural language ' || quote_ident($1) || ' should be trusted' );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _opc_exists( NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT EXISTS (
        SELECT TRUE
          FROM pg_catalog.pg_opclass oc
          JOIN pg_catalog.pg_namespace n ON oc.opcnamespace = n.oid
         WHERE n.nspname  = COALESCE($1, n.nspname)
           AND oc.opcname = $2
    );
$$ LANGUAGE SQL;

-- has_opclass( schema, name, description )
CREATE OR REPLACE FUNCTION has_opclass( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _opc_exists( $1, $2 ), $3 );
$$ LANGUAGE SQL;

-- has_opclass( schema, name )
CREATE OR REPLACE FUNCTION has_opclass( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok( _opc_exists( $1, $2 ), 'Operator class ' || quote_ident($1) || '.' || quote_ident($2) || ' should exist' );
$$ LANGUAGE SQL;

-- has_opclass( name, description )
CREATE OR REPLACE FUNCTION has_opclass( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( _opc_exists( NULL, $1 ), $2)
$$ LANGUAGE SQL;

-- has_opclass( name )
CREATE OR REPLACE FUNCTION has_opclass( NAME )
RETURNS TEXT AS $$
    SELECT ok( _opc_exists( NULL, $1 ), 'Operator class ' || quote_ident($1) || ' should exist' );
$$ LANGUAGE SQL;

-- hasnt_opclass( schema, name, description )
CREATE OR REPLACE FUNCTION hasnt_opclass( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _opc_exists( $1, $2 ), $3 );
$$ LANGUAGE SQL;

-- hasnt_opclass( schema, name )
CREATE OR REPLACE FUNCTION hasnt_opclass( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok( NOT _opc_exists( $1, $2 ), 'Operator class ' || quote_ident($1) || '.' || quote_ident($2) || ' should exist' );
$$ LANGUAGE SQL;

-- hasnt_opclass( name, description )
CREATE OR REPLACE FUNCTION hasnt_opclass( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT ok( NOT _opc_exists( NULL, $1 ), $2)
$$ LANGUAGE SQL;

-- hasnt_opclass( name )
CREATE OR REPLACE FUNCTION hasnt_opclass( NAME )
RETURNS TEXT AS $$
    SELECT ok( NOT _opc_exists( NULL, $1 ), 'Operator class ' || quote_ident($1) || ' should exist' );
$$ LANGUAGE SQL;

-- opclasses_are( schema, opclasses[], description )
CREATE OR REPLACE FUNCTION _nosuch( NAME, NAME, NAME[])
RETURNS TEXT AS $$
    SELECT E'\n' || diag(
        '    Function '
          || CASE WHEN $1 IS NOT NULL THEN quote_ident($1) || '.' ELSE '' END
          || quote_ident($2) || '('
          || array_to_string($3, ', ') || ') does not exist'
    );
$$ LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION _func_compare( NAME, NAME, NAME[], anyelement, anyelement, TEXT)
RETURNS TEXT AS $$
    SELECT CASE WHEN $4 IS NULL
      THEN ok( FALSE, $6 ) || _nosuch($1, $2, $3)
      ELSE is( $4, $5, $6 )
      END;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _func_compare( NAME, NAME, NAME[], boolean, TEXT)
RETURNS TEXT AS $$
    SELECT CASE WHEN $4 IS NULL
      THEN ok( FALSE, $5 ) || _nosuch($1, $2, $3)
      ELSE ok( $4, $5 )
      END;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _func_compare( NAME, NAME, anyelement, anyelement, TEXT)
RETURNS TEXT AS $$
    SELECT CASE WHEN $3 IS NULL
      THEN ok( FALSE, $5 ) || _nosuch($1, $2, '{}')
      ELSE is( $3, $4, $5 )
      END;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _func_compare( NAME, NAME, boolean, TEXT)
RETURNS TEXT AS $$
    SELECT CASE WHEN $3 IS NULL
      THEN ok( FALSE, $4 ) || _nosuch($1, $2, '{}')
      ELSE ok( $3, $4 )
      END;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _lang ( NAME, NAME, NAME[] )
RETURNS NAME AS $$
    SELECT l.lanname
      FROM tap_funky f
      JOIN pg_catalog.pg_language l ON f.langoid = l.oid
     WHERE f.schema = $1
       and f.name   = $2
       AND f.args   = array_to_string($3, ',')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _lang ( NAME, NAME )
RETURNS NAME AS $$
    SELECT l.lanname
      FROM tap_funky f
      JOIN pg_catalog.pg_language l ON f.langoid = l.oid
     WHERE f.schema = $1
       and f.name   = $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _lang ( NAME, NAME[] )
RETURNS NAME AS $$
    SELECT l.lanname
      FROM tap_funky f
      JOIN pg_catalog.pg_language l ON f.langoid = l.oid
     WHERE f.name = $1
       AND f.args = array_to_string($2, ',')
       AND f.is_visible;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _lang ( NAME )
RETURNS NAME AS $$
    SELECT l.lanname
      FROM tap_funky f
      JOIN pg_catalog.pg_language l ON f.langoid = l.oid
     WHERE f.name = $1
       AND f.is_visible;
$$ LANGUAGE SQL;

-- function_lang_is( schema, function, args[], language, description )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME, NAME[], NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, $3, _lang($1, $2, $3), $4, $5 );
$$ LANGUAGE SQL;

-- function_lang_is( schema, function, args[], language )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME, NAME[], NAME )
RETURNS TEXT AS $$
    SELECT function_lang_is(
        $1, $2, $3, $4,
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should be written in ' || quote_ident($4)
    );
$$ LANGUAGE SQL;

-- function_lang_is( schema, function, language, description )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, _lang($1, $2), $3, $4 );
$$ LANGUAGE SQL;

-- function_lang_is( schema, function, language )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME, NAME )
RETURNS TEXT AS $$
    SELECT function_lang_is(
        $1, $2, $3,
        'Function ' || quote_ident($1) || '.' || quote_ident($2)
        || '() should be written in ' || quote_ident($3)
    );
$$ LANGUAGE SQL;

-- function_lang_is( function, args[], language, description )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME[], NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, $2, _lang($1, $2), $3, $4 );
$$ LANGUAGE SQL;

-- function_lang_is( function, args[], language )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME[], NAME )
RETURNS TEXT AS $$
    SELECT function_lang_is(
        $1, $2, $3,
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should be written in ' || quote_ident($3)
    );
$$ LANGUAGE SQL;

-- function_lang_is( function, language, description )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, _lang($1), $2, $3 );
$$ LANGUAGE SQL;

-- function_lang_is( function, language )
CREATE OR REPLACE FUNCTION function_lang_is( NAME, NAME )
RETURNS TEXT AS $$
    SELECT function_lang_is(
        $1, $2,
        'Function ' || quote_ident($1)
        || '() should be written in ' || quote_ident($2)
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _returns ( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT returns
      FROM tap_funky
     WHERE schema = $1
       AND name   = $2
       AND args   = array_to_string($3, ',')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _returns ( NAME, NAME )
RETURNS TEXT AS $$
    SELECT returns FROM tap_funky WHERE schema = $1 AND name = $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _returns ( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT returns
      FROM tap_funky
     WHERE name = $1
       AND args = array_to_string($2, ',')
       AND is_visible;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _returns ( NAME )
RETURNS TEXT AS $$
    SELECT returns FROM tap_funky WHERE name = $1 AND is_visible;
$$ LANGUAGE SQL;

-- function_returns( schema, function, args[], type, description )
CREATE OR REPLACE FUNCTION function_returns( NAME, NAME, NAME[], TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, $3, _returns($1, $2, $3), $4, $5 );
$$ LANGUAGE SQL;

-- function_returns( schema, function, args[], type )
CREATE OR REPLACE FUNCTION function_returns( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT function_returns(
        $1, $2, $3, $4,
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should return ' || $4
    );
$$ LANGUAGE SQL;

-- function_returns( schema, function, type, description )
CREATE OR REPLACE FUNCTION function_returns( NAME, NAME, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, _returns($1, $2), $3, $4 );
$$ LANGUAGE SQL;

-- function_returns( schema, function, type )
CREATE OR REPLACE FUNCTION function_returns( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT function_returns(
        $1, $2, $3,
        'Function ' || quote_ident($1) || '.' || quote_ident($2)
        || '() should return ' || $3
    );
$$ LANGUAGE SQL;

-- function_returns( function, args[], type, description )
CREATE OR REPLACE FUNCTION function_returns( NAME, NAME[], TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, $2, _returns($1, $2), $3, $4 );
$$ LANGUAGE SQL;

-- function_returns( function, args[], type )
CREATE OR REPLACE FUNCTION function_returns( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT function_returns(
        $1, $2, $3,
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should return ' || $3
    );
$$ LANGUAGE SQL;

-- function_returns( function, type, description )
CREATE OR REPLACE FUNCTION function_returns( NAME, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, _returns($1), $2, $3 );
$$ LANGUAGE SQL;

-- function_returns( function, type )
CREATE OR REPLACE FUNCTION function_returns( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT function_returns(
        $1, $2,
        'Function ' || quote_ident($1) || '() should return ' || $2
    );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _definer ( NAME, NAME, NAME[] )
RETURNS BOOLEAN AS $$
    SELECT is_definer
      FROM tap_funky
     WHERE schema = $1
       AND name   = $2
       AND args   = array_to_string($3, ',')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _definer ( NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT is_definer FROM tap_funky WHERE schema = $1 AND name = $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _definer ( NAME, NAME[] )
RETURNS BOOLEAN AS $$
    SELECT is_definer
      FROM tap_funky
     WHERE name = $1
       AND args = array_to_string($2, ',')
       AND is_visible;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _definer ( NAME )
RETURNS BOOLEAN AS $$
    SELECT is_definer FROM tap_funky WHERE name = $1 AND is_visible;
$$ LANGUAGE SQL;

-- is_definer( schema, function, args[], description )
CREATE OR REPLACE FUNCTION is_definer ( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, $3, _definer($1, $2, $3), $4 );
$$ LANGUAGE SQL;

-- is_definer( schema, function, args[] )
CREATE OR REPLACE FUNCTION is_definer( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        _definer($1, $2, $3),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should be security definer'
    );
$$ LANGUAGE sql;

-- is_definer( schema, function, description )
CREATE OR REPLACE FUNCTION is_definer ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, _definer($1, $2), $3 );
$$ LANGUAGE SQL;

-- is_definer( schema, function )
CREATE OR REPLACE FUNCTION is_definer( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
        _definer($1, $2),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '() should be security definer'
    );
$$ LANGUAGE sql;

-- is_definer( function, args[], description )
CREATE OR REPLACE FUNCTION is_definer ( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, $2, _definer($1, $2), $3 );
$$ LANGUAGE SQL;

-- is_definer( function, args[] )
CREATE OR REPLACE FUNCTION is_definer( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        _definer($1, $2),
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should be security definer'
    );
$$ LANGUAGE sql;

-- is_definer( function, description )
CREATE OR REPLACE FUNCTION is_definer( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, _definer($1), $2 );
$$ LANGUAGE sql;

-- is_definer( function )
CREATE OR REPLACE FUNCTION is_definer( NAME )
RETURNS TEXT AS $$
    SELECT ok( _definer($1), 'Function ' || quote_ident($1) || '() should be security definer' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _strict ( NAME, NAME, NAME[] )
RETURNS BOOLEAN AS $$
    SELECT is_strict
      FROM tap_funky
     WHERE schema = $1
       AND name   = $2
       AND args   = array_to_string($3, ',')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _strict ( NAME, NAME )
RETURNS BOOLEAN AS $$
    SELECT is_strict FROM tap_funky WHERE schema = $1 AND name = $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _strict ( NAME, NAME[] )
RETURNS BOOLEAN AS $$
    SELECT is_strict
      FROM tap_funky
     WHERE name = $1
       AND args = array_to_string($2, ',')
       AND is_visible;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _strict ( NAME )
RETURNS BOOLEAN AS $$
    SELECT is_strict FROM tap_funky WHERE name = $1 AND is_visible;
$$ LANGUAGE SQL;

-- is_strict( schema, function, args[], description )
CREATE OR REPLACE FUNCTION is_strict ( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, $3, _strict($1, $2, $3), $4 );
$$ LANGUAGE SQL;

-- is_strict( schema, function, args[] )
CREATE OR REPLACE FUNCTION is_strict( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        _strict($1, $2, $3),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should be strict'
    );
$$ LANGUAGE sql;

-- is_strict( schema, function, description )
CREATE OR REPLACE FUNCTION is_strict ( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, _strict($1, $2), $3 );
$$ LANGUAGE SQL;

-- is_strict( schema, function )
CREATE OR REPLACE FUNCTION is_strict( NAME, NAME )
RETURNS TEXT AS $$
    SELECT ok(
        _strict($1, $2),
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '() should be strict'
    );
$$ LANGUAGE sql;

-- is_strict( function, args[], description )
CREATE OR REPLACE FUNCTION is_strict ( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, $2, _strict($1, $2), $3 );
$$ LANGUAGE SQL;

-- is_strict( function, args[] )
CREATE OR REPLACE FUNCTION is_strict( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT ok(
        _strict($1, $2),
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should be strict'
    );
$$ LANGUAGE sql;

-- is_strict( function, description )
CREATE OR REPLACE FUNCTION is_strict( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, _strict($1), $2 );
$$ LANGUAGE sql;

-- is_strict( function )
CREATE OR REPLACE FUNCTION is_strict( NAME )
RETURNS TEXT AS $$
    SELECT ok( _strict($1), 'Function ' || quote_ident($1) || '() should be strict' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _expand_vol( char )
RETURNS TEXT AS $$
   SELECT CASE $1
          WHEN 'i' THEN 'IMMUTABLE'
          WHEN 's' THEN 'STABLE'
          WHEN 'v' THEN 'VOLATILE'
          ELSE          'UNKNOWN' END
$$ LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION _refine_vol( text )
RETURNS text AS $$
    SELECT _expand_vol(substring(LOWER($1) FROM 1 FOR 1)::char);
$$ LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION _vol ( NAME, NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT _expand_vol(volatility)
      FROM tap_funky f
     WHERE f.schema = $1
       and f.name   = $2
       AND f.args   = array_to_string($3, ',')
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _vol ( NAME, NAME )
RETURNS TEXT AS $$
    SELECT _expand_vol(volatility) FROM tap_funky f
     WHERE f.schema = $1 and f.name = $2
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _vol ( NAME, NAME[] )
RETURNS TEXT AS $$
    SELECT _expand_vol(volatility)
      FROM tap_funky f
     WHERE f.name = $1
       AND f.args = array_to_string($2, ',')
       AND f.is_visible;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _vol ( NAME )
RETURNS TEXT AS $$
    SELECT _expand_vol(volatility) FROM tap_funky f
     WHERE f.name = $1 AND f.is_visible;
$$ LANGUAGE SQL;

-- volatility_is( schema, function, args[], volatility, description )
CREATE OR REPLACE FUNCTION volatility_is( NAME, NAME, NAME[], TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, $3, _vol($1, $2, $3), _refine_vol($4), $5 );
$$ LANGUAGE SQL;

-- volatility_is( schema, function, args[], volatility )
CREATE OR REPLACE FUNCTION volatility_is( NAME, NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT volatility_is(
        $1, $2, $3, $4,
        'Function ' || quote_ident($1) || '.' || quote_ident($2) || '(' ||
        array_to_string($3, ', ') || ') should be ' || _refine_vol($4)
    );
$$ LANGUAGE SQL;

-- volatility_is( schema, function, volatility, description )
CREATE OR REPLACE FUNCTION volatility_is( NAME, NAME, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare($1, $2, _vol($1, $2), _refine_vol($3), $4 );
$$ LANGUAGE SQL;

-- volatility_is( schema, function, volatility )
CREATE OR REPLACE FUNCTION volatility_is( NAME, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT volatility_is(
        $1, $2, $3,
        'Function ' || quote_ident($1) || '.' || quote_ident($2)
        || '() should be ' || _refine_vol($3)
    );
$$ LANGUAGE SQL;

-- volatility_is( function, args[], volatility, description )
CREATE OR REPLACE FUNCTION volatility_is( NAME, NAME[], TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, $2, _vol($1, $2), _refine_vol($3), $4 );
$$ LANGUAGE SQL;

-- volatility_is( function, args[], volatility )
CREATE OR REPLACE FUNCTION volatility_is( NAME, NAME[], TEXT )
RETURNS TEXT AS $$
    SELECT volatility_is(
        $1, $2, $3,
        'Function ' || quote_ident($1) || '(' ||
        array_to_string($2, ', ') || ') should be ' || _refine_vol($3)
    );
$$ LANGUAGE SQL;

-- volatility_is( function, volatility, description )
CREATE OR REPLACE FUNCTION volatility_is( NAME, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _func_compare(NULL, $1, _vol($1), _refine_vol($2), $3 );
$$ LANGUAGE SQL;

-- volatility_is( function, volatility )
CREATE OR REPLACE FUNCTION volatility_is( NAME, TEXT )
RETURNS TEXT AS $$
    SELECT volatility_is(
        $1, $2,
        'Function ' || quote_ident($1) || '() should be ' || _refine_vol($2)
    );
$$ LANGUAGE SQL;

-- check_test( test_output, pass, name, description, diag, match_diag )
CREATE OR REPLACE FUNCTION findfuncs( NAME, TEXT )
RETURNS TEXT[] AS $$
    SELECT ARRAY(
        SELECT DISTINCT quote_ident(n.nspname) || '.' || quote_ident(p.proname) AS pname
          FROM pg_catalog.pg_proc p
          JOIN pg_catalog.pg_namespace n ON p.pronamespace = n.oid
         WHERE n.nspname = $1
           AND p.proname ~ $2
         ORDER BY pname
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION findfuncs( TEXT )
RETURNS TEXT[] AS $$
    SELECT ARRAY(
        SELECT DISTINCT quote_ident(n.nspname) || '.' || quote_ident(p.proname) AS pname
          FROM pg_catalog.pg_proc p
          JOIN pg_catalog.pg_namespace n ON p.pronamespace = n.oid
         WHERE pg_catalog.pg_function_is_visible(p.oid)
           AND p.proname ~ $1
         ORDER BY pname
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _runem( text[], boolean )
RETURNS SETOF TEXT AS $$
DECLARE
    tap    text;
    lbound int := array_lower($1, 1);
BEGIN
    IF lbound IS NULL THEN RETURN; END IF;
    FOR i IN lbound..array_upper($1, 1) LOOP
        -- Send the name of the function to diag if warranted.
        IF $2 THEN RETURN NEXT diag( $1[i] || '()' ); END IF;
        -- Execute the tap function and return its results.
        FOR tap IN EXECUTE 'SELECT * FROM ' || $1[i] || '()' LOOP
            RETURN NEXT tap;
        END LOOP;
    END LOOP;
    RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _is_verbose()
RETURNS BOOLEAN AS $$
    SELECT current_setting('client_min_messages') NOT IN (
        'warning', 'error', 'fatal', 'panic'
    );
$$ LANGUAGE sql STABLE;

-- do_tap( schema, pattern )
CREATE OR REPLACE FUNCTION do_tap( name, text )
RETURNS SETOF TEXT AS $$
    SELECT * FROM _runem( findfuncs($1, $2), _is_verbose() );
$$ LANGUAGE sql;

-- do_tap( schema )
CREATE OR REPLACE FUNCTION do_tap( name )
RETURNS SETOF TEXT AS $$
    SELECT * FROM _runem( findfuncs($1, '^test'), _is_verbose() );
$$ LANGUAGE sql;

-- do_tap( pattern )
CREATE OR REPLACE FUNCTION do_tap( text )
RETURNS SETOF TEXT AS $$
    SELECT * FROM _runem( findfuncs($1), _is_verbose() );
$$ LANGUAGE sql;

-- do_tap()
CREATE OR REPLACE FUNCTION do_tap( )
RETURNS SETOF TEXT AS $$
    SELECT * FROM _runem( findfuncs('^test'), _is_verbose());
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _currtest()
RETURNS INTEGER AS $$
BEGIN
    RETURN currval('__tresults___numb_seq');
EXCEPTION
    WHEN object_not_in_prerequisite_state THEN RETURN 0;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _cleanup()
RETURNS boolean AS $$
    DROP TABLE __tresults__;
    DROP SEQUENCE __tresults___numb_seq;
    DROP TABLE __tcache__;
    DROP SEQUENCE __tcache___id_seq;
    SELECT TRUE;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _runner( text[], text[], text[], text[], text[] )
RETURNS SETOF TEXT AS $$
DECLARE
    startup  ALIAS FOR $1;
    shutdown ALIAS FOR $2;
    setup    ALIAS FOR $3;
    teardown ALIAS FOR $4;
    tests    ALIAS FOR $5;
    tap      text;
    verbos   boolean := _is_verbose(); -- verbose is a reserved word in 8.5.
    num_faild INTEGER := 0;
BEGIN
    BEGIN
        -- No plan support.
        PERFORM * FROM no_plan();
        FOR tap IN SELECT * FROM _runem(startup, false) LOOP RETURN NEXT tap; END LOOP;
    EXCEPTION
        -- Catch all exceptions and simply rethrow custom exceptions. This
        -- will roll back everything in the above block.
        WHEN raise_exception THEN
            RAISE EXCEPTION '%', SQLERRM;
    END;

    BEGIN
        FOR i IN 1..array_upper(tests, 1) LOOP
            BEGIN
                -- What test are we running?
                IF verbos THEN RETURN NEXT diag(tests[i] || '()'); END IF;

                -- Run the setup functions.
                FOR tap IN SELECT * FROM _runem(setup, false) LOOP RETURN NEXT tap; END LOOP;

                -- Run the actual test function.
                FOR tap IN EXECUTE 'SELECT * FROM ' || tests[i] || '()' LOOP
                    RETURN NEXT tap;
                END LOOP;

                -- Run the teardown functions.
                FOR tap IN SELECT * FROM _runem(teardown, false) LOOP RETURN NEXT tap; END LOOP;

                -- Remember how many failed and then roll back.
                num_faild := num_faild + num_failed();
                RAISE EXCEPTION '__TAP_ROLLBACK__';

            EXCEPTION WHEN raise_exception THEN
                IF SQLERRM <> '__TAP_ROLLBACK__' THEN
                    -- We didn't raise it, so propagate it.
                    RAISE EXCEPTION '%', SQLERRM;
                END IF;
            END;
        END LOOP;

        -- Run the shutdown functions.
        FOR tap IN SELECT * FROM _runem(shutdown, false) LOOP RETURN NEXT tap; END LOOP;

        -- Raise an exception to rollback any changes.
        RAISE EXCEPTION '__TAP_ROLLBACK__';
    EXCEPTION WHEN raise_exception THEN
        IF SQLERRM <> '__TAP_ROLLBACK__' THEN
            -- We didn't raise it, so propagate it.
            RAISE EXCEPTION '%', SQLERRM;
        END IF;
    END;
    -- Finish up.
    FOR tap IN SELECT * FROM _finish( currval('__tresults___numb_seq')::integer, 0, num_faild ) LOOP
        RETURN NEXT tap;
    END LOOP;

    -- Clean up and return.
    PERFORM _cleanup();
    RETURN;
END;
$$ LANGUAGE plpgsql;

-- runtests( schema, match )
CREATE OR REPLACE FUNCTION runtests( NAME, TEXT )
RETURNS SETOF TEXT AS $$
    SELECT * FROM _runner(
        findfuncs( $1, '^startup' ),
        findfuncs( $1, '^shutdown' ),
        findfuncs( $1, '^setup' ),
        findfuncs( $1, '^teardown' ),
        findfuncs( $1, $2 )
    );
$$ LANGUAGE sql;

-- runtests( schema )
CREATE OR REPLACE FUNCTION runtests( NAME )
RETURNS SETOF TEXT AS $$
    SELECT * FROM runtests( $1, '^test' );
$$ LANGUAGE sql;

-- runtests( match )
CREATE OR REPLACE FUNCTION runtests( TEXT )
RETURNS SETOF TEXT AS $$
    SELECT * FROM _runner(
        findfuncs( '^startup' ),
        findfuncs( '^shutdown' ),
        findfuncs( '^setup' ),
        findfuncs( '^teardown' ),
        findfuncs( $1 )
    );
$$ LANGUAGE sql;

-- runtests( )
CREATE OR REPLACE FUNCTION runtests( )
RETURNS SETOF TEXT AS $$
    SELECT * FROM runtests( '^test' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _temptable ( TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    EXECUTE 'CREATE TEMP TABLE ' || $2 || ' AS ' || _query($1);
    return $2;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _temptable ( anyarray, TEXT )
RETURNS TEXT AS $$
BEGIN
    CREATE TEMP TABLE _____coltmp___ AS
    SELECT $1[i]
    FROM generate_series(array_lower($1, 1), array_upper($1, 1)) s(i);
    EXECUTE 'ALTER TABLE _____coltmp___ RENAME TO ' || $2;
    return $2;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _temptypes( TEXT )
RETURNS TEXT AS $$
    SELECT array_to_string(ARRAY(
        SELECT pg_catalog.format_type(a.atttypid, a.atttypmod)
          FROM pg_catalog.pg_attribute a
          JOIN pg_catalog.pg_class c ON a.attrelid = c.oid
          JOIN pg_catalog.pg_namespace n ON c.relnamespace = n.oid
         WHERE c.relname = $1
           AND n.nspname LIKE 'pg_temp%'
           AND attnum > 0
           AND NOT attisdropped
         ORDER BY attnum
    ), ',');
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _docomp( TEXT, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    have    ALIAS FOR $1;
    want    ALIAS FOR $2;
    extras  TEXT[]  := '{}';
    missing TEXT[]  := '{}';
    res     BOOLEAN := TRUE;
    msg     TEXT    := '';
    rec     RECORD;
BEGIN
    BEGIN
        -- Find extra records.
        FOR rec in EXECUTE 'SELECT * FROM ' || have || ' EXCEPT ' || $4
                        || 'SELECT * FROM ' || want LOOP
            extras := extras || rec::text;
        END LOOP;

        -- Find missing records.
        FOR rec in EXECUTE 'SELECT * FROM ' || want || ' EXCEPT ' || $4
                        || 'SELECT * FROM ' || have LOOP
            missing := missing || rec::text;
        END LOOP;

        -- Drop the temporary tables.
        EXECUTE 'DROP TABLE ' || have;
        EXECUTE 'DROP TABLE ' || want;
    EXCEPTION WHEN syntax_error OR datatype_mismatch THEN
        msg := E'\n' || diag(
            E'    Columns differ between queries:\n'
            || '        have: (' || _temptypes(have) || E')\n'
            || '        want: (' || _temptypes(want) || ')'
        );
        EXECUTE 'DROP TABLE ' || have;
        EXECUTE 'DROP TABLE ' || want;
        RETURN ok(FALSE, $3) || msg;
    END;

    -- What extra records do we have?
    IF extras[1] IS NOT NULL THEN
        res := FALSE;
        msg := E'\n' || diag(
            E'    Extra records:\n        '
            ||  array_to_string( extras, E'\n        ' )
        );
    END IF;

    -- What missing records do we have?
    IF missing[1] IS NOT NULL THEN
        res := FALSE;
        msg := msg || E'\n' || diag(
            E'    Missing records:\n        '
            ||  array_to_string( missing, E'\n        ' )
        );
    END IF;

    RETURN ok(res, $3) || msg;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _relcomp( TEXT, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _docomp(
        _temptable( $1, '__taphave__' ),
        _temptable( $2, '__tapwant__' ),
        $3, $4
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _relcomp( TEXT, anyarray, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _docomp(
        _temptable( $1, '__taphave__' ),
        _temptable( $2, '__tapwant__' ),
        $3, $4
    );
$$ LANGUAGE sql;

-- set_eq( sql, sql, description )
CREATE OR REPLACE FUNCTION set_eq( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, '' );
$$ LANGUAGE sql;

-- set_eq( sql, sql )
CREATE OR REPLACE FUNCTION set_eq( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::text, '' );
$$ LANGUAGE sql;

-- set_eq( sql, array, description )
CREATE OR REPLACE FUNCTION set_eq( TEXT, anyarray, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, '' );
$$ LANGUAGE sql;

-- set_eq( sql, array )
CREATE OR REPLACE FUNCTION set_eq( TEXT, anyarray )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::text, '' );
$$ LANGUAGE sql;

-- bag_eq( sql, sql, description )
CREATE OR REPLACE FUNCTION bag_eq( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, 'ALL ' );
$$ LANGUAGE sql;

-- bag_eq( sql, sql )
CREATE OR REPLACE FUNCTION bag_eq( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::text, 'ALL ' );
$$ LANGUAGE sql;

-- bag_eq( sql, array, description )
CREATE OR REPLACE FUNCTION bag_eq( TEXT, anyarray, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, 'ALL ' );
$$ LANGUAGE sql;

-- bag_eq( sql, array )
CREATE OR REPLACE FUNCTION bag_eq( TEXT, anyarray )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::text, 'ALL ' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _do_ne( TEXT, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    have    ALIAS FOR $1;
    want    ALIAS FOR $2;
    extras  TEXT[]  := '{}';
    missing TEXT[]  := '{}';
    res     BOOLEAN := TRUE;
    msg     TEXT    := '';
BEGIN
    BEGIN
        -- Find extra records.
        EXECUTE 'SELECT EXISTS ( '
             || '( SELECT * FROM ' || have || ' EXCEPT ' || $4
             || '  SELECT * FROM ' || want
             || ' ) UNION ( '
             || '  SELECT * FROM ' || want || ' EXCEPT ' || $4
             || '  SELECT * FROM ' || have
             || ' ) LIMIT 1 )' INTO res;

        -- Drop the temporary tables.
        EXECUTE 'DROP TABLE ' || have;
        EXECUTE 'DROP TABLE ' || want;
    EXCEPTION WHEN syntax_error OR datatype_mismatch THEN
        msg := E'\n' || diag(
            E'    Columns differ between queries:\n'
            || '        have: (' || _temptypes(have) || E')\n'
            || '        want: (' || _temptypes(want) || ')'
        );
        EXECUTE 'DROP TABLE ' || have;
        EXECUTE 'DROP TABLE ' || want;
        RETURN ok(FALSE, $3) || msg;
    END;

    -- Return the value from the query.
    RETURN ok(res, $3);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION _relne( TEXT, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _do_ne(
        _temptable( $1, '__taphave__' ),
        _temptable( $2, '__tapwant__' ),
        $3, $4
    );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _relne( TEXT, anyarray, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _do_ne(
        _temptable( $1, '__taphave__' ),
        _temptable( $2, '__tapwant__' ),
        $3, $4
    );
$$ LANGUAGE sql;

-- set_ne( sql, sql, description )
CREATE OR REPLACE FUNCTION set_ne( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, $3, '' );
$$ LANGUAGE sql;

-- set_ne( sql, sql )
CREATE OR REPLACE FUNCTION set_ne( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, NULL::text, '' );
$$ LANGUAGE sql;

-- set_ne( sql, array, description )
CREATE OR REPLACE FUNCTION set_ne( TEXT, anyarray, TEXT )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, $3, '' );
$$ LANGUAGE sql;

-- set_ne( sql, array )
CREATE OR REPLACE FUNCTION set_ne( TEXT, anyarray )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, NULL::text, '' );
$$ LANGUAGE sql;

-- bag_ne( sql, sql, description )
CREATE OR REPLACE FUNCTION bag_ne( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, $3, 'ALL ' );
$$ LANGUAGE sql;

-- bag_ne( sql, sql )
CREATE OR REPLACE FUNCTION bag_ne( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, NULL::text, 'ALL ' );
$$ LANGUAGE sql;

-- bag_ne( sql, array, description )
CREATE OR REPLACE FUNCTION bag_ne( TEXT, anyarray, TEXT )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, $3, 'ALL ' );
$$ LANGUAGE sql;

-- bag_ne( sql, array )
CREATE OR REPLACE FUNCTION bag_ne( TEXT, anyarray )
RETURNS TEXT AS $$
    SELECT _relne( $1, $2, NULL::text, 'ALL ' );
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _relcomp( TEXT, TEXT, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    have    TEXT    := _temptable( $1, '__taphave__' );
    want    TEXT    := _temptable( $2, '__tapwant__' );
    results TEXT[]  := '{}';
    res     BOOLEAN := TRUE;
    msg     TEXT    := '';
    rec     RECORD;
BEGIN
    BEGIN
        -- Find relevant records.
        FOR rec in EXECUTE 'SELECT * FROM ' || want || ' ' || $4
                       || ' SELECT * FROM ' || have LOOP
            results := results || rec::text;
        END LOOP;

        -- Drop the temporary tables.
        EXECUTE 'DROP TABLE ' || have;
        EXECUTE 'DROP TABLE ' || want;
    EXCEPTION WHEN syntax_error OR datatype_mismatch THEN
        msg := E'\n' || diag(
            E'    Columns differ between queries:\n'
            || '        have: (' || _temptypes(have) || E')\n'
            || '        want: (' || _temptypes(want) || ')'
        );
        EXECUTE 'DROP TABLE ' || have;
        EXECUTE 'DROP TABLE ' || want;
        RETURN ok(FALSE, $3) || msg;
    END;

    -- What records do we have?
    IF results[1] IS NOT NULL THEN
        res := FALSE;
        msg := msg || E'\n' || diag(
            '    ' || $5 || E' records:\n        '
            ||  array_to_string( results, E'\n        ' )
        );
    END IF;

    RETURN ok(res, $3) || msg;
END;
$$ LANGUAGE plpgsql;

-- set_has( sql, sql, description )
CREATE OR REPLACE FUNCTION set_has( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, 'EXCEPT', 'Missing' );
$$ LANGUAGE sql;

-- set_has( sql, sql )
CREATE OR REPLACE FUNCTION set_has( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::TEXT, 'EXCEPT', 'Missing' );
$$ LANGUAGE sql;

-- bag_has( sql, sql, description )
CREATE OR REPLACE FUNCTION bag_has( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, 'EXCEPT ALL', 'Missing' );
$$ LANGUAGE sql;

-- bag_has( sql, sql )
CREATE OR REPLACE FUNCTION bag_has( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::TEXT, 'EXCEPT ALL', 'Missing' );
$$ LANGUAGE sql;

-- set_hasnt( sql, sql, description )
CREATE OR REPLACE FUNCTION set_hasnt( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, 'INTERSECT', 'Extra' );
$$ LANGUAGE sql;

-- set_hasnt( sql, sql )
CREATE OR REPLACE FUNCTION set_hasnt( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::TEXT, 'INTERSECT', 'Extra' );
$$ LANGUAGE sql;

-- bag_hasnt( sql, sql, description )
CREATE OR REPLACE FUNCTION bag_hasnt( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, $3, 'INTERSECT ALL', 'Extra' );
$$ LANGUAGE sql;

-- bag_hasnt( sql, sql )
CREATE OR REPLACE FUNCTION bag_hasnt( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT _relcomp( $1, $2, NULL::TEXT, 'INTERSECT ALL', 'Extra' );
$$ LANGUAGE sql;

-- results_eq( cursor, cursor, description )
CREATE OR REPLACE FUNCTION results_eq( refcursor, refcursor, text )
RETURNS TEXT AS $$
DECLARE
    have       ALIAS FOR $1;
    want       ALIAS FOR $2;
    have_rec   RECORD;
    want_rec   RECORD;
    have_found BOOLEAN;
    want_found BOOLEAN;
    rownum     INTEGER := 1;
BEGIN
    FETCH have INTO have_rec;
    have_found := FOUND;
    FETCH want INTO want_rec;
    want_found := FOUND;
    WHILE have_found OR want_found LOOP
        IF have_rec::text IS DISTINCT FROM want_rec::text OR have_found <> want_found THEN
            RETURN ok( false, $3 ) || E'\n' || diag(
                '    Results differ beginning at row ' || rownum || E':\n' ||
                '        have: ' || CASE WHEN have_found THEN have_rec::text ELSE 'NULL' END || E'\n' ||
                '        want: ' || CASE WHEN want_found THEN want_rec::text ELSE 'NULL' END
            );
        END IF;
        rownum = rownum + 1;
        FETCH have INTO have_rec;
        have_found := FOUND;
        FETCH want INTO want_rec;
        want_found := FOUND;
    END LOOP;

    RETURN ok( true, $3 );
EXCEPTION
    WHEN datatype_mismatch THEN
        RETURN ok( false, $3 ) || E'\n' || diag(
            E'    Columns differ between queries:\n' ||
            '        have: ' || CASE WHEN have_found THEN have_rec::text ELSE 'NULL' END || E'\n' ||
            '        want: ' || CASE WHEN want_found THEN want_rec::text ELSE 'NULL' END
        );
END;
$$ LANGUAGE plpgsql;

-- results_eq( cursor, cursor )
CREATE OR REPLACE FUNCTION results_eq( refcursor, refcursor )
RETURNS TEXT AS $$
    SELECT results_eq( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_eq( sql, sql, description )
CREATE OR REPLACE FUNCTION results_eq( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    have REFCURSOR;
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN have FOR EXECUTE _query($1);
    OPEN want FOR EXECUTE _query($2);
    res := results_eq(have, want, $3);
    CLOSE have;
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_eq( sql, sql )
CREATE OR REPLACE FUNCTION results_eq( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT results_eq( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_eq( sql, array, description )
CREATE OR REPLACE FUNCTION results_eq( TEXT, anyarray, TEXT )
RETURNS TEXT AS $$
DECLARE
    have REFCURSOR;
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN have FOR EXECUTE _query($1);
    OPEN want FOR SELECT $2[i]
    FROM generate_series(array_lower($2, 1), array_upper($2, 1)) s(i);
    res := results_eq(have, want, $3);
    CLOSE have;
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_eq( sql, array )
CREATE OR REPLACE FUNCTION results_eq( TEXT, anyarray )
RETURNS TEXT AS $$
    SELECT results_eq( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_eq( sql, cursor, description )
CREATE OR REPLACE FUNCTION results_eq( TEXT, refcursor, TEXT )
RETURNS TEXT AS $$
DECLARE
    have REFCURSOR;
    res  TEXT;
BEGIN
    OPEN have FOR EXECUTE _query($1);
    res := results_eq(have, $2, $3);
    CLOSE have;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_eq( sql, cursor )
CREATE OR REPLACE FUNCTION results_eq( TEXT, refcursor )
RETURNS TEXT AS $$
    SELECT results_eq( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_eq( cursor, sql, description )
CREATE OR REPLACE FUNCTION results_eq( refcursor, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN want FOR EXECUTE _query($2);
    res := results_eq($1, want, $3);
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_eq( cursor, sql )
CREATE OR REPLACE FUNCTION results_eq( refcursor, TEXT )
RETURNS TEXT AS $$
    SELECT results_eq( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_eq( cursor, array, description )
CREATE OR REPLACE FUNCTION results_eq( refcursor, anyarray, TEXT )
RETURNS TEXT AS $$
DECLARE
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN want FOR SELECT $2[i]
    FROM generate_series(array_lower($2, 1), array_upper($2, 1)) s(i);
    res := results_eq($1, want, $3);
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_eq( cursor, array )
CREATE OR REPLACE FUNCTION results_eq( refcursor, anyarray )
RETURNS TEXT AS $$
    SELECT results_eq( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_ne( cursor, cursor, description )
CREATE OR REPLACE FUNCTION results_ne( refcursor, refcursor, text )
RETURNS TEXT AS $$
DECLARE
    have       ALIAS FOR $1;
    want       ALIAS FOR $2;
    have_rec   RECORD;
    want_rec   RECORD;
    have_found BOOLEAN;
    want_found BOOLEAN;
BEGIN
    FETCH have INTO have_rec;
    have_found := FOUND;
    FETCH want INTO want_rec;
    want_found := FOUND;
    WHILE have_found OR want_found LOOP
        IF have_rec::text IS DISTINCT FROM want_rec::text OR have_found <> want_found THEN
            RETURN ok( true, $3 );
        ELSE
            FETCH have INTO have_rec;
            have_found := FOUND;
            FETCH want INTO want_rec;
            want_found := FOUND;
        END IF;
    END LOOP;
    RETURN ok( false, $3 );
EXCEPTION
    WHEN datatype_mismatch THEN
        RETURN ok( false, $3 ) || E'\n' || diag(
            E'    Columns differ between queries:\n' ||
            '        have: ' || CASE WHEN have_found THEN have_rec::text ELSE 'NULL' END || E'\n' ||
            '        want: ' || CASE WHEN want_found THEN want_rec::text ELSE 'NULL' END
        );
END;
$$ LANGUAGE plpgsql;

-- results_ne( cursor, cursor )
CREATE OR REPLACE FUNCTION results_ne( refcursor, refcursor )
RETURNS TEXT AS $$
    SELECT results_ne( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_ne( sql, sql, description )
CREATE OR REPLACE FUNCTION results_ne( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    have REFCURSOR;
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN have FOR EXECUTE _query($1);
    OPEN want FOR EXECUTE _query($2);
    res := results_ne(have, want, $3);
    CLOSE have;
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_ne( sql, sql )
CREATE OR REPLACE FUNCTION results_ne( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT results_ne( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_ne( sql, array, description )
CREATE OR REPLACE FUNCTION results_ne( TEXT, anyarray, TEXT )
RETURNS TEXT AS $$
DECLARE
    have REFCURSOR;
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN have FOR EXECUTE _query($1);
    OPEN want FOR SELECT $2[i]
    FROM generate_series(array_lower($2, 1), array_upper($2, 1)) s(i);
    res := results_ne(have, want, $3);
    CLOSE have;
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_ne( sql, array )
CREATE OR REPLACE FUNCTION results_ne( TEXT, anyarray )
RETURNS TEXT AS $$
    SELECT results_ne( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_ne( sql, cursor, description )
CREATE OR REPLACE FUNCTION results_ne( TEXT, refcursor, TEXT )
RETURNS TEXT AS $$
DECLARE
    have REFCURSOR;
    res  TEXT;
BEGIN
    OPEN have FOR EXECUTE _query($1);
    res := results_ne(have, $2, $3);
    CLOSE have;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_ne( sql, cursor )
CREATE OR REPLACE FUNCTION results_ne( TEXT, refcursor )
RETURNS TEXT AS $$
    SELECT results_ne( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_ne( cursor, sql, description )
CREATE OR REPLACE FUNCTION results_ne( refcursor, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN want FOR EXECUTE _query($2);
    res := results_ne($1, want, $3);
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_ne( cursor, sql )
CREATE OR REPLACE FUNCTION results_ne( refcursor, TEXT )
RETURNS TEXT AS $$
    SELECT results_ne( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- results_ne( cursor, array, description )
CREATE OR REPLACE FUNCTION results_ne( refcursor, anyarray, TEXT )
RETURNS TEXT AS $$
DECLARE
    want REFCURSOR;
    res  TEXT;
BEGIN
    OPEN want FOR SELECT $2[i]
    FROM generate_series(array_lower($2, 1), array_upper($2, 1)) s(i);
    res := results_ne($1, want, $3);
    CLOSE want;
    RETURN res;
END;
$$ LANGUAGE plpgsql;

-- results_ne( cursor, array )
CREATE OR REPLACE FUNCTION results_ne( refcursor, anyarray )
RETURNS TEXT AS $$
    SELECT results_ne( $1, $2, NULL::text );
$$ LANGUAGE sql;

-- isa_ok( value, regtype, description )
CREATE OR REPLACE FUNCTION isa_ok( anyelement, regtype, TEXT )
RETURNS TEXT AS $$
DECLARE
    typeof regtype := pg_typeof($1);
BEGIN
    IF typeof = $2 THEN RETURN ok(true, $3 || ' isa ' || $2 ); END IF;
    RETURN ok(false, $3 || ' isa ' || $2 ) || E'\n' ||
        diag('    ' || $3 || ' isn''t a "' || $2 || '" it''s a "' || typeof || '"');
END;
$$ LANGUAGE plpgsql;

-- isa_ok( value, regtype )
CREATE OR REPLACE FUNCTION isa_ok( anyelement, regtype )
RETURNS TEXT AS $$
    SELECT isa_ok($1, $2, 'the value');
$$ LANGUAGE sql;

-- is_empty( sql, description )
CREATE OR REPLACE FUNCTION is_empty( TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    extras  TEXT[]  := '{}';
    res     BOOLEAN := TRUE;
    msg     TEXT    := '';
    rec     RECORD;
BEGIN
    -- Find extra records.
    FOR rec in EXECUTE _query($1) LOOP
        extras := extras || rec::text;
    END LOOP;

    -- What extra records do we have?
    IF extras[1] IS NOT NULL THEN
        res := FALSE;
        msg := E'\n' || diag(
            E'    Unexpected records:\n        '
            ||  array_to_string( extras, E'\n        ' )
        );
    END IF;

    RETURN ok(res, $2) || msg;
END;
$$ LANGUAGE plpgsql;

-- is_empty( sql )
CREATE OR REPLACE FUNCTION is_empty( TEXT )
RETURNS TEXT AS $$
    SELECT is_empty( $1, NULL );
$$ LANGUAGE sql;

-- collect_tap( tap, tap, tap )
CREATE OR REPLACE FUNCTION collect_tap( text[] )
RETURNS TEXT AS $$
    SELECT array_to_string($1, E'\n');
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _tlike ( BOOLEAN, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT ok( $1, $4 ) || CASE WHEN $1 THEN '' ELSE E'\n' || diag(
           '   error message: ' || COALESCE( quote_literal($2), 'NULL' ) ||
       E'\n   doesn''t match: ' || COALESCE( quote_literal($3), 'NULL' )
    ) END;
$$ LANGUAGE sql;

-- throws_like ( sql, pattern, description )
CREATE OR REPLACE FUNCTION throws_like ( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    EXECUTE _query($1);
    RETURN ok( FALSE, $3 ) || E'\n' || diag( '    no exception thrown' );
EXCEPTION WHEN OTHERS THEN
    return _tlike( SQLERRM ~~ $2, SQLERRM, $2, $3 );
END;
$$ LANGUAGE plpgsql;

-- throws_like ( sql, pattern )
CREATE OR REPLACE FUNCTION throws_like ( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT throws_like($1, $2, 'Should throw exception like ' || quote_literal($2) );
$$ LANGUAGE sql;

-- throws_ilike ( sql, pattern, description )
CREATE OR REPLACE FUNCTION throws_ilike ( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    EXECUTE _query($1);
    RETURN ok( FALSE, $3 ) || E'\n' || diag( '    no exception thrown' );
EXCEPTION WHEN OTHERS THEN
    return _tlike( SQLERRM ~~* $2, SQLERRM, $2, $3 );
END;
$$ LANGUAGE plpgsql;

-- throws_ilike ( sql, pattern )
CREATE OR REPLACE FUNCTION throws_ilike ( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT throws_ilike($1, $2, 'Should throw exception like ' || quote_literal($2) );
$$ LANGUAGE sql;

-- throws_matching ( sql, pattern, description )
CREATE OR REPLACE FUNCTION throws_matching ( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    EXECUTE _query($1);
    RETURN ok( FALSE, $3 ) || E'\n' || diag( '    no exception thrown' );
EXCEPTION WHEN OTHERS THEN
    return _tlike( SQLERRM ~ $2, SQLERRM, $2, $3 );
END;
$$ LANGUAGE plpgsql;

-- throws_matching ( sql, pattern )
CREATE OR REPLACE FUNCTION throws_matching ( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT throws_matching($1, $2, 'Should throw exception matching ' || quote_literal($2) );
$$ LANGUAGE sql;

-- throws_imatching ( sql, pattern, description )
CREATE OR REPLACE FUNCTION throws_imatching ( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
BEGIN
    EXECUTE _query($1);
    RETURN ok( FALSE, $3 ) || E'\n' || diag( '    no exception thrown' );
EXCEPTION WHEN OTHERS THEN
    return _tlike( SQLERRM ~* $2, SQLERRM, $2, $3 );
END;
$$ LANGUAGE plpgsql;

-- throws_imatching ( sql, pattern )
CREATE OR REPLACE FUNCTION throws_imatching ( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT throws_imatching($1, $2, 'Should throw exception matching ' || quote_literal($2) );
$$ LANGUAGE sql;

-- roles_are( roles[], description )
CREATE OR REPLACE FUNCTION _dexists ( NAME, NAME )
RETURNS BOOLEAN AS $$
   SELECT EXISTS(
       SELECT true
         FROM pg_catalog.pg_namespace n
         JOIN pg_catalog.pg_type t on n.oid = t.typnamespace
        WHERE n.nspname = $1
          AND t.typname = $2
   );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _dexists ( NAME )
RETURNS BOOLEAN AS $$
   SELECT EXISTS(
       SELECT true
         FROM pg_catalog.pg_type t
        WHERE t.typname = $1
          AND pg_catalog.pg_type_is_visible(t.oid)
   );
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION _get_dtype( NAME, TEXT, BOOLEAN )
RETURNS TEXT AS $$
    SELECT display_type(CASE WHEN $3 THEN tn.nspname ELSE NULL END, t.oid, t.typtypmod)
      FROM pg_catalog.pg_type d
      JOIN pg_catalog.pg_namespace dn ON d.typnamespace = dn.oid
      JOIN pg_catalog.pg_type t       ON d.typbasetype  = t.oid
      JOIN pg_catalog.pg_namespace tn ON d.typnamespace = tn.oid
     WHERE d.typisdefined
       AND dn.nspname = $1
       AND d.typname  = LOWER($2)
       AND d.typtype  = 'd'
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION _get_dtype( NAME )
RETURNS TEXT AS $$
    SELECT display_type(t.oid, t.typtypmod)
      FROM pg_catalog.pg_type d
      JOIN pg_catalog.pg_type t  ON d.typbasetype  = t.oid
     WHERE d.typisdefined
       AND d.typname = LOWER($1)
       AND d.typtype = 'd'
$$ LANGUAGE sql;

-- domain_type_is( schema, domain, schema, type, description )
CREATE OR REPLACE FUNCTION domain_type_is( NAME, TEXT, NAME, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    actual_type TEXT := _get_dtype($1, $2, true);
BEGIN
    IF actual_type IS NULL THEN
        RETURN fail( $5 ) || E'\n' || diag (
            '   Domain ' || quote_ident($1) || '.' || $2
            || ' does not exist'
        );
    END IF;

    RETURN is( actual_type, quote_ident($3) || '.' || _quote_ident_like($4, actual_type), $5 );
END;
$$ LANGUAGE plpgsql;

-- domain_type_is( schema, domain, schema, type )
CREATE OR REPLACE FUNCTION domain_type_is( NAME, TEXT, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT domain_type_is(
        $1, $2, $3, $4,
        'Domain ' || quote_ident($1) || '.' || $2
        || ' should extend type ' || quote_ident($3) || '.' || $4
    );
$$ LANGUAGE SQL;

-- domain_type_is( schema, domain, type, description )
CREATE OR REPLACE FUNCTION domain_type_is( NAME, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    actual_type TEXT := _get_dtype($1, $2, false);
BEGIN
    IF actual_type IS NULL THEN
        RETURN fail( $4 ) || E'\n' || diag (
            '   Domain ' || quote_ident($1) || '.' || $2
            || ' does not exist'
        );
    END IF;

    RETURN is( actual_type, _quote_ident_like($3, actual_type), $4 );
END;
$$ LANGUAGE plpgsql;

-- domain_type_is( schema, domain, type )
CREATE OR REPLACE FUNCTION domain_type_is( NAME, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT domain_type_is(
        $1, $2, $3,
        'Domain ' || quote_ident($1) || '.' || $2
        || ' should extend type ' || $3
    );
$$ LANGUAGE SQL;

-- domain_type_is( domain, type, description )
CREATE OR REPLACE FUNCTION domain_type_is( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    actual_type TEXT := _get_dtype($1);
BEGIN
    IF actual_type IS NULL THEN
        RETURN fail( $3 ) || E'\n' || diag (
            '   Domain ' ||  $1 || ' does not exist'
        );
    END IF;

    RETURN is( actual_type, _quote_ident_like($2, actual_type), $3 );
END;
$$ LANGUAGE plpgsql;

-- domain_type_is( domain, type )
CREATE OR REPLACE FUNCTION domain_type_is( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT domain_type_is(
        $1, $2,
        'Domain ' || $1 || ' should extend type ' || $2
    );
$$ LANGUAGE SQL;

-- domain_type_isnt( schema, domain, schema, type, description )
CREATE OR REPLACE FUNCTION domain_type_isnt( NAME, TEXT, NAME, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    actual_type TEXT := _get_dtype($1, $2, true);
BEGIN
    IF actual_type IS NULL THEN
        RETURN fail( $5 ) || E'\n' || diag (
            '   Domain ' || quote_ident($1) || '.' || $2
            || ' does not exist'
        );
    END IF;

    RETURN isnt( actual_type, quote_ident($3) || '.' || _quote_ident_like($4, actual_type), $5 );
END;
$$ LANGUAGE plpgsql;

-- domain_type_isnt( schema, domain, schema, type )
CREATE OR REPLACE FUNCTION domain_type_isnt( NAME, TEXT, NAME, TEXT )
RETURNS TEXT AS $$
    SELECT domain_type_isnt(
        $1, $2, $3, $4,
        'Domain ' || quote_ident($1) || '.' || $2
        || ' should not extend type ' || quote_ident($3) || '.' || $4
    );
$$ LANGUAGE SQL;

-- domain_type_isnt( schema, domain, type, description )
CREATE OR REPLACE FUNCTION domain_type_isnt( NAME, TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    actual_type TEXT := _get_dtype($1, $2, false);
BEGIN
    IF actual_type IS NULL THEN
        RETURN fail( $4 ) || E'\n' || diag (
            '   Domain ' || quote_ident($1) || '.' || $2
            || ' does not exist'
        );
    END IF;

    RETURN isnt( actual_type, _quote_ident_like($3, actual_type), $4 );
END;
$$ LANGUAGE plpgsql;

-- domain_type_isnt( schema, domain, type )
CREATE OR REPLACE FUNCTION domain_type_isnt( NAME, TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT domain_type_isnt(
        $1, $2, $3,
        'Domain ' || quote_ident($1) || '.' || $2
        || ' should not extend type ' || $3
    );
$$ LANGUAGE SQL;

-- domain_type_isnt( domain, type, description )
CREATE OR REPLACE FUNCTION domain_type_isnt( TEXT, TEXT, TEXT )
RETURNS TEXT AS $$
DECLARE
    actual_type TEXT := _get_dtype($1);
BEGIN
    IF actual_type IS NULL THEN
        RETURN fail( $3 ) || E'\n' || diag (
            '   Domain ' ||  $1 || ' does not exist'
        );
    END IF;

    RETURN isnt( actual_type, _quote_ident_like($2, actual_type), $3 );
END;
$$ LANGUAGE plpgsql;

-- domain_type_isnt( domain, type )
CREATE OR REPLACE FUNCTION domain_type_isnt( TEXT, TEXT )
RETURNS TEXT AS $$
    SELECT domain_type_isnt(
        $1, $2,
        'Domain ' || $1 || ' should not extend type ' || $2
    );
$$ LANGUAGE SQL;

-- row_eq( sql, record, description )
CREATE OR REPLACE FUNCTION row_eq( TEXT, anyelement, TEXT )
RETURNS TEXT AS $$
DECLARE
    rec    RECORD;
BEGIN
    EXECUTE _query($1) INTO rec;
    IF NOT rec::text IS DISTINCT FROM $2::text THEN RETURN ok(true, $3); END IF;
    RETURN ok(false, $3 ) || E'\n' || diag(
           '        have: ' || CASE WHEN rec IS NULL THEN 'NULL' ELSE rec::text END ||
        E'\n        want: ' || CASE WHEN $2  IS NULL THEN 'NULL' ELSE $2::text  END
    );
END;
$$ LANGUAGE plpgsql;

-- row_eq( sql, record )
CREATE OR REPLACE FUNCTION row_eq( TEXT, anyelement )
RETURNS TEXT AS $$
    SELECT row_eq($1, $2, NULL );
$$ LANGUAGE sql;

-- triggers_are( schema, table, triggers[], description )
