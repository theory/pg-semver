\set ECHO 0
BEGIN;

\i test/pgtap-core.sql
\i sql/semver.sql

-- Before 8.4, there was no unnest(), so create one.
CREATE FUNCTION create_unnest(
) RETURNS SETOF BOOLEAN LANGUAGE PLPGSQL AS $$
BEGIN
    IF pg_version_num() < 80400 THEN
        EXECUTE $F$ CREATE FUNCTION unnest(
            anyarray
        ) RETURNS SETOF anyelement LANGUAGE sql AS $_$
            SELECT $1[i]
              FROM generate_series(array_lower($1, 1), array_upper($1, 1)) AS i;
        $_$;$F$;
    END IF;
END;
$$;

SELECT * FROM create_unnest();

SELECT plan(176);
--SELECT * FROM no_plan();

SELECT has_type('semver');
SELECT is( NULL::semver, NULL, 'semvers should be NULLable' );

SELECT lives_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    '"' || v || '" is a valid semver'
)  FROM unnest(ARRAY[
    '1.2.2',
    '0.2.2',
    '1.2.2',
    '0.0.0',
    '0.1.999',
    '9999.9999999.823823',
    '1.0.0beta1',
    '1.0.0beta2',
    '1.0.0',
    '20110204.0.0'
]) AS v;

SELECT throws_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    NULL,
    '"' || v || '" is not a valid semver'
)  FROM unnest(ARRAY[
   '1.2',
   '1.2.02',
   '1.2.2-',
   '1.2.3b#5',
   '03.3.3',
   'v1.2.2',
   '1.3b',
   '1.4b.0',
   '1v',
   '1v.2.2v',
   '1.2.4b.5'
]) AS v;

-- Test =, <=, and >=.
SELECT collect_tap(ARRAY[
    ok(semver_cmp(lv::semver, rv::semver) = 0, 'semver(' || lv || ', ' || rv || ') should = 0'),
    ok(lv::semver = rv::semver, 'v' || lv || ' should = v' || rv),
    ok(lv::semver <= rv::semver, 'v' || lv || ' should be <= v' || rv),
    ok(lv::semver >= rv::semver, 'v' || lv || ' should be >= v' || rv)
]) FROM (VALUES
    ('1.2.2',  '1.2.2'),
    ('1.2.23', '1.2.23'),
    ('0.0.0', '0.0.0'),
    ('999.888.7777', '999.888.7777'),
    ('0.1.2beta3', '0.1.2beta3'),
    ('1.0.0rc-1', '1.0.0RC-1')
 ) AS f(lv, rv);

-- Test semver <> semver
SELECT collect_tap(ARRAY[
    ok(semver_cmp(lv::semver, rv::semver) <> 0, 'semver(' || lv || ', ' || rv || ') should <> 0'),
    ok(lv::semver <> rv::semver, 'v' || lv || ' should not equal v' || rv)
]) FROM (VALUES
    ('1.2.2', '1.2.3'),
    ('0.0.1', '1.0.0'),
    ('1.0.1', '1.1.0'),
    ('1.1.1', '1.1.0'),
    ('1.2.3b', '1.2.3'),
    ('1.2.3', '1.2.3b'),
    ('1.2.3a', '1.2.3b'),
    ('1.2.3aaaaaaa1', '1.2.3aaaaaaa2')
  ) AS f(lv, rv);

-- Test >, >=, <, and <=.
SELECT collect_tap(ARRAY[
    ok( semver_cmp(lv::semver, rv::semver) > 0, 'semver(' || lv || ', ' || rv || ') should > 0'),
    ok( semver_cmp(rv::semver, lv::semver) < 0, 'semver(' || rv || ', ' || lv || ') should < 0'),
    ok(lv::semver > rv::semver, 'v' || lv || ' should be > v' || rv),
    ok(lv::semver >= rv::semver, 'v' || lv || ' should be >= v' || rv),
    ok(rv::semver < lv::semver, 'v' || rv || ' should be < v' || lv),
    ok(rv::semver <= lv::semver, 'v' || rv || ' should be <= v' || lv)
]) FROM (VALUES
    ('2.2.2', '1.1.1'),
    ('2.2.2', '2.1.1'),
    ('2.2.2', '2.2.1'),
    ('2.2.2b', '2.2.1'),
    ('2.2.2', '2.2.2b'),
    ('2.2.2c', '2.2.2b'),
    ('2.2.2rc-2', '2.2.2RC-1'),
    ('0.9.10', '0.9.9')
  ) AS f(lv, rv);

-- Test to_semver().
SELECT has_function('to_semver');
SELECT has_function('to_semver', ARRAY['text']);
SELECT function_returns('to_semver', 'semver');

SELECT is(
    to_semver(dirty),
    clean::semver,
    'to_semver(' || dirty || ') should return ' || clean
) FROM (VALUES
    ('1.2.2',          '1.2.2'),
    ('01.2.2',         '1.2.2'),
    ('1.02.2',         '1.2.2'),
    ('1.2.02',         '1.2.2'),
    ('1.2.02b',        '1.2.2b'),
    ('1.2.02beta-3  ', '1.2.2beta-3'),
    ('1.02.02rc1',     '1.2.2rc1'),
    ('1.0',            '1.0.0'),
    ('1',              '1.0.0'),
    ('.0.02',          '0.0.2'),
    ('1..02',          '1.0.2'),
    ('1..',            '1.0.0'),
    ('1.1',            '1.1.0'),
    ('1.2.b1',         '1.2.0b1'),
    ('9.0beta4',       '9.0.0beta4'), -- PostgreSQL format.
    ('9b',             '9.0.0b'),
    ('rc1',            '0.0.0rc1'),
    ('',               '0.0.0'),
    ('..2',            '0.0.2'),
    ('1.2.3 a',        '1.2.3a'),
    ('..2 b',          '0.0.2b'),
    ('  012.2.2',      '12.2.2'),
    ('20110204',  '20110204.0.0')
) v(dirty, clean);

-- to_semver still needs to reject truly bad input
SELECT throws_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    NULL,
    '"' || v || '" is not a valid semver'
)  FROM unnest(ARRAY[
   '1.2.0 beta 4',
   '1.2.2-',
   '1.2.3b#5',
   'v1.2.2',
   '1.4b.0',
   '1v.2.2v',
   '1.2.4b.5',
   '1.2.3.4',
   '1.2.3 4',
   '1.2000000000000000.3.4'
]) AS v;

-- Test sort ordering
CREATE TABLE vs (
    version semver
);

INSERT INTO vs VALUES ('1.2.0'), ('1.0.0'), ('0.9.9'), ('0.9.10');

SELECT is(max(version), '1.2.0', 'max(semver) should work')
  FROM vs;

SELECT is(min(version), '0.9.9', 'min(semver) should work')
  FROM vs;

SELECT results_eq(
    $$ SELECT version FROM vs ORDER BY version USING < $$,
    $$ VALUES ('0.9.9'::semver), ('0.9.10'::semver), ('1.0.0'::semver), ('1.2.0'::semver) $$,
    'ORDER BY semver USING < should work'
);

SELECT results_eq(
    $$ SELECT version FROM vs ORDER BY version USING > $$,
    $$ VALUES ('1.2.0'::semver), ('1.0.0'::semver), ('0.9.10'::semver), ('0.9.9'::semver) $$,
    'ORDER BY semver USING > should work'
);

-- Test constructors.
SELECT is( text('1.2.0'::semver), '1.2.0', 'construct to text' );
SELECT is( semver('1.2.0'), '1.2.0'::semver, 'construct from text' );
SELECT is( semver(1.2), '1.2.0'::semver, 'construct from bare number' );
SELECT is( semver(1.2::numeric), '1.2.0'::semver, 'construct from numeric' );
SELECT is( semver(1), '1.0.0'::semver, 'construct from bare integer' );
SELECT is( semver(1::integer), '1.0.0'::semver, 'construct from integer' );
SELECT is( semver(1::bigint), '1.0.0'::semver, 'construct from bigint' );
SELECT is( semver(1::smallint), '1.0.0'::semver, 'construct from smallint' );
SELECT is( semver(1.2::decimal), '1.2.0'::semver, 'construct from decimal' );
SELECT is( semver(1.2::real), '1.2.0'::semver, 'construct from real' );
SELECT is( semver(1.2::double precision), '1.2.0'::semver, 'construct from double' );
SELECT is( semver(1.2::float), '1.2.0'::semver, 'construct from float' );

-- Test casting.
SELECT is( semver('1.2.0'::text), '1.2.0', 'cast to text' );
SELECT is( text('1.2.0')::semver, '1.2.0'::semver, 'cast from text' );
SELECT is( 1::semver, '1.0.0'::semver, 'Cast from bare integer');
SELECT is( 1.2::semver, '1.2.0'::semver, 'Cast from bare number');
SELECT is( 1.2::numeric::semver, '1.2.0'::semver, 'Cast from numeric');
SELECT is( 1::integer::semver, '1.0.0'::semver, 'Cast from integer');
SELECT is( 1::bigint::semver, '1.0.0'::semver, 'Cast from bigint');
SELECT is( 1::smallint::semver, '1.0.0'::semver, 'Cast from smallint');
SELECT is( 1.0::decimal::semver, '1.0.0'::semver, 'Cast from decimal');
SELECT is( 1::decimal::semver, '1.0.0'::semver, 'Cast from decimal');
SELECT is( 1.0::real::semver, '1.0.0'::semver, 'Cast from real');
SELECT is( 1.0::double precision::semver, '1.0.0'::semver, 'Cast from double precision');
SELECT is( 1.0::float::semver, '1.0.0'::semver, 'Cast from float');

SELECT * FROM finish();
ROLLBACK;
