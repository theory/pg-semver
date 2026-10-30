\set ECHO none
BEGIN;

\i test/pgtap-core.sql
CREATE EXTENSION semver;

SELECT plan(497);
--SELECT * FROM no_plan();

SELECT has_type('semver');
SELECT is( NULL::semver, NULL, 'semvers should be NULLable' );

SELECT lives_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    '"' || v || '" is a valid semver'
)  FROM unnest(ARRAY[
    '1.2.2',
    '0.2.2',
    '0.0.0',
    '0.1.999',
    '9999.9999999.823823',
    '1.0.0-beta1',
    '1.0.0-beta2',
    '1.0.0',
    '1.0.0-1',
    '1.0.0-alpha+d34dm34t',
    '1.0.0+d34dm34t',
    '20110204.0.0',
    '1.0.0-alpha.0a',
    '1.0.0+010',
    '1.0.0+alpha.010',
    '1.0.0-0AEF'
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
    '1.2.4b.5',
    '1.0.0-alpha.010',
    '1.0.0-02799',
    '1.1.2+.123',
    '1.1.2-.123',
    '1.2.3-ñø',
    '1.2.3+ñø1'
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
    ('0.1.2-beta3', '0.1.2-beta3')
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
    ('1.2.3-b', '1.2.3'),
    ('1.2.3', '1.2.3-b'),
    ('1.2.3-a', '1.2.3-b'),
    ('1.2.3-aaaaaaa1', '1.2.3-aaaaaaa2'),
    ('1.2.3-1.2.3', '1.2.3-1.2.3.4')
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
    ('2.2.2-b', '2.2.1'),
    ('2.2.2', '2.2.2-b'),
    ('2.2.2-c', '2.2.2-b'),
    ('2.2.2-rc-2', '2.2.2-RC-1'),
    ('2.2.2-rc-1', '2.2.2-RC-1'),
    ('0.9.10', '0.9.9'),
    ('1.0.1-1.2.3', '1.0.1-0.9.9.9')
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
    ('1.2.2',           '1.2.2'),
    ('01.2.2',          '1.2.2'),
    ('1.02.2',          '1.2.2'),
    ('1.2.02',          '1.2.2'),
    ('1.2.02b',        '1.2.2-b'),
    ('1.2.02beta-3  ', '1.2.2-beta-3'),
    ('1.02.02rc1',     '1.2.2-rc1'),
    ('1.0',             '1.0.0'),
    ('1',               '1.0.0'),
    ('.0.02',           '0.0.2'),
    ('1..02',           '1.0.2'),
    ('1..',             '1.0.0'),
    ('1.1',             '1.1.0'),
    ('1.2.b1',          '1.2.0-b1'),
    ('9.0beta4',        '9.0.0-beta4'), -- PostgreSQL format.
    ('9b',              '9.0.0-b'),
    ('rc1',             '0.0.0-rc1'),
    ('',                '0.0.0'),
    ('..2',             '0.0.2'),
    ('1.2.3 a',         '1.2.3-a'),
    ('..2 b',           '0.0.2-b'),
    ('  012.2.2',       '12.2.2'),
    ('20110204',        '20110204.0.0')
) v(dirty, clean);

SELECT is(
    to_semver(clean),
    clean::semver,
    'to_semver(' || clean || ') should return incoming text'
) FROM (VALUES
    ('1.0.0-alpha'),               -- SV2 9.
    ('1.0.0-alpha.1'),             -- SV2 9.
    ('1.0.0-0.3.7'),               -- SV2 9.
    ('1.0.0-x.7.z.92'),            -- SV2 9.
    ('1.0.0-alpha+001'),           -- SV2 10.
    ('1.0.0+20130313144700'),      -- SV2 10.
    ('1.0.0-beta+exp.sha.5114f85') -- SV2 10.
) v(clean);

-- to_semver still needs to reject truly bad input
SELECT throws_ok(
    $$ SELECT to_semver('$$ || bad || $$') $$,
    '22P02',
    err,
    $$to_semver('$$ || bad || $$') should fail$$
)  FROM (VALUES
   ('1.2.2-', $$bad semver value '1.2.2-': expected alphanumeric at char 6$$),
   ('1.2.3b#5', $$bad semver value '1.2.3b#5': non-alphanumeric pre-release at char 6$$),
   ('1.2.3.4', $$bad semver value '1.2.3.4': empty pre-release section at char 5$$),
   ('1.2000000000000000.3.4', $$bad semver value '1.2000000000000000.3.4': empty pre-release section at char 20$$),
   ('1.2.3-café', $$bad semver value '1.2.3-café': invalid character at char 10$$)
) v(bad, err);

-- semver() is stricter.
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
   '1.2000000000000000.3.4',
   '1.2.3-café'
]) AS v;

-- Test sort ordering
CREATE TABLE vs (
    version semver
);

INSERT INTO vs VALUES ('1.2.0'), ('1.0.0'), ('1.0.0-p0'), ('0.9.9'), ('0.9.10');

SELECT is(max(version), '1.2.0', 'max(semver) should work')
  FROM vs;

SELECT is(min(version), '0.9.9', 'min(semver) should work')
  FROM vs;

SELECT results_eq(
    $$ SELECT version FROM vs ORDER BY version USING < $$,
    $$ VALUES ('0.9.9'::semver), ('0.9.10'::semver), ('1.0.0-p0'::semver), ('1.0.0'::semver), ('1.2.0'::semver) $$,
    'ORDER BY semver USING < should work'
);

SELECT results_eq(
    $$ SELECT version FROM vs ORDER BY version USING > $$,
    $$ VALUES ('1.2.0'::semver), ('1.0.0'::semver), ('1.0.0-p0'::semver), ('0.9.10'::semver), ('0.9.9'::semver) $$,
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
SELECT is( semver(1.02::real), '1.2.0'::semver, 'construct from real with leading 0s' );
SELECT is( semver(1.2::double precision), '1.2.0'::semver, 'construct from double' );
SELECT is( semver(1.02::double precision), '1.2.0'::semver, 'construct from double with leading 0s' );
SELECT is( semver(1.2::float), '1.2.0'::semver, 'construct from float' );
SELECT is( semver(1.02::float), '1.2.0'::semver, 'construct from float with leading 0s' );

-- Test constructor errors.
SELECT throws_ok(
    $$ SELECT semver('$$ || v || $$'::float) $$,
    NULL,
    'cannot convert "' || v || '" to semver'
)  FROM unnest(ARRAY['infinity', '-infinity', '+infinity', 'nan']) AS v;
SELECT throws_ok(
    $$ SELECT semver('$$ || v || $$'::float8) $$,
    NULL,
    'cannot convert "' || v || '" to semver'
)  FROM unnest(ARRAY['infinity', '-infinity', '+infinity', 'nan']) AS v;
-- Numeric Infinity not added till Postgres 13.
SELECT throws_imatching(
    $$ SELECT semver('$$ || v || $$'::numeric) $$,
    '^(cannot convert "' || replace(v, '+', '') || '" to semver|invalid input syntax for type numeric: "' || replace(v, '+', '\+') || '")$',
    $$Should catch exception for semver('$$ || v || $$'::numeric)$$
)  FROM unnest(ARRAY['infinity', '-infinity', '+infinity', 'nan']) AS v;

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

-- Test casting errors.
SELECT throws_ok(
    $$ SELECT '$$ || v || $$'::float::semver $$,
    NULL,
    'cannot convert "' || v || '" to semver'
)  FROM unnest(ARRAY['infinity', '-infinity', '+infinity', 'nan']) AS v;
SELECT throws_ok(
    $$ SELECT '$$ || v || $$'::float8::semver $$,
    NULL,
    'cannot convert "' || v || '" to semver'
)  FROM unnest(ARRAY['infinity', '-infinity', '+infinity', 'nan']) AS v;
-- Numeric Infinity not added till Postgres 13.
SELECT throws_imatching(
    $$ SELECT '$$ || v || $$'::numeric::semver $$,
    '^(cannot convert "' || replace(v, '+', '') || '" to semver|invalid input syntax for type numeric: "' || replace(v, '+', '\+') || '")$',
    $$Should catch exception for '$$ || v || $$'::numeric::semver$$
)  FROM unnest(ARRAY['infinity', '-infinity', '+infinity', 'nan']) AS v;

-- Test casting some more.
SELECT IS(lv::text, rv, 'Should correctly cast "' || rv || '" to text')
  FROM (VALUES
    ('1.0.0-beta'::semver,   '1.0.0-beta'),
    ('1.0.0-beta1'::semver,  '1.0.0-beta1'),
    ('1.0.0-alpha'::semver,  '1.0.0-alpha'),
    ('1.0.0-alph'::semver,   '1.0.0-alph'),
    ('1.0.0-food'::semver,   '1.0.0-food'),
    ('1.0.0-f111'::semver,   '1.0.0-f111'),
    ('1.0.0-f111asbcdasdfasdfasdfasdfasdfasdffasdfadsf'::semver,
     '1.0.0-f111asbcdasdfasdfasdfasdfasdfasdffasdfadsf')
 ) AS f(lv, rv);

-- SEMV 2.0.0 tests.
SELECT lives_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    '"' || v || '" is a valid 2.0.0 semver'
)  FROM unnest(ARRAY[
    '1.0.0+1',
    '1.0.0-1+1',
    '1.0.0-1.1+1',
    '1.0.0-1.1.1.1.1.1.1.1.1.1.1+1.1.1.1.1.1.1.1',
    '1.0.0-1.2',
    '1.0.0-1.0.2',
    '1.0.0-alpha',
    '1.0.0-alpha.1',
    '1.0.0-0.3.7',
    '1.0.0-x.7.z.92',
    '0.2.13+1583426134.07de632'
]) AS v;

SELECT throws_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    NULL,
    '"' || v || '" is not a valid 2.0.0 semver'
)  FROM unnest(ARRAY[
   '1.0.0-a..',
   '1.0.0-a.1.',
   '1.0.0+1_1',
   '1.0.0-1....',
   '1.0.0-1_2',
   '1.0.0-1.02'
]) AS v;

-- Test sort ordering.
DELETE FROM vs;
INSERT INTO vs
VALUES ('0.9.9-a1.1+1234'::semver),
       ('0.9.9-a1.2.3'),
       ('0.9.9-a1.2'),
       ('0.9.9-a1.2222222222222222222222222222222222222222'),
       ('0.9.9-01ab'),
       ('0.9.9-01ba'),
       ('0.9.9'),
       ('1.0.0+99'),
       ('1.0.0-1');

SELECT results_eq(
    $$ SELECT version FROM vs ORDER BY version USING < $$,
    $$ VALUES ('0.9.9-01ab'::semver),
              ('0.9.9-01ba'),
              ('0.9.9-a1.1+1234'),
              ('0.9.9-a1.2'),
              ('0.9.9-a1.2.3'),
              ('0.9.9-a1.2222222222222222222222222222222222222222'),
              ('0.9.9'),
              ('1.0.0-1'),
              ('1.0.0+99')
    $$,
    'ORDER BY semver (2.0.0) USING < should work'
);

SELECT results_eq(
    $$ SELECT version FROM vs ORDER BY version USING > $$,
    $$ VALUES ('1.0.0+99'::semver),
              ('1.0.0-1'),
              ('0.9.9'),
              ('0.9.9-a1.2222222222222222222222222222222222222222'),
              ('0.9.9-a1.2.3'),
              ('0.9.9-a1.2'),
              ('0.9.9-a1.1+1234'),
              ('0.9.9-01ba'),
              ('0.9.9-01ab')
    $$,
    'ORDER BY semver (2.0.0) USING > should work'
);

SELECT collect_tap(ARRAY[
    ok(semver_cmp(lv::semver, rv::semver) = 0, 'semver(' || lv || ', ' || rv || ') should = 0'),
    ok(lv::semver = rv::semver, 'v' || lv || ' should = v' || rv),
    ok(lv::semver <= rv::semver, 'v' || lv || ' should be <= v' || rv),
    ok(lv::semver >= rv::semver, 'v' || lv || ' should be >= v' || rv)
]) FROM (VALUES
    ('1.0.0-1+1',  '1.0.0-1+5'),
    ('1.0.0-1.1+1',  '1.0.0-1.1+5')
 ) AS f(lv, rv);

-- Regressions.
SELECT is(
    to_semver(lv)::TEXT, rv,
    'Should correctly represent "' || lv || '" as "' || rv || '"'
) FROM (VALUES
      ('0.5.0-release1', '0.5.0-release1'),
      ('0.5.0release1',  '0.5.0-release1'),
      ('0.5-release1',   '0.5.0-release1'),
      ('0.5release1',    '0.5.0-release1'),
      ('0.5-1',          '0.5.0-1'),
      ('1.2.3-1.02',     '1.2.3-1.2')
) AS f(lv, rv);

-- Test is_semver().
SELECT has_function('is_semver');
SELECT has_function('is_semver', ARRAY['text']);
SELECT function_returns('is_semver', 'boolean');

SELECT is(
    is_semver(stimulus),
    expected,
    'is_semver(' || stimulus || ') should return ' || expected::text
) FROM (VALUES
    ('1.2.2',                     true),
    ('0.2.2',                     true),
    ('0.0.0',                     true),
    ('0.1.999',                   true),
    ('9999.9999999.823823',       true),
    ('1.0.0-beta1',               true),
    ('1.0.0-beta2',               true),
    ('1.0.0',                     true),
    ('1.0.0-1',                   true),
    ('1.0.0-alpha+d34dm34t',      true),
    ('1.0.0+d34dm34t',            true),
    ('20110204.0.0',              true),
    ('1.2',                       false),
    ('1.2.02',                    false),
    ('1.2.2-',                    false),
    ('1.2.3b#5',                  false),
    ('03.3.3',                    false),
    ('v1.2.2',                    false),
    ('1.3b',                      false),
    ('1.4b.0',                    false),
    ('1v',                        false),
    ('1v.2.2v',                   false),
    ('1.2.4b.5',                  false),
    ('1.2.3+a+b',                 false),
    ('1.2.3-x@',                  false),
    ('2016.5.18-MYW-600',         true),
    ('1010.5.0+2016-05-27-1832',  true),
    ('0.2.13+1583426134.07de632', true)
) v(stimulus, expected);

-- issue-gh-23
SELECT lives_ok(
    $$ SELECT '$$ || v || $$'::semver $$,
    '"' || v || '" is a valid semver'
)  FROM unnest(ARRAY[
    '2.3.0+80'
]) AS v;
SELECT is(
    to_semver(dirty),
    clean::semver,
    'to_semver(' || dirty || ') should return ' || clean
) FROM (VALUES
    ('2.3.0+80', '2.3.0+80')
) v(dirty, clean);
SELECT is(lv::text, rv, 'Should correctly cast "' || rv || '" to text')
  FROM (VALUES
    ('2.3.0+80'::semver, '2.3.0+80')
) AS f(lv, rv);
SELECT isnt(lv::semver > rv::semver, true, '"' || lv || '" > "' || rv || '" (NOT!)')
  FROM (VALUES
    ('2.3.0+80', '2.3.0+110')
) AS f(lv, rv);
SELECT is(lv::semver > rv::semver, true, '"' || lv || '" > "' || rv || '"')
  FROM (VALUES
    ('2.3.0+80', '2.3.0-alpha+110')
) AS f(lv, rv);
CREATE TABLE vs23 (
    version semver
);
INSERT INTO vs23 VALUES ('1.0.0-alpha'), ('1.0.0-alpha.1'), ('1.0.0-alpha.beta'), ('1.0.0-beta'), ('1.0.0-beta.2'), ('1.0.0-beta.11'), ('1.0.0-rc.1'), ('1.0.0');
SELECT results_eq(
    $$ SELECT version FROM vs23 ORDER BY version USING < $$,
    $$ VALUES ('1.0.0-alpha'::semver), ('1.0.0-alpha.1'::semver), ('1.0.0-alpha.beta'::semver), ('1.0.0-beta'::semver), ('1.0.0-beta.2'::semver), ('1.0.0-beta.11'::semver), ('1.0.0-rc.1'::semver), ('1.0.0'::semver) $$,
    'ORDER BY semver USING < should work (section 11)'
);
SELECT results_eq(
    $$ SELECT version FROM vs23 ORDER BY version USING > $$,
    $$ VALUES ('1.0.0'::semver), ('1.0.0-rc.1'::semver), ('1.0.0-beta.11'::semver), ('1.0.0-beta.2'::semver), ('1.0.0-beta'::semver), ('1.0.0-alpha.beta'::semver), ('1.0.0-alpha.1'::semver), ('1.0.0-alpha'::semver) $$,
    'ORDER BY semver USING > should work (section 11)'
);
SELECT is(lv::semver = rv::semver, true, '"' || lv || '" = "' || rv || '"')
  FROM (VALUES
    ('1.0.0', '1.0.0+535')
) AS f(lv, rv);
SELECT isnt(lv::semver < rv::semver, true, '"' || lv || '" < "' || rv || '" (NOT!)')
  FROM (VALUES
    ('1.0.0', '1.0.0+535')
) AS f(lv, rv);
SELECT isnt(lv::semver > rv::semver, true, '"' || lv || '" > "' || rv || '" (NOT!)')
  FROM (VALUES
    ('1.0.0', '1.0.0+535'),
    -- Works when prerelease numbers exceed integer bounds.
    ('1.0.0-a.2147483649', '1.0.0-a.2147483649'),
    ('1.0.0-a.99999999999999999999', '1.0.0-a.99999999999999999999'),
    ('1.0.0-a.99999999999999999999k', '1.0.0-a.99999999999999999999k')
) AS f(lv, rv);

SELECT is(lv::semver = rv::semver, false, '"' || lv || '" = "' || rv || '"')
  FROM (VALUES
    ('1.0.0-a.2147483649', '1.0.0-a.2147483650'),
    ('1.0.0-a.99999999999999999998', '1.0.0-a.99999999999999999999'),
    ('1.0.0-a.99999999999999999999k', '1.0.0-a.99999999999999999999j')
) AS f(lv, rv);

-- Test get_semver_major
SELECT has_function('get_semver_major');
SELECT has_function('get_semver_major', 'semver');
SELECT function_returns('get_semver_major', 'integer');
SELECT is(get_semver_major('2.1.0-alpha'::semver), 2, 'major version check');

-- Test get_semver_minor
SELECT has_function('get_semver_minor');
SELECT has_function('get_semver_minor', 'semver');
SELECT function_returns('get_semver_minor', 'integer');
SELECT is(get_semver_minor('2.1.0-alpha'::semver), 1, 'minor version check');

-- Test get_semver_patch
SELECT has_function('get_semver_patch');
SELECT has_function('get_semver_patch', 'semver');
SELECT function_returns('get_semver_patch', 'integer');
SELECT is(get_semver_patch('2.1.0-alpha'::semver), 0, 'patch version check');

-- Test get_semver_prerelease
SELECT has_function('get_semver_prerelease');
SELECT has_function('get_semver_prerelease', 'semver');
SELECT function_returns('get_semver_prerelease', 'text');
SELECT is(get_semver_prerelease('2.1.0-alpha'::semver), 'alpha', 'prerelease label check');
SELECT is(get_semver_prerelease('2.1.0-alpha+build'::semver), 'alpha', 'prerelease label check. must return prerelease only');
SELECT is(get_semver_prerelease('2.1.0+build'::semver), '', 'prerelease label check. must return empty string');

-- Test range type.
SELECT ok(
    '1.0.0'::semver <@ '[1.0.0, 2.0.0]'::semverrange,
    '1.0.0 should be in range [1.0.0, 2.0.0]'
);
SELECT ok(
    NOT '1.0.0'::semver <@ '[1.0.1, 2.0.0]'::semverrange,
    '1.0.0 should not be in range [1.0.1, 2.0.0]'
);

SELECT ok(
    NOT semverrange('1.0.0', '2.0.0') @> '2.0.0'::semver,
    '2.0.0 should not be in range [1.0.1, 2.0.0)'
);

SELECT ok(
    semverrange('1.0.0', '2.0.0') @> '1.9999.9999'::semver,
    '1.9999.9999 should be in range [1.0.1, 2.0.0)'
);

SELECT ok(
    '1000.0.0'::semver <@ '[1.0.0,]'::semverrange,
    '1000.0.0 should be in range [1.0.0,)'
);

SELECT bag_eq($$
    SELECT version, version <@ ANY(
        '{"(1.0.0,1.2.3)", "(1.2.3,1.4.5)", "(1.4.5,2.0.0)"}'::semverrange[]
    ) AS valid FROM (VALUES
        ('1.0.0'::semver), ('1.0.1'), ('1.2.3'), ('1.2.4'), ('1.4.4'), ('1.4.5'),
        ('1.7.0'), ('2.0.0')
    ) AS v(version)
$$, $$ VALUES
    ('1.0.0'::semver, false),
    ('1.0.1', true),
    ('1.2.3', false),
    ('1.2.4', true),
    ('1.4.4', true),
    ('1.4.5', false),
    ('1.7.0', true),
    ('2.0.0', false)
$$, 'Should be able to work with arrays of semverranges');

-- Test formatting (issue-gh-48)
SELECT is(
    '1.0.0+1234567890123456789012345'::semver::text,
    '1.0.0+1234567890123456789012345',
    'Should properly format a 32 character semver'
);

SELECT is(
    '1.0.0+12345678901234567890123456'::semver::text,
    '1.0.0+12345678901234567890123456',
    'Should properly format a 33 character semver'
);

-- issue-gh-46
SELECT is(
    '1.0.0-alpha-0'::semver::text, '1.0.0-alpha-0',
    'Should properly format a prerelease with a hyphen'
);

-- Make sure hashing works by forcing a hash aggregation. Also confirms
-- that prerelease strings are compared case-sensitively.
SET enable_sort=false;
SELECT bag_eq(
    $$
        SELECT DISTINCT v FROM (VALUES
            ('1.2.1'::SEMVER),
            ('1.2.1-RC1'),
            ('2.0.0-alph'),
            ('2.0.0-ALPH'),
            ('1.2.1'),
            ('9.20.30-OMG-1.23')
        ) v(v)
    $$, $$
        (VALUES
            ('1.2.1'::SEMVER),
            ('1.2.1-RC1'),
            ('2.0.0-alph'),
            ('2.0.0-ALPH'),
            ('9.20.30-OMG-1.23')
        )
    $$,
    'Should get distinct values via hash aggregation'
);

-- Hash index contract tests
SELECT isnt(
    hash_semver('1.0.0-abc'::semver),
    hash_semver('1.0.0-xyz'::semver),
    'hash_semver should generate different hashes for different prereleases'
);
SELECT is(
    hash_semver('1.0.0+foo'::semver),
    hash_semver('1.0.0+bar'::semver),
    'hash_semver should ignore build metadata'
);
SELECT is(
    hash_semver('1.0.0'::semver),
    hash_semver('1.0.0+foo'::semver),
    'hash_semver should match for version with and without build metadata'
);
SELECT is(
    hash_semver('2.1.3-alpha.1+build1'::semver),
    hash_semver('2.1.3-alpha.1+build2'::semver),
    'hash_semver should match for prereleases with different build metadata'
);

-- Test send and receive.
SELECT has_function('semver_send');
SELECT has_function('semver_send', ARRAY['semver']);
SELECT function_returns('semver_send', 'bytea');
SELECT has_function('semver_recv');
SELECT has_function('semver_recv', ARRAY['internal']);
SELECT function_returns('semver_recv', 'semver');

-- Set up some values to copy.
INSERT INTO vs VALUES
    ('1.2.2'::semver),
    ('9999.9999999.823823'),
    ('1.0.0-beta1'),
    ('1.0.0-1'),
    ('1.0.0-alpha+d34dm34t'),
    ('1.0.0+d34dm34t'),
    ('20110204.0.0'),
    ('1.0.0-0AEF'),
    (NULL)
;

-- Test semver_send().
SELECT is(
    semver_send(version), '\x01' || version::bytea,
    'semver_send(' || COALESCE(quote_literal(version::text), 'NULL') || ')'
) FROM vs;

-- Cannot test semver_recv() directly, so instead copy to a file and back into
-- another table.
\set copy_file '/tmp/semver_binary_copy.bin'
CREATE TABLE vs_recv(version semver);
COPY vs TO :'copy_file' WITH BINARY;
COPY vs_recv FROM :'copy_file' with binary;
SELECT bag_eq(
    'SELECT * FROM vs',
    'SELECT * FROM vs_recv',
    'Should have binary copied all of the semvers'
);

-- Reject binary input with nul byte.
COPY (SELECT '\x0131002e312e31'::bytea) TO :'copy_file' WITH BINARY;

SELECT throws_ok(
    format('COPY vs_recv FROM %L WITH BINARY', :'copy_file'),
    '22021',
    format('invalid byte sequence for encoding %I: 0x00', (SELECT current_setting('server_encoding'))),
    'Should get invalid input syntax error with nul byte'
);

--- Verify we support only plain storage.
 SELECT throws_ok(
    'ALTER TABLE vs ALTER version SET STORAGE EXTERNAL',
    NULL,
    'column data type semver can only have storage PLAIN'
);

-- Extended 64-bit hash tests
SELECT has_function('hash_semver_extended');
SELECT has_function('hash_semver_extended', ARRAY['semver', 'bigint']);
SELECT function_returns('hash_semver_extended', 'bigint');
SELECT is(
    hash_semver_extended('1.0.0+foo'::semver, 0),
    hash_semver_extended('1.0.0+bar'::semver, 0),
    'hash_semver_extended with seed 0 matches for equal semver'
);
SELECT is(
    hash_semver_extended('1.0.0+foo'::semver, 12345),
    hash_semver_extended('1.0.0+bar'::semver, 12345),
    'hash_semver_extended with seed 12345 matches for equal semver'
);
SELECT is(
    hash_semver_extended('2.1.3-alpha.1+build1'::semver, 999),
    hash_semver_extended('2.1.3-alpha.1+build2'::semver, 999),
    'hash_semver_extended matches for prereleases with different build metadata'
);
SELECT isnt(
    hash_semver_extended('1.0.0'::semver, 1),
    hash_semver_extended('1.0.0'::semver, 2),
    'hash_semver_extended varies by seed'
);

-- Round 2 security: boundary conditions, null character validation, and operator commutators
SELECT is( is_semver('1.0.0+foo+bar'), false, 'multiple plus signs in metadata returns false via is_semver' );
SELECT is( is_semver('1.0.0-01+foo'), false, 'leading zero in numeric prerelease rejected even with metadata' );
SELECT is( is_semver('1.0.0+'), false, 'empty metadata after plus returns false' );
SELECT is( is_semver('1.0.0-alpha+'), false, 'empty metadata after prerelease returns false' );
SELECT is( is_semver('1.0.0-'), false, 'empty prerelease after minus returns false' );

-- Operator commutators and selectivity
SELECT ok( '1.0.0'::semver < '2.0.0'::semver, 'semver < operator works' );
SELECT ok( '2.0.0'::semver > '1.0.0'::semver, 'semver > operator works' );
SELECT ok( '1.0.0'::semver <= '1.0.0'::semver, 'semver <= operator works' );
SELECT ok( '1.0.0'::semver >= '1.0.0'::semver, 'semver >= operator works' );
SELECT ok( '1.0.0'::semver <> '2.0.0'::semver, 'semver <> operator works' );

-- Aggregate tests
SELECT is( min(version), '0.9.9-01ab'::semver, 'min(semver) aggregate works' ) FROM vs WHERE version IS NOT NULL;
SELECT is( max(version), '20110204.0.0'::semver, 'max(semver) aggregate works' ) FROM vs WHERE version IS NOT NULL;

-- Security Review: leading zeros rejection for 00 in version numbers
SELECT is( is_semver('00.1.2'), false, 'leading 00 in major version rejected' );
SELECT is( is_semver('1.00.2'), false, 'leading 00 in minor version rejected' );
SELECT is( is_semver('1.2.00'), false, 'leading 00 in patch version rejected' );
SELECT is( is_semver('0.0.0'), true, 'single 0 version parts are valid' );

-- Security Review: internal whitespace rejection
SELECT is( is_semver('1. 2.3'), false, 'internal whitespace before minor rejected' );
SELECT is( is_semver('1.2. 3'), false, 'internal whitespace before patch rejected' );
SELECT is( is_semver(E'1.2.\n3'), false, 'internal newline in semver rejected' );

-- Security Review: release vs prerelease precedence and memory leak safety
SELECT ok( '1.0.0'::semver > '1.0.0-alpha'::semver, 'release version has higher precedence than prerelease' );
SELECT ok( '1.0.0-alpha'::semver < '1.0.0'::semver, 'prerelease has lower precedence than release' );

-- Security Review: declarative hash table partitioning (hash_semver_extended)
CREATE TABLE test_hash_part (v semver) PARTITION BY HASH (v);
CREATE TABLE test_hash_part_p0 PARTITION OF test_hash_part FOR VALUES WITH (MODULUS 2, REMAINDER 0);
CREATE TABLE test_hash_part_p1 PARTITION OF test_hash_part FOR VALUES WITH (MODULUS 2, REMAINDER 1);
INSERT INTO test_hash_part VALUES ('1.0.0'), ('1.0.0+build1'), ('2.1.0-alpha');
SELECT is( count(DISTINCT tableoid)::bigint, 1::bigint, 'equal semvers with different build metadata route to the same hash partition' )
    FROM test_hash_part WHERE v = '1.0.0'::semver;
DROP TABLE test_hash_part;

-- Security Review: storage and accessors on long semvers
CREATE TABLE test_toast_semver (id int, v semver);
INSERT INTO test_toast_semver VALUES (1, ('1.0.0-' || repeat('a', 2000))::semver);
SELECT is( length(v::text), 2006, 'long semver text length matches' ) FROM test_toast_semver;
SELECT is( get_semver_major(v), 1, 'get_semver_major on long semver' ) FROM test_toast_semver;
SELECT is( get_semver_minor(v), 0, 'get_semver_minor on long semver' ) FROM test_toast_semver;
SELECT is( get_semver_patch(v), 0, 'get_semver_patch on long semver' ) FROM test_toast_semver;
SELECT is( length(get_semver_prerelease(v)), 2000, 'get_semver_prerelease on long semver' ) FROM test_toast_semver;
CREATE INDEX idx_toast_btree ON test_toast_semver USING btree (v);
CREATE INDEX idx_toast_hash ON test_toast_semver USING hash (v);
SELECT is( count(*), 1::bigint, 'btree index scan on long semver' ) FROM test_toast_semver WHERE v = ('1.0.0-' || repeat('a', 2000))::semver;
DROP TABLE test_toast_semver;

-- Security Review: unprivileged role access and permission safety (Rule 28)
CREATE ROLE semver_test_unpriv NOSUPERUSER NOCREATEDB NOCREATEROLE LOGIN;
GRANT USAGE ON SCHEMA public TO semver_test_unpriv;
SET ROLE semver_test_unpriv;
SELECT is( '1.2.3'::semver < '1.2.4'::semver, true, 'unprivileged role can use semver operators' );
SELECT is( to_semver('1.2')::text, '1.2.0', 'unprivileged role can execute to_semver' );
SELECT is( to_semver1(1, 2, 3)::text, '1.2.3', 'unprivileged role can execute to_semver1' );
RESET ROLE;
--
-- semver2 tests: bigint and text accessors, >32-bit and >64-bit components,
-- component digit limits, and legacy v1 compatibility.
--

-- Function existence and return types
SELECT has_function('get_semver_major_bigint');
SELECT has_function('get_semver_major_bigint', ARRAY['semver']);
SELECT function_returns('get_semver_major_bigint', 'bigint');
SELECT has_function('get_semver_minor_bigint');
SELECT has_function('get_semver_minor_bigint', ARRAY['semver']);
SELECT function_returns('get_semver_minor_bigint', 'bigint');
SELECT has_function('get_semver_patch_bigint');
SELECT has_function('get_semver_patch_bigint', ARRAY['semver']);
SELECT function_returns('get_semver_patch_bigint', 'bigint');
SELECT has_function('get_semver_major_text');
SELECT has_function('get_semver_major_text', ARRAY['semver']);
SELECT function_returns('get_semver_major_text', 'text');
SELECT has_function('get_semver_minor_text');
SELECT has_function('get_semver_minor_text', ARRAY['semver']);
SELECT function_returns('get_semver_minor_text', 'text');
SELECT has_function('get_semver_patch_text');
SELECT has_function('get_semver_patch_text', ARRAY['semver']);
SELECT function_returns('get_semver_patch_text', 'text');

-- Standard values with bigint and text accessors
SELECT is(get_semver_major_bigint('2.1.0-alpha'::semver), 2::bigint, 'major bigint');
SELECT is(get_semver_minor_bigint('2.1.0-alpha'::semver), 1::bigint, 'minor bigint');
SELECT is(get_semver_patch_bigint('2.1.0-alpha'::semver), 0::bigint, 'patch bigint');
SELECT is(get_semver_major_text('2.1.0-alpha'::semver), '2', 'major text');
SELECT is(get_semver_minor_text('2.1.0-alpha'::semver), '1', 'minor text');
SELECT is(get_semver_patch_text('2.1.0-alpha'::semver), '0', 'patch text');

-- Values exceeding 32-bit integer range
SELECT throws_ok($$ SELECT get_semver_major('3000000000.4000000000.5000000000'::semver) $$, '22003', NULL, 'major 32-bit overflow error');
SELECT throws_ok($$ SELECT get_semver_minor('3000000000.4000000000.5000000000'::semver) $$, '22003', NULL, 'minor 32-bit overflow error');
SELECT throws_ok($$ SELECT get_semver_patch('3000000000.4000000000.5000000000'::semver) $$, '22003', NULL, 'patch 32-bit overflow error');
SELECT is(get_semver_major_bigint('3000000000.4000000000.5000000000'::semver), 3000000000::bigint, 'major bigint > 32-bit');
SELECT is(get_semver_minor_bigint('3000000000.4000000000.5000000000'::semver), 4000000000::bigint, 'minor bigint > 32-bit');
SELECT is(get_semver_patch_bigint('3000000000.4000000000.5000000000'::semver), 5000000000::bigint, 'patch bigint > 32-bit');
SELECT is(get_semver_major_text('3000000000.4000000000.5000000000'::semver), '3000000000', 'major text > 32-bit');
SELECT is(get_semver_minor_text('3000000000.4000000000.5000000000'::semver), '4000000000', 'minor text > 32-bit');
SELECT is(get_semver_patch_text('3000000000.4000000000.5000000000'::semver), '5000000000', 'patch text > 32-bit');

-- Values exceeding 64-bit integer range
SELECT throws_ok($$ SELECT get_semver_major_bigint(('99999999999999999999999999999999999999999999999999.0.0')::semver) $$, '22003', NULL, 'major 64-bit overflow error');
SELECT is(get_semver_major_text(('99999999999999999999999999999999999999999999999999.0.0')::semver), '99999999999999999999999999999999999999999999999999', 'major text > 64-bit');

-- Component limits (127 vs 128 digits)
SELECT lives_ok($$ SELECT (repeat('1', 127) || '.0.0')::semver $$, '127 digits component is valid');
SELECT is(is_semver(repeat('1', 128) || '.0.0'), false, '128 digits component rejected by is_semver');
SELECT throws_ok($$ SELECT (repeat('1', 128) || '.0.0')::semver $$, '22P02', NULL, '128 digits component throws error');

-- Legacy v1 format compatibility via to_semver1
SELECT is(to_semver1(1, 2, 3, 'alpha')::text, '1.2.3-alpha', 'to_semver1 text cast');
SELECT is(to_semver1(1, 2, 3)::text, '1.2.3', 'to_semver1 without prerel text cast');
SELECT is(to_semver1(NULL::int, 1, 2), NULL, 'to_semver1 NULL major returns NULL');
SELECT is(to_semver1(1, NULL::int, 2), NULL, 'to_semver1 NULL minor returns NULL');
SELECT is(to_semver1(1, 2, NULL::int), NULL, 'to_semver1 NULL patch returns NULL');
SELECT throws_ok($$ SELECT to_semver1(-1, 0, 0) $$, '22023', NULL, 'to_semver1 negative major rejected');
SELECT throws_ok($$ SELECT to_semver1(0, -1, 0) $$, '22023', NULL, 'to_semver1 negative minor rejected');
SELECT throws_ok($$ SELECT to_semver1(0, 0, -1) $$, '22023', NULL, 'to_semver1 negative patch rejected');
SELECT throws_ok($$ SELECT to_semver1(1, 0, 0, 'a' || chr(0) || 'b') $$, '54000', NULL, 'to_semver1 embedded null prerel rejected');
SELECT ok(to_semver1(1, 2, 3, 'alpha') = '1.2.3-alpha'::semver, 'v1 vs v2 equality');
SELECT ok(to_semver1(1, 2, 3) < '1.2.4'::semver, 'v1 vs v2 less than');
SELECT ok('1.2.4'::semver > to_semver1(1, 2, 3), 'v2 vs v1 greater than');
SELECT ok(to_semver1(1, 2, 3) < '3000000000.0.0'::semver, 'v1 vs large v2 less than');
SELECT ok('3000000000.0.0'::semver > to_semver1(1, 2, 3), 'large v2 vs v1 greater than');
SELECT is(get_semver_major(to_semver1(1, 2, 3)), 1, 'get_semver_major on v1');
SELECT is(get_semver_major_bigint(to_semver1(1, 2, 3)), 1::bigint, 'get_semver_major_bigint on v1');
SELECT is(get_semver_major_text(to_semver1(1, 2, 3)), '1', 'get_semver_major_text on v1');
SELECT is(get_semver_prerelease(to_semver1(1, 2, 3, 'alpha+build')), 'alpha', 'get_semver_prerelease on v1');
SELECT is(hash_semver(to_semver1(1, 2, 3, 'alpha')), hash_semver('1.2.3-alpha'::semver), 'v1 and v2 hash_semver match');
SELECT is(hash_semver_extended(to_semver1(1, 2, 3, 'alpha'), 42), hash_semver_extended('1.2.3-alpha'::semver, 42), 'v1 and v2 hash_semver_extended match');

-- Additional edge case tests: v2 length vs numeric ordering, v1 vs v2 mixed ordering, and build metadata hashing
SELECT ok('10.0.0'::semver > '9.0.0'::semver, 'v2 numeric len comparison 10 > 9');
SELECT ok('1.0.0-alpha.10'::semver > '1.0.0-alpha.2'::semver, 'v2 prerelease numeric comparison 10 > 2');
SELECT ok(to_semver1(10, 0, 0) > '9.0.0'::semver, 'v1 vs v2 mixed comparison 10 > 9');
SELECT ok(to_semver1(9, 0, 0) < '10.0.0'::semver, 'v1 vs v2 mixed comparison 9 < 10');
SELECT ok(to_semver1(1, 0, 0) > '1.0.0-alpha'::semver, 'v1 release > v2 prerelease');
SELECT ok(to_semver1(1, 0, 0, 'alpha') < '1.0.0'::semver, 'v1 prerelease < v2 release');
SELECT is(hash_semver('1.0.0+foo'::semver), hash_semver('1.0.0+bar'::semver), 'v2 build metadata ignored in hash_semver');
SELECT is(hash_semver_extended('1.0.0+foo'::semver, 123), hash_semver_extended('1.0.0+bar'::semver, 123), 'v2 build metadata ignored in hash_semver_extended');

-- Control character and trailing space rejection in strict mode, and build metadata leading zeros in lax mode
SELECT is(is_semver(E'1.2.3\n'), false, 'trailing newline rejected by is_semver');
SELECT is(is_semver(E'\t1.2.3'), false, 'leading tab rejected by is_semver');
SELECT is(is_semver('1.2.3 '), false, 'trailing space rejected by is_semver');
SELECT throws_ok($$ SELECT E'1.2.3\n'::semver $$, '22P02', NULL, 'trailing newline throws invalid character error');
SELECT throws_ok($$ SELECT E'\t1.2.3'::semver $$, '22P02', NULL, 'leading tab throws invalid character error');
SELECT throws_ok($$ SELECT '1.2.3 '::semver $$, '22P02', NULL, 'trailing space throws error in strict mode');
SELECT is(to_semver('1.0.0+build.01')::text, '1.0.0+build.01', 'to_semver preserves leading zero in dot-separated build metadata');
SELECT is(to_semver('1.0.0+01.02')::text, '1.0.0+01.02', 'to_semver preserves leading zeros across multiple build metadata parts');
SELECT is(to_semver('1.0.0-01.002+03.004')::text, '1.0.0-1.2+03.004', 'to_semver strips leading zeros in prerelease but preserves them in build metadata');

SELECT * FROM finish();
ROLLBACK;
