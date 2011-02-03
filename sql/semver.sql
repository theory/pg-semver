SET client_min_messages TO warning;
SET log_min_messages    TO warning;

-- Create a semantic version data type.
--
-- http://semver.org/.
--

-- 1. A normal version number MUST take the form X.Y.Z where X, Y, and Z are
-- integers. X is the major version, Y is the minor version, and Z is the
-- patch version. Each element MUST increase numerically. For instance: 1.9.0
-- < 1.10.0 < 1.11.0.
--
-- 2. A special version number MAY be denoted by appending an arbitrary string
-- immediately following the patch version. The string MUST be comprised of
-- only alphanumerics plus dash [0-9A-Za-z-] and MUST begin with an alpha
-- character [A-Za-z]. Special versions satisfy but have a lower precedence
-- than the associated normal version. Precedence SHOULD be determined by
-- lexicographic ASCII sort order. For instance: 1.0.0beta1 < 1.0.0beta2 <
-- 1.0.0.

CREATE DOMAIN semver AS TEXT
 CHECK ( VALUE ~ '^([1-9][0-9]*|0)[.]([1-9][0-9]*|0)[.]([1-9][0-9]*|0)([a-zA-Z][-0-9A-Za-z]*)?$' );

CREATE FUNCTION semver_cmp(
    lver semver,
    rver semver
) RETURNS INTEGER IMMUTABLE LANGUAGE plpgsql AS $$
DECLARE
    lparts TEXT[] := regexp_split_to_array(lver, '[.]');
    rparts TEXT[] := regexp_split_to_array(rver, '[.]');
    ret    INTEGER;
    lstr   TEXT;
    rstr   TEXT;
BEGIN
    ret := btint8cmp(lparts[1]::bigint, rparts[1]::bigint);
    IF ret <> 0 THEN RETURN ret; END IF;

    ret := btint8cmp(lparts[2]::bigint, rparts[2]::bigint);
    IF ret <> 0 THEN RETURN ret; END IF;

    lstr = substring(lparts[3] FROM '[a-zA-Z][-0-9A-Za-z]*$');
    rstr = substring(rparts[3] FROM '[a-zA-Z][-0-9A-Za-z]*$');

    IF lstr IS NULL THEN
        IF rstr IS NULL THEN
            -- No special string, just compare integers.
            RETURN btint8cmp(lparts[3]::bigint, rparts[3]::bigint);
        ELSE
            ret := btint8cmp(lparts[3]::bigint, substring(rparts[3], '^[0-9]+')::bigint);
            IF ret = 0 THEN
                -- NULL is greater than non NULL.
                RETURN 1;
            ELSE
                -- The numbers out-weigh the strings.
                RETURN ret;
            END IF;
        END IF;
    ELSE
        IF rstr IS NULL THEN
            ret := btint8cmp(substring(lparts[3], '^[0-9]+')::bigint, rparts[3]::bigint);
            IF ret = 0 THEN
                -- The string is less than the number.
                RETURN -1;
            ELSE
                -- The numbers out-weigh the strings.
                RETURN ret;
            END IF;
        ELSE
            -- Both numbers have appended strings.
            ret := btint8cmp(substring(lparts[3], '^[0-9]+')::bigint, substring(rparts[3], '^[0-9]+')::bigint);
            IF ret = 0 THEN
                -- Compare the strings case-insensitively.
                RETURN bttext_pattern_cmp(LOWER(lstr), LOWER(rstr));
            ELSE
                -- The numeric comparision trumps the strings.
                RETURN ret;
            END IF;
       END IF;
    END IF;
END;
$$;

CREATE OR REPLACE FUNCTION clean_semver(
    to_clean TEXT
) RETURNS SEMVER IMMUTABLE LANGUAGE sql AS $$
    SELECT (
           COALESCE(substring(v[1], '^[[:space:]]*[0-9]+')::bigint, '0') || '.'
        || COALESCE(substring(v[2], '^[[:space:]]*[0-9]+')::bigint, '0') || '.'
        || COALESCE(substring(v[3], '^[[:space:]]*[0-9]+')::bigint, '0')
        || COALESCE(trim(substring($1 FROM '[a-zA-Z][-0-9A-Za-z]*[[:space:]]*$')), '')
    )::semver
      FROM string_to_array($1, '.') v;
$$;

CREATE OR REPLACE FUNCTION semver_eq(
    semver,
    semver
) RETURNS BOOLEAN LANGUAGE SQL AS $$
    SELECT semver_cmp($1, $2) = 0;
$$;

CREATE OR REPLACE FUNCTION semver_ne(
    semver,
    semver
) RETURNS BOOLEAN LANGUAGE SQL AS $$
    SELECT semver_cmp($1, $2) <> 0;
$$;

CREATE OR REPLACE FUNCTION semver_lt(
    semver,
    semver
) RETURNS BOOLEAN LANGUAGE SQL AS $$
    SELECT semver_cmp($1, $2) < 0;
$$;

CREATE OR REPLACE FUNCTION semver_le(
    semver,
    semver
) RETURNS BOOLEAN LANGUAGE SQL AS $$
    SELECT semver_cmp($1, $2) <= 0;
$$;

CREATE OR REPLACE FUNCTION semver_gt(
    semver,
    semver
) RETURNS BOOLEAN LANGUAGE SQL AS $$
    SELECT semver_cmp($1, $2) > 0;
$$;

CREATE OR REPLACE FUNCTION semver_ge(
    semver,
    semver
) RETURNS BOOLEAN LANGUAGE SQL AS $$
    SELECT semver_cmp($1, $2) >= 0;
$$;

CREATE OPERATOR = (
    LEFTARG    = SEMVER,
    RIGHTARG   = SEMVER,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = semver_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    HASHES,
    MERGES
);

CREATE OPERATOR <> (
    LEFTARG    = SEMVER,
    RIGHTARG   = SEMVER,
    NEGATOR    = =,
    COMMUTATOR = <>,
    PROCEDURE  = semver_ne,
    RESTRICT   = neqsel,
    JOIN       = neqjoinsel
);

CREATE OPERATOR < (
    LEFTARG    = SEMVER,
    RIGHTARG   = SEMVER,
    NEGATOR    = >=,
    COMMUTATOR = >,
    PROCEDURE  = semver_lt,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR <= (
    LEFTARG    = SEMVER,
    RIGHTARG   = SEMVER,
    NEGATOR    = >,
    COMMUTATOR = >=,
    PROCEDURE  = semver_le,
    RESTRICT   = scalarltsel,
    JOIN       = scalarltjoinsel
);

CREATE OPERATOR >= (
    LEFTARG    = SEMVER,
    RIGHTARG   = SEMVER,
    NEGATOR    = <,
    COMMUTATOR = <=,
    PROCEDURE  = semver_ge,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR > (
    LEFTARG    = SEMVER,
    RIGHTARG   = SEMVER,
    NEGATOR    = <=,
    COMMUTATOR = <,
    PROCEDURE  = semver_gt,
    RESTRICT   = scalargtsel,
    JOIN       = scalargtjoinsel
);

CREATE OPERATOR CLASS semver_ops
DEFAULT FOR TYPE SEMVER USING btree AS
    OPERATOR    1   <  (semver, semver),
    OPERATOR    2   <= (semver, semver),
    OPERATOR    3   =  (semver, semver),
    OPERATOR    4   >= (semver, semver),
    OPERATOR    5   >  (semver, semver),
    FUNCTION    1   semver_cmp(semver, semver);

CREATE FUNCTION semver_smaller(
    semver,
    semver
) RETURNS SEMVER LANGUAGE plpgsql IMMUTABLE STRICT AS $$
BEGIN
    IF semver_cmp($1, $2) < 0 THEN RETURN $1; ELSE RETURN $2; END IF;
END;
$$;

CREATE FUNCTION semver_larger(
    semver,
    semver
) RETURNS SEMVER LANGUAGE plpgsql IMMUTABLE STRICT AS $$
BEGIN
    IF semver_cmp($1, $2) > 0 THEN RETURN $1; ELSE RETURN $2; END IF;
END;
$$;

CREATE AGGREGATE min(semver)  (
    SFUNC = semver_smaller,
    STYPE = semver,
    SORTOP = <
);

CREATE AGGREGATE max(semver)  (
    SFUNC = semver_larger,
    STYPE = semver,
    SORTOP = >
);
