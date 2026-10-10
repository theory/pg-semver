--
--  Update numeric cast functions with C implementations.
--
CREATE OR REPLACE FUNCTION semver(numeric)
	RETURNS semver
	AS 'MODULE_PATHNAME', 'numeric_to_semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION semver(real)
	RETURNS semver
	AS 'MODULE_PATHNAME', 'real_to_semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION semver(double precision)
	RETURNS semver
	AS 'MODULE_PATHNAME', 'double_to_semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION semver(integer)
	RETURNS semver
	AS 'MODULE_PATHNAME', 'int_to_semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION semver(smallint)
	RETURNS semver
	AS 'MODULE_PATHNAME', 'smallint_to_semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION semver(bigint)
	RETURNS semver
	AS 'MODULE_PATHNAME', 'bigint_to_semver'
	LANGUAGE C STRICT IMMUTABLE;
