--
-- Accessor functions
--

CREATE OR REPLACE FUNCTION get_semver_major(semver)
	RETURNS int4
	AS 'semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION get_semver_minor(semver)
	RETURNS int4
	AS 'semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION get_semver_patch(semver)
	RETURNS int4
	AS 'semver'
	LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION get_semver_prerelease(semver)
        RETURNS text
        AS 'semver'
        LANGUAGE C STRICT IMMUTABLE;
