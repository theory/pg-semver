CREATE OR REPLACE FUNCTION semver_recv(internal)
    RETURNS semver
    AS 'semver'
    LANGUAGE C STRICT IMMUTABLE;

CREATE OR REPLACE FUNCTION semver_send(semver)
    RETURNS bytea
    AS 'semver'
    LANGUAGE C STRICT IMMUTABLE;

ALTER TYPE semver SET
	RECEIVE = semver_recv,
	SEND = semver_send;
