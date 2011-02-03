/*
 * PostgreSQL type definitions for semver type
 * Written by Sam Vilain
 * sam@vilain.net
 */

#include "postgres.h"

#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <math.h>

#include "utils/builtins.h"
//#include "utils/lsyscache.h"
//#include "access/xact.h"

#include "fmgr.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

/* IO methods */
Datum		semver_in_cstring(PG_FUNCTION_ARGS);
Datum		semver_out_cstring(PG_FUNCTION_ARGS);

Datum		semver_eq(PG_FUNCTION_ARGS);
Datum		hash_semver(PG_FUNCTION_ARGS);
Datum		semver_ne(PG_FUNCTION_ARGS);
Datum		semver_lt(PG_FUNCTION_ARGS);
Datum		semver_le(PG_FUNCTION_ARGS);
Datum		semver_ge(PG_FUNCTION_ARGS);
Datum		semver_gt(PG_FUNCTION_ARGS);
Datum		semver_cmp(PG_FUNCTION_ARGS);

/* these typecasts are necessary for passing to functions that take text */
Datum		semver_in_text(PG_FUNCTION_ARGS);
Datum		semver_out_text(PG_FUNCTION_ARGS);

Datum		semver_smaller(PG_FUNCTION_ARGS);
Datum		semver_larger(PG_FUNCTION_ARGS);

/* memory/heap structure (not for binary marshalling) */
typedef struct semver
{
	int32 vl_len_;	/* varlena header */
        int16 numbers[3];
	char patchname[]; /* patch name, including the null byte for convenience */
} semver;

semver* make_semver(const int16 *numbers, const char* patchname) {
	int varsize = offsetof(semver, patchname) + (patchname ? strlen(patchname) : 0) + 1;
	semver *rv = palloc(varsize);
	int i;
	SET_VARSIZE(rv, varsize);
	elog(WARNING, "making semver: [%d,%d,%d],%s", numbers[0],numbers[1],numbers[2],patchname);
	for (i = 0; i < 3; i++) {
		rv->numbers[i] = numbers[i];
	}
	if (patchname) {
		strcpy(rv->patchname, patchname);
	}
	else {
		rv->patchname[0] = '\0';
	}
	return rv;
}

/* handy little function for dumping chunks of memory as hex :) */
char* dump_hex(void* ptr, int len)
{
	char* hexa;
	int i;
	char* x;
	unsigned char* nx;

	hexa = palloc( len * 2 + 1 );
	x = hexa;
	nx = ptr;
	for ( i = 0; i < len; i++ ) {
		sprintf(x, "%.2x", *nx);
		x += 2;
		nx++;
	}
	return hexa;
}

/* creating a currency from a string */
semver* parse_semver(char* str)
{
	int16 numbers[3];
	char* patchname = palloc(strlen(str));
	char junk[2];
	semver * newval;
	int parsed;

	parsed = sscanf(
		str, "%hd.%hd.%hd%[A-Za-z0-9-]%c",
		&numbers[0], &numbers[1], &numbers[2],
		patchname, (char*)&junk
		);
	elog(WARNING, "parsed fields: %d", parsed);

	if ( parsed < 2 || parsed > 4 || numbers[0] < 0 || numbers[1] < 0
	     || (parsed >= 3 && numbers[2] < 0) )
		elog(ERROR, "bad semver value '%s'", str);

	if ( parsed == 2 ) {
		numbers[2] = -1;
	}
	if ( parsed < 4 ) {
		elog(WARNING, "no patchname");
		pfree(patchname);
		patchname = 0;
	}
	else {
		if (*patchname != '\0' &&
		    !( ( *patchname >= 'A' && *patchname <= 'Z' ) ||
		       ( *patchname >= 'a' && *patchname <= 'z' ) ) )
			elog(ERROR, "bad patchlevel '%s' in semver value '%s' (must start with a letter)", patchname, str);
		elog(WARNING, "valid patchname '%s'", patchname);
	}

	newval = make_semver(numbers, patchname);
	if (patchname)
		pfree(patchname);

	elog(WARNING, "made semver '%s'", dump_hex(newval, VARSIZE(newval)));

	return newval;
}

char* emit_semver(semver* version) {
	char* res;
	int i,x,len;

	len = 0;
	for (i = 0; i < 3; i++) {
		x = version->numbers[i];
		len += (len ? 1 : 0) +
			(x >= 10000 ? 5 :
			 x >= 1000  ? 4 :
			 x >= 100   ? 3 :
			 x >= 10    ? 2 :
			 x <  0     ? 0 : 1);
	}
	len += strlen(version->patchname);
	res = palloc(len+1);
	sprintf(
		res, "%d.%d.%d%s",
		version->numbers[0],
		version->numbers[1],
		version->numbers[2],
		version->patchname
		);
		 
	return res;
}


/*
 * Pg bindings
 */

/* input function: C string */
PG_FUNCTION_INFO_V1(semver_in_cstring);
Datum
semver_in_cstring(PG_FUNCTION_ARGS)
{
	char *str = PG_GETARG_CSTRING(0);
	elog(WARNING, "parsing semver: %s", str);
	semver *result = parse_semver(str);
	if (!result)
		PG_RETURN_NULL();

	PG_RETURN_POINTER(result);
}/* output function: C string */

PG_FUNCTION_INFO_V1(semver_out_cstring);
Datum
semver_out_cstring(PG_FUNCTION_ARGS)
{
	semver* amount = (void*)PG_GETARG_POINTER(0);
	char *result;
	result = emit_semver(amount);

	PG_RETURN_CSTRING(result);
}

PG_FUNCTION_INFO_V1(semver_in_text);
Datum
semver_in_text(PG_FUNCTION_ARGS)
{
	text* sv = PG_GETARG_TEXT_PP(0);
	semver* rs = parse_semver(VARDATA(sv));
	PG_FREE_IF_COPY(sv, 0);
	PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(semver_out_text);
Datum
semver_out_text(PG_FUNCTION_ARGS)
{
	semver* semver = (void*) PG_GETARG_POINTER(0);
	char* xxx = emit_semver(semver);
	text* res = cstring_to_text(xxx);
	pfree(xxx);
	PG_RETURN_TEXT_P(res);
}

/* comparisons */
int _semver_cmp(semver* a, semver* b)
{
	int rv, i, a_x, b_x;
	rv = 0;
	for (i = 0; i < 3; i++) {
		a_x = a->numbers[i];
		b_x = b->numbers[i];
		if (a_x < b_x) {
			rv = -1;
			break;
		}
		else if (a_x > b_x) {
			rv = 1;
			break;
		}
	}
	if (rv == 0) {
		rv = strcasecmp(a->patchname, b->patchname);
	}
	return rv;
}

PG_FUNCTION_INFO_V1(semver_eq);
Datum
semver_eq(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_BOOL(diff == 0);
}

PG_FUNCTION_INFO_V1(semver_ne);
Datum
semver_ne(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_BOOL(diff != 0);
}

PG_FUNCTION_INFO_V1(semver_le);
Datum
semver_le(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_BOOL(diff <= 0);
}

PG_FUNCTION_INFO_V1(semver_lt);
Datum
semver_lt(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_BOOL(diff < 0);
}

PG_FUNCTION_INFO_V1(semver_ge);
Datum
semver_ge(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_BOOL(diff >= 0);
}

PG_FUNCTION_INFO_V1(semver_gt);
Datum
semver_gt(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_BOOL(diff > 0);
}

PG_FUNCTION_INFO_V1(semver_cmp);
Datum
semver_cmp(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	PG_FREE_IF_COPY(a, 0);
	PG_FREE_IF_COPY(b, 1);
	PG_RETURN_INT32(diff);
}

/* from catalog/pg_proc.h */
#define hashtext 400 
#define hashint2 449

/* so the '=' function can be 'hashes' */
PG_FUNCTION_INFO_V1(hash_semver);
Datum
hash_semver(PG_FUNCTION_ARGS)
{
	semver* version = (void*)PG_GETARG_POINTER(0);
	uint32 hash = 0;
	int i;
	Datum patchname;

	if (*version->patchname != '\0') {
		patchname = CStringGetTextDatum(version->patchname);
		hash = OidFunctionCall1(hashtext, patchname);
	}
	for (i = 0; i < 3; i++) {
		hash = (hash << (7+(i<<1))) & (hash >> (25-(i<<1)));
		hash ^= OidFunctionCall1(hashint2, version->numbers[i]);
	}

	PG_RETURN_INT32(hash);
}

PG_FUNCTION_INFO_V1(semver_larger);
Datum
semver_larger(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	if (diff >= 0) {
		PG_FREE_IF_COPY(b, 0);
		PG_RETURN_POINTER(a);
	}
	else {
		PG_FREE_IF_COPY(a, 0);
		PG_RETURN_POINTER(b);
	}
}

PG_FUNCTION_INFO_V1(semver_smaller);
Datum
semver_smaller(PG_FUNCTION_ARGS)
{
	semver* a = (void*)PG_GETARG_POINTER(0);
	semver* b = (void*)PG_GETARG_POINTER(1);
	int diff = _semver_cmp(a, b);
	if (diff <= 0) {
		PG_FREE_IF_COPY(b, 0);
		PG_RETURN_POINTER(a);
	}
	else {
		PG_FREE_IF_COPY(a, 0);
		PG_RETURN_POINTER(b);
	}
}
