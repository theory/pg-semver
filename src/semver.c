/*
 * PostgreSQL type definitions for semver type
 * Written by Sam Vilain
 * sam@vilain.net
 *
 * Copyright 2010-2014, Sam Vilain and David Wheeler. This program is Free
 * Software; see the README.md file for the license conditions.
 */

#include "postgres.h"
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <ctype.h>
#include "utils/builtins.h"

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

/* IO methods */
Datum       semver_in(PG_FUNCTION_ARGS);
Datum       semver_out(PG_FUNCTION_ARGS);

Datum       semver_eq(PG_FUNCTION_ARGS);
Datum       hash_semver(PG_FUNCTION_ARGS);
Datum       semver_ne(PG_FUNCTION_ARGS);
Datum       semver_lt(PG_FUNCTION_ARGS);
Datum       semver_le(PG_FUNCTION_ARGS);
Datum       semver_ge(PG_FUNCTION_ARGS);
Datum       semver_gt(PG_FUNCTION_ARGS);
Datum       semver_cmp(PG_FUNCTION_ARGS);

/* these typecasts are necessary for passing to functions that take text */
Datum       text_to_semver(PG_FUNCTION_ARGS);
Datum       semver_to_text(PG_FUNCTION_ARGS);

/* this constructor gives access to the lax parsing mode */
Datum       to_semver(PG_FUNCTION_ARGS);

Datum       semver_smaller(PG_FUNCTION_ARGS);
Datum       semver_larger(PG_FUNCTION_ARGS);

/* heap format of version numbers */
typedef int32 vernum;

/* memory/heap structure (not for binary marshalling) */
typedef struct semver
{
  int32 vl_len_;  /* varlena header */
  vernum numbers[3];
  char patchname[]; /* patch name, including the null byte for convenience */
} semver;

#define PG_GETARG_SEMVER_P(n) (semver *)PG_GETARG_POINTER(n)

// forward declarations, mostly to shut the compiler up but some are
// actually necessary.
char*   emit_semver(semver* version);
semver* make_semver(const int *numbers, const char* patchname);
semver* parse_semver(char* str, bool lax);
int     patchnamecmp(const char* a, const char* b);
int     _semver_cmp(semver* a, semver* b);
char*   clean_junk(const char* str);

semver* make_semver(const int *numbers, const char* patchname) {
  int varsize = offsetof(semver, patchname) + (patchname ? strlen(patchname) : 0) + 1;
  semver *rv = palloc(varsize);
  int i;
  SET_VARSIZE(rv, varsize);
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

semver* parse_semver(char* str, bool lax) {
  int parts[] = {-1, -1, -1};
  long int num;
  int len;
  int i = 0;
  int p = 0;
  int atchar = 0;
  int curpart = 0;
  char next = NULL;
  char* patch = 0;
  char* ptr, *endptr;
  bool dotlast = false;
  bool started_prerel = false;
  bool started_meta = false;
  bool skip_char = false;
  semver* newval;

  ptr = str;
  len = strlen(str);

  do {
    next = (char)*ptr;
    skip_char = false;
    if (curpart < 3) {  // Still figuring out X.Y.Z
      if (next == '.') {  // First, check if we hit a period
        ptr++;
        atchar++;
        curpart++;
      } else {  // OK, it should be a version part number then
        num = strtol(ptr, &endptr, 10);
        if (ptr == endptr) {  // Not a number
          if (lax) {
            // Since it's not a period, we have to assume it's a legit pre-
            // related token. We'll skip to the next number part, but leave
            // the pointers.
            curpart++;
          } else {
            elog(ERROR, "bad semver value '%s': expected number/separator at char %d", str, atchar);
          }
        }
        if (num > INT_MAX)  // Too big
          elog(ERROR, "bad semver value '%s': version number exceeds 31-bit range", str);

        if (next == '0' && num != 0 && !lax)  // Leading zeros
          elog(ERROR, "bad semver value '%s': semver version numbers can't start with 0", str);

        parts[curpart] = num;
        atchar += (strlen(ptr) - strlen(endptr));
        ptr = endptr;
        if (curpart == 2)  // Break out of the `if`; we won't see another period
          curpart++;
      }
    } else {  // Onto pre-release/metadata
      if (!started_prerel && (next == '-' || (next != '+' && lax) )) {  // Starts with -
        if (started_meta)  // Pre-release flag can't come after metadata
          elog(ERROR, "bad semver value '%s': pre-release (-) after metadata (+) at char %d", str, atchar);

        started_prerel = true;
        if (next == '-') {
          skip_char = true;
        }
      } else if (!started_meta && next == '+') {  // Starts with +
        started_meta = true;
        ptr++;
        atchar++;
      }

      if (!patch && (started_meta || started_prerel))
        patch = palloc(len - atchar + 1);

      // NB: This section is here to maintain compatibility with the SEMV 1.0b
      // version of the library that accepted multiple - (dash) separators.
      // This is invalid for SEMV 1.0/2.0 so just turn them into . (period)
      if (started_prerel && next == '-')
        next = '.';

      if (!skip_char &&
          (!started_prerel && next != '-') &&
          (!started_meta && next != '+'))  {  // Didn't start with -/+
        elog(ERROR, "bad semver value '%s': expected - (dash) or + (plus) at char %d", str, atchar);
      }
      if (next == '.' && dotlast)
        elog(ERROR, "bad semver value '%s': empty pre-release section at char %d", str, atchar);

      if (!skip_char && (next != '.' && next != '+' && !isalpha(next) && !isdigit(next))) {
        if (lax && isspace(next))  // In lax mode, ignore whitespace
          skip_char = true;
        else
          elog(ERROR, "bad semver value '%s': non-alphanumeric pre-release at char %d", str, atchar);
      }
      if ((started_prerel || started_meta) && !skip_char) {
        dotlast = (next == '.');
        patch[i] = next;
        i++;
      }
      atchar++;
      ptr++;
    }
  } while (atchar < len);

  for (p=0; p < 3; p++) {
    if (parts[p] == -1) {
      if (lax)
        parts[p] = 0;
      else
        elog(ERROR, "bad semver value '%s': missing major, minor, or patch version", str);
    }
  }

  if ((started_prerel || started_meta) && i == 0)  // No pre-release value after -
    elog(ERROR, "bad semver value '%s': expected alphanumeric at char %d", str, atchar);

  if (started_prerel || started_meta)
    patch[i] = '\0';

  newval = make_semver(parts, patch);
  if (patch)
    pfree(patch);

  return newval;
}

char* emit_semver(semver* version) {
  int len;
  char tmpbuf[32];
  char *buf;

  if (*version->patchname == '\0') {
    len = snprintf(tmpbuf, sizeof(tmpbuf), "%d.%d.%d",
                   version->numbers[0],
                   version->numbers[1],
                   version->numbers[2]
                   );
  }
  else {
    len = snprintf(
                   tmpbuf, sizeof(tmpbuf),"%d.%d.%d-%s",
                   version->numbers[0],
                   version->numbers[1],
                   version->numbers[2],
                   version->patchname
                   );
  }

  /* Should cover the vast majority of cases. */
  if (len < sizeof(tmpbuf)) return pstrdup(tmpbuf);

  /* Try again, this time with the known length. */
  buf = palloc(len+1);
  if (*version->patchname == '\0') {
    len = snprintf(buf, len+1, "%d.%d.%d",
                   version->numbers[0],
                   version->numbers[1],
                   version->numbers[2]
                   );
  }
  else {
    len = snprintf(
                   buf, len+1,"%d.%d.%d-%s",
                   version->numbers[0],
                   version->numbers[1],
                   version->numbers[2],
                   version->patchname
                   );
  }
  return buf;
}

/*
 * Pg bindings
 */

/* input function: C string */
PG_FUNCTION_INFO_V1(semver_in);
Datum
semver_in(PG_FUNCTION_ARGS)
{
  char *str = PG_GETARG_CSTRING(0);
  semver *result = parse_semver(str, false);
  if (!result)
    PG_RETURN_NULL();

  PG_RETURN_POINTER(result);
}/* output function: C string */

PG_FUNCTION_INFO_V1(semver_out);
Datum
semver_out(PG_FUNCTION_ARGS)
{
  semver* amount = PG_GETARG_SEMVER_P(0);
  char *result;
  result = emit_semver(amount);

  PG_RETURN_CSTRING(result);
}

PG_FUNCTION_INFO_V1(text_to_semver);
Datum
text_to_semver(PG_FUNCTION_ARGS)
{
  text* sv = PG_GETARG_TEXT_PP(0);
  semver* rs = parse_semver(text_to_cstring(sv), false);
  PG_RETURN_POINTER(rs);
}

PG_FUNCTION_INFO_V1(semver_to_text);
Datum
semver_to_text(PG_FUNCTION_ARGS)
{
  semver* sv = PG_GETARG_SEMVER_P(0);
  char* xxx = emit_semver(sv);
  text* res = cstring_to_text(xxx);
  pfree(xxx);
  PG_RETURN_TEXT_P(res);
}

char* clean_junk(const char *str)
{
  int n = strlen(str);
  char *copy = palloc(n + 1);
  int j = 0;   // current character
  strcpy(copy, str);

  while (j < n)
    {
      /* if current character is b */
      if (str[j] == '.' || str[j] == '+')
        j++;
      else
        copy[j] = str[j++];
    }
  copy[j] = '\0';
  return copy;
}

int patchnamecmp(const char* a, const char* b)
{
  int res;
  char *ac, *bc;

  if (*a == '\0' && *b != '\0') {
    return 1;
  }
  if (*a != '\0' && *b == '\0') {
    return -1;
  }
  ac = clean_junk(a);
  bc = clean_junk(b);
  res = strcasecmp(ac, bc);
  pfree(ac);
  pfree(bc);
  return res;
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
    rv = patchnamecmp(a->patchname, b->patchname);
  }
  return rv;
}

PG_FUNCTION_INFO_V1(semver_eq);
Datum
semver_eq(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  PG_RETURN_BOOL(diff == 0);
}

PG_FUNCTION_INFO_V1(semver_ne);
Datum
semver_ne(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  PG_RETURN_BOOL(diff != 0);
}

PG_FUNCTION_INFO_V1(semver_le);
Datum
semver_le(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  PG_RETURN_BOOL(diff <= 0);
}

PG_FUNCTION_INFO_V1(semver_lt);
Datum
semver_lt(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  PG_RETURN_BOOL(diff < 0);
}

PG_FUNCTION_INFO_V1(semver_ge);
Datum
semver_ge(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  PG_RETURN_BOOL(diff >= 0);
}

PG_FUNCTION_INFO_V1(semver_gt);
Datum
semver_gt(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  PG_RETURN_BOOL(diff > 0);
}

PG_FUNCTION_INFO_V1(semver_cmp);
Datum
semver_cmp(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
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
  semver* version = PG_GETARG_SEMVER_P(0);
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
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  if (diff >= 0) {
    PG_RETURN_POINTER(a);
  }
  else {
    PG_RETURN_POINTER(b);
  }
}

PG_FUNCTION_INFO_V1(semver_smaller);
Datum
semver_smaller(PG_FUNCTION_ARGS)
{
  semver* a = PG_GETARG_SEMVER_P(0);
  semver* b = PG_GETARG_SEMVER_P(1);
  int diff = _semver_cmp(a, b);
  if (diff <= 0) {
    PG_RETURN_POINTER(a);
  }
  else {
    PG_RETURN_POINTER(b);
  }
}

PG_FUNCTION_INFO_V1(to_semver);
Datum
to_semver(PG_FUNCTION_ARGS)
{
  text* sv = PG_GETARG_TEXT_PP(0);
  semver* rs = parse_semver(text_to_cstring(sv), true);
  PG_RETURN_POINTER(rs);
}
