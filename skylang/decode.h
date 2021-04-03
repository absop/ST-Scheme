#ifndef __DECODE_H__
#define __DECODE_H__

#include <assert.h>
#include <ctype.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <fcntl.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/time.h>
#include <sys/poll.h>
#include <unistd.h>
#endif

/* Copyright Fabrice Bellard, from tcc.h */
#ifdef _WIN32
# define IS_DIRSEP(c) (c == '/' || c == '\\')
# define IS_ABSPATH(p) (IS_DIRSEP(p[0]) || \
    (p[0] && p[1] == ':' && IS_DIRSEP(p[2])))
# define PATHCMP stricmp
# define PATHSEP ";"
#else
# define IS_DIRSEP(c) (c == '/')
# define IS_ABSPATH(p) IS_DIRSEP(p[0])
# define PATHCMP strcmp
# define PATHSEP ":"
#endif

#ifdef __x86_64__
#define __BIT64__
#elif defined(__i386__)
#else
#error "unsupported machine"
#endif


#ifdef __BIT64__
typedef long long iptr_t;
typedef unsigned long long uptr_t;
typedef unsigned long long value_t;
#else
typedef long iptr_t;
typedef unsigned long uptr_t;
typedef unsigned long value_t;
#endif

#ifdef __GNUC__
#define __unlikely(x) __builtin_expect(!!(x), 0)
#define __likely(x)   __builtin_expect(!!(x), 1)
#else
#define __unlikely(x) (x)
#define __likely(x)   (x)
#endif

#define xglue(x, y) x ## y
#define glue(x, y) xglue(x, y)
#define stringify(s)    tostring(s)
#define tostring(s)     #s

#ifndef offsetof
#define offsetof(type, field) ((size_t) &((type *)0)->field)
#endif
#ifndef countof
#define countof(x) (sizeof(x) / sizeof((x)[0]))
#endif

#define TAG_atomic 0x0
#define TAG_quote  0x1
#define TAG_list   0x2

#define __ALIGN(x) __attribute__((aligned(x)))
#define tag(x) ((x)&0x7)
#define ptr(x) ((void*)((x)&(~(value_t)0x7)))
#define tagptr(p, t) (((value_t)(p))|(t))
#define taglist(p) (tagptr(p, TAG_list))
#define tagquote(p) (tagptr(p, TAG_quote))
#define tagatomic(p) (tagptr(p, TAG_atomic))
#define islist(x) (tag(x) == TAG_list)
#define isquote(x) (tag(x) == TAG_quote)
#define isatomic(x) (tag(x) == TAG_atomic)

#define TOK_quote               0x0
#define TOK_quasiquote          0x1
#define TOK_unquote             0x2
#define TOK_unquote_splicing    0x3
#define TOK_syntax              0x4
#define TOK_quasisyntax         0x5
#define TOK_unsyntax            0x6
#define TOK_unsyntax_splicing   0x7
#define TOK_cparen              0x8
#define TOK_cbrack              0x9
#define TOK_oparen              0xa
#define TOK_obrack              0xb
#define TOK_vparen              0xc
#define TOK_vu8paren            0xd
#define TOK_vfxparen            0xe
#define TOK_vnparen             0xf
#define TOK_vu8nparen           0x10
#define TOK_vfxnparen           0x11
#define TOK_atomic              0x12
#define TOK_string              0x13
#define TOK_boolean             0x14
#define TOK_character           0x15
#define TOK_aline_comment       0x17
#define TOK_block_comment       0x18
#define TOK_datum_comment       0x19
#define TOK_eof                 0x1a

typedef struct _bind bind_t;
typedef struct _dict dict_t;

typedef struct _token token_t;
typedef struct _quote quote_t;
typedef struct _list list_t;
typedef struct _symbol symbol_t;
typedef struct _bucket bucket_t;
typedef struct _oblist oblist_t;
typedef struct _istream istream_t;

struct _token {
    unsigned int type;
    unsigned int size;
    unsigned int linenum;
    unsigned int column;
    char string[0];
} __ALIGN(8);

struct _quote {
    symbol_t *sym;
    unsigned int linenum;
    unsigned int column;
} __ALIGN(8);

struct _list {
    value_t *values;
    iptr_t length;
    iptr_t nalloc;
    iptr_t veclen;
    symbol_t *opening;
    symbol_t *closing;
    unsigned int o_linenum;
    unsigned int o_column;
    unsigned int c_linenum;
    unsigned int c_column;
} __ALIGN(8);

struct _symbol {
    iptr_t hash;
    unsigned int size;
    char str[1];
};

struct _bucket {
    struct _bucket *next;
    symbol_t sym;
};

struct _oblist {
    iptr_t idxmask;
    iptr_t nbucket;
    iptr_t nentry;
    bucket_t **buckets;
};

struct _istream {
    unsigned int linenum;
    unsigned int column;

    char *filename;
    char *ptr;
    char *end;

    int fd;
    char buffer[4];
};

#define CH_EOB '\0'
#define CH_EOF (-1)
#define INFILE_BUFSIZ 8192

iptr_t hashs(const char *str, unsigned int *len);
oblist_t *make_oblist(iptr_t nbucket);
bucket_t *oblist_insert_string(oblist_t *oblist, const char *str);
int oblist_remove_symbol(oblist_t *oblist, symbol_t *sym);
int oblist_remove_string(oblist_t *oblist, const char *str);
int oblist_haskey_string(oblist_t *oblist, const char *str);
void oblist_resize(oblist_t *oblist, iptr_t new_nbucket);
void oblist_expand(oblist_t *oblist);
void oblist_shrink(oblist_t *oblist);
void free_oblist(oblist_t *oblist);

extern oblist_t *symbols;
static inline symbol_t *symbolize(const char *string)
{
    bucket_t *bucket = oblist_insert_string(symbols, string);
    return &bucket->sym;
}
static inline void unsymbolize(const char *string)
{
    oblist_remove_string(symbols, string);
}

void free_istream(istream_t *is);
void flush_file_istream(istream_t *is);
istream_t *open_file_istream(const char *filename);
istream_t *open_string_istream(const char *filename, const char *str);


void fprint_value(FILE *fp, value_t value);
void fprint_list(FILE *fp, list_t *list);

#endif /* __DECODE_H__ */
