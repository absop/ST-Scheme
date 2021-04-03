#include "decode.h"

void sky_error(char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "error: ");
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    exit(1);
    // if (s1->error_set_jmp_enabled) {
    //     longjmp(s1->error_jmp_buf, 1);
    // } else {
    //     exit(1);
    // }
}

void *sky_malloc(iptr_t size)
{
    void *ptr;
    ptr = malloc(size);
    if (!ptr) {
        sky_error("sky_malloc: out of memory!\n");
    }
    return ptr;
}

void *sky_mallocz(iptr_t size)
{
    void *ptr;
    ptr = malloc(size);
    if (!ptr) {
        sky_error("sky_mallocz: out of memory!\n");
    }
    return memset(ptr, 0, size);
}

void *sky_realloc(void *oldptr,  iptr_t size)
{
    void *ptr;
    ptr = realloc(oldptr, size);
    if (!ptr) {
        sky_error("sky_realloc: out of memory!\n");
    }
    return ptr;
}

#define MGET(n, t) ((t*)sky_malloc((n)*sizeof(t)))
#define MGETZ(n, t) ((t*)sky_mallocz((n)*sizeof(t)))
#define MNEW(oldptr, n, t) ((t*)sky_realloc(oldptr, (n)*sizeof(t)))

/* ===================== oblist_t ===================== */
oblist_t *make_oblist(iptr_t nbucket)
{
    if (nbucket <= 8) nbucket = 8;
    else {
        iptr_t size = nbucket;
        for (nbucket = 8; nbucket < size; nbucket <<= 1);
    }
    oblist_t *oblist = MGET(1, oblist_t);
    oblist->buckets = MGETZ(nbucket, bucket_t*);
    oblist->idxmask = nbucket - 1;
    oblist->nbucket = nbucket;
    oblist->nentry = 0;
    return oblist;
}

void free_oblist(oblist_t *oblist)
{
    bucket_t *b, *next;
    bucket_t **buckets = oblist->buckets;
    iptr_t nbucket = oblist->nbucket;
    for (iptr_t i = 0; i < nbucket; ++i) {
        for (b = buckets[i]; b;) {
            next = b->next;
            free(b);
            b = next;
        }
    }
    free(buckets);
    free(oblist);
}

iptr_t hashs(const char *str, unsigned int *len)
{
    iptr_t hash;
    unsigned int i;

#ifdef XOR_MUL_HASH
    hash = 2166136261;
    for (i = 0; str[i] != '\0'; ++i) {
        hash ^= str[i];
        hash *= 16777619;
    }
#else
/* TODO: 64 bit hash */
#define HASHC(h, c) ((h) + ((h) << 5) + ((h) >> 27) + (c))
    hash = 1;
    for (i = 0; str[i] != '\0'; ++i)
        hash = HASHC(hash, str[i]);
#endif
    *len = i;

    return hash;
}

int oblist_haskey_string(oblist_t *oblist, const char *str)
{
    iptr_t hash;
    unsigned int len;
    bucket_t *bucket, **pb;

    hash = hashs(str, &len);
    pb = &oblist->buckets[hash & oblist->idxmask];
    for (; (bucket = *pb) != NULL; pb = &bucket->next) {
        if (bucket->sym.hash == hash &&
            !memcmp(bucket->sym.str, str, len)) {
            return 1;
        }
    }
    return 0;
}

bucket_t *oblist_insert_string(oblist_t *oblist, const char *str)
{
    iptr_t hash;
    unsigned int len;
    bucket_t *bucket, **pb;

    hash = hashs(str, &len);
    pb = &oblist->buckets[hash & oblist->idxmask];
    for (; (bucket = *pb) != NULL; pb = &bucket->next) {
        if (bucket->sym.hash == hash &&
            !memcmp(bucket->sym.str, str, len)) {
            return bucket;
        }
    }
    oblist->nentry++;
    bucket = sky_malloc(sizeof(bucket_t) + len);
    bucket->next = NULL;
    bucket->sym.hash = hash;
    bucket->sym.size = len;
    memcpy(bucket->sym.str, str, len + 1);
    *pb = bucket;
    oblist_expand(oblist);
    return bucket;
}

int oblist_remove_string(oblist_t *oblist, const char *str)
{
    iptr_t hash;
    unsigned int len;
    bucket_t *bucket, **pb;

    hash = hashs(str, &len);
    pb = &oblist->buckets[hash & oblist->idxmask];
    for (; (bucket = *pb) != NULL; pb = &bucket->next) {
        if (bucket->sym.hash == hash &&
            !memcmp(bucket->sym.str, str, len)) {
            oblist->nentry--;
            *pb = bucket->next;
            free(bucket);
            oblist_shrink(oblist);
            return 1;
        }
    }
    return 0;
}

int oblist_remove_symbol(oblist_t *oblist, symbol_t *sym)
{
    iptr_t hash = sym->hash;
    bucket_t *bucket, **pb;

    pb = &oblist->buckets[hash & oblist->idxmask];
    for (; (bucket = *pb) != NULL; pb = &bucket->next) {
        if (&bucket->sym == sym) {
            oblist->nentry--;
            *pb = bucket->next;
            free(bucket);
            oblist_shrink(oblist);
            return 1;
        }
    }
    return 0;
}

void oblist_resize(oblist_t *oblist, iptr_t new_nbucket)
{
    iptr_t i, idx, hash;
    iptr_t new_idxmask = new_nbucket - 1, old_nbucket;
    bucket_t *b, *oldnext;
    bucket_t **old_buckets = oblist->buckets;
    bucket_t **new_buckets = MGETZ(new_nbucket, bucket_t*);

    old_nbucket = oblist->nbucket;
    oblist->nbucket = new_nbucket;
    oblist->buckets = new_buckets;
    oblist->idxmask = new_idxmask;

    for (i = 0; i < old_nbucket; ++i) {
        for (b = old_buckets[i]; b != NULL; b = oldnext) {
            oldnext = b->next;
            idx = b->sym.hash & new_idxmask;
#ifdef INSERT_HEAD_WHEN_ADJUST
            b->next = new_buckets[idx];
            new_buckets[idx] = b;
#else
            bucket_t *tail, **pb;
            for (pb = &new_buckets[idx]; (tail = *pb) != NULL;) {
                pb = &tail->next;
            }
            b->next = NULL;
            *pb = b;
#endif
        }
    }
    free(old_buckets);
}

void oblist_expand(oblist_t *oblist)
{
    iptr_t nb = oblist->nbucket;
    if (oblist->nentry <= (nb >> 1) + (nb >> 2)) /* upper limit 0.75 */
        return;
    oblist_resize(oblist, nb << 1);
}

void oblist_shrink(oblist_t *oblist)
{
    iptr_t nb = oblist->nbucket;
    if (oblist->nentry >= (nb >> 2) || nb <= 8) /* lower limit 0.25 */
        return;
    oblist_resize(oblist, nb >> 1);
}
/* ===================== oblist_t ===================== */

/* ===================== istream_t ===================== */
/* unsafe */
char *abspath(const char *path, char *buf, int bufsize)
{
    int i, c, len;
    getcwd(buf, bufsize);
    len = strlen(buf);
    int layer = 0;
    if (path[0] == '.') {
        if (IS_DIRSEP(path[1])) {
            path += 2;
        } else if (path[1] == '.' && IS_DIRSEP(path[2])) {
            path += 3;
            layer++;
        }
    }
    for (len -= 1; layer > 0; --layer) {
        for (; len >= 0; --len) {
            c = buf[len];
            if (IS_DIRSEP(c)) {
                break;
            }
        }
    }
    buf[len++] = '/';
    strcpy(&buf[len], path);

    return buf;
}

#ifdef _WIN32
static char *pathcpy(char *dst, const char *src, int size)
{
    if (size > 0) {
        char c;
        int i, len = size - 1;
        for (i = 0; i < len; ++i) {
            c = src[i];
            if (c == '\0')
                break;
            dst[i] = c != '\\' ? c : '/';
        }
        dst[i] = '\0';
    }

    return dst;
}
#else
#define pathcpy strncpy
#endif
static char *pathdup(const char *path)
{
    int size;
    char *dst, buf[1024];
    if (!(IS_ABSPATH(path))) {
        path = abspath(path, buf, sizeof(buf));
    }
    size = strlen(path) + 1;
    dst = sky_malloc(size);
    pathcpy(dst, path, size);

    return dst;
}

static istream_t *open_istream(const char *filename, int len, char endc)
{
    istream_t *is;
    is = sky_malloc(sizeof(istream_t) + len);
    is->filename = pathdup(filename);
    is->fd = -1;
    is->ptr = is->buffer;
    is->end = is->buffer + len;
    is->end[0] = endc;
    is->linenum = 1;
    is->column = 0;
    return is;
}

void free_istream(istream_t *is)
{
    if (is->fd > 0) {
        close(is->fd);
    }
    free(is->filename);
    free(is);
}

istream_t *open_string_istream(const char *filename, const char *str)
{
    int len = strlen(str);
    istream_t *is = open_istream(filename, len, CH_EOF);
    memcpy(is->buffer, str, len);
    return is;
}

istream_t *open_file_istream(const char *filename)
{
    int fd;
    if (!strcmp(filename, "-")) {
        filename = "<stdin>";
        fd = 0;
    } else {
        fd = open(filename, O_RDONLY | O_BINARY);
        if (fd < 0)
            return NULL;
    }
    istream_t *fs;
    fs = open_istream(filename, INFILE_BUFSIZ, CH_EOB);
    fs->fd = fd;
    flush_file_istream(fs);
    return fs;
}

void flush_file_istream(istream_t *is)
{
    int len = read(is->fd, is->buffer, INFILE_BUFSIZ);
    if (len < INFILE_BUFSIZ) {
        if (len < 0) len = 0;
        is->buffer[len] = CH_EOF;
    }
    is->ptr = is->buffer;
}
/* ===================== istream_t ===================== */

static istream_t *infile;
static inline int peek_char() {
    if (*infile->ptr == CH_EOB) {
        flush_file_istream(infile);
    }
    return *infile->ptr;
}
static inline int read_char() {
    if (*infile->ptr == CH_EOB) {
        flush_file_istream(infile);
    }
    return *infile->ptr++;
}
static inline void unread_char(char c) { *--infile->ptr = c; }

static struct _token_state {
    unsigned int linenum;
    unsigned int column;
    iptr_t veclen;
} token_state;

static char *tokbuf;
static int toklen, tokbuf_size;
static inline void init_tokbuf()
{
    toklen = 0;
    tokbuf = MGET(1024, char);
    tokbuf_size = 1024;
}
static inline void token_accept(char c)
{
    if (__unlikely(toklen == tokbuf_size)) {
        tokbuf_size <<= 1;
        tokbuf = MNEW(tokbuf, tokbuf_size, char);
    }
    tokbuf[toklen++] = c;
}
static inline void token_start(char c) {
    toklen = 0;
    token_accept(c);
}
static inline bool is_space(char c)
{
    return c == ' ' || c == '\t' || c == '\f' || c == '\v';
}
static inline bool is_separator(char c)
{
    switch (c) {
        case ' ':
        case '(':
        case ')':
        case '[':
        case ']':
        case '{':
        case '}':
        case '|':
        case ';':
        case '"':
        case ',':
        case '`':
        case '\'':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case '\v':
        case '\\':
            return true;
        default:
            return false;
    }
}

static void sky_token_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "read token error at:\n");
    fprintf(stderr, "%s:%u:%u\n", infile->filename,
            token_state.linenum, token_state.column);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    exit(1);
}

static inline int read_token_aline_comment(char ch)
{
    token_start(';');
    for (;;) {
        ch = read_char();
        switch (ch) {
            case CH_EOF:
                unread_char(ch);
                return TOK_aline_comment;
            case '\r': break;
            case '\n':
                infile->linenum++;
                infile->column = 0;
                return TOK_aline_comment;
            default:
                infile->column++;
                token_accept(ch);
        }
    }
}

static inline int read_token_block_comment(char ch)
{
    token_accept('|');
    int block_comment_level = 1;
    for (;;) {
        ch = read_char();
        switch(ch) {
            case CH_EOF:
                sky_token_error("unexpected end of block comment");
                break;
            case '\r': break;
            case '\n':
                infile->linenum++;
                infile->column = 0;
                token_accept('\n');
                break;
            default:
                infile->column++;
                token_accept(ch);
                if (ch == '|') {
                    ch = peek_char();
                    if (ch == '#') {
                        read_char();
                        infile->column++;
                        token_accept('#');
                        block_comment_level--;
                        if (block_comment_level == 0) {
                            return TOK_block_comment;
                        }
                    }
                } else if (ch == '#') {
                    ch = peek_char();
                    if (ch == '|') {
                        read_char();
                        infile->column++;
                        token_accept('|');
                        block_comment_level++;
                    }
                }
        }
    }
}

static inline int read_token_string(char ch)
{
    token_start('"');
    bool escape = false;
    bool skip_string_indent_space = false;
    for (;;) {
        ch = read_char();
        if (ch == CH_EOF) {
            sky_token_error("unexpected end of string");
        }
        if (__unlikely(ch == '\r')) continue;
        if (__unlikely(ch == '\n')) {
            infile->linenum++;
            infile->column = 0;
            if (escape) {
                toklen--;
                escape = false;
                skip_string_indent_space = true;
            } else {
                skip_string_indent_space = false;
                token_accept('\\');
                token_accept('n');
            }
        } else {
            infile->column++;
            if (skip_string_indent_space) {
                if (is_space(ch))
                    continue;
                else
                    skip_string_indent_space = false;
            }
            token_accept(ch);
            if (ch == '\\') {
                escape = !escape;
            } else {
                if (ch == '"' && !escape) {
                    break;
                }
                escape = false;
            }
        }
    }
    return TOK_string;
}

static inline int read_token_character(char ch)
{
    token_accept('\\');
    ch = read_char();
    switch(ch) {
        case CH_EOF:
            sky_token_error("unexpected end of reading character");
        case '\n':
            sky_token_error("invalid character");
            break;
    }

    return TOK_character;
}

static inline int read_token_sharp(char ch)
{
    token_start('#');
    ch = read_char();
    infile->column++;
    switch(ch) {
        case CH_EOF:
            sky_token_error("unexpected end of # prefix");

        case 'f': case 'F':
        case 't': case 'T':
            token_accept(ch > 'a'? ch : (ch + 'a' - 'A'));
            ch = peek_char();
            if (is_separator(ch))
                return TOK_boolean;
            sky_token_error("invalid delimiter %c for boolean", ch);
            break;

        case '0'...'9':
            break;

        case '(': return TOK_vparen;
        case '!': break;
        case '|': return read_token_block_comment(ch);
        case '\\': return read_token_character(ch);
        case '\'': return TOK_syntax;
        case '`': return TOK_quasisyntax;
        case ',':
            ch = peek_char();
            if (ch == '@') {
                read_char();
                infile->column++;
                return TOK_unsyntax_splicing;
            } else if (ch == CH_EOF) {
                return TOK_unsyntax;
            }
        case ';': return TOK_datum_comment;

        default:
            sky_token_error("invalid sharp-sign prefix #%c", ch);
    }
}

static inline int read_token_atomic(char ch)
{
    token_start(ch);
    for (;;) {
        ch = read_char();
        if (__unlikely(ch == '\r')) continue;
        if (__unlikely(ch == '\n')) {
            infile->linenum++;
            infile->column = 0;
            break;
        }
        if (!is_separator(ch)) {
            infile->column++;
            token_accept(ch);
        } else {
            unread_char(ch);
            break;
        }
    }
    return TOK_atomic;
}

/*
static inline int read_token_bar(char ch)
{
    return TOK_absolute;
}
*/

int read_token()
{
    char ch;
    for (;;) {
        ch = read_char();
        if (__unlikely(ch == '\r')) continue;
        if (__unlikely(ch == '\n')) {
            infile->linenum++;
            infile->column = 0;
            continue;
        }

        infile->column++;
        if (is_space(ch)) {
            continue;
        }

        token_state.linenum = infile->linenum;
        token_state.column = infile->column;
        switch (ch) {
            case CH_EOF:
                return TOK_eof;

            case '(': return TOK_oparen;
            case '[': return TOK_obrack;
            case ')': return TOK_cparen;
            case ']': return TOK_cbrack;

            case ',':
                ch = peek_char();
                if (ch == '@') {
                    read_char();
                    infile->column++;
                    return TOK_unquote_splicing;
                } else {
                    return TOK_unquote;
                }
            case '\'': return TOK_quote;
            case '`': return TOK_quasiquote;
            case '"': return read_token_string(ch);
            case ';': return read_token_aline_comment(ch);
            case '#': return read_token_sharp(ch);
            // case '|': return read_token_bar(ch);

            default:
                return read_token_atomic(ch);
        }
    }
}

oblist_t *symbols;
symbol_t *brackets[TOK_vfxnparen + 1];
symbol_t *quotes[TOK_unsyntax_splicing + 1];
symbol_t *sky_true, *sky_false;
static inline void init_common_symbols()
{
    symbols = make_oblist(1024);
    sky_true = symbolize("#t");
    sky_false = symbolize("#f");
    brackets[TOK_oparen] = symbolize("(");
    brackets[TOK_cparen] = symbolize(")");
    brackets[TOK_obrack] = symbolize("[");
    brackets[TOK_cbrack] = symbolize("]");
    brackets[TOK_vparen] = symbolize("'#(");
    brackets[TOK_vu8paren] = symbolize("#vu8(");
    brackets[TOK_vfxparen] = symbolize("#vfx(");
#ifdef __BIT64__
    brackets[TOK_vnparen]   = symbolize("'#%lld(");
    brackets[TOK_vu8nparen] = symbolize("#%lldvu8(");
    brackets[TOK_vfxnparen] = symbolize("#%lldvfx(");
#else
    brackets[TOK_vnparen]   = symbolize("'#%ld(");
    brackets[TOK_vu8nparen] = symbolize("#%ldvu8(");
    brackets[TOK_vfxnparen] = symbolize("#%ldvfx(");
#endif
    quotes[TOK_quote]             = symbolize("'");
    quotes[TOK_quasiquote]        = symbolize("`");
    quotes[TOK_unquote]           = symbolize(",");
    quotes[TOK_unquote_splicing]  = symbolize(",@");
    quotes[TOK_syntax]            = symbolize("#'");
    quotes[TOK_quasisyntax]       = symbolize("#`");
    quotes[TOK_unsyntax]          = symbolize("#,");
    quotes[TOK_unsyntax_splicing] = symbolize("#,@");
}

quote_t *make_quote(int tok)
{
    quote_t *quote = (quote_t *)sky_malloc(sizeof(quote_t));
    quote->sym = quotes[tok];
    quote->linenum = token_state.linenum;
    quote->column = token_state.column;
    return quote;
}

token_t *make_token(int type)
{
    token_t *token = (token_t *)sky_malloc(sizeof(token_t) + toklen + 1);
    memcpy(token->string, tokbuf, toklen);
    token->string[toklen] = '\0';
    token->size = toklen;
    token->type = type;
    token->linenum = token_state.linenum;
    token->column = token_state.column;
    return token;
}
/* ===================== list_t ===================== */
list_t *make_list(symbol_t *opening, symbol_t *closing)
{
    list_t *list = MGET(1, list_t);
    list->opening = opening;
    list->closing = closing;
    list->values = NULL;
    list->nalloc = 0;
    list->length = 0;
    list->veclen = token_state.veclen;
    list->o_linenum = token_state.linenum;
    list->o_column = token_state.column;
    return list;
}

void free_list(list_t *list)
{
    if (list->values)
        free(list->values);
    free(list);
}

void list_append(list_t *list, value_t value)
{
    unsigned int length = list->length++;
    unsigned int nalloc = list->nalloc;
    if (__unlikely(length >= nalloc)) {
        list->nalloc = nalloc == 0 ? 4 : nalloc << 1;
        list->values = MNEW(list->values, list->nalloc, value_t);
    }
    list->values[length] = value;
}
/* ===================== list_t ===================== */

static list_t **stack;
int sp = -1;  /* stack pointer */
int ss = 0;   /* stack size */
#define INIT_STACK(n) do { ss = (n); stack = MGET(n, list_t*); } while(0)
#define STACK_EMPTY() (sp < 0)
#define TOP()   (stack[sp])
#define POP()   (stack[sp--])
#define PUSH(v) do {                     \
    ++sp;                                \
    if (__unlikely(sp >= ss)) {          \
        ss <<= 1;                        \
        list_t **ns = MGET(ss, list_t*); \
        free(stack);                     \
        stack = ns;                      \
    }                                    \
    stack[sp] = (v);                     \
} while(0)

list_t *sky_parse()
{
    int tok, cb_tok;
    list_t *list;

    init_tokbuf();
    INIT_STACK(256);
    PUSH(make_list(NULL, NULL));

    for (;;) {
        tok = read_token();
        switch (tok) {
            case TOK_eof:
                goto read_done;
            case TOK_atomic:
            case TOK_string:
            case TOK_boolean:
            case TOK_character:
            case TOK_aline_comment:
            case TOK_block_comment:
            case TOK_datum_comment:
                tokbuf[toklen] = '\0';
                // printf("[atomic](%u:%u): %s\n",
                //        token_state.linenum, token_state.column,
                //        tokbuf);
                list_append(TOP(), tagatomic(make_token(tok)));
                break;

            case TOK_quote:
            case TOK_quasiquote:
            case TOK_unquote:
            case TOK_unquote_splicing:
            case TOK_syntax:
            case TOK_quasisyntax:
            case TOK_unsyntax:
            case TOK_unsyntax_splicing:
                // printf("[quote](%u:%u): %s\n",
                //        token_state.linenum, token_state.column,
                //        quotes[tok]->str);
                list_append(TOP(), tagquote(make_quote(tok)));
                break;

            case TOK_oparen:
            case TOK_obrack:
            case TOK_vparen:
            case TOK_vu8paren:
            case TOK_vfxparen:
            case TOK_vnparen:
            case TOK_vu8nparen:
            case TOK_vfxnparen:
                // printf("[opening](%u:%u): %s\n",
                //        token_state.linenum, token_state.column,
                //        brackets[tok]->str);
                cb_tok = tok == TOK_obrack ? TOK_cbrack : TOK_cparen;
                PUSH(make_list(brackets[tok], brackets[cb_tok]));
                break;

            case TOK_cparen:
            case TOK_cbrack:
                // printf("[closing](%u:%u): %s\n",
                //        token_state.linenum, token_state.column,
                //        brackets[tok]->str);
                if (!STACK_EMPTY() &&
                    (list = POP(), list->closing == brackets[tok])) {
                    list->c_linenum = token_state.linenum;
                    list->c_column = token_state.column;
                    list_append(TOP(), taglist(list));
                } else {
                    sky_token_error("unmatched brackets");
                }
                break;
        }
    }

read_done:
    list = POP();
    if (!STACK_EMPTY()) {
        sky_error("parse error");
    }
    return list;
}


void fprint_value(FILE *fp, value_t value)
{
    if (isatomic(value)) {
        token_t *token = ptr(value);
        fprintf(fp, "[%u:(%u, %u)], Token(type=%s, text=%s)\n",
                token->linenum, token->column,
                token->column + token->size,
                (token->type == TOK_block_comment ? "bcmt" :
                 token->type == TOK_aline_comment ? "lcmt" :
                 token->type == TOK_datum_comment ? "dcmt" :
                 token->type == TOK_string ? "str" :
                 token->type == TOK_character ? "char" :
                 token->type == TOK_boolean? "bool": "atomic"),
                token->string);
    } else if (isquote(value)) {
        quote_t *quote = ptr(value);
        fprintf(fp, "[%u:(%u, %u)], Token(type=quote, text=\"%s\")\n",
                quote->linenum, quote->column,
                quote->column + quote->sym->size, quote->sym->str);
    } else if (islist(value)) {
        fprint_list(fp, ptr(value));
    }
}

void fprint_list(FILE *fp, list_t *list)
{
    symbol_t *opening = list->opening;
    symbol_t *closing = list->closing;
    if (opening) {
        char buf[32];
        if (list->veclen > 0)
            sprintf(buf, opening->str, list->veclen);
         else
            sprintf(buf, opening->str);
        fprintf(fp, "[%u:(%u, %u)], Token(type=obrk, text=\"%s\")\n",
                list->o_linenum, list->o_column,
                list->o_column + opening->size, buf);
    }
    if (list->length > 0) {
        for (int i = 0; i < list->length; ++i) {
            fprint_value(fp, list->values[i]);
        }
    }
    if (closing) {
        fprintf(fp, "[%u:(%u, %u)], Token(type=cbrk, text=\"%s\")\n",
                list->c_linenum, list->c_column,
                list->c_column + closing->size, closing->str);
    }
}

static void sky_open_file_error(const char *path)
{
    fprintf(stderr, "Can not open file %s:\n", path);
    exit(1);
}

int main(int argc, char *argv[])
{

    FILE *fp;
    list_t *list;
    char path[256];
    const char *test_files[] = {
        "test",
        // "read",
        // "while-loop",
        // "cmacros",
        // "pretty",
        // "syntax",
        // "cpnanopass",
        // "prettytest"
    };

    init_common_symbols();

    for (int i = 0; i < countof(test_files); ++i) {
        sprintf(path, "../test/%s.ss", test_files[i]);
        infile = open_file_istream(path);
        if (infile == NULL) {
            sky_open_file_error(path);
        }

        list = sky_parse();
        if (list) {
            fprintf(stdout, "%s: %s\n", path, "parse done");

            sprintf(path, "../test/%s.token.c", test_files[i]);
            fp = fopen(path, "w+");
            if (fp == NULL) {
                sky_open_file_error(path);
            }
            fprint_list(fp, list);
            fclose(fp);
            fprintf(stdout, "write file: %s\n", path);

        } else {
            fprintf(stderr, "%s: %s\n", path, "parse error");
        }

        free_istream(infile);
    }

    return 0;
}
