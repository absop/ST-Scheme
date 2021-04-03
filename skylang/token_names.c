#include "decode.h"

const char *token_names[] = {
    stringify(eof),
    stringify(atomic),
    stringify(string),
    stringify(quote),
    stringify(quasiquote),
    stringify(unquote),
    stringify(unquote_splicing),
    stringify(syntax),
    stringify(quasisyntax),
    stringify(unsyntax),
    stringify(unsyntax_splicing),
    stringify(aline_comment),
    stringify(block_comment),
    stringify(datum_comment),
    stringify(vparen),
    stringify(vnparen),
    stringify(vu8paren),
    stringify(vu8nparen),
    stringify(vfxparen),
    stringify(vfxnparen)
};


int main()
{
    for (int i = 0; i < countof(token_names); ++i) {
        printf("%s\n", token_names[i]);
    }
    return 0;
}
