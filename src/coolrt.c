#include "coolrt.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* This file provides the runtime library for cool. It implements
   the functions of the cool classes in C
   */

/* Class vtable prototypes */


/* ADD CODE HERE FOR MORE VTABLE PROTOTYPES */

// All of them are defined exactly as the reference binary
// use extern as they are already defined in generated program
extern const Object_vtable Object_vtable_prototype;
extern const Int_vtable Int_vtable_prototype;
extern const String_vtable String_vtable_prototype;
extern const IO_vtable IO_vtable_prototype;
extern const Bool_vtable Bool_vtable_prototype;


/*
// Methods in class object (only some are provided to you)
*/
Object *Object_abort(Object *self) {
    printf("Abort called from class %s\n",
           !self ? "Unknown" : self->vtblptr->name);
    exit(1);
    return self;
}

const String *Object_type_name(Object *self) {
    if (self == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
        abort();
    }
    String *s = String_new();
    s->val = self->vtblptr->name;
    return s;
}


/* ADD CODE HERE FOR MORE METHODS OF CLASS OBJECT */


/*
// Methods in class IO (only some are provided to you)
*/

IO *IO_out_string(IO *self, String *x) {
    if (self == 0 || x == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
        abort();
    }
    printf("%s", x->val);
    return self;
}

IO *IO_out_int(IO *self, int x) {
    if (self == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__): NULL object\n");
        abort();
    }
    printf("%d", x);
    return self;
}


/*
 * Get one line from stream using get_line(), then discard newline character.
 * Allocate string *in_string_p and store result there.
 * Return number of chars read.
 */
static int get_one_line(char **in_string_p, FILE *stream) {
    /* Get one line worth of input */
    size_t len = 0;
    ssize_t num_chars_read;
    num_chars_read = getline(in_string_p, &len, stdin);
    if (*in_string_p == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
        fprintf(stderr, "    allocation failed in IO::in_string()\n");
        exit(1);
    }

    /* Discard the newline char, if any.  It may not exist if EOF reached. */
    if (num_chars_read > 0 && (*in_string_p)[num_chars_read - 1] == '\n') {
        (*in_string_p)[num_chars_read - 1] = '\0';
        --len;
    }

    return len;
}

/*
 * The method IO::in_string(): String reads a string from
 * the standard input, up to but not including a newline character.
 */
String *IO_in_string(IO *self) {
    if (self == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
        abort();
    }

    /* Get one line worth of input with the newline, if any, discarded */
    char *in_string = 0;
    ssize_t len = get_one_line(&in_string, stdin);
    assert(in_string);

    /* We can take advantage of knowing the internal layout of String objects */
    String *str = String_new();
    str->val = in_string;
    return str;
}

/*
 * The method IO::in_int(): Int reads a single integer, which may be preceded
 * by whitespace.
 * Any characters following the integer, up to and including the next newline,
 * are discarded by in_int.
 */
int IO_in_int(IO *self) {
    if (self == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__): self is NULL\n");
        abort();
    }

    /* Get one line worth of input with the newline, if any, discarded */
    char *in_string = 0;
    ssize_t len = get_one_line(&in_string, stdin);
    assert(in_string);

    /* Now extract initial int and ignore the rest of the line */
    int x;
    int num_ints = 0;
    if (len)
        num_ints = sscanf(in_string, " %d", &x); /* Discards initial spaces*/

    /* If no text found, abort. */
    if (num_ints == 0) {
        fprintf(stderr, "At __FILE__(line __LINE__):\n   ");
        fprintf(stderr, "    Invalid integer on input in IO::in_int()");
        Object_abort((Object *) self);
    }
    return x;
}


/* ADD CODE HERE FOR MORE METHODS OF CLASS IO */


/* ADD CODE HERE FOR METHODS OF OTHER BUILTIN CLASSES */

Object *Object_copy(Object *self) {
    // malloc a new piece of memory according to self's type
    Object *copied = malloc(self->vtblptr->size);

    memcpy(copied, self, self->vtblptr->size);
    return copied;
}

Object *Object_new(void) {
    Object *result = malloc(sizeof(Object));
    result->vtblptr = &Object_vtable_prototype;
    return result;
}

IO *IO_new(void) {
    IO *result = malloc(sizeof(IO));
    result->vtblptr = &IO_vtable_prototype;
    return result;
}

Int *Int_new(void) {
    Int *result = malloc(sizeof(Int));
    //init it to 0
    result->val = 0;
    result->vtblptr = &Int_vtable_prototype;
    return result;
}

Bool *Bool_new(void) {
    Bool *result = malloc(sizeof(Bool));
    //init it to false
    result->val = false;
    result->vtblptr = &Bool_vtable_prototype;
    return result;
}

String *String_new(void) {
    String *result = malloc(sizeof(String));
    //init it to empty str
    result->val = "";
    result->vtblptr = &String_vtable_prototype;
    return result;
}

int String_length(String *s) {
    return (int) strlen(s->val);
}

String *String_concat(String *self, String *s) {
    char *s1 = self->val;
    char *s2 = s->val;

    int strlen1 = strlen(s1);
    int strlen2 = strlen(s2);
    char *result = malloc(strlen1 + strlen2 + 1);

    strncpy(result, s1, strlen1);
    strncpy(result + strlen1, s2, strlen2);
    result[strlen1 + strlen2] = '\0';

    //generate a new string object and copy things into it;
    String *strObj = String_new();
    strObj->vtblptr = self->vtblptr;
    strObj->val = result;
    return strObj;
}

String *String_substr(String *x, int i, int l) {
    char *wholeStr = x->val;
    size_t length = strlen(wholeStr);

    // error if out of boundry
    assert(length >= (i + l));

    char *result = malloc(l + 1);
    strncpy(result, wholeStr + i, l);
    result[l] = '\0';

    //generate a new string object and copy things into it;
    String *subStrObj = String_new();
    subStrObj->vtblptr = x->vtblptr;
    subStrObj->val = result;
    return subStrObj;
}

void Int_init(Int * obj, int val){
    obj->val = val;
}

void Bool_init(Bool * obj, bool val){
    obj->val = val;
}
