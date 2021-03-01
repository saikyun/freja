#include <janet.h>

static Janet cfun_backward_lines_until_limit(int32_t argc, Janet *argv) {
  janet_fixarity(argc, 7);
  JanetArray *lines = janet_getarray(argv, 0);
  JanetTable *sizes = janet_gettable(argv, 1);
  int32_t width = janet_getinteger(argv, 2);
  int32_t top_y = janet_getinteger(argv, 3);
  int32_t x = janet_getinteger(argv, 4);
  int32_t y = janet_getinteger(argv, 5);
  JanetBuffer *chars = janet_getbuffer(argv, 6);

  //janet_array_push(lines, janet_wrap_integer(chars->count));

  int should_be_wrapped = 0;

  int needs_wrapping = -1;

  int32_t h = janet_unwrap_integer(janet_unwrap_tuple(janet_table_get(sizes, janet_wrap_integer('a')))[1]);

  int i = chars->count - 1;
  
  for (; i >= 0; i--) {
    uint8_t c = chars->data[i];
    if ('\n' == c) {
      y -= h;

      if (y < top_y) {
	break;
      }
      
      if (should_be_wrapped == 1) {
	janet_array_push(lines, janet_wrap_integer(needs_wrapping));
	should_be_wrapped = 0;
      }
      
      x = width;
      janet_array_push(lines, janet_wrap_integer(i + 1));
    } else {
      Janet v = janet_table_get(sizes, janet_wrap_integer(c));
      if (janet_checktype(v, JANET_NIL)) {
	janet_panicf("GOT NIL! for %d", c);
      }
      
      const Janet *t = janet_unwrap_tuple(v);
      int32_t new_x = x - janet_unwrap_integer(t[0]);
      
      if (new_x <= 0) {
	y -= h;
	should_be_wrapped = 1;
	x = width;
      } else {
	x = new_x;
      }
    }
  }

  if (should_be_wrapped == 1) {
    janet_array_push(lines, janet_wrap_integer(needs_wrapping));
    should_be_wrapped = 0;
  }

  
  if (i == -1) {
    janet_array_push(lines, janet_wrap_integer(0));
  }
  
  return janet_wrap_array(lines);
}

static const JanetReg cfuns[] = {
  {"backward-lines-until-limit", cfun_backward_lines_until_limit, "(yeboi)."},
  {NULL, NULL, NULL}
};

JANET_MODULE_ENTRY(JanetTable *env) {
  janet_cfuns (env, "text-rendering", cfuns);
}
