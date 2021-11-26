#include <janet.h>
#include <raylib.h>
#include "types.h"

typedef struct {
  int x;
  int y;
  Color color;
} Tile;

static const JanetAbstractType AT_Tile = {"freja/tile", JANET_ATEND_NAME};

typedef struct {
  Tile** tiles;
  size_t size;
  size_t used;
} TileHolder;

static const JanetAbstractType AT_TileHolder = {"freja/tile-holder", JANET_ATEND_NAME};

Janet create_tile(int32_t argc, Janet * argv) {
  Tile* box = janet_abstract(&AT_Tile, sizeof(Tile));
  box->y = janet_getinteger(argv, 0);
  box->x = janet_getinteger(argv, 1);
  box->color = jaylib_getcolor(argv, 2);
  janet_wrap_abstract(box);
}

Janet create_tile_holder(int32_t argc, Janet * argv) {
  TileHolder* box = janet_abstract(&AT_TileHolder, sizeof(TileHolder));
  int n = janet_getinteger(argv, 0);
  box->tiles = malloc(n * (sizeof(Tile*)));
  box->size = n;
  box->used = 0;
  janet_wrap_abstract(box);
}

Janet insert_tile_holder(int32_t argc, Janet * argv) {
  TileHolder* th = janet_getabstract(argv, 0, &AT_TileHolder);
  Tile* t = janet_getabstract(argv, 1, &AT_Tile);
  if ((th->used) == (th->size)) {
    {
      th->size = 2 * (th->size);
      th->tiles = realloc(th->tiles, (th->size) * (sizeof(Tile*)));
    }
  }
  Tile* t2 = malloc(sizeof(Tile));
  t2->y = t->y;
  t2->x = t->x;
  t2->color = t->color;
  (th->tiles)[th->used] = t2;
  ++(th->used);
  janet_wrap_abstract(th);
}

Janet render_tile_holder(int32_t argc, Janet * argv) {
  TileHolder* th = janet_getabstract(argv, 0, &AT_TileHolder);
  int i = 0;
  int size = 1;
  while (i < (th->used)) {
    {
      Tile* t = (th->tiles)[i];
      if (((t->x) < 1000) && ((t->y) < 1000)) {
        DrawRectangle(size * (t->x), size * (t->y), size, size, t->color);
      }
      ++(i);
    }
  }
  janet_wrap_nil();
}

Janet print_tile(int32_t argc, Janet * argv) {
  Tile* t = janet_getabstract(argv, 0, &AT_Tile);
  janet_wrap_number(t->x);
}

Janet render_tile(int32_t argc, Janet * argv) {
  Tile* t = janet_getabstract(argv, 0, &AT_Tile);
  DrawRectangle(100 * (t->x), 100 * (t->y), 100, 100, t->color);
  janet_wrap_nil();
}

Janet get_screen_width(int32_t argc, Janet * argv) {
  janet_wrap_number(GetScreenWidth());
}

static const JanetReg cfuns[] = {{"get-screen-width", get_screen_width, ""}, {"create-tile", create_tile, ""}, {"create-tile-holder", create_tile_holder, ""}, {"insert-tile-holder", insert_tile_holder, ""}, {"render-tile-holder", render_tile_holder, ""}, {"print-tile", print_tile, ""}, {"render-tile", render_tile, ""}, {NULL, NULL, NULL}};
JANET_MODULE_ENTRY(JanetTable *env) {
  janet_cfuns (env, "get-screen-width", cfuns);
}
