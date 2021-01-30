/*******************************************************************************************
*
*   raylib [text] example - Font filters
*
*   After font loading, font texture atlas filter could be configured for a softer
*   display of the font when scaling it to different sizes, that way, it's not required
*   to generate multiple fonts at multiple sizes (as long as the scaling is not very different)
*
*   This example has been created using raylib 1.3.0 (www.raylib.com)
*   raylib is licensed under an unmodified zlib/libpng license (View raylib.h for details)
*
*   Copyright (c) 2015 Ramon Santamaria (@raysan5)
*
********************************************************************************************/

// Loaded Font

#include <stdlib.h>                     // Required for: exit()
#include <stdio.h>                      // Required for: vprintf()
#include <stdarg.h>                     // Required for: va_list, va_start(), va_end()
#include <string.h>                     // Required for: strcpy(), strcat()

#include "raylib.h"
#include "rlgl.h"


#if defined(GRAPHICS_API_OPENGL_33)
    #if defined(__APPLE__)
        #include <OpenGL/gl3.h>         // OpenGL 3 library for OSX
        #include <OpenGL/gl3ext.h>      // OpenGL 3 extensions library for OSX
    #else
        #define GLAD_REALLOC RL_REALLOC
        #define GLAD_FREE RL_FREE

        #define GLAD_IMPLEMENTATION
        #if defined(RLGL_STANDALONE)
            #include "glad.h"           // GLAD extensions loading library, includes OpenGL headers
        #else
            #include "external/glad.h"  // GLAD extensions loading library, includes OpenGL headers
        #endif
    #endif
#endif


#define STB_TRUETYPE_IMPLEMENTATION
#define STB_RECT_PACK_IMPLEMENTATION
#include "stb_rect_pack.h"
#include "stb_truetype.h"

#define GL_FRAMEBUFFER_SRGB_EXT           0x8DB9

#define SIZE_X  1024
#define SIZE_Y  768

stbtt_packedchar chardata[6][128];

int sx=SIZE_X, sy=SIZE_Y;

#define BITMAP_W 512
#define BITMAP_H 512
unsigned char temp_bitmap[BITMAP_W][BITMAP_H];
unsigned char ttf_buffer[1 << 25];

GLuint font_tex;

float scale[2] = { 24.0f, 14.0f };

int sf[6] = { 0,1,2, 0,1,2 };

    Texture2D texture = { 0 };


// Draw a part of a texture (defined by a rectangle) with 'pro' parameters
// NOTE: origin is relative to destination rectangle size
void DrawTexturePro2(Rectangle source, Rectangle dest, Vector2 origin, float rotation, Color tint)
{
    // Check if texture is valid
    if (texture.id > 0)
    {
        float width = (float)texture.width;
        float height = (float)texture.height;

        bool flipX = false;

        if (source.width < 0) { flipX = true; source.width *= -1; }
        if (source.height < 0) source.y -= source.height;

        rlEnableTexture(texture.id);

	        rlPushMatrix();
            rlTranslatef(dest.x, dest.y, 0.0f);
            rlRotatef(rotation, 0.0f, 0.0f, 1.0f);
            rlTranslatef(-origin.x, -origin.y, 0.0f);

            rlBegin(RL_QUADS);
                rlColor4ub(tint.r, tint.g, tint.b, tint.a);
                rlNormal3f(0.0f, 0.0f, 1.0f);                          // Normal vector pointing towards viewer

                // Bottom-left corner for texture and quad
                if (flipX) rlTexCoord2f((source.x + source.width)/width, source.y/height);
                else rlTexCoord2f(source.x/width, source.y/height);
                rlVertex2f(0.0f, 0.0f);

                // Bottom-right corner for texture and quad
                if (flipX) rlTexCoord2f((source.x + source.width)/width, (source.y + source.height)/height);
                else rlTexCoord2f(source.x/width, (source.y + source.height)/height);
                rlVertex2f(0.0f, dest.height);

                // Top-right corner for texture and quad
                if (flipX) rlTexCoord2f(source.x/width, (source.y + source.height)/height);
                else rlTexCoord2f((source.x + source.width)/width, (source.y + source.height)/height);
                rlVertex2f(dest.width, dest.height);

                // Top-left corner for texture and quad
                if (flipX) rlTexCoord2f(source.x/width, source.y/height);
                else rlTexCoord2f((source.x + source.width)/width, source.y/height);
                rlVertex2f(dest.width, 0.0f);
            rlEnd();
	            rlPopMatrix();

        rlDisableTexture();
    }
}

void load_fonts(void)
{
   stbtt_pack_context pc;
   int i;
   FILE *f;
   char filename[256];
   
   f = fopen("Monaco.ttf", "rb");
   if (!f) {
     exit(0);
   }

   fread(ttf_buffer, 1, 1<<25, f);

   
   stbtt_PackBegin(&pc, temp_bitmap[0], BITMAP_W, BITMAP_H, 0, 1, NULL);
   for (i=0; i < 2; ++i) {
      stbtt_PackSetOversampling(&pc, 1, 1);
      stbtt_PackFontRange(&pc, ttf_buffer, 0, scale[i], 32, 95, chardata[i*3+0]+32);
      stbtt_PackSetOversampling(&pc, 2, 2);
      stbtt_PackFontRange(&pc, ttf_buffer, 0, scale[i], 32, 95, chardata[i*3+1]+32);
      stbtt_PackSetOversampling(&pc, 3, 1);
      stbtt_PackFontRange(&pc, ttf_buffer, 0, scale[i], 32, 95, chardata[i*3+2]+32);
   }
   stbtt_PackEnd(&pc);

   glGenTextures(1, &font_tex);
   glBindTexture(GL_TEXTURE_2D, font_tex);
   glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, BITMAP_W, BITMAP_H, 0, GL_ALPHA, GL_UNSIGNED_BYTE, temp_bitmap);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   
   texture.id = font_tex;
   texture.width = BITMAP_W;
   texture.height = BITMAP_H;
   texture.mipmaps = 1;
   //   texture.format = UNCOMPRESSED_R8G8B8;//UNCOMPRESSED_R8G8B8A8;
}


void drawBoxTC(float x0, float y0, float x1, float y1, float s0, float t0, float s1, float t1)
{
   rlTexCoord2f(s0,t0); rlVertex2f(x0,y0);
   rlTexCoord2f(s1,t0); rlVertex2f(x1,y0);
   rlTexCoord2f(s1,t1); rlVertex2f(x1,y1);
   rlTexCoord2f(s0,t1); rlVertex2f(x0,y1);
}

int integer_align;

void print(float x, float y, int font, char *text)
{
  rlEnableTexture(font_tex);

          rlPushMatrix();

	  
    rlBegin(RL_QUADS);
   while (*text) {
      stbtt_aligned_quad q;
      stbtt_GetPackedQuad(chardata[font], BITMAP_W, BITMAP_H, *text++, &x, &y, &q, font ? 0 : integer_align);
          drawBoxTC(q.x0,q.y0,q.x1,q.y1, q.s0,q.t0,q.s1,q.t1);
   }
   rlEnd();

        rlPopMatrix();

        rlDisableTexture();
}

int x = 0;

int main(void)
{
 
    // Initialization
    //--------------------------------------------------------------------------------------
    const int screenWidth = 800;
    const int screenHeight = 450;

    InitWindow(screenWidth, screenHeight, "raylib [text] example - font filters");

    load_fonts();

    const char msg[50] = "Loaded Font";

    // NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

    // TTF Font loading with custom generation parameters
    Font font = LoadFontEx("resources/KAISG.ttf", 14, 0, 0);

    // Generate mipmap levels to use trilinear filtering
    // NOTE: On 2D drawing it won't be noticeable, it looks like FILTER_BILINEAR
    GenTextureMipmaps(&font.texture);

    float fontSize = font.baseSize;
    Vector2 fontPosition = { 40, screenHeight/2 - 80 };
    Vector2 textSize = { 0.0f, 0.0f };

    // Setup texture scaling filter
    SetTextureFilter(font.texture, FILTER_POINT);
    int currentFontFilter = 0;      // FILTER_POINT

    SetTargetFPS(60);               // Set our game to run at 60 frames-per-second
    //--------------------------------------------------------------------------------------

    // Main game loop
    while (!WindowShouldClose())    // Detect window close button or ESC key
    {
        // Update
        //----------------------------------------------------------------------------------
      fontSize += GetMouseWheelMove()*4.0f;

      x += GetMouseWheelMove();

        // Choose font texture filter method
        if (IsKeyPressed(KEY_ONE))
        {
            SetTextureFilter(font.texture, FILTER_POINT);
            currentFontFilter = 0;
        }
        else if (IsKeyPressed(KEY_TWO))
        {
            SetTextureFilter(font.texture, FILTER_BILINEAR);
            currentFontFilter = 1;
        }
        else if (IsKeyPressed(KEY_THREE))
        {
            // NOTE: Trilinear filter won't be noticed on 2D drawing
            SetTextureFilter(font.texture, FILTER_TRILINEAR);
            currentFontFilter = 2;
        }

        textSize = MeasureTextEx(font, msg, fontSize, 0);

        if (IsKeyDown(KEY_LEFT)) fontPosition.x -= 10;
        else if (IsKeyDown(KEY_RIGHT)) fontPosition.x += 10;

        // Load a dropped TTF file dynamically (at current fontSize)
        if (IsFileDropped())
        {
            int count = 0;
            char **droppedFiles = GetDroppedFiles(&count);

            // NOTE: We only support first ttf file dropped
            if (IsFileExtension(droppedFiles[0], ".ttf"))
            {
                UnloadFont(font);
                font = LoadFontEx(droppedFiles[0], fontSize, 0, 0);
                ClearDroppedFiles();
            }
        }
        //----------------------------------------------------------------------------------

        // Draw
        //----------------------------------------------------------------------------------

	BeginDrawing();



            ClearBackground(GREEN);

            DrawTextEx(font, msg, fontPosition, fontSize, 0, BLACK);

	    /*
            DrawText("Use mouse wheel to change font size", 20, 20, 10, GRAY);
            DrawText("Use KEY_RIGHT and KEY_LEFT to move text", 20, 40, 10, GRAY);
            DrawText("Use 1, 2, 3 to change texture filter", 20, 60, 10, GRAY);
            DrawText("Drop a new TTF font for dynamic loading", 20, 80, 10, DARKGRAY);

	    
            // TODO: It seems texSize measurement is not accurate due to chars offsets...
            //DrawRectangleLines(fontPosition.x, fontPosition.y, textSize.x, textSize.y, RED);

            DrawRectangle(0, screenHeight - 80, screenWidth, 80, LIGHTGRAY);
            DrawText(TextFormat("Font size: %02.02f", fontSize), 20, screenHeight - 50, 10, DARKGRAY);
            DrawText(TextFormat("Text size: [%02.02f, %02.02f]", textSize.x, textSize.y), 20, screenHeight - 30, 10, DARKGRAY);
            DrawText("CURRENT TEXTURE FILTER:", 250, 400, 20, GRAY);

            if (currentFontFilter == 0) DrawText("POINT", 570, 400, 20, BLACK);
            else if (currentFontFilter == 1) DrawText("BILINEAR", 570, 400, 20, BLACK);
            else if (currentFontFilter == 2) DrawText("TRILINEAR", 570, 400, 20, BLACK);
	    */


	    DrawTexturePro2((Rectangle){0, 0, 2*BITMAP_W, 2*BITMAP_H},
			    (Rectangle){10, 10, BITMAP_W-10, BITMAP_H-200},
			    (Vector2){0, 0}, 0, WHITE);
	    

      rlColor3f(1,0,1);

      print(10, 10, 0, "wat");
      print(20, 20, 1, "heoatnush etnsoahuntseoah unsteohansuhensoat");

            	    print(-200, 54320, 1, "heoatnush etnsoahuntseoah unsteohansuhensoat");

      	    print(x, x, 2, "hello");
        EndDrawing();
        //----------------------------------------------------------------------------------
    }

    // De-Initialization
    //--------------------------------------------------------------------------------------
    ClearDroppedFiles();        // Clear internal buffers

    UnloadFont(font);           // Font unloading

    CloseWindow();              // Close window and OpenGL context
    //--------------------------------------------------------------------------------------

    return 0;
}
