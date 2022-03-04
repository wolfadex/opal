#include <stdlib.h>
#if defined(_MSC_VER)
#include "SDL.h"
#else
#include "SDL2/SDL.h"
#endif

struct FWrapper {
    int arity;

}

void f1(int arity, fun, wrapper)
{
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}

void decrement(n)
{
	return n - 1;
}
void multiply(x, y)
{
	return x * y;
}
const int one = 1;
void main()
{
	return multiply(2)(decrement(2 + 3)); }

int main()
{
    if (SDL_Init(SDL_INIT_VIDEO) != 0)
    {
        printf("Failed to initialize");
        return EXIT_FAILURE;
    }

    SDL_Window *windowPtr = SDL_CreateWindow("Hello, Decal!", 100, 100, 800, 600, SDL_WINDOW_SHOWN);
    if (windowPtr == NULL)
    {
        printf("Failed to create a window");
        SDL_Quit();
        return EXIT_FAILURE;
    }

    SDL_Renderer *rendererPtr = SDL_CreateRenderer(windowPtr, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (rendererPtr == NULL)
    {
        printf("Failed to create a renderer");
        SDL_DestroyWindow(windowPtr);
        SDL_Quit();
        return EXIT_FAILURE;
    }
    printf("Should show window for 3s");

    while (1)
    {
        SDL_Event event;

        if (SDL_PollEvent(&event))
        {
            switch (event.type)
            {
            case SDL_QUIT:
                goto quit_app;
                break;
            case SDL_KEYUP:
                switch (event.key.keysym.scancode)
                {
                case SDL_SCANCODE_ESCAPE:
                    goto quit_app;
                    break;
                default:
                    break;
                }
                break;
            default:
                break;
            }

            SDL_SetRenderDrawColor(rendererPtr, 100, 149, 237, 255);
            SDL_RenderClear(rendererPtr);
            // SDL_RenderDrawRect
            SDL_RenderPresent(rendererPtr);
        }
    }

quit_app:
    printf("Closing...");

    SDL_DestroyRenderer(rendererPtr);
    SDL_DestroyWindow(windowPtr);
    SDL_Quit();

    return EXIT_SUCCESS;
}