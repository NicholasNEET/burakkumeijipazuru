program project1;
uses sdl, sdl_mixer_nosmpeg, sdl_ttf;

{ minimum code for an sdl window.. }

var
  screen: PSDL_Surface;
  event: PSDL_Event;
  rect: PSDL_Rect;
  exit: Boolean;
begin
  new(screen);
  new(event);
  new(rect);

  SDL_Init(SDL_INIT_EVERYTHING);
  screen:= SDL_SetVideoMode(800,600,24, SDL_HWACCEL or SDL_DOUBLEBUF);

  rect^.x:= 0;
  rect^.y:= 0;
  rect^.w:= 800;
  rect^.h:= 600;

  while (not exit) do
  begin
       while (SDL_PollEvent(event) <> 0) do
       begin
          if (event^.type_ = SDL_KEYDOWN) then
          begin
               if (event^.key.keysym.sym = SDLK_ESCAPE) then
               begin
                    exit:= true;
               end;
          end;
       end;

       SDL_FillRect(screen, rect, SDL_MapRGB(screen^.format, 0,0,0));
       SDL_Flip(screen);
  end;

end.

