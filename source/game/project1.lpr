program project1;
uses sdl, sdl_mixer_nosmpeg, sdl_ttf, sdl_gfx, burakku;

begin
  burakku.LoadImages;
  level:= TGameLevel.Create;

  new(screen);
  new(event);
  new(rect);
  new(fpsman);

  SDL_Init(SDL_INIT_EVERYTHING);
  screen:= SDL_SetVideoMode(800, 600,24, SDL_HWACCEL or SDL_DOUBLEBUF);
  SDL_EnableKeyRepeat(1, 150);
  SDL_initFramerate(fpsman);
  SDL_setFramerate(fpsman, 30);

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
               level.HandleInput;
          end;
       end;

       SDL_FillRect(screen, rect, SDL_MapRGB(screen^.format, 0,0,0));

       level.Update;
       level.Draw(screen);

       SDL_Flip(screen);
       SDL_framerateDelay(fpsman);
  end;

  SDL_FreeSurface(screen);
end.

