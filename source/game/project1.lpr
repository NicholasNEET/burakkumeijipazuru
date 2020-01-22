program project1;
uses sdl, sdl_mixer_nosmpeg, sdl_ttf, burakku;

var
  screen: PSDL_Surface;
  event: PSDL_Event;
  rect: PSDL_Rect;
  exit: Boolean;

  i: Integer;
  bimage: burakku.TImage;
  bimage_dstrect: PSDL_Rect;

begin
  burakku.LoadImages;

  new(screen);
  new(event);
  new(rect);

  new(bimage_dstrect);

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

       for i:= 0 to burakku.images_tile.Count -1 do
       begin
          bimage_dstrect^.w:=32;
          bimage_dstrect^.h:=32;
          bimage_dstrect^.x:= 0;
          bimage_dstrect^.y:= i*32;
          bimage:= (burakku.images_tile.Items[i] as burakku.TImage);
          SDL_BlitSurface(bimage.GetSurface, nil, screen, bimage_dstrect);
       end;
       for i:= 0 to burakku.images_player.Count -1 do
       begin
          bimage_dstrect^.w:=32;
          bimage_dstrect^.h:=32;
          bimage_dstrect^.x:= 32;
          bimage_dstrect^.y:= i*32;
          bimage:= (burakku.images_player.Items[i] as burakku.TImage);
          SDL_BlitSurface(bimage.GetSurface, nil, screen, bimage_dstrect);
       end;
       for i:= 0 to burakku.images_object.Count -1 do
       begin
          bimage_dstrect^.w:=32;
          bimage_dstrect^.h:=32;
          bimage_dstrect^.x:= 64;
          bimage_dstrect^.y:= i*32;
          bimage:= (burakku.images_object.Items[i] as burakku.TImage);
          SDL_BlitSurface(bimage.GetSurface, nil, screen, bimage_dstrect);
       end;

       SDL_Flip(screen);
  end;

  SDL_FreeSurface(screen);
end.

