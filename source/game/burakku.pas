unit burakku;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, sdl, sdl_ttf, sdl_mixer_nosmpeg, sdl_gfx;

const
  LEVELTILE_WIDTH = 25;
  LEVELTILE_HEIGHT = 19;

type

  TGameObject_Type = ( START, HEX, ORB, PLAYER, TILE );
  TTileType = ( FLOOR, WALL );

  TImage = class
    public
      constructor Create;
      destructor Destroy;
      procedure LoadBMP(filen: string);
      function GetSurface: PSDL_Surface;
    private
      surface: PSDL_Surface;
  end;

  TGameObject = class
    public
      constructor Create;
      destructor Destroy;

      { movement }
      procedure MoveUp;
      procedure MoveDown;
      procedure MoveLeft;
      procedure MoveRight;

      procedure SetIsOnHex;
      function IsOnHex: Boolean;

      function GetHitBox: TRect;

      procedure SetPrevPos;
      procedure RevtPrevPos;

    private
      pos: TPoint;
      surface: PSDL_Surface;
      rect: TRect;
      _type: TGameObject_Type;

      { orb }
      onhexpad: boolean;

      { hexagram }
      contains_orb: Boolean;

      { player }
      step_taken: Integer;
      prev_pos: TPoint;

      { tile }
      tiletype: TTileType;

  end;

  TGameLevel_Info = record
    name: string;
    author: string;
    version: string;
  end;

  TGameLevel = class
    public
      constructor Create;
      destructor Destroy;

      procedure Draw(dstsurface: PSDL_Surface);
      procedure Load(filen: string);

      function GetPlayer: TGameObject;
      function PlayerExists: Boolean;

      function IsOrbAtPos(x,y: Integer): Boolean;
      function GetOrbAtPos(x,y :Integer): TGameObject;

      procedure HandleInput;
      procedure Update;

    private
      info: TGameLevel_Info;
      objects: TObjectList;
      tiles: TObjectList;
  end;

  procedure LoadImages;
  procedure AddImage(filen, list: string);

  var
    images_tile: TObjectList;
    images_object: TObjectList;
    images_player: TObjectList;

    screen: PSDL_Surface;
    event: PSDL_Event;
    rect: PSDL_Rect;
    exit: Boolean;
    fpsman: PFPSmanager;

    level: TGameLevel;

implementation

procedure AddImage(filen, list: string);
var
  image: TImage;
begin
  image:= TImage.Create;
  image.LoadBMP(filen);

  if (list = 'p') then
  begin
    images_player.Add(image);
  end;
  if (list = 't') then
  begin
    images_tile.Add(image);
  end;
  if (list = 'o') then
  begin
    images_object.Add(image);
  end;
end;

procedure LoadImages;
begin
  images_tile:= TObjectList.Create;
  AddImage('assets/image/level/blacktile.bmp', 't');
  AddImage('assets/image/level/greentile.bmp', 't');
  AddImage('assets/image/level/stonebrickfloor.bmp', 't');
  AddImage('assets/image/level/stonebrickwall.bmp', 't');
  AddImage('assets/image/level/stonewalldown.bmp', 't');

  images_object:= TObjectList.Create;
  AddImage('assets/image/object/hexgoal.bmp', 'o');
  AddImage('assets/image/object/pushorb.bmp', 'o');

  images_player:= TObjectList.Create;
  AddImage('assets/image/player/blackmage_backl.bmp', 'p');
  AddImage('assets/image/player/blackmage_backr.bmp', 'p');
  AddImage('assets/image/player/blackmage_frontl.bmp', 'p');
  AddImage('assets/image/player/blackmage_frontr.bmp', 'p');
  AddImage('assets/image/player/blackmage_leftstand.bmp', 'p');
  AddImage('assets/image/player/blackmage_leftwalk.bmp', 'p');
  AddImage('assets/image/player/blackmage_rightstand.bmp', 'p');
  AddImage('assets/image/player/blackmage_rightwalk.bmp', 'p');
end;

{ IMAGE }
constructor TImage.Create;
begin
  new(self.surface);
end;

destructor TImage.Destroy;
begin
  SDL_FreeSurface(self.surface);
end;

procedure TImage.LoadBMP(filen: string);
begin
  self.surface:= SDL_LoadBMP(Pchar(filen));
  SDL_SetColorKey(
    self.surface, SDL_SRCCOLORKEY,
    SDL_MapRGB(self.surface^.format, 255,0,255)
  );
end;

function TImage.GetSurface: PSDL_Surface;
begin
  Result:= self.surface;
end;

{ GAMEOBJECT }

constructor TGameObject.Create;
begin
  self.rect.Width:= 32;
  self.rect.Height:= 32;
end;

destructor TGameObject.Destroy;
begin

end;

procedure TGameObject.MoveUp;
begin
  self.pos.SetLocation(self.pos.x, self.pos.y - 32);
  self.rect.SetLocation(self.pos.x, self.pos.y);
end;

procedure TGameObject.MoveDown;
begin
  self.pos.SetLocation(self.pos.x, self.pos.y + 32);
  self.rect.SetLocation(self.pos.x, self.pos.y);
end;

procedure TGameObject.MoveRight;
begin
  self.pos.SetLocation(self.pos.x + 32, self.pos.y);
  self.rect.SetLocation(self.pos.x, self.pos.y);
end;

procedure TGameObject.MoveLeft;
begin
  self.pos.SetLocation(self.pos.x - 32, self.pos.y);
  self.rect.SetLocation(self.pos.x, self.pos.y);
end;

function TGameObject.IsOnHex: Boolean;
begin
  Result:= self.onhexpad;
end;

procedure TGameObject.SetIsOnHex;
begin
  self.onhexpad:= true;
end;

function TGameObject.GetHitBox: TRect;
begin
  Result:= Self.rect;
end;

procedure TGameObject.SetPrevPos;
begin
  self.prev_pos.SetLocation(self.pos.x, self.pos.y);
end;

procedure TGameObject.RevtPrevPos;
begin
  self.pos.SetLocation(self.prev_pos.x, self.prev_pos.y);
end;

{ GAMELVEL }

constructor TGameLevel.Create;
var
  i,j : Integer;
  tile: TGameObject;
  player: TGameObject;
  gameobj: TGameObject;
begin

  { fill level with green tiles }
  self.tiles:= TObjectList.Create;
  for i:= 0 to LEVELTILE_WIDTH do
  begin
    for j:= 0 to LEVELTILE_HEIGHT do
    begin
      tile:= TGameObject.Create;
      tile._type:= TGameObject_Type.TILE;
      tile.surface:= (images_tile.Items[2] as TImage).surface;
      tile.pos.SetLocation(i*32, j*32);
      self.tiles.Add(tile);
    end;
  end;

  self.objects:= TObjectList.Create;

  { create player }
  player:= TGameObject.Create;
  player.pos.SetLocation(0,0);
  player.rect.SetLocation(player.pos.x, player.pos.y);
  player.surface:= (images_player.Items[2] as TImage).surface;
  player._type:= TGameObject_Type.PLAYER;
  self.objects.Add(player);

  { create orbs }
  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(5*32,8*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[1] as TImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(8*32,4*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[1] as TImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(12*32,9*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[1] as TImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(15*32,10*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[1] as TImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  { create hexpad }
  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(10*32,5*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[0] as TImage).surface;
  gameobj._type:= TGameObject_Type.HEX;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(12*32,5*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[0] as TImage).surface;
  gameobj._type:= TGameObject_Type.HEX;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(11*32,4*32);
  gameObj.rect.SetLocation(gameObj.pos.x, gameObj.pos.y);
  gameobj.surface:= (images_object.Items[0] as TImage).surface;
  gameobj._type:= TGameObject_Type.HEX;
  self.objects.Add(gameobj);

end;

destructor TGameLevel.Destroy;
begin

end;

procedure TGameLevel.Update;
var
  i,j: Integer;
  objhex: TGameObject;
  objorb: TGameObject;
  gameobj: TGameObject;
begin

  { see if orbs are on hex pad. and set "isonhex" (true) }
  for i:= 0 to self.objects.Count - 1 do
  begin
    objorb:= (self.objects.Items[i] as TGameObject);
    if (objorb._type = TGameObject_Type.ORB) then
    begin
       for j:= 0 to self.objects.Count -1 do
       begin
         objhex:= (self.objects.Items[j] as TGameObject);
         if (objhex._type =TGameObject_Type.HEX) then
         begin
           if (objorb.pos.x = objhex.pos.x) and (objorb.pos.y = objhex.pos.y) then
           begin
             objorb.SetIsOnHex;
           end;
         end;
       end;
    end;
  end;

  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (GetPlayer.GetHitBox.IntersectsWith(gameobj.GetHitBox)) and (gameobj._type <> TGameObject_Type.PLAYER) then
    begin
       GetPlayer.RevtPrevPos;
    end;
  end;

  for i:= 0 to self.objects.Count - 1 do
  begin
    objorb:= (self.objects.Items[i] as TGameObject);
    if (objorb._type = TGameObject_Type.ORB) then
    begin
       for j:= 0 to self.objects.Count -1 do
       begin
         gameobj:= (self.objects.Items[j] as TGameObject);
         if (objhex._type =TGameObject_Type.ORB) then
         begin
           if (objorb.pos.x = objhex.pos.x) and (objorb.pos.y = objhex.pos.y) then
           begin
             objorb.RevtPrevPos;
           end;
         end;
       end;
    end;
  end;

end;

procedure TGameLevel.Draw(dstsurface: PSDL_Surface);
var
  i: Integer;
  tile: TGameObject;
  gameobj: TGameObject;
  srcrect, dstrect :PSDL_Rect;
begin

  { init rect pointers and values }
  new(srcrect);
  srcrect^.x:= 0;
  srcrect^.y:= 0;
  srcrect^.w:= 32;
  srcrect^.h:= 32;
  new(dstrect);
  dstrect^.w:= 32;
  dstrect^.h:= 32;

  { draw tiles }
  for i:= 0 to self.tiles.Count -1 do
  begin
    tile:= (self.tiles.Items[i] as TGameObject);
    dstrect^.x:= tile.pos.x;
    dstrect^.y:= tile.pos.y;
    SDL_BlitSurface(tile.surface, srcrect, dstsurface, dstrect);
  end;

  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.HEX) then
    begin
      dstrect^.x:= gameobj.pos.x;
      dstrect^.y:= gameobj.pos.y;
      SDL_BlitSurface(gameobj.surface, srcrect, dstsurface, dstrect);
    end;
  end;

  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.ORB) then
    begin
      dstrect^.x:= gameobj.pos.x;
      dstrect^.y:= gameobj.pos.y;
      SDL_BlitSurface(gameobj.surface, srcrect, dstsurface, dstrect);
    end;
  end;

  if (self.PlayerExists) then
  begin
    dstrect^.x:= self.GetPlayer.pos.x;
    dstrect^.y:= self.GetPlayer.pos.y;
    SDL_BlitSurface(self.GetPlayer.surface, srcrect, dstsurface, dstrect);
  end;

  {
  { draw hit boxes }
  for i:= 0 to self.objects.Count - 1 do
   begin

   end;
   }

end;

procedure TGameLevel.HandleInput;
var
  i, j: Integer;
  gameobj: TGameObject;
begin
     if (event^.type_ = SDL_KEYDOWN) then
     begin
          if (event^.key.keysym.sym = SDLK_UP) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveUp;

            for i:= 0 to self.objects.Count - 1 do
            begin
              gameobj:= (self.objects.Items[i] as TGameObject);
              if (GetPlayer.GetHitBox.IntersectsWith(gameobj.GetHitBox)) and
              (gameobj._type = TGameObject_Type.ORB) and not (gameobj.IsOnHex) then
              begin
                GameObj.SetPrevPos;
                Gameobj.MoveUp;
              end;
            end;
          end;

          if (event^.key.keysym.sym = SDLK_DOWN) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveDown;

            for i:= 0 to self.objects.Count - 1 do
            begin
              gameobj:= (self.objects.Items[i] as TGameObject);
              if (GetPlayer.GetHitBox.IntersectsWith(gameobj.GetHitBox)) and
              (gameobj._type = TGameObject_Type.ORB) and not (gameobj.IsOnHex)  then
              begin
                 gameobj.SetPrevPos;
                 gameobj.MoveDown;
              end;
            end;
          end;

          if (event^.key.keysym.sym = SDLK_LEFT) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveLeft;

            for i:= 0 to self.objects.Count - 1 do
            begin
              gameobj:= (self.objects.Items[i] as TGameObject);
              if (GetPlayer.GetHitBox.IntersectsWith(gameobj.GetHitBox)) and
              (gameobj._type = TGameObject_Type.ORB) and not (gameobj.IsOnHex)  then
              begin
                 gameobj.SetPrevPos;
                 gameobj.MoveLeft;
              end;
            end;
          end;

          if (event^.key.keysym.sym = SDLK_RIGHT) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveRight;

            for i:= 0 to self.objects.Count - 1 do
            begin
              gameobj:= (self.objects.Items[i] as TGameObject);
              if (GetPlayer.GetHitBox.IntersectsWith(gameobj.GetHitBox)) and
              (gameobj._type = TGameObject_Type.ORB) and not (gameobj.IsOnHex)  then
              begin
                 gameobj.SetPrevPos;
                 gameobj.MoveRight;
              end;
            end;
          end;
     end;
end;

procedure TGameLevel.Load(filen: string);
begin

end;

function TGameLevel.IsOrbAtPos(x,y: Integer): Boolean;
var
  i: Integer;
  gameobj: TGameObject;
  rect: TRect;
begin
  rect.Width:= 32;
  rect.Height:= 32;
  rect.SetLocation(x,y);

  for i:= 0 to self.objects.Count- 1 do
  begin
       gameobj:= (self.objects.Items[i] as TGameObject);
       if (gameobj._type = TGameObject_Type.ORB) then
       begin
         if gameobj.rect.IntersectsWith(rect) then
         begin
           Result:= true;
         end;
       end;
  end;
  Result:= false;
end;

function TGameLevel.GetOrbAtPos(x,y: Integer): TGameObject;
var
  i: Integer;
  gameobj: TGameObject;
  rect: TRect;
begin
  rect.Width:= 32;
  rect.Height:= 32;
  rect.SetLocation(x,y);

  for i:= 0 to self.objects.Count- 1 do
  begin
       gameobj:= (self.objects.Items[i] as TGameObject);
       if (gameobj._type = TGameObject_Type.ORB) then
       begin
         if gameobj.rect.IntersectsWith(rect) then
         begin
           Result:= gameobj;
         end;
       end;
  end;
  Result:= nil;
end;

function TGameLevel.GetPlayer: TGameObject;
var
  i: Integer;
  gameobj: TGameObject;
begin
  for i:= 0 to self.objects.Count -1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.PLAYER) then
    begin
      Result:= gameobj;
    end;
  end;
end;

function TGameLevel.PlayerExists: Boolean;
var
  i: Integer;
  gameobj: TGameObject;
begin
  for i:= 0 to self.objects.Count -1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.PLAYER) then
    begin
      Result:= true;
    end;
  end;
end;

end.

