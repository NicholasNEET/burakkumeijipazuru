{%RunFlags MESSAGES+}
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
  TPlayerFacing = (UP, DOWN, LEFT, RIGHT);

  TSDLImage = class
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

      procedure UpdateHitBox;

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
      facing: TPlayerFacing;

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
  image: TSDLImage;
begin
  image:= TSDLImage.Create;
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
  { load all bitmap images into TSDLImage (TobjectList compat) }

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

{ SDL_IMAGE }
constructor TSDLImage.Create;
begin
  new(self.surface);
end;

destructor TSDLImage.Destroy;
begin
  SDL_FreeSurface(self.surface);
end;

procedure TSDLImage.LoadBMP(filen: string);
begin
  self.surface:= SDL_LoadBMP(Pchar(filen));
  SDL_SetColorKey(
    self.surface, SDL_SRCCOLORKEY,
    SDL_MapRGB(self.surface^.format, 255,0,255)
  );
end;

function TSDLImage.GetSurface: PSDL_Surface;
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
  self.pos.SetLocation(self.pos.x, self.pos.y - 1);
  self.SetPrevPos;
end;

procedure TGameObject.MoveDown;
begin
  self.pos.SetLocation(self.pos.x, self.pos.y + 1);
  self.SetPrevPos;
end;

procedure TGameObject.MoveRight;
begin
  self.pos.SetLocation(self.pos.x + 1, self.pos.y);
  self.SetPrevPos;
end;

procedure TGameObject.MoveLeft;
begin
  self.pos.SetLocation(self.pos.x - 1, self.pos.y);
  self.SetPrevPos;
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

procedure TGameObject.UpdateHitBox;
begin
  self.rect.SetLocation(self.pos.x * 32, self.pos.y * 32);
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
      tile.surface:= (images_tile.Items[2] as TSDLImage).surface;
      tile.pos.SetLocation(i, j);
      self.tiles.Add(tile);
    end;
  end;

  self.objects:= TObjectList.Create;

  { create player }
  player:= TGameObject.Create;
  player.pos.SetLocation(0,0);
  player.UpdateHitBox;
  player.surface:= (images_player.Items[2] as TSDLImage).surface;
  player._type:= TGameObject_Type.PLAYER;
  self.objects.Add(player);

  { create orbs }
  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(5,8);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[1] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(8,4);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[1] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(12,9);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[1] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(15,10);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[1] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.ORB;
  self.objects.Add(gameobj);

  { create hexpad }
  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(10,5);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[0] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.HEX;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(12,5);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[0] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.HEX;
  self.objects.Add(gameobj);

  gameobj:= TGameObject.Create;
  gameObj.pos.SetLocation(11,4);
  gameObj.UpdateHitBox;
  gameobj.surface:= (images_object.Items[0] as TSDLImage).surface;
  gameobj._type:= TGameObject_Type.HEX;
  self.objects.Add(gameobj);

end;

destructor TGameLevel.Destroy;
begin

end;

procedure TGameLevel.Update;
var
  i,j: Integer;
  hexobj: TGameObject;
  orbobj: TGameObject;
  gameobj: TGameObject;
begin

  { update hit boxes }
  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    gameobj.UpdateHitBox;
  end;

  { move orbs }
  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.ORB) and gameobj.rect.IntersectsWith(GetPlayer.rect) then
    begin
      if (GetPlayer.facing = TPlayerFacing.UP) then
      begin
        if not (IsOrbAtPos(gameobj.pos.x, gameobj.pos.y - 1)) then
        begin
          gameobj.MoveUp;
        end
        else
        begin
          WriteLn('orb collision possible');
        end;
      end;

      if (GetPlayer.facing = TPlayerFacing.DOWN) then
      begin
           gameobj.MoveDown;
      end;
      if (GetPlayer.facing = TPlayerFacing.LEFT) then
      begin
           gameobj.MoveLeft;
      end;
      if (GetPlayer.facing = TPlayerFacing.RIGHT) then
      begin
           gameobj.MoveRight;
      end;
    end;
  end;

  { stop orbs from taking up the same space. (orb collision) }
  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.ORB) then
    begin
      for j:= 0 to self.objects.Count - 1 do
      begin
        orbobj:= (self.objects.Items[j] as TGameObject);
          if (gameobj.rect.IntersectsWith(orbobj.rect)) and
          (orbobj._type = TGameObject_Type.ORB) and (i <> j) then
          begin
            gameobj.RevtPrevPos;
          end;
      end;
    end;
  end;

  { see if orbs are on hex pad. and set "isonhex" (true) }
  for i:= 0 to self.objects.Count - 1 do
  begin
    orbobj:= (self.objects.Items[i] as TGameObject);
    if (orbobj._type = TGameObject_Type.ORB) then
    begin
       for j:= 0 to self.objects.Count -1 do
       begin
         hexobj:= (self.objects.Items[j] as TGameObject);
         if (hexobj._type = TGameObject_Type.HEX) then
         begin
           if (orbobj.pos.x = hexobj.pos.x) and (orbobj.pos.y = hexobj.pos.y) then
           begin
             orbobj.SetIsOnHex;
           end;
         end;
       end;
    end;
  end;

  { revtpos collision on everything.. }
  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (GetPlayer.GetHitBox.IntersectsWith(gameobj.GetHitBox)) and (gameobj._type <> TGameObject_Type.PLAYER) then
    begin
       GetPlayer.RevtPrevPos;
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
    dstrect^.x:= tile.pos.x * 32;
    dstrect^.y:= tile.pos.y * 32;
    SDL_BlitSurface(tile.surface, srcrect, dstsurface, dstrect);
  end;

  { draw hex }
  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.HEX) then
    begin
      dstrect^.x:= gameobj.pos.x * 32;
      dstrect^.y:= gameobj.pos.y * 32;
      SDL_BlitSurface(gameobj.surface, srcrect, dstsurface, dstrect);
    end;
  end;

  { draw orbs }
  for i:= 0 to self.objects.Count - 1 do
  begin
    gameobj:= (self.objects.Items[i] as TGameObject);
    if (gameobj._type = TGameObject_Type.ORB) then
    begin
      dstrect^.x:= gameobj.pos.x * 32;
      dstrect^.y:= gameobj.pos.y * 32;
      SDL_BlitSurface(gameobj.surface, srcrect, dstsurface, dstrect);
    end;
  end;

  { draw player }
  if (self.PlayerExists) then
  begin
    dstrect^.x:= self.GetPlayer.pos.x * 32;
    dstrect^.y:= self.GetPlayer.pos.y * 32;
    SDL_BlitSurface(self.GetPlayer.surface, srcrect, dstsurface, dstrect);
  end;

end;

procedure TGameLevel.HandleInput;
var
  i, j: Integer;
  gameobj: TGameObject;
  orbobj: TGameObject;
begin
     if (event^.type_ = SDL_KEYDOWN) then
     begin
          if (event^.key.keysym.sym = SDLK_UP) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveUp;
            GetPlayer.facing:= TPlayerFacing.UP;
            GetPlayer.surface:= (images_player.Items[0] as TSDLImage).surface;
          end;

          if (event^.key.keysym.sym = SDLK_DOWN) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveDown;
            GetPlayer.facing:= TPlayerFacing.DOWN;
            GetPlayer.surface:= (images_player.Items[2] as TSDLImage).surface;
          end;

          if (event^.key.keysym.sym = SDLK_LEFT) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveLeft;
            GetPlayer.facing:= TPlayerFacing.LEFT;
            GetPlayer.surface:= (images_player.Items[5] as TSDLImage).surface;
          end;

          if (event^.key.keysym.sym = SDLK_RIGHT) then
          begin
            GetPlayer.SetPrevPos;
            GetPlayer.MoveRight;
            GetPlayer.facing:= TPlayerFacing.RIGHT;
            GetPlayer.surface:= (images_player.Items[6] as TSDLImage).surface;
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
begin
  for i:= 0 to self.objects.Count- 1 do
  begin
       gameobj:= (self.objects.Items[i] as TGameObject);
         if (gameobj._type = TGameObject_Type.ORB) and
         (gameobj.pos.x = x) and (gameobj.pos.y = y) then
         begin
             Result:= true;
         end;
  end;
  Result:= false;
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

