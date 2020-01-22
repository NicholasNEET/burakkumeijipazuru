unit burakku;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, sdl, sdl_ttf, sdl_mixer_nosmpeg;

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
    private
      pos: TPoint;
      surface: PSDL_Surface;
      rect: TRect;
      _type: TGameObject_Type;

      { orb }

      { hexagram }

      { player }
      step_taken: Integer;

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

      procedure Draw;
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

implementation

{ LOOSE }

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

end;

destructor TGameObject.Destroy;
begin

end;


{ GAMELVEL }

constructor TGameLevel.Create;
begin

  self.tiles:= TObjectList.Create;
  self.objects:= TObjectList.Create;

end;

destructor TGameLevel.Destroy;
begin

end;

procedure TGameLevel.Draw;
begin

end;

end.

