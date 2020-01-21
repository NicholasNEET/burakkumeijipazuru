unit burakku;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, contnrs;

type

  TGameObject_Type = (START, PLAYER, TILE, ORB, HEXAGRAM, TELEPORT);
  TTile_Type = (WALL, FLOOR);

  TGameObject = class
      public
          constructor Create;
          destructor Destroy;

          { basic object }

          function GetPos: TPoint;
          procedure SetPos(x,y: Integer);

          function GetBitmap: TBitmap;
          procedure SetBitmap(bitmap: TBitmap);

          function GetObjectType: TGameObject_Type;

          { tile }
          procedure SetTileType(_type: TTile_Type);
          function GetTileType: TTile_Type;

      private
      { object }
      _type: TGameObject_Type;
      id: string;
      pos: TPoint;
      bmp: TBitmap;
      rect: TRect;

      { tile }
      tile_type: TTile_Type;

      { player }
      steps_taken: Integer;
  end;

  TGameLevel = class
      public
          constructor Create;
          destructor Destroy;

          function GetTiles: TObjectList;
          procedure SetTile(x,y: Integer; bitmap: TBitmap; _type: TTile_Type);

          //procedure SetStart(x,y: Integer);
          //function GetStart: TGameObject;

          function GetObjects: TObjectList;
          procedure AddObject(x,y: Integer; _type: TGameObject_Type);

      private
          tiles: TObjectList;
          objects: TObjectList;
  end;

  TGame_Menu = class
      public
          constructor Create;
          destructor Destroy;
      private

  end;

var
  game_character: TGameObject;

  { bitmap }
  bitmaps_tiles: TObjectList;
  bitmaps_objects: TObjectList;
  bitmaps_player: TObjectList;
  bitmap_starting: TBitmap;


procedure burakku_bitmap_init;
procedure burakku_addtile_bitmap(filen: string);
procedure burakku_addplayer_bitmap(filen: string);
procedure burakku_addobject_bitmap(filen: string);

implementation

{ loose }

procedure burakku_bitmap_init;
begin
  bitmap_starting := TBitmap.Create;
  bitmap_starting.LoadFromFile('assets/image/editor/icon_playerstart.bmp');

  bitmaps_tiles:= TObjectList.Create;
  burakku_addtile_bitmap('assets/image/level/blacktile.bmp');
  burakku_addtile_bitmap('assets/image/level/greentile.bmp');
  burakku_addtile_bitmap('assets/image/level/stonebrickfloor.bmp');
  burakku_addtile_bitmap('assets/image/level/stonebrickwall.bmp');
  burakku_addtile_bitmap('assets/image/level/stonewalldown.bmp');

  bitmaps_objects:= TObjectList.Create;
  burakku_addobject_bitmap('assets/image/object/pushorb.bmp');
  burakku_addobject_bitmap('assets/image/object/hexgoal.bmp');
  burakku_addobject_bitmap('assets/image/object/teleport.bmp');

  bitmaps_player:= TObjectList.Create;

end;

procedure burakku_addtile_bitmap(filen: string);
var
  bitmap: TBitmap;
begin
  bitmap:= TBitmap.Create;
  bitmap.LoadFromFile(filen);
  bitmaps_tiles.Add(bitmap);
end;

procedure burakku_addobject_bitmap(filen: string);
var
  bitmap: TBitmap;
begin
  bitmap:= TBitmap.Create;
  bitmap.LoadFromFile(filen);
  bitmap.TransparentColor:= RGBToColor(255,0,255);
  bitmap.Transparent:= true;
  bitmaps_objects.Add(bitmap);
end;

procedure burakku_addplayer_bitmap(filen: string);
begin

end;

{ TGameObject }
constructor TGameObject.Create;
begin

end;

destructor TGameObject.Destroy;
begin

end;

function TGameObject.GetPos: TPoint;
begin
  Result:= self.pos;
end;

procedure TGameObject.SetPos(x,y: Integer);
begin
  self.pos.x:= x;
  self.pos.y:= y;
end;

function TGameObject.GetBitmap: TBitmap;
begin
  Result:= self.bmp;
end;

procedure TGameObject.SetBitmap(bitmap:TBitmap);
begin
  self.bmp := bitmap;
end;

procedure TGameObject.SetTileType(_type: TTile_Type);
begin
  self.tile_type:= _type;
end;

function TGameObject.GetTileType: TTile_Type;
begin
  Result:= self.tile_type;
end;

function TGameObject.GetObjectType: TGameObject_Type;
begin
  Result:= _type;
end;

{ TGameLevel }

constructor TGameLevel.Create;
var
  i,j: Integer;
  new_tile: TGameObject;
begin
  { fill level with black tiles }
  self.tiles:= TObjectList.Create;
  for i:= 0 to 25 do
  begin
    for j:= 0 to 18 do
    begin
      new_tile:= TGameObject.Create;
      new_tile.pos.SetLocation(i,j);
      new_tile.bmp:= TBitmap.Create;
      new_tile.bmp:= (bitmaps_tiles.Items[0] as TBitmap);
      self.tiles.Add(new_tile);
    end;
  end;

  self.objects:= TObjectList.Create;

  //self.start:= TGameObject.Create;
  //self.start.SetBitmap(bitmap_starting);

end;

destructor TGameLevel.Destroy;
begin

end;

{
procedure TGameLevel.SetStart(x,y: Integer);
begin
  self.start.pos.x:= x;
  self.start.pos.y:= y;
end;

function TGameLevel.GetStart: TGameObject;
begin
  Result:= self.start;
end;
}

function TGameLevel.GetTiles: TObjectList;
begin
  Result:= Self.tiles;
end;

procedure TGameLevel.SetTile(x,y: Integer; bitmap:TBitmap; _type: TTile_Type);
var
  i: Integer;
  tile: TGameObject;
begin
  for i:= 0 to self.tiles.Count - 1 do
  begin
    tile:= (self.tiles.Items[i] as TGameObject);
    if ((tile.GetPos.x = x) and (tile.GetPos.y = y)) then
    begin
      tile.SetBitmap(bitmap);
      tile.SetTileType(_type);
    end;
  end;
end;

procedure TGameLevel.AddObject(x,y: Integer; _type: TGameObject_Type);
var
    gameobj: TGameObject;
    list_item: TGameObject;
    contains_one: Boolean;
    i: Integer;
    bitmap_index: Integer;
begin

    gameobj:= TGameObject.Create;
    if (_type =TGameObject_Type.HEXAGRAM) then
    begin
      gameobj.bmp:= (bitmaps_objects.Items[1] as TBitmap);
    end
    else if (_type = TGameObject_Type.ORB) then
    begin
      gameobj.bmp:= (bitmaps_objects.Items[0] as TBitmap);
    end
    else if (_type = TGameObject_Type.START) then
    begin
      gameobj.bmp:= bitmap_starting;
    end;
    gameobj.SetPos(X, Y);
    gameobj._type:= _type;

    contains_one:= false;
    for i:= 0 to self.objects.Count - 1 do
    begin
      list_item:= (self.objects.Items[i] as TGameObject);
      if (list_item._type = _type) then
      begin
         if (list_item.pos.x = X) and (list_item.pos.y = Y) then
         begin
           contains_one:= true;
           break;
         end;
        end;
      end;

    if (not contains_one) then
    begin
      self.objects.Add(gameobj);
    end;
end;

function TGameLevel.GetObjects: TObjectList;
begin
  Result:= self.objects;
end;

{ Menu }

constructor TGame_Menu.Create;
begin

end;

destructor TGame_Menu.Destroy;
begin

end;

end.

