unit mainwindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, Menus,
  ExtCtrls, ComCtrls,

  burakku;

type
  TEditorObject_Type = (
  TILE_FLOOR, TILE_WALL, TILE_WALL2, TILE_GREEN, TILE_BLACK,
    OBJ_ORB, OBJ_START, OBJ_HEX, OBJ_TELE
  );

type

  { TfrmMainWindow }

  TfrmMainWindow = class(TForm)
    imgList_main: TImageList;
    Listview_Tiles: TListView;
    ListView_Objects: TListView;
    MainMenu: TMainMenu;
    MenuItem_File: TMenuItem;
    dlg_Open: TOpenDialog;
    DodadPageControl: TPageControl;
    MenuItem_About: TMenuItem;
    MenuItem_FileNew: TMenuItem;
    MenuItem_FileOpen: TMenuItem;
    MenuItem_FileSave: TMenuItem;
    PaintBox_Level: TPaintBox;
    MainPairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    dlg_Save: TSaveDialog;
    MainStatusBar: TStatusBar;
    TabSheet_Tiles: TTabSheet;
    TabSheet_Objects: TTabSheet;
    MainToolbar: TToolBar;
    procedure FormCreate(Sender: TObject);
    procedure ListView_ObjectsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView_ObjectsClick(Sender: TObject);
    procedure Listview_TilesClick(Sender: TObject);
    procedure MenuItem_AboutClick(Sender: TObject);
    procedure PaintBox_LevelClick(Sender: TObject);
    procedure PaintBox_LevelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox_LevelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox_LevelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox_LevelPaint(Sender: TObject);
  private
    bitmap_editor_tilebackground: TBitmap;
    editor_object: TEditorObject_Type;
    game_level: TGameLevel;
    mouse_leftdown: Boolean;
    mouse_rightdown: Boolean;
    mouse_pos: TPoint;

    procedure Set_LevelEditorObject(X,Y: Integer);
    procedure Remove_LevelEditorObject(X,Y: Integer);
  public

  end;

var
  frmMainWindow: TfrmMainWindow;

implementation
uses aboutform;

{$R *.lfm}

{ TfrmMainWindow }

procedure TfrmMainWindow.PaintBox_LevelPaint(Sender: TObject);
var
  center_rect :TRect;
  i,j : Integer;
  gameobj: TGameObject;
begin

  { set center rect values.. }
  center_rect.Width:= 800;
  center_rect.Height:= 600;

  { Set pos half drawing area and half graphic size }
  center_rect.SetLocation(
    PaintBox_Level.Width div 2 - 400,
    PaintBox_Level.Height div 2 - 300
  );

  { clear screen (black) }
  PaintBox_Level.Canvas.Brush.Color:= RGBToColor(0,0,0);
  PaintBox_Level.Canvas.Clear;

  { tile background }
  for i:= 0 to PaintBox_Level.Width div 128 do
  begin
     for j:= 0 to PaintBox_Level.Height div 128 do
     begin
        PaintBox_Level.Canvas.Draw(i*128,j*128, bitmap_editor_tilebackground );
     end;
  end;

  {draw level tiles}
  for i:= 0 to game_level.GetTiles.Count - 1 do
  begin
     gameobj:= TGameObject.Create;;
     gameobj := (game_level.GetTiles.Items[i] as TGameObject);
     PaintBox_Level.Canvas.Draw((gameobj.GetPos.x * 32) + center_rect.Location.x,
     (gameobj.GetPos.y * 32) + center_rect.Location.y, gameobj.GetBitmap) ;
  end;

  {draw level level objects}
  for i:= 0 to game_level.GetObjects.Count - 1 do
  begin
     gameobj:= TGameObject.Create;
     gameobj := (game_level.GetObjects.Items[i] as TGameObject);
     PaintBox_Level.Canvas.Draw((gameobj.GetPos.x * 32) + center_rect.Location.x,
     (gameobj.GetPos.y * 32) + center_rect.Location.y, gameobj.GetBitmap) ;
  end;

end;

procedure TfrmMainWindow.MenuItem_AboutClick(Sender: TObject);
begin
frmAbout.Show;
end;

procedure TfrmMainWindow.PaintBox_LevelClick(Sender: TObject);
begin

end;

procedure TfrmMainWindow.PaintBox_LevelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if (Button = mbLeft) then
     begin
        mouse_leftdown:= true;
        Set_LevelEditorObject(X,Y);
     end;
     if (Button = mbRight) then
     begin
        Remove_LevelEditorObject(((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32));
     end;
     PaintBox_Level.Invalidate;
end;

procedure TfrmMainWindow.PaintBox_LevelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
    if (mouse_leftdown) then
    begin
         Set_LevelEditorObject(X,Y);
    end;
    PaintBox_Level.Invalidate;
end;

procedure TfrmMainWindow.PaintBox_LevelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
      mouse_leftdown:= false;
  end;
  if (Button = mbRight) then
  begin

  end;
end;

procedure TfrmMainWindow.FormCreate(Sender: TObject);
begin

{ create and load tilebackground }
 bitmap_editor_tilebackground:= TBitmap.Create;
 bitmap_editor_tilebackground.LoadFromFile('assets/image/editor/editorbackground.bmp');

 { load bitmaps }
 burakku_bitmap_init;

 { construct gamelevel }
 game_level:= TGameLevel.Create;
end;

procedure TfrmMainWindow.ListView_ObjectsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin

end;

procedure TfrmMainWindow.ListView_ObjectsClick(Sender: TObject);
begin
 if (ListView_Objects.Selected.Caption = 'player_start') then
 begin
    editor_object:= TEditorObject_Type.OBJ_START;
 end;
 if (ListView_Objects.Selected.Caption = 'hexagram') then
 begin
    editor_object:= TEditorObject_Type.OBJ_HEX;
 end;
 if (ListView_Objects.Selected.Caption = 'orb') then
 begin
    editor_object:= TEditorObject_Type.OBJ_ORB;
 end;
 if (ListView_Objects.Selected.Caption = 'teleport') then
 begin
    editor_object:= TEditorObject_Type.OBJ_TELE;
 end;
end;

procedure TfrmMainWindow.Listview_TilesClick(Sender: TObject);
begin
 if (Listview_Tiles.Selected.Caption = 'floor') then
 begin
    editor_object:= TEditorObject_Type.TILE_FLOOR;
 end;
 if (Listview_Tiles.Selected.Caption = 'wall') then
 begin
    editor_object:= TEditorObject_Type.TILE_WALL;
 end;
 if (Listview_Tiles.Selected.Caption = 'wall2') then
 begin
    editor_object:= TEditorObject_Type.TILE_WALL2;
 end;
 if (Listview_Tiles.Selected.Caption = 'black') then
 begin
    editor_object:= TEditorObject_Type.TILE_BLACK;
 end;
 if (Listview_Tiles.Selected.Caption = 'green') then
 begin
    editor_object:= TEditorObject_Type.TILE_GREEN;
 end;
end;

procedure TfrmMainWindow.Set_LevelEditorObject(X,Y: Integer);
var
  i: Integer;
  gameobj: TGameObject;
begin
          { Place / move start tile }
          if (editor_object = TEditorObject_Type.OBJ_START) then
         begin
            //game_level.SetStart((X div 32) * 32, (Y div 32) * 32 );
         end;
          { Add Hex Goal }
          if (editor_object = TEditorObject_Type.OBJ_HEX) then
         begin
           game_level.AddObject(((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
           ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32), TGameObject_Type.HEXAGRAM);
         end;

           { Add orb }
          if (editor_object = TEditorObject_Type.OBJ_ORB) then
         begin
            game_level.AddObject(((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32),TGameObject_Type.ORB);
         end;

          {
         { Add tele }
          if (editor_object = TEditorObject_Type.OBJ_TELE) then
         begin
            game_level.AddTele(((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32));
         end;
         }

          { _ TILES }

             { place tile }
             if (editor_object = TEditorObject_Type.TILE_BLACK) then
             begin
                 game_level.SetTile(
                 ((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32),
                 (bitmaps_tiles.Items[0] as TBitmap), TTile_Type.FLOOR
                 );
             end;
             { place tile }
             if (editor_object = TEditorObject_Type.TILE_GREEN) then
             begin
                 game_level.SetTile(
                 ((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32),
                 (bitmaps_tiles.Items[1] as TBitmap), TTile_Type.FLOOR
                 );
             end;
             { place tile }
             if (editor_object = TEditorObject_Type.TILE_FLOOR) then
             begin
                 game_level.SetTile(
                 ((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32),
                 (bitmaps_tiles.Items[2] as TBitmap), TTile_Type.FLOOR
                 );
             end;
             { place tile }
             if (editor_object = TEditorObject_Type.TILE_WALL) then
             begin
                 game_level.SetTile(
                 ((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32),
                 (bitmaps_tiles.Items[3] as TBitmap), TTile_Type.WALL
                 );
             end;
             { place tile }
             if (editor_object = TEditorObject_Type.TILE_WALL2) then
             begin
                 game_level.SetTile(
                 ((((X div 32) * 32) - (PaintBox_Level.Width div 2 - 400 ) ) div 32),
                 ((((Y div 32) * 32) - (PaintBox_Level.Height div 2 - 300) ) div 32),
                 (bitmaps_tiles.Items[4] as TBitmap), TTile_Type.WALL
                 );
             end;
end;

procedure TfrmMainWindow.Remove_LevelEditorObject(X,Y: Integer);
var
  i: Integer;
  gameobj: TGameObject;
begin
   { move 'remove' logic to burakku.GameLevel.RemoveXX ?? }

   { HEX GOAL }
   if (editor_object = TEditorObject_Type.OBJ_HEX) then
   begin
       if game_level.GetObjects.Count <> 0 then
       begin
          for i:= 0 to game_level.GetObjects.Count - 1 do
          begin
            gameobj:= (game_level.GetObjects.Items[i] as TGameObject);
            if (gameobj.GetObjectType = TGameObject_Type.HEXAGRAM) then
            begin
                if ((gameobj.GetPos.x = x) and ( gameobj.GetPos.y = y)) then
                begin
                   game_level.GetObjects.Remove(gameobj);
                   Exit;
                end;
            end;
          end;
       end;
   end;

   { ORB }
   if (editor_object = TEditorObject_Type.OBJ_ORB) then
   begin
       if game_level.GetObjects.Count <> 0 then
       begin
          for i:= 0 to game_level.GetObjects.Count - 1 do
          begin
            gameobj:= (game_level.GetObjects.Items[i] as TGameObject);
            if (gameobj.GetObjectType = TGameObject_Type.ORB) then
            begin
                if ((gameobj.GetPos.x = x) and ( gameobj.GetPos.y = y)) then
                begin
                   game_level.GetObjects.Remove(gameobj);
                   Exit;
                end;
            end;
          end;
       end;
   end;

   {
    { TELEPORT }
   if (editor_object = TEditorObject_Type.OBJ_TELE) then
   begin
       if game_level.GetTele.Count <> 0 then
       begin
          for i:= 0 to game_level.GetTele.Count - 1 do
          begin
            gameobj:= (game_level.GetTele.Items[i] as TGameObject);
            if ((gameobj.GetPos.x = x) and ( gameobj.GetPos.y = y)) then
            begin
               game_level.GetTele.Remove(gameobj);
               Exit;
            end;
          end;
       end;
   end;
   }
end;

end.

