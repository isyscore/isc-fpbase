{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Database Sequence Component                }
{                                                         }
{        Originally written by Stefan Glienke             }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

{ constributors:
  cipto
}
unit ZSequence;

interface

{$I ZComponent.inc}

uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZDbcIntfs, ZAbstractConnection;

type
  {** Represents a component which wraps a sequence to database. }
  TZSequence = class(TZAbstractConnectionLinkedComponent)
  private
    FSequence: IZSequence;
    FSequenceName: string;
    FBlockSize: Integer;
    procedure SetBlockSize(const Value: Integer);
    procedure SetSequenceName(const Value: string);
  protected
    procedure SetConnection(Value: TZAbstractConnection); override;
    function GetSequence: IZSequence;
  public
    constructor Create(AOwner: TComponent); override;
    // Modified by cipto 8/2/2007 12:09:21 PM
    destructor Destroy; override;

    function GetCurrentValue: Int64;
    function GetNextValue: Int64;
    function GetCurrentValueSQL: string; // Get the Sequence as SQL-Text
    function GetNextValueSQL: string;   // Get the Sequence as SQL-Text
    // Modified by cipto 8/2/2007 10:04:41 AM
    procedure CloseSequence;
  published
    property BlockSize: Integer read FBlockSize write SetBlockSize default 1;
    property Connection;
    property SequenceName: string read FSequenceName write SetSequenceName;
  end;

implementation

{ TZSequence }

procedure TZSequence.CloseSequence;
begin
  FSequence := nil;
end;

constructor TZSequence.Create(AOwner: TComponent);
begin
  inherited;
  FBlockSize := 1;
end;

destructor TZSequence.Destroy;
begin
  if Assigned(FConnection) then
     FConnection.UnregisterComponent(self);
  inherited;
end;

{**
  Gets the current unique key generated by this sequence.
  @param the next generated unique key.
}
function TZSequence.GetCurrentValue: Int64;
begin
  GetSequence;
  if Assigned(FSequence) then
    Result := FSequence.GetCurrentValue
  else
    Result := 0;
end;

function TZSequence.GetCurrentValueSQL: string;
begin
  GetSequence;
  if Assigned(FSequence) then begin
    Result := FSequence.GetCurrentValueSQL;
  end else begin
    Result := 'IMPLEMENT';
  end;
end;

{**
  Gets the next unique key generated by this sequence.
  @param the next generated unique key.
}
function TZSequence.GetNextValue: Int64;
begin
  GetSequence;
  if Assigned(FSequence) then
    Result := FSequence.GetNextValue
  else
    Result := 0;
end;

function TZSequence.GetNextValueSQL: string;
begin
  GetSequence;
  if Assigned(FSequence) then begin
    Result := FSequence.GetNextValueSQL;
  end else begin
    Result := 'IMPLEMENT';
  end;
end;

function TZSequence.GetSequence: IZSequence;
begin
  if not Assigned(FSequence) and Assigned(FConnection) and Assigned(FConnection.DbcConnection) then
    FSequence := FConnection.DbcConnection.CreateSequence(FSequenceName, FBlockSize);
  Result := FSequence;
end;

procedure TZSequence.SetBlockSize(const Value: Integer);
begin
  FBlockSize := Value;
  GetSequence;
  if Assigned(FSequence) then
    FSequence.SetBlockSize(FBlockSize);
end;

procedure TZSequence.SetConnection(Value: TZAbstractConnection);
begin
  if FConnection <> Value then begin
    FSequence := nil;
    if Value = nil then
      FConnection.UnregisterComponent(Self);
    FConnection := Value;
    if FConnection <> nil then begin
      FConnection.RegisterComponent(self);
      GetSequence;
    end;
  end;
end;

procedure TZSequence.SetSequenceName(const Value: string);
begin
  FSequenceName := Value;
  GetSequence;
  if Assigned(FSequence) then
    FSequence.SetName(FSequenceName);
end;

end.
