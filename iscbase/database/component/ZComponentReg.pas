{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Components Registration             }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZComponentReg;

interface

{$I ZComponent.inc}

{ Zeos palette names }
const
  ZEOS_DB_PALETTE = 'Zeos Access';

procedure Register;

implementation

{$R ZComponentReg.res}  //=== ct9999 ==================

uses
{$IFDEF WITH_PROPERTY_EDITOR}
  ZPropertyEditor,
{$IFDEF FPC}
  PropEdits,
  ZUpdateSqlEditor,
  ComponentEditors,
  LResources,
{$ELSE}
{$IFNDEF UNIX}
{$IFNDEF FPC}
  ZUpdateSqlEditor,
{$ENDIF}
{$ENDIF}
  DesignIntf,
  SysUtils,                                     // **** Pitfiend addition, required to be able to put info in the delphi ide splash screen and about box 
  ToolsAPI,                                     //
{$ENDIF}
{$ENDIF}
  ZPropertiesEditor,
  Classes, ZConnection, ZAbstractConnection, ZDataset, ZSqlUpdate, ZSqlProcessor,
  ZStoredProcedure, ZGroupedConnection, ZConnectionGroup, ZEventListener,
  ZSqlMonitor, ZSqlMetadata, ZSequence, ZAbstractRODataset
  {$IFDEF ENABLE_INTERBASE}, ZIBEventAlerter {$ENDIF}
  {$IFDEF ENABLE_POSTGRESQL}, ZPgEventAlerter {$ENDIF};

{**
  Registers components in a component palette.
}
procedure Register;
{$IFNDEF FPC}                                   // **** Pitfiend addition start
{$IF DECLARED(IOTAAboutBoxServices)}            //   this allow to put a nice and pro entry in the delphi ide splash screen and about box
var                                             //
  AboutSvcs: IOTAAboutBoxServices;              //
{$IFEND}                                        //
{$ENDIF}                                        // **** Pitfiend addition end
begin
  RegisterComponents(ZEOS_DB_PALETTE, [TZConnection,
    TZTransaction, TZReadOnlyQuery, TZQuery, TZTable, TZMemTable, TZUpdateSQL,
    TZConnectionGroup, TZGroupedConnection, TZEventListener,
    TZStoredProc, TZSQLMetadata, TZSQLProcessor, TZSQLMonitor, TZSequence
    {$IFDEF ENABLE_INTERBASE}, TZIBEventAlerter {$ENDIF}
    {$IFDEF ENABLE_POSTGRESQL}, TZPgEventAlerter{$ENDIF}]) ;

{$IFDEF WITH_REGISTER_CLASSES_BUG}RegisterNoIcon{$ELSE}RegisterClasses{$ENDIF}([
  TZDateField, TZDateTimeField, TZTimeField, TZBooleanField,
  TZSmallIntField, TZShortIntField, TZWordField, TZByteField, TZIntegerField,
  TZInt64Field, TZCardinalField, TZUInt64Field, TZDoubleField, TZSingleField,
  TZBCDField, TZFMTBCDField, TZGuidField, TZRawStringField,
  TZUnicodeStringField, TZBytesField, TZVarBytesField, TZRawCLobField,
  TZUnicodeCLobField, TZBlobField]);
{$IFNDEF FPC}                                   // **** Pitfiend addition start
{$IF DECLARED(IOTAAboutBoxServices)}
    if Assigned(SplashScreenServices) then
       SplashScreenServices.AddPluginBitmap('ZEOSLib Open Source Database Objects', loadbitmap(HInstance, 'ZEOSLIBSPLASH')); // to have a nice icon, a .res file must be included, then replace 0 by loadbitmap(HInstance, 'RESOURCENAME')
    if (BorlandIDEServices<>nil) and supports(BorlandIDEServices, IOTAAboutBoxServices, AboutSvcs) then
       AboutSvcs.AddPluginInfo('ZEOSLib', 'ZEOSLib'+sLineBreak+'OpenSource database components collection'+sLineBreak+sLineBreak+'Forum:http://zeoslib.sourceforge.net', loadbitmap(HInstance, 'ZEOSLIBSPLASH'), False, 'OpenSource'); // replace 0 by loadbitmap(HInstance, 'ZEOSLIBSPLASH')
{$IFEND}
{$ENDIF}                                        // **** Pitfiend addition end

{$IFDEF WITH_PROPERTY_EDITOR}

  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'ClientCodepage', TZClientCodePagePropertyEditor); {EgonHugeist}
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'Protocol', TZProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'Database', TZDatabasePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'Catalog', TZCatalogPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnection, 'LibraryLocation', TZLibLocationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TZConnection, 'Properties', TZProperitesEditor);

  RegisterPropertyEditor(TypeInfo(string), TZConnectionGroup, 'Protocol', TZProtocolPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnectionGroup, 'Database', TZConnectionGroupPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZGroupedConnection, 'Catalog', TZGroupedConnectionCatalogPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZConnectionGroup, 'LibraryLocation', TZConnectionGroupLibLocationPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'LinkedFields', TZDataFieldPropertyEditor); {renamed by bangfauzan}
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'MasterFields', TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'SortedFields', TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZQuery, 'SequenceField', TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TZQuery, 'Properties', TZProperitesEditor);

  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'LinkedFields', TZDataFieldPropertyEditor); {renamed by bangfauzan}
  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'MasterFields', TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZReadOnlyQuery, 'SortedFields', TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TZReadOnlyQuery, 'Properties', TZProperitesEditor);

  RegisterPropertyEditor(TypeInfo(string), TZTable, 'TableName', TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'LinkedFields', TZDataFieldPropertyEditor); {renamed by bangfauzan}
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'MasterFields', TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'SortedFields', TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZTable, 'SequenceField', TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TZTable, 'Properties', TZProperitesEditor);

  RegisterPropertyEditor(TypeInfo(string), TZStoredProc, 'StoredProcName', TZProcedureNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZStoredProc, 'SortedFields', TZDataFieldPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TZSequence, 'SequenceName', TZSequenceNamePropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TZTransaction, 'Properties', TZProperitesEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TZEventListener, 'Properties', TZProperitesEditor);
{$IFDEF USE_METADATA}
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'Catalog', TZCatalogProperty);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ColumnName', TZColumnNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ForeignCatalog', TZCatalogProperty);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ForeignSchema', TZSchemaPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ForeignTableName', TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'LinkedFields', TZDataFieldPropertyEditor); {renamed by bangfauzan}
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'MasterFields', TZMasterFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'ProcedureName', TZProcedureNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'Schema', TZSchemaPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'SequenceName', TZSequenceNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'SortedFields', TZDataFieldPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'TableName', TZTableNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TZSQLMetadata, 'TypeName', TZTypeNamePropertyEditor);
{$ENDIF}
{$IFDEF FPC}
  RegisterComponentEditor(TZUpdateSQL, TZUpdateSQLEditor);
{$ELSE}
    {$IFNDEF UNIX}
  RegisterComponentEditor(TZUpdateSQL, TZUpdateSQLEditor);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

//=== ct9999 ==================

end.

