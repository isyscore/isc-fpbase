{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            SQL Statements Analysing classes             }
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

unit ZOracleAnalyser;

interface

{$I ZParseSql.inc}

{$IF defined(ZEOS_DISABLE_ORACLE) and defined(ZEOS_DISABLE_ADO) and
     defined(ZEOS_DISABLE_OLEDB) and defined(ZEOS_DISABLE_ODBC) and defined(ZEOS_DISABLE_PROXY)}
  {$DEFINE EMPTY_ZOracleAnalyser}
{$IFEND}

{$IFNDEF EMPTY_ZOracleAnalyser}

uses Classes, ZGenericSqlAnalyser;

type

  /// <summary>Implements an Oracle statements analyser.</summary>
  TZOracleStatementAnalyser = class (TZGenericStatementAnalyser)
  public
    /// <summary>Creates the object and assignes the main properties.</summary>
    constructor Create;
  end;

{$ENDIF EMPTY_ZOracleAnalyser}
implementation
{$IFNDEF EMPTY_ZOracleAnalyser}

const
  {** The generic constants.}
  OracleSectionNames: array[0..13] of string = (
    'SELECT', 'UPDATE', 'DELETE', 'INSERT', 'FROM',
    'WHERE', 'INTO', 'GROUP*BY', 'HAVING', 'ORDER*BY',
    'ORDER*SIBLINGS*BY', 'FOR*UPDATE', 'START*WITH', 'CONNECT*BY'
  );
  OracleSelectOptions: array[0..2] of string = (
    'DISTINCT', 'ALL', 'UNIQUE'
  );
  OracleFromJoins: array[0..7] of string = (
    'NATURAL', 'RIGHT', 'LEFT', 'FULL', 'INNER', 'OUTER', 'JOIN', 'CROSS'
  );
  OracleFromClauses: array[0..1] of string = (
    'ON', 'USING'
  );

{ TZOracleStatementAnalyser }

constructor TZOracleStatementAnalyser.Create;
begin
  SectionNames := ArrayToStrings(OracleSectionNames);
  SelectOptions := ArrayToStrings(OracleSelectOptions);
  FromJoins := ArrayToStrings(OracleFromJoins);
  FromClauses := ArrayToStrings(OracleFromClauses);
end;

{$ENDIF EMPTY_ZOracleAnalyser}

end.
