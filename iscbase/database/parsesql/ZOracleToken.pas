{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for Oracle             }
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

unit ZOracleToken;

interface

{$I ZParseSql.inc}

{$IF defined(ZEOS_DISABLE_ORACLE) and defined(ZEOS_DISABLE_ADO) and
     defined(ZEOS_DISABLE_OLEDB) and defined(ZEOS_DISABLE_ODBC) and defined(ZEOS_DISABLE_PROXY)}
  {$DEFINE EMPTY_ZOracleToken}
{$IFEND}

{$IFNDEF EMPTY_ZOracleToken}
uses
  Classes, ZTokenizer, ZGenericSqlToken;

type

  {** Implements a Oracle-specific number state object. }
  TZOracleNumberState = TZGenericSQLNoHexNumberState;

  {** Implements a Oracle-specific quote string state object. }
  TZOracleQuoteState = TZGenericSQLQuoteState;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZOracleCommentState = TZGenericSQLCommentState;

  {** Implements a symbol state object. }
  TZOracleSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZOracleWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZOracleTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  public
    function NormalizeParamToken(const Token: TZToken; out ParamName: String;
      LookUpList: TStrings; out ParamIndex: Integer;
      out IngoreParam: Boolean): String; override;
  end;

{$ENDIF EMPTY_ZOracleToken}

implementation

{$IFNDEF EMPTY_ZOracleToken}

{ TZOracleSymbolState }

{**
  Creates this Oracle-specific symbol state object.
}
constructor TZOracleSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!=');
  Add('||');
  //Add(':=');
end;

{ TZOracleWordState }

{**
  Constructs this Oracle-specific word state object.
}
constructor TZOracleWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
  SetWordChars('$', '$', True);
  SetWordChars('#', '#', True);
  SetWordChars('@', '@', True);
end;

{ TZOracleTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZOracleTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZOracleSymbolState.Create;
  NumberState := TZOracleNumberState.Create;
  QuoteState := TZOracleQuoteState.Create;
  WordState := TZOracleWordState.Create;
  CommentState := TZOracleCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);
  SetCharacterState('#', '#', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

function TZOracleTokenizer.NormalizeParamToken(const Token: TZToken;
  out ParamName: String; LookUpList: TStrings; out ParamIndex: Integer;
  out IngoreParam: Boolean): String;
var P: PChar;
begin
  if (Token.L >= 2) and (Ord(Token.P^) in [Ord(#39), Ord('`'), Ord('"'), Ord('[')])
  then ParamName := GetQuoteState.DecodeToken(Token, Token.P^)
  else System.SetString(ParamName, Token.P, Token.L);
  System.SetString(Result, nil, Token.L+1);
  P := Pointer(Result);
  P^ := ':';
  Move(Token.P^, (P+1)^, Token.L*SizeOf(Char));
  ParamIndex := LookUpList.IndexOf(ParamName);
  if ParamIndex < 0 then
    ParamIndex := LookUpList.Add(ParamName);
  IngoreParam := False;
end;

{$ENDIF EMPTY_ZOracleToken}

end.

