unit TestOmniValue;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, GpStuff, Windows, TypInfo, DSiWin32, Classes, SysUtils, Variants,
  OtlCommon;

{$I OtlOptions.inc}

type
  // Test methods for class TOmniValueContainer

  TestTOmniValueContainer = class(TTestCase)
  strict protected
    procedure CheckSimpleType(const ov: TOmniValue; expected: array of boolean);
    procedure CheckWrappedType(const ov: TOmniValue; expected: array of boolean);
  public
    procedure TearDown; override;
  published
    procedure TestComposed;
    procedure TestInterface;
    procedure TestSimpleValues;
    procedure TestWrappedValues;
  {$IFDEF OTL_TypeInfoHasTypeData}
    procedure TestCastToInterface_issue_128;
  {$ENDIF OTL_TypeInfoHasTypeData}
  end;

implementation

uses
  TestValue;

type
  ITestInterface = interface ['{D9F4791C-1FD3-46F6-BE8E-DF5C5356402D}']
    function GetValue: int64;
    procedure SetValue(const value: int64);
    property Value: int64 read GetValue write SetValue;
  end;

  TTestInterface = class(TInterfacedObject, ITestInterface)
  strict private
    FValue: int64;
  public
    function GetValue: int64;
    procedure SetValue(const value: int64);
  end;

procedure TestTOmniValueContainer.CheckSimpleType(const ov: TOmniValue; expected: array
  of boolean);
begin
  CheckEquals(4, Length(expected));
  CheckEquals(expected[0], ov.IsBoolean);
  CheckEquals(expected[1], ov.IsInteger);
  CheckEquals(expected[2], ov.IsFloating);
  CheckEquals(expected[3], ov.IsDateTime);
end;

procedure TestTOmniValueContainer.CheckWrappedType(const ov: TOmniValue;
  expected: array of boolean);
begin
  CheckEquals(4, Length(expected));
  CheckEquals(expected[0], ov.IsObject);
  CheckEquals(expected[1], ov.IsString);
  CheckEquals(expected[2], ov.IsWideString);
  CheckEquals(expected[3], ov.IsVariant);
end;

procedure TestTOmniValueContainer.TearDown;
begin
  CheckEquals(0, GTestValueCount);
end;

{$IFDEF OTL_TypeInfoHasTypeData}
procedure TestTOmniValueContainer.TestCastToInterface_issue_128;
var
  ov: TOmniValue;
  intf: ITestInterface;
begin
  intf := TTestInterface.Create;
  intf.Value := $42000000000017;
  ov := intf;
  intf := nil;
  CheckEquals($42000000000017, ov.CastTo<ITestInterface>.Value);
end;
{$ENDIF OTL_TypeInfoHasTypeData}

procedure TestTOmniValueContainer.TestComposed;
var
  ov: TOmniValue;
begin
  ov := TOmniValue.Create([17, '42']);
  CheckTrue(ov.IsArray);
  CheckEquals(integer(17), integer(ov[0]));
  CheckEquals('42', ov[1]);
  ov := TOmniValue.CreateNamed(['42', 17]);
  CheckTrue(ov.IsArray);
  CheckEquals(integer(17), integer(ov['42']));
end;

procedure TestTOmniValueContainer.TestInterface;
var
  ov: TOmniValue;

  procedure TestInterface;
  var
    intf: ITestValue;
  begin
    intf := TTestValue.Create(42);
    ov.AsInterface := intf;
    CheckTrue(ov.IsInterface); CheckSimpleType(ov, [false, false, false, false]); CheckWrappedType(ov, [false, false, false, false]);
    CheckEquals(42, (ov.AsInterface as ITestValue).Value);
  end;

begin
  TestInterface;
  ov := true; CheckFalse(ov.IsInterface);
  TestInterface;
  ov := 17; CheckFalse(ov.IsInterface);
  TestInterface;
  ov := 17.42; CheckFalse(ov.IsInterface);
  TestInterface;
  ov := '17'; CheckFalse(ov.IsInterface);
  TestInterface;
  ov := TTestValue.Create(17); CheckFalse(ov.IsInterface); ov.AsObject.Free;
  TestInterface;
  ov := TOmniValue.Create([17, '42']); CheckFalse(ov.IsInterface);
  TestInterface;
  ov := TOmniValue.CreateNamed(['42', 17]); CheckFalse(ov.IsInterface);
  TestInterface;
end;

procedure TestTOmniValueContainer.TestSimpleValues;
var
  ov: TOmniValue;
begin
  ov := true;
  CheckEquals(true, ov.AsBoolean);
  CheckSimpleType(ov, [true, false, false, false]); CheckWrappedType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov := false;
  CheckEquals(false, ov.AsBoolean);
  CheckSimpleType(ov, [true, false, false, false]); CheckWrappedType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov := 0;
  CheckEquals(0, ov.AsInteger);
  CheckSimpleType(ov, [false, true, false, false]); CheckWrappedType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov := 42;
  CheckEquals(42, ov.AsInteger);
  CheckSimpleType(ov, [false, true, false, false]); CheckWrappedType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov := 3.14;
  CheckEquals(3.14, ov.AsExtended);
  CheckSimpleType(ov, [false, false, true, false]); CheckWrappedType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov.AsDateTime := EncodeDate(2011,12,19) + EncodeTime(19,53,42,17);
  CheckEqualsString(FormatDateTime('yyyy-mm-ddThh:nn:ss.zzz', EncodeDate(2011,12,19) + EncodeTime(19,53,42,17)),
    FormatDateTime('yyyy-mm-ddThh:nn:ss.zzz', ov.AsDateTime));
  CheckSimpleType(ov, [false, false, false, true]); CheckWrappedType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
end;

procedure TestTOmniValueContainer.TestWrappedValues;
var
  ov: TOmniValue;
  v : Variant;
begin
  ov := TTestValue.Create(42);
  CheckEquals('TTestValue', ov.AsObject.ClassName);
  CheckWrappedType(ov, [true, false, false, false]); CheckSimpleType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov.AsObject.Free;
  ov := '42';
  CheckEquals('42', ov.AsString);
  CheckWrappedType(ov, [false, true, false, false]); CheckSimpleType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  ov.AsWideString := '17';
  CheckEquals('17', ov.AsWideString);
  CheckWrappedType(ov, [false, false, true, false]); CheckSimpleType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
  v := 127;
  ov := v;
  CheckEquals(integer(127), integer(ov.AsVariant));
  CheckWrappedType(ov, [false, false, false, true]); CheckSimpleType(ov, [false, false, false, false]); CheckFalse(ov.IsInterface);
end;

{ TTestInterface }

function TTestInterface.GetValue: int64;
begin
  Result := FValue;
end;

procedure TTestInterface.SetValue(const value: int64);
begin
  FValue := value;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOmniValueContainer.Suite);
end.

