unit TestOtlCommon1;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, GpStuff, Windows, TypInfo, DSiWin32, Classes, RTTI, SysUtils, Variants,
  OtlCommon;

type
  // Test methods for class IOmniInterfaceDictionary

  TestIOmniInterfaceDictionary = class(TTestCase)
  strict private
    FIOmniInterfaceDictionary: IOmniInterfaceDictionary;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClear;
    procedure TestCount;
    procedure TestEnumerate;
    procedure TestRemove;
    procedure TestResize;
    procedure TestRetrieve1;
    procedure TestRetrieve2;
    procedure TestRetrieve3;
    procedure TestRetrieve4;
    procedure TestRetrieve5;
  end;

implementation

var
  GTestValueCount: integer;

type
  ITestValue = interface ['{48E54332-2E07-47F5-A6AE-63CF75168CA6}']
    function  GetValue: integer;
    procedure SetValue(const value: integer);
    property Value: integer read GetValue write SetValue;
  end;

  TTestValue = class(TInterfacedObject, ITestValue)
  strict private
    FValue: integer;
  protected
    function  GetValue: integer;
    procedure SetValue(const value: integer);
  public
    constructor Create(val: integer);
    destructor  Destroy;
    property Value: integer read GetValue write SetValue;
  end;

procedure TestIOmniInterfaceDictionary.SetUp;
begin
  FIOmniInterfaceDictionary := CreateInterfaceDictionary;
end;

procedure TestIOmniInterfaceDictionary.TearDown;
begin
  FIOmniInterfaceDictionary := nil;
  CheckEquals(0, GTestValueCount);
end;

procedure TestIOmniInterfaceDictionary.TestClear;
begin
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  FIOmniInterfaceDictionary.Clear;
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestCount;
begin
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  CheckEquals(1, FIOmniInterfaceDictionary.Count);
  FIOmniInterfaceDictionary.Clear;
  CheckEquals(0, FIOmniInterfaceDictionary.Count);
end;

procedure TestIOmniInterfaceDictionary.TestEnumerate;
begin

end;

procedure TestIOmniInterfaceDictionary.TestRemove;
var
  key: Int64;
begin
  // TODO: Setup method call parameters
  FIOmniInterfaceDictionary.Remove(key);
  // TODO: Validate method results
end;

procedure TestIOmniInterfaceDictionary.TestResize;
begin

end;

procedure TestIOmniInterfaceDictionary.TestRetrieve1;
var
  retIntf: ITestValue;
begin
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  CheckNotNull(retIntf);
  CheckEquals(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve2;
var
  retIntf: ITestValue;
begin
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  retIntf := FIOmniInterfaceDictionary.ValueOf(2) as ITestValue;
  CheckNull(retIntf);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve3;
var
  retIntf: ITestValue;
begin
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  FIOmniInterfaceDictionary.Add(2, TTestValue.Create(2));
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  CheckNotNull(retIntf);
  CheckEquals(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve4;
var
  retIntf: ITestValue;
begin
  FIOmniInterfaceDictionary.Add(2, TTestValue.Create(2));
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  CheckNotNull(retIntf);
  CheckEquals(1, retIntf.Value);
end;

procedure TestIOmniInterfaceDictionary.TestRetrieve5;
var
  retIntf: ITestValue;
begin
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(1));
  FIOmniInterfaceDictionary.Add(1, TTestValue.Create(2));
  retIntf := FIOmniInterfaceDictionary.ValueOf(1) as ITestValue;
  CheckNotNull(retIntf);
  CheckEquals(2, retIntf.Value);
end;

{ TTestValue }

constructor TTestValue.Create(val: integer);
begin
  inherited Create;
  FValue := val;
  Inc(GTestValueCount);
end;

destructor TTestValue.Destroy;
begin
  Dec(GTestValueCount);
  inherited;
end;

function TTestValue.GetValue: integer;
begin
  Result := FValue;
end;

procedure TTestValue.SetValue(const value: integer);
begin
  FValue := value;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestIOmniInterfaceDictionary.Suite);
end.

