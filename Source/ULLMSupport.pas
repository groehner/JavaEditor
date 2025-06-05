unit ULLMSupport;

interface

uses
  System.Classes,
  System.JSON,
  System.JSON.Serializers,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  SynEditTypes,
  SynEdit,
  UEditorForm;

type
  TLLMProvider = (
    llmProviderOpenAI,
    llmProviderGemini,
    llmProviderOllama,
    llmProviderDeepSeek,
    llmProviderGrok);

  TEndpointType = (
    etUnsupported,
    etOllamaGenerate,
    etOllamaChat,
    etOpenAICompletion,
    etOpenAIChatCompletion,
    etGemini);

  TLLMSettingsValidation = (
    svValid,
    svModelEmpty,
    svInvalidEndpoint,
    svInvalidModel,
    svAPIKeyMissing,
    svInvalidTemperature);

  TLLMSettings = record
    EndPoint: string;
    ApiKey: string;
    Model: string;
    TimeOut: Integer;
    MaxTokens: Integer;
    Temperature: Single;
    SystemPrompt: string;
    function Validate: TLLMSettingsValidation;
    function IsLocal: Boolean;
    function EndpointType: TEndpointType;
  end;

  TLLMProviders = record
    Provider: TLLMProvider;
    OpenAI: TLLMSettings;
    Gemini: TLLMSettings;
    Ollama: TLLMSettings;
    DeepSeek: TLLMSettings;
    Grok: TLLMSettings;
  end;

  TQAItem = record
    Prompt: string;
    Answer: string;
    Reason: string;
    constructor Create(const AQuestion, AnAnswer, Reason: string);
  end;

  TChatTopic = record
    Title: string;
    QAItems: TArray<TQAItem>;
  end;
  TChatTopics = TArray<TChatTopic>;

  TOnLLMResponseEvent = procedure(Sender: TObject; const Prompt, Answer, Reason: string) of object;
  TOnLLMErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  TLLMBase = class
  private
    FActiveTopicIndex: Integer;
    FChatTopics: TArray<TChatTopic>;
    FHttpClient: TNetHTTPClient;
    FHttpResponse: IHTTPResponse;
    FSourceStream: TStringStream;
    FOnLLMResponse: TOnLLMResponseEvent;
    FOnLLMError: TOnLLMErrorEvent;
    FLastPrompt: string;
    FEndPointType: TEndpointType;
    procedure OnRequestError(const Sender: TObject; const AError: string);
    procedure OnRequestCompleted(const Sender: TObject; const AResponse: IHTTPResponse);
    function GetIsBusy: Boolean;
    function GetLLMSettings: TLLMSettings;
  protected
    FSerializer: TJsonSerializer;
    procedure DoResponseCompleted(const AResponse: IHTTPResponse); virtual;
    procedure DoResponseCreated(const AResponse: IHTTPResponse); virtual;
    procedure DoResponseOK(const Msg, Reason: string); virtual;
    function RequestParams(const Prompt: string; const Suffix: string = ''): string; virtual; abstract;
    procedure AddGeminiSystemPrompt(Params: TJSONObject);
    function GeminiMessage(const Role, Content: string): TJSONObject;
  public
    Providers: TLLMProviders;  // doesn't work as property
    function ValidateSettings: TLLMSettingsValidation; virtual;
    function ValidationErrMsg(Validation: TLLMSettingsValidation): string;
    constructor Create;
    destructor Destroy; override;
    function DefaultAssistantSettings(Num: Integer): TLLMSettings;
    function DefaultChatSettings(Num: Integer): TLLMSettings;
    procedure Ask(const Prompt: string; const Suffix: string = '');
    procedure CancelRequest;
    property ActiveTopicIndex: Integer read FActiveTopicIndex;
    property ChatTopics: TArray<TChatTopic> read FChatTopics;
    property Settings: TLLMSettings read GetLLMSettings;
    property IsBusy: Boolean read GetIsBusy;
    property OnLLMResponse: TOnLLMResponseEvent read FOnLLMResponse write FOnLLMResponse;
    property OnLLMError: TOnLLMErrorEvent read FOnLLMError write FOnLLMError;
  end;

  TLLMChat = class(TLLMBase)
  private
    FActiveTopicIndex: Integer;
    FChatTopics: TArray<TChatTopic>;
  protected
    procedure DoResponseOK(const Msg, Reason: string); override;
    function RequestParams(const Prompt: string; const Suffix: string = ''): string; override;
  public
    function ValidateSettings: TLLMSettingsValidation; override;
    constructor Create;
    function ActiveTopic: TChatTopic;
    procedure NextTopic;
    procedure PreviousTopic;
    procedure ClearTopic;
    procedure RemoveTopic;
    procedure NewTopic;
    procedure SaveChat(const FName: string);
    procedure LoadChat(const FName: string);
    property ActiveTopicIndex: Integer read FActiveTopicIndex;
    property ChatTopics: TArray<TChatTopic> read FChatTopics;
  end;

  TLLMAssistant = class(TLLMBase)
  private
    FEditForm: TFEditForm;
    FActiveEditor: TCustomSynEdit;
    FCaret: TBufferCoord;
    FSelText: string;
    FStopSequence: TArray<string>;
    procedure DoCancelRequest(Sender: TObject);
  protected
    const
      MaxPrefixLines = 100;
      MaxSuffixLines = 50;
    procedure DoResponseCompleted(const AResponse: IHTTPResponse); override;
    procedure DoResponseCreated(const AResponse: IHTTPResponse); override;
    procedure DoResponseOK(const Msg, Reason: string); override;
    procedure ShowError(Sender: TObject; const Error: string);
    function RequestParams(const Prompt: string; const Suffix: string = ''): string; override;
  public
    function ValidateSettings: TLLMSettingsValidation; override;
    constructor Create;
    procedure Suggest;
    procedure Optimize;
    procedure FixBugs;
    procedure Explain;
  end;

var
  LLMAssistant: TLLMAssistant;

const
  DefaultSystemPrompt = 'You are my expert java coding assistant.';

  OpenaiChatSettings: TLLMSettings = (
    EndPoint: 'https://api.openai.com/v1/chat/completions';
    ApiKey: '';
    Model: 'gpt-4o';
    TimeOut: 20000;
    MaxTokens: 2000;
    Temperature: 1.0;
    SystemPrompt: DefaultSystemPrompt);

  OpenaiCompletionSettings: TLLMSettings = (
    EndPoint: 'https://api.openai.com/v1/completions';
    ApiKey: '';
    Model: 'gpt-3.5-turbo-instruct';
    TimeOut: 20000;
    MaxTokens: 1000;
    Temperature: 0.2;
    SystemPrompt: '');

  GeminiSettings: TLLMSettings = (
    EndPoint: 'https://generativelanguage.googleapis.com/v1beta';
    ApiKey: '';
    Model: 'gemini-2.0-flash';
    TimeOut: 20000;
    MaxTokens: 2000;
    Temperature: 1.0;
    SystemPrompt: DefaultSystemPrompt);

  DeepSeekChatSettings: TLLMSettings = (
    EndPoint: 'https://api.deepseek.com/chat/completions';
    ApiKey: '';
    Model: 'deepseek-chat';
    TimeOut: 20000;
    MaxTokens: 3000;
    Temperature: 1.0;
    SystemPrompt: DefaultSystemPrompt);

  DeepSeekCompletionSettings: TLLMSettings = (
    EndPoint: 'https://api.deepseek.com/beta/completions';
    ApiKey: '';
    Model: 'deepseek-chat';
    TimeOut: 20000;
    MaxTokens: 1000;
    Temperature: 0;
    SystemPrompt: '');

  OllamaChatSettings: TLLMSettings = (
    EndPoint: 'http://localhost:11434/api/chat';
    ApiKey: '';
    Model: 'codellama';
    //Model: 'codegema';
    //Model: 'starcoder2';
    //Model: 'stable-code';
    TimeOut: 60000;
    MaxTokens: 2000;
    Temperature: 1.0;
    SystemPrompt: DefaultSystemPrompt);

  OllamaCompletionSettings: TLLMSettings = (
    EndPoint: 'http://localhost:11434/api/generate';
    ApiKey: '';
    Model: 'codellama:code';
    TimeOut: 60000;
    MaxTokens: 1000;
    Temperature: 0.2;
    SystemPrompt: DefaultSystemPrompt);

  GrokChatSettings: TLLMSettings = (
    EndPoint: 'https://api.x.ai/v1/chat/completions';
    ApiKey: '';
    Model: 'grok-2-latest';
    TimeOut: 20000;
    MaxTokens: 3000;
    Temperature: 1.0;
    SystemPrompt: DefaultSystemPrompt);

  GrokCompletionSettings: TLLMSettings = (
    EndPoint: 'https://api.x.ai/v1/completions';
    ApiKey: '';
    Model: 'grok-2-latest';
    TimeOut: 20000;
    MaxTokens: 1000;
    Temperature: 0;
    SystemPrompt: '');

implementation

uses
  System.Math,
  System.IOUtils,
  JvGnugettext,
  System.SysUtils,
  SynUnicode,
  ULLMSuggestForm,
  UJava,
  UConfiguration,
  UUtils;

resourcestring
  CLLMBusy = 'The LLM client is busy.';
  CNoResponse = 'No response from the LLM Server.';
  CNoAPIKey = 'The LLM API key is missing.';
  CNoModel = 'The LLM model has not been set.';
  CInvalidTemperature = 'Invalid temperature: It should be a decimal number between 0.0 and 2.0.';
  CUnsupportedEndpoint = 'The LLM endpoint is missing or not supported.';
  CUnsupportedModel = 'The LLM model is not supported.';
  CUnexpectedResponse = 'Unexpected response from the LLM Server.';

{ TLLMBase }

procedure TLLMBase.AddGeminiSystemPrompt(Params: TJSONObject);
begin
  if Settings.SystemPrompt <> '' then
  begin
    var JsonText := TJSONObject.Create;
    JsonText.AddPair('text', Settings.SystemPrompt);

    var JsonParts := TJSONObject.Create;
    JsonParts.AddPair('parts', JsonText);

    Params.AddPair('system_instruction', JsonParts);
  end;
end;

procedure TLLMBase.Ask(const Prompt: string; const Suffix: string = '');
var
  ErrMsg: string;
  Params: string;
begin
  if Prompt = '' then Exit;

  if Assigned(FHttpResponse) then
    ErrMsg := CLLMBusy
  else
  begin
    var Validation := ValidateSettings;
    ErrMsg := ValidationErrMsg(Validation);
  end;

  if ErrMsg <> '' then
  begin
    if Assigned(FOnLLMError) then
      FOnLLMError(Self, ErrMsg);
    Exit;
  end;

  FEndPointType := Settings.EndpointType;
  FHttpClient.ConnectionTimeout := Settings.TimeOut;
  FHttpClient.ResponseTimeout := Settings.TimeOut * 2;

  FLastPrompt := Prompt;
  Params := RequestParams(Prompt, Suffix);

  FSourceStream.Clear;
  FSourceStream.WriteString(Params);
  FSourceStream.Position := 0;

  FHttpClient.CustHeaders.Clear;
  var EndPoint := Settings.EndPoint;
  case FEndPointType of
    etOpenAICompletion, etOpenAIChatCompletion:
      FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + Settings.ApiKey;
    etGemini:
      EndPoint := Format('%s/models/%s:generateContent?key=%s',
        [Settings.EndPoint, Settings.Model, Settings.ApiKey]);
  end;

  FHttpClient.CustomHeaders['Content-Type'] := 'application/json';
  FHttpClient.CustomHeaders['AcceptEncoding'] := 'deflate, gzip;q=1.0, *;q=0.5';
  FHttpResponse := FHttpClient.Post(EndPoint , FSourceStream);
  DoResponseCreated(FHttpResponse);
end;

procedure TLLMBase.CancelRequest;
begin
  if Assigned(FHttpResponse) then
    FHttpResponse.AsyncResult.Cancel;
end;

constructor TLLMBase.Create;
begin
  inherited;
  FHttpClient := TNetHTTPClient.Create(nil);
  FHttpClient.OnRequestCompleted := OnRequestCompleted;
  FHttpClient.OnRequestError := OnRequestError;
  FHttpClient.Asynchronous := True;

  FSourceStream := TStringStream.Create('', TEncoding.UTF8);

  FSerializer := TJsonSerializer.Create;
end;

function TLLMBase.DefaultAssistantSettings(Num: Integer): TLLMSettings;
begin
  case Num of
    0: Result := OpenaiCompletionSettings;
    1: Result := GeminiSettings;
    2: Result := OllamaCompletionSettings;
    3: Result := DeepSeekCompletionSettings;
    4: Result := GrokCompletionSettings;
  end;
end;

function TLLMBase.DefaultChatSettings(Num: Integer): TLLMSettings;
begin
  case Num of
    0: Result := OpenaiChatSettings;
    1: Result := GeminiSettings;
    2: Result := OllamaChatSettings;
    3: Result := DeepSeekChatSettings;
    4: Result := GrokChatSettings;
  end;
end;

destructor TLLMBase.Destroy;
begin
  FSerializer.Free;
  FSourceStream.Free;
  FHttpClient.Free;
  inherited;
end;

procedure TLLMBase.DoResponseCompleted(const AResponse: IHTTPResponse);
begin
  // Do nothing
end;

procedure TLLMBase.DoResponseCreated(const AResponse: IHTTPResponse);
begin
  // Do Nothing
end;

procedure TLLMBase.DoResponseOK(const Msg, Reason: string);
begin
  // Do nothing
end;

function TLLMBase.GeminiMessage(const Role, Content: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('role', Role);
  var Parts := TJSONObject.Create;
  Parts.AddPair('text', Content);
  Result.AddPair('parts', Parts);
end;

function TLLMBase.GetIsBusy: Boolean;
begin
  Result := Assigned(FHttpResponse);
end;

function TLLMBase.GetLLMSettings: TLLMSettings;
begin
  case Providers.Provider of
    llmProviderOpenAI: Result := Providers.OpenAI;
    llmProviderOllama: Result := Providers.Ollama;
    llmProviderGemini: Result := Providers.Gemini;
    llmProviderDeepSeek: Result := Providers.DeepSeek;
    llmProviderGrok: Result := Providers.Grok;
  end;
end;

procedure TLLMBase.OnRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
var
  ResponseData: TBytes;
  ResponseOK: Boolean;
  ErrMsg, Msg, Reason: string;
begin
  FHttpResponse := nil;
  DoResponseCompleted(AResponse);
  if AResponse.AsyncResult.IsCancelled then
    Exit;
  ResponseOK := False;
  if AResponse.ContentStream.Size > 0 then
  begin
    SetLength(ResponseData, AResponse.ContentStream.Size);
    AResponse.ContentStream.Read(ResponseData, AResponse.ContentStream.Size);
    var JsonResponse := TJSONValue.ParseJSONValue(ResponseData, 0);
    try
      if not (JsonResponse.TryGetValue('error.message', ErrMsg)
        or JsonResponse.TryGetValue('error', ErrMsg))
      then
        case FEndPointType of
          etOpenAIChatCompletion:
            begin
              ResponseOK := JsonResponse.TryGetValue('choices[0].message.content', Msg);
              // for DeepSeek R1 model (deepseek-reasoner)
              JsonResponse.TryGetValue('choices[0].message.reasoning_content', Reason);
            end;
          etOpenAICompletion:
            ResponseOK := JsonResponse.TryGetValue('choices[0].text', Msg);
          etOllamaGenerate:
            ResponseOK := JsonResponse.TryGetValue('response', Msg);
          etOllamaChat:
            ResponseOK := JsonResponse.TryGetValue('message.content', Msg);
          etGemini:
            ResponseOK := JsonResponse.TryGetValue('candidates[0].content.parts[0].text', Msg);
        end;
    finally
      JsonResponse.Free;
    end;
  end else
    ErrMsg := CNoResponse;

  if ResponseOK then
  begin
    DoResponseOK(Msg, Reason);
    if Assigned(FOnLLMResponse)  then
      FOnLLMResponse(Self, FLastPrompt, Msg, Reason);
  end
  else
  begin
    if ErrMsg = '' then
      ErrMsg := CUnexpectedResponse;
    if Assigned(FOnLLMError) then
      FOnLLMError(Self, ErrMsg);
  end;
end;

procedure TLLMBase.OnRequestError(const Sender: TObject; const AError: string);
begin
  FHttpResponse := nil;
  if Assigned(FOnLLMError) then
    FOnLLMError(Self, AError);
end;

function TLLMBase.ValidateSettings: TLLMSettingsValidation;
begin
  Result := Settings.Validate;
end;

function TLLMBase.ValidationErrMsg(Validation: TLLMSettingsValidation): string;
begin
  case Validation of
    svValid: Result := '';
    svModelEmpty: Result := CNoModel;
    svInvalidEndpoint: Result := CUnsupportedEndpoint;
    svInvalidModel: Result := CUnsupportedModel;
    svAPIKeyMissing: Result := CNoAPIKey;
    svInvalidTemperature: Result := CInvalidTemperature;
  end;
end;

{ TLLMChat }

function TLLMChat.ActiveTopic: TChatTopic;
begin
  Result := ChatTopics[ActiveTopicIndex];
end;

procedure TLLMChat.ClearTopic;
begin
  ChatTopics[ActiveTopicIndex] := Default(TChatTopic);
end;

constructor TLLMChat.Create;
begin
  inherited;
  Providers.Provider := llmProviderOpenAI;
  Providers.OpenAI := OpenaiChatSettings;
  Providers.Ollama := OllamaChatSettings;
  Providers.Gemini := GeminiSettings;
  Providers.DeepSeek := DeepSeekChatSettings;
  Providers.Grok := GrokChatSettings;
  FChatTopics := [Default(TChatTopic)];
  FActiveTopicIndex := 0;
end;

procedure TLLMChat.DoResponseOK(const Msg, Reason: string);
begin
  ChatTopics[ActiveTopicIndex].QAItems := ActiveTopic.QAItems + [TQAItem.Create(FLastPrompt, Msg, Reason)];
end;

procedure TLLMChat.LoadChat(const FName: string);
begin
  if FileExists(FName) then
  begin
    FChatTopics :=
      FSerializer.Deserialize<TArray<TChatTopic>>(
      TFile.ReadAllText(FName, TEncoding.UTF8));
    FActiveTopicIndex := High(ChatTopics);
  end;
end;

procedure TLLMChat.NewTopic;
begin
  if Length(ActiveTopic.QAItems) = 0 then
    Exit;
  if Length(ChatTopics[High(ChatTopics)].QAItems) > 0 then
    FChatTopics := ChatTopics + [Default(TChatTopic)];
  FActiveTopicIndex := High(ChatTopics);
end;

procedure TLLMChat.NextTopic;
begin
  if ActiveTopicIndex < Length(ChatTopics) - 1 then
    Inc(FActiveTopicIndex);
end;

procedure TLLMChat.PreviousTopic;
begin
  if ActiveTopicIndex > 0 then
    Dec(FActiveTopicIndex);
end;

function TLLMChat.RequestParams(const Prompt: string; const Suffix: string = ''): string;

  function GeminiParams: string;
  begin
    var JSON := TSmartPtr.Make(TJSONObject.Create)();

    // start with the system message
    AddGeminiSystemPrompt(JSON);

    // then add the chat history
    var Contents := TJSONArray.Create;
    for var QAItem in ActiveTopic.QAItems do
    begin
      Contents.Add(GeminiMessage('user', QAItem.Prompt));
      Contents.Add(GeminiMessage('model', QAItem.Answer));
    end;
    // finally add the new prompt
    Contents.Add(GeminiMessage('user', Prompt));
    JSON.AddPair('contents', Contents);

    // now add parameters
    var GenerationConfig := TJSONObject.Create;
    GenerationConfig.AddPair('temperature', Settings.Temperature);
    GenerationConfig.AddPair('maxOutputTokens', Settings.MaxTokens);
    JSON.AddPair('generationConfig', GenerationConfig);

    Result := JSON.ToJSON;
  end;

  function NewOpenAIMessage(const Role, Content: string): TJSONObject;
  begin
    Result := TJSONObject.Create;
    if Settings.Model.StartsWith('o') and (Role = 'system') then
    // newer OpenAI models do support system messages
      Result.AddPair('role', 'user')
    else
      Result.AddPair('role', Role);
    Result.AddPair('content', Content);
  end;

begin
  if FEndPointType = etGemini then
    Exit(GeminiParams);

  var JSON := TSmartPtr.Make(TJSONObject.Create)();
  JSON.AddPair('model', Settings.Model);
  JSON.AddPair('stream', False);

  case FEndPointType of
    etOllamaChat:
      begin
        var Options := TJSONObject.Create;
        Options.AddPair('num_predict', Settings.MaxTokens);
        Options.AddPair('temperature', Settings.Temperature);
        JSON.AddPair('options', Options);
      end;
    etOpenAIChatCompletion:
    begin
      JSON.AddPair('temperature', Settings.Temperature);
      // Newer OpenAI models do not support max_tokens
      if Settings.Model.StartsWith('o') then
        JSON.AddPair('max_completion_tokens', Settings.MaxTokens)
      else
        JSON.AddPair('max_tokens', Settings.MaxTokens);
    end;
  end;

  var Messages := TJSONArray.Create;
  // start with the system message
  if Settings.SystemPrompt <> '' then
    Messages.Add(NewOpenAIMessage('system', Settings.SystemPrompt));
  // add the history
  for var QAItem in ActiveTopic.QAItems do
  begin
    Messages.Add(NewOpenAIMessage('user', QAItem.Prompt));
    Messages.Add(NewOpenAIMessage('assistant', QAItem.Answer));
  end;
  // finally add the new prompt
  Messages.Add(NewOpenAIMessage('user', Prompt));

  JSON.AddPair('messages', Messages);

  Result := JSON.ToJSON;
end;

procedure TLLMChat.RemoveTopic;
begin
  Delete(FChatTopics, FActiveTopicIndex, 1);

  if ActiveTopicIndex > High(ChatTopics) then
  begin
    if ActiveTopicIndex > 0 then
      Dec(FActiveTopicIndex)
    else
      FChatTopics := [Default(TChatTopic)];
  end;
end;

procedure TLLMChat.SaveChat(const FName: string);
begin
  TFile.WriteAllText(FName, FSerializer.Serialize(ChatTopics));
end;

function TLLMChat.ValidateSettings: TLLMSettingsValidation;
begin
  Result := Settings.Validate;
  if (Result = svValid) and
    not (Settings.EndpointType in [etOllamaChat, etGemini, etOpenAIChatCompletion])
  then
    Result := svInvalidEndpoint;
end;

{ TQAItem }

constructor TQAItem.Create(const AQuestion, AnAnswer, Reason: string);
begin
  Self.Prompt := AQuestion;
  Self.Answer := AnAnswer;
  Self.Reason := Reason;
end;

{ TLLMSettings }

function TLLMSettings.EndpointType: TEndpointType;
begin
  Result := etUnsupported;
  if EndPoint.Contains('googleapis') then
    Result := etGemini
  else if EndPoint.Contains('openai') or EndPoint.Contains('deepseek') or
    EndPoint.Contains('x.ai')
  then
  begin
    if EndPoint.EndsWith('chat/completions') then
      Result := etOpenAIChatCompletion
    else if EndPoint.EndsWith('/completions') then
      Result := etOpenAICompletion;
  end
  else
  begin
    if EndPoint.EndsWith('api/generate') then
      Result := etOllamaGenerate
    else if EndPoint.EndsWith('api/chat') then
      Result := etOllamaChat;
  end;
end;

function TLLMSettings.IsLocal: Boolean;
begin
  Result := EndPoint.Contains('localhost')  or EndPoint.Contains('127.0.0.1');
end;

function TLLMSettings.Validate: TLLMSettingsValidation;
begin
  if Model = '' then
    Exit(svModelEmpty);
  if not InRange(Temperature, 0.0, 2.0) then Exit(svInvalidTemperature);
  case EndpointType of
    etUnsupported: Exit(svInvalidEndpoint);
    etOpenAICompletion, etOpenAIChatCompletion, etGemini:
      if ApiKey = '' then
        Exit(svAPIKeyMissing);
  end;
  Result := svValid;
end;

{ TLLMAssistant }

procedure TLLMAssistant.Explain;
const
  ExplainPrompt: string =
     'Please analyze the following java code and add detailed java comments ' +
     'and docstrings explaining what each part of the code is doing. '  +
     'The comments should be comprehensive and should help users ' +
     'understand the logic and functionality of the code. ' +
     'Ensure that the explanations and comments are integrated into the source ' +
     'code. The final output should be valid java code.'#$A#$A +
     'Here is the source code that needs comments:'#$A'```'#$A'%s'#$A'```';
begin
  FEditForm:= FJava.GetActiveEditor;
  if IsBusy or not Assigned(FEditForm) then Exit;

  FActiveEditor := FEditForm.Editor;
  FCaret := FActiveEditor.CaretXY;
  FSelText := AdjustLineBreaks(FActiveEditor.SelText, tlbsLF);

  if FSelText = '' then Exit;

  var Prompt := Format(ExplainPrompt, [FSelText]);
  if FConfiguration.LanguageCode = 'de' then
    Prompt:= 'Answer in German. ' + Prompt;

  Ask(Prompt);
end;

constructor TLLMAssistant.Create;
begin
  inherited;
  OnLLMError := ShowError;
  Providers.Provider := llmProviderOpenAI;
  Providers.OpenAI := OpenaiCompletionSettings;
  Providers.Ollama := OllamaCompletionSettings;
  Providers.Gemini := GeminiSettings;
  Providers.Gemini.Temperature := 0.2;
  Providers.DeepSeek := DeepSeekCompletionSettings;
  Providers.Grok := GrokCompletionSettings;
end;

procedure TLLMAssistant.DoCancelRequest(Sender: TObject);
begin
  CancelRequest;
end;

procedure TLLMAssistant.DoResponseCompleted(const AResponse: IHTTPResponse);
begin
  inherited;
  FEditForm.SetActivityIndicator(False);
end;

procedure TLLMAssistant.DoResponseCreated(const AResponse: IHTTPResponse);
begin
  inherited;
  FEditForm.SetActivityIndicator(True,
    _('Assistant is busy. Click to cancel.'), DoCancelRequest);
end;

procedure TLLMAssistant.DoResponseOK(const Msg, Reason: string);

  procedure RemoveLeadingLB(var Str: string);
  var
    Count: Integer;
  begin
    Count := 0;
    if Str.StartsWith(#13) then
      Inc(Count);
    if Str.StartsWith(#10) then
      Inc(Count);
    if Count > 0 then
      Str := Copy(Str, Count + 1);
  end;

begin
  FEditForm:= FJava.GetActiveEditor;
  if not Assigned(FEditForm) or
    (FActiveEditor <> FEditForm.Editor) or
    (FCaret <> FEditForm.Editor.CaretXY) or
    (FSelText <> AdjustLineBreaks(FActiveEditor.SelText, tlbsLF))
  then
    Exit;

  var Code := Msg;
  var CodeStart :=  Pos('```', Code);
  if CodeStart > 0 then
  begin
    Inc(CodeStart, 3);
    var CodeEnd := Pos('```', Code, CodeStart);
    Code := Copy(Code, CodeStart, CodeEnd - CodeStart);
    RemoveLeadingLB(Code);
  end;

  if Code.StartsWith('java') then
  begin
    Code := Copy(Code, 5);
    RemoveLeadingLB(Code);
  end;
  if Code <> '' then
    ShowSuggestion(Code, FEditForm.Editor);
end;

procedure TLLMAssistant.FixBugs;
const
  FixBugsPrompt: string =
    'Please analyze the following java code, identify any bugs, ' +
    'and provide fixes. The response should contain the fixed code ' +
    'and should be complete and ready to run. ' +
    'Along with the fixes, insert detailed java comments explaining the nature ' +
    'of the original issues and how they were resolved.'#$A#$A +
    'Here is the source code that needs fixing:'#$A'```'#$A'%s'#$A'```';
begin
  FEditForm:= FJava.GetActiveEditor;
  if IsBusy or not Assigned(FEditForm) then Exit;

  FActiveEditor := FEditForm.Editor;
  FCaret := FActiveEditor.CaretXY;
  FSelText := AdjustLineBreaks(FActiveEditor.SelText, tlbsLF);

  if FSelText = '' then Exit;

  var Prompt := Format(FixBugsPrompt, [FSelText]);
  if FConfiguration.LanguageCode = 'de' then
    Prompt:= 'Answer in German. ' + Prompt;
  Ask(Prompt);
end;

procedure TLLMAssistant.Optimize;
const
  OptimizePrompt: string =
    'Please analyze the following java code and suggest optimizations to ' +
    'reduce the number of operations during execution. The optimizations '+
    'should maintain the original functionality of the code.'#10#10 +
    'The response should contain only the optimized code ' +
    'and should be complete and ready to run.'#$A#$A +
    'Here is the source code that needs optimization:'#$A'```'#$A'%s'#$A'```';
begin
  FEditForm:= FJava.GetActiveEditor;
  if IsBusy or not Assigned(FEditForm) then Exit;

  FActiveEditor := FEditForm.Editor;
  FCaret := FActiveEditor.CaretXY;
  FSelText := FActiveEditor.SelText;

  if FSelText = '' then Exit;

  var Prompt := Format(OptimizePrompt, [FSelText]);
  if FConfiguration.LanguageCode = 'de' then
    Prompt:= 'Answer in German. ' + Prompt;
  Ask(Prompt);
end;

function TLLMAssistant.RequestParams(const Prompt: string; const Suffix: string = ''): string;

  function GeminiParams: string;
  begin
    var JSON := TSmartPtr.Make(TJSONObject.Create)();

    // start with the system message
    AddGeminiSystemPrompt(JSON);

    // Add the prompt
    JSON.AddPair('contents', GeminiMessage('user', Prompt));

    // now add parameters
    var GenerationConfig := TJSONObject.Create;
    GenerationConfig.AddPair('temperature', Settings.Temperature);
    GenerationConfig.AddPair('maxOutputTokens', Settings.MaxTokens);
    if Length(FStopSequence) > 0 then
    begin
      var StopArray := TJSONArray.Create;
      for var Term in FStopSequence do
        StopArray.Add(Term);
      GenerationConfig.AddPair('stopSequences', StopArray);
    end;
    JSON.AddPair('generationConfig', GenerationConfig);

    Result := JSON.ToJSON;
  end;

begin
  if FEndPointType = etGemini then
    Exit(GeminiParams);

  var JSON := TSmartPtr.Make(TJSONObject.Create)();
  JSON.AddPair('model', Settings.Model);
  JSON.AddPair('stream', False);
  JSON.AddPair('prompt', Prompt);
  if Suffix <> '' then
    JSON.AddPair('suffix', Suffix);
  case FEndPointType of
    etOllamaGenerate:
      begin
        JSON.AddPair('system', Settings.SystemPrompt);

        var Options := TJSONObject.Create;
        if Length(FStopSequence) > 0 then
        begin
          var StopArray := TJSONArray.Create;
          for var Term in FStopSequence do
            StopArray.Add(Term);
          Options.AddPair('stop', StopArray);
        end;
        Options.AddPair('num_predict', Settings.MaxTokens);
        Options.AddPair('temperature', Settings.Temperature);
        JSON.AddPair('options', Options);
      end;
    etOpenAICompletion:
      begin
        JSON.AddPair('max_tokens', Settings.MaxTokens);
        JSON.AddPair('temperature', Settings.Temperature);
      end;
  end;

  Result := JSON.ToJSON;
end;

procedure TLLMAssistant.ShowError(Sender: TObject; const Error: string);
begin
  FEditForm.SetActivityIndicator(False);
  FEditForm.ShowAssistantError(Error);
end;

procedure TLLMAssistant.Suggest;
const
  OpenAISuggestPrompt: string =
    'You are my java coding assistant.  Please complete the following' +
    ' Java code. Return only the missing part:'#13#10'%s';
  GeminiSuggestPrompt =
    'Please fill the blank indicated by "____" in the following java code. '+
    'Return only what is missing and nothing else:'#13#10'```%s____%s```';

  function GetPrefix: string;
  begin
    Result := '';
    for var I := Max(0, FCaret.Line - MaxPrefixLines) to FCaret.Line - 2 do
      Result := Result + FActiveEditor.Lines[I] + WideLF;
    Result := Result + Copy(FActiveEditor.Lines[FCaret.Line - 1], 1, FCaret.Char - 1);
  end;

  function GetSuffix: string;
  begin
    Result := Copy(FActiveEditor.Lines[FCaret.Line - 1], FCaret.Char + 1);
    for var I := FCaret.Line to Min(FActiveEditor.Lines.Count - 1, FCaret.Line + MaxSuffixLines) do
      Result := Result + WideLF + FActiveEditor.Lines[I];
  end;

begin
  FEditForm:= FJava.GetActiveEditor;
  if IsBusy or not Assigned(FEditForm) then Exit;

  FActiveEditor := FEditForm.Editor;
  FCaret := FActiveEditor.CaretXY;
  FSelText := FActiveEditor.SelText;

  if FSelText <> '' then Exit;

  case Settings.EndpointType of
    etOpenAICompletion:
      begin
        var Prompt := Format(OpenAISuggestPrompt, [GetPrefix]);
        Ask(Prompt, GetSuffix);
      end;
    etOllamaGenerate:
      begin
        if Settings.Model.StartsWith('codellama') then
          FStopSequence := ['<END>', '<EOD>', '<EOT>'];
        var Prompt := GetPrefix;
        Ask(Prompt, GetSuffix);
        FStopSequence := [];
      end;
    etGemini:
      begin
        FStopSequence := ['____'];
        var Prompt := Format(GeminiSuggestPrompt, [GetPrefix, GetSuffix]);
        Ask(Prompt, '');
        FStopSequence := [];
      end;
  end;
end;

function TLLMAssistant.ValidateSettings: TLLMSettingsValidation;
begin
  Result := Settings.Validate;
  if (Result = svValid) and
    not (Settings.EndpointType in [etOllamaGenerate, etOpenAICompletion, etGemini])
  then
    Result := svInvalidEndpoint;
end;

initialization
  LLMAssistant := TLLMAssistant.Create;

finalization
  LLMAssistant.Free;
end.
