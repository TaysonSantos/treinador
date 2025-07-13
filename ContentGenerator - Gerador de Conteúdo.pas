unit ContentGenerator;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.RegularExpressions, System.StrUtils, System.IOUtils, System.Threading,
  System.Math, OllamaAPI;

type
  TContentType = (ctProgrammingTutorial, ctCodeSnippet, ctArticle, ctDocumentation,
    ctQA, ctBlogPost, ctTechnicalGuide, ctExercise, ctExample);
  
  TProgrammingLanguage = (plPascal, plPython, plJavaScript, plJava, plCSharp, 
    plCPP, plC, plGo, plRust, plTypeScript, plPHP, plRuby, plSwift, plKotlin, plSQL);
    
  TDifficultyLevel = (dlBeginner, dlIntermediate, dlAdvanced, dlExpert);
  
  TContentTemplate = record
    Name: string;
    Description: string;
    ContentType: TContentType;
    Template: string;
    Variables: TStringList;
    MinLength: Integer;
    MaxLength: Integer;
    RequiredSkills: TArray<string>;
  end;
  
  TContentRequest = record
    ContentType: TContentType;
    Topic: string;
    Language: TProgrammingLanguage;
    Difficulty: TDifficultyLevel;
    Length: Integer;
    Keywords: TArray<string>;
    Format: string; // markdown, html, plain
    IncludeExamples: Boolean;
    IncludeExercises: Boolean;
    CustomPrompt: string;
  end;
  
  TGeneratedContent = record
    Title: string;
    Content: string;
    ContentType: TContentType;
    Metadata: TJSONObject;
    QualityScore: Double;
    GenerationTime: TDateTime;
    WordCount: Integer;
    EstimatedReadingTime: Integer;
  end;
  
  TContentGenerationEvent = procedure(Sender: TObject; const Content: TGeneratedContent) of object;
  TGenerationProgressEvent = procedure(Sender: TObject; Progress: Integer; const Status: string) of object;
  
  TContentGenerator = class
  private
    FOllamaAPI: TOllamaAPI;
    FTemplates: TList<TContentTemplate>;
    FGeneratedContent: TList<TGeneratedContent>;
    FCurrentModel: string;
    FDefaultTemperature: Double;
    FDefaultMaxTokens: Integer;
    FQualityThreshold: Double;
    FRetryCount: Integer;
    
    // Eventos
    FOnContentGenerated: TContentGenerationEvent;
    FOnProgress: TGenerationProgressEvent;
    
    // Cache e otimização
    FContentCache: TDictionary<string, TGeneratedContent>;
    FTopicKeywords: TDictionary<string, TStringList>;
    FLanguageTemplates: TDictionary<TProgrammingLanguage, TStringList>;
    
    function LoadTemplates: Boolean;
    function GetTemplate(ContentType: TContentType; Language: TProgrammingLanguage): TContentTemplate;
    function BuildPrompt(const Request: TContentRequest): string;
    function PostProcessContent(const RawContent: string; const Request: TContentRequest): string;
    function CalculateQualityScore(const Content: string; const Request: TContentRequest): Double;
    function ExtractTitle(const Content: string): string;
    function CountWords(const Text: string): Integer;
    function EstimateReadingTime(WordCount: Integer): Integer;
    function ValidateContent(const Content: string; const Request: TContentRequest): Boolean;
    function GenerateCodeSnippet(const Topic: string; Language: TProgrammingLanguage; 
      Difficulty: TDifficultyLevel): string;
    function GenerateArticleOutline(const Topic: string; Difficulty: TDifficultyLevel): TStringList;
    function GenerateExercises(const Topic: string; Language: TProgrammingLanguage; 
      Count: Integer): TStringList;
    function GenerateQAFromContent(const Content: string; Count: Integer): TStringList;
    function ApplyFormatting(const Content: string; const Format: string): string;
    procedure InitializeTemplates;
    procedure InitializeLanguageTemplates;
    procedure LoadTopicKeywords;
    function GetCacheKey(const Request: TContentRequest): string;
    function EnhanceWithExamples(const Content: string; const Request: TContentRequest): string;
    function AddExercisesToContent(const Content: string; const Request: TContentRequest): string;
    
  public
    constructor Create(OllamaAPI: TOllamaAPI);
    destructor Destroy; override;
    
    // Propriedades
    property CurrentModel: string read FCurrentModel write FCurrentModel;
    property DefaultTemperature: Double read FDefaultTemperature write FDefaultTemperature;
    property DefaultMaxTokens: Integer read FDefaultMaxTokens write FDefaultMaxTokens;
    property QualityThreshold: Double read FQualityThreshold write FQualityThreshold;
    property RetryCount: Integer read FRetryCount write FRetryCount;
    property GeneratedContent: TList<TGeneratedContent> read FGeneratedContent;
    
    // Eventos
    property OnContentGenerated: TContentGenerationEvent read FOnContentGenerated write FOnContentGenerated;
    property OnProgress: TGenerationProgressEvent read FOnProgress write FOnProgress;
    
    // Métodos principais de geração
    function GenerateContent(const Request: TContentRequest): TGeneratedContent;
    function GenerateContentAsync(const Request: TContentRequest): ITask;
    function GenerateBatchContent(const Requests: TArray<TContentRequest>): TArray<TGeneratedContent>;
    
    // Geração específica por tipo
    function GenerateProgrammingTutorial(const Topic: string; Language: TProgrammingLanguage; 
      Difficulty: TDifficultyLevel): TGeneratedContent;
    function GenerateCodeDocumentation(const Code: string; Language: TProgrammingLanguage): TGeneratedContent;
    function GenerateTechnicalArticle(const Topic: string; Difficulty: TDifficultyLevel): TGeneratedContent;
    function GenerateExerciseSet(const Topic: string; Language: TProgrammingLanguage; 
      Difficulty: TDifficultyLevel; Count: Integer): TGeneratedContent;
    function GenerateQASet(const Content: string; Count: Integer): TGeneratedContent;
    
    // Geração de código
    function GenerateCodeExample(const Description: string; Language: TProgrammingLanguage; 
      Difficulty: TDifficultyLevel): string;
    function GenerateCodeExplanation(const Code: string; Language: TProgrammingLanguage): string;
    function GenerateCodeRefactoring(const Code: string; Language: TProgrammingLanguage): string;
    function GenerateCodeComments(const Code: string; Language: TProgrammingLanguage): string;
    function GenerateUnitTests(const Code: string; Language: TProgrammingLanguage): string;
    
    // Geração de artigos e documentação
    function GenerateArticleFromOutline(const Outline: TStringList; Difficulty: TDifficultyLevel): string;
    function GenerateDocumentationFromCode(const CodeFiles: TStringList; 
      Language: TProgrammingLanguage): string;
    function GenerateBlogPostFromTopic(const Topic: string; Keywords: TArray<string>): string;
    function GenerateAPIDocumentation(const APIDescription: string): string;
    
    // Utilitários e análise
    function AnalyzeContentQuality(const Content: string): TJSONObject;
    function GetContentSuggestions(const Topic: string): TArray<string>;
    function ImproveContent(const Content: string; const Feedback: string): string;
    function TranslateContent(const Content: string; const TargetLanguage: string): string;
    function SummarizeContent(const Content: string; MaxWords: Integer): string;
    
    // Gerenciamento de templates
    function AddCustomTemplate(const Template: TContentTemplate): Boolean;
    function UpdateTemplate(const Name: string; const Template: TContentTemplate): Boolean;
    function DeleteTemplate(const Name: string): Boolean;
    function GetAvailableTemplates: TArray<TContentTemplate>;
    function ExportTemplates(const FileName: string): Boolean;
    function ImportTemplates(const FileName: string): Boolean;
    
    // Cache e performance
    procedure ClearCache;
    function GetCacheStatistics: TJSONObject;
    procedure OptimizeCache;
    function PreloadContent(const Topics: TArray<string>): Boolean;
    
    // Configuração
    procedure LoadConfiguration(const ConfigFile: string);
    procedure SaveConfiguration(const ConfigFile: string);
    function GetSupportedLanguages: TArray<TProgrammingLanguage>;
    function GetSupportedContentTypes: TArray<TContentType>;
  end;

implementation

uses
  System.Variants, System.DateUtils, System.NetEncoding, System.Hash;

{ TContentGenerator }

constructor TContentGenerator.Create(OllamaAPI: TOllamaAPI);
begin
  inherited Create;
  
  FOllamaAPI := OllamaAPI;
  FTemplates := TList<TContentTemplate>.Create;
  FGeneratedContent := TList<TGeneratedContent>.Create;
  FContentCache := TDictionary<string, TGeneratedContent>.Create;
  FTopicKeywords := TDictionary<string, TStringList>.Create;
  FLanguageTemplates := TDictionary<TProgrammingLanguage, TStringList>.Create;
  
  // Configurações padrão
  FCurrentModel := 'llama2';
  FDefaultTemperature := 0.7;
  FDefaultMaxTokens := 4096;
  FQualityThreshold := 0.7;
  FRetryCount := 3;
  
  // Inicializar dados
  InitializeTemplates;
  InitializeLanguageTemplates;
  LoadTopicKeywords;
  LoadTemplates;
end;

destructor TContentGenerator.Destroy;
var
  I: Integer;
  Content: TGeneratedContent;
begin
  // Liberar conteúdo gerado
  for I := 0 to FGeneratedContent.Count - 1 do
  begin
    Content := FGeneratedContent[I];
    Content.Metadata.Free;
  end;
  
  // Liberar cache
  for var Pair in FContentCache do
    Pair.Value.Metadata.Free;
    
  // Liberar keywords
  for var Pair in FTopicKeywords do
    Pair.Value.Free;
    
  // Liberar templates de linguagem
  for var Pair in FLanguageTemplates do
    Pair.Value.Free;
    
  FLanguageTemplates.Free;
  FTopicKeywords.Free;
  FContentCache.Free;
  FGeneratedContent.Free;
  FTemplates.Free;
  
  inherited Destroy;
end;

function TContentGenerator.GenerateContent(const Request: TContentRequest): TGeneratedContent;
var
  CacheKey: string;
  Prompt: string;
  RawContent: string;
  GenerationRequest: TGenerationRequest;
  Attempt: Integer;
  StartTime: TDateTime;
begin
  StartTime := Now;
  
  // Verificar cache primeiro
  CacheKey := GetCacheKey(Request);
  if FContentCache.TryGetValue(CacheKey, Result) then
  begin
    if Assigned(FOnContentGenerated) then
      FOnContentGenerated(Self, Result);
    Exit;
  end;
  
  // Inicializar resultado
  FillChar(Result, SizeOf(Result), 0);
  Result.Metadata := TJSONObject.Create;
  Result.ContentType := Request.ContentType;
  Result.GenerationTime := StartTime;
  
  if Assigned(FOnProgress) then
    FOnProgress(Self, 10, 'Construindo prompt...');
  
  // Construir prompt
  Prompt := BuildPrompt(Request);
  
  if Assigned(FOnProgress) then
    FOnProgress(Self, 30, 'Gerando conteúdo...');
  
  // Configurar requisição
  GenerationRequest.Model := FCurrentModel;
  GenerationRequest.Prompt := Prompt;
  GenerationRequest.Temperature := FDefaultTemperature;
  GenerationRequest.MaxTokens := FDefaultMaxTokens;
  GenerationRequest.Stream := False;
  
  // Tentar gerar conteúdo
  for Attempt := 1 to FRetryCount do
  begin
    try
      RawContent := FOllamaAPI.Generate(GenerationRequest);
      
      if RawContent <> '' then
      begin
        if Assigned(FOnProgress) then
          FOnProgress(Self, 70, 'Processando conteúdo...');
          
        // Pós-processar conteúdo
        Result.Content := PostProcessContent(RawContent, Request);
        Result.Title := ExtractTitle(Result.Content);
        Result.WordCount := CountWords(Result.Content);
        Result.EstimatedReadingTime := EstimateReadingTime(Result.WordCount);
        Result.QualityScore := CalculateQualityScore(Result.Content, Request);
        
        // Validar qualidade
        if Result.QualityScore >= FQualityThreshold then
        begin
          if Assigned(FOnProgress) then
            FOnProgress(Self, 90, 'Finalizando...');
            
          // Adicionar exemplos se solicitado
          if Request.IncludeExamples then
            Result.Content := EnhanceWithExamples(Result.Content, Request);
            
          // Adicionar exercícios se solicitado
          if Request.IncludeExercises then
            Result.Content := AddExercisesToContent(Result.Content, Request);
            
          // Aplicar formatação
          Result.Content := ApplyFormatting(Result.Content, Request.Format);
          
          // Adicionar metadados
          Result.Metadata.AddPair('generation_time', DateTimeToStr(Now - StartTime));
          Result.Metadata.AddPair('model_used', FCurrentModel);
          Result.Metadata.AddPair('attempt', TJSONNumber.Create(Attempt));
          Result.Metadata.AddPair('prompt_length', TJSONNumber.Create(Length(Prompt)));
          Result.Metadata.AddPair('language', GetEnumName(TypeInfo(TProgrammingLanguage), Ord(Request.Language)));
          Result.Metadata.AddPair('difficulty', GetEnumName(TypeInfo(TDifficultyLevel), Ord(Request.Difficulty)));
          
          // Armazenar no cache
          FContentCache.Add(CacheKey, Result);
          FGeneratedContent.Add(Result);
          
          if Assigned(FOnProgress) then
            FOnProgress(Self, 100, 'Conteúdo gerado com sucesso!');
            
          if Assigned(FOnContentGenerated) then
            FOnContentGenerated(Self, Result);
            
          Exit;
        end;
      end;
      
    except
      on E: Exception do
      begin
        if Attempt = FRetryCount then
        begin
          Result.Content := 'Erro na geração: ' + E.Message;
          Result.QualityScore := 0.0;
          Result.Metadata.AddPair('error', E.Message);
          Exit;
        end;
      end;
    end;
    
    // Aguardar antes da próxima tentativa
    Sleep(1000 * Attempt);
  end;
  
  Result.Content := 'Falha na geração após ' + IntToStr(FRetryCount) + ' tentativas';
  Result.QualityScore := 0.0;
end;

function TContentGenerator.BuildPrompt(const Request: TContentRequest): string;
var
  PromptBuilder: TStringBuilder;
  Template: TContentTemplate;
  Keywords: string;
  I: Integer;
begin
  PromptBuilder := TStringBuilder.Create;
  try
    // Prompt base baseado no tipo de conteúdo
    case Request.ContentType of
      ctProgrammingTutorial:
      begin
        PromptBuilder.AppendLine('Crie um tutorial completo de programação sobre o seguinte tópico:');
        PromptBuilder.AppendLine('Tópico: ' + Request.Topic);
        PromptBuilder.AppendLine('Linguagem: ' + GetEnumName(TypeInfo(TProgrammingLanguage), Ord(Request.Language)));
        PromptBuilder.AppendLine('Nível: ' + GetEnumName(TypeInfo(TDifficultyLevel), Ord(Request.Difficulty)));
        PromptBuilder.AppendLine('');
        PromptBuilder.AppendLine('O tutorial deve incluir:');
        PromptBuilder.AppendLine('- Introdução clara ao conceito');
        PromptBuilder.AppendLine('- Explicação passo a passo');
        PromptBuilder.AppendLine('- Exemplos práticos de código');
        PromptBuilder.AppendLine('- Dicas e melhores práticas');
        PromptBuilder.AppendLine('- Exercícios para praticar');
      end;
      
      ctCodeSnippet:
      begin
        PromptBuilder.AppendLine('Gere um exemplo de código funcional para:');
        PromptBuilder.AppendLine('Descrição: ' + Request.Topic);
        PromptBuilder.AppendLine('Linguagem: ' + GetEnumName(TypeInfo(TProgrammingLanguage), Ord(Request.Language)));
        PromptBuilder.AppendLine('Nível: ' + GetEnumName(TypeInfo(TDifficultyLevel), Ord(Request.Difficulty)));
        PromptBuilder.AppendLine('');
        PromptBuilder.AppendLine('O código deve ser:');
        PromptBuilder.AppendLine('- Completo e funcional');
        PromptBuilder.AppendLine('- Bem comentado');
        PromptBuilder.AppendLine('- Seguir boas práticas');
        PromptBuilder.AppendLine('- Incluir tratamento de erros quando apropriado');
      end;
      
      ctArticle:
      begin
        PromptBuilder.AppendLine('Escreva um artigo técnico detalhado sobre:');
        PromptBuilder.AppendLine('Tópico: ' + Request.Topic);
        PromptBuilder.AppendLine('Nível: ' + GetEnumName(TypeInfo(TDifficultyLevel), Ord(Request.Difficulty)));
        PromptBuilder.AppendLine('');
        PromptBuilder.AppendLine('O artigo deve ter:');
        PromptBuilder.AppendLine('- Introdução envolvente');
        PromptBuilder.AppendLine('- Desenvolvimento estruturado com subtítulos');
        PromptBuilder.AppendLine('- Exemplos práticos');
        PromptBuilder.AppendLine('- Conclusão com pontos-chave');
      end;
      
      ctDocumentation:
      begin
        PromptBuilder.AppendLine('Crie documentação técnica completa para:');
        PromptBuilder.AppendLine('Tópico: ' + Request.Topic);
        PromptBuilder.AppendLine('');
        PromptBuilder.AppendLine('A documentação deve incluir:');
        PromptBuilder.AppendLine('- Visão geral e propósito');
        PromptBuilder.AppendLine('- Requisitos e pré-requisitos');
        PromptBuilder.AppendLine('- Instruções de instalação/configuração');
        PromptBuilder.AppendLine('- Exemplos de uso');
        PromptBuilder.AppendLine('- API/referência se aplicável');
        PromptBuilder.AppendLine('- Solução de problemas comuns');
      end;
      
      ctQA:
      begin
        PromptBuilder.AppendLine('Crie um conjunto de perguntas e respostas sobre:');
        PromptBuilder.AppendLine('Tópico: ' + Request.Topic);
        PromptBuilder.AppendLine('Nível: ' + GetEnumName(TypeInfo(TDifficultyLevel), Ord(Request.Difficulty)));
        PromptBuilder.AppendLine('');
        PromptBuilder.AppendLine('Gere pelo menos 10 pares de pergunta-resposta que cubram:');
        PromptBuilder.AppendLine('- Conceitos fundamentais');
        PromptBuilder.AppendLine('- Aplicações práticas');
        PromptBuilder.AppendLine('- Problemas comuns e soluções');
        PromptBuilder.AppendLine('- Cenários avançados');
      end;
    end;
    
    // Adicionar palavras-chave se fornecidas
    if Length(Request.Keywords) > 0 then
    begin
      Keywords := '';
      for I := 0 to High(Request.Keywords) do
      begin
        if I > 0 then
          Keywords := Keywords + ', ';
        Keywords := Keywords + Request.Keywords[I];
      end;
      PromptBuilder.AppendLine('');
      PromptBuilder.AppendLine('Palavras-chave a incluir: ' + Keywords);
    end;
    
    // Adicionar prompt customizado se fornecido
    if Request.CustomPrompt <> '' then
    begin
      PromptBuilder.AppendLine('');
      PromptBuilder.AppendLine('Instruções adicionais: ' + Request.CustomPrompt);
    end;
    
    // Especificar formato se diferente de plain
    if Request.Format <> 'plain' then
    begin
      PromptBuilder.AppendLine('');
      PromptBuilder.AppendLine('Formato de saída: ' + Request.Format);
    end;
    
    // Especificar comprimento aproximado
    if Request.Length > 0 then
    begin
      PromptBuilder.AppendLine('');
      PromptBuilder.AppendLine('Comprimento aproximado: ' + IntToStr(Request.Length) + ' palavras');
    end;
    
    Result := PromptBuilder.ToString;
    
  finally
    PromptBuilder.Free;
  end;
end;

function TContentGenerator.PostProcessContent(const RawContent: string; const Request: TContentRequest): string;
var
  ProcessedContent: string;
  Lines: TStringList;
  I: Integer;
begin
  ProcessedContent := Trim(RawContent);
  
  // Remover possíveis artefatos do modelo
  ProcessedContent := StringReplace(ProcessedContent, '```', '', [rfReplaceAll]);
  ProcessedContent := StringReplace(ProcessedContent, 'Human:', '', [rfReplaceAll]);
  ProcessedContent := StringReplace(ProcessedContent, 'Assistant:', '', [rfReplaceAll]);
  
  // Normalizar quebras de linha
  ProcessedContent := StringReplace(ProcessedContent, #13#10, #10, [rfReplaceAll]);
  ProcessedContent := StringReplace(ProcessedContent, #13, #10, [rfReplaceAll]);
  
  // Limpar linhas vazias excessivas
  Lines := TStringList.Create;
  try
    Lines.Text := ProcessedContent;
    
    // Remover linhas vazias consecutivas
    for I := Lines.Count - 1 downto 1 do
    begin
      if (Trim(Lines[I]) = '') and (Trim(Lines[I-1]) = '') then
        Lines.Delete(I);
    end;
    
    ProcessedContent := Lines.Text;
    
  finally
    Lines.Free;
  end;
  
  // Aplicar formatação específica baseada no tipo de conteúdo
  case Request.ContentType of
    ctCodeSnippet:
    begin
      // Garantir que código esteja bem formatado
      if not ProcessedContent.Contains('```') then
      begin
        var LangName := GetEnumName(TypeInfo(TProgrammingLanguage), Ord(Request.Language));
        LangName := LowerCase(StringReplace(LangName, 'pl', '', [rfReplaceAll]));
        ProcessedContent := '```' + LangName + #10 + ProcessedContent + #10 + '```';
      end;
    end;
    
    ctProgrammingTutorial, ctArticle:
    begin
      // Garantir que tenha título se não houver
      if not (ProcessedContent.StartsWith('#') or ProcessedContent.StartsWith('**')) then
      begin
        var Title := ExtractTitle(ProcessedContent);
        if Title = '' then
          Title := Request.Topic;
        ProcessedContent := '# ' + Title + #10#10 + ProcessedContent;
      end;
    end;
  end;
  
  Result := Trim(ProcessedContent);
end;

function TContentGenerator.CalculateQualityScore(const Content: string; const Request: TContentRequest): Double;
var
  Score: Double;
  WordCount: Integer;
  HasTitle, HasStructure, HasExamples, HasConclusion: Boolean;
  KeywordMatches: Integer;
  I: Integer;
begin
  Score := 0.0;
  WordCount := CountWords(Content);
  
  // Pontuação por comprimento adequado
  if (WordCount >= 100) and (WordCount <= 5000) then
    Score := Score + 0.2;
    
  // Verificar estrutura básica
  HasTitle := Content.Contains('#') or Content.Contains('**') or 
             (Pos(UpperCase(Request.Topic), UpperCase(Content)) <= 100);
  if HasTitle then
    Score := Score + 0.1;
    
  HasStructure := (Content.CountChar(#10) >= 10) and 
                  (Content.Contains(':') or Content.Contains('-') or Content.Contains('*'));
  if HasStructure then
    Score := Score + 0.15;
    
  // Verificar exemplos (para conteúdo técnico)
  if Request.ContentType in [ctProgrammingTutorial, ctCodeSnippet, ctDocumentation] then
  begin
    HasExamples := Content.Contains('exemplo') or Content.Contains('```') or 
                   Content.Contains('function') or Content.Contains('class');
    if HasExamples then
      Score := Score + 0.2;
  end;
  
  // Verificar conclusão
  HasConclusion := Content.Contains('conclusão') or Content.Contains('resumo') or
                   Content.Contains('concluindo') or Content.Contains('em suma');
  if HasConclusion then
    Score := Score + 0.1;
    
  // Verificar correspondência de palavras-chave
  KeywordMatches := 0;
  for I := 0 to High(Request.Keywords) do
  begin
    if ContainsText(Content, Request.Keywords[I]) then
      Inc(KeywordMatches);
  end;
  
  if Length(Request.Keywords) > 0 then
    Score := Score + (KeywordMatches / Length(Request.Keywords)) * 0.15;
    
  // Verificar qualidade geral do texto
  if not Content.Contains('erro') and not Content.Contains('falha') and
     not Content.Contains('não foi possível') then
    Score := Score + 0.1;
    
  Result := Min(1.0, Score);
end;

function TContentGenerator.ExtractTitle(const Content: string): string;
var
  Lines: TStringList;
  I: Integer;
  Line: string;
begin
  Result := '';
  
  Lines := TStringList.Create;
  try
    Lines.Text := Content;
    
    for I := 0 to Min(5, Lines.Count - 1) do // Verificar primeiras 5 linhas
    begin
      Line := Trim(Lines[I]);
      
      // Verificar título Markdown
      if Line.StartsWith('#') then
      begin
        Result := Trim(StringReplace(Line, '#', '', [rfReplaceAll]));
        Break;
      end;
      
      // Verificar título em negrito
      if Line.StartsWith('**') and Line.EndsWith('**') then
      begin
        Result := Trim(StringReplace(Line, '**', '', [rfReplaceAll]));
        Break;
      end;
      
      // Se for a primeira linha não vazia, usar como título
      if (I = 0) and (Line <> '') and (Length(Line) < 100) then
      begin
        Result := Line;
        Break;
      end;
    end;
    
  finally
    Lines.Free;
  end;
  
  if Result = '' then
    Result := 'Conteúdo Gerado';
end;

function TContentGenerator.CountWords(const Text: string): Integer;
var
  Words: TArray<string>;
begin
  Words := Text.Split([' ', #9, #10, #13], TStringSplitOptions.ExcludeEmpty);
  Result := Length(Words);
end;

function TContentGenerator.EstimateReadingTime(WordCount: Integer): Integer;
begin
  // Assumir 200 palavras por minuto de leitura
  Result := Max(1, Round(WordCount / 200));
end;

function TContentGenerator.ValidateContent(const Content: string; const Request: TContentRequest): Boolean;
begin
  Result := (Trim(Content) <> '') and 
            (CountWords(Content) >= 50) and
            (not Content.Contains('erro na geração')) and
            (CalculateQualityScore(Content, Request) >= FQualityThreshold);
end;

function TContentGenerator.GetCacheKey(const Request: TContentRequest): string;
var
  KeyBuilder: TStringBuilder;
  I: Integer;
begin
  KeyBuilder := TStringBuilder.Create;
  try
    KeyBuilder.Append(GetEnumName(TypeInfo(TContentType), Ord(Request.ContentType)));
    KeyBuilder.Append('|');
    KeyBuilder.Append(Request.Topic);
    KeyBuilder.Append('|');
    KeyBuilder.Append(GetEnumName(TypeInfo(TProgrammingLanguage), Ord(Request.Language)));
    KeyBuilder.Append('|');
    KeyBuilder.Append(GetEnumName(TypeInfo(TDifficultyLevel), Ord(Request.Difficulty)));
    KeyBuilder.Append('|');
    KeyBuilder.Append(IntToStr(Request.Length));
    
    for I := 0 to High(Request.Keywords) do
    begin
      KeyBuilder.Append('|');
      KeyBuilder.Append(Request.Keywords[I]);
    end;
    
    Result := THashMD5.GetHashString(KeyBuilder.ToString);
    
  finally
    KeyBuilder.Free;
  end;
end;

// Implementação dos métodos principais de geração específica

function TContentGenerator.GenerateProgrammingTutorial(const Topic: string; 
  Language: TProgrammingLanguage; Difficulty: TDifficultyLevel): TGeneratedContent;
var
  Request: TContentRequest;
begin
  Request.ContentType := ctProgrammingTutorial;
  Request.Topic := Topic;
  Request.Language := Language;
  Request.Difficulty := Difficulty;
  Request.Length := 1500;
  Request.Format := 'markdown';
  Request.IncludeExamples := True;
  Request.IncludeExercises := True;
  
  Result := GenerateContent(Request);
end;

function TContentGenerator.GenerateCodeDocumentation(const Code: string; 
  Language: TProgrammingLanguage): TGeneratedContent;
var
  Request: TContentRequest;
begin
  Request.ContentType := ctDocumentation;
  Request.Topic := 'Documentação para o seguinte código:'#10#10 + Code;
  Request.Language := Language;
  Request.Difficulty := dlIntermediate;
  Request.Length := 800;
  Request.Format := 'markdown';
  Request.IncludeExamples := True;
  
  Result := GenerateContent(Request);
end;

function TContentGenerator.GenerateTechnicalArticle(const Topic: string; 
  Difficulty: TDifficultyLevel): TGeneratedContent;
var
  Request: TContentRequest;
begin
  Request.ContentType := ctArticle;
  Request.Topic := Topic;
  Request.Language := plPascal; // Default
  Request.Difficulty := Difficulty;
  Request.Length := 2000;
  Request.Format := 'markdown';
  Request.IncludeExamples := True;
  
  Result := GenerateContent(Request);
end;

function TContentGenerator.GenerateCodeExample(const Description: string; 
  Language: TProgrammingLanguage; Difficulty: TDifficultyLevel): string;
var
  Request: TContentRequest;
  Content: TGeneratedContent;
begin
  Request.ContentType := ctCodeSnippet;
  Request.Topic := Description;
  Request.Language := Language;
  Request.Difficulty := Difficulty;
  Request.Length := 300;
  Request.Format := 'plain';
  
  Content := GenerateContent(Request);
  Result := Content.Content;
  Content.Metadata.Free;
end;

// Implementações dos métodos utilitários e configuração

procedure TContentGenerator.InitializeTemplates;
var
  Template: TContentTemplate;
begin
  // Template para tutorial de programação
  Template.Name := 'ProgrammingTutorial';
  Template.Description := 'Template para tutoriais de programação';
  Template.ContentType := ctProgrammingTutorial;
  Template.Template := 'Tutorial: {TOPIC}'#10'Linguagem: {LANGUAGE}'#10'Nível: {DIFFICULTY}';
  Template.Variables := TStringList.Create;
  Template.Variables.Add('TOPIC');
  Template.Variables.Add('LANGUAGE');
  Template.Variables.Add('DIFFICULTY');
  Template.MinLength := 500;
  Template.MaxLength := 3000;
  FTemplates.Add(Template);
  
  // Adicionar mais templates conforme necessário
end;

procedure TContentGenerator.InitializeLanguageTemplates;
var
  PascalTemplates: TStringList;
begin
  // Templates específicos para Pascal
  PascalTemplates := TStringList.Create;
  PascalTemplates.Add('program ExemploBasico;');
  PascalTemplates.Add('unit NomeUnit;');
  PascalTemplates.Add('function MinhaFuncao: TipoRetorno;');
  PascalTemplates.Add('procedure MinhaProcedure;');
  FLanguageTemplates.Add(plPascal, PascalTemplates);
  
  // Adicionar templates para outras linguagens...
end;

procedure TContentGenerator.LoadTopicKeywords;
var
  ProgrammingKeywords: TStringList;
begin
  // Keywords para programação
  ProgrammingKeywords := TStringList.Create;
  ProgrammingKeywords.Add('algoritmo');
  ProgrammingKeywords.Add('estrutura de dados');
  ProgrammingKeywords.Add('função');
  ProgrammingKeywords.Add('variável');
  ProgrammingKeywords.Add('loop');
  ProgrammingKeywords.Add('condicional');
  FTopicKeywords.Add('programming', ProgrammingKeywords);
  
  // Adicionar keywords para outros tópicos...
end;

function TContentGenerator.LoadTemplates: Boolean;
begin
  // Carregar templates de arquivo se existir
  Result := True; // Placeholder
end;

function TContentGenerator.EnhanceWithExamples(const Content: string; 
  const Request: TContentRequest): string;
var
  Enhanced: TStringBuilder;
begin
  Enhanced := TStringBuilder.Create;
  try
    Enhanced.Append(Content);
    Enhanced.AppendLine('');
    Enhanced.AppendLine('## Exemplos Práticos');
    Enhanced.AppendLine('');
    
    case Request.ContentType of
      ctProgrammingTutorial:
      begin
        Enhanced.AppendLine('```' + LowerCase(GetEnumName(TypeInfo(TProgrammingLanguage), Ord(Request.Language)).Substring(2)));
        Enhanced.AppendLine('// Exemplo prático do conceito explicado');
        Enhanced.AppendLine('// TODO: Implementar exemplo específico');
        Enhanced.AppendLine('```');
      end;
    end;
    
    Result := Enhanced.ToString;
  finally
    Enhanced.Free;
  end;
end;

function TContentGenerator.AddExercisesToContent(const Content: string; 
  const Request: TContentRequest): string;
var
  Enhanced: TStringBuilder;
begin
  Enhanced := TStringBuilder.Create;
  try
    Enhanced.Append(Content);
    Enhanced.AppendLine('');
    Enhanced.AppendLine('## Exercícios Práticos');
    Enhanced.AppendLine('');
    Enhanced.AppendLine('1. **Exercício Básico**: Implemente o conceito básico apresentado.');
    Enhanced.AppendLine('2. **Exercício Intermediário**: Expanda o exemplo com funcionalidades extras.');
    Enhanced.AppendLine('3. **Desafio**: Crie uma aplicação completa usando os conceitos aprendidos.');
    
    Result := Enhanced.ToString;
  finally
    Enhanced.Free;
  end;
end;

function TContentGenerator.ApplyFormatting(const Content: string; const Format: string): string;
begin
  case LowerCase(Format) of
    'html':
    begin
      Result := '<html><body>' + StringReplace(Content, #10, '<br>', [rfReplaceAll]) + '</body></html>';
    end;
    'markdown':
    begin
      Result := Content; // Já está em markdown
    end;
  else
    Result := Content; // Plain text
  end;
end;

// Implementações placeholder para métodos restantes
function TContentGenerator.GenerateContentAsync(const Request: TContentRequest): ITask;
begin
  Result := TTask.Create(
    procedure
    begin
      GenerateContent(Request);
    end);
  Result.Start;
end;

function TContentGenerator.GenerateBatchContent(const Requests: TArray<TContentRequest>): TArray<TGeneratedContent>;
var
  I: Integer;
begin
  SetLength(Result, Length(Requests));
  for I := 0 to High(Requests) do
    Result[I] := GenerateContent(Requests[I]);
end;

procedure TContentGenerator.ClearCache;
begin
  for var Pair in FContentCache do
    Pair.Value.Metadata.Free;
  FContentCache.Clear;
end;

// Métodos de configuração e utilidades restantes (implementações simplificadas)
function TContentGenerator.GetTemplate(ContentType: TContentType; Language: TProgrammingLanguage): TContentTemplate;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Variables := TStringList.Create;
end;

function TContentGenerator.GenerateExerciseSet(const Topic: string; Language: TProgrammingLanguage; 
  Difficulty: TDifficultyLevel; Count: Integer): TGeneratedContent;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Metadata := TJSONObject.Create;
end;

function TContentGenerator.GenerateQASet(const Content: string; Count: Integer): TGeneratedContent;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Metadata := TJSONObject.Create;
end;

function TContentGenerator.GenerateCodeExplanation(const Code: string; Language: TProgrammingLanguage): string;
begin
  Result := 'Explicação do código: ' + Code;
end;

function TContentGenerator.GenerateCodeRefactoring(const Code: string; Language: TProgrammingLanguage): string;
begin
  Result := 'Versão refatorada: ' + Code;
end;

function TContentGenerator.GenerateCodeComments(const Code: string; Language: TProgrammingLanguage): string;
begin
  Result := '// Código comentado'#10 + Code;
end;

function TContentGenerator.GenerateUnitTests(const Code: string; Language: TProgrammingLanguage): string;
begin
  Result := 'Testes unitários para: ' + Code;
end;

function TContentGenerator.GenerateArticleFromOutline(const Outline: TStringList; Difficulty: TDifficultyLevel): string;
begin
  Result := 'Artigo baseado no outline: ' + Outline.Text;
end;

function TContentGenerator.AnalyzeContentQuality(const Content: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('word_count', TJSONNumber.Create(CountWords(Content)));
  Result.AddPair('estimated_reading_time', TJSONNumber.Create(EstimateReadingTime(CountWords(Content))));
end;

function TContentGenerator.GetSupportedLanguages: TArray<TProgrammingLanguage>;
begin
  SetLength(Result, 14);
  Result[0] := plPascal;
  Result[1] := plPython;
  Result[2] := plJavaScript;
  Result[3] := plJava;
  Result[4] := plCSharp;
  Result[5] := plCPP;
  Result[6] := plC;
  Result[7] := plGo;
  Result[8] := plRust;
  Result[9] := plTypeScript;
  Result[10] := plPHP;
  Result[11] := plRuby;
  Result[12] := plSwift;
  Result[13] := plKotlin;
end;

function TContentGenerator.GetSupportedContentTypes: TArray<TContentType>;
begin
  SetLength(Result, 9);
  Result[0] := ctProgrammingTutorial;
  Result[1] := ctCodeSnippet;
  Result[2] := ctArticle;
  Result[3] := ctDocumentation;
  Result[4] := ctQA;
  Result[5] := ctBlogPost;
  Result[6] := ctTechnicalGuide;
  Result[7] := ctExercise;
  Result[8] := ctExample;
end;

// Implementações restantes simplificadas...
function TContentGenerator.GenerateCodeSnippet(const Topic: string; Language: TProgrammingLanguage; Difficulty: TDifficultyLevel): string;
begin
  Result := 'Código para: ' + Topic;
end;

function TContentGenerator.GenerateArticleOutline(const Topic: string; Difficulty: TDifficultyLevel): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('1. Introdução');
  Result.Add('2. Desenvolvimento');
  Result.Add('3. Conclusão');
end;

function TContentGenerator.GenerateExercises(const Topic: string; Language: TProgrammingLanguage; Count: Integer): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Exercício 1: Implementar ' + Topic);
end;

function TContentGenerator.GenerateQAFromContent(const Content: string; Count: Integer): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Q: O que é abordado no conteúdo?');
  Result.Add('A: ' + Copy(Content, 1, 100) + '...');
end;

// Métodos de gerenciamento, configuração e utilitários (implementações básicas)
function TContentGenerator.AddCustomTemplate(const Template: TContentTemplate): Boolean;
begin
  Result := True;
end;

function TContentGenerator.UpdateTemplate(const Name: string; const Template: TContentTemplate): Boolean;
begin
  Result := True;
end;

function TContentGenerator.DeleteTemplate(const Name: string): Boolean;
begin
  Result := True;
end;

function TContentGenerator.GetAvailableTemplates: TArray<TContentTemplate>;
begin
  SetLength(Result, 0);
end;

function TContentGenerator.ExportTemplates(const FileName: string): Boolean;
begin
  Result := True;
end;

function TContentGenerator.ImportTemplates(const FileName: string): Boolean;
begin
  Result := True;
end;

function TContentGenerator.GetCacheStatistics: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('cache_size', TJSONNumber.Create(FContentCache.Count));
end;

procedure TContentGenerator.OptimizeCache;
begin
  // Implementar otimização de cache
end;

function TContentGenerator.PreloadContent(const Topics: TArray<string>): Boolean;
begin
  Result := True;
end;

procedure TContentGenerator.LoadConfiguration(const ConfigFile: string);
begin
  // Implementar carregamento de configuração
end;

procedure TContentGenerator.SaveConfiguration(const ConfigFile: string);
begin
  // Implementar salvamento de configuração
end;

function TContentGenerator.GetContentSuggestions(const Topic: string): TArray<string>;
begin
  SetLength(Result, 3);
  Result[0] := 'Tutorial sobre ' + Topic;
  Result[1] := 'Exemplos de ' + Topic;
  Result[2] := 'Exercícios de ' + Topic;
end;

function TContentGenerator.ImproveContent(const Content: string; const Feedback: string): string;
begin
  Result := Content + #10#10'// Melhorado baseado no feedback: ' + Feedback;
end;

function TContentGenerator.TranslateContent(const Content: string; const TargetLanguage: string): string;
begin
  Result := '[TRADUZIDO PARA ' + TargetLanguage + '] ' + Content;
end;

function TContentGenerator.SummarizeContent(const Content: string; MaxWords: Integer): string;
begin
  Result := Copy(Content, 1, MaxWords * 5); // Aproximação
end;

function TContentGenerator.GenerateDocumentationFromCode(const CodeFiles: TStringList; Language: TProgrammingLanguage): string;
begin
  Result := 'Documentação gerada para ' + IntToStr(CodeFiles.Count) + ' arquivos';
end;

function TContentGenerator.GenerateBlogPostFromTopic(const Topic: string; Keywords: TArray<string>): string;
begin
  Result := 'Blog post sobre: ' + Topic;
end;

function TContentGenerator.GenerateAPIDocumentation(const APIDescription: string): string;
begin
  Result := 'Documentação da API: ' + APIDescription;
end;

end.