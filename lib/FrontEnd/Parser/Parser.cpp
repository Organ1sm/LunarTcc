#include "FrontEnd/Parser/Parser.hpp"
#include "FrontEnd/AST/AST.hpp"
#include "FrontEnd/Support.hpp"
#include "MiddleEnd/IR/IRFactory.hpp"
#include "Utils/DiagnosticPrinter.hpp"
#include "fmt/core.h"
#include <algorithm>
#include <memory>
#include <tuple>

static bool IsUnsupported(const Token &T)
{
    switch (T.GetKind())
    {
        case Token::Goto:
        case Token::Alignas:
        case Token::Alignof:
        case Token::Atomic:
        case Token::Complex:
        case Token::Generic:
        case Token::Imaginary:
        case Token::Noreturn:
        case Token::StaticAssert:
        case Token::ThreadLocal: return true;

        default: return false;
    }
}

bool Parser::IsTypeSpecifier(Token T)
{
    switch (T.GetKind())
    {
        case Token::Void:
        case Token::Char:
        case Token::Short:
        case Token::Int:
        case Token::Long:
        case Token::Float:
        case Token::Double:
        case Token::Signed:
        case Token::Unsigned:
        case Token::Struct: return true;

        case Token::Identifier: {
            auto Id = T.GetString();
            if (TypeDefinitions.count(Id) != 0)
                return true;
        }

        default: break;
    }

    return false;
}

static bool IsUnaryOperator(Token::TokenKind tk)
{
    switch (tk)
    {
        case Token::Tilde:
        case Token::Sizeof:
        case Token::Inc:
        case Token::Dec:
        case Token::And:
        case Token::Minus:
        case Token::Not:
        case Token::Mul: return true;

        default: return false;
    }
}

bool Parser::IsReturnTypeSpecifier(Token T)
{
    return T.GetKind() == Token::Void || IsTypeSpecifier(T);
}

bool Parser::IsQualifer(Token::TokenKind tk)
{
    switch (tk)
    {
        case Token::TypeDef:
        case Token::Const: return true;

        default: break;
    }

    return false;
}

bool Parser::IsQualifedType(Token T)
{
    return IsQualifer(T.GetKind()) || IsTypeSpecifier(T);
}

bool Parser::IsUserDefinedType(const std::string &Name)
{
    return UserDefinedTypes.count(Name) > 0 || TypeDefinitions.count(Name);
}

Type Parser::GetUserDefinedType(const std::string &Name)
{
    assert(IsUserDefinedType(Name));

    if (UserDefinedTypes.count(Name) > 0)
        return std::get<0>(UserDefinedTypes[Name]);
    else
        return TypeDefinitions[Name];
}

std::vector<Token> Parser::GetUserDefinedTypeMembers(std::string Name)
{
    assert(IsUserDefinedType(Name));

    if (TypeDefinitions.count(Name) > 0)
        Name = TypeDefinitions[Name].GetName();

    return std::get<1>(UserDefinedTypes[Name]);
}

unsigned Parser::ParseQualifiers()
{
    unsigned Qualifiers   = 0;
    auto CurrentTokenKind = GetCurrentTokenKind();

    while (IsQualifer(CurrentTokenKind))
    {
        Lex();    // eat the qualifier token

        switch (CurrentTokenKind)
        {
            case Token::TypeDef: Qualifiers |= Type::TypeDef; break;
            case Token::Const: Qualifiers |= Type::Const; break;

            default: break;
        }

        CurrentTokenKind = GetCurrentTokenKind();
    }

    return Qualifiers;
}

Type Parser::ParseType(Token::TokenKind tk)
{
    Type Result;
    switch (tk)
    {
        case Token::Void: Result.SetTypeVariant(Type::Void); break;
        case Token::Char: Result.SetTypeVariant(Type::Char); break;
        case Token::Short: Result.SetTypeVariant(Type::Short); break;
        case Token::Int: Result.SetTypeVariant(Type::Int); break;
        case Token::Float: Result.SetTypeVariant(Type::Float); break;
        case Token::Double: Result.SetTypeVariant(Type::Double); break;
        case Token::Long: {
            auto NextTokenKind = lexer.LookAhead(2).GetKind();
            if (NextTokenKind == Token::Long)
            {
                Lex();    // eat 'long'
                Result.SetTypeVariant(Type::LongLong);
                break;
            }
            Result.SetTypeVariant(Type::Long);
            break;
        }

        case Token::Signed: {
            Lex();
            auto CurrentToken = lexer.GetCurrentToken();
            Result            = ParseType(CurrentToken.GetKind());

            // TODO: Move this into semantics. For now here is the most appropriate
            // to handle it.
            if (Result.IsUnsigned())
            {
                std::string Msg =
                    "both 'signed' and 'unsigned' in declaration specifiers";
                DiagPrinter.AddError(Msg, CurrentToken);
            }

            break;
        }

        case Token::Unsigned: {
            auto NextTokenKind = lexer.LookAhead(2).GetKind();
            if (NextTokenKind == Token::Int || NextTokenKind == Token::Char ||
                NextTokenKind == Token::Short || NextTokenKind == Token::Long)
            {
                Lex();    // eat 'unsigned'
            }
            else
            {
                // if bare the 'unsigned' is not followed by other type then its an
                // 'unsigned int' by default
                Result.SetTypeVariant(Type::UnsignedInt);
                return Result;
            }

            auto CurrentToken = lexer.GetCurrentToken();
            switch (CurrentToken.GetKind())
            {
                case Token::Char: Result.SetTypeVariant(Type::UnsignedChar); break;
                case Token::Short: Result.SetTypeVariant(Type::UnsignedShort); break;
                case Token::Int: Result.SetTypeVariant(Type::UnsignedInt); break;
                case Token::Long: {
                    auto NextTokenKind = lexer.LookAhead(2).GetKind();
                    if (NextTokenKind == Token::Long)
                    {
                        Lex();    // eat 'long';
                        Result.SetTypeVariant(Type::UnsignedLongLong);
                        break;
                    }
                    Result.SetTypeVariant(Type::UnsignedLong);
                    break;
                }

                default: assert(!"Unreachable");
            }
            break;
        }

        case Token::Struct: {
            Lex();    // eat 'struct'

            auto CurrentTK   = lexer.GetCurrentToken();
            std::string Name = CurrentTK.GetString();

            Result = std::get<0>(UserDefinedTypes[Name]);
            break;
        }

        case Token::Identifier: {
            // assuming we parsing the current token
            // TODO: Change this function expect the Token and not the TokenKind
            assert(GetCurrentTokenKind() == Token::Identifier);
            auto Id = GetCurrentToken().GetString();
            Result  = TypeDefinitions[Id];
            break;
        }

        default: assert(!"Unknown token kind."); break;
    }

    return Result;
}

/// Parse the dimensions of an array declaration
void Parser::ParseArrayDimensions(Type &type)
{
    std::vector<unsigned> Dimensions;
    while (lexer.Is(Token::LeftBracket))
    {
        Lex();    // consume '['

        // empty dimension like "int arr []"
        if (lexer.Is(Token::RightBracket))
        {
            Lex();
            // using  ~0 to flag an unspecified dimension
            Dimensions.push_back(EmptyDimension);
        }
        else
        {
            // dimension defined by constant case like "int arr[5][5]"
            Dimensions.push_back(ParseIntegerConstant());
            Expect(Token::RightBracket);
        }
    }

    if (!Dimensions.empty())
        type.SetDimensions(std::move(Dimensions));
}

/// Issues an implicit cast for the expr if it is needed and possible
/// for the given @ExpectedType
void DoImplicitCastIfNeed(std::unique_ptr<Expression> &Expr, const Type &ExpectedType)
{
    if ((ExpectedType != Expr->GetResultType()) &&
        !Type::OnlySignednessDifference(ExpectedType.GetTypeVariant(),
                                        Expr->GetResultType().GetTypeVariant()))
    {
        {
            bool IsImplicitlyCastable =
                Type::IsImplicitlyCastable(Expr->GetResultType(), ExpectedType);

            IsImplicitlyCastable |=
                ExpectedType.IsPointerType() && Expr->GetResultType().IsIntegerType();

            if (!IsImplicitlyCastable)
                assert(!"Invalid initialization");
            else
                Expr = std::make_unique<ImplicitCastExpression>(std::move(Expr),
                                                                ExpectedType);
        }
    }
}

/// Helper function to try to figure out the unspecified dimension of the array
/// type @type from its initializer expression @InitExpr
/// example:
///     int a[] = {1, 2, 3}
///
/// since the initializer expression has 3 element, therefore a type is int[3]
void DetermineUnspecifiedDimension(Expression *InitExpr, Type &type)
{
    // If there was an initializer expression like "{1, 2, 3}",
    if (auto InitListExpr = dynamic_cast<InitializerListExpression *>(InitExpr);
        InitListExpr != nullptr && type.GetDimensions()[0] == Parser::EmptyDimension)
    {
        // TODO: only 1 dimensional init list are handled here now, although C
        // only allows the first dimension to be a unspecified so arr[][] would
        // be invalid anyway
        type.GetDimensions()[0] = InitListExpr->GetExprList().size();
    }
}

Token Parser::Expect(Token::TokenKind TKind)
{
    auto t = GetCurrentToken();

    if (t.GetKind() != TKind)
    {
        if (t.GetKind() != Token::EndOfFile)
        {
            std::string Format = "Unexpected Symbol `{}`. Expected is `{}`.";
            auto Error = fmt::format(Format, t.GetString(), Token::ToString(TKind));

            DiagPrinter.AddError(Error, t);

            if (IsUnsupported(t))
                DiagPrinter.AddNote(fmt::format("'{}' is unsupported\n", t.GetString()));
        }

        else
        {
            std::string Error =
                fmt::format("Reached the end of file, but expected `{}`\n",
                            Token::ToString(TKind));

            DiagPrinter.AddError(Error);
        }

        if (t.GetKind() == Token::Identifier)
            Lex();
    }
    else
        Lex();

    return t;
}

std::unique_ptr<Node> Parser::Parse() { return ParseExternalDeclaration(); }

unsigned Parser::ParseIntegerConstant()
{
    Token T         = Expect(Token::Integer);
    auto TokenStr   = T.GetString();
    unsigned Result = 0;

    // In case if a hex constant, then its value already parsed,
    // return that.
    if (T.GetValue() > 0 || (T.GetString().size() > 2 &&
                             (T.GetString().at(1) == 'x' || T.GetString().at(1) == 'X')))
    {
        return T.GetValue();
    }

    for (auto c : TokenStr)
    {
        Result *= 10;
        Result += c - '0';
    }

    return Result;
}

double Parser::ParseRealConstant()
{
    Token T          = Expect(Token::Real);
    auto TokenStr    = T.GetString();
    double WholePart = 0.0;

    for (auto c : TokenStr)
    {
        if (c == '.')
            break;

        WholePart *= 10;
        WholePart += (c - '0');
    }

    double FractionalPart = 0.0;
    unsigned Divider      = 1;

    for (auto c : TokenStr.substr(TokenStr.find('.') + 1))
    {
        FractionalPart += (c - '0');
        Divider *= 10;
    }

    FractionalPart /= Divider;

    return WholePart + FractionalPart;
}

std::unique_ptr<Node> Parser::ParseTranslationUnit() { return std::unique_ptr<Node>(); }


//=--------------------------------------------------------------------------=//
//=------------------------- Parse Declaration ------------------------------=//
//=--------------------------------------------------------------------------=//

// <ExternalDeclaration> ::= <FunctionDeclaration>
//                         | <VariableDeclaration>
std::unique_ptr<Node> Parser::ParseExternalDeclaration()
{
    std::unique_ptr<TranslationUnit> TU = std::make_unique<TranslationUnit>();
    auto TK                             = GetCurrentToken();

    while (IsReturnTypeSpecifier(TK) || lexer.Is(Token::Struct) ||
           lexer.Is(Token::Enum) || IsQualifer(TK.GetKind()))
    {
        auto Qualifiers = ParseQualifiers();
        TK              = GetCurrentToken();

        bool IsAlsoStructVarDecl = false;
        Type BaseType;

        if (lexer.Is(Token::Struct) &&
            (lexer.LookAhead(3).GetKind() == Token::LeftBrace ||
             (lexer.LookAhead(2).GetKind() == Token::Identifier &&
              lexer.LookAhead(3).GetKind() == Token::LeftBrace)))
        {
            auto SD    = ParseStructDeclaration(Qualifiers);
            auto SDPtr = SD.get();

            TU->AddDeclaration(std::move(SD));
            TK = GetCurrentToken();

            if (TK.GetKind() == Token::Identifier || TK.GetKind() == Token::Mul)
            {
                IsAlsoStructVarDecl = true;
                BaseType            = std::get<0>(UserDefinedTypes[SDPtr->GetName()]);
            }
            else
            {
                Expect(Token::SemiColon);
                TK = GetCurrentToken();
                continue;
            }
        }

        if (lexer.Is(Token::Enum))
        {
            TU->AddDeclaration(ParseEnumDeclaration(Qualifiers));
            TK = GetCurrentToken();
            continue;
        }

        if (!IsAlsoStructVarDecl)
        {
            BaseType = ParseType(TK.GetKind());
            Lex();
        }

        BaseType.SetQualifiers(Qualifiers);
        Type CurrentType = BaseType;

        while (lexer.Is(Token::Mul))
        {
            CurrentType.IncrementPointerLevel();
            Lex();
        }

        auto Name           = Expect(Token::Identifier);
        bool FailedToFindId = (Name.GetKind() != Token::Identifier);
        auto NameStr        = Name.GetString();

        if (Qualifiers & Type::TypeDef)
        {
            TypeDefinitions[NameStr] = CurrentType;
            Expect(Token::SemiColon);
            TK = GetCurrentToken();
            continue;
        }

        // typeSpecifier funcName (T1 a, ...);
        if (lexer.Is(Token::LeftParen) && !FailedToFindId)
        {
            CurrentFuncRetType = CurrentType;

            TU->AddDeclaration(ParseFunctionDeclaration(CurrentType, Name));
        }
        else if (!FailedToFindId)
        {    // Variable Declaration;
             // int a = 0;
             // int arr[x]...
             // int *ptr, foo;
            bool IsFirstIteration = true;
            do
            {
                if (!IsFirstIteration)
                {
                    while (lexer.Is(Token::Mul))
                    {
                        CurrentType.IncrementPointerLevel();
                        Lex();    // eat '*'
                    }

                    Name    = Expect(Token::Identifier);
                    NameStr = Name.GetString();
                }

                IsFirstIteration = false;

                ParseArrayDimensions(CurrentType);

                // If the variable initialized
                std::unique_ptr<Expression> InitExpr {nullptr};
                if (lexer.Is(Token::Assign))
                {
                    Lex();

                    auto ExpectedType = CurrentType;
                    if (ExpectedType.IsArray())
                        ExpectedType.RemoveFirstDimension();

                    const bool IsInitList = lexer.Is(Token::LeftBrace);
                    if (IsInitList)
                        InitExpr = ParseInitializerListExpression(CurrentType);
                    else
                        InitExpr = ParseExpression();

                    if (!IsInitList && ! instanceof
                        <StringLiteralExpression>(InitExpr.get()) &&
                            !ExpectedType.IsStruct())
                    {
                        DoImplicitCastIfNeed(InitExpr, ExpectedType);
                    }
                }


                if (CurrentType.IsArray() && !CurrentType.GetDimensions().empty())
                    DetermineUnspecifiedDimension(InitExpr.get(), CurrentType);

                InsertToSymbolTable(Name, CurrentType);

                TU->AddDeclaration(
                    std::make_unique<VariableDeclaration>(Name,
                                                          CurrentType,
                                                          std::move(InitExpr)));

                // TODO: typedef is not allowed  for now, becase complex TypeDef noto
                // supported like "typedef int int_5, *intptr_t ..."
                if (lexer.Is(Token::Comma) && !BaseType.IsTypeDef())
                    Lex();    // eat ','
                else
                {
                    Expect(Token::SemiColon);
                    break;
                }

                CurrentType = BaseType;
            }
            while (lexer.Is(Token::Mul) || lexer.Is(Token::Identifier));
        }

        else if (Name.GetKind() == Token::LeftParen)
        {
            std::string Msg = "Function pointers are not supported yet";
            DiagPrinter.AddNote(Msg, Name);
        }

        TK = GetCurrentToken();
    }

    if (lexer.IsNot(Token::EndOfFile))
    {
        auto Msg = fmt::format("Unexpected token '{}'", TK.GetString());
        DiagPrinter.AddError(Msg, TK);

        if (IsUnsupported(TK))
        {
            Msg = fmt::format("'{}' is unsupported ", TK.GetString());
            DiagPrinter.AddNote(Msg, TK);
        }
    }

    return TU;
}
/// <FunctionDeclaration> := <ReturnTypeSpecifier> <Identifier>
///                             '(' <ParameterList>? ')' ';'
///                        | <FunctionDefinition>
/// <FunctionDefinition> :=  <ReturnTypeSpecifier> <Identifier>
///                             '(' <ParameterList>? ')' <CompoundStatement>
///
std::unique_ptr<FunctionDeclaration>
    Parser::ParseFunctionDeclaration(const Type &ReturnType, const Token &Name)
{
    Expect(Token::LeftParen);

    // Creating new scope by pushing a new symbol table to the stack.
    SymTabStack.PushSymbolTable();

    bool HasVarArg = false;
    auto PL        = ParseParameterList(HasVarArg);

    Expect(Token::RightParen);

    auto FuncType = FunctionDeclaration::CreateType(ReturnType, PL);
    if (HasVarArg)
        FuncType.SetVarArg(true);

    InsertToSymbolTable(Name, FuncType, true);

    this->ReturnNumber = 0;
    std::unique_ptr<CompoundStatement> Body {nullptr};
    if (lexer.Is(Token::SemiColon))
        Lex();    // eat ';'
    else
        Body = ParseCompoundStatement();

    SymTabStack.PopSymbolTable();

    return std::make_unique<FunctionDeclaration>(FuncType, Name, PL, Body, ReturnNumber);
}

// <ParameterDeclaration> ::= { <TypeSpecifier> '*' <Identifier>? }?
std::unique_ptr<FunctionParameterDeclaration> Parser::ParseParameterDeclaration()
{
    std::unique_ptr<FunctionParameterDeclaration> FPD =
        std::make_unique<FunctionParameterDeclaration>();

    Type Ty = ParseTypeSpecifier();
    Lex();

    while (lexer.Is(Token::Mul))
    {
        Ty.IncrementPointerLevel();
        Lex();    // Eat the `*` character
    }

    if (lexer.Is(Token::Identifier))
    {
        auto IdName = Expect(Token::Identifier);
        FPD->SetName(IdName);

        // support only empty dimensions for now like "int foo(int arr[]) "
        if (lexer.Is(Token::LeftBracket))
        {
            Lex();
            Ty.IncrementPointerLevel();
            Expect(Token::RightBracket);
        }

        InsertToSymbolTable(IdName, Ty);
    }

    FPD->SetType(Ty);

    return FPD;
}

// <ParameterList> ::= <ParameterDeclaration>? {',' <ParameterDeclaration>}* (',' '...')?
std::vector<std::unique_ptr<FunctionParameterDeclaration>>
    Parser::ParseParameterList(bool &HasVarArg)
{
    std::vector<std::unique_ptr<FunctionParameterDeclaration>> Params;

    if (!IsQualifedType(GetCurrentToken()) && lexer.Is(Token::RightParen))
        return Params;

    Params.push_back(ParseParameterDeclaration());

    while (lexer.Is(Token::Comma))
    {
        Lex();

        /// ellipse(...) case
        if (GetCurrentToken().GetKind() == Token::Ellipsis)
        {
            Lex();    // eat '...'
            HasVarArg = true;
        }
        else
            Params.push_back(ParseParameterDeclaration());
    }

    return Params;
}


Type Parser::ParseTypeSpecifier()
{
    auto TK = GetCurrentToken();

    unsigned Qualifiers = 0;
    if (IsQualifer(TK.GetKind()))
    {
        Qualifiers = ParseQualifiers();
        TK         = GetCurrentToken();
    }

    if (!IsTypeSpecifier(TK) || (Qualifiers & Type::TypeDef))
    {
        auto Msg = fmt::format("Unexpected token '{}'", TK.GetString());
        DiagPrinter.AddError(Msg, TK);
        Lex();

        TK = GetCurrentToken();
    }

    auto ParsedType = ParseType(TK.GetKind());
    ParsedType.SetQualifiers(Qualifiers);

    return ParsedType;
}

Node Parser::ParseReturnTypeSpecifier() { return Node(); }

// <VariableDeclarationList> ::= <TypeSpecifier> <VariableDeclaration>
//                             |               {,<VariableDeclaration>} ';'
std::vector<std::unique_ptr<VariableDeclaration>> Parser::ParseVariableDeclarationList()
{
    Type type = ParseTypeSpecifier();
    Lex();

    // int a, b;
    std::vector<std::unique_ptr<VariableDeclaration>> VariableDeclarations;

    while (lexer.IsNot(Token::SemiColon) && lexer.IsNot(Token::EndOfFile))
    {
        VariableDeclarations.push_back(ParseVariableDeclaration(type));

        if (lexer.Is(Token::Comma))
            Lex();
        else
        {
            Expect(Token::SemiColon);
            break;
        }
    }

    return VariableDeclarations;
}

// <VaraibleDeclaration> ::= <TypeSpecifier> <Identifier>
//                           {'[' <IntegerConstant> ]'}* { = <Expression> } ';'
std::unique_ptr<VariableDeclaration> Parser::ParseVariableDeclaration(Type Ty)
{
    while (lexer.Is(Token::Mul))
    {
        Ty.IncrementPointerLevel();
        Lex();    // Eat the `*` character
    }

    Token Name = Expect(Token::Identifier);

    ParseArrayDimensions(Ty);

    // If the variable initialized
    std::unique_ptr<Expression> InitExpr {nullptr};
    if (lexer.Is(Token::Assign))
    {
        Token T = Lex();    // eat `=`

        auto ExpectedType = Ty;
        if (ExpectedType.IsArray())
            ExpectedType.RemoveFirstDimension();

        const bool IsInitList = lexer.Is(Token::LeftBrace);
        if (IsInitList)
            InitExpr = ParseInitializerListExpression(ExpectedType);
        else
        {
            InitExpr = ParseExpression();

            if (InitExpr == nullptr)
            {
                std::string Msg = "expected expression here";
                DiagPrinter.AddError(Msg, T);

                return nullptr;
            }

            if (!IsInitList && ! instanceof <StringLiteralExpression>(InitExpr.get()) &&
                                                !ExpectedType.IsStruct())
            {
                DoImplicitCastIfNeed(InitExpr, ExpectedType);
            }
        }
    }

    if (Ty.IsArray() && !Ty.GetDimensions().empty())
        DetermineUnspecifiedDimension(InitExpr.get(), Ty);

    InsertToSymbolTable(Name, Ty);

    auto VD = std::make_unique<VariableDeclaration>(Name, Ty);

    if (InitExpr)
        VD->SetInitExpr(std::move(InitExpr));

    return VD;
}

std::unique_ptr<MemberDeclaration> Parser::ParseMemberDeclaration()
{
    Type Ty = ParseTypeSpecifier();
    Lex();

    while (lexer.Is(Token::Mul))
    {
        Ty.IncrementPointerLevel();
        Lex();    // Eat the * character
    }

    Token Name = Expect(Token::Identifier);

    std::vector<unsigned> Dimensions;
    while (lexer.Is(Token::LeftBracket))
    {
        Lex();
        Dimensions.push_back(ParseIntegerConstant());
        Expect(Token::RightBracket);
    }

    Expect(Token::SemiColon);

    return std::make_unique<MemberDeclaration>(Name, Ty, Dimensions);
}

// <StructDeclaration> ::= 'struct' <Identifier>
//                                  '{' <StructDeclarationList>+ '}'
std::unique_ptr<StructDeclaration> Parser::ParseStructDeclaration(unsigned Qualifiers)
{
    Token T = Expect(Token::Struct);

    if (lexer.IsNot(Token::Identifier))
    {
        std::string Msg = "unnamed structures are not supported yet";
        DiagPrinter.AddNote(Msg, T);
    }

    Token Name   = Expect(Token::Identifier);
    auto NameStr = Name.GetString();

    Expect(Token::LeftBrace);    // eat '{'

    std::vector<std::unique_ptr<MemberDeclaration>> Members;
    Type type(Type::Struct);
    type.SetName(NameStr);
    type.SetQualifiers(Qualifiers);

    // register the type already even though it is an incomplete type
    // at this time of parsing
    UserDefinedTypes[NameStr] = {type, {}};

    std::vector<Token> StructMemberIdentifiers;
    while (lexer.IsNot(Token::RightBrace))
    {
        auto MD = ParseMemberDeclaration();
        type.GetTypeList().push_back(MD->GetType());
        StructMemberIdentifiers.push_back(MD->GetNameToken());
        Members.push_back(std::move(MD));
    }

    Expect(Token::RightBrace);

    if (Qualifiers & Type::TypeDef)
    {
        auto AliasName             = Expect(Token::Identifier).GetString();
        TypeDefinitions[AliasName] = type;
    }

    // saving the struct type and name
    UserDefinedTypes[NameStr] = {type, std::move(StructMemberIdentifiers)};

    return std::make_unique<StructDeclaration>(Name, Members, type);
}

// <EnumDeclaration> ::= 'enum' '{' <Identifier> (, <Identifier>)* '}' ';'
std::unique_ptr<EnumDeclaration> Parser::ParseEnumDeclaration(unsigned Qualifiers)
{
    Expect(Token::Enum);
    Expect(Token::LeftBrace);

    EnumDeclaration::EnumList Enumerators;

    int EnumCounter = 0;
    do
    {
        if (lexer.Is(Token::Comma))
            Lex();

        auto Identifier = Expect(Token::Identifier);
        Enumerators.push_back({Identifier.GetString(), EnumCounter});

        // Insert into the symbol table and for now assign the index of the enum
        // to it, not considering explicit assignments like "enum { A = 10 };"
        InsertToSymbolTable(Identifier,
                            Type(Type::Int),
                            false,
                            ValueType((unsigned)EnumCounter));
        EnumCounter++;
    }
    while (lexer.Is(Token::Comma));

    if (lexer.Is(Token::Assign))
    {
        Token T         = GetCurrentToken();
        std::string Msg = "assigning values to enumrations  are not supported yet";

        DiagPrinter.AddNote(Msg, T);
    }

    Expect(Token::RightBrace);

    if (Qualifiers & Type::TypeDef)
    {
        auto AliasName             = Expect(Token::Identifier).GetString();
        TypeDefinitions[AliasName] = Type(Type::Int);
    }

    Expect(Token::SemiColon);

    return std::make_unique<EnumDeclaration>(Enumerators);
}


//=--------------------------------------------------------------------------=//
//=------------------------- Parse Statement --------------------------------=//
//=--------------------------------------------------------------------------=//

// <Statement> ::= <ExpressionStatement>
//               | <WhileStatement>
//               | <DoWhileStatement>
//               | <IfStatement>
//               | <ForStatement>
//               | <CompoundStatement>
//               | <ReturnStatement>
std::unique_ptr<Statement> Parser::ParseStatement()
{
    if (lexer.Is(Token::If))
        return ParseIfStatement();
    if (lexer.Is(Token::Switch))
        return ParseSwitchStatement();
    if (lexer.Is(Token::Break))
        return ParseBreakStatement();
    if (lexer.Is(Token::Continue))
        return ParseContinueStatement();
    if (lexer.Is(Token::While))
        return ParseWhileStatement();
    if (lexer.Is(Token::Do))
        return ParseDoWhileStatement();
    if (lexer.Is(Token::For))
        return ParseForStatement();
    if (lexer.Is(Token::LeftBrace))
        return ParseCompoundStatement();
    if (lexer.Is(Token::Return))
        return ParseReturnStatement();

    return ParseExpressionStatement();
}

// <IfStatement> ::= if '(' <Expression> ')' <Statement> {else <Statement>}?
std::unique_ptr<IfStatement> Parser::ParseIfStatement()
{
    auto IS = std::make_unique<IfStatement>();

    Expect(Token::If);

    auto T         = Expect(Token::LeftParen);
    auto Condition = ParseExpression();
    if (Condition == nullptr)
    {
        std::string Msg = "expected expression here";
        DiagPrinter.AddError(Msg, T);
    }

    if (Condition && !Condition->GetResultType().IsIntegerType())
        Condition = std::make_unique<ImplicitCastExpression>(std::move(Condition),
                                                             Type(Type::Int));

    IS->SetCondition(std::move(Condition));
    Expect(Token::RightParen);
    IS->SetIfBody(ParseStatement());

    if (lexer.Is(Token::Else))
    {
        Lex();
        IS->SetElseBody(ParseStatement());
    }

    return IS;
}

// <SwitchStatement> ::= switch '(' <Expression> ')' '{'
//                       'case' <Constant> ':' <Statement>*
//                       'default' ':' <Statement>*
//                       '}'
std::unique_ptr<SwitchStatement> Parser::ParseSwitchStatement()
{
    std::unique_ptr<SwitchStatement> SS = std::make_unique<SwitchStatement>();

    Expect(Token::Switch);

    auto T         = Expect(Token::LeftParen);
    auto Condition = ParseExpression();
    if (Condition == nullptr)
    {
        std::string Msg = "expected expression here";
        DiagPrinter.AddError(Msg, T);
    }

    SS->SetCondition(std::move(Condition));

    Expect(Token::RightParen);
    Expect(Token::LeftBrace);

    SwitchStatement::CasesDataVec CasesData;
    unsigned FoundDefaults = 0;

    while (lexer.Is(Token::Case) || lexer.Is(Token::Default))
    {
        const bool IsCase = lexer.Is(Token::Case);
        Token Label       = Lex();    // eat 'case' or 'default'

        std::unique_ptr<Expression> CaseExpr;

        if (IsCase)
            CaseExpr = ParseExpression();

        if (CaseExpr == nullptr && IsCase)
        {
            std::string msg {"expected expression here"};
            DiagPrinter.AddError(msg, Label);
        }

        Expect(Token::Colon);

        StmtPtrVec Statements;
        while (lexer.IsNot(Token::RightBrace) && lexer.IsNot(Token::Case) &&
               lexer.IsNot(Token::Default))
            Statements.push_back(ParseStatement());

        if (IsCase)
        {
            CasesData.push_back({std::move(CaseExpr), std::move(Statements)});
        }
        else
        {
            FoundDefaults++;
            // TODO: Make it a semantic check
            if (FoundDefaults > 1)
            {
                std::string ErrMsg = "multiple default labels in one switch";
                DiagPrinter.AddError(ErrMsg, Label);
            }
            else
                SS->SetDefaultBody(std::move(Statements));
        }
    }

    SS->SetCasesBodies(std::move(CasesData));

    Expect(Token::RightBrace);

    return SS;
}

// <BreakStatement> ::= 'break' ';'
std::unique_ptr<BreakStatement> Parser::ParseBreakStatement()
{
    Expect(Token::Break);
    Expect(Token::SemiColon);

    return std::make_unique<BreakStatement>();
}

// <ContinueStatement> ::= 'continue' ';'
std::unique_ptr<ContinueStatement> Parser::ParseContinueStatement()
{
    Expect(Token::Continue);
    Expect(Token::SemiColon);
    return std::make_unique<ContinueStatement>();
}

// <WhileStatement> ::= while '(' <Expression> ')' <Statement>
std::unique_ptr<WhileStatement> Parser::ParseWhileStatement()
{
    auto WS = std::make_unique<WhileStatement>();
    Expect(Token::While);

    auto T         = Expect(Token::LeftParen);
    auto Condition = ParseExpression();
    if (Condition == nullptr)
    {
        std::string Msg = "expected expression here";
        DiagPrinter.AddError(Msg, T);
    }

    if (Condition && !Condition->GetResultType().IsIntegerType())
        Condition = std::make_unique<ImplicitCastExpression>(std::move(Condition),
                                                             Type(Type::Int));

    WS->SetCondition(std::move(Condition));
    Expect(Token::RightParen);
    WS->SetBody(ParseStatement());

    return WS;
}

// <DoWhileStatement> ::= do <Statement> while '(' <Expression> ')' ';'
std::unique_ptr<DoWhileStatement> Parser::ParseDoWhileStatement()
{
    auto DWS = std::make_unique<DoWhileStatement>();

    Expect(Token::Do);
    DWS->SetBody(ParseStatement());
    Expect(Token::While);

    auto T         = Expect(Token::LeftParen);
    auto Condition = ParseExpression();
    if (Condition == nullptr)
    {
        std::string Msg = "expected expression here";
        DiagPrinter.AddError(Msg, T);
    }

    if (Condition && !Condition->GetResultType().IsIntegerType())
        Condition = std::make_unique<ImplicitCastExpression>(std::move(Condition),
                                                             Type(Type::Int));

    DWS->SetCondition(std::move(Condition));
    Expect(Token::RightParen);
    Expect(Token::SemiColon);

    return DWS;
}

// <ForStatement> ::= for '(' <Expression> ';' <Expression> ';' <Expression> ')'
//                            <Statement>
//                  | for '(' <VariableDeclaration> <Expression> ';'
//                            <Expression> ')' <Statement>
std::unique_ptr<ForStatement> Parser::ParseForStatement()
{
    std::unique_ptr<ForStatement> FS = std::make_unique<ForStatement>();

    Expect(Token::For);
    Expect(Token::LeftParen);

    SymTabStack.PushSymbolTable();

    // Parse variable declaration.
    if (IsTypeSpecifier(GetCurrentToken()))
    {
        auto Declarations = ParseVariableDeclarationList();
        FS->SetVarDecls(std::move(Declarations));
    }
    else
    {
        FS->SetInit(ParseExpression());
        Expect(Token::SemiColon);
    }

    auto Condition = ParseExpression();
    if (Condition && !Condition->GetResultType().IsIntegerType())
        Condition = std::make_unique<ImplicitCastExpression>(std::move(Condition),
                                                             Type(Type::Int));
    FS->SetCondition(std::move(Condition));
    Expect(Token::SemiColon);

    FS->SetIncrement(ParseExpression());
    Expect(Token::RightParen);

    FS->SetBody(ParseStatement());

    SymTabStack.PopSymbolTable();

    return FS;
}


// <ReturnStatement> ::= return <Expression>? ';'
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement()
{
    ReturnNumber++;
    Expect(Token::Return);

    auto Expr = ParseExpression();
    if (Expr == nullptr)
    {
        Expect(Token::SemiColon);
        return std::make_unique<ReturnStatement>(nullptr);
    }

    auto LeftType  = CurrentFuncRetType.GetTypeVariant();
    auto RightType = Expr->GetResultType().GetTypeVariant();

    std::unique_ptr<ReturnStatement> RS;

    if (LeftType != RightType)
    {
        std::unique_ptr<Expression> CastExpr =
            std::make_unique<ImplicitCastExpression>(std::move(Expr), Type(LeftType));

        RS = std::make_unique<ReturnStatement>(std::move(CastExpr));
    }
    else
    {
        RS = std::make_unique<ReturnStatement>(std::move(Expr));
    }

    Expect(Token::SemiColon);

    return RS;
}

// <CompoundStatement> ::= '{' <VariableDeclaration>* <Statement>* '}'
std::unique_ptr<CompoundStatement> Parser::ParseCompoundStatement()
{
    Expect(Token::LeftBrace);

    std::vector<std::unique_ptr<Statement>> Statements;

    while ((IsQualifedType(GetCurrentToken()) || lexer.IsNot(Token::RightBrace)) &&
           lexer.IsNot(Token::EndOfFile))
    {
        if (IsQualifedType(GetCurrentToken()))
        {
            auto Declarations = ParseVariableDeclarationList();
            for (auto &Declaration : Declarations)
                Statements.push_back(std::move(Declaration));
        }
        else
            Statements.push_back(ParseStatement());
    }

    Expect(Token::RightBrace);

    return std::make_unique<CompoundStatement>(Statements);
}

// <ExpressionStatement> ::= <Expression>? ';'
std::unique_ptr<ExpressionStatement> Parser::ParseExpressionStatement()
{
    auto ES = std::make_unique<ExpressionStatement>();

    if (lexer.IsNot(Token::SemiColon))
        ES->SetExpression(ParseExpression());

    auto T = Expect(Token::SemiColon);

    if (T.GetKind() != Token::SemiColon && ES->GetExpression() == nullptr)
    {
        auto Msg = fmt::format("Unexpected token '{}'", GetCurrentToken().GetString());

        DiagPrinter.AddError(Msg, T);
        Lex();
    }

    return ES;
}

//=--------------------------------------------------------------------------=//
//=------------------------- Parse Expression -------------------------------=//
//=--------------------------------------------------------------------------=//

std::unique_ptr<Expression> Parser::ParseExpression() { return ParseBinaryExpression(); }

static bool IsPostfixOperator(Token TK)
{
    switch (TK.GetKind())
    {
        case Token::Arrow:
        case Token::Inc:
        case Token::Dec:
        case Token::LeftParen:
        case Token::LeftBracket:
        case Token::Dot: return true;

        default: break;
    }

    return false;
}

// <PostFixExpression> ::= <PrimaryExpression>
//                       | <PostFixExpression> '++'
//                       | <PostFixExpression> '--'
//                       | <PostFixExpression> '(' <Arguments> ')'
//                       | <PostFixExpression> '[' <Expression> ']'
//                       | <PostFixExpression> '.' <Identifier>
//                       | <PostFixExpression> '->' <Identifier>
//                       | ( TypeName ) '{' <Initializer-List> '}'
std::unique_ptr<Expression> Parser::ParsePostFixExpression()
{
    auto CurrentToken = lexer.GetCurrentToken();

    // Struct Initializing case
    if (lexer.Is(Token::LeftParen) &&
        ((lexer.LookAhead(2).GetKind() == Token::Identifier &&
          IsUserDefinedType(lexer.LookAhead(2).GetString())) ||
         (lexer.LookAhead(2).GetKind() == Token::Struct &&
          IsUserDefinedType(lexer.LookAhead(3).GetString()))))
    {
        Expect(Token::LeftParen);

        if (lexer.Is(Token::Struct))
            Lex();

        auto UserDTypeName = Expect(Token::Identifier).GetString();
        Expect(Token::RightParen);

        Expect(Token::LeftBrace);

        std::vector<Token> InitializedMemberList;
        ExprPtrVec InitList;

        while (lexer.Is(Token::Dot) || lexer.Is(Token::Identifier))
        {
            Token Member;

            if (lexer.Is(Token::Dot))
            {
                Lex();    // eat '.'
                Member = Expect(Token::Identifier);
                Expect(Token::Assign);
            }

            InitializedMemberList.push_back(Member);
            InitList.push_back(ParseExpression());

            if (!lexer.Is(Token::Comma))
                break;

            Lex();    // eat ','
        }

        Expect(Token::RightBrace);

        // construct a list of the orders how the fields are initialized
        // example:
        //      ```
        //      struct P {int x; int y};
        //      ...
        //
        //      struct P Obj = (struct P) { .y = 2, .x = 1}
        //
        //      ```
        //  in the above case the InitOrder would look like: {1, 0}, so the first
        //  init expression actually initializing the 2nd (index 1) struct member
        auto MemberNames = GetUserDefinedTypeMembers(UserDTypeName);
        std::vector<unsigned> InitOrder;

        for (auto &Member : InitializedMemberList)
        {
            unsigned Order = 0;
            bool Found     = false;
            for (auto &TypeMemberName : MemberNames)
            {
                if (TypeMemberName == Member)
                {
                    InitOrder.push_back(Order);
                    Found = true;
                    break;
                }
                Order++;
            }
            // TODO: move this to semantic
            if (!Found)
            {
                auto StructType = GetUserDefinedType(UserDTypeName);
                auto ErrMsg     = fmt::format("'{}' has no member named '{}'",
                                          StructType.ToString(),
                                          Member.GetString());

                DiagPrinter.AddError(ErrMsg, Member);
            }
        }


        return std::make_unique<StructInitExpression>(GetUserDefinedType(UserDTypeName),
                                                      std::move(InitList),
                                                      std::move(InitOrder));
    }

    auto Expr = ParsePrimaryExpression();
    if (!Expr)
        return nullptr;

    while (IsPostfixOperator(lexer.GetCurrentToken()))
    {
        if (lexer.Is(Token::Inc) || lexer.Is(Token::Dec))
        {
            auto Operation = lexer.GetCurrentToken();

            Lex();
            Expr->SetLValueness(true);
            Expr = std::make_unique<UnaryExpression>(Operation, std::move(Expr), true);
        }
        // Parse a CallExpression here
        else if (lexer.Is(Token::LeftParen))
        {
            Expr = ParseCallExpression(CurrentToken);
        }
        // parse ArrayExpression
        else if (lexer.Is(Token::LeftBracket))
        {
            Expr = ParseArrayExpression(std::move(Expr));
        }
        // parse StructMemberAccess
        else if (lexer.Is(Token::Dot) || lexer.Is(Token::Arrow))
        {
            const bool IsArrow = lexer.Is(Token::Arrow);

            Lex();    // eat the token
            auto MemberId    = Expect(Token::Identifier);
            auto MemberIdStr = MemberId.GetString();

            // find the type of the member
            auto StructDataTuple   = UserDefinedTypes[Expr->GetResultType().GetName()];
            auto StructType        = std::get<0>(StructDataTuple);
            auto StructMemberNames = std::get<1>(StructDataTuple);

            std::size_t MemberIndex = -1;
            for (std::size_t i = 0; i < StructMemberNames.size(); i++)
            {
                if (StructMemberNames[i] == MemberId)
                {
                    MemberIndex = i;
                    break;
                }
            }

            Expr = std::make_unique<StructMemberReference>(std::move(Expr),
                                                           MemberId,
                                                           MemberIndex,
                                                           IsArrow);

            if (Expr->GetResultType().IsStruct() || Expr->GetResultType().IsArray())
                Expr->SetLValueness(true);
        }
    }

    return Expr;
}

static int GetBinOpPrecedence(Token::TokenKind TK)
{
    switch (TK)
    {
        case Token::Assign:
        case Token::ModEqual:
        case Token::AndEqual:
        case Token::OrEqual:
        case Token::XorEqual:
        case Token::LeftShiftEqual:
        case Token::RightShiftEqual:
        case Token::PlusEqual:
        case Token::MinusEuqal:
        case Token::MulEqual:
        case Token::DivEqual: return 10;

        case Token::LogicalOr: return 20;
        case Token::LogicalAnd: return 30;
        case Token::Or: return 40;
        case Token::Xor: return 50;
        case Token::And: return 60;

        // ==, != , <=, >=
        case Token::GreaterEqual:
        case Token::LessEqual:
        case Token::Equal:
        case Token::NotEqual: return 70;

        /// <, >
        case Token::Less:
        case Token::Greater: return 80;

        case Token::LeftShift:
        case Token::RightShift: return 90;

        case Token::Plus:
        case Token::Minus: return 100;

        case Token::Mul:
        case Token::Div:
        case Token::Mod: return 110;

        default: return -1;
    }
}

std::unique_ptr<Expression> Parser::ParseUnaryExpression()
{
    auto UnaryOperation = GetCurrentToken();

    // cast expression case
    if (GetCurrentToken().GetKind() == Token::LeftParen &&
        IsTypeSpecifier(lexer.LookAhead(2)) &&
        !((lexer.LookAhead(2).GetKind() == Token::Identifier &&
           lexer.LookAhead(4).GetKind() == Token::LeftBrace) ||
          (lexer.LookAhead(2).GetKind() == Token::Struct &&
           lexer.LookAhead(5).GetKind() ==
               Token::LeftBrace)))    // it is not a struct initialization like
                                      // "(StructType){ ...}"
    {
        Lex();    // eat '('
        auto Ty = ParseType(GetCurrentToken().GetKind());
        Lex();

        while (lexer.Is(Token::Mul))
        {
            Ty.IncrementPointerLevel();
            Lex();    // Eat the * character
        }

        Expect(Token::RightParen);

        auto ExprToCast = ParseExpression();
        return std::make_unique<ImplicitCastExpression>(std::move(ExprToCast), Ty, true);
    }

    if (!IsUnaryOperator(UnaryOperation.GetKind()))
        return ParsePostFixExpression();

    Lex();

    std::unique_ptr<Expression> Expr;
    bool hasSizeof = false;

    // sizeof handing
    if (UnaryOperation.GetKind() == Token::Sizeof)
    {
        if (lexer.GetCurrentToken().GetKind() == Token::LeftParen)
        {
            Lex();
            hasSizeof = true;
        }

        if (IsTypeSpecifier(lexer.GetCurrentToken()))
        {
            auto Ty = ParseType(lexer.GetCurrentToken().GetKind());
            Lex();

            while (lexer.Is(Token::Mul))
            {
                Ty.IncrementPointerLevel();
                Lex();    // Eat the * character
            }

            if (hasSizeof)
                Expect(Token::RightParen);

            auto UE = std::make_unique<UnaryExpression>(UnaryOperation, nullptr);
            UE->SetSizeOfType(Ty);

            return UE;
        }
    }

    if (IsUnaryOperator(UnaryOperation.GetKind()))
    {
        auto UnaryExpr = ParseUnaryExpression();

        if (hasSizeof)
            Expect(Token::RightParen);

        if (UnaryOperation.GetKind() == Token::Inc ||
            UnaryOperation.GetKind() == Token::Dec)
            UnaryExpr->SetLValueness(true);

        return std::make_unique<UnaryExpression>(UnaryOperation, std::move(UnaryExpr));
    }

    // TODO: Add semantic check that only pointer types are dereferenced
    Expr = ParsePostFixExpression();

    if (hasSizeof)
        Expect(Token::RightParen);

    if (UnaryOperation.GetKind() == Token::Inc || UnaryOperation.GetKind() == Token::Dec)
        Expr->SetLValueness(true);

    return std::make_unique<UnaryExpression>(UnaryOperation, std::move(Expr));
}

std::unique_ptr<Expression> Parser::ParseBinaryExpression()
{
    auto LeftExpression = ParseUnaryExpression();
    if (!LeftExpression)
        return nullptr;

    // TODO: learn llvm
    if (lexer.Is(Token::Cond))
        LeftExpression = ParseTernaryExpression(std::move(LeftExpression));

    return ParseBinaryExpressionRHS(0, std::move(LeftExpression));
}

// <PrimaryExpression> ::= <IdentifierExpression>
//                       | '(' <Expression> ')'
//                       | <ConstantExpression>
std::unique_ptr<Expression> Parser::ParsePrimaryExpression()
{
    if (lexer.Is(Token::LeftParen))
    {
        Lex();    // consume '('
        auto Expression = ParseExpression();
        Expect(Token::RightParen);

        return Expression;
    }
    else if (lexer.Is(Token::Identifier))
        return ParseIdentifierExpression();
    else if (lexer.Is(Token::Real) || lexer.Is(Token::Integer) ||
             lexer.Is(Token::CharacterLiteral) || lexer.Is(Token::StringLiteral))
        return ParseConstantExpression();
    else
        return nullptr;
}

// <IdentifierExpression> ::= CallExpression
//                          | Identifier {'[' <Expression> ']'}+
std::unique_ptr<Expression> Parser::ParseIdentifierExpression()
{
    auto Id = Expect(Token::Identifier);

    auto RE    = std::make_unique<ReferenceExpression>(Id);
    auto IdStr = Id.GetString();

    if (auto SymEntry = SymTabStack.Contains(IdStr))
    {
        // If the symbol value is a know constant like in case of enumerators, then
        // return just a constant expression
        // TODO: Maybe do ths check earlier to save ourself from creating RE for
        // nothing
        if (auto Val = std::get<2>(SymEntry.value()); !Val.IsEmpty())
            return std::make_unique<IntegerLiteralExpression>(Val.GetIntVal());

        auto Ty = std::get<1>(SymEntry.value());
        RE->SetType(Ty);
    }
    else if (UserDefinedTypes.count(IdStr) > 0)
    {
        auto Ty = std::get<0>(UserDefinedTypes[IdStr]);
        RE->SetType(Ty);
    }

    return RE;
}

// <InitializerListExpression> ::= '{' {<ConstantExpression> |
//                                      <InitializerListExpression>}
//                                     {',' {<ConstantExpression> |
//                                      <InitializerListExpression>} }* '}'
std::unique_ptr<Expression> Parser::ParseInitializerListExpression(const Type &LHSType)
{
    Expect(Token::LeftBrace);

    auto ExpectedType = LHSType;
    if (ExpectedType.IsArray())
        ExpectedType.RemoveFirstDimension();

    std::unique_ptr<Expression> E;
    const bool IsInitList = lexer.Is(Token::LeftBrace);

    if (IsInitList)
        E = ParseInitializerListExpression(ExpectedType);
    else
        E = ParseConstantExpression();

    assert(E && "Cannot be null");

    // For now do not casting initializerlists
    // TODO: might be nice to do it in the future.
    if (!IsInitList && ! instanceof <StringLiteralExpression>(E.get()) &&
                                        !ExpectedType.IsStruct())
    {
        DoImplicitCastIfNeed(E, ExpectedType);
    }

    std::vector<ExprPtr> ExprList;
    ExprList.push_back(std::move(E));

    while (lexer.Is(Token::Comma))
    {
        Lex();    // eat ','

        const bool IsInitList = lexer.Is(Token::LeftBrace);
        if (IsInitList)
            E = ParseInitializerListExpression(ExpectedType);
        else
            E = ParseConstantExpression();

        if (!IsInitList && ! instanceof <StringLiteralExpression>(E.get()) &&
                                            !ExpectedType.IsStruct())
        {
            DoImplicitCastIfNeed(E, ExpectedType);
        }

        ExprList.push_back(std::move(E));
    }

    Expect(Token::RightBrace);

    return std::make_unique<InitializerListExpression>(std::move(ExprList));
}

std::unique_ptr<Expression> Parser::ParseCallExpression(Token Id)
{
    assert(Id.GetKind() == Token::Identifier && "Identifier expected");
    Lex();    // eat the '('

    // default return type is int
    Type FuncType {Type::Int};

    if (auto SymEntry = SymTabStack.Contains(Id.GetString()))
        FuncType = std::get<1>(SymEntry.value());

    std::vector<std::unique_ptr<Expression>> CallArgs;

    if (lexer.IsNot(Token::RightParen))
        CallArgs.push_back(ParseExpression());

    while (lexer.Is(Token::Comma))
    {
        Lex();
        CallArgs.push_back(ParseExpression());
    }

    // Currently a function without argument is actually a function with
    // a type of ...(void), which is a special case checked first.
    auto FuncArgTypes = FuncType.GetArgTypes();
    auto FuncArgNum   = FuncArgTypes.size();
    if (!(CallArgs.size() == 0 && FuncArgNum == 1 && FuncArgTypes[0] == Type(Type::Void)))
    {
        for (size_t i = 0; i < std::min(FuncArgNum, CallArgs.size()); i++)
        {
            auto CallArgType = CallArgs[i]->GetResultType();

            // If the ith argument type is not matching the expected one
            if (CallArgType != FuncArgTypes[i])
            {
                // Cast if allowed
                if (Type::IsImplicitlyCastable(CallArgType, FuncArgTypes[i]))
                    CallArgs[i] =
                        std::make_unique<ImplicitCastExpression>(std::move(CallArgs[i]),
                                                                 FuncArgTypes[i]);
            }
        }
    }

    Expect(Token::RightParen);

    return std::make_unique<CallExpression>(Id, CallArgs, FuncType);
}

std::unique_ptr<Expression> Parser::ParseArrayExpression(std::unique_ptr<Expression> Base)
{
    Lex();
    auto IndexExpr = ParseExpression();
    Expect(Token::RightBracket);

    Type type = Base->GetResultType();

    /// Remove the first  dimensions from the actual type. Example:
    /// ActualType is 'int arr[5][10]' and our reference is 'arr[0]'
    /// then the result type of 'arr[0]' is 'int[10]'. N is the
    /// amount of index expressions used when referencing the array here
    /// 'arr'. In the example its 1.
    if (!type.IsPointerType() && type.IsArray())
    {
        type.GetDimensions().erase(type.GetDimensions().begin());

        // if the result is now a scalar, then change the type to Simple (scalar)
        if (type.GetDimensions().empty())
            type.SetTypeKind(Type::Simple);
    }

    else if (type.IsPointerType())
        type.DecrementPointerLevel();

    Base->SetLValueness(true);
    return std::make_unique<ArrayExpression>(Base, IndexExpr, type);
}

// <ConstantExpression> ::= '-'? [1-9][0-9]*
//                        | '-'? [0-9]+.[0-9]+
//                        | -?'\'' \?. '\''
std::unique_ptr<Expression> Parser::ParseConstantExpression()
{
    bool IsNegative = false;
    if (lexer.Is(Token::Minus))
    {
        Lex();
        IsNegative = true;
    }

    if (lexer.Is(Token::Identifier))
    {
        auto Id    = Expect(Token::Identifier);
        auto IdStr = Id.GetString();

        if (auto SymEntry = SymTabStack.Contains(IdStr))
        {
            if (auto Val = std::get<2>(SymEntry.value()); !Val.IsEmpty())
            {
                auto EnumILExpr =
                    std::make_unique<IntegerLiteralExpression>(Val.GetIntVal());
                if (IsNegative)
                    EnumILExpr->SetValue(-EnumILExpr->GetSIntValue());

                return EnumILExpr;
            }
            else
                assert(!"Not an enumerator constant");
        }
        else
            assert(!"Not an enumerator constant");
    }
    else if (lexer.Is(Token::Integer))
    {
        auto IntLitExpr =
            std::make_unique<IntegerLiteralExpression>(ParseIntegerConstant());

        if (IsNegative)
            IntLitExpr->SetValue(-IntLitExpr->GetSIntValue());

        // TODO: example:
        // currently `1 ull` would be valid since the lexer will ignore the
        // white spaces, make such input invalid
        if (lexer.Is(Token::Identifier))
        {
            auto Str = GetCurrentToken().GetString();
            if (Str == "u")
            {
                Lex();
                IntLitExpr->SetType(Type(Type::UnsignedInt));
            }
            else if (Str == "l")
            {
                Lex();
                IntLitExpr->SetType(Type(Type::Long));
            }
            else if (Str == "ul")
            {
                Lex();
                IntLitExpr->SetType(Type(Type::UnsignedLong));
            }
            else if (Str == "ll")
            {
                Lex();
                IntLitExpr->SetType(Type(Type::LongLong));
            }
            else if (Str == "ull")
            {
                Lex();
                IntLitExpr->SetType(Type(Type::UnsignedLongLong));
            }
        }

        return IntLitExpr;
    }
    else if (lexer.Is(Token::CharacterLiteral))
    {
        auto CharToken = Expect(Token::CharacterLiteral);

        auto IntList = std::make_unique<IntegerLiteralExpression>(CharToken.GetValue());

        if (IsNegative)
            IntList->SetValue(-IntList->GetSIntValue());

        return IntList;
    }
    else if (lexer.Is(Token::StringLiteral))
    {
        auto StringToken = Expect(Token::StringLiteral);

        assert(StringToken.GetString().length() >= 2);

        return std::make_unique<StringLiteralExpression>(
            StringToken.GetString().substr(1, StringToken.GetString().length() - 2));
    }
    else
    {
        auto FPLitExpr = std::make_unique<FloatLiteralExpression>(ParseRealConstant());

        if (IsNegative)
            FPLitExpr->SetValue(-FPLitExpr->GetValue());

        return FPLitExpr;
    }
}

std::unique_ptr<Expression>
    Parser::ParseBinaryExpressionRHS(int Precedence,
                                     std::unique_ptr<Expression> LeftExpression)
{
    while (true)
    {
        int TokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());

        if (TokenPrec < Precedence)
            return LeftExpression;

        Token BinaryOperator = Lex();
        auto BinOPTK         = BinaryOperator.GetKind();

        auto RightExpression = ParseUnaryExpression();

        bool IsArithmetic = BinaryOperator.IsArithmetic(BinOPTK);
        bool IsAssignment = BinaryOperator.IsCompositeAssignment(BinOPTK);

        if (IsArithmetic &&
            Type::IsSmallerThanInt(LeftExpression->GetResultType().GetTypeVariant()))
        {
            LeftExpression =
                std::make_unique<ImplicitCastExpression>(std::move(LeftExpression),
                                                         Type(Type::Int));
        }

        if (IsArithmetic &&
            Type::IsSmallerThanInt(RightExpression->GetResultType().GetTypeVariant()))
        {
            RightExpression =
                std::make_unique<ImplicitCastExpression>(std::move(RightExpression),
                                                         Type(Type::Int));
        }

        /// In case of Assignment check if the left operand since if should be an
        /// lvalue. Which is either an identifier reference or an array expression.
        const auto LHS = LeftExpression.get();
        if (IsAssignment)
        {
            // clang-format off
            if (instanceof <ArrayExpression>(LHS) || 
                instanceof <ReferenceExpression>(LHS) ||
                instanceof <StructMemberReference>(LHS) ||
                (instanceof<UnaryExpression>(LHS) && dynamic_cast<UnaryExpression *>(LHS)->GetOperationKind() == UnaryExpression::DeRef))
            // clang-format on
            {
                LeftExpression->SetLValueness(true);
            }
        }

        // convert left expression to Rvalue if the operantion is not assignment
        if (!IsAssignment)
        {
            LeftExpression->SetLValueness(false);
        }

        // convert RightExpression to RValue.
        RightExpression->SetLValueness(false);

        int NextTokenPrec = GetBinOpPrecedence(GetCurrentTokenKind());

        int Associativity = 1;    // left associative
        if (BinaryOperator.GetKind() == Token::Assign)
        {
            Associativity = 0;    // right associative
            NextTokenPrec++;
        }

        if (TokenPrec < NextTokenPrec)
            RightExpression = ParseBinaryExpressionRHS(TokenPrec + Associativity,
                                                       std::move(RightExpression));

        // Implicit Cast Insertion if needed.
        auto LeftType  = LeftExpression->GetResultType();
        auto RightType = RightExpression->GetResultType();

        if (LeftType != RightType)
        {
            /// if an assignment, then try to cast the RHS to type of LHS.
            if (IsAssignment)
            {
                if (Type::IsImplicitlyCastable(RightType, LeftType))
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression),
                        LeftType);
            }
            /// Otherwise cast the one with lower conversion rank to higher one .
            else if (Type::IsImplicitlyCastable(RightType, LeftType) ||
                     Type::IsImplicitlyCastable(LeftType, RightType))
            {
                auto DesiredType =
                    Type::GetStrongestType(LeftType, RightType).GetTypeVariant();

                const bool LeftIsPtr = LeftType.IsPointerType();
                const bool LeftNeedConversion =
                    (LeftType != Type(DesiredType)) && (!LeftIsPtr);

                // if LHS needs the conversion.
                if (LeftNeedConversion)
                    LeftExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(LeftExpression),
                        RightType);
                else
                    RightExpression = std::make_unique<ImplicitCastExpression>(
                        std::move(RightExpression),
                        LeftType);
            }
        }
        // TODO: This will only work here if the ternary condition was in
        //  parenthesis, which for the time being is sufficient. Make it work as it
        //  should.
        if (lexer.Is(Token::Cond))
            RightExpression = ParseTernaryExpression(std::move(RightExpression));

        LeftExpression = std::make_unique<BinaryExpression>(std::move(LeftExpression),
                                                            BinaryOperator,
                                                            std::move(RightExpression));
    }
}

// <TernaryExpression> ::= <Expression> '?' <Expression> ':' <Expression>
std::unique_ptr<Expression> Parser::ParseTernaryExpression(ExprPtr Condition)
{
    if (Condition && !Condition->GetResultType().IsIntegerType())
        Condition = std::make_unique<ImplicitCastExpression>(std::move(Condition),
                                                             Type(Type::Int));
    Expect(Token::Cond);

    auto TrueExpr = ParseExpression();
    Expect(Token::Colon);
    auto FalseExpr = ParseExpression();

    return std::make_unique<TernaryExpression>(Condition, TrueExpr, FalseExpr);
}

void Parser::InsertToSymbolTable(const Token &SymbolName,
                                 const Type &SymType,
                                 const bool ToGlobal,
                                 ValueType SymValue)
{
    SymbolTableStack::Entry SymEntry(SymbolName, SymType, SymValue);
    ToGlobal ? SymTabStack.InsertGlobalEntry(SymEntry) :
               SymTabStack.InsertEntry(SymEntry);
}
